use std::{collections::BTreeMap, env, fs, process};

#[derive(Debug)]
struct Variable {
    name: String,
    size: usize,
    lo: i64,
    hi: i64,
    givens: Vec<(usize, i64)>,
}

#[derive(Debug)]
struct Problem {
    vars: Vec<Variable>,
    all_different: Vec<Vec<(usize, usize)>>,
}

fn fail(line: usize, message: impl std::fmt::Display) -> String {
    format!(
        "{{\"ok\":false,\"error\":{{\"code\":\"CSP{:04}\",\"line\":{},\"message\":\"{}\"}}}}",
        1,
        line,
        message.to_string().replace('"', "\\\"")
    )
}

fn parse_index_list(s: &str, line: usize) -> Result<Vec<usize>, String> {
    let inside = s
        .strip_prefix('[')
        .and_then(|s| s.strip_suffix(']'))
        .ok_or_else(|| fail(line, "expected indexes in [..]"))?;
    let mut out = Vec::new();
    for item in inside.split(',').map(str::trim) {
        if let Some((a, b)) = item.split_once("..") {
            let a: usize = a
                .trim()
                .parse()
                .map_err(|_| fail(line, "invalid range start"))?;
            let b: usize = b
                .trim()
                .parse()
                .map_err(|_| fail(line, "invalid range end"))?;
            if b <= a {
                return Err(fail(line, "ranges are non-empty and half-open"));
            }
            out.extend(a..b);
        } else {
            out.push(
                item.parse()
                    .map_err(|_| fail(line, "invalid array index"))?,
            );
        }
    }
    if out.is_empty() {
        return Err(fail(line, "constraint cannot be empty"));
    }
    Ok(out)
}

fn parse(src: &str) -> Result<Problem, String> {
    let mut vars = Vec::<Variable>::new();
    let mut names = BTreeMap::<String, usize>::new();
    let mut raw_constraints = Vec::<(usize, String, Vec<usize>)>::new();
    for (line0, raw) in src.lines().enumerate() {
        let line_no = line0 + 1;
        let line = raw.split("//").next().unwrap().trim();
        if line.is_empty() {
            continue;
        }
        if let Some(rest) = line.strip_prefix("var ") {
            let (lhs, domain) = rest
                .split_once(" in ")
                .ok_or_else(|| fail(line_no, "expected `var name[size] in lo..hi`"))?;
            let (name, size) = lhs
                .split_once('[')
                .ok_or_else(|| fail(line_no, "expected variable size"))?;
            let size: usize = size
                .strip_suffix(']')
                .ok_or_else(|| fail(line_no, "expected closing ]"))?
                .parse()
                .map_err(|_| fail(line_no, "invalid variable size"))?;
            let (lo, hi) = domain
                .split_once("..")
                .ok_or_else(|| fail(line_no, "expected integer domain lo..hi"))?;
            let lo: i64 = lo
                .trim()
                .parse()
                .map_err(|_| fail(line_no, "invalid domain start"))?;
            let hi: i64 = hi
                .trim()
                .parse()
                .map_err(|_| fail(line_no, "invalid domain end"))?;
            if size == 0 || size > 100_000 || hi <= lo || hi - lo > 10_000 {
                return Err(fail(line_no, "invalid or oversized domain"));
            }
            if names.contains_key(name.trim()) {
                return Err(fail(line_no, "duplicate variable"));
            }
            names.insert(name.trim().into(), vars.len());
            vars.push(Variable {
                name: name.trim().into(),
                size,
                lo,
                hi,
                givens: vec![],
            });
        } else if let Some(rest) = line.strip_prefix("given ") {
            let (place, value) = rest
                .split_once('=')
                .ok_or_else(|| fail(line_no, "expected `given name[index] = value`"))?;
            let (name, index) = place
                .split_once('[')
                .ok_or_else(|| fail(line_no, "expected array index"))?;
            let name = name.trim();
            let index: usize = index
                .trim()
                .strip_suffix(']')
                .ok_or_else(|| fail(line_no, "expected closing ]"))?
                .parse()
                .map_err(|_| fail(line_no, "invalid index"))?;
            let value: i64 = value
                .trim()
                .parse()
                .map_err(|_| fail(line_no, "invalid given value"))?;
            let id = *names
                .get(name)
                .ok_or_else(|| fail(line_no, format!("unknown variable `{name}`")))?;
            let var = &mut vars[id];
            if index >= var.size || value < var.lo || value >= var.hi {
                return Err(fail(line_no, "given is outside the variable or domain"));
            }
            var.givens.push((index, value));
        } else if let Some(rest) = line.strip_prefix("all_different ") {
            let (name, indexes) = rest
                .split_once('[')
                .ok_or_else(|| fail(line_no, "expected `all_different name[indexes]`"))?;
            let name = name.trim().to_string();
            let indexes = parse_index_list(&format!("[{indexes}"), line_no)?;
            raw_constraints.push((line_no, name, indexes));
        } else {
            return Err(fail(
                line_no,
                "unknown declaration; use var, given, or all_different",
            ));
        }
    }
    if vars.is_empty() {
        return Err(fail(1, "problem needs at least one variable"));
    }
    let mut all_different = Vec::new();
    for (line, name, indexes) in raw_constraints {
        let id = *names
            .get(&name)
            .ok_or_else(|| fail(line, format!("unknown variable `{name}`")))?;
        if indexes.iter().any(|&i| i >= vars[id].size) {
            return Err(fail(line, "constraint index is out of bounds"));
        }
        if indexes.len() > (vars[id].hi - vars[id].lo) as usize {
            return Err(fail(line, "all_different has more values than the domain"));
        }
        all_different.push(indexes.into_iter().map(|i| (id, i)).collect());
    }
    Ok(Problem {
        vars,
        all_different,
    })
}

fn propagate(problem: &Problem, domains: &mut [Vec<Vec<i64>>]) -> bool {
    loop {
        let mut changed = false;
        for group in &problem.all_different {
            let mut fixed = Vec::new();
            for &(v, i) in group {
                if domains[v][i].len() == 1 {
                    fixed.push((v, i, domains[v][i][0]));
                }
            }
            for a in 0..fixed.len() {
                for b in a + 1..fixed.len() {
                    if fixed[a].2 == fixed[b].2 && fixed[a].0 != fixed[b].0
                        || fixed[a].2 == fixed[b].2 && fixed[a].1 != fixed[b].1
                    {
                        return false;
                    }
                }
            }
            for &(v, i) in group {
                if domains[v][i].len() == 1 {
                    continue;
                }
                let old = domains[v][i].len();
                domains[v][i].retain(|x| {
                    !fixed
                        .iter()
                        .any(|&(fv, fi, value)| value == *x && (fv, fi) != (v, i))
                });
                if domains[v][i].is_empty() {
                    return false;
                }
                changed |= domains[v][i].len() != old;
            }
        }
        if !changed {
            return true;
        }
    }
}

fn search(problem: &Problem, mut domains: Vec<Vec<Vec<i64>>>) -> Option<Vec<Vec<Vec<i64>>>> {
    if !propagate(problem, &mut domains) {
        return None;
    }
    let choice = domains
        .iter()
        .enumerate()
        .flat_map(|(v, cells)| cells.iter().enumerate().map(move |(i, d)| (v, i, d.len())))
        .filter(|(_, _, n)| *n > 1)
        .min_by_key(|x| x.2);
    let Some((v, i, _)) = choice else {
        return Some(domains);
    };
    for value in domains[v][i].clone() {
        let mut branch = domains.clone();
        branch[v][i] = vec![value];
        if let Some(answer) = search(problem, branch) {
            return Some(answer);
        }
    }
    None
}

fn main() {
    let path = env::args().nth(1).unwrap_or_else(|| {
        eprintln!("usage: slop-csp <file.csp>");
        process::exit(2)
    });
    let src = fs::read_to_string(&path).unwrap_or_else(|e| {
        eprintln!("{{\"ok\":false,\"error\":{{\"code\":\"CSP0000\",\"message\":\"{e}\"}}}}");
        process::exit(2)
    });
    let problem = match parse(&src) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("{e}");
            process::exit(1)
        }
    };
    let mut domains: Vec<Vec<Vec<i64>>> = problem
        .vars
        .iter()
        .map(|v| vec![(v.lo..v.hi).collect(); v.size])
        .collect();
    for (v, variable) in problem.vars.iter().enumerate() {
        for &(i, value) in &variable.givens {
            domains[v][i] = vec![value];
        }
    }
    match search(&problem, domains) {
        Some(answer) => {
            let values = problem
                .vars
                .iter()
                .enumerate()
                .map(|(v, var)| {
                    format!(
                        "\"{}\":[{}]",
                        var.name,
                        answer[v]
                            .iter()
                            .map(|d| d[0].to_string())
                            .collect::<Vec<_>>()
                            .join(",")
                    )
                })
                .collect::<Vec<_>>()
                .join(",");
            println!("{{\"ok\":true,\"value\":{{{values}}}}}");
        }
        None => {
            println!(
                "{{\"ok\":false,\"error\":{{\"code\":\"CSP1000\",\"message\":\"no solution\"}}}}"
            );
            process::exit(1);
        }
    }
}
