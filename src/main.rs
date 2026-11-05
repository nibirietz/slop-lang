use std::{cell::RefCell, env, fs, process, rc::Rc};

#[derive(Clone, Debug, PartialEq)]
enum Tok {
    Id(String),
    Num(i64),
    Sym(String),
    Eof,
}

#[derive(Clone, Debug)]
struct Diag {
    code: &'static str,
    message: String,
    at: usize,
}
impl Diag {
    fn new(code: &'static str, message: impl Into<String>, at: usize) -> Self {
        Self {
            code,
            message: message.into(),
            at,
        }
    }
}

fn lex(src: &str) -> Result<Vec<Tok>, Diag> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    while i < b.len() {
        if b[i].is_ascii_whitespace() {
            i += 1;
            continue;
        }
        if b[i] == b'/' && b.get(i + 1) == Some(&b'/') {
            while i < b.len() && b[i] != b'\n' {
                i += 1
            }
            continue;
        }
        if b[i].is_ascii_alphabetic() || b[i] == b'_' {
            let s = i;
            i += 1;
            while i < b.len() && (b[i].is_ascii_alphanumeric() || b[i] == b'_') {
                i += 1
            }
            out.push(Tok::Id(src[s..i].into()));
            continue;
        }
        if b[i].is_ascii_digit() {
            let s = i;
            i += 1;
            while i < b.len() && b[i].is_ascii_digit() {
                i += 1
            }
            out.push(Tok::Num(src[s..i].parse().unwrap()));
            continue;
        }
        let two = if i + 1 < b.len() { &src[i..i + 2] } else { "" };
        if ["==", "!=", "<=", ">=", "&&", "||", "->"].contains(&two) {
            out.push(Tok::Sym(two.into()));
            i += 2;
            continue;
        }
        let c = b[i] as char;
        if "{}()[],:;=+-*/%!<>".contains(c) {
            out.push(Tok::Sym(c.to_string()));
            i += 1;
            continue;
        }
        return Err(Diag::new("E0001", format!("unexpected character `{c}`"), i));
    }
    out.push(Tok::Eof);
    Ok(out)
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Ty {
    Int,
    Bool,
    Array,
    Unit,
}
#[derive(Clone, Debug)]
enum Expr {
    Num(i64),
    Bool(bool),
    Var(String),
    Array(Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    Unary(String, Box<Expr>),
    Bin(String, Box<Expr>, Box<Expr>),
}
#[derive(Clone, Debug)]
enum Stmt {
    Let(String, Ty, Expr),
    Set(String, Expr),
    SetIndex(String, Expr, Expr),
    Expr(Expr),
    Return(Expr),
    If(Expr, Vec<Stmt>, Vec<Stmt>),
    While(Expr, Vec<Stmt>),
}
#[derive(Clone, Debug)]
struct Fun {
    name: String,
    args: Vec<(String, Ty)>,
    ret: Ty,
    body: Vec<Stmt>,
}

struct Parser {
    t: Vec<Tok>,
    p: usize,
}
impl Parser {
    fn at(&self) -> &Tok {
        &self.t[self.p]
    }
    fn bump(&mut self) -> Tok {
        let x = self.t[self.p].clone();
        self.p += 1;
        x
    }
    fn sym(&mut self, s: &str) -> Result<(), Diag> {
        if self.at() == &Tok::Sym(s.into()) {
            self.bump();
            Ok(())
        } else {
            Err(Diag::new(
                "E0002",
                format!("expected `{s}`, found {:?}", self.at()),
                self.p,
            ))
        }
    }
    fn id(&mut self) -> Result<String, Diag> {
        if let Tok::Id(s) = self.bump() {
            Ok(s)
        } else {
            Err(Diag::new(
                "E0002",
                format!("expected name, found {:?}", self.at()),
                self.p,
            ))
        }
    }
    fn ty(&mut self) -> Result<Ty, Diag> {
        let s = self.id()?;
        match s.as_str() {
            "Int" => Ok(Ty::Int),
            "Bool" => Ok(Ty::Bool),
            "Array" => Ok(Ty::Array),
            "Unit" => Ok(Ty::Unit),
            _ => Err(Diag::new("E0003", format!("unknown type `{s}`"), self.p)),
        }
    }
    fn program(&mut self) -> Result<Vec<Fun>, Diag> {
        let mut f = vec![];
        while self.at() != &Tok::Eof {
            self.id_kw("fn")?;
            let name = self.id()?;
            self.sym("(")?;
            let mut args = vec![];
            if self.at() != &Tok::Sym(")".into()) {
                loop {
                    let n = self.id()?;
                    self.sym(":")?;
                    let t = self.ty()?;
                    args.push((n, t));
                    if self.at() == &Tok::Sym(",".into()) {
                        self.bump();
                        continue;
                    }
                    break;
                }
            }
            self.sym(")")?;
            self.sym("->")?;
            let ret = self.ty()?;
            let body = self.block()?;
            f.push(Fun {
                name,
                args,
                ret,
                body,
            })
        }
        Ok(f)
    }
    fn id_kw(&mut self, s: &str) -> Result<(), Diag> {
        if self.at() == &Tok::Id(s.into()) {
            self.bump();
            Ok(())
        } else {
            Err(Diag::new("E0002", format!("expected `{s}`"), self.p))
        }
    }
    fn block(&mut self) -> Result<Vec<Stmt>, Diag> {
        self.sym("{")?;
        let mut v = vec![];
        while self.at() != &Tok::Sym("}".into()) {
            if self.at() == &Tok::Eof {
                return Err(Diag::new("E0002", "unterminated block", self.p));
            }
            v.push(self.stmt()?)
        }
        self.sym("}")?;
        Ok(v)
    }
    fn stmt(&mut self) -> Result<Stmt, Diag> {
        if self.at() == &Tok::Id("let".into()) {
            self.bump();
            let n = self.id()?;
            self.sym(":")?;
            let t = self.ty()?;
            self.sym("=")?;
            let e = self.expr(0)?;
            self.sym(";")?;
            return Ok(Stmt::Let(n, t, e));
        }
        if self.at() == &Tok::Id("return".into()) {
            self.bump();
            let e = self.expr(0)?;
            self.sym(";")?;
            return Ok(Stmt::Return(e));
        }
        if self.at() == &Tok::Id("if".into()) {
            self.bump();
            let c = self.expr(0)?;
            let a = self.block()?;
            let b = if self.at() == &Tok::Id("else".into()) {
                self.bump();
                self.block()?
            } else {
                vec![]
            };
            return Ok(Stmt::If(c, a, b));
        }
        if self.at() == &Tok::Id("while".into()) {
            self.bump();
            let c = self.expr(0)?;
            let b = self.block()?;
            return Ok(Stmt::While(c, b));
        }
        if let Tok::Id(n) = self.at().clone() {
            if let Some(Tok::Sym(s)) = self.t.get(self.p + 1) {
                if s == "=" {
                    self.bump();
                    self.bump();
                    let e = self.expr(0)?;
                    self.sym(";")?;
                    return Ok(Stmt::Set(n, e));
                }
                if s == "[" {
                    self.bump();
                    self.bump();
                    let i = self.expr(0)?;
                    self.sym("]")?;
                    self.sym("=")?;
                    let e = self.expr(0)?;
                    self.sym(";")?;
                    return Ok(Stmt::SetIndex(n, i, e));
                }
            }
        }
        let e = self.expr(0)?;
        self.sym(";")?;
        Ok(Stmt::Expr(e))
    }
    fn expr(&mut self, min: u8) -> Result<Expr, Diag> {
        let mut x = match self.bump() {
            Tok::Num(n) => Expr::Num(n),
            Tok::Id(s) if s == "true" => Expr::Bool(true),
            Tok::Id(s) if s == "false" => Expr::Bool(false),
            Tok::Id(s) => {
                if self.at() == &Tok::Sym("(".into()) {
                    self.bump();
                    let mut a = vec![];
                    if self.at() != &Tok::Sym(")".into()) {
                        loop {
                            a.push(self.expr(0)?);
                            if self.at() == &Tok::Sym(",".into()) {
                                self.bump();
                                continue;
                            }
                            break;
                        }
                    }
                    self.sym(")")?;
                    Expr::Call(s, a)
                } else {
                    Expr::Var(s)
                }
            }
            Tok::Sym(s) if s == "-" || s == "!" => Expr::Unary(s, Box::new(self.expr(8)?)),
            Tok::Sym(s) if s == "(" => {
                let e = self.expr(0)?;
                self.sym(")")?;
                e
            }
            Tok::Sym(s) if s == "[" => {
                let mut a = vec![];
                if self.at() != &Tok::Sym("]".into()) {
                    loop {
                        a.push(self.expr(0)?);
                        if self.at() == &Tok::Sym(",".into()) {
                            self.bump();
                            continue;
                        }
                        break;
                    }
                }
                self.sym("]")?;
                Expr::Array(a)
            }
            t => {
                return Err(Diag::new(
                    "E0004",
                    format!("expected expression, found {t:?}"),
                    self.p,
                ));
            }
        };
        loop {
            if self.at() == &Tok::Sym("[".into()) {
                self.bump();
                let i = self.expr(0)?;
                self.sym("]")?;
                x = Expr::Index(Box::new(x), Box::new(i));
                continue;
            }
            let op = if let Tok::Sym(s) = self.at() {
                s.clone()
            } else {
                break;
            };
            let prec = match op.as_str() {
                "||" => 1,
                "&&" => 2,
                "==" | "!=" | "<" | ">" | "<=" | ">=" => 3,
                "+" | "-" => 4,
                "*" | "/" | "%" => 5,
                _ => break,
            };
            if prec < min {
                break;
            }
            self.bump();
            let y = self.expr(prec + 1)?;
            x = Expr::Bin(op, Box::new(x), Box::new(y));
        }
        Ok(x)
    }
}

#[derive(Clone, Debug)]
enum Ins {
    Const(i64),
    Bool(bool),
    Load(usize),
    Store(usize),
    Array(usize),
    Fill,
    Get,
    Put,
    Pop,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
    Neg,
    Not,
    Jump(usize),
    Jz(usize),
    Call(usize, usize),
    Ret,
    Len,
}
#[derive(Clone, Debug)]
struct Code {
    name: String,
    ret: Ty,
    slots: usize,
    ops: Vec<Ins>,
}
#[derive(Clone, Debug)]
enum Val {
    Int(i64),
    Bool(bool),
    Arr(Rc<RefCell<Vec<i64>>>),
    Unit,
}
struct Compiler {
    funcs: Vec<Fun>,
    codes: Vec<Code>,
    names: std::collections::HashMap<String, usize>,
}
impl Compiler {
    fn ty_expr(&self, e: &Expr, env: &std::collections::HashMap<String, Ty>) -> Result<Ty, Diag> {
        use Expr::*;
        Ok(match e {
            Num(_) => Ty::Int,
            Bool(_) => Ty::Bool,
            Var(n) => *env
                .get(n)
                .ok_or_else(|| Diag::new("E0101", format!("unknown variable `{n}`"), 0))?,
            Array(v) => {
                for e in v {
                    if self.ty_expr(e, env)? != Ty::Int {
                        return Err(Diag::new("E0102", "array elements must be Int", 0));
                    }
                }
                Ty::Array
            }
            Index(a, i) => {
                if self.ty_expr(a, env)? != Ty::Array || self.ty_expr(i, env)? != Ty::Int {
                    return Err(Diag::new("E0102", "index requires Array[Int] and Int", 0));
                }
                Ty::Int
            }
            Call(n, a) => {
                if n == "len" {
                    if a.len() != 1 || self.ty_expr(&a[0], env)? != Ty::Array {
                        return Err(Diag::new("E0102", "len expects one Array", 0));
                    }
                    Ty::Int
                } else if n == "fill" {
                    if a.len() != 2
                        || self.ty_expr(&a[0], env)? != Ty::Int
                        || self.ty_expr(&a[1], env)? != Ty::Int
                    {
                        return Err(Diag::new(
                            "E0102",
                            "fill expects (Int length, Int value)",
                            0,
                        ));
                    }
                    Ty::Array
                } else {
                    let f = self
                        .names
                        .get(n)
                        .ok_or_else(|| Diag::new("E0101", format!("unknown function `{n}`"), 0))?;
                    let sig = &self.funcs[*f];
                    if sig.args.len() != a.len() {
                        return Err(Diag::new(
                            "E0103",
                            format!("{} expects {} arguments", n, sig.args.len()),
                            0,
                        ));
                    }
                    for (e, (_, t)) in a.iter().zip(&sig.args) {
                        if self.ty_expr(e, env)? != *t {
                            return Err(Diag::new(
                                "E0102",
                                format!("argument type mismatch in `{n}`"),
                                0,
                            ));
                        }
                    }
                    sig.ret
                }
            }
            Unary(op, a) => {
                let t = self.ty_expr(a, env)?;
                if (op == "-" && t != Ty::Int) || (op == "!" && t != Ty::Bool) {
                    return Err(Diag::new("E0102", "invalid unary operand", 0));
                }
                t
            }
            Bin(op, a, b) => {
                let x = self.ty_expr(a, env)?;
                let y = self.ty_expr(b, env)?;
                if x != y {
                    return Err(Diag::new("E0102", "operand types differ", 0));
                }
                match op.as_str() {
                    "+" | "-" | "*" | "/" | "%" if x == Ty::Int => Ty::Int,
                    "&&" | "||" if x == Ty::Bool => Ty::Bool,
                    "==" | "!=" if x == Ty::Int || x == Ty::Bool => Ty::Bool,
                    "<" | ">" | "<=" | ">=" if x == Ty::Int => Ty::Bool,
                    _ => {
                        return Err(Diag::new(
                            "E0102",
                            format!("operator `{op}` does not accept {x:?}"),
                            0,
                        ));
                    }
                }
            }
        })
    }
    fn expr(
        &mut self,
        e: &Expr,
        env: &std::collections::HashMap<String, (Ty, usize)>,
        ops: &mut Vec<Ins>,
    ) -> Result<(), Diag> {
        use Expr::*;
        match e {
            Num(n) => ops.push(Ins::Const(*n)),
            Bool(b) => ops.push(Ins::Bool(*b)),
            Var(n) => ops.push(Ins::Load(
                env.get(n)
                    .ok_or_else(|| Diag::new("E0101", format!("unknown variable `{n}`"), 0))?
                    .1,
            )),
            Array(v) => {
                for x in v {
                    self.expr(x, env, ops)?
                }
                ops.push(Ins::Array(v.len()))
            }
            Index(a, i) => {
                self.expr(a, env, ops)?;
                self.expr(i, env, ops)?;
                ops.push(Ins::Get)
            }
            Call(n, a) => {
                for x in a {
                    self.expr(x, env, ops)?
                }
                if n == "len" {
                    ops.push(Ins::Len)
                } else if n == "fill" {
                    ops.push(Ins::Fill)
                } else {
                    ops.push(Ins::Call(*self.names.get(n).unwrap(), a.len()))
                }
            }
            Unary(op, a) => {
                self.expr(a, env, ops)?;
                ops.push(if op == "-" { Ins::Neg } else { Ins::Not })
            }
            Bin(op, a, b) => {
                self.expr(a, env, ops)?;
                self.expr(b, env, ops)?;
                ops.push(match op.as_str() {
                    "+" => Ins::Add,
                    "-" => Ins::Sub,
                    "*" => Ins::Mul,
                    "/" => Ins::Div,
                    "%" => Ins::Mod,
                    "==" => Ins::Eq,
                    "!=" => Ins::Ne,
                    "<" => Ins::Lt,
                    ">" => Ins::Gt,
                    "<=" => Ins::Le,
                    ">=" => Ins::Ge,
                    "&&" => Ins::And,
                    _ => Ins::Or,
                })
            }
        }
        Ok(())
    }
    fn block(
        &mut self,
        ss: &[Stmt],
        env: &mut std::collections::HashMap<String, (Ty, usize)>,
        ops: &mut Vec<Ins>,
        next: &mut usize,
    ) -> Result<(), Diag> {
        for s in ss {
            match s {
                Stmt::Let(n, t, e) => {
                    let etenv = env.iter().map(|(n, (t, _))| (n.clone(), *t)).collect();
                    let got = self.ty_expr(e, &etenv)?;
                    if got != *t {
                        return Err(Diag::new(
                            "E0102",
                            format!("`{n}` declared {t:?} but initialized with {got:?}"),
                            0,
                        ));
                    }
                    self.expr(e, env, ops)?;
                    let slot = *next;
                    *next += 1;
                    env.insert(n.clone(), (*t, slot));
                    ops.push(Ins::Store(slot))
                }
                Stmt::Set(n, e) => {
                    let (t, slot) = *env
                        .get(n)
                        .ok_or_else(|| Diag::new("E0101", format!("unknown variable `{n}`"), 0))?;
                    let etenv = env.iter().map(|(n, (t, _))| (n.clone(), *t)).collect();
                    if self.ty_expr(e, &etenv)? != t {
                        return Err(Diag::new(
                            "E0102",
                            format!("assignment type mismatch for `{n}`"),
                            0,
                        ));
                    }
                    self.expr(e, env, ops)?;
                    ops.push(Ins::Store(slot))
                }
                Stmt::SetIndex(n, i, e) => {
                    if env.get(n).map(|x| x.0) != Some(Ty::Array) {
                        return Err(Diag::new("E0102", format!("`{n}` is not an array"), 0));
                    }
                    let etenv = env.iter().map(|(n, (t, _))| (n.clone(), *t)).collect();
                    if self.ty_expr(i, &etenv)? != Ty::Int || self.ty_expr(e, &etenv)? != Ty::Int {
                        return Err(Diag::new(
                            "E0102",
                            "array assignment needs an Int index and value",
                            0,
                        ));
                    }
                    self.expr(&Expr::Var(n.clone()), env, ops)?;
                    self.expr(i, env, ops)?;
                    self.expr(e, env, ops)?;
                    ops.push(Ins::Put)
                }
                Stmt::Expr(e) => {
                    self.expr(e, env, ops)?;
                    ops.push(Ins::Pop)
                }
                Stmt::Return(e) => {
                    let etenv = env.iter().map(|(n, (t, _))| (n.clone(), *t)).collect();
                    if self.ty_expr(e, &etenv)? != self.codes.last().unwrap().ret {
                        return Err(Diag::new("E0102", "return type mismatch", 0));
                    }
                    self.expr(e, env, ops)?;
                    ops.push(Ins::Ret)
                }
                Stmt::If(c, a, b) => {
                    let etenv = env.iter().map(|(n, (t, _))| (n.clone(), *t)).collect();
                    if self.ty_expr(c, &etenv)? != Ty::Bool {
                        return Err(Diag::new("E0102", "if condition must be Bool", 0));
                    }
                    self.expr(c, env, ops)?;
                    let jz = ops.len();
                    ops.push(Ins::Jz(0));
                    let mut then_env = env.clone();
                    self.block(a, &mut then_env, ops, next)?;
                    let j = ops.len();
                    ops.push(Ins::Jump(0));
                    let else_at = ops.len();
                    let mut else_env = env.clone();
                    self.block(b, &mut else_env, ops, next)?;
                    let end = ops.len();
                    ops[jz] = Ins::Jz(else_at);
                    ops[j] = Ins::Jump(end)
                }
                Stmt::While(c, b) => {
                    let start = ops.len();
                    let etenv = env.iter().map(|(n, (t, _))| (n.clone(), *t)).collect();
                    if self.ty_expr(c, &etenv)? != Ty::Bool {
                        return Err(Diag::new("E0102", "while condition must be Bool", 0));
                    }
                    self.expr(c, env, ops)?;
                    let jz = ops.len();
                    ops.push(Ins::Jz(0));
                    let mut loop_env = env.clone();
                    self.block(b, &mut loop_env, ops, next)?;
                    ops.push(Ins::Jump(start));
                    let end = ops.len();
                    ops[jz] = Ins::Jz(end)
                }
            }
        }
        Ok(())
    }
    fn compile(mut self) -> Result<Vec<Code>, Diag> {
        for (i, f) in self.funcs.clone().iter().enumerate() {
            let mut env = std::collections::HashMap::new();
            let mut next = 0;
            for (n, t) in &f.args {
                env.insert(n.clone(), (*t, next));
                next += 1
            }
            let mut c = Code {
                name: f.name.clone(),
                ret: f.ret,
                slots: 0,
                ops: vec![],
            };
            self.codes.push(c.clone());
            self.block(&f.body, &mut env, &mut c.ops, &mut next)?;
            if f.ret == Ty::Unit {
                c.ops.push(Ins::Ret)
            }
            c.slots = next;
            self.codes[i] = c
        }
        Ok(self.codes)
    }
}

fn pop_int(s: &mut Vec<Val>) -> Result<i64, Diag> {
    match s.pop() {
        Some(Val::Int(v)) => Ok(v),
        _ => Err(Diag::new("R0001", "expected Int on VM stack", 0)),
    }
}
fn pop_bool(s: &mut Vec<Val>) -> Result<bool, Diag> {
    match s.pop() {
        Some(Val::Bool(v)) => Ok(v),
        _ => Err(Diag::new("R0001", "expected Bool on VM stack", 0)),
    }
}
#[derive(Clone)]
struct Frame {
    fun: usize,
    ip: usize,
    locals: Vec<Val>,
    base: usize,
}
fn run(code: &[Code], args: Vec<Val>) -> Result<Val, Diag> {
    let main = code
        .iter()
        .position(|f| f.name == "main")
        .ok_or_else(|| Diag::new("E0200", "entry function `main` is missing", 0))?;
    let mut stack = vec![];
    let mut frames = vec![Frame {
        fun: main,
        ip: 0,
        locals: vec![Val::Unit; code[main].slots],
        base: 0,
    }];
    for (i, a) in args.into_iter().enumerate() {
        if i < frames[0].locals.len() {
            frames[0].locals[i] = a
        }
    }
    loop {
        let fr = frames
            .last_mut()
            .ok_or_else(|| Diag::new("R0002", "empty call stack", 0))?;
        let ins = code[fr.fun]
            .ops
            .get(fr.ip)
            .cloned()
            .ok_or_else(|| Diag::new("R0002", "instruction pointer out of range", 0))?;
        fr.ip += 1;
        match ins {
            Ins::Const(n) => stack.push(Val::Int(n)),
            Ins::Bool(b) => stack.push(Val::Bool(b)),
            Ins::Load(i) => stack.push(fr.locals[i].clone()),
            Ins::Store(i) => {
                fr.locals[i] = stack
                    .pop()
                    .ok_or_else(|| Diag::new("R0001", "stack underflow", 0))?
            }
            Ins::Array(n) => {
                let mut a = vec![0; n];
                for i in (0..n).rev() {
                    a[i] = pop_int(&mut stack)?
                }
                stack.push(Val::Arr(Rc::new(RefCell::new(a))))
            }
            Ins::Fill => {
                let value = pop_int(&mut stack)?;
                let n = pop_int(&mut stack)?;
                if !(0..=1_000_000).contains(&n) {
                    return Err(Diag::new("R0005", "invalid array length", 0));
                }
                stack.push(Val::Arr(Rc::new(RefCell::new(vec![value; n as usize]))));
            }
            Ins::Get => {
                let i = pop_int(&mut stack)?;
                let a = stack.pop();
                if let Some(Val::Arr(a)) = a {
                    let a = a.borrow();
                    stack.push(Val::Int(*a.get(i as usize).ok_or_else(|| {
                        Diag::new(
                            "R0003",
                            format!("array index {i} out of bounds (len {})", a.len()),
                            0,
                        )
                    })?))
                } else {
                    return Err(Diag::new("R0001", "indexing non-array", 0));
                }
            }
            Ins::Put => {
                let v = pop_int(&mut stack)?;
                let i = pop_int(&mut stack)?;
                if let Some(Val::Arr(a)) = stack.last() {
                    let mut a = a.borrow_mut();
                    let n = a.len();
                    *a.get_mut(i as usize).ok_or_else(|| {
                        Diag::new(
                            "R0003",
                            format!("array index {i} out of bounds (len {n})",),
                            0,
                        )
                    })? = v
                } else {
                    return Err(Diag::new("R0001", "index assignment to non-array", 0));
                }
            }
            Ins::Pop => {
                stack.pop();
            }
            Ins::Add
            | Ins::Sub
            | Ins::Mul
            | Ins::Div
            | Ins::Mod
            | Ins::Lt
            | Ins::Gt
            | Ins::Le
            | Ins::Ge => {
                let b = pop_int(&mut stack)?;
                let a = pop_int(&mut stack)?;
                let v =
                    match ins {
                        Ins::Add => Val::Int(a + b),
                        Ins::Sub => Val::Int(a - b),
                        Ins::Mul => Val::Int(a * b),
                        Ins::Div => Val::Int(a.checked_div(b).ok_or_else(|| {
                            Diag::new("R0004", "division by zero or overflow", 0)
                        })?),
                        Ins::Mod => Val::Int(a.checked_rem(b).ok_or_else(|| {
                            Diag::new("R0004", "remainder by zero or overflow", 0)
                        })?),
                        Ins::Lt => Val::Bool(a < b),
                        Ins::Gt => Val::Bool(a > b),
                        Ins::Le => Val::Bool(a <= b),
                        _ => Val::Bool(a >= b),
                    };
                stack.push(v)
            }
            Ins::Eq | Ins::Ne => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                let eq = match (&a, &b) {
                    (Val::Int(a), Val::Int(b)) => a == b,
                    (Val::Bool(a), Val::Bool(b)) => a == b,
                    _ => false,
                };
                stack.push(Val::Bool(if matches!(ins, Ins::Ne) { !eq } else { eq }))
            }
            Ins::And | Ins::Or => {
                let b = pop_bool(&mut stack)?;
                let a = pop_bool(&mut stack)?;
                stack.push(Val::Bool(if matches!(ins, Ins::And) {
                    a && b
                } else {
                    a || b
                }))
            }
            Ins::Neg => {
                let a = pop_int(&mut stack)?;
                stack.push(Val::Int(-a))
            }
            Ins::Not => {
                let a = pop_bool(&mut stack)?;
                stack.push(Val::Bool(!a))
            }
            Ins::Len => {
                if let Some(Val::Arr(a)) = stack.pop() {
                    stack.push(Val::Int(a.borrow().len() as i64))
                } else {
                    return Err(Diag::new("R0001", "len argument is not array", 0));
                }
            }
            Ins::Jump(to) => frames.last_mut().unwrap().ip = to,
            Ins::Jz(to) => {
                if !pop_bool(&mut stack)? {
                    frames.last_mut().unwrap().ip = to
                }
            }
            Ins::Call(fi, n) => {
                let mut av = vec![Val::Unit; n];
                for i in (0..n).rev() {
                    av[i] = stack
                        .pop()
                        .ok_or_else(|| Diag::new("R0001", "not enough call arguments", 0))?
                }
                let base = stack.len();
                let mut locals = vec![Val::Unit; code[fi].slots];
                for (i, a) in av.into_iter().enumerate() {
                    locals[i] = a
                }
                frames.push(Frame {
                    fun: fi,
                    ip: 0,
                    locals,
                    base,
                })
            }
            Ins::Ret => {
                let result = if code[fr.fun].ret == Ty::Unit {
                    Val::Unit
                } else {
                    stack
                        .pop()
                        .ok_or_else(|| Diag::new("R0001", "missing return value", 0))?
                };
                let done = frames.pop().unwrap();
                stack.truncate(done.base);
                if frames.is_empty() {
                    return Ok(result);
                }
                stack.push(result)
            }
        }
    }
}

fn error_json(d: Diag) -> String {
    format!(
        "{{\"ok\":false,\"error\":{{\"code\":\"{}\",\"message\":\"{}\",\"offset\":{}}}}}",
        d.code,
        d.message.replace('\\', "\\\\").replace('"', "\\\""),
        d.at
    )
}
fn main() {
    let mut a = env::args().skip(1);
    let path = a.next().unwrap_or_else(|| {
        eprintln!("usage: slop <file.slop>");
        process::exit(2)
    });
    let src = fs::read_to_string(&path).unwrap_or_else(|e| {
        eprintln!(
            "{{\"ok\":false,\"error\":{{\"code\":\"E0000\",\"message\":\"{}\"}}}}",
            e
        );
        process::exit(2)
    });
    let res = (|| {
        let t = lex(&src)?;
        let fs = Parser { t, p: 0 }.program()?;
        let names = fs
            .iter()
            .enumerate()
            .map(|(i, f)| (f.name.clone(), i))
            .collect();
        if fs.iter().filter(|f| f.name == "main").count() != 1 {
            return Err(Diag::new(
                "E0200",
                "program must define exactly one `main`",
                0,
            ));
        }
        if fs.iter().find(|f| f.name == "main").unwrap().args.len() != 0 {
            return Err(Diag::new(
                "E0201",
                "entry function `main` must take no arguments",
                0,
            ));
        }
        let codes = Compiler {
            funcs: fs,
            codes: vec![],
            names,
        }
        .compile()?;
        run(&codes, vec![])
    })();
    match res {
        Ok(Val::Int(n)) => println!("{{\"ok\":true,\"value\":{n}}}"),
        Ok(Val::Bool(b)) => println!("{{\"ok\":true,\"value\":{b}}}"),
        Ok(Val::Arr(v)) => println!("{{\"ok\":true,\"value\":{:?}}}", v.borrow()),
        Ok(Val::Unit) => println!("{{\"ok\":true,\"value\":null}}"),
        Err(d) => {
            eprintln!("{}", error_json(d));
            process::exit(1)
        }
    }
}
