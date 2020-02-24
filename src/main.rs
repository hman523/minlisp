extern crate colored;
extern crate rustyline;

use colored::*;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::collections::HashMap;
use std::collections::LinkedList;
use std::sync::Arc;

type Tokens = Vec<String>;

fn main() {
    println!("minlisp interpreter");
    repl();
}

#[derive(Debug, Clone)]
struct Memory {
    vars: HashMap<String, Expr>,
    call_stack: LinkedList<HashMap<String, Expr>>,
}

impl Memory {
    /// new function
    /// This function returns a memory object with the built in values and
    /// functions
    pub fn new() -> Memory {
        Memory {
            vars: Memory::default_env(),
            call_stack: {
                let mut c = LinkedList::new();
                let hm = HashMap::new();
                c.push_front(hm);
                c
            },
        }
    }

    pub fn enter_fn(&mut self) {
        let new_scope = HashMap::new();
        self.call_stack.push_front(new_scope);
    }

    pub fn exit_fn(&mut self) {
        self.call_stack.pop_front();
    }

    /// get function
    /// returns either the value or a not a variable error
    pub fn get(&self, s: &String) -> Option<Expr> {
        for i in self.call_stack.clone() {
            if i.contains_key(s) {
                return match i.get(s) {
                    Some(v) => Some(v.clone()),
                    None => None,
                };
            }
        }
        match self.vars.get(s) {
            Some(v) => Some(v.clone()),
            None => None,
        }
    }

    /// insert function
    /// returns either an empty tuple on success or an error if redefing
    /// a variable
    pub fn insert(&mut self, s: &str, e: Expr) -> Result<(), Error> {
        let exists = self
            .call_stack
            .front_mut()
            .unwrap()
            .insert(s.to_string(), e);
        match exists {
            Some(_) => Err(Error::RedefiningVar(s.to_string())),
            None => Ok(()),
        }
    }

    fn math_fn<F>(name: String, lst: LinkedList<Expr>, f: F) -> Result<Expr, Error>
    where
        F: Fn(f64, f64) -> f64,
    {
        let mut list = lst;
        let check = arity_type_check(
            name.to_string(),
            list.clone(),
            vec!["Num".to_string(), "Num".to_string()],
        );
        if let Err(err) = check {
            return Err(err);
        }
        if let Expr::Num(a) = list.pop_front().unwrap() {
            if let Expr::Num(b) = list.pop_front().unwrap() {
                return Ok(Expr::Num(f(a, b)));
            }
        }
        panic!("Error in {} function", name);
    }

    fn comp_fn<F>(name: String, lst: LinkedList<Expr>, f: F) -> Result<Expr, Error>
    where
        F: Fn(f64, f64) -> bool,
    {
        let mut list = lst;
        let check = arity_type_check(
            name.to_string(),
            list.clone(),
            vec!["Num".to_string(), "Num".to_string()],
        );
        if let Err(err) = check {
            return Err(err);
        }
        if let Expr::Num(a) = list.pop_front().unwrap() {
            if let Expr::Num(b) = list.pop_front().unwrap() {
                return Ok(Expr::Bool(f(a, b)));
            }
        }
        panic!("Error in {} function", name);
    }

    pub fn default_env() -> HashMap<String, Expr> {
        let mut values = HashMap::new();
        values.insert(String::from("PI"), Expr::Num(std::f64::consts::PI));
        // Add Function
        values.insert(
            String::from("cons"),
            Expr::Func(|lst| {
                let mut lst = lst.clone();
                if lst.len() != 2 {
                    return Err(Error::ArityMismatch(String::from("cons"), lst.len(), 2));
                }
                let first = lst.pop_front().unwrap();
                let second = lst.pop_front().unwrap();
                if let Expr::List(mut l) = second {
                    l.push_front(first);
                    return Ok(Expr::List(l));
                } else {
                    return Err(Error::TypeMismatch(
                        String::from("cons"),
                        second,
                        "List".to_string(),
                        2,
                    ));
                }
            }),
        );
        values.insert(
            String::from("car"),
            Expr::Func(|lst| {
                let mut lst = lst.clone();
                if lst.len() != 1 {
                    return Err(Error::ArityMismatch("car".to_string(), lst.len(), 1));
                }
                let val = lst.pop_front().unwrap();
                if let Expr::List(mut l) = val {
                    if l.is_empty() {
                        return Err(Error::ContractViolation(
                            String::from("car"),
                            String::from("expected parameter to be a list of at least size 1"),
                        ));
                    }
                    return Ok(l.pop_front().unwrap());
                } else {
                    return Err(Error::TypeMismatch(
                        "car".to_string(),
                        val,
                        "List".to_string(),
                        1,
                    ));
                }
            }),
        );
        values.insert(
            String::from("cdr"),
            Expr::Func(|lst| {
                let mut lst = lst.clone();
                if lst.len() != 1 {
                    return Err(Error::ArityMismatch("cdr".to_string(), lst.len(), 1));
                }
                let val = lst.pop_front().unwrap();
                if let Expr::List(mut l) = val {
                    if l.is_empty() {
                        return Err(Error::ContractViolation(
                            String::from("cdr"),
                            String::from("expected parameter to be list of at least size 1"),
                        ));
                    }
                    l.pop_front();
                    return Ok(Expr::List(l));
                } else {
                    return Err(Error::TypeMismatch(
                        "cdr".to_string(),
                        val,
                        "List".to_string(),
                        1,
                    ));
                }
            }),
        );
        values.insert(
            String::from("null?"),
            Expr::Func(|lst| {
                let mut lst = lst.clone();
                if lst.len() != 1 {
                    return Err(Error::ArityMismatch(String::from("null?"), lst.len(), 1));
                }
                let val = lst.pop_front().unwrap();
                if let Expr::List(l) = val {
                    return Ok(Expr::Bool(l.is_empty()));
                } else {
                    return Ok(Expr::Bool(false));
                }
            }),
        );
        values.insert(
            String::from("+"),
            Expr::Func(|lst| Memory::math_fn(String::from("+"), lst.clone(), |a, b| a + b)),
        );
        values.insert(
            String::from("-"),
            Expr::Func(|lst| Memory::math_fn(String::from("-"), lst.clone(), |a, b| a - b)),
        );
        values.insert(
            String::from("*"),
            Expr::Func(|lst| Memory::math_fn(String::from("*"), lst.clone(), |a, b| a * b)),
        );
        values.insert(
            String::from("/"),
            Expr::Func(|lst| Memory::math_fn(String::from("/"), lst.clone(), |a, b| a / b)),
        );
        values.insert(
            String::from("expt"),
            Expr::Func(|lst| Memory::math_fn(String::from("expt"), lst.clone(), |a, b| a.powf(b))),
        );
        values.insert(
            String::from("="),
            Expr::Func(|lst| {
                Memory::comp_fn(String::from("="), lst.clone(), |a, b| {
                    ((a - b).abs() < std::f64::EPSILON)
                })
            }),
        );
        values.insert(
            String::from("/="),
            Expr::Func(|lst| {
                Memory::comp_fn(String::from("/="), lst.clone(), |a, b| {
                    (a - b).abs() > std::f64::EPSILON
                })
            }),
        );
        values.insert(
            String::from("<"),
            Expr::Func(|lst| Memory::comp_fn(String::from("<"), lst.clone(), |a, b| a < b)),
        );
        values.insert(
            String::from(">"),
            Expr::Func(|lst| Memory::comp_fn(String::from(">"), lst.clone(), |a, b| a > b)),
        );
        values.insert(
            String::from(">="),
            Expr::Func(|lst| Memory::comp_fn(String::from(">="), lst.clone(), |a, b| a >= b)),
        );
        values.insert(
            String::from("<="),
            Expr::Func(|lst| Memory::comp_fn(String::from("<="), lst.clone(), |a, b| a <= b)),
        );
        values
    }
}

fn arity_type_check(name: String, list: LinkedList<Expr>, types: Vec<String>) -> Result<(), Error> {
    if list.len() != types.len() {
        return Err(Error::ArityMismatch(name, list.len(), types.len()));
    }
    for (i, val) in list.iter().enumerate() {
        if get_type_name(val.clone()) != *types.get(i).unwrap() {
            return Err(Error::TypeMismatch(
                name,
                val.clone(),
                (*types.get(i).unwrap()).to_string(),
                i + 1,
            ));
        }
    }
    Ok(())
}

/// Expr Enum
/// Essentially, these are all the possible types this lisp can have
/// Their name should be easy to determine what they do
#[derive(Clone)]
enum Expr {
    Var(String),
    Str(String),
    Bool(bool),
    Num(f64),
    Func(fn(&LinkedList<Expr>) -> Result<Expr, Error>),
    Lambda(LambdaExpr),
    List(LinkedList<Expr>),
    Lines(LinkedList<Expr>),
}

impl std::cmp::PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match (self.clone(), other.clone()) {
            (Expr::Var(a), Expr::Var(b)) => a == b,
            (Expr::Str(a), Expr::Str(b)) => a == b,
            (Expr::Bool(a), Expr::Bool(b)) => a == b,
            (Expr::Num(a), Expr::Num(b)) => a == b,
            (Expr::Func(_), Expr::Func(_)) => unimplemented!(),
            (Expr::Lambda(a), Expr::Lambda(b)) => a == b,
            (Expr::List(a), Expr::List(b)) => a == b,
            (Expr::Lines(a), Expr::Lines(b)) => a == b,
            _ => false,
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.clone() {
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Str(s) => write!(f, "{}", s),
            Expr::Bool(b) => write!(f, "#{}", (if b { "t" } else { "f" })),
            Expr::Num(n) => write!(f, "{}", n),
            Expr::Func(_) => Err(std::fmt::Error),
            Expr::Lambda(_) => Err(std::fmt::Error),
            Expr::List(l) => write!(
                f,
                "{}",
                format!("{:?}", l)
                    .replace("[", "(")
                    .replace("]", ")")
                    .replace(",", "")
            ),
            Expr::Lines(l) => write!(f, "{:?}", l),
        }
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Expr::Func(_) => write!(f, "function"),
            Expr::Lambda(l) => write!(f, "{:?}", l),
            _ => write!(f, "{}", self),
        }
    }
}

fn get_type_name(e: Expr) -> String {
    match e {
        Expr::Var(_) => "Variable",
        Expr::Str(_) => "String",
        Expr::Bool(_) => "Bool",
        Expr::Num(_) => "Num",
        Expr::Func(_) => "Function",
        Expr::Lambda(_) => "Lambda",
        Expr::List(_) => "List",
        Expr::Lines(_) => "Lines",
    }
    .to_string()
}

#[derive(Clone, Debug, PartialEq)]
struct LambdaExpr {
    params: LinkedList<String>,
    body: Arc<Expr>,
}

impl LambdaExpr {
    pub fn new(par: LinkedList<String>, bod: Expr) -> LambdaExpr {
        LambdaExpr {
            params: par,
            body: Arc::new(bod),
        }
    }
}

/// Error Enum
/// This enum is used for reporting errors
/// Some contain a string for the variable part of the error
#[derive(Clone, Debug, PartialEq)]
enum Error {
    //fn name, given variable, expected type, position
    TypeMismatch(String, Expr, String, usize),
    //fn name
    NotAProcedure(String),
    NotAVariable(String),
    RedefiningVar(String),
    //fn name, given number of params, expected number
    ArityMismatch(String, usize, usize),
    CloseParenMissing(),
    OpenParenMissing(),
    CannotPrint(String),
    NoFuncGiven(),
    EvalCalledOnNothing(),
    ParameterRepeats(),
    //fn name, what the parameter didn't do
    ContractViolation(String, String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.clone() {
            Error::TypeMismatch(func, given, expected, pos) => write!(
                f,
                "Type error in function {}\n\
                 \tExpected type: {}\n\
                 \tGiven type: {}\n\
                 \tIn parameter number {}",
                func,
                expected,
                get_type_name(given),
                pos
            ),
            Error::NotAProcedure(func) => write!(f, "Unable to call {}, function not found", func),
            Error::NotAVariable(name) => write!(f, "Variable {} does not exist", name),
            Error::RedefiningVar(name) => write!(f, "Cannot redeclare variable {}", name),
            Error::ArityMismatch(func, given, expected) => write!(
                f,
                "Arity mismatch error in {}\n\
                 \tGiven parameters: {}\n\
                 \tExpected parameters: {}",
                func, given, expected
            ),
            Error::CloseParenMissing() => {
                write!(f, "Parenthesis mismatch: too many open parenthesis")
            }
            Error::OpenParenMissing() => {
                write!(f, "Parenthesis mismatch: too many close parenthesis")
            }

            Error::CannotPrint(typeofval) => write!(f, "Cannot print type {}", typeofval),
            Error::NoFuncGiven() => write!(f, "Cannot call eval on empty string"),
            Error::EvalCalledOnNothing() => write!(f, "Cannot call eval on nothing"),
            Error::ParameterRepeats() => write!(f, "Cannot repeat parameter name in lambda"),
            Error::ContractViolation(name, reason) => write!(
                f,
                "Contract violation\n\
                 \tIn function: {}\n\
                 \tReason: {}",
                name, reason
            ),
        }
    }
}
/// Tokenize function
/// This function just gets an input (String) and returns
/// a tokenized result (a vector of strings, either a token or parenthesis)
fn tokenize(code: String) -> Result<Tokens, Error> {
    let mut v = Vec::new();
    let mut cur = String::new();
    let mut in_quotes = false;
    let mut last_escaped = false;
    let mut paren_count: i32 = 0;
    for i in code.chars() {
        if i.is_whitespace() && !in_quotes {
            if !cur.is_empty() {
                v.push(cur.clone());
                cur = String::new();
            } else {
                //do nothing
            }
        } else if i == '(' && !in_quotes {
            if !cur.is_empty() {
                v.push(cur.clone());
                cur = String::new()
            }
            v.push(String::from("("));
            paren_count += 1;
        } else if i == ')' && !in_quotes {
            if !cur.is_empty() {
                v.push(cur.clone());
                cur = String::new();
            }
            v.push(String::from(")"));
            paren_count -= 1;
            if paren_count < 0 {
                return Err(Error::OpenParenMissing());
            }
        } else if i == '"' && !last_escaped {
            if in_quotes {
                cur.push('"');
                v.push(cur.clone());
                cur = String::new();
            } else {
                cur.push('"');
            }
            in_quotes = !in_quotes;
            continue;
        } else if i == '"' && last_escaped {
            cur.push('"');
        } else if i == '\\' {
            if last_escaped {
                cur.push('\\');
            } else {
                last_escaped = true;
                continue;
            }
        } else {
            cur.push(i);
        }
        last_escaped = false;
    }
    if !cur.is_empty() {
        v.push(cur);
    }
    match paren_count {
        x if x > 0 => Err(Error::CloseParenMissing()),
        x if x < 0 => Err(Error::OpenParenMissing()),
        _ => Ok(v),
    }
}

/// is_in_quotes function
/// Checks if the string starts and ends with a quotes character
fn is_in_quotes(s: &String) -> bool {
    s.len() >= 2
        && (s.clone().into_bytes()[0] as char) == '"'
        && (s.clone().into_bytes()[s.len() - 1] as char) == '"'
}

/// parse_expr function
/// Function converts a string into either a string, variable, boolean, or number
fn parse_expr(token: String) -> Expr {
    if token == "#t" {
        Expr::Bool(true)
    } else if token == "#f" {
        Expr::Bool(false)
    } else if is_in_quotes(&token) {
        Expr::Str(token[1..(token.len() - 1)].to_string())
    } else {
        let parsed = token.parse::<f64>();
        match parsed {
            Ok(x) => Expr::Num(x),
            Err(_) => Expr::Var(token),
        }
    }
}

/// read_seq function
/// This is heavily inspirired by Stepan Parunashvili's
/// implementation of the rust lisp interpreter
fn read_seq(tokens: Tokens) -> Result<(Expr, Tokens), Error> {
    let mut result: LinkedList<Expr> = LinkedList::new();
    let mut ts = tokens;
    loop {
        let (next, rest) = ts.split_first().ok_or(Error::OpenParenMissing())?;
        if next == ")" {
            return Ok((Expr::List(result), rest.to_vec()));
        }
        let (expr, new_ts) = parseh(ts)?;
        result.push_back(expr);
        ts = new_ts;
    }
}

fn parseh(tokens: Tokens) -> Result<(Expr, Tokens), Error> {
    let (token, tail) = tokens.split_first().unwrap();
    match token.as_ref() {
        "(" => read_seq(tail.to_vec()),
        ")" => Err(Error::OpenParenMissing()),
        _ => Ok((parse_expr(token.to_string()), tail.to_vec())),
    }
}

/// parse function
/// This function converts tokens into a valid vector of expressions
/// Each element in the vector is another line in the code
/// The code would be parsed from `(print "hi") (print "bye")` to a
/// Expr::Lines
/// ```vec![Expr::List(vec![Expr::Var("print"), Expr::Str("hi")]),
/// Expr::List(Vec![Expr::var("print"), Expr::Str("bye")])]```
fn parse(tokens: Tokens) -> Result<Expr, Error> {
    let mut code: LinkedList<Expr> = LinkedList::new();
    let mut curr_tokens = tokens;
    loop {
        let current_line: Result<(Expr, Vec<String>), Error> = parseh(curr_tokens);
        let mut empty = false;
        if let Ok((x, y)) = current_line {
            code.push_back(x);
            if y.is_empty() {
                empty = true;
            }
            curr_tokens = y;
        } else {
            return Err(current_line.unwrap_err());
        }

        if empty {
            break;
        }
    }
    Ok(Expr::Lines(code))
}

fn eval(expression: Expr, state: Memory) -> Result<(Expr, Memory), (Error, Memory)> {
    let mut current_state = state;
    match expression {
        Expr::Lines(lines) => {
            let mut result = Err((Error::EvalCalledOnNothing(), current_state.clone()));
            for (count, i) in lines.iter().enumerate() {
                result = eval(i.clone(), current_state);
                if result.is_ok() {
                    let (_, m) = result.clone().unwrap();
                    current_state = m;
                } else {
                    return result;
                }
                if count == lines.len() - 1 {
                    return result;
                }
            }
            result
        }
        Expr::List(mut list) => {
            let func = list.pop_front();
            if func.is_none() {
                return Err((Error::NoFuncGiven(), current_state));
            }
            let func = func.unwrap();
            if func == Expr::Var("if".to_string()) {
                eval_if(current_state, list)
            } else if func == Expr::Var("set".to_string()) {
                eval_set(current_state, list)
            } else if func == Expr::Var("lambda".to_string()) {
                eval_lambda(current_state, list)
            } else if func == Expr::Var("quote".to_string()) {
                if list.len() == 1 {
                    return Ok((list.pop_front().unwrap(), current_state));
                }
                return Err((
                    Error::ArityMismatch("quote".to_string(), list.len(), 1),
                    current_state,
                ));
            } else {
                apply(current_state, func, list)
            }
        }
        Expr::Num(n) => Ok((Expr::Num(n), current_state)),
        Expr::Bool(b) => Ok((Expr::Bool(b), current_state)),
        Expr::Str(s) => Ok((Expr::Str(s), current_state)),
        Expr::Func(f) => Ok((Expr::Func(f), current_state)),
        Expr::Lambda(l) => Ok((Expr::Lambda(l), current_state)),
        Expr::Var(v) => match current_state.get(&v) {
            Some(val) => Ok((val, current_state)),
            None => Err((Error::NotAVariable(v), current_state)),
        },
    }
}

fn eval_lambda(state: Memory, list: LinkedList<Expr>) -> Result<(Expr, Memory), (Error, Memory)> {
    let mut list = list;
    if list.len() != 2 {
        return Err((
            Error::ArityMismatch(String::from("lambda"), list.len(), 2),
            state,
        ));
    }
    let params = list.pop_front().unwrap();
    if let Expr::List(parameters) = params.clone() {
        let bod = list.pop_front().unwrap();
        let mut parameters_as_strings = LinkedList::new();
        for i in parameters {
            if let Expr::Var(var) = i {
                parameters_as_strings.push_back(var);
            } else {
                return Err((
                    Error::TypeMismatch(String::from("lambda"), i, "Var".to_string(), 1),
                    state,
                ));
            }
        }
        Ok((
            Expr::Lambda(LambdaExpr::new(parameters_as_strings, bod)),
            state,
        ))
    } else {
        Err((
            Error::TypeMismatch(String::from("lambda"), params, "List".to_string(), 1),
            state,
        ))
    }
}

fn eval_set(state: Memory, list: LinkedList<Expr>) -> Result<(Expr, Memory), (Error, Memory)> {
    let mut new_state = state;
    let mut list = list;
    if list.len() != 2 {
        return Err((
            Error::ArityMismatch(String::from("set"), list.len(), 2),
            new_state,
        ));
    }
    let e = list.pop_front().unwrap();
    if let Expr::Var(symbol) = e {
        let expr = list.pop_front().unwrap();
        if let Err(err) = new_state.insert(&symbol, expr.clone()) {
            return Err((err, new_state));
        }
        Ok((expr, new_state))
    } else {
        Err((
            Error::TypeMismatch("set".to_string(), e, "Var".to_string(), 1),
            new_state,
        ))
    }
}

fn eval_if(state: Memory, list: LinkedList<Expr>) -> Result<(Expr, Memory), (Error, Memory)> {
    let new_state = state;
    let mut list = list;
    if list.len() != 3 {
        return Err((
            Error::ArityMismatch(String::from("if"), list.len(), 3),
            new_state,
        ));
    }
    let e = list.pop_front().unwrap();
    let cond = eval_to_bool(e.clone(), new_state.clone());
    if cond.is_none() {
        return Err((
            Error::TypeMismatch("if".to_string(), e, "Bool".to_string(), 1),
            new_state,
        ));
    }
    let (cond, new_state) = cond.unwrap();
    let first = list.pop_front().unwrap();
    let second = list.pop_front().unwrap();
    eval(if cond { first } else { second }, new_state)
}

fn eval_to_bool(e: Expr, state: Memory) -> Option<(bool, Memory)> {
    match e {
        Expr::Bool(b) => Some((b, state)),
        Expr::List(_) | Expr::Var(_) => {
            let res = eval(e, state);
            if res.is_err() {
                return None;
            }
            let (result, mem) = res.unwrap();
            eval_to_bool(result, mem)
        }
        _ => None,
    }
}

fn apply(
    state: Memory,
    f: Expr,
    params: LinkedList<Expr>,
) -> Result<(Expr, Memory), (Error, Memory)> {
    let mut new_state = state;
    let original_params = params.clone();
    let mut params = LinkedList::new();
    for i in original_params {
        let res = eval(i, new_state);
        if res.is_err() {
            return Err(res.unwrap_err());
        }
        let res = res.unwrap();
        new_state = res.1;
        params.push_back(res.0);
    }
    let params = Expr::List(params);
    let params: LinkedList<Expr> = match params {
        Expr::List(l) => l,
        _ => LinkedList::new(),
    };
    let res = eval(f, new_state);
    if res.is_err() {
        return Err(res.unwrap_err());
    }
    let (f, new_state) = res.unwrap();
    match f {
        Expr::Func(func) => {
            let res = func(&params);
            match res {
                Ok(e) => Ok((e, new_state)),
                Err(e) => Err((e, new_state)),
            }
        }
        Expr::Lambda(lambda) => execute_lambda(lambda, new_state, &params),
        Expr::List(l) => {
            let res = eval(Expr::List(l), new_state);
            if res.is_err() {
                return Err(res.unwrap_err());
            }
            let (f, state) = res.unwrap();
            apply(state, f, params)
        }
        Expr::Var(var) => {
            let func = new_state.get(&var);
            if func == None {
                return Err((Error::NotAProcedure(var), new_state));
            }
            let func = func.unwrap();
            match func {
                Expr::List(l) => {
                    let res = eval(Expr::List(l), new_state);
                    if res.is_err() {
                        return Err(res.unwrap_err());
                    }
                    let (f, state) = res.unwrap();
                    apply(state, f, params)
                }
                Expr::Func(function) => match function(&params) {
                    Ok(e) => Ok((e, new_state)),
                    Err(e) => Err((e, new_state)),
                },
                Expr::Lambda(lambda) => execute_lambda(lambda, new_state, &params),
                _ => Err((Error::NotAProcedure(func.to_string()), new_state)),
            }
        }
        _ => Err((Error::NotAProcedure(f.to_string()), new_state)),
    }
}

fn execute_lambda(
    lambda: LambdaExpr,
    state: Memory,
    params: &LinkedList<Expr>,
) -> Result<(Expr, Memory), (Error, Memory)> {
    let mut new_state = state.clone();
    let mut params = params.clone();
    //First enter new scope
    if lambda.params.len() != params.len() {
        return Err((
            Error::ArityMismatch(String::from("lambda"), params.len(), lambda.params.len()),
            state,
        ));
    }
    new_state.enter_fn();
    //Now evaluate and populate the parameters
    for item in lambda.params {
        if new_state
            .insert(&item, params.pop_front().unwrap())
            .is_err()
        {
            return Err((Error::ParameterRepeats(), state));
        }
    }
    let res = eval(Arc::try_unwrap(lambda.body).unwrap(), new_state);
    if res.is_err() {
        let (err, mut state) = res.unwrap_err();
        state.exit_fn();
        return Err((err, state));
    }
    let (result, mut new_state) = res.unwrap();
    new_state.exit_fn();
    Ok((result, new_state))
}

fn print(x: Result<(Expr, Memory), (Error, Memory)>) {
    match x {
        Ok((e, _m)) => match e {
            Expr::Func(_) => println!(
                "{}",
                Error::CannotPrint(String::from("Function"))
                    .to_string()
                    .red()
                    .bold()
            ),
            Expr::Lambda(_) => println!(
                "{}",
                Error::CannotPrint(String::from("Lambda"))
                    .to_string()
                    .red()
                    .bold()
            ),
            _ => println!("{}", e),
        },
        Err((e, _)) => println!("{}", e.to_string().red().bold()),
    }
}

fn repl() {
    let mut mem = Memory::new();
    let mut rl = Editor::<()>::new();
    if rl.load_history(".history.txt").is_err() {
        println!(
            "{}",
            "No existing history, stating a new one in .history.txt".yellow()
        );
    }
    loop {
        //read
        let readline = rl.readline("> ");
        let mut input = String::new();
        print!("{}", &input); //To stop a warning that isn't real
        match readline {
            Ok(r) => {
                input = r;
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                println!("{}", "Goodbye!".green().bold());
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        };
        rl.add_history_entry(input.as_str());
        rl.save_history(".history.txt").unwrap();
        //tokenize
        let tokens = tokenize(input);
        match tokens {
            Ok(ts) => {
                //parse
                let parsed = parse(ts);
                match parsed {
                    Ok(expr) => {
                        //eval
                        let evaled = eval(expr, mem.clone());
                        //print
                        if evaled.is_ok() {
                            mem = evaled.clone().unwrap().1;
                        }
                        print(evaled);
                    }
                    Err(e) => println!("{}", e.to_string().red().bold()),
                }
            }
            Err(e) => println!("{}", e.to_string().red().bold()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_fn() {
        let empty: Vec<String> = Vec::new();
        assert_eq!(
            tokenize(String::from("1 2 3")).unwrap(),
            vec!["1", "2", "3"]
        );
        assert_eq!(
            tokenize(String::from("(1 2 3)")).unwrap(),
            vec!["(", "1", "2", "3", ")"]
        );
        assert_eq!(
            tokenize(String::from("( 1 2 )")).unwrap(),
            vec!["(", "1", "2", ")"]
        );
        assert_eq!(tokenize(String::from("     1")).unwrap(), vec!["1"]);
        assert_eq!(tokenize(String::from("")).unwrap(), empty);
        assert_eq!(tokenize(String::from("1       ")).unwrap(), vec!["1"]);
        assert_eq!(tokenize(String::from("\"1 2\"")).unwrap(), vec!["\"1 2\""]);
        assert_eq!(
            tokenize(String::from("\"12 2\"")).unwrap(),
            vec!["\"12 2\""]
        );
        assert_eq!(tokenize(String::from("\\\"")).unwrap(), vec!["\""]);
        assert_eq!(tokenize(String::from("\"((((")).unwrap(), vec!["\"(((("]);
        assert_eq!(
            tokenize(String::from("(())")).unwrap(),
            vec!["(", "(", ")", ")"]
        );
        assert_eq!(tokenize(String::from("(\n\n)")).unwrap(), vec!["(", ")"]);
        assert_eq!(
            tokenize(String::from("(\n(\n))")).unwrap(),
            vec!["(", "(", ")", ")"]
        );
        assert_eq!(tokenize(String::from("(\t\t)")).unwrap(), vec!["(", ")"]);
        assert_eq!(tokenize(String::from("(\r\r)")).unwrap(), vec!["(", ")"]);
    }

    #[test]
    fn tokenize_error() {
        let open_error = Error::OpenParenMissing();
        let close_error = Error::CloseParenMissing();
        assert_eq!(tokenize(String::from("(1 2 3")).unwrap_err(), close_error);
        assert_eq!(tokenize(String::from("1 2 3)")).unwrap_err(), open_error);
        assert_eq!(tokenize(String::from(")")).unwrap_err(), open_error);
        assert_eq!(tokenize(String::from("(")).unwrap_err(), close_error);
        assert_eq!(tokenize(String::from("())")).unwrap_err(), open_error);
    }

    #[test]
    fn type_formating() {
        assert_eq!(Expr::Bool(true).to_string(), "#t");
        assert_eq!(Expr::Bool(false).to_string(), "#f");
        assert_eq!(Expr::Str("A".to_string()).to_string(), "A");
        assert_eq!(Expr::Num(-1.0).to_string(), "-1");
        assert_eq!(Expr::Num(0.5).to_string(), "0.5");
    }

    #[test]
    fn parse_expr_fn() {
        assert_eq!(parse_expr(String::from("#t")), Expr::Bool(true));
        assert_eq!(parse_expr(String::from("#f")), Expr::Bool(false));
        assert_eq!(parse_expr(String::from("0")), Expr::Num(0.0));
        assert_eq!(parse_expr(String::from("-1")), Expr::Num(-1.0));
        assert_eq!(parse_expr(String::from("-1.0")), Expr::Num(-1.0));
        assert_eq!(parse_expr(String::from("-1.0000")), Expr::Num(-1.0));
        assert_eq!(
            parse_expr("\"aaa\"".to_string()),
            Expr::Str(String::from("aaa"))
        );
    }

    #[test]
    fn parse_01() {
        let tokens = tokenize(String::from("(1 2 3)"));
        let tokens = match tokens {
            Ok(a) => a,
            Err(e) => panic!(e),
        };
        let result = parse(tokens);
        let result = match result {
            Ok(a) => a,
            Err(e) => panic!(e),
        };
        let mut list: LinkedList<Expr> = LinkedList::new();
        list.push_back(Expr::Num(1.0));
        list.push_back(Expr::Num(2.0));
        list.push_back(Expr::Num(3.0));
        let mut lines = LinkedList::new();
        lines.push_back(Expr::List(list));
        let expected = Expr::Lines(lines);
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_02() {
        let tokens = tokenize(String::from("(1) (2 3)"));
        let tokens = match tokens {
            Ok(a) => a,
            Err(e) => panic!(e),
        };
        let result = parse(tokens);
        let result = match result {
            Ok(a) => a,
            Err(e) => panic!(e),
        };
        let mut list: LinkedList<Expr> = LinkedList::new();
        list.push_back(Expr::Num(1.0));
        let first = list;
        list = LinkedList::new();
        list.push_back(Expr::Num(2.0));
        list.push_back(Expr::Num(3.0));
        let mut lines: LinkedList<Expr> = LinkedList::new();
        lines.push_back(Expr::List(first));
        lines.push_back(Expr::List(list));
        let expected = Expr::Lines(lines);
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_03() {
        let tokens = tokenize(String::from("((1) 2 3)"));
        let tokens = match tokens {
            Ok(a) => a,
            Err(e) => panic!(e),
        };
        let result = parse(tokens);
        let result = match result {
            Ok(a) => a,
            Err(e) => panic!(e),
        };
        let mut list: LinkedList<Expr> = LinkedList::new();
        list.push_back(Expr::Num(1.0));
        let inner = Expr::List(list);
        let mut list: LinkedList<Expr> = LinkedList::new();
        list.push_back(inner);
        list.push_back(Expr::Num(2.0));
        list.push_back(Expr::Num(3.0));
        let mut lines: LinkedList<Expr> = LinkedList::new();
        lines.push_back(Expr::List(list));
        let expected = Expr::Lines(lines);
        assert_eq!(result, expected);
    }

    #[test]
    fn memory_get_insert() {
        let mut mem = Memory::new();
        if mem.insert(&String::from("a"), Expr::Bool(true)).is_err() {
            panic!("Error should not happen on this insert");
        }
        assert_eq!(mem.get(&String::from("a")).unwrap(), Expr::Bool(true));
    }

    #[test]
    fn memory_insert_twice() {
        let mut mem = Memory::new();
        let first = mem.insert(&String::from("a"), Expr::Bool(true)).unwrap();
        let second = mem.insert(&String::from("a"), Expr::Bool(true));
        assert_eq!(first, ());
        assert_eq!(second, Err(Error::RedefiningVar(String::from("a"))));
    }

    fn eval_or_none(s: String) -> Option<Expr> {
        let state = Memory::new();
        let t = tokenize(s);
        if t.is_err() {
            println!("{:?}", t);
            return None;
        }
        let p = parse(t.unwrap());
        if p.is_err() {
            println!("{:?}", p);
            return None;
        }
        let e = eval(p.unwrap(), state);
        match e {
            Ok((val, _)) => Some(val),
            Err(e) => {
                println!("{:?}", e);
                None
            }
        }
    }

    #[test]
    fn eval_valid_add() {
        assert_eq!(
            Expr::Num(3.0),
            eval_or_none(String::from("(+ 1 2)")).unwrap()
        );
        assert_eq!(
            Expr::Num(3.0),
            eval_or_none(String::from("(+ 1 (+ 1 1))")).unwrap()
        );
    }

    #[test]
    fn eval_valid_math() {
        assert_eq!(
            Expr::Num(1.0),
            eval_or_none(String::from("(- 2 (* 1 1))")).unwrap()
        );
    }

    #[test]
    fn numeric_equality() {
        assert_eq!(
            Expr::Bool(true),
            eval_or_none(String::from("(= 1 1)")).unwrap()
        );
        assert_eq!(
            Expr::Bool(false),
            eval_or_none(String::from("(= 2 1)")).unwrap()
        );
    }

    #[test]
    fn numeric_comparison() {
        assert_eq!(
            Expr::Bool(true),
            eval_or_none(String::from("(/= 1 0)")).unwrap()
        );
        assert_eq!(
            Expr::Bool(false),
            eval_or_none(String::from("(/= 1 1)")).unwrap()
        );
        assert_eq!(
            Expr::Bool(true),
            eval_or_none(String::from("(< 0 1)")).unwrap()
        );
        assert_eq!(
            Expr::Bool(false),
            eval_or_none(String::from("(< 1 0)")).unwrap()
        );
        assert_eq!(
            Expr::Bool(true),
            eval_or_none(String::from("(> 1 0)")).unwrap()
        );
        assert_eq!(
            Expr::Bool(false),
            eval_or_none(String::from("(> 0 1)")).unwrap()
        );
        assert_eq!(
            Expr::Bool(true),
            eval_or_none(String::from("(>= 1 0)")).unwrap()
        );
        assert_eq!(
            Expr::Bool(false),
            eval_or_none(String::from("(>= 0 1)")).unwrap()
        );
        assert_eq!(
            Expr::Bool(true),
            eval_or_none(String::from("(<= 0 1)")).unwrap()
        );
        assert_eq!(
            Expr::Bool(false),
            eval_or_none(String::from("(<= 1 0)")).unwrap()
        );
    }

    #[test]
    fn if_bif() {
        assert_eq!(
            Expr::Num(1.0),
            eval_or_none(String::from("(if #t 1 2)")).unwrap()
        );
        assert_eq!(
            Expr::Num(2.0),
            eval_or_none(String::from("(if #f 1 2)")).unwrap()
        );
        assert_eq!(
            Expr::Num(2.0),
            eval_or_none(String::from("(+ 1 (if #t 1 2))")).unwrap()
        );
        assert_eq!(
            Expr::Str("Hello".to_string()),
            eval_or_none(String::from("(if #f 1 \"Hello\")")).unwrap()
        );
        assert_eq!(
            Expr::Num(1.0),
            eval_or_none(String::from("(if (= 1 1) 1 2)")).unwrap()
        );
        assert_eq!(
            Expr::Num(2.0),
            eval_or_none(String::from("(if (= 1 2) 1 2)")).unwrap()
        );
    }

    #[test]
    fn set_bif() {
        assert_eq!(
            Expr::Num(3.0),
            eval_or_none(String::from("(set a 3)")).unwrap()
        );
        assert_eq!(None, eval_or_none(String::from("(set a 3) (set a 4)")));
        assert_eq!(
            Expr::Bool(true),
            eval_or_none(String::from("(set a #t) a")).unwrap()
        );
    }

    #[test]
    fn lambda_identity() {
        //Identity lambda
        assert_eq!(
            Expr::Num(1.0),
            eval_or_none(String::from("((lambda (x) x) 1)")).unwrap()
        );
        assert_eq!(
            Expr::Num(1.0),
            eval_or_none(String::from("(set id (lambda (x) x))(id 1)")).unwrap()
        );
    }

    #[test]
    fn lambda_factorial() {
        assert_eq!(
            Expr::Num(1.0),
            eval_or_none(String::from(
                "(set fact (lambda (x) (if (< x 1) 1 (* x (fact (- x 1)))))) (fact 1)"
            ))
            .unwrap()
        );
        assert_eq!(
            Expr::Num(1.0),
            eval_or_none(String::from(
                "(set fact (lambda (x) (if (< x 1) 1 (* x (fact (- x 1)))))) (fact 0)"
            ))
            .unwrap()
        );
        assert_eq!(
            Expr::Num(6.0),
            eval_or_none(String::from(
                "(set fact (lambda (x) (if (< x 1) 1 (* x (fact (- x 1)))))) (fact 3)"
            ))
            .unwrap()
        );
        assert_eq!(
            Expr::Num(3628800.0),
            eval_or_none(String::from(
                "(set fact (lambda (x) (if (< x 1) 1 (* x (fact (- x 1)))))) (fact 10)"
            ))
            .unwrap()
        );
    }

    #[test]
    fn quote_bif() {
        assert_eq!(
            Expr::Bool(true),
            eval_or_none(String::from("(quote #t)")).unwrap()
        );
        assert_eq!(
            Expr::Num(1.0),
            eval_or_none(String::from("(quote 1)")).unwrap()
        );
        let mut l = LinkedList::new();
        for x in 1..4 {
            l.push_back(Expr::Num(f64::from(x)));
        }
        assert_eq!(
            Expr::List(l),
            eval_or_none(String::from("(quote (1 2 3))")).unwrap()
        );
    }

    #[test]
    fn cons_bif() {
        let mut l = LinkedList::new();
        for x in 1..4 {
            l.push_back(Expr::Num(f64::from(x)));
        }
        assert_eq!(
            Expr::List(l),
            eval_or_none(String::from("(cons 1 (quote (2 3)))")).unwrap()
        );
    }

    #[test]
    fn car_bif() {
        let mut l = LinkedList::new();
        for x in 1..4 {
            l.push_back(Expr::Num(f64::from(x)));
        }
        assert_eq!(
            Expr::List(l),
            eval_or_none(String::from("(car (quote ((1 2 3) 4 5 6)))")).unwrap()
        );
        assert_eq!(
            Expr::Bool(true),
            eval_or_none(String::from("(car (quote (#t #f #f)))")).unwrap()
        );
    }

    #[test]
    fn cdr_bif() {
        let mut l = LinkedList::new();
        for x in 1..4 {
            l.push_back(Expr::Num(f64::from(x)));
        }
        assert_eq!(
            Expr::List(l),
            eval_or_none(String::from("(cdr (quote (0 1 2 3)))")).unwrap()
        );
    }

    #[test]
    fn null_questionmark_bif() {
        assert_eq!(
            Expr::Bool(true),
            eval_or_none(String::from("(null? (quote ()))")).unwrap()
        );
        assert_eq!(
            Expr::Bool(false),
            eval_or_none(String::from("(null? #t)")).unwrap()
        );
        assert_eq!(
            Expr::Bool(false),
            eval_or_none(String::from("(null? 42)")).unwrap()
        );
    }

    #[test]
    fn create_and_test_len_fn() {
        //This test was created because of a failed live demo
        assert_eq!(
			Expr::Num(3.0),
			eval_or_none(String::from("(set len (lambda (lst) (if (null? lst) 0 (+ 1 (len (cdr lst)))))) (len (quote (1 2 3)))")).unwrap(),
		);
    }

    #[test]
    fn lambda_and_quote() {
        assert_eq!(
            Expr::Num(1.0),
            eval_or_none(String::from(
                "(set mycar (lambda (lst) (car lst))) (mycar (quote (1 2 3)))"
            ))
            .unwrap(),
        );
    }
}
