extern crate colored;

use colored::*;
use std::collections::HashMap;
use std::collections::LinkedList;
use std::io::Write;

type Tokens = Vec<String>;

fn main() {
    println!("minlisp interpreter");
    repl();
}

#[derive(Debug, Clone)]
struct Memory {
    vars: HashMap<String, Expr>,
}

impl Memory {
    /// new function
    /// This function returns a memory object with the built in values and
    /// functions
    pub fn new() -> Memory {
        Memory {
            vars: Memory::default_env(),
        }
    }

    /// get function
    /// returns either the value or a not a variable error
    pub fn get(&self, s: &String) -> Option<Expr> {
        match self.vars.get(s) {
            Some(v) => Some(v.clone()),
            None => None,
        }
    }

    pub fn default_env() -> HashMap<String, Expr> {
        let mut values = HashMap::new();
        values.insert(String::from("PI"), Expr::Num(std::f64::consts::PI));
        values.insert(
            String::from("+"),
            Expr::Func(|lst| -> Result<Expr, Error> {
                let mut list = lst.clone();
                if list.len() != 2 {
                    return Err(Error::ArityMismatch("+".to_string(), list.len(), 2));
                }
                if let Some(Expr::Num(a)) = list.pop_front() {
                    if let Some(Expr::Num(b)) = list.pop_front() {
                        return Ok(Expr::Num(a + b));
                    }
                    let b = list.pop_front().unwrap();
                    return Err(Error::TypeError("+".to_string(), b, "Num".to_string()));
                }
                let a = list.pop_front().unwrap();
                return Err(Error::TypeError("+".to_string(), a, "Num".to_string()));
            }),
        );
        values
    }

    /// insert function
    /// returns either an empty tuple on success or an error if redefing
    /// a variable
    pub fn insert(&mut self, s: &String, e: Expr) -> Result<(), Error> {
        let exists = self.vars.insert(s.clone(), e);
        match exists {
            Some(_) => Err(Error::RedefiningVar(s.clone())),
            None => Ok(()),
        }
    }
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
            Expr::List(l) => write!(f, "{:?}", l),
            Expr::Lines(l) => write!(f, "{:?}", l),
        }
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Expr::Func(fun) => write!(f, "function"),
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
        Expr::List(_) => "List",
        Expr::Lines(_) => "Lines",
    }
    .to_string()
}

/// Error Enum
/// This enum is used for reporting errors
/// Some contain a string for the variable part of the error
#[derive(Clone, Debug, PartialEq)]
enum Error {
    //fn name, given variable, expected type
    TypeError(String, Expr, String),
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
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.clone() {
            Error::TypeError(func, given, expected) => write!(
                f,
                "Type error in function {}\n\
                 \tExpected type:{}\n\
                 \tGiven type:{}",
                func,
                expected,
                get_type_name(given)
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
    let mut ts = tokens.clone();
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
        let mut current_line: Result<(Expr, Vec<String>), Error> = parseh(curr_tokens);
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

fn read() -> String {
    let mut input = String::new();
    print!("> ");
    std::io::stdout().flush().unwrap();
    std::io::stdin().read_line(&mut input).unwrap();
    return input;
}

fn eval(expression: Expr, state: Memory) -> Result<(Expr, Memory), (Error, Memory)> {
    let mut current_state = state.clone();
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
            return result;
        }
        Expr::List(mut list) => {
            let func = list.pop_front();
            if func.is_none() {
                return Err((Error::NoFuncGiven(), current_state));
            }
            let func = func.unwrap();
            return apply(current_state, func, list);
        }
        Expr::Num(n) => {
            return Ok((Expr::Num(n), current_state));
        }
        Expr::Bool(b) => {
            return Ok((Expr::Bool(b), current_state));
        }
        Expr::Str(s) => {
            return Ok((Expr::Str(s), current_state));
        }
        Expr::Func(f) => {
            return Ok((Expr::Func(f), current_state));
        }
        Expr::Var(v) => match current_state.get(&v) {
            Some(val) => Ok((val, current_state)),
            None => Err((Error::NotAVariable(v), current_state)),
        },
    }
}

fn apply(
    state: Memory,
    f: Expr,
    params: LinkedList<Expr>,
) -> Result<(Expr, Memory), (Error, Memory)> {
    let mut new_state = state.clone();
    match f {
        Expr::Func(func) => {
            let res = func(&params);
            let result = match res {
                Ok(e) => Ok((e, new_state)),
                Err(e) => Err((e, new_state)),
            };
            return result;
        }
        Expr::Var(var) => {
            let func = state.get(&var);
            if func == None {
                return Err((Error::NotAProcedure(var), state));
            }
            let func = func.unwrap();
            let res = match func {
                Expr::Func(function) => function(&params),
                _ => Err(Error::NotAProcedure(func.to_string())),
            };
            let result = match res {
                Ok(e) => Ok((e, new_state)),
                Err(e) => Err((e, new_state)),
            };
            return result;
        }
        _ => Err((Error::NotAProcedure(f.to_string()), state)),
    }
}

fn print(x: Result<(Expr, Memory), (Error, Memory)>) {
    match x {
        Ok((e, m)) => match e {
            Expr::Func(_) => println!(
                "{}",
                Error::CannotPrint(String::from("function"))
                    .to_string()
                    .red()
                    .bold()
            ),
            Expr::Var(v) => match m.get(&v) {
                Some(val) => println!("{}", val),
                None => eprintln!("{}", Error::NotAVariable(v).to_string().red().bold()),
            },
            _ => println!("{}", e),
        },
        Err((e, _)) => println!("{}", e.to_string().red().bold()),
    }
}

fn repl() {
    let mut mem = Memory::new();
    loop {
        //read
        let input = read();
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
    fn test_tokenize() {
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
    fn test_tokenize_error() {
        let open_error = Error::OpenParenMissing(None);
        let close_error = Error::CloseParenMissing(None);
        assert_eq!(tokenize(String::from("(1 2 3")).unwrap_err(), close_error);
        assert_eq!(tokenize(String::from("1 2 3)")).unwrap_err(), open_error);
        assert_eq!(tokenize(String::from(")")).unwrap_err(), open_error);
        assert_eq!(tokenize(String::from("(")).unwrap_err(), close_error);
        assert_eq!(tokenize(String::from("())")).unwrap_err(), open_error);
    }

    #[test]
    fn test_type_formating() {
        assert_eq!(Expr::Bool(true).to_string(), "#t");
        assert_eq!(Expr::Bool(false).to_string(), "#f");
        assert_eq!(Expr::Str("A".to_string()).to_string(), "A");
        assert_eq!(Expr::Num(-1.0).to_string(), "-1");
        assert_eq!(Expr::Num(0.5).to_string(), "0.5");
    }

    #[test]
    fn test_parse_expr() {
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
    fn test_parse_01() {
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
    fn test_parse_02() {
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
    fn test_parse_03() {
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
    fn test_memory_get_insert() {
        let mut mem = Memory::new();
        mem.insert(&String::from("a"), Expr::Bool(true));
        assert_eq!(mem.get(&String::from("a")).unwrap(), Expr::Bool(true));
    }

    #[test]
    fn test_memory_insert_twice() {
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
            return None;
        }
        let p = parse(t.unwrap());
        if p.is_err() {
            return None;
        }
        let e = eval(p.unwrap(), state);
        match e {
            Ok((val, _)) => Some(val),
            Err(_) => None,
        }
    }

    #[test]
    fn eval_valid_add() {
        assert_eq!(Expr::Num(3.0), eval_or_none("(+ 1 2)".to_string()).unwrap());
    }
}
