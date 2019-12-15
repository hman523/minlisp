use std::collections::HashMap;
use std::collections::LinkedList;

type Tokens = Vec<String>;

fn main() {
    println!("minlisp interpreter");
}

struct Memory {
    vars: HashMap<String, Expr>,
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            vars: HashMap::new(),
        }
    }

    pub fn get(&self, s: &String) -> Result<Expr, Error> {
        match self.vars.get(s).ok_or(Error::NotAVariable(s.to_string())) {
            Ok(e) => Ok((*e).clone()),
            Err(e) => Err(e),
        }
    }

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
    Func(fn(&[Expr]) -> Result<Expr, Error>),
    List(LinkedList<Expr>),
    Lines(Vec<Expr>),
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
        write!(f, "{}", self)
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
/// Some contain a string for the variable part of the error and
/// an option<usize> to indicate the line number if there is one
#[derive(Clone, Debug, PartialEq)]
enum Error {
    //fn name, given variable, expected type
    TypeError(String, Expr, String, Option<usize>),
    //fn name
    NotAProcedure(String, Option<usize>),
    NotAVariable(String),
    RedefiningVar(String),
    //fn name, given number of params, expected number
    ArityMismatch(String, usize, usize, Option<usize>),
    CloseParenMissing(Option<usize>),
    OpenParenMissing(Option<usize>),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.clone() {
            Error::TypeError(func, given, expected, line) => {
                let on_line = match line {
                    Some(x) => format!(" on line {}", x),
                    None => String::new(),
                };
                write!(
                    f,
                    "Type error in function {}{}
					\tExpected type:{}
					\tGiven type:{}",
                    func,
                    on_line,
                    expected,
                    get_type_name(given)
                )
            }
            Error::NotAProcedure(func, line) => {
                let on_line = match line {
                    Some(x) => format!(" on line {}", x),
                    None => String::new(),
                };
                write!(f, "Unable to call {}, function not found{}", func, on_line)
            }
            Error::NotAVariable(name) => write!(f, "Variable {} does not exist", name),
            Error::RedefiningVar(name) => write!(f, "Cannot redeclare variable {}", name),
            Error::ArityMismatch(func, given, expected, line) => {
                let on_line = match line {
                    Some(x) => format!(" on line {}", x),
                    None => String::new(),
                };
                write!(
                    f,
                    "Arity mismatch error in {}{}
				\tGiven parameters: {}
				\tExpected parameters: {}",
                    func, on_line, given, expected
                )
            }
            Error::CloseParenMissing(line) => match line {
                Some(x) => write!(
                    f,
                    "Parenthesis mismatch on line {}: too many open parenthesis",
                    x
                ),
                None => write!(f, "Parenthesis mismatch: too many open parenthesis"),
            },
            Error::OpenParenMissing(line) => match line {
                Some(x) => write!(
                    f,
                    "Parenthesis mismatch on line {}: too many close parenthesis",
                    x
                ),
                None => write!(f, "Parenthesis mismatch: too many close parenthesis"),
            },
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
        if i == ' ' && !in_quotes {
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
        x if x > 0 => Err(Error::CloseParenMissing(None)),
        x if x < 0 => Err(Error::OpenParenMissing(None)),
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

/// parse function
/// This function converts tokens into a valid vector of expressions
/// Each element in the vector is another line in the code
/// The code would be parsed from `(print "hi") (print "bye")` to
/// ```vec![Expr::List(vec![Expr::Func("print"), Expr::Str("hi")]),
/// Expr::List(vec![Expr::Func("print"), Expr::Str("bye")])]```
fn parse(tokens: Tokens) -> Result<Expr, Error> {
    let mut code: Vec<Expr> = Vec::new();
    let mut line: LinkedList<Expr> = LinkedList::new();
    for i in 0..tokens.len() {
        if tokens[i] == "(" {
            let inner = parse(tokens[1..].to_vec());
            if inner.is_ok() {
                code.push(inner.unwrap());
            } else {
                return inner;
            }
        } else if tokens[i] == ")" {
            return Ok(Expr::List(line));
        } else {
            let e = parse_expr(tokens[i].clone());
            line.push_back(e);
        }
    }
    return Ok(Expr::Lines(code));
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
        let expected = Expr::Lines(vec![Expr::List(list)]);
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
        let expected = Expr::Lines(vec![Expr::List(first), Expr::List(list)]);
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
        let expected = Expr::Lines(vec![Expr::List(list)]);
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
}
