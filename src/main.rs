use std::collections::HashMap;
use std::collections::LinkedList;

type Tokens = Vec<String>;

fn main() {
    println!("minlisp interpreter");
}

struct Memory {
    vars: HashMap<String, Expr>,
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
            Expr::List(_l) => unimplemented!(), //write!(f, "{}", l),
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
fn parse(tokens: Tokens) -> Result<Vec<Expr>, Error> {
    return Ok(vec![Expr::Bool(true)]);

    /*if tokens.len() == 1 {
        return vec![parse_expr(tokens.get(0).unwrap().to_string())];
    }
    let mut lines: Vec<Expr> = Vec::new();
    let mut line = LinkedList::new();
    let mut currentlist = LinkedList::new();
    for t in tokens {
        if t == "(" {
            if currentline.is_empty() {
            } else {
            }
        } else if t == ")" {
            if currentlist.is_empty() {
                lines.push_back(l);
                line = LinkedList::new();
            } else {
                let l = Expr::List(currentlist);
            }

            let l = Expr::List(currentlist);
            line.push_back(l);
            currentlist = LinkedList::new();
        } else {
            let e = parse_expr(t);
            currentlist.push_back(e);
        }
    }**/
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
}
