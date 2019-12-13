use std::collections::LinkedList;

type Tokens = Vec<String>;

fn main() {
    println!("minlisp interpreter");
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

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.clone() {
            Expr::Var(x) => unimplemented!(), //write!(f, "{}", x),
            Expr::Str(s) => write!(f, "{}", s),
            Expr::Bool(b) => write!(f, "#{}", (if b { "t" } else { "f" })),
            Expr::Num(n) => write!(f, "{}", n),
            Expr::Func(_) => Err(std::fmt::Error),
            Expr::List(l) => unimplemented!(), //write!(f, "{}", l),
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
    }
    .to_string()
}

/// Error Enum
/// This enum is used for reporting errors
/// Some contain a string for the variable part of the error and
/// an option<usize> to indicate the line number if there is one
#[derive(Clone)]
enum Error {
    //fn name, given variable, expected type
    TypeError(String, Expr, String, Option<usize>),
    //fn name
    NotAProcedure(String, Option<usize>),
    //fn name, given number of params, expected number
    ArityMismatch(String, usize, usize, Option<usize>),
    CloseParenMissing(Option<usize>),
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
                Some(x) => write!(f, "Parentheis mismatch on line {}", x),
                None => write!(f, "Parenthesis mismatch"),
            },
        }
    }
}
/// Tokenize function
/// This function just gets an input (String) and returns
/// a tokenized result (a vector of strings, either a token or parenthesis)
fn tokenize(code: String) -> Tokens {
    let mut v = Vec::new();
    let mut cur = String::new();
    let mut in_quotes = false;
    let mut last_escaped = false;
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
        } else if i == ')' && !in_quotes {
            if !cur.is_empty() {
                v.push(cur.clone());
                cur = String::new();
            }
            v.push(String::from(")"));
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
    return v;
}

fn is_in_quotes(s: &String) -> bool {
    s.len() >= 2
        && (s.clone().into_bytes()[0] as char) == '"'
        && (s.clone().into_bytes()[s.len() - 1] as char) == '"'
}

fn parse_expr(token: String) -> Expr {
    if token == "#t" {
        return Expr::Bool(true);
    } else if token == "#f" {
        return Expr::Bool(false);
    } else if is_in_quotes(&token) {
        return Expr::Str(token[1..token.len()].to_string());
    } else {
        let parsed = token.parse::<f64>();
        return match parsed {
            Ok(x) => Expr::Num(x),
            Err(_) => Expr::Var(token),
        };
    }
    panic!("Implementation error in parsing")
}

fn parse(tokens: Tokens) -> Expr {
    unimplemented!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize() {
        let empty: Vec<String> = Vec::new();
        assert_eq!(tokenize(String::from("1 2 3")), vec!["1", "2", "3"]);
        assert_eq!(
            tokenize(String::from("(1 2 3)")),
            vec!["(", "1", "2", "3", ")"]
        );
        assert_eq!(tokenize(String::from("( 1 2 )")), vec!["(", "1", "2", ")"]);
        assert_eq!(tokenize(String::from("     1")), vec!["1"]);
        assert_eq!(tokenize(String::from("")), empty);
        assert_eq!(tokenize(String::from("1       ")), vec!["1"]);
        assert_eq!(tokenize(String::from("\"1 2\"")), vec!["\"1 2\""]);
        assert_eq!(tokenize(String::from("\"12 2\"")), vec!["\"12 2\""]);
        assert_eq!(tokenize(String::from("\\\"")), vec!["\""]);
        assert_eq!(tokenize(String::from("\"((((")), vec!["\"(((("]);
        assert_eq!(tokenize(String::from("((((")), vec!["(", "(", "(", "("]);
    }

    #[test]
    fn type_format() {
        assert_eq!(Expr::Bool(true).to_string(), "#t");
        assert_eq!(Expr::Bool(false).to_string(), "#f");
        assert_eq!(Expr::Str("A".to_string()).to_string(), "A");
        assert_eq!(Expr::Num(-1.0).to_string(), "-1");
        assert_eq!(Expr::Num(0.5).to_string(), "0.5");
    }
}
