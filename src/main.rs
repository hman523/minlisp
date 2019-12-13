use std::collections::LinkedList;

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

/// Error Enum
/// This enum is used for reporting errors
/// They all contain a string for the variable part of the error and
/// an option<usize> to indicate the line number if there is one
enum Error {
    TypeError(String, Option<usize>),
    NotAProcedure(String, Option<usize>),
    ArityMismatch(String, Option<usize>),
    CloseParenMissing(String, Option<usize>),
}

/// Tokenize function
/// This function just gets an input (String) and returns
/// a tokenized result (a vector of strings, either a token or parenthesis)
fn tokenize(code: String) -> Vec<String> {
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
}
