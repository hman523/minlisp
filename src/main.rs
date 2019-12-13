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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize() {}
}
