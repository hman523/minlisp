# Minlisp
## A minimum lisp implementation

[![Build Status](https://travis-ci.com/hman523/minlisp.svg?branch=master)](https://travis-ci.com/hman523/minlisp)

This is just for fun, I've been messing around in Rust and want to get better.

Some of the inspiration for this project (and the parser) came from this
[article](https://m.stopa.io/risp-lisp-in-rust-90a0dad5b116)

The goal is to have the implementation of the language in less than 1000 lines of
Rust, excluding tests and comments. It should support all of the features that 
McCarthy designed in his initial paper.

The langauge supports:
- [x] Tokenizing
- [x] Parsing
- [x] Evaluating
- [x] Error handling
- [x] Type checking
- [x] Numeric calculations
- [x] Numeric comparison
- [x] if statements
- [x] set expressions
- [ ] lambdas
- [ ] quoting
- [ ] List processing
- [ ] Garbage collection
