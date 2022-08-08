#![no_main]
use libfuzzer_sys::fuzz_target;

use std::{
    path::Path,
    fs
};

pub mod space_lang { 
    include!("../../src/main.rs");
}

use crate::space_lang::*;

fuzz_target!(|data: &[u8]| {
    let file = "examples/syntax.space";
    let path = Path::new(&file).parent().unwrap().to_str().unwrap().to_string();
    let contents = fs::read_to_string(&file).expect(&format!("Couldn't read this file, result is {}", file));

    let mut lexer = Lexer::new(contents, path);
    let contextlol = lexer.lex(0, lexer.contents.len());
    let mut modules: Vec<Token> = Vec::new();
    println!("{:#?}", &contextlol);
});
