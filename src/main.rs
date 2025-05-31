use from_pest::FromPest;
use parser::{EFile, Rule, TreeParser};
use pest::Parser;
use std::fs;

mod parser;

fn main() {
    let input = fs::read_to_string("trees/xxx-0001.tree").unwrap();
    let mut output = TreeParser::parse(Rule::file, input.as_str()).unwrap();
    match EFile::from_pest(&mut output) {
        Ok(f) => println!("{:?}", f),
        Err(err) => panic!("{}", err),
    }
}
