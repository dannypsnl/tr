use from_pest::FromPest;
use pest::Parser;
use std::fs;
use tr::*;

fn main() {
    let input = fs::read_to_string("trees/xxx-0001.tree").unwrap();
    let mut output = TreeParser::parse(Rule::file, input.as_str()).unwrap();
    match EFile::from_pest(&mut output) {
        Ok(f) => {
            for t in f.tops {
                println!("{:?}", t)
            }
        }
        Err(err) => panic!("{}", err),
    }
}
