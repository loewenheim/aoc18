use std::env::args;
use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufReader};

fn main() -> io::Result<()> {
    let filename = args().nth(1).expect("Expected filename");
    let file = File::open(&filename)?;
    let br = BufReader::new(file);

    let sum: i32 = br
        .lines()
        .map(|line| line.unwrap())
        .map(|line| line.parse::<i32>().unwrap())
        .sum();
    println!("{}", sum);
    Ok(())
}
