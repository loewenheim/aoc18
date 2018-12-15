use std::collections::HashSet;
use std::env::args;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;


fn main() {
    let filename = args().nth(1).expect("Expected filename");
    let file = File::open(&filename).unwrap();

    let numbers: Vec<i32> = BufReader::new(file)
        .lines()
        .map(|l| l.unwrap())
        .map(|line| line.parse::<i32>().unwrap())
        .collect();

    let sum: i32 = numbers.iter().sum();
    println!("Part 1: {}", sum);

    let mut ret = 0;
    let mut reached = HashSet::new();
    reached.insert(ret);

    for i in numbers.iter().cycle() {
        ret += i;
        if reached.contains(&ret) {
            break;
        } else {
            reached.insert(ret);
        }
    }

    println!("Part 2: {}", ret);
}
