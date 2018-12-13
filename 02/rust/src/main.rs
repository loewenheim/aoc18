use std::collections::HashSet;
use std::env::args;
use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufReader};

struct PartialSums<I: Iterator<Item=i32>> {
    current_sum: Option<i32>,
    summands: I,
}

impl<I: Iterator<Item=i32>> Iterator for PartialSums<I> {
    type Item = i32;

    fn next(&mut self) -> Option<i32> {
        let ret = self.current_sum;
        self.current_sum = self.current_sum.and_then(|s| self.summands.next().map(|i| s+i));
        ret
    }
}

fn partial_sums<I: Iterator<Item=i32>>(summands: I) -> impl Iterator<Item=i32> {
    PartialSums {
        current_sum: Some(0),
        summands,
    }
}
    
fn main() -> io::Result<()> {
    let filename = args().nth(1).expect("Expected filename");
    let file = File::open(&filename)?;
    let br = BufReader::new(file);

    let summands = br
        .lines()
        .map(|line| line.unwrap())
        .map(|line| line.parse::<i32>().unwrap())
        .collect::<Vec<i32>>()
        .into_iter();

    let partials = partial_sums(summands.cycle());

    let mut ret = None;
    let mut reached = HashSet::new();

    for i in partials {
        if reached.contains(&i) {
            ret = Some(i);
            break;
        } else {
            reached.insert(i);
        }
    }

    match ret {
        Some(i) => println!("{}", i),
        None => println!("No frequency was found twice"),
    }

    Ok(())
}
