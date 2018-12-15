use std::collections::{HashSet,HashMap};
use std::env::args;
use std::fs::File;
use std::io::prelude::*;

fn count_letters(input: &str) -> HashSet<i32> {
    let mut letters = HashMap::new();
    for c in input.chars() {
        let counter = letters.entry(c).or_insert(0);
        *counter += 1;
    }

    letters.into_iter().map(|(_,v)| v).collect()
}

fn check_ids(first: &str, second: &str) -> Option<String> {
    let mut diff = false;
    let mut result = String::new();
    for (c1, c2) in first.chars().zip(second.chars()) {
        if c1 == c2 {
            result.push(c1);
        } else if diff {
            return None;
        } else {
            diff = true;
        }
    }

    Some(result)
}
fn main() {
    let filename = args().nth(1).expect("Expected filename");
    let mut file = File::open(&filename).unwrap();
    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents).unwrap();

    let mut doubles = 0;
    let mut triples = 0;

    for line in file_contents.lines() {
        let count = count_letters(&line);
        if count.contains(&2) {
            doubles += 1;
        }

        if count.contains(&3) {
            triples += 1;
        }
    }
    println!("Part 1: {}", doubles * triples);

    let mut result = String::new();
    for (i, line) in file_contents.lines().enumerate() {
       for other_line in file_contents.lines().skip(i+1) {
           if let Some(x) = check_ids(line, other_line) {
               result = x;
               break;
           }
       }
    }

    println!("Part 2: {}", result);
}
