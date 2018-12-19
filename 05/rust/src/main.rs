use std::env::args;
use std::fs::File;
use std::io::Read;

fn inverses(a: char, b: char) -> bool {
    ((a.is_ascii_lowercase() && b.is_ascii_uppercase())
        || (a.is_ascii_uppercase() && b.is_ascii_lowercase()))
        && a.to_ascii_lowercase() == b.to_ascii_lowercase()
}

fn reduce_polymer<P>(polymer: &str, pred: P) -> usize
where
    P: Fn(&char) -> bool,
{
    let mut stack: Vec<char> = Vec::new();

    for c in polymer.trim().chars().filter(pred) {
        match stack.last() {
            Some(&d) if inverses(c, d) => {
                stack.pop();
            }
            _ => stack.push(c),
        }
    }

    stack.len()
}

fn main() {
    let polymer: String = {
        let filename = args().nth(1).expect("Expected file name");
        let mut input = String::new();
        File::open(filename)
            .unwrap()
            .read_to_string(&mut input)
            .unwrap();

        input.trim();
        input
    };

    println!("Part 1: {}", reduce_polymer(&polymer, |_| true));

    let min_length = "abcdefghijklmnopqrstuvwxyz"
        .chars()
        .map(|c| reduce_polymer(&polymer, |d| d.to_ascii_lowercase() != c))
        .min()
        .unwrap();

    println!("Part 2: {}", min_length);
}
