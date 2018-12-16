use std::env::args;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::str::FromStr;

#[derive(Debug)]
struct Claim {
    id: u16,
    left: u16,
    right: u16,
    top: u16,
    bottom: u16,
}

impl Claim {
    fn contains(&self, x: u16, y: u16) -> bool {
        x >= self.left && x < self.right && y >= self.top && y < self.bottom
    }

    fn disjoint(&self, other: &Claim) -> bool {
        self.top >= other.bottom
            || other.top >= self.bottom
            || self.left >= other.right
            || other.left >= self.right
    }
}

impl FromStr for Claim {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, String> {
        let mut zeroth = input.split('#').nth(1).unwrap().split('@');
        let id = zeroth.next().unwrap().trim().parse().unwrap();
        let mut first = zeroth.next().unwrap().split(',');
        let left = first.next().unwrap().trim().parse().unwrap();
        let mut second = first.next().unwrap().split(':');
        let top = second.next().unwrap().trim().parse().unwrap();
        let mut third = second.next().unwrap().split('x');
        let width: u16 = third.next().unwrap().trim().parse().unwrap();
        let height: u16 = third.next().unwrap().trim().parse().unwrap();

        Ok(Claim {
            id,
            left,
            top,
            right: left + width,
            bottom: top + height,
        })
    }
}

fn main() {
    let filename = args().nth(1).expect("Expected file name");

    let br = BufReader::new(File::open(filename).unwrap());

    let claims: Vec<Claim> = br
        .lines()
        .map(|l| l.unwrap())
        .map(|l| l.parse().unwrap())
        .collect();

    let mut count = 0;
    for x in 0..1000 {
        'y: for y in 0..1000 {
            let mut seen = false;

            for claim in claims.iter() {
                if claim.contains(x, y) {
                    if seen {
                        count += 1;
                        continue 'y;
                    } else {
                        seen = true;
                    }
                }
            }
        }
    }

    println!("Part 1: {}", count);

    for claim in claims.iter() {
        if claims
            .iter()
            .filter(|c| c.id != claim.id)
            .all(|c| c.disjoint(claim))
        {
            println!("Part 2: {}", claim.id);
        }
    }
}
