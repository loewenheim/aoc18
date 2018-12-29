use lazy_static::*;
use re_parse::*;
use regex::Regex;
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::env::args;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter::Sum;

#[derive(Hash, Debug, PartialOrd, Ord, PartialEq, Eq, Copy, Clone, Deserialize, ReParse)]
#[re_parse(regex = r#"(?P<x>-?\d+), (?P<y>-?\d+)"#)]
struct Point {
    x: i32,
    y: i32,
}

fn manhattan(n: u16) -> impl Iterator<Item = (i32, i32)> {
    let n = n as i32;
    (0..n)
        .map(move |i| (n - i, i))
        .chain((0..n).map(move |i| (-i, n - i)))
        .chain((0..n).map(move |i| (i - n, -i)))
        .chain((0..n).map(move |i| (i, i - n)))
}

fn md(Point { x: x1, y: y1 }: Point, Point { x: x2, y: y2 }: Point) -> u32 {
    ((x1 - x2).abs() + (y1 - y2).abs()) as u32
}

fn least_distance<I: Iterator<Item = Point>>(xs: I, p: Point) -> Option<Point> {
    let mut result = None;
    let mut is_unique = true;

    for x in xs {
        match result {
            None => result = Some(x),
            Some(y) if md(x, p) == md(p, y) => is_unique = false,
            Some(y) if md(x, p) < md(y, p) => {
                result = Some(x);
                is_unique = true;
            }
            _ => {}
        }
    }

    result.filter(|_| is_unique)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let points: Vec<Point> = {
        let filename = args().nth(1).expect("Expected file name");
        let br = BufReader::new(File::open(filename).unwrap());
        br.lines()
            .map(|line| line.unwrap())
            .map(|line| line.parse())
            .collect::<Result<Vec<Point>, _>>()
    }?;

    let left_border: i32 = *points.iter().map(|Point { x, .. }| x).min().unwrap();
    let right_border: i32 = *points.iter().map(|Point { x, .. }| x).max().unwrap() + 1;
    let top_border: i32 = *points.iter().map(|Point { y, .. }| y).max().unwrap() + 1;
    let bottom_border: i32 = *points.iter().map(|Point { y, .. }| y).min().unwrap();

    {
        let regions = {
            let mut regions: HashMap<Point, HashSet<Point>> = HashMap::new();

            for x in left_border..right_border {
                for y in bottom_border..top_border {
                    let q = Point { x, y };
                    if let Some(p) = least_distance(points.iter().map(|p| *p), q) {
                        (*regions.entry(p).or_insert(HashSet::new())).insert(q);
                    }
                }
            }

            regions
        };

        let max_finite = regions
            .into_iter()
            .map(|(_, s)| s)
            .filter(|s| !s.iter().any(|&Point { x, .. }| x == left_border))
            .filter(|s| !s.iter().any(|&Point { x, .. }| x == right_border - 1))
            .filter(|s| !s.iter().any(|&Point { y, .. }| y == bottom_border))
            .filter(|s| !s.iter().any(|&Point { y, .. }| y == top_border - 1))
            .map(|s| s.len())
            .max()
            .unwrap();

        println!("Part 1: {}", max_finite);
    }

    {
        let region = (left_border - 10000..right_border + 10000)
            .flat_map(move |x| (bottom_border - 10000..top_border + 10000).map(move |y| Point { x, y }))
            .filter(|p| points.iter().map(|q| md(*p, *q)).sum() < 10000)
            .count();

            println!("Part 2: {}", region);
    }
    Ok(())
}
