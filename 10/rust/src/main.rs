use re_parse::*;
use regex::Regex;
use serde::Deserialize;
use std::collections::HashSet;
use std::env::args;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::thread::sleep;
use std::time::Duration;

#[derive(Debug, Clone, Copy, Deserialize, ReParse)]
#[re_parse(
    regex = r#"position=< *(?P<px>-?\d+), *(?P<py>-?\d+)> *velocity=< *(?P<vx>-?\d+), *(?P<vy>-?\d+)>"#
)]
struct Point {
    px: isize,
    py: isize,
    vx: isize,
    vy: isize,
}

impl Point {
    fn mov(&mut self) {
        self.px += self.vx;
        self.py += self.vy;
    }
}

struct Grid {
    seconds: usize,
    points: Vec<Point>,
}

impl Grid {
    fn tick(&mut self) {
        self.points.iter_mut().for_each(|p| p.mov());
        self.seconds += 1;
    }

    fn width(&self) -> usize {
        let left_border = self.points.iter().map(|p| p.px).min().unwrap();
        let right_border = self.points.iter().map(|p| p.px).max().unwrap();

        (right_border - left_border) as usize
    }
}

impl Display for Grid {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let point_set: HashSet<(isize, isize)> =
            self.points.iter().cloned().map(|p| (p.px, p.py)).collect();
        let left_border = *point_set.iter().map(|(x, _)| x).min().unwrap();
        let right_border = *point_set.iter().map(|(x, _)| x).max().unwrap();
        let bottom_border = *point_set.iter().map(|(_, y)| y).min().unwrap();
        let top_border = *point_set.iter().map(|(_, y)| y).max().unwrap();

        for y in bottom_border..=top_border {
            for x in left_border..=right_border {
                if point_set.contains(&(x, y)) {
                    write!(f, "#")?;
                } else {
                    write!(f, " ")?;
                }
            }

            writeln!(f)?;
        }

        Ok(())
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut grid = {
        let filename = args().nth(1).unwrap();
        let points: Result<Vec<Point>, _> = BufReader::new(File::open(filename)?)
            .lines()
            .map(|line| line.unwrap().parse())
            .collect();
        let points = points?;
        Grid { points, seconds: 0 }
    };

    let pause = Duration::new(2, 0);

    while grid.width() > 80 {
        grid.tick();
    }

    loop {
        println!("{}", grid);
        println!("Seconds: {}", grid.seconds);
        grid.tick();
        sleep(pause);
    }
}
