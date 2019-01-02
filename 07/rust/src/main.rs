use re_parse::*;
use serde::Deserialize;
use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashSet};
use std::env::args;
use std::fs::File;
use std::io::{BufRead, BufReader};

type Step = char;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Deserialize, ReParse)]
#[re_parse(
    regex = r#"Step (?P<before>[A-Z]) must be finished before step (?P<after>[A-Z]) can begin."#
)]
struct Dependency {
    before: Step,
    after: Step,
}

fn has_dependency(deps: &HashSet<Dependency>, s: Step) -> bool {
    deps.iter().any(|&Dependency { after, .. }| after == s)
}

fn do_work<S: Fn(char) -> u8>(
    mut deps: HashSet<Dependency>,
    num_workers: usize,
    step_duration: S,
) -> (String, usize) {
    let mut workers = vec![None; num_workers];
    let mut tbd = HashSet::new();
    let mut ready = BinaryHeap::new();
    let mut result = String::new();
    let mut seconds_passed = 0;

    for &Dependency { before, after } in deps.iter() {
        tbd.insert(before);
        tbd.insert(after);
    }

    while !(tbd.is_empty() && ready.is_empty() && workers.iter().all(|w| w.is_none())) {
        //all steps that have no dependencies become ready
        let readies: HashSet<Step> = tbd
            .iter()
            .cloned()
            .filter(|&s| !has_dependency(&deps, s))
            .collect();

        for s in readies.into_iter() {
            ready.push(Reverse(s));
            tbd.remove(&s);
        }

        for w in workers.iter_mut() {
            if w.is_none() {
                // worker is idle, take next ready step if possible
                ready.pop().into_iter().for_each(|Reverse(s)| {
                    *w = Some((s, step_duration(s)));
                })
            }
        }

        for w in workers.iter_mut() {
            if let Some((s, i)) = *w {
                // worker has more work to do
                *w = Some((s, i - 1));
            }
        }

        // check finished workers
        for w in workers.iter_mut() {
            if let Some((s, 0)) = *w {
                // worker is done
                deps.retain(|&Dependency { before, .. }| before != s);
                *w = None;
                result.push(s);
            }
        }

        seconds_passed += 1;
    }

    (result, seconds_passed)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let deps: HashSet<Dependency> = {
        let filename = args().nth(1).expect("Expected file name");
        let br = BufReader::new(File::open(filename).unwrap());
        br.lines()
            .map(|line| line.unwrap())
            .map(|line| line.parse())
            .collect::<Result<HashSet<Dependency>, _>>()
    }?;

    let (s, _) = do_work(deps.clone(), 1, |_| 1);

    println!("Part 1: {}", s);

    let (_, n) = do_work(deps, 5, |c| c as u8 - 4);
    println!("Part 2: {}", n);

    Ok(())
}
