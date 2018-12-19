use lazy_static::*;
use regex::Regex;
use std::collections::HashMap;
use std::env::args;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Copy, Clone)]
struct Timestamp {
    month: u16,
    day: u16,
    hour: u16,
    minute: u16,
}

#[derive(Debug)]
enum Event {
    BeginsShift(u16),
    FallsAsleep,
    WakesUp,
}

#[derive(Debug)]
struct LogEntry {
    event: Event,
    timestamp: Timestamp,
}

struct Nap {
    guard: u16,
    start: u16,
    end: u16,
}

struct NapStats {
    most: u16,
    count: u16,
    total: u16,
}

fn to_naps(log: &[LogEntry]) -> Vec<Nap> {
    let mut guard = 0;
    let mut naps = Vec::new();
    let mut start = 0;

    for le in log {
        match le.event {
            Event::BeginsShift(g) => guard = g,
            Event::FallsAsleep => start = le.timestamp.minute,
            Event::WakesUp => naps.push(Nap {
                guard,
                start,
                end: le.timestamp.minute,
            }),
        }
    }

    naps
}

fn stats(naps: &[(u16, u16)]) -> NapStats {
    let total = naps.iter().map(|(s, e)| e - s).sum();
    let mut minutes = HashMap::new();

    for (s, e) in naps {
        for m in *s..*e {
            *minutes.entry(m).or_insert(0) += 1;
        }
    }

    let (most, count) = minutes.into_iter().max_by_key(|p| p.1).unwrap();
    NapStats { most, count, total }
}

fn main() {
    let filename = args().nth(1).expect("Expected file name");

    let br = BufReader::new(File::open(filename).unwrap());

    let stats_by_guard: HashMap<u16, NapStats> = {
        let mut log: Vec<LogEntry> = br
            .lines()
            .map(|line| line.unwrap())
            .map(|line| parse_log_entry(&line).unwrap())
            .collect();

        log.sort_by_key(|le| le.timestamp);

        let naps = to_naps(&log);

        let mut naps_by_guard = HashMap::new();

        for Nap { guard, start, end } in naps {
            naps_by_guard
                .entry(guard)
                .or_insert(Vec::new())
                .push((start, end));
        }
        naps_by_guard
            .into_iter()
            .map(|(k, v)| (k, stats(&v)))
            .collect()
    };

    let (&g, &NapStats { most, .. }) = stats_by_guard.iter().max_by_key(|(_, s)| s.total).unwrap();

    println!("Part 1: {}", (g as u32) * (most as u32));

    let (&g, &NapStats { most, .. }) = stats_by_guard.iter().max_by_key(|(_, s)| s.count).unwrap();

    println!("Part 2: {}", (g as u32) * (most as u32));
}

// Parsing

fn parse_log_entry(input: &str) -> Result<LogEntry, String> {
    lazy_static! {
        static ref TS: Regex = Regex::new(r"^\[\d{4}-(\d{2})-(\d{2}) (\d{2}):(\d{2})\]").unwrap();
        static ref EV: Regex =
            Regex::new(r" (Guard #(\d+) begins shift|falls asleep|wakes up)$").unwrap();
    }

    if let Some(cap) = TS.captures_iter(input).next() {
        let timestamp = Timestamp {
            month: cap[1].parse().unwrap(),
            day: cap[2].parse().unwrap(),
            hour: cap[3].parse().unwrap(),
            minute: cap[4].parse().unwrap(),
        };

        if let Some(cap) = EV.captures_iter(input).next() {
            let event = match &cap[1] {
                "falls asleep" => Event::FallsAsleep,
                "wakes up" => Event::WakesUp,
                _ => Event::BeginsShift(cap[2].parse().unwrap()),
            };

            Ok(LogEntry { timestamp, event })
        } else {
            Err(format!("Failed to parse event: {}", input))
        }
    } else {
        Err(format!("Failed to parse timestamp: {}", input))
    }
}
