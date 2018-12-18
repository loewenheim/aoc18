use nom::types::CompleteStr;
use nom::*;
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
            Event::WakesUp => naps.push(Nap {guard, start, end: le.timestamp.minute}),
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
            .map(|l| l.unwrap())
            .map(|l| log_entry(CompleteStr(&l)).unwrap().1)
            .collect();

        log.sort_by_key(|le| le.timestamp);

        let naps = to_naps(&log);

        let mut naps_by_guard = HashMap::new();

        for Nap { guard, start, end} in naps {
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

    let (g, NapStats { most, .. }) = stats_by_guard
        .iter()
        .max_by_key(|t| (t.1).total)
        .unwrap();

    println!("Part 1: {}", (*g as u32) * (*most as u32));

    let (g, NapStats { most, .. }) = stats_by_guard
        .iter()
        .max_by_key(|t| (t.1).count)
        .unwrap();

    println!("Part 2: {}", (*g as u32) * (*most as u32));
}


// Parsers
named! {
    number<CompleteStr, u16>,
    map_res!(take_while!(|c: char| c.is_digit(10)), |s: CompleteStr| s.0.parse())
}

named! {
    timestamp<CompleteStr, Timestamp>,
    do_parse!(
        number >>
        tag!("-") >>
        month: number >>
        tag!("-") >>
        day: number >>
        tag!(" ") >>
        hour: number >>
        tag!(":") >>
        minute: number >>
        (Timestamp { month, day, hour, minute })
    )
}

named! {
    falls_asleep<CompleteStr, Event>,
    value!(Event::FallsAsleep, tag!("falls asleep"))
}

named! {
    wakes_up<CompleteStr, Event>,
    value!(Event::WakesUp, tag!("wakes up"))
}

named! {
    begins_shift<CompleteStr, Event>,
    map!(delimited!(
        tag!("Guard #"),
        number,
        tag!(" begins shift")),
        Event::BeginsShift)
}

named! {
    event<CompleteStr, Event>,
    alt!(begins_shift | wakes_up | falls_asleep)
}

named! {
    log_entry<CompleteStr, LogEntry>,
    do_parse!(
        timestamp: delimited!(tag!("["), timestamp, tag!("] ")) >>
        event: event >>
        (LogEntry { event, timestamp })
    )
}

