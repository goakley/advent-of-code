//use std::cmp::{max, min};
use std::collections::HashSet;

#[derive(Clone, Debug, PartialEq, Eq)]
struct Point {
    x: i64,
    y: i64,
}

impl Point {
    fn mdistance(&self, other: &Self) -> i64 {
        (self.x - other.x).abs() + (self.y - other.y).abs()
    }
}

impl<'a, 'b> std::ops::Add<&'b Point> for &'a Point {
    type Output = Point;

    fn add(self, other: &'b Point) -> Point {
        Point {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl<'a, 'b> std::ops::Sub<&'b Point> for &'a Point {
    type Output = Point;

    fn sub(self, other: &'b Point) -> Point {
        Point {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Bound {
    min: Point,
    max: Point,
}

impl Bound {
    fn merge(&self, other: &Bound) -> Bound {
        Bound {
            min: Point {
                x: std::cmp::min(self.min.x, other.min.x),
                y: std::cmp::min(self.min.y, other.min.y),
            },
            max: Point {
                x: std::cmp::max(self.max.x, other.max.x),
                y: std::cmp::max(self.max.y, other.max.y),
            },
        }
    }
}

struct LR(i64, i64);

impl LR {
    fn add(&mut self, l: i64, r: i64) {
        if l > self.1 || r < self.0 {
            return false;
        }
        
        true
    }
}

fn flush(mut vs: Vec<(i64, i64)>) -> Vec<(i64, i64)> {
    vs.sort();
    let v = Vec::with_capacity(vs.len());
    let mut vsi = vs.into_iter();
    match vsi.next() {
        None => v,
        Some(mut candidate) => {
            for (l, r) in vsi {
            }
        }
    }
}

struct Seq(Vec<(i64,i64)>);


impl Seq {
    fn add(&mut self, l: i64, r: i64) {
        s: Vec<(i64, i64)> = Vec::with_capacity(self.0.len() + 1);
        for (a, b) in self.0.iter() {
            if r < *a {
                s.push((*a, *b));


            if l > *b || r < *a {
                s.push((*a, *b));
            } else {
                s.push((std::cmp::min(l, a), std::cmp::max(r, b)))
            }
        }
        Seq(self.0.into_iter().fold(vec![], |s: Vec<(i64, i64)>, (l, r)| { todo!() }))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Reading(Point, Point);

impl Reading {
    fn bounds(&self) -> Bound {
        let Reading(sensor, beacon) = self;
        let d = sensor.mdistance(beacon);
        let delta = Point { x: d, y: d };
        Bound {
            min: sensor - &delta,
            max: sensor + &delta,
        }
    }
}

fn nom_point(s: &str) -> nom::IResult<&str, Point> {
    nom::combinator::map(
        nom::sequence::separated_pair(
            nom::sequence::preceded(
                nom::bytes::complete::tag("x="),
                nom::character::complete::i64,
            ),
            nom::bytes::complete::tag(", "),
            nom::sequence::preceded(
                nom::bytes::complete::tag("y="),
                nom::character::complete::i64,
            ),
        ),
        |(x, y)| Point { x, y },
    )(s)
}

impl std::str::FromStr for Reading {
    type Err = nom::Err<nom::error::Error<String>>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        nom::combinator::map(
            nom::sequence::pair(
                nom::sequence::preceded(nom::bytes::complete::tag("Sensor at "), nom_point),
                nom::sequence::preceded(
                    nom::bytes::complete::tag(": closest beacon is at "),
                    nom_point,
                ),
            ),
            |(a, b)| Reading(a, b),
        )(s)
        .map(|(_, r)| r)
        .map_err(|e: nom::Err<nom::error::Error<&str>>| e.to_owned())
    }
}

fn taken_positions(readings: &[Reading], y: i64) -> usize {
    let bound: Bound = readings
        .iter()
        .map(|r| r.bounds())
        .reduce(|a, b| a.merge(&b))
        .unwrap();
    let mut seen: Vec<bool> = (bound.min.x..=bound.max.x).map(|_| false).collect();
    readings.iter().for_each(|Reading(sensor, beacon)| {
        let offset = sensor.mdistance(beacon) - ((sensor.y - y).abs());
        (-offset..=offset).for_each(|dx| {
            seen[(sensor.x + dx - bound.min.x) as usize] = true;
        });
    });
    readings
        .iter()
        .filter_map(|Reading(_, beacon)| if beacon.y == y { Some(beacon.x) } else { None })
        .for_each(|x| {
            seen[(x - bound.min.x) as usize] = false;
        });
    seen.iter().filter(|b| **b).count()
}

advent_2022::day_function!(15, input, {
    let readings: Vec<Reading> = input.lines();
    (
        taken_positions(&readings, 2000000).to_string(),
        "".to_string(),
    )
});
