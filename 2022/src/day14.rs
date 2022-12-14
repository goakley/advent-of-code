use std::cmp::{max, min};
use std::collections::HashSet;

const START: (i64, i64) = (500, 0);

#[derive(Clone, Debug, PartialEq, Eq)]
struct Line(Vec<(i64, i64)>);

impl std::str::FromStr for Line {
    type Err = nom::Err<nom::error::Error<String>>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        nom::combinator::map(
            nom::sequence::terminated(
                nom::multi::separated_list1(
                    nom::bytes::complete::tag(" -> "),
                    nom::sequence::separated_pair(
                        nom::character::complete::i64,
                        nom::character::complete::char(','),
                        nom::character::complete::i64,
                    ),
                ),
                nom::combinator::eof,
            ),
            Line,
        )(s)
        .map(|(_, r)| r)
        .map_err(|e: nom::Err<nom::error::Error<&str>>| e.to_owned())
    }
}

fn build_cave(lines: Vec<Line>) -> HashSet<(i64, i64)> {
    lines
        .into_iter()
        .flat_map(|Line(mut points)| {
            if points.len() < 2 {
                points.push(points[0]);
            }
            points
                .iter()
                .zip(points.iter().skip(1))
                .flat_map(|((xl, yl), (x, y))| match (x == xl, y == yl) {
                    (true, true) => {
                        vec![(*x, *y)]
                    }
                    (true, _) => (min(*y, *yl)..=max(*y, *yl))
                        .map(|y| (*x, y))
                        .collect::<Vec<(i64, i64)>>(),
                    (_, true) => (min(*x, *xl)..=max(*x, *xl))
                        .map(|x| (x, *y))
                        .collect::<Vec<(i64, i64)>>(),
                    _ => panic!(),
                })
                .collect::<Vec<(i64, i64)>>()
        })
        .collect()
}

fn drop_grain(
    (mut x, mut y): (i64, i64),
    deadzone: i64,
    cave: &HashSet<(i64, i64)>,
) -> Option<(i64, i64)> {
    while y < deadzone {
        if !cave.contains(&(x, y + 1)) {
            // down
            y += 1;
        } else if !cave.contains(&(x - 1, y + 1)) {
            // left
            x -= 1;
            y += 1;
        } else if !cave.contains(&(x + 1, y + 1)) {
            // right
            x += 1;
            y += 1;
        } else {
            // blocked
            return Some((x, y));
        }
    }
    None
}

advent_2022::day_function!(14, input, {
    let lines: Vec<Line> = input.lines();
    let cave = build_cave(lines);
    let ymin = cave.iter().min_by_key(|(_, y)| y).unwrap_or(&(0, 0)).1;
    let ymax = cave.iter().max_by_key(|(_, y)| y).unwrap_or(&(0, 0)).1;
    let xmin = cave.iter().min_by_key(|(x, _)| x).unwrap_or(&(0, 0)).0;
    let xmax = cave.iter().max_by_key(|(x, _)| x).unwrap_or(&(0, 0)).0;

    let mut filled_cave = cave.clone();
    while let Some(point) = drop_grain(START, ymax + 1, &filled_cave) {
        filled_cave.insert(point);
    }
    let sands1 = filled_cave.len() - cave.len();

    let mut floored_cave: HashSet<(i64, i64)> = cave;
    floored_cave
        .extend((xmin - (ymax - ymin) * 2..xmax + (ymax - ymin) * 2).map(|x| (x, ymax + 2)));
    filled_cave = floored_cave.clone();
    while let Some(point) = drop_grain(START, ymax + 3, &filled_cave) {
        filled_cave.insert(point);
        if point == START {
            break;
        }
    }
    let sands2 = filled_cave.len() - floored_cave.len();

    (sands1.to_string(), sands2.to_string())
});
