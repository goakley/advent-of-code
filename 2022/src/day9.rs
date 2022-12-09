use std::collections::HashSet;
use std::cmp::max;

#[derive(Debug, PartialEq, Eq)]
enum Direction {
    L,
    R,
    U,
    D,
}

impl std::str::FromStr for Direction {
    type Err = std::convert::Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "L" => Direction::L,
            "R" => Direction::R,
            "U" => Direction::U,
            "D" => Direction::D,
            _ => panic!(),
        })
    }
}

fn shift(knots: &mut Vec<(i64, i64)>, direction: &Direction) {
    knots[0] = match direction {
        Direction::L => (knots[0].0 - 1, knots[0].1),
        Direction::R => (knots[0].0 + 1, knots[0].1),
        Direction::U => (knots[0].0, knots[0].1 - 1),
        Direction::D => (knots[0].0, knots[0].1 + 1),
    };
    for i in 1..knots.len() {
        let (dx, dy) = (knots[i-1].0 - knots[i].0, knots[i-1].1 - knots[i].1);
        knots[i] = if (dx.abs() + dy.abs()) < 3 {
            (knots[i].0 + dx.signum() * max(0, dx.abs() - 1), knots[i].1 + dy.signum() * max(0, dy.abs() - 1))
        } else {
            (knots[i].0 + dx.signum(), knots[i].1 + dy.signum())
        };
    }
}

fn process(commands: &[(Direction, i64)], knot_count: usize) -> usize {
    let mut knots: Vec<(i64, i64)> = (0..knot_count).map(|_| (0i64, 0i64)).collect();
    let mut positions: HashSet<(i64, i64)> = HashSet::new();
    positions.insert((0, 0));
    for (direction, magnitude) in commands.iter() {
        for _ in 0..*magnitude {
            shift(&mut knots, direction);
            positions.insert(knots[knots.len()-1]);
        }
    }
    positions.len()
}

advent_2022::day_function!(6, input, {
    let commands: Vec<(Direction, i64)> = input.pairs();
    (process(&commands, 2).to_string(), process(&commands, 10).to_string())
});
