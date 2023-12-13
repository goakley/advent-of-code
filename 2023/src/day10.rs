use std::collections::{HashMap, HashSet};

#[derive(Debug, PartialEq, Eq)]
enum Direction {
    North,
    South,
    East,
    West,
}

#[derive(Debug, PartialEq, Eq)]
enum Pipe {
    NorthSouth,
    EastWest,
    NorthEast,
    NorthWest,
    SouthEast,
    SouthWest,
    Ground,
    Start,
}

impl Pipe {
    fn dir(&self) -> (bool, bool, bool, bool) {
        match self {
            Pipe::NorthSouth => (true, false, true, false),
            Pipe::EastWest => (false, true, false, true),
            Pipe::NorthEast => (true, true, false, false),
            Pipe::NorthWest => (true, false, false, true),
            Pipe::SouthWest => (false, false, true, true),
            Pipe::SouthEast => (false, true, true, false),
            Pipe::Ground => (false, false, false, false),
            Pipe::Start => (true, true, true, true),
        }
    }

    fn opens(&self, direction: Direction) -> bool {
        let d = self.dir();
        match direction {
            Direction::North => d.0,
            Direction::East => d.1,
            Direction::South => d.2,
            Direction::West => d.3,
        }
    }

    fn connects(&self, other: &Self, direction: Direction) -> bool {
        let a = self.dir();
        let b = other.dir();
        match direction {
            Direction::North => a.0 && b.2,
            Direction::East => a.1 && b.3,
            Direction::South => a.2 && b.0,
            Direction::West => a.3 && b.1,
        }
    }
}

impl From<char> for Pipe {
    fn from(c: char) -> Self {
        match c {
            '|' => Pipe::NorthSouth,
            '-' => Pipe::EastWest,
            'L' => Pipe::NorthEast,
            'J' => Pipe::NorthWest,
            '7' => Pipe::SouthWest,
            'F' => Pipe::SouthEast,
            '.' => Pipe::Ground,
            'S' => Pipe::Start,
            _ => panic!("unknown"),
        }
    }
}

type Coordinate = (i64, i64);
type Grid = HashMap<Coordinate, Pipe>;

fn find_pipes(grid: &Grid, point: Coordinate) -> (Coordinate, Coordinate) {
    let pipe = grid.get(&point).unwrap_or(&Pipe::Ground);
    let pipes: Vec<Coordinate> = vec![
        ((point.0 - 1, point.1), Direction::North),
        ((point.0 + 1, point.1), Direction::South),
        ((point.0, point.1 - 1), Direction::West),
        ((point.0, point.1 + 1), Direction::East),
    ]
    .into_iter()
    .filter_map(|(c, d)| grid.get(&c).filter(|p| pipe.connects(p, d)).and(Some(c)))
    .collect();
    match &pipes[..] {
        [a, b, c] if grid.get(a) == Some(&Pipe::Start) => (*b, *c),
        [a, b, c] if grid.get(b) == Some(&Pipe::Start) => (*a, *c),
        [a, b, c] if grid.get(c) == Some(&Pipe::Start) => (*a, *b),
        [a, b] => (*a, *b),
        _ => panic!("impossible"),
    }
}

fn get_next_pipe(grid: &Grid, point: Coordinate, exclude: Coordinate) -> Coordinate {
    let (a, b) = find_pipes(grid, point);
    match (a == exclude, b == exclude) {
        (true, false) => b,
        (false, true) => a,
        _ => panic!("programmer error"),
    }
}

/// Double the size of the grid, marking what's a pipe wall and what's not
fn double(grid: &Grid, wall: &HashSet<Coordinate>) -> HashMap<Coordinate, bool> {
    grid.iter()
        .flat_map(|((x_, y_), pipe)| {
            let x = *x_ * 2;
            let y = *y_ * 2;
            if wall.contains(&(*x_, *y_)) {
                vec![
                    ((x, y), true),
                    ((x + 1, y), pipe.opens(Direction::South)),
                    ((x, y + 1), pipe.opens(Direction::East)),
                    ((x + 1, y + 1), false),
                ]
            } else {
                vec![
                    ((x, y), false),
                    ((x + 1, y), false),
                    ((x, y + 1), false),
                    ((x + 1, y + 1), false),
                ]
            }
        })
        .collect()
}

/// Flood outward from a point, determining if we've escaped the area or not
fn expand(
    walls: &HashMap<Coordinate, bool>,
    tracker: &mut HashSet<Coordinate>,
    (x, y): Coordinate,
) -> bool {
    if walls.get(&(x, y)).is_none() {
        // if we leave the grid then we've escaped
        return true;
    }
    if walls.get(&(x, y)) == Some(&true) {
        // if we hit a wall then we haven't escaped
        return false;
    }
    if tracker.contains(&(x, y)) {
        // if we double back then we haven't escaped
        return false;
    }
    tracker.insert((x, y));
    // escape in any direction is an escape
    expand(walls, tracker, (x - 1, y))
        || expand(walls, tracker, (x + 1, y))
        || expand(walls, tracker, (x, y - 1))
        || expand(walls, tracker, (x, y + 1))
}

/// Mark all points on the (expanded) grid as either inside or not
fn mark_inside_outside(walls: &HashMap<Coordinate, bool>) -> HashMap<Coordinate, bool> {
    let mut inside: HashMap<Coordinate, bool> = walls
        .iter()
        .filter_map(|(k, v)| if *v { None } else { Some((*k, false)) })
        .collect();
    for (x, y) in walls
        .iter()
        .filter_map(|(k, v)| if *v { None } else { Some(*k) })
    {
        if inside.get(&(x, y)) == Some(&true) {
            continue;
        }
        let mut tracker: HashSet<Coordinate> = HashSet::new();
        let escaped = expand(walls, &mut tracker, (x, y));
        for xy in tracker.into_iter() {
            inside.insert(xy, !escaped);
        }
    }
    inside
}

advent_2023::day_function!(10, input, {
    let grid: Grid = input
        .grid::<Pipe>()
        .into_iter()
        .map(|((x, y), v)| ((x as i64, y as i64), v))
        .collect();

    let start = grid
        .iter()
        .filter_map(|(xy, pipe)| if pipe == &Pipe::Start { Some(xy) } else { None })
        .next()
        .unwrap();
    let (mut one, mut two) = find_pipes(&grid, *start);
    let mut last_one = *start;
    let mut last_two = *start;
    let mut seen: HashSet<Coordinate> = HashSet::new();
    seen.insert(*start);
    seen.insert(one);
    seen.insert(two);
    let mut count = 1;
    loop {
        let next_one = get_next_pipe(&grid, one, last_one);
        let next_two = get_next_pipe(&grid, two, last_two);
        if seen.contains(&next_one) || seen.contains(&next_two) {
            seen.insert(next_one);
            seen.insert(next_two);
            break;
        }
        last_one = one;
        last_two = two;
        one = next_one;
        two = next_two;
        count += 1;
        seen.insert(next_one);
        seen.insert(next_two);
    }

    let bigger_grid = double(&grid, &seen);
    let insides = mark_inside_outside(&bigger_grid);
    let inside_count = grid
        .keys()
        .filter(|(x, y)| *insides.get(&(x * 2, y * 2)).unwrap_or(&false))
        .count();

    (count.to_string(), inside_count.to_string())
});
