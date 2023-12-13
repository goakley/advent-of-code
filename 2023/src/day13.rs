use std::collections::{HashMap, HashSet};

fn bounds(grid: &HashSet<(i64, i64)>) -> ((i64, i64), (i64, i64)) {
    let xmin = *grid.iter().map(|(x, _)| x).min().unwrap_or(&0);
    let xmax = *grid.iter().map(|(x, _)| x).max().unwrap_or(&0);
    let ymin = *grid.iter().map(|(_, y)| y).min().unwrap_or(&0);
    let ymax = *grid.iter().map(|(_, y)| y).max().unwrap_or(&0);
    ((xmin, ymin), (xmax, ymax))
}

fn process_grid(grid: &HashSet<(i64, i64)>, not: Option<i64>) -> Option<i64> {
    let ((xmin, ymin), (xmax, ymax)) = bounds(grid);
    let rows: HashMap<i64, HashSet<i64>> = (xmin..=xmax)
        .map(|x| {
            (
                x,
                grid.iter()
                    .filter_map(|(gx, gy)| if *gx == x { Some(*gy) } else { None })
                    .collect(),
            )
        })
        .collect();
    let cols: HashMap<i64, HashSet<i64>> = (ymin..=ymax)
        .map(|y| {
            (
                y,
                grid.iter()
                    .filter_map(|(gx, gy)| if *gy == y { Some(*gx) } else { None })
                    .collect(),
            )
        })
        .collect();
    for x in xmin..xmax {
        //eprintln!("X {:?}", (xmin, xmax, x));
        if (0..=(x - xmin)).all(|d| {
            let top = rows.get(&(x - d));
            let bot = rows.get(&(x + 1 + d));
            match (top, bot) {
                (Some(t), Some(b)) => t == b,
                _ => true,
            }
        }) && Some(((x - xmin) + 1) * 100) != not
        {
            return Some(((x - xmin) + 1) * 100);
        }
    }
    for y in ymin..ymax {
        //eprintln!("Y {:?}", (ymin, ymax, y));
        if (0..=(y - ymin)).all(|d| {
            let left = cols.get(&(y - d));
            let right = cols.get(&(y + 1 + d));
            match (left, right) {
                (Some(l), Some(r)) => l == r,
                _ => true,
            }
        }) && Some((y - ymin) + 1) != not
        {
            return Some((y - ymin) + 1);
        }
    }
    None
}

fn process_grid_smudged(mut grid: HashSet<(i64, i64)>) -> i64 {
    let ((xmin, ymin), (xmax, ymax)) = bounds(&grid);
    let old = process_grid(&grid, None).unwrap();
    for x in xmin..=xmax {
        for y in ymin..=ymax {
            if grid.contains(&(x, y)) {
                //eprintln!("REMOVE {:?}", (x,y));
                grid.remove(&(x, y));
                let result = process_grid(&grid, Some(old));
                grid.insert((x, y));
                if let Some(i) = result {
                    return i;
                }
            } else {
                //eprintln!("ADD {:?}", (x,y));
                grid.insert((x, y));
                let result = process_grid(&grid, Some(old));
                grid.remove(&(x, y));
                if let Some(i) = result {
                    return i;
                }
            }
        }
    }
    panic!("Can't get here");
}

fn parse_grid(input: &str) -> nom::IResult<&str, HashSet<(i64, i64)>> {
    nom::combinator::map(
        nom::multi::many0(nom::combinator::map(
            nom::sequence::terminated(
                nom::multi::many1(nom::branch::alt((
                    nom::combinator::value(true, nom::character::complete::char('#')),
                    nom::combinator::value(false, nom::character::complete::char('.')),
                ))),
                nom::character::complete::line_ending,
            ),
            |v| {
                v.into_iter()
                    .enumerate()
                    .filter_map(|(i, b)| if b { Some(i as i64) } else { None })
                    .collect::<Vec<i64>>()
            },
        )),
        |v| {
            v.into_iter()
                .enumerate()
                .flat_map(|(j, is)| is.into_iter().map(move |i| (j as i64, i)))
                .collect::<HashSet<(i64, i64)>>()
        },
    )(input)
}

fn parse_input(input: &str) -> nom::IResult<&str, Vec<HashSet<(i64, i64)>>> {
    nom::multi::separated_list1(nom::character::complete::line_ending, parse_grid)(input)
}

advent_2023::day_function!(13, input, {
    let grids = parse_input(input.str()).unwrap().1;
    let sum: i64 = grids.iter().map(|g| process_grid(g, None).unwrap()).sum();
    let smudged_sum: i64 = grids.into_iter().map(process_grid_smudged).sum();
    (sum.to_string(), smudged_sum.to_string())
});
