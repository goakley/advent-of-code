use nom_locate::{position, LocatedSpan};
use std::collections::{HashMap, HashSet};

#[derive(PartialEq, Eq)]
enum Cell {
    Blank,
    Symbol,
    Gear,
    Number(u32),
}

impl From<char> for Cell {
    fn from(value: char) -> Self {
        match value.to_digit(10) {
            Some(i) => Cell::Number(i),
            None => match value {
                '.' => Cell::Blank,
                '*' => Cell::Gear,
                _ => Cell::Symbol,
            },
        }
    }
}

type Span<'a> = LocatedSpan<&'a str>;
type LookupMap = HashMap<(usize, usize), usize>;
type NumberMap = HashMap<usize, u32>;

/// Convert the input into the positions of all the numbers in the grid
///
/// The numbers are indirectly addressed: The first map is used to look up
/// the number index, and the second map the number itself.
/// This allows for uniquely identifying numbers when the number itself
/// may appear in multiple places in the input.
fn parse_numbers(input: Span) -> nom::IResult<Span, (LookupMap, NumberMap)> {
    nom::combinator::map(
        nom::combinator::all_consuming(nom::multi::many0(nom::branch::alt((
            nom::combinator::map(
                nom::sequence::tuple((
                    position::<Span, _>,
                    nom::character::complete::u32,
                    position::<Span, _>,
                )),
                |(pl, x, pr)| {
                    Some((
                        pl.location_line() as usize,
                        pl.get_column(),
                        pr.get_column(),
                        x,
                    ))
                },
            ),
            nom::combinator::value(None, nom::character::complete::anychar),
        )))),
        |vec| {
            vec.into_iter()
                .flatten()
                .enumerate()
                .flat_map(|(i, (x, yl, yr, number))| {
                    (yl..yr).map(move |y| (((x - 1, y - 1), i), (i, number)))
                })
                .unzip()
        },
    )(input)
}

fn expand((x, y): &(usize, usize)) -> Vec<(usize, usize)> {
    let xn = x.saturating_sub(1);
    let yn = y.saturating_sub(1);
    let xp = x.saturating_add(1);
    let yp = y.saturating_add(1);
    vec![
        (xn, yn),
        (xn, *y),
        (xn, yp),
        (*x, yn),
        (*x, yp),
        (xp, yn),
        (xp, *y),
        (xp, yp),
    ]
}

advent_2023::day_function!(3, input, {
    let grid = input.grid::<Cell>();
    let (number_positions, numbers) = parse_numbers(Span::new(input.str())).unwrap().1;

    // all the positions adjacent to a symbol
    let adjacent_positions: HashSet<(usize, usize)> = grid
        .iter()
        .filter_map(|(k, v)| {
            if v == &Cell::Symbol || v == &Cell::Gear {
                Some(k)
            } else {
                None
            }
        })
        .flat_map(expand)
        .collect();
    // the number indices for numbers adjacent to a symbol
    let adjacent_indices: HashSet<_> = adjacent_positions
        .iter()
        .filter_map(|p| number_positions.get(p))
        .collect();
    // the sum of all numbers adjacent to a symbol
    let adjacent_sum: u32 = adjacent_indices.iter().filter_map(|i| numbers.get(i)).sum();

    let gear_ratio_sum: u32 = grid
        .iter()
        .filter_map(|(xy, cell)| if cell == &Cell::Gear { Some(xy) } else { None })
        .filter_map(|gear_xy| {
            let gear_adjacent_indices: HashSet<_> = expand(gear_xy)
                .iter()
                .filter_map(|xy| number_positions.get(xy))
                .collect();
            let mut iter = gear_adjacent_indices.into_iter();
            match (
                iter.next().and_then(|i| numbers.get(i)),
                iter.next().and_then(|i| numbers.get(i)),
                iter.next(),
            ) {
                (Some(a), Some(b), None) => Some(a * b),
                _ => None,
            }
        })
        .sum();

    (adjacent_sum.to_string(), gear_ratio_sum.to_string())
});
