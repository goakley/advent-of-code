use std::collections::HashMap;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum SpringState {
    Operational,
    Damaged,
    Unknown,
}

enum GroupExpectation {
    Any,
    Damaged,
    Operational,
}

impl GroupExpectation {
    fn get(group_len: u64, k: u64) -> Self {
        if k == 0 {
            Self::Any
        } else if k < group_len {
            Self::Damaged
        } else {
            Self::Operational
        }
    }
}

fn process(springs: &[SpringState], groups: &[u64]) -> u64 {
    let mut table: HashMap<(usize, usize, u64), u64> = HashMap::new();
    let kmax = *groups.iter().max().unwrap_or(&0);
    for k in 0..=kmax {
        table.insert((springs.len(), groups.len(), k), 1);
    }
    for j in 0..groups.len() {
        for k in 0..=kmax {
            table.insert((springs.len(), j, k), 0);
        }
    }
    table.insert(
        (springs.len(), groups.len() - 1, groups[groups.len() - 1]),
        1,
    );
    for i in 0..springs.len() {
        for k in 0..=kmax {
            table.insert(
                (i, groups.len(), k),
                if springs[i..].iter().all(|s| s != &SpringState::Damaged) {
                    1
                } else {
                    0
                },
            );
        }
    }
    for i in 0..springs.len() {
        table.insert(
            (i, groups.len() - 1, groups[groups.len() - 1]),
            if springs[i..].iter().all(|s| s != &SpringState::Damaged) {
                1
            } else {
                0
            },
        );
    }
    for i in (0..springs.len()).rev() {
        for j in (0..groups.len()).rev() {
            for k in 0..=groups[j] {
                let group_expectation = GroupExpectation::get(groups[j], k);
                table.insert(
                    (i, j, k),
                    match (springs[i], group_expectation) {
                        (SpringState::Operational, GroupExpectation::Damaged) => 0,
                        (SpringState::Operational, _) => {
                            table[&(i + 1, j + (if k == 0 { 0 } else { 1 }), 0)]
                        }
                        (SpringState::Damaged, GroupExpectation::Operational) => 0,
                        (SpringState::Damaged, _) => table[&(i + 1, j, k + 1)],
                        (SpringState::Unknown, GroupExpectation::Damaged) => {
                            table[&(i + 1, j, k + 1)]
                        }
                        (SpringState::Unknown, GroupExpectation::Operational) => {
                            table[&(i + 1, j + (if k == 0 { 0 } else { 1 }), 0)]
                        }
                        (SpringState::Unknown, GroupExpectation::Any) => {
                            table[&(i + 1, j, k + 1)]
                                + table[&(i + 1, j + (if k == 0 { 0 } else { 1 }), 0)]
                        }
                    },
                );
            }
        }
    }
    table[&(0, 0, 0)]
}

type Line = (Vec<SpringState>, Vec<u64>);

fn parse_line(input: &str) -> nom::IResult<&str, Line> {
    nom::sequence::separated_pair(
        nom::multi::many0(nom::branch::alt((
            nom::combinator::value(
                SpringState::Operational,
                nom::character::complete::char('.'),
            ),
            nom::combinator::value(SpringState::Damaged, nom::character::complete::char('#')),
            nom::combinator::value(SpringState::Unknown, nom::character::complete::char('?')),
        ))),
        nom::character::complete::space1,
        nom::multi::separated_list0(
            nom::character::complete::char(','),
            nom::character::complete::u64,
        ),
    )(input)
}

fn parse_input(input: &str) -> nom::IResult<&str, Vec<Line>> {
    nom::combinator::all_consuming(nom::multi::many0(nom::sequence::terminated(
        parse_line,
        nom::character::complete::line_ending,
    )))(input)
}

advent_2023::day_function!(12, input, {
    let lines = parse_input(input.str()).unwrap().1;
    let sum: u64 = lines
        .iter()
        .map(|(springs, groups)| process(springs, groups))
        .sum();
    let expanded_lines: Vec<Line> = lines
        .into_iter()
        .map(|(springs, groups)| {
            // tfw no stable `intersperse()` for iterators
            let len = springs.len();
            (
                std::iter::repeat(springs)
                    .take(5)
                    .flat_map(|mut v| {
                        v.push(SpringState::Unknown);
                        v
                    })
                    .take(len * 5 + 4)
                    .collect(),
                std::iter::repeat(groups).take(5).flatten().collect(),
            )
        })
        .collect();
    let expanded_sum: u64 = expanded_lines
        .iter()
        .map(|(springs, groups)| process(springs, groups))
        .sum();
    (sum.to_string(), expanded_sum.to_string())
});
