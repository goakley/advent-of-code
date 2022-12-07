use std::collections::HashMap;

#[derive(Debug)]
enum Line {
    ChDirUp,
    ChDirRoot,
    ChDir(String),
    List,
    File(i64, String),
    Dir(String),
}

impl std::str::FromStr for Line {
    type Err = std::convert::Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(nom::branch::alt((
            nom::combinator::map(nom::bytes::complete::tag("$ cd .."), |_: &str| {
                Line::ChDirUp
            }),
            nom::combinator::map(nom::bytes::complete::tag("$ cd /"), |_: &str| {
                Line::ChDirRoot
            }),
            nom::combinator::map(
                nom::sequence::preceded(nom::bytes::complete::tag("$ cd "), nom::combinator::rest),
                |s: &str| Line::ChDir(s.to_string()),
            ),
            nom::combinator::map(nom::bytes::complete::tag("$ ls"), |_: &str| Line::List),
            nom::combinator::map(
                nom::sequence::separated_pair(
                    nom::character::complete::i64::<_, nom::error::VerboseError<_>>,
                    nom::bytes::complete::tag(" "),
                    nom::combinator::rest,
                ),
                |(i, s): (i64, &str)| Line::File(i, s.to_string()),
            ),
            nom::combinator::map(
                nom::sequence::preceded(nom::bytes::complete::tag("dir "), nom::combinator::rest),
                |s: &str| Line::Dir(s.to_string()),
            ),
        ))(s)
        .unwrap()
        .1)
    }
}

fn add(value: i64, map: &mut HashMap<Vec<String>, i64>, key: Vec<String>) {
    map.entry(key).and_modify(|x| *x += value).or_insert(value);
}

fn build(lines: &[Line]) -> HashMap<Vec<String>, i64> {
    let mut sizes: HashMap<Vec<String>, i64> = HashMap::default();
    let mut cwd: Vec<String> = Vec::default();
    sizes.insert(cwd.clone(), 0);
    for line in lines.iter() {
        match line {
            Line::ChDir(dir) => {
                cwd.push(dir.clone());
            }
            Line::ChDirUp => {
                cwd.pop();
            }
            Line::ChDirRoot => {
                cwd.clear();
            }
            Line::List => {}
            Line::File(size, _) => {
                let mut up = cwd.clone();
                while !up.is_empty() {
                    add(*size, &mut sizes, up.clone());
                    up.pop();
                }
                add(*size, &mut sizes, up);
            }
            Line::Dir(_) => {}
        }
    }
    sizes
}

advent_2022::day_function!(7, input, {
    let lines = input.lines();
    let sizes = build(&lines);
    let sum: i64 = sizes.values().filter(|x| *x <= &100000).sum();
    let unused: i64 = 70000000 - sizes[&Vec::default()];
    let missing = 30000000 - unused;
    if missing <= 0 {
        panic!();
    }
    let smallest = sizes
        .iter()
        .filter(|(_, x)| **x >= missing)
        .min_by_key(|(_, x)| **x)
        .unwrap()
        .1;
    (sum.to_string(), smallest.to_string())
});
