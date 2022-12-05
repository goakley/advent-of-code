use std::collections::{HashMap, LinkedList};

struct Move {
    count: i64,
    from: char,
    to: char,
}

fn read(s: &str) -> (HashMap<char, LinkedList<char>>, Vec<Move>) {
    let (s, crates): (&str, Vec<Vec<Option<char>>>) = nom::multi::many1(nom::sequence::terminated(
        nom::multi::separated_list1(
            nom::bytes::complete::tag(" "),
            nom::branch::alt((
                nom::combinator::map(
                    nom::sequence::delimited(
                        nom::bytes::complete::tag("["),
                        nom::character::complete::anychar::<_, nom::error::VerboseError<_>>,
                        nom::bytes::complete::tag("]"),
                    ),
                    Some,
                ),
                nom::combinator::map(nom::bytes::complete::tag("   "), |_| None),
            )),
        ),
        nom::character::complete::line_ending,
    ))(s)
    .unwrap();
    let mut stacks: Vec<LinkedList<char>> = crates[0].iter().map(|_| LinkedList::new()).collect();
    for row in crates.iter() {
        for (i, col) in row.iter().enumerate() {
            match col {
                Some(c) => stacks[i].push_front(*c),
                None => {}
            }
        }
    }
    let (s, labels): (&str, Vec<char>) = nom::sequence::terminated(
        nom::multi::separated_list1(
            nom::bytes::complete::tag(" "),
            nom::sequence::delimited(
                nom::character::complete::char(' '),
                nom::character::complete::anychar::<_, nom::error::VerboseError<_>>,
                nom::character::complete::char(' '),
            ),
        ),
        nom::character::complete::line_ending,
    )(s)
    .unwrap();
    let dock: HashMap<char, LinkedList<char>> =
        labels.into_iter().zip(stacks.into_iter()).collect();
    let (s, _) =
        nom::character::complete::line_ending::<_, nom::error::VerboseError<_>>(s).unwrap();
    let (s, moves): (&str, Vec<Move>) = nom::multi::many1(nom::sequence::terminated(
        nom::combinator::map(
            nom::sequence::separated_pair(
                nom::sequence::preceded(
                    nom::bytes::complete::tag("move "),
                    nom::character::complete::i64::<_, nom::error::VerboseError<_>>,
                ),
                nom::bytes::complete::tag(" from "),
                nom::sequence::separated_pair(
                    nom::character::complete::anychar,
                    nom::bytes::complete::tag(" to "),
                    nom::character::complete::anychar,
                ),
            ),
            |(count, (from, to))| Move { count, from, to },
        ),
        nom::character::complete::line_ending,
    ))(s)
    .unwrap();
    nom::combinator::eof::<_, nom::error::VerboseError<_>>(s).unwrap();
    (dock, moves)
}

fn show_dock(dock: HashMap<char, LinkedList<char>>) -> String {
    let mut pairs: Vec<(char, char)> = dock
        .iter()
        .filter_map(|(k, v)| v.back().map(|i| (*k, *i)))
        .collect();
    pairs.sort();
    pairs.iter().map(|(_, v)| v).collect()
}

advent_2022::day_function!(5, input, {
    let (mut dock1, moves) = read(input.str());
    let mut dock2 = dock1.clone();

    for &Move { count, from, to } in moves.iter() {
        let mut inter = LinkedList::new();
        let fromer = dock1.get_mut(&from).unwrap();
        for _ in 0..count {
            inter.push_back(fromer.pop_back().unwrap());
        }
        let toer = dock1.get_mut(&to).unwrap();
        for _ in 0..count {
            toer.push_back(inter.pop_front().unwrap());
        }
    }

    for &Move { count, from, to } in moves.iter() {
        let mut inter = LinkedList::new();
        let fromer = dock2.get_mut(&from).unwrap();
        for _ in 0..count {
            inter.push_back(fromer.pop_back().unwrap());
        }
        let toer = dock2.get_mut(&to).unwrap();
        for _ in 0..count {
            toer.push_back(inter.pop_back().unwrap());
        }
    }

    (show_dock(dock1), show_dock(dock2))
});
