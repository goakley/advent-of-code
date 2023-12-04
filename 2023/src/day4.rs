use std::collections::{BTreeMap, HashSet};

type Card = (Vec<i32>, Vec<i32>);

fn parse_numbers(input: &str) -> nom::IResult<&str, Vec<i32>> {
    nom::sequence::delimited(
        nom::character::complete::space0,
        nom::multi::separated_list0(
            nom::character::complete::space1,
            nom::character::complete::i32,
        ),
        nom::character::complete::space0,
    )(input)
}

fn parse_card(input: &str) -> nom::IResult<&str, (u32, Card)> {
    nom::sequence::terminated(
        nom::sequence::pair(
            nom::sequence::delimited(
                nom::bytes::complete::tag("Card "),
                nom::sequence::delimited(
                    nom::character::complete::space0,
                    nom::character::complete::u32,
                    nom::character::complete::space0,
                ),
                nom::bytes::complete::tag(": "),
            ),
            nom::sequence::separated_pair(
                parse_numbers,
                nom::character::complete::char('|'),
                parse_numbers,
            ),
        ),
        nom::character::complete::line_ending,
    )(input)
}

fn parse_cards(input: &str) -> nom::IResult<&str, BTreeMap<u32, Card>> {
    nom::combinator::all_consuming(nom::combinator::map(
        nom::multi::many0(parse_card),
        |cards| cards.into_iter().collect(),
    ))(input)
}

advent_2023::day_function!(4, input, {
    let cards = parse_cards(input.str()).unwrap().1;

    let winner_counts: BTreeMap<u32, usize> = cards
        .iter()
        .map(|(i, (winners, results))| {
            let winner_set: HashSet<&i32, std::collections::hash_map::RandomState> =
                HashSet::from_iter(winners.iter());
            let result_set = HashSet::from_iter(results.iter());
            (*i, winner_set.intersection(&result_set).count())
        })
        .collect();

    let card_score_sum: usize = winner_counts
        .values()
        .map(|count| {
            if *count < 1 {
                0
            } else {
                2_usize.pow(*count as u32 - 1)
            }
        })
        .sum();

    let mut instance_counts: BTreeMap<u32, usize> = cards.keys().map(|i| (*i, 1)).collect();
    for (i, count) in winner_counts.iter() {
        for c in 1..=*count {
            let x = *instance_counts.get(i).unwrap_or(&0);
            if let Some(n) = instance_counts.get_mut(&(i + c as u32)) {
                *n += x;
            }
        }
    }
    let card_instance_sum: usize = instance_counts.values().sum();

    (card_score_sum.to_string(), card_instance_sum.to_string())
});
