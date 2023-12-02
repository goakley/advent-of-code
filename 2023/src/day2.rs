use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric1, line_ending, u32 as nomu32};
use nom::combinator::{complete, map};
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, pair, separated_pair, terminated};
use std::collections::HashMap;

/// Turn a game line into structure data
///
/// "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
fn parse_game(input: &str) -> nom::IResult<&str, (u32, Vec<HashMap<&str, u32>>)> {
    pair(
        delimited(tag("Game "), nomu32, tag(": ")),
        separated_list0(
            tag("; "),
            map(
                separated_list0(tag(", "), separated_pair(nomu32, tag(" "), alphanumeric1)),
                |v| v.into_iter().map(|(a, b)| (b, a)).collect(),
            ),
        ),
    )(input)
}

/// Turn multiple game lines into structured data
fn parse_games(input: &str) -> nom::IResult<&str, HashMap<u32, Vec<HashMap<&str, u32>>>> {
    map(complete(many0(terminated(parse_game, line_ending))), |v| {
        v.into_iter().collect()
    })(input)
}

advent_2023::day_function!(2, input, {
    let games = parse_games(input.str()).unwrap().1;

    let conditions = HashMap::from([("red", 12), ("green", 13), ("blue", 14)]);
    let games_sum: u32 = games
        .iter()
        .filter_map(|(i, pulls)| {
            if pulls.iter().all(|pull| {
                conditions
                    .iter()
                    .all(|(color, max)| pull.get(color).unwrap_or(&0) <= max)
            }) {
                Some(i)
            } else {
                None
            }
        })
        .sum();

    let colors = ["red", "green", "blue"];
    let viable_games_power_sum: u32 = games
        .values()
        .map(|pulls| {
            colors
                .iter()
                .map(|color| {
                    *pulls
                        .iter()
                        .filter_map(|pull| pull.get(color))
                        .max()
                        .unwrap_or(&0)
                })
                .product::<u32>()
        })
        .sum();

    (games_sum.to_string(), viable_games_power_sum.to_string())
});
