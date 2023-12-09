/// Turn a list of values into a list of the deltas between each value
///
/// Returns `None` if the diffs are all zero (there are no deltas)
fn collapse(values: &[i64]) -> Option<Vec<i64>> {
    let deltas: Vec<i64> = values
        .iter()
        .zip(values[1..].iter())
        .map(|(a, b)| b - a)
        .collect();
    if deltas.iter().all(|x| x == &0) {
        None
    } else {
        Some(deltas)
    }
}

/// Turn a list of values into its sequence of collapses (including the initial list)
///
/// The result does not include the zero-delta list.
///
/// e.g. `[0 3 6 9]` -> `[[0 3 6 9], [3 3 3]]`
fn collapse_all(values: Vec<i64>) -> Vec<Vec<i64>> {
    std::iter::successors(Some(values), |v| collapse(v.as_ref())).collect()
}

/// Extrapolate the next value in a sequence
fn next_value(values: &[i64]) -> i64 {
    collapse_all(values.to_vec())
        .iter()
        .rev()
        .fold(0, |a, v| a + v.last().expect("undefined"))
}

/// Extrapolate the previous value in a sequence
fn previous_value(values: &[i64]) -> i64 {
    collapse_all(values.to_vec())
        .iter()
        .rev()
        .fold(0, |a, v| v.first().expect("undefined") - a)
}

fn parse_input(input: &str) -> nom::IResult<&str, Vec<Vec<i64>>> {
    nom::combinator::all_consuming(nom::multi::many0(nom::sequence::terminated(
        nom::multi::separated_list0(
            nom::character::complete::space1,
            nom::character::complete::i64,
        ),
        nom::character::complete::line_ending,
    )))(input)
}

advent_2023::day_function!(9, input, {
    let data = parse_input(input.str()).unwrap().1;
    let next_value_sum: i64 = data.iter().map(|v| next_value(v)).sum();
    let previous_value_sum: i64 = data.iter().map(|v| previous_value(v)).sum();
    (next_value_sum.to_string(), previous_value_sum.to_string())
});
