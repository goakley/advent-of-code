fn parse_row(input: &str) -> nom::IResult<&str, Vec<i64>> {
    nom::sequence::terminated(
        nom::sequence::preceded(
            nom::sequence::tuple((
                nom::character::complete::alphanumeric1,
                nom::character::complete::char(':'),
                nom::character::complete::space1,
            )),
            nom::multi::separated_list0(
                nom::character::complete::space1,
                nom::character::complete::i64,
            ),
        ),
        nom::character::complete::line_ending,
    )(input)
}

fn parse_input(input: &str) -> nom::IResult<&str, Vec<(i64, i64)>> {
    nom::combinator::map(
        nom::combinator::all_consuming(nom::sequence::pair(parse_row, parse_row)),
        |(times, distances)| times.into_iter().zip(distances).collect(),
    )(input)
}

fn parse_row_fixed(input: &str) -> nom::IResult<&str, i64> {
    nom::sequence::terminated(
        nom::sequence::preceded(
            nom::sequence::tuple((
                nom::character::complete::alphanumeric1,
                nom::character::complete::char(':'),
                nom::character::complete::space1,
            )),
            nom::combinator::map_res(
                nom::multi::separated_list0(
                    nom::character::complete::space1,
                    nom::character::complete::digit1,
                ),
                |strs| strs.join("").parse(),
            ),
        ),
        nom::character::complete::line_ending,
    )(input)
}

fn parse_input_fixed(input: &str) -> nom::IResult<&str, (i64, i64)> {
    nom::combinator::all_consuming(nom::sequence::pair(parse_row_fixed, parse_row_fixed))(input)
}

fn binary_search<F>(func: F, target: i64, range: (i64, i64)) -> i64
where
    F: Fn(i64) -> i64,
{
    let (left, right) = range;
    let middle = (left + right) / 2;
    let l = func(left);
    let r = func(right);
    let m = func(middle);
    assert!(l.cmp(&target) != r.cmp(&target));
    assert!(l != target && r != target);
    if middle == left {
        // we can't bisect anymore, pick one of the two numbers in the current range
        if l > target {
            left
        } else {
            assert!(r > target);
            right
        }
    } else if m == target {
        // we've found the exact target, pick the correct adjacent value
        if func(middle + 1) > target {
            middle + 1
        } else if func(middle - 1) > target {
            middle - 1
        } else {
            panic!("unlikely")
        }
    } else if m.cmp(&target) == l.cmp(&target) {
        binary_search(func, target, (middle, right))
    } else if m.cmp(&target) == r.cmp(&target) {
        binary_search(func, target, (left, middle))
    } else {
        // the input must be super messed up to get here
        panic!("surely this is impossible")
    }
}

advent_2023::day_function!(6, input, {
    let races = parse_input(input.str()).unwrap().1;
    let product: usize = races
        .iter()
        .map(|(time, distance)| {
            (0..=*time)
                .map(|speed| speed * (time - speed))
                .filter(|d| d > distance)
                .count()
        })
        .product();

    let (time, distance) = parse_input_fixed(input.str()).unwrap().1;
    assert!((time / 2) * (time - (time / 2)) > distance);
    let left = binary_search(|s: i64| s * (time - s), distance, (0, time / 2));
    let right = binary_search(|s: i64| s * (time - s), distance, (time / 2, time));
    let combinations = (right - left) + 1;

    (product.to_string(), combinations.to_string())
});
