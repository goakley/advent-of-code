/// Convert a number (digit or word) to a u32, consuming the numeric representation
fn parse_number(input: &str) -> nom::IResult<&str, u32> {
    return nom::branch::alt((
        nom::combinator::value(0, nom::bytes::complete::tag_no_case("zero")),
        nom::combinator::value(1, nom::bytes::complete::tag_no_case("one")),
        nom::combinator::value(2, nom::bytes::complete::tag_no_case("two")),
        nom::combinator::value(3, nom::bytes::complete::tag_no_case("three")),
        nom::combinator::value(4, nom::bytes::complete::tag_no_case("four")),
        nom::combinator::value(5, nom::bytes::complete::tag_no_case("five")),
        nom::combinator::value(6, nom::bytes::complete::tag_no_case("six")),
        nom::combinator::value(7, nom::bytes::complete::tag_no_case("seven")),
        nom::combinator::value(8, nom::bytes::complete::tag_no_case("eight")),
        nom::combinator::value(9, nom::bytes::complete::tag_no_case("nine")),
        nom::combinator::map_opt(nom::character::complete::anychar, |c| c.to_digit(10)),
    ))(input);
}

/// Get all the numbers (digit or word) from some text
fn identify_numbers(input: &str) -> Vec<u32> {
    // peek and advance 1 character to allow for overlapping number words
    nom::combinator::complete(nom::multi::many0(nom::sequence::terminated(
        nom::combinator::peek(nom::combinator::opt(parse_number)),
        nom::character::complete::anychar,
    )))(input)
    .unwrap()
    .1
    .into_iter()
    .filter_map(|x| x)
    .collect()
}

advent_2023::day_function!(1, input, {
    let lines: Vec<String> = input.lines();

    let sum_digit: u32 = lines
        .iter()
        .map(|s| {
            let mut numbers = s.chars().filter_map(|c| c.to_digit(10)).peekable();
            numbers.peek().unwrap_or(&0) * 10 + numbers.last().unwrap_or(0)
        })
        .sum();

    let sum_character: u32 = lines
        .iter()
        .map(|s| {
            let numbers = identify_numbers(s);
            numbers.first().unwrap_or(&0) * 10 + numbers.last().unwrap_or(&0)
        })
        .sum();

    (sum_digit.to_string(), sum_character.to_string())
});
