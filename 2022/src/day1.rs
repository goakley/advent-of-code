/*
pub fn solve(inp: advent_2022::Input) {
*/
advent_2022::day_function!(1, input, {
    let mut groups: Vec<i64> = input.groups().iter().map(|nums| nums.iter().sum()).collect();
    let max: i64 = groups.clone().into_iter().max().unwrap_or(0);

    groups.sort_by_key(|x| -x);
    let sum = &groups[0..std::cmp::min(3, groups.len())]
        .iter()
        .fold(0i64, |s, v| s + v);

    (max.to_string(), sum.to_string())
});
