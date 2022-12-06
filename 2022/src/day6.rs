use std::collections::HashSet;

advent_2022::day_function!(6, input, {
    let sequence = input.str().trim();

    let mut answer1: usize = 0;
    for i in 0..(sequence.len() - 3) {
        let s: HashSet<char> = sequence[i..i + 4].chars().collect();
        if s.len() > 3 {
            answer1 = i + 4;
            break;
        }
    }

    let mut answer2: usize = 0;
    for i in 0..(sequence.len() - 13) {
        let s: HashSet<char> = sequence[i..i + 14].chars().collect();
        if s.len() > 13 {
            answer2 = i + 14;
            break;
        }
    }

    (answer1.to_string(), answer2.to_string())
});
