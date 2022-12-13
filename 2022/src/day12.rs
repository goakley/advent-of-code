use std::collections::VecDeque;

use advent_2022::{Grid, Input, Point};

fn read(input: Input) -> ((Point, Point), Grid<i16>) {
    let mut start = None;
    let mut end = None;
    let c: Vec<Vec<i16>> = input
        .lines::<String>()
        .into_iter()
        .enumerate()
        .map(|(x, s)| {
            s.chars()
                .enumerate()
                .map(|(y, c)| {
                    ((match c {
                        'S' => {
                            start = Some((x, y).into());
                            'a'
                        }
                        'E' => {
                            end = Some((x, y).into());
                            'z'
                        }
                        _ => c,
                    }) as i16)
                        - ('a' as i16)
                })
                .collect()
        })
        .collect();
    ((start.unwrap(), end.unwrap()), c.into())
}

advent_2022::day_function!(12, input, {
    let ((start, end), grid) = read(input);
    let max = grid.max();
    let mut costs: Grid<Option<i16>> = grid.map(|p, _| if p == &end { Some(0) } else { None });
    let mut queue = VecDeque::new();
    queue.push_back((end, 0));
    while let Some((point, cost)) = queue.pop_front() {
        for p in point
            .adjacent(&max)
            .into_iter()
            .filter(|p| grid[point] - grid[*p] <= 1)
        {
            let newcost = cost + 1;
            if costs[p].map(|c| newcost < c).unwrap_or(true) {
                costs[p] = Some(newcost);
                queue.push_back((p, newcost));
            }
        }
    }
    let start_cost = costs[start].unwrap();
    let cheapest = grid
        .into_iter()
        .filter_map(|(p, t)| {
            if t == 0 {
                Some(costs[p].unwrap_or(start_cost))
            } else {
                None
            }
        })
        .min()
        .unwrap();
    (start_cost.to_string(), cheapest.to_string())
});
