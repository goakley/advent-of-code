use advent_2022::{Grid, Point};

fn visibility(grid: &Grid<u8>) -> u64 {
    let min = Point(1, 1);
    let max = grid.max();
    let max = Point(max.0 - 1, max.1 - 1);
    grid.border_size()
        + grid
            .iter()
            .filter(|(p, _)| p.within(&min, &max))
            .map(|(p, _)| {
                grid.cardinal_trees(&p)
                    .into_iter()
                    .any(|ts| ts.into_iter().all(|t| t < grid[p])) as u64
            })
            .sum::<u64>()
}

fn scenic(grid: Grid<u8>) -> u64 {
    grid.iter()
        .map(|(p, _)| {
            grid.cardinal_trees(&p)
                .into_iter()
                .map(|ts| {
                    ts.iter()
                        .enumerate()
                        .find_map(|(i, t)| if *t >= grid[p] { Some(i + 1) } else { None })
                        .unwrap_or(ts.len()) as u64
                })
                .product()
        })
        .max()
        .unwrap_or_default()
}

advent_2022::day_function!(7, input, {
    let lines: Vec<String> = input.lines();
    let grid: Grid<u8> = lines
        .into_iter()
        .map(|l| l.chars().map(|c| (c as u8) - b'0').collect())
        .collect::<Vec<Vec<u8>>>()
        .into();

    (visibility(&grid).to_string(), scenic(grid).to_string())
});
