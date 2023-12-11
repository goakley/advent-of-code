use std::collections::BTreeSet as HashSet;

fn expand(distance: i64, mut galaxies: HashSet<(i64, i64)>) -> HashSet<(i64, i64)> {
    let xs: HashSet<i64> = galaxies.iter().map(|(x, _)| *x).collect();
    for (i, cx) in (*xs.iter().min().unwrap()..=*xs.iter().max().unwrap())
        .filter(|x| !xs.contains(x))
        .enumerate()
    {
        galaxies = galaxies
            .into_iter()
            .map(|(x, y)| {
                (
                    if x > (cx + (i as i64 * distance)) {
                        x + distance
                    } else {
                        x
                    },
                    y,
                )
            })
            .collect();
    }
    let ys: HashSet<i64> = galaxies.iter().map(|(_, y)| *y).collect();
    for (i, cy) in (*ys.iter().min().unwrap()..=*ys.iter().max().unwrap())
        .filter(|y| !ys.contains(y))
        .enumerate()
    {
        galaxies = galaxies
            .into_iter()
            .map(|(x, y)| {
                (
                    x,
                    if y > (cy + (i as i64 * distance)) {
                        y + distance
                    } else {
                        y
                    },
                )
            })
            .collect();
    }
    galaxies
}

fn sum_distances(galaxies: &HashSet<(i64, i64)>) -> i64 {
    galaxies
        .iter()
        .flat_map(|(x1, y1)| {
            galaxies
                .iter()
                .map(move |(x2, y2)| (x1 - x2).abs() + (y1 - y2).abs())
        })
        .sum::<i64>()
        / 2
}

advent_2023::day_function!(11, input, {
    let galaxies: HashSet<(i64, i64)> = input
        .grid::<char>()
        .into_iter()
        .filter_map(|((x, y), v)| {
            if v == '#' {
                Some((x as i64, y as i64))
            } else {
                None
            }
        })
        .collect();
    let one = sum_distances(&expand(1, galaxies.clone()));
    let million = sum_distances(&expand(999999, galaxies));
    (one.to_string(), million.to_string())
});
