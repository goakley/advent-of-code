use std::collections::VecDeque;

struct Grid<T>(Vec<Vec<T>>);

impl<T> std::ops::Index<Point> for Grid<T> {
    type Output = T;
    fn index(&self, xy: Point) -> &Self::Output {
        &self.0[xy.0 .0][xy.0 .1]
    }
}

impl<T> std::ops::IndexMut<Point> for Grid<T> {
    fn index_mut(&mut self, xy: Point) -> &mut Self::Output {
        let a = &mut self.0[xy.0 .0];
        &mut a[xy.0 .1]
    }
}

impl<T> Grid<T> {
    fn max(&self) -> Point {
        Point((self.0.len() - 1, self.0[0].len() - 1))
    }

    /*fn inside(&self, xy: Point) -> bool {
        let (x, y) = xy.0;
        x >= 0 && y >= 0 && x < self.0.len() && y < self.0[0].len()
    }*/
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Point((usize, usize));

impl Point {
    fn adjacent(&self, max: Point) -> Vec<Point> {
        let (x, y) = self.0;
        let mut v = Vec::with_capacity(4);
        if x > 0 {
            v.push(Point((x - 1, y)));
        }
        if y > 0 {
            v.push(Point((x, y - 1)));
        }
        if x < max.0 .0 {
            v.push(Point((x + 1, y)));
        }
        if y < max.0 .1 {
            v.push(Point((x, y + 1)));
        }
        v
    }
}

fn read(input: advent_2022::Input) -> ((Point, Point), Grid<i16>) {
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
                            start = Some(Point((x, y)));
                            'a'
                        }
                        'E' => {
                            end = Some(Point((x, y)));
                            'z'
                        }
                        _ => c,
                    }) as i16)
                        - ('a' as i16)
                })
                .collect()
        })
        .collect();
    ((start.unwrap(), end.unwrap()), Grid(c))
}

advent_2022::day_function!(12, input, {
    let ((start, end), grid) = read(input);
    let max = grid.max();
    let mut costs: Grid<Option<i16>> = Grid(
        (0..=max.0 .0)
            .map(|x| {
                (0..=max.0 .1)
                    .map(|y| if Point((x, y)) == end { Some(0) } else { None })
                    .collect()
            })
            .collect(),
    );
    let mut queue = VecDeque::new();
    queue.push_back((end, 0));
    while let Some((point, cost)) = queue.pop_front() {
        for p in point
            .adjacent(max)
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
    // closure wouldn't stop trying to move x :|||
    let mut cheapest = start_cost;
    for x in 0..=max.0 .0 {
        for y in 0..=max.0 .1 {
            if grid[Point((x, y))] == 0 {
                cheapest = std::cmp::min(cheapest, costs[Point((x, y))].unwrap_or(cheapest));
            }
        }
    }
    (start_cost.to_string(), cheapest.to_string())
});
