struct Grid(Vec<Vec<u8>>);

impl std::ops::Index<(usize, usize)> for Grid {
    type Output = u8;
    fn index(&self, (x, y): (usize, usize)) -> &Self::Output {
        &self.0[x][y]
    }
}

impl Grid {
    fn size(&self) -> (usize, usize) {
        (self.0.len(), self.0[0].len())
    }

    fn border_size(&self) -> u64 {
        (self.0.len() as u64) * 2 + ((self.0[0].len() as u64) - 2) * 2
    }

    fn cardinal_trees(&self, (x, y): (usize, usize)) -> Vec<Vec<u8>> {
        vec![
            (0..x).map(|a| (a,y)).rev().collect(),
            (x+1..self.0.len()).map(|a| (a,y)).collect(),
            (0..y).map(|a| (x,a)).rev().collect(),
            (y+1..self.0[0].len()).map(|a| (x,a)).collect(),
        ].into_iter().map(|is: Vec<(usize,usize)>| is.into_iter().map(|i| self[i]).collect()).collect()
    }
}

fn visibility(grid: &Grid) -> u64 {
    grid.border_size() + (1 .. grid.size().0 - 1).map(|x| {
        (1 .. grid.size().1 - 1).map(|y| {
            grid.cardinal_trees((x, y)).into_iter().any(|ts| ts.into_iter().all(|t| t < grid[(x,y)])) as u64
        }).sum::<u64>()
    }).sum::<u64>()
}

fn scenic(grid: &Grid) -> u64 {
    (0 .. grid.size().0).map(|x| {
        (0 .. grid.size().1).map(|y| {
            grid.cardinal_trees((x, y)).into_iter().map(|ts| {
                ts.iter().enumerate().find_map(|(i, t)| if *t >= grid[(x,y)] { Some(i + 1) } else { None }).unwrap_or(ts.len()) as u64
            }).product()
        }).max().unwrap_or_default()
    }).max().unwrap_or_default()
}

advent_2022::day_function!(7, input, {
    let lines: Vec<String> = input.lines();
    let grid = Grid(lines.into_iter().map(|l| l.chars().map(|c| (c as u8) - b'0').collect()).collect());
    (visibility(&grid).to_string(), scenic(&grid).to_string())
});
