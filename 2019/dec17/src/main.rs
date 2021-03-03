use aoc_2019_17::solve_a;
use aoc_2019_17::solve_b;
use util::io::get_input;

fn main() {
    let input = get_input();

    println!("a: {}", solve_a(&input));
    println!("b: {}", solve_b(&input));
}
