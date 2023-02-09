use std::fs;

enum Play {
    Rock,
    Scissor,
    Paper,
}

impl Play {
    fn to_play(play: &str) -> Play {
        match play {
            "A" | "X" => Play::Rock,
            "B" | "Y" => Play::Paper,
            "C" | "Z" => Play::Scissor,
            _ => panic!("Oh no!"),
        }
    }

    fn play_points(&self) -> u32 {
        match self {
            Play::Scissor => 3,
            Play::Paper => 2,
            Play::Rock => 1,
        }
    }

    fn outcome(&self, enemy_play: &Play) -> Outcome {
        match (self, enemy_play) {
            (Play::Rock, Play::Paper)
            | (Play::Scissor, Play::Rock)
            | (Play::Paper, Play::Scissor) => Outcome::Win,
            (Play::Paper, Play::Paper)
            | (Play::Rock, Play::Rock)
            | (Play::Scissor, Play::Scissor) => Outcome::Draw,
            (Play::Paper, Play::Rock)
            | (Play::Rock, Play::Scissor)
            | (Play::Scissor, Play::Paper) => Outcome::Lose,
        }
    }
}

enum Outcome {
    Win,
    Lose,
    Draw,
}

impl Outcome {
    fn to_outcome(play: &str) -> Outcome {
        match play {
            "X" => Outcome::Lose,
            "Y" => Outcome::Draw,
            "Z" => Outcome::Win,
            _ => panic!("Oh no!"),
        }
    }

    fn when_my_enemy_plays(&self, enemy_play: &Play) -> Play {
        match (enemy_play, self) {
            (Play::Rock, Outcome::Win) => Play::Paper,
            (Play::Paper, Outcome::Win) => Play::Scissor,
            (Play::Scissor, Outcome::Win) => Play::Rock,
            (Play::Rock, Outcome::Lose) => Play::Scissor,
            (Play::Paper, Outcome::Lose) => Play::Rock,
            (Play::Scissor, Outcome::Lose) => Play::Paper,
            (Play::Rock, Outcome::Draw) => Play::Rock,
            (Play::Paper, Outcome::Draw) => Play::Paper,
            (Play::Scissor, Outcome::Draw) => Play::Scissor,
        }
    }

    fn outcome_points(&self) -> u32 {
        match self {
            Outcome::Win => 6,
            Outcome::Draw => 3,
            Outcome::Lose => 0,
        }
    }
}

pub fn main() -> () {
    let inp = fs::read_to_string("../inputs/day-02/inp.txt")
        .expect("Should have been able to read the file");

    part1(inp.clone());
    part2(inp.clone());
}

fn part1(input: String) {
    let parsed_data: u32 = input
        .split("\n")
        .map(|itms| {
            let (enemy_play, my_play) = itms.split_once(" ").unwrap();
            let enemy_play = Play::to_play(enemy_play);
            let my_play = Play::to_play(my_play);
            let outcome = enemy_play.outcome(&my_play);

            outcome.outcome_points() + my_play.play_points()
        })
        .sum();

    println!("Day 02 - Part 01 - {}", parsed_data)
}

fn part2(input: String) {
    let parsed_data: u32 = input
        .split("\n")
        .map(|itms| {
            let (enemy_play, my_outcome) = itms.split_once(" ").unwrap();

            let enemy_play = Play::to_play(enemy_play);
            let my_outcome = Outcome::to_outcome(my_outcome);
            let my_play = my_outcome.when_my_enemy_plays(&enemy_play);

            my_outcome.outcome_points() + my_play.play_points()
        })
        .sum();

    println!("Day 02 - Part 02 - {}", parsed_data)
}
