use std::fs;

fn main() {
    let contents =
        fs::read_to_string("./lib/day-01/inp.txt").expect("Should have been able to read the file");

    let mut parsed_data: Vec<i32> = contents
        .split("\n\n")
        .map(|itms| {
            itms.split("\n")
                .map(|itm| itm.parse::<i32>().unwrap())
                .sum()
        })
        .collect();
    parsed_data.sort();
    parsed_data.reverse();

    println!("{:?}", parsed_data[0]);
    println!("{:?}", parsed_data[0] + parsed_data[1] + parsed_data[2]);
}
