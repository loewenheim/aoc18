use ring_queue::Ring;

type Marbles = Ring<usize>;

fn insert_marble(marbles: &mut Marbles, marble: usize) -> usize {
    if marble % 23 == 0 {
        marbles.rotate(7);
        marble + marbles.pop_left().unwrap()
    } else {
        marbles.rotate(-2);
        marbles.push_left(marble);
        0
    }
}

fn play(num_players: usize, num_marbles: usize) -> Vec<usize> {
    let mut result = vec![0; num_players];
    let mut marbles = Ring::with_capacity(num_marbles);
    marbles.push(0);

    for i in 0..num_marbles {
        result[i % num_players] += insert_marble(&mut marbles, i + 1);
    }

    result
}

fn main() {
    println!("Part 1: {}", play(468, 71_010).iter().max().unwrap());
    println!("Part 2: {}", play(468, 7_101_000).iter().max().unwrap());
}
