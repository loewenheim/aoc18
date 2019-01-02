use core::iter::once;
use std::env::args;
use std::error::Error;
use std::fs::File;
use std::io::Read;

#[derive(Debug, Clone)]
struct Node {
    children: Vec<Node>,
    meta: Vec<usize>,
}

fn preorder(node: &Node) -> Box<dyn Iterator<Item = &Node> + '_> {
    Box::new(once(node).chain(node.children.iter().flat_map(preorder)))
}

fn parse_tree(numbers: &[usize]) -> Node {
    fn go(numbers: &[usize]) -> (Node, usize) {
        let num_children = numbers[0];
        let num_meta = numbers[1];
        let mut i = 2;

        let mut children = Vec::with_capacity(num_children);

        for _ in 0..num_children {
            let (child, i_new) = go(&numbers[i..]);
            i += i_new;
            children.push(child);
        }

        let meta = numbers[i..i + num_meta].to_vec();

        (Node { children, meta }, i + num_meta)
    }
    go(numbers).0
}

fn value(node: &Node) -> usize {
    if node.children.is_empty() {
        node.meta.iter().sum()
    } else {
        node.meta
            .iter()
            .filter_map(|i| node.children.get(*i-1))
            .map(value)
            .sum()
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let filename = args().nth(1).expect("Expected file name");
    let numbers: Vec<usize> = {
        let mut contents = String::new();
        File::open(filename)?.read_to_string(&mut contents)?;
        contents
            .trim()
            .split(' ')
            .map(|s| s.parse())
            .collect::<Result<Vec<_>, _>>()?
    };

    let tree = parse_tree(&numbers);
    let meta_sum: usize = preorder(&tree).flat_map(|n| n.meta.iter()).sum();

    println!("Part 1: {}", meta_sum);
    println!("Part 2: {}", value(&tree));

    Ok(())
}
