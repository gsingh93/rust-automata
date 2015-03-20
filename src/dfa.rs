use Automaton;
use std::fmt::Display;
use std::io::Write;
use std::fs::OpenOptions;
use std::collections::{HashSet, HashMap};
use std::hash::Hash;

pub struct DFA<S: Eq + PartialEq + Hash = usize, I: Eq + PartialEq + Hash = char> {
    start: S,
    accept_states: HashSet<S>,
    transitions: HashMap<(S, I), S>
}

impl<S: Eq + Hash, I: Eq + Hash> DFA<S, I> {
    pub fn new(start: S, accept_states: HashSet<S>, transitions: HashMap<(S, I), S>) -> DFA<S, I> {
        DFA { start: start, accept_states: accept_states, transitions: transitions }
    }
}

pub struct DFAIter<'a, S: 'a, I: 'a> {
    input: Vec<I>,
    transitions: &'a HashMap<(S, I), S>,
    pos: usize,
    cur_state: &'a S
}

impl<'a, S: 'a + Hash + Eq + Copy, I: Hash + Eq + Copy> Iterator for DFAIter<'a, S, I> {
    type Item = &'a S;

    fn next(&mut self) -> Option<&'a S> {
        if self.pos > self.input.len() {
            None
        } else if self.pos == self.input.len() {
            self.pos += 1;
            Some(self.cur_state)
        } else {
            let c = self.input[self.pos];
            match self.transitions.get(&(*self.cur_state, c)) {
                Some(s) => {
                    self.pos += 1;
                    let ret = self.cur_state;
                    self.cur_state = &s;
                    Some(ret)
                },
                None => {
                    // Watch out for overflow
                    assert!(self.input.len() < self.input.len() + 1);

                    // Skip the rest of the input
                    self.pos = self.input.len() + 1;

                    Some(self.cur_state)
                }
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>){
        (self.input.len(), Some(self.input.len()))
    }
}

impl<S: Eq + Hash, I: Eq + Hash> DFA<S, I> {
    pub fn iter(&self, input: Vec<I>) -> DFAIter<S, I> {
        DFAIter { input: input, transitions: &self.transitions, cur_state: &self.start, pos: 0 }
    }
}

impl<S, I> Automaton for DFA<S, I> where S: Hash + Eq + Copy, I: Hash + Eq + Copy {
    type State = S;
    type Alphabet = I;

    fn run(&self, s: Vec<I>) -> Option<S> {
        let mut cur_state = self.start;
        for c in s {
            match self.transitions.get(&(cur_state, c)) {
                Some(s) => cur_state = *s,
                None => return None
            }
        }
        if self.accept_states.contains(&cur_state) {
            Some(cur_state)
        } else {
            None
        }
    }

    fn output_graphviz(&self, filename: &str) where S: Display, I: Display {
        let mut options = OpenOptions::new();
        let mut f = options.truncate(true).create(true).write(true).open(filename).unwrap();
        write!(&mut f, "digraph nfa {{\n").unwrap();
        write!(&mut f, "\tnode [shape = doublecircle]; ").unwrap();
        for state in self.accept_states.iter() {
            write!(&mut f, "{} ", state).unwrap();
        }
        write!(&mut f, ";\n").unwrap();
        write!(&mut f, "\tnode [shape = circle];\n").unwrap();
        for (trans, state) in self.transitions.iter() {
            write!(&mut f, "\t{} -> {} [ label = \"{}\"]\n", trans.0, state, trans.1).unwrap();
        }
        write!(&mut f, "}}\n").unwrap();
    }
}

#[cfg(test)]
mod test {
    use Automaton;
    use dfa::DFA;

    macro_rules! set {
        ($($elem:expr),*) => ({
            let mut s = ::std::collections::HashSet::new();
            $(s.insert($elem);)*
            s
        })
    }

    macro_rules! map {
        ($($key:expr => $val:expr),*) => ({
            let mut h = ::std::collections::HashMap::new();
            $(h.insert($key, $val);)*
            h
        })
    }

    #[test]
    fn test_dfa() {
        let transitions = map!((0, 'a') => 0, (0, 'b') => 1, (1, 'a') => 0, (1, 'b') => 2);
        let dfa = DFA::new(0, set!(2), transitions);
        assert_eq!(dfa.run("aaaaa".chars().collect()), None);
        assert_eq!(dfa.run("aabaa".chars().collect()), None);
        assert_eq!(dfa.run("aababbb".chars().collect()), None);
        assert_eq!(dfa.run("aababb".chars().collect()), Some(2));
        assert_eq!(dfa.run("aabb".chars().collect()), Some(2));
    }

    #[test]
    fn test_iter() {
        let transitions = map!((0, 'a') => 0, (0, 'b') => 1, (1, 'a') => 0, (1, 'b') => 2);
        let dfa = DFA::new(0, set!(2), transitions);
        let mut it = dfa.iter("aababbb".chars().collect());
        assert_eq!(it.next(), Some(&0));
        assert_eq!(it.next(), Some(&0));
        assert_eq!(it.next(), Some(&0));
        assert_eq!(it.next(), Some(&1));
        assert_eq!(it.next(), Some(&0));
        assert_eq!(it.next(), Some(&1));
        assert_eq!(it.next(), Some(&2));
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }
}
