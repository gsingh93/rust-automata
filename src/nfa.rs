use Automaton;
use std::fmt::Display;
use std::fs::OpenOptions;
use std::io::Write;
use std::collections::{HashSet, HashMap, VecDeque};
use std::hash::Hash;
use nfa::Transition::{Input, Epsilon};

#[derive(Debug)]
pub struct NFA<S: Eq + Hash = usize, I: Eq + Hash = char> {
    start: S,
    accept_states: HashSet<S>,
    transitions: HashMap<(S, Transition<I>), HashSet<S>>
}

#[derive(Debug, Hash, Eq, PartialEq, Copy, Clone)]
pub enum Transition<I> {
    Epsilon,
    Input(I)
}

pub struct NFAIter<'a, S: 'a, I: 'a> {
    queue: VecDeque<(&'a S, usize)>,
    input: Vec<I>,
    transitions: &'a HashMap<(S, Transition<I>), HashSet<S>>
}

impl<'a, S: 'a + Hash + Eq + Copy, I: Hash + Eq + Copy> Iterator for NFAIter<'a, S, I> {
    type Item = &'a S;

    fn next(&mut self) -> Option<&'a S> {
        if self.queue.is_empty() {
            None
        } else {
            let (state, pos) = match self.queue.pop_front() {
                Some(s) => s,
                None => panic!("Shouldn't happen")
            };

            if pos < self.input.len() {
                if let Some(set) = self.transitions.get(&(*state, Input(self.input[pos]))) {
                    for item in set {
                        self.queue.push_back((item, pos + 1))
                    }
                }
            }
            if let Some(set) = self.transitions.get(&(*state, Epsilon)) {
                for item in set {
                    self.queue.push_back((item, pos))
                }
            }

            Some(state)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>){
        (0, None) // TODO
    }
}

impl<S: Clone + Eq + Hash, I: Eq + Hash> NFA<S, I> {
    pub fn new(start: S, accept_states: HashSet<S>,
               transitions: HashMap<(S, Transition<I>), HashSet<S>>) -> NFA<S, I> {
        NFA { start: start, accept_states: accept_states, transitions: transitions }
    }

    pub fn get_accept_states(&self) -> &HashSet<S> {
        &self.accept_states
    }

    pub fn get_start_state(&self) -> &S {
        &self.start
    }

    pub fn get_transitions(&self) -> &HashMap<(S, Transition<I>), HashSet<S>> {
        &self.transitions
    }

    pub fn iter(&self, input: Vec<I>) -> NFAIter<S, I> {
        let mut queue = VecDeque::new();
        queue.push_back((&self.start, 0));
        NFAIter { queue: queue, input: input, transitions: &self.transitions }
    }
}

impl<S, I> Automaton for NFA<S, I> where S: Hash + Eq + Copy, I: Hash + Eq + Copy {
    type State = S;
    type Alphabet = I;

    fn run(&self, s: Vec<I>) -> Option<S> {
        let mut queue = VecDeque::new();
        queue.push_back((self.start, 0));
        while !queue.is_empty() {
            let (state, pos) = match queue.pop_front() {
                Some(s) => s,
                None => panic!("Shouldn't happen")
            };

            if let Some(set) = self.transitions.get(&(state, Epsilon)) {
                for item in set {
                    queue.push_back((*item, pos))
                }
            }

            if pos == s.len() {
                if self.accept_states.contains(&state) {
                    return Some(state)
                }
            } else {
                if let Some(set) = self.transitions.get(&(state, Input(s[pos]))) {
                    for item in set {
                        queue.push_back((*item, pos + 1))
                    }
                }
            }
        }
        None
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
        for (trans, states) in self.transitions.iter() {
            for s in states {
                let label = match trans.1 {
                    Epsilon => "&#949;".to_string(),
                    Input(c) => c.to_string()
                };
                write!(&mut f, "\t{} -> {} [ label = \"{}\"]\n", trans.0, s, label).unwrap();
            }
        }
        write!(&mut f, "}}\n").unwrap();
    }
}

#[cfg(test)]
mod test {
    use Automaton;
    use nfa::NFA;
    use nfa::Transition::Input;

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
    fn test_nfa() {
        let transitions = map!((0, Input('a')) => set!(0, 1),
                               (0, Input('b')) => set!(1),
                               (1, Input('a')) => set!(0, 1),
                               (1, Input('b')) => set!(2));
        let nfa = NFA::new(1, set!(2), transitions);
        assert_eq!(nfa.run("aaaaa".chars().collect()), None);
        assert_eq!(nfa.run("aabaa".chars().collect()), None);
        assert_eq!(nfa.run("aababbb".chars().collect()), None);
        assert_eq!(nfa.run("aababb".chars().collect()), Some(2));
        assert_eq!(nfa.run("aabb".chars().collect()), Some(2));
    }
}
