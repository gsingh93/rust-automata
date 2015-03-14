#![feature(io)]

use std::fmt::Display;

pub mod dfa;
pub mod nfa;

pub use nfa::{NFA, Transition};
pub use dfa::DFA;

pub trait Automaton {
    type State;
    type Alphabet;

    fn run(&self, Vec<Self::Alphabet>) -> Option<Self::State>;
    fn output_graphviz(&self, filename: &str) where Self::State: Display, Self::Alphabet: Display;
}
