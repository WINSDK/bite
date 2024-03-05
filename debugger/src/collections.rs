#![allow(dead_code)]

use crate::{Error, Pid};
use std::collections::HashMap;
use std::fmt;

pub struct Node<K, V> {
    value: V,
    children: Vec<K>,
}

impl<K, V> Node<K, V> {
    fn new(value: V) -> Self {
        Self {
            value,
            children: Vec::new(),
        }
    }
}

impl<K, V> std::ops::Deref for Node<K, V> {
    type Target = V;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<K, V> std::ops::DerefMut for Node<K, V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

pub struct Tree<V> {
    root: Option<Pid>,
    nodes: HashMap<Pid, Node<Pid, V>>,
}

impl<V> Tree<V> {
    pub fn new(root_key: Pid, root_value: V) -> Self {
        let mut nodes = HashMap::new();
        nodes.insert(root_key, Node::new(root_value));
        Self {
            root: Some(root_key),
            nodes,
        }
    }

    pub fn root(&self) -> &V {
        self.root.as_ref().and_then(|root| self.nodes.get(root)).unwrap()
    }

    pub fn push(&mut self, parent: Pid, key: Pid, value: V) {
        let parent_node = self.nodes.get_mut(&parent).expect("Failed to find parent.");

        parent_node.children.push(key);
        self.nodes.insert(key, Node::new(value));
    }

    pub fn get(&self, key: Pid) -> Result<&V, Error> {
        self.nodes.get(&key).map(|node| &node.value).ok_or(Error::TraceeLost(key))
    }

    pub fn get_mut(&mut self, key: Pid) -> Result<&mut V, Error> {
        self.nodes
            .get_mut(&key)
            .map(|node| &mut node.value)
            .ok_or(Error::TraceeLost(key))
    }

    pub fn remove(&mut self, key: Pid) {
        let node = self.nodes.remove(&key).expect("Key isn't part of tree");

        // remove a node's children recursively
        for child_key in node.children {
            self.remove(child_key);
        }

        // remove the key from the children of other nodes
        for (_, node) in self.nodes.iter_mut() {
            if let Some(index) = node.children.iter().position(|&k| k == key) {
                node.children.remove(index);
            }
        }

        // update the root if necessary
        if self.root == Some(key) {
            self.root = None;
        }
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    pub fn pids(&self) -> impl Iterator<Item = Pid> + '_ {
        self.nodes.keys().copied()
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.nodes.values().map(|node| (&node.value))
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.nodes.values_mut().map(|node| (&mut node.value))
    }
}

impl<V> fmt::Debug for Tree<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ref root_key) = self.root {
            self.recursive_debug_print(f, root_key, 0)?;
        }

        Ok(())
    }
}

impl<V> Tree<V> {
    fn recursive_debug_print(
        &self,
        f: &mut fmt::Formatter<'_>,
        key: &Pid,
        depth: usize,
    ) -> fmt::Result {
        if let Some(node) = self.nodes.get(key) {
            if depth > 0 {
                f.write_str(" ")?;
            }

            for _ in 0..depth {
                f.write_str("-")?;
            }

            if depth > 0 {
                f.write_str("> ")?;
            }

            f.write_fmt(format_args!("{:?}\n", key))?;

            for child_key in &node.children {
                self.recursive_debug_print(f, child_key, depth + 1)?;
            }
        }

        Ok(())
    }
}
