#![allow(dead_code)]

use crate::{Error, Pid, Tracee};
use std::collections::HashMap;
use std::fmt;

pub struct Node<K, V> {
    value: V,
    parent: Option<K>,
    children: Vec<K>,
    to_be_removed: bool,
}

impl<K, V> Node<K, V> {
    fn new(value: V) -> Self {
        Self {
            value,
            parent: None,
            children: Vec::new(),
            to_be_removed: false,
        }
    }

    fn with_parent(value: V, parent: K) -> Self {
        Self {
            value,
            parent: Some(parent),
            children: Vec::new(),
            to_be_removed: false,
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

pub struct Tree {
    root: Option<Pid>,
    nodes: HashMap<Pid, Node<Pid, Tracee>>,
}

impl Tree {
    pub fn new(root_pid: Pid, root_tracee: Tracee) -> Self {
        let mut nodes = HashMap::new();
        nodes.insert(root_pid, Node::new(root_tracee));
        Self {
            root: Some(root_pid),
            nodes,
        }
    }

    pub fn root(&self) -> &Tracee {
        self.root.as_ref().and_then(|root| self.nodes.get(root)).unwrap()
    }

    pub fn push(&mut self, parent: Pid, pid: Pid, tracee: Tracee) {
        let parent_node = self.nodes.get_mut(&parent).expect("Failed to find parent.");

        parent_node.children.push(pid);
        self.nodes.insert(pid, Node::with_parent(tracee, parent));
    }

    pub fn remove(&mut self, pid: Pid) {
        let node = self.nodes.get_mut(&pid).expect("Key isn't part of tree");

        // If node has children, mark it to be removed later and exit.
        if !node.children.is_empty() {
            node.to_be_removed = true;
            return;
        }

        // If the node has a parent, remove this node from the parent's children.
        if let Some(parent_pid) = node.parent {
            let parent_node = self.nodes.get_mut(&parent_pid).unwrap();

            // Remove this child.
            parent_node.children.retain(|&x| x != pid);

            // Additionally, check if parent is marked to_be_removed and has no more children
            if parent_node.to_be_removed {
                self.remove(parent_pid);
            }
        }

        // Remove the node from the tree.
        self.nodes.remove(&pid);

        // Update the root if necessary.
        if self.root == Some(pid) {
            self.root = None;
        }
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    pub fn get(&self, key: Pid) -> Result<&Tracee, Error> {
        self.nodes
            .get(&key)
            .filter(|node| !node.to_be_removed)
            .map(|node| &node.value)
            .ok_or(Error::TraceeLost(key))
    }

    pub fn get_mut(&mut self, key: Pid) -> Result<&mut Tracee, Error> {
        self.nodes
            .get_mut(&key)
            .filter(|node| !node.to_be_removed)
            .map(|node| &mut node.value)
            .ok_or(Error::TraceeLost(key))
    }

    pub fn pids(&self) -> impl Iterator<Item = Pid> + '_ {
        self.nodes
            .iter()
            .filter(|(_, node)| !node.to_be_removed)
            .map(|(pid, _)| *pid)
    }

    pub fn values(&self) -> impl Iterator<Item = &Tracee> {
        self.nodes
            .values()
            .filter(|node| !node.to_be_removed)
            .map(|node| &node.value)
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut Tracee> {
        self.nodes
            .values_mut()
            .filter(|node| !node.to_be_removed)
            .map(|node| &mut node.value)
    }

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

            if node.to_be_removed {
                f.write_fmt(format_args!("{key:?} (dead)\n"))?;
            } else {
                f.write_fmt(format_args!("{key:?}\n"))?;
            }

            for child_key in &node.children {
                self.recursive_debug_print(f, child_key, depth + 1)?;
            }
        }

        Ok(())
    }
}

impl fmt::Debug for Tree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ref root_key) = self.root {
            self.recursive_debug_print(f, root_key, 0)?;
        }

        Ok(())
    }
}
