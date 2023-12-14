use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;

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

pub struct Tree<K: Copy + Hash + Eq, V> {
    root: Option<K>,
    nodes: HashMap<K, Node<K, V>>,
}

impl<K: Copy + Hash + Eq, V> Tree<K, V> {
    pub fn new(root: K, root_value: V) -> Self {
        let mut nodes = HashMap::new();
        nodes.insert(root, Node::new(root_value));
        Self {
            root: Some(root),
            nodes,
        }
    }

    pub fn root(&mut self) -> &mut V {
        self.root.as_ref().and_then(|root| self.nodes.get_mut(root)).unwrap()
    }

    pub fn push_child(&mut self, parent: &K, key: K, value: V) {
        let parent_node = self.nodes.get_mut(parent).expect("Failed to find parent.");

        parent_node.children.push(key);
        self.nodes.insert(key, Node::new(value));
    }

    pub fn get(&mut self, key: &K) -> Option<&V> {
        self.nodes.get_mut(key).map(|node| &node.value)
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.nodes.get_mut(key).map(|node| &mut node.value)
    }

    pub fn remove(&mut self, key: &K) {
        let node = self.nodes.remove(key).expect("Key isn't part of tree");

        // remove a node's children recursively
        for child_key in node.children {
            self.remove(&child_key);
        }

        // remove the key from the children of other nodes
        for (_, node) in self.nodes.iter_mut() {
            if let Some(index) = node.children.iter().position(|k| k == key) {
                node.children.remove(index);
            }
        }

        // update the root if necessary
        if self.root.as_ref() == Some(key) {
            self.root = None;
        }
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.nodes.iter().map(|(_, node)| (&node.value))
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.nodes.iter_mut().map(|(_, node)| (&mut node.value))
    }
}

impl<K: fmt::Debug + Copy + Hash + Eq, V> fmt::Debug for Tree<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ref root_key) = self.root {
            self.recursive_debug_print(f, root_key, 0)?;
        }

        Ok(())
    }
}

impl<K: fmt::Debug + Copy + Hash + Eq, V> Tree<K, V> {
    fn recursive_debug_print(
        &self,
        f: &mut fmt::Formatter<'_>,
        key: &K,
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
