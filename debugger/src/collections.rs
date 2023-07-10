use std::collections::HashMap;

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Tree<K, V> {
    root: Option<K>,
    nodes: HashMap<K, Node<K, V>>,
}

impl<K: Clone + std::hash::Hash + Eq, V: Clone> Tree<K, V> {
    pub fn new() -> Self {
        Self {
            root: None,
            nodes: HashMap::new(),
        }
    }

    pub fn root(&self) -> K {
        self.root.as_ref().expect("No root").clone()
    }

    pub fn push_child(&mut self, parent: &K, key: K, value: V) {
        let parent = self.nodes.get_mut(parent).expect("Failed to find parent");

        parent.children.push(key.clone());
        self.nodes.insert(key, Node::new(value));
    }

    pub fn push_root(&mut self, key: K, value: V) {
        self.root = Some(key.clone());
        self.nodes.insert(key, Node::new(value));
    }

    pub fn find(&mut self, key: &K) -> &mut V {
        &mut self.nodes.get_mut(key).expect("Failed a find()").value
    }

    pub fn remove(&mut self, key: &K) {
        if Some(key) == self.root.as_ref() {
            self.root = None;
        }

        self.nodes.remove(key);
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    #[allow(dead_code)]
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.nodes.iter().map(|(key, node)| (key, &node.value))
    }

    #[allow(dead_code)]
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&K, &mut V)> {
        self.nodes.iter_mut().map(|(key, node)| (key, &mut node.value))
    }
}
