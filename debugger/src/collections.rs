use std::collections::HashMap;

#[derive(Debug)]
pub struct Node<K, V> {
    value: V,
    parent: Option<K>,
    children: Vec<K>,
}

impl<K, V> Node<K, V> {
    fn new(value: V, parent: Option<K>) -> Self {
        Self {
            value,
            parent,
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
        let parent_node = self.nodes.get_mut(parent).expect("Failed to find parent");

        parent_node.children.push(key.clone());
        self.nodes.insert(key, Node::new(value, Some(parent.clone())));
    }

    pub fn push_root(&mut self, key: K, value: V) {
        self.root = Some(key.clone());
        self.nodes.insert(key, Node::new(value, None));
    }

    pub fn find(&mut self, key: &K) -> Option<&mut V> {
        self.nodes.get_mut(key).map(|node| &mut node.value)
    }

    pub fn remove(&mut self, key: &K) {
        if Some(key) == self.root.as_ref() {
            assert!(
                self.nodes.len() == 1,
                "can't remove root when there are still leafs"
            );

            self.root = None;
        }

        // find parent of node
        let opt_parent = self
            .nodes
            .get(key)
            .expect("value being removed isn't part of the tree")
            .parent
            .clone();

        // if we aren't removing the root
        if let Some(parent) = opt_parent {
            // find the parent node
            let parent_node = self.nodes.get_mut(&parent).unwrap();

            // get the index of the parent's child (the node we're removing)
            let child_of_parent = parent_node.children.iter().position(|k| k == key).unwrap();

            // remove reference
            parent_node.children.remove(child_of_parent);
        }

        self.nodes.remove(key);
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.nodes.iter().map(|(key, node)| (key, &node.value))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&K, &mut V)> {
        self.nodes.iter_mut().map(|(key, node)| (key, &mut node.value))
    }

    pub fn into_iter(&mut self) -> impl Iterator<Item = (K, V)> {
        let nodes = std::mem::take(&mut self.nodes);
        self.root = None;

        nodes.into_iter().map(|(key, node)| (key, node.value))
    }
}
