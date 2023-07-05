struct Node<K, V> {
    key: K,
    value: V,
    childeren: Vec<Node<K, V>>,
}

impl<K: Ord, V> Node<K, V> {
    fn new(key: K, value: V) -> Self {
        Self {
            key,
            value,
            childeren: Vec::new(),
        }
    }

    fn find_node(&mut self, key: &K) -> Option<&mut Self> {
        if &self.key == key {
            return Some(self);
        }

        self.childeren.iter_mut().find_map(|child| child.find_node(key))
    }
}

pub struct Tree<K: Ord, V> {
    root: Option<Node<K, V>>,
    length: usize,
}

impl<K: Ord + Clone, V> Tree<K, V> {
    pub fn new() -> Self {
        Self {
            root: None,
            length: 0,
        }
    }

    pub fn root(&self) -> K {
        self.root.as_ref().expect("No root.").key.clone()
    }

    #[allow(dead_code)]
    pub fn push_child(&mut self, parent: &K, key: K, value: V) -> Option<()> {
        let root = self.root.as_mut().expect("Can't push a child, if there are no parents.");
        let parent = root.find_node(parent)?;

        parent.childeren.push(Node::new(key, value));
        self.length += 1;

        Some(())
    }

    pub fn push_root(&mut self, key: K, value: V) {
        self.root = Some(Node::new(key, value));
        self.length += 1;
    }

    pub fn find(&mut self, key: &K) -> &mut V {
        let root = self.root.as_mut().expect("Key wasn't found.");
        root.find_node(key).map(|node| &mut node.value).expect("Key wasn't found.")
    }

    pub fn remove(&mut self, key: &K) {
        let root = self.root.as_mut().expect("Key wasn't found");

        if key == &root.key {
            self.root = None;
            self.length = 0;
            return;
        }

        root.find_node(key)
            .expect("Key wasn't found")
            .childeren
            .retain(|c| &c.key != key);

        self.length -= 1;
    }

    pub fn is_empty(&self) -> bool {
        self.length == 0
    }
}
