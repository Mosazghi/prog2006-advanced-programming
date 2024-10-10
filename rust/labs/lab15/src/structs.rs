use crate::enums::Stone;

// ** Player ** //

#[derive(Debug, Clone, Default)]
/// Struct representing a player
/// # Fields
/// * `name` - name of the player
/// * `stone_type` - type of the stone (color) of the player
/// * `score` - score of the player
pub struct Player {
    pub name: String,
    pub stone_type: Stone,
    pub score: usize,
}

impl Player {
    pub fn new(name: String, stone_type: Stone) -> Self {
        Self {
            name,
            stone_type,
            score: 0,
        }
    }
}

/// Struct representing a command input
/// # Fields
/// * `0` - vector of strings
/// # Methods
/// * `new` - create a new Cmd struct
/// * `first` - get the first element of the command
/// * `all` - get all elements of the command
pub struct Cmd(Vec<String>);
impl Cmd {
    pub fn new(cmd: &str) -> Self {
        let cmd: Vec<String> = cmd.split_whitespace().map(|s| s.to_uppercase()).collect();

        Self(cmd)
    }

    pub fn first(&self) -> &str {
        self.0.first().map(|s| s.as_str()).unwrap_or("")
    }

    pub fn all(&self) -> &[String] {
        self.0.as_slice()
    }

    pub fn insert(&mut self, idx: usize, s: &str) {
        self.0.insert(idx, s.to_string());
    }
}
