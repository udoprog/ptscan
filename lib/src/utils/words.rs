#[derive(Clone, Debug)]
pub struct Words<'a> {
    string: &'a str,
    it: std::str::CharIndices<'a>,
    /// one character lookahead.
    b0: Option<(usize, char)>,
    buffer: String,
}

impl Words<'static> {
    /// Construct an empty iterator over words.
    pub fn empty() -> Self {
        Self {
            string: "",
            it: "".char_indices(),
            b0: None,
            buffer: String::new(),
        }
    }
}

impl<'a> Words<'a> {
    /// Split the commandline.
    pub fn new(string: &str) -> Words<'_> {
        let mut it = string.char_indices();
        let b0 = it.next();
        Words {
            string,
            it,
            b0,
            buffer: String::new(),
        }
    }

    /// Access the underlying string.
    pub fn string(&self) -> &'a str {
        self.string
    }

    /// Take the next character.
    pub fn take(&mut self) -> Option<(usize, char)> {
        std::mem::replace(&mut self.b0, self.it.next())
    }

    /// Look at the next character.
    pub fn peek(&self) -> Option<(usize, char)> {
        self.b0.clone()
    }

    /// The rest of the input.
    pub fn rest(&self) -> &'a str {
        let s = self.peek().map(|(i, _)| i).unwrap_or(self.string.len());
        &self.string[s..]
    }

    /// Process an escape.
    fn escape(&mut self) {
        let c = match self.take() {
            Some((_, c)) => c,
            None => return,
        };

        match c {
            't' => self.buffer.push('\t'),
            'r' => self.buffer.push('\r'),
            'n' => self.buffer.push('\n'),
            o => self.buffer.push(o),
        }
    }
}

impl<'a> Iterator for Words<'a> {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        if self.string.is_empty() {
            return None;
        }

        while let Some((_, c)) = self.take() {
            match c {
                ' ' | '\t' | '\r' | '\n' => {
                    // Consume all whitespace so that `rest` behaves better.
                    while let Some((_, c)) = self.peek() {
                        match c {
                            ' ' | '\t' | '\r' | '\n' => {
                                self.take();
                            }
                            _ => break,
                        }
                    }

                    if !self.buffer.is_empty() {
                        let ret = self.buffer.clone();
                        self.buffer.clear();
                        return Some(ret);
                    }

                    continue;
                }
                '\\' => self.escape(),
                // parse string
                '"' => {
                    while let Some((_, c)) = self.take() {
                        match c {
                            '\\' => self.escape(),
                            '"' => break,
                            o => self.buffer.push(o),
                        }
                    }
                }
                '\'' => {
                    while let Some((_, c)) = self.take() {
                        match c {
                            '\\' => self.escape(),
                            '\'' => break,
                            o => self.buffer.push(o),
                        }
                    }
                }
                o => self.buffer.push(o),
            }
        }

        if !self.buffer.is_empty() {
            let ret = self.buffer.clone();
            self.buffer.clear();
            return Some(ret);
        }

        None
    }
}
