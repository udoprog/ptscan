CREATE TABLE config (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL CHECK (json_valid(value))
) STRICT;

CREATE TABLE sessions (
    id INTEGER PRIMARY KEY
) STRICT;

CREATE TABLE session_config (
    session_id INTEGER NOT NULL REFERENCES sessions (id) ON DELETE CASCADE,
    key TEXT NOT NULL,
    value TEXT NOT NULL CHECK (json_valid(value)),
    PRIMARY KEY (session_id, key)
) STRICT;
