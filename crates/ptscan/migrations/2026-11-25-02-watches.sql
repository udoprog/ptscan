CREATE TABLE watches (
    id INTEGER PRIMARY KEY,
    session_id INTEGER NOT NULL REFERENCES sessions (id) ON DELETE CASCADE
) STRICT;

CREATE INDEX watches_session ON watches (session_id);

CREATE TABLE watch_config (
    watch_id INTEGER NOT NULL REFERENCES watches (id) ON DELETE CASCADE,
    key TEXT NOT NULL,
    value TEXT NOT NULL CHECK (json_valid(value)),
    PRIMARY KEY (watch_id, key)
) STRICT;
