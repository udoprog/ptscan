#ifndef PTS_TOKEN_H
#define PTS_TOKEN_H

#include <memory>

#include <ptscan.h>

namespace pts {
class Token {
    friend class Scan;

public:
    // Construct a default empty Token.
    Token();
    Token(Token &&other);
    Token(const Token &) = delete;
    ~Token();

    // Set the token.
    void set();

    // Access the raw underlying pointer to interface with the C-api.
    pts_token_t *ptr();
private:
    pts_token_t *inner;
};
};

#endif // PTS_TOKEN_H
