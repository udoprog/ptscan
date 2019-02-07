#include <pts/Token.h>

namespace pts
{
Token::Token() : inner(pts_token_new())
{
}

Token::Token(Token&& other) :
    inner(other.inner)
{
    other.inner = nullptr;
}

void Token::set()
{
    pts_token_set(inner);
}

Token::~Token()
{
    if (inner) {
        pts_token_free(inner);
        inner = nullptr;
    }
}
}
