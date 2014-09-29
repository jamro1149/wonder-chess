#ifdef SEARCH_INL
#error "Search.inl included multiple times!"
#endif
#define SEARCH_INL

#include <algorithm>
#include <climits>
#include <stdexcept>

template <typename Func>
int Chess::NegaMaxScore(const Board& b, const Func eval, int alpha,
                        const int beta, const int depth)
{
    if (depth <= 0)
    {
        return eval(b);
    }

    // overgenerate moves - check later to see if legal
    const auto moves = GenerateMoves(b, b.toMove, false);

    for (const auto move : moves)
    {
        const auto nextB = NextBoard(b, move);
        const int newScore = -NegaMaxScore(nextB, eval, -beta, -alpha, depth-1);

        if (newScore >= beta)
        {
            // newScore is above our upper bound, so cannot be the solution
            return newScore;
        }

        if (newScore > alpha && !InCheck(NextBoard(b, move), false))
        {
            alpha = newScore;
        }
    }

    return alpha;
}

template <typename Func>
Chess::Move Chess::NegaMaxSearch(const Board& b, const Func eval,
                                 const int depth)
{
    const auto moves = GenerateMoves(b, b.toMove, false);

    if (moves.empty())
    {
        throw std::invalid_argument("No moves available for " +
                                    ToString(b.toMove));
    }

    int alpha = INT_MIN;
    Move bestMove = *moves.begin();
    for (const auto move : moves)
    {
        const auto nextB = NextBoard(b, move);
        const int newScore =
            -NegaMaxScore(nextB, eval, alpha, INT_MAX, depth - 1);

        //std::cout << nextB << "\nHas a score of " << newScore << "\n\n";

        if (newScore > alpha && !InCheck(NextBoard(b, move), false))
        {
            alpha = newScore;
            bestMove = move;
        }
    }

    //std::cout << bestMove << " selected\n";

    return bestMove;
}
