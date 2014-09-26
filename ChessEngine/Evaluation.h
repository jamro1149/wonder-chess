#pragma once

namespace Chess
{
    struct Board;
    int BasicShannonEvaluation(const Board&, int numMoves) NOEXCEPT;
} // namespace Chess
