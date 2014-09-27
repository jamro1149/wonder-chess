#pragma once

#include "BoardRep.h"

namespace Chess
{
template <typename Func>
int NegaMaxScore(const Board&, Func eval, int alpha, int beta, int depth);
template <typename Func> Move NegaMaxSearch(const Board&, Func eval, int depth);
} // namespace Chess

#include "Search.inl"
