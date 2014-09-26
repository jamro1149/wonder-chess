#include <array>
#include <climits>
#include <iostream>
#include "BoardRep.h"
#include "Evaluation.h"

using namespace Chess;
using namespace std;
using boost::optional;

namespace
{
class PieceCounts
{
    array<int, 12> counts;
public:
    PieceCounts(const Board&) NOEXCEPT;
    int& operator[](Piece) NOEXCEPT;
    int operator[](Piece) const NOEXCEPT;
};
} // anonymous namespace

PieceCounts::PieceCounts(const Board& b) NOEXCEPT : counts{}
{
    for (int file = 0; file < NumFiles; ++file)
    {
        for (int rank = 0; rank < NumRanks; ++rank)
        {
            if (const auto& op = b.squares[file][rank])
            {
                ++this->operator[](*op);
            }
        }
    }
}

static int getIndex(const Piece p) NOEXCEPT
{
    return static_cast<int>(p.type) + (p.colour == Colour::White ? 0 : 6);
}

int& PieceCounts::operator[](const Piece p) NOEXCEPT
{
    return counts[getIndex(p)];
}

int PieceCounts::operator[](const Piece p) const NOEXCEPT
{
    return counts[getIndex(p)];
}

static int Diff(const PieceCounts& pc, const Type t)
{
    return pc[Piece(t, Colour::White)] - pc[Piece(t, Colour::Black)];
}

static int CalculateMaterialScore(const Board& b, const int kingWt,
                                  const int queenWt, const int rookWt,
                                  const int bishopWt,
                                  const int knightWt) NOEXCEPT
{
    const int pawnWt = 100; // since scores are measured in centipawns
    const PieceCounts pc(b);

    const int kingScore = kingWt * Diff(pc, Type::King);
    const int queenScore = queenWt * Diff(pc, Type::Queen);
    const int rookScore = rookWt * Diff(pc, Type::Rook);
    const int bishopScore = bishopWt * Diff(pc, Type::Bishop);
    const int knightScore = knightWt * Diff(pc, Type::Knight);
    const int pawnScore = pawnWt * Diff(pc, Type::Pawn);

    return kingScore + queenScore + rookScore + bishopScore + knightScore +
           pawnScore;
}

static pair<int, int> DoubledPawns(const Board& b) NOEXCEPT
{
    auto ret = make_pair(0, 0);

    for (int file = 0; file < NumFiles; ++file)
    {
        int numWhitePawns = 0;
        int numBlackPawns = 0;
        for (int rank = 0; rank < NumRanks; ++rank)
        {
            const auto& op = b.squares[file][rank];
            if (op && op->type == Type::Pawn)
            {
                ++(op->colour == Colour::White ? numWhitePawns : numBlackPawns);
            }
        }
        if (numWhitePawns >= 2)
        {
            ret.first += numWhitePawns - 1;
        }
        if (numBlackPawns >= 2)
        {
            ret.second += numBlackPawns - 1;
        }
    }

    return ret;
}

static bool DirContainsPiece(const Board& b, const Square start,
                             const pair<int, int> dir, const Piece p) NOEXCEPT
{
    for (optional<Square> curr = start; curr; curr += dir)
    {
        if (b[*curr] && *b[*curr] == p)
        {
            return true;
        }
    }
    return false;
}

static bool IsBackwardsPawn(const Board& b, const Square s,
                            const Colour c) NOEXCEPT
{
    const int forwards = c == Colour::White ? 1 : -1;
    const auto backwardsDir = make_pair(0, -forwards);

    const Piece friendlyPawn(Type::Pawn, c);
    const Piece enemyPawn(Type::Pawn, !c);

    // first check if the pawn is backed up by a frendly pawn

    const auto left = s + make_pair(-1, 0);
    if (left && DirContainsPiece(b, *left, backwardsDir, friendlyPawn))
    {
        return false;
    }

    const auto right = s + make_pair(1, 0);
    if (left && DirContainsPiece(b, *left, backwardsDir, friendlyPawn))
    {
        return false;
    }

    // next check if the pawn is blocked by a sentry

    const auto leftSentrySq = s + make_pair(-1, 2 * forwards);
    if (leftSentrySq && b[*leftSentrySq] && *b[*leftSentrySq] == enemyPawn)
    {
        return true;
    }

    const auto rightSentrySq = s + make_pair(1, 2 * forwards);
    if (rightSentrySq && b[*rightSentrySq] && *b[*rightSentrySq] == enemyPawn)
    {
        return true;
    }

    return false;
}

static int NumBackwardsPawns(const Board& b, const Colour c) NOEXCEPT
{
    const Piece friendlyPawn(Type::Pawn, c);

    int ret = 0;

    for (int file = 0; file < NumFiles; ++file)
    {
       for (int rank = 0; rank < NumRanks; ++rank)
       {
            const auto op = b.squares[file][rank];
            if (op && *op == friendlyPawn &&
                IsBackwardsPawn(b, Square(file, rank), c))
            {
                ++ret;
            }
       }
    }

    return ret;
}

static int NumIsolatedPawns(const Board& b, const Colour c) NOEXCEPT
{
    const Piece friendlyPawn(Type::Pawn, c);
    array<int, NumFiles + 2> friendlyPawnsPerFile{}; // zero pad each side

    for (int file = 0; file < NumFiles; ++file)
    {
        for (int rank = 0; rank < NumRanks; ++rank)
        {
            const auto op = b.squares[file][rank];
            if (op && *op == friendlyPawn)
            {
                ++friendlyPawnsPerFile[file+1];
            }
        }
    }

    int ret = 0;
    for (int filePlusOne = 1; filePlusOne <= NumFiles; ++filePlusOne)
    {
        if (friendlyPawnsPerFile[filePlusOne - 1] == 0 &&
            friendlyPawnsPerFile[filePlusOne] > 0 &&
            friendlyPawnsPerFile[filePlusOne + 1] == 0)
        {
            ret += friendlyPawnsPerFile[filePlusOne];     
        }
    }
    return ret;
}

static int CalculatePawnStructScore(const Board& b, const int doubledWt,
                                    const int backwardsWt,
                                    const int isolatedWt) NOEXCEPT
{
   const auto dubPawns = DoubledPawns(b);
   const int doubledScore = doubledWt * (dubPawns.second - dubPawns.first);

   cout << "\nWhite backwards pawns: " << NumBackwardsPawns(b, Colour::White)
        << "\nBlack backwards pawns: " << NumBackwardsPawns(b, Colour::Black)
        << endl;
   const int backwardsScore =
       backwardsWt * (NumBackwardsPawns(b, Colour::Black) -
                      NumBackwardsPawns(b, Colour::White));

   const int isolatedScore = isolatedWt * (NumIsolatedPawns(b, Colour::Black) -
                                           NumIsolatedPawns(b, Colour::White));

   cout << "\nDoubled Score: " << doubledScore
        << "\nBackwards Score: " << backwardsScore << "\nIsolated Score: "
        << isolatedScore << endl;

   return doubledScore + backwardsScore + isolatedScore;
}

int Chess::BasicShannonEvaluation(const Board& b, const int numMoves) NOEXCEPT
{
    const int MaterialScore =
        CalculateMaterialScore(b, 20000, 900, 500, 300, 300);
    const int PawnStructScore = CalculatePawnStructScore(b, 50, 50, 50);
    const int numEnemyMoves = GenerateMoves(b, !b.toMove).size();
    const int MobilityScore = 10 * (numMoves - numEnemyMoves);

    const int sign = b.toMove == Colour::White ? 1 : -1;

    cout << "\nMaterial Score is: " << MaterialScore
         << "\nPawn Structure Score is: " << PawnStructScore
         << "\nMobility Score is: " << MobilityScore << endl;

    return sign * (MaterialScore + PawnStructScore + MobilityScore);
}
