#pragma once

#include <boost/optional/optional.hpp>
#include <array>
#include <functional>
#include <iosfwd>
#include <string>
#include "Utility.h"

namespace Chess
{
struct Piece;
class Square;
struct Move;
struct Board;
}

namespace std
{
template <> struct hash<boost::optional<Chess::Piece>>
{
    std::size_t operator()(const boost::optional<Chess::Piece>&) const;
};
template <> struct hash<Chess::Square>
{
    std::size_t operator()(const Chess::Square&) const;
};
template <> struct hash<boost::optional<Chess::Square>>
{
    std::size_t operator()(const boost::optional<Chess::Square>&) const;
};
template <> struct hash<Chess::Move>
{
    std::size_t operator()(const Chess::Move&) const;
};
template <> struct hash<Chess::Board>
{
    std::size_t operator()(const Chess::Board&) const;
};
}

namespace Chess
{

constexpr int NumRanks = 8;
constexpr int NumFiles = 8;
constexpr int NumSquares = NumFiles * NumRanks;

enum class Type
{
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
};

std::string ToString(Type);
char TypeToChar(Type) NOEXCEPT;

enum class Colour
{
    White,
    Black,
};

Colour operator!(Colour)NOEXCEPT; // inverts the colour (e.g. !Colour::White ==
                                  // Colour::Black)
std::string ToString(Colour);
std::ostream& operator<<(std::ostream&, Colour);

struct Piece
{
    Type type;
    Colour colour;
    Piece(Type t, Colour c) NOEXCEPT : type(t), colour(c)
    {
    }
};

bool operator==(Piece, Piece) NOEXCEPT;
bool operator!=(Piece, Piece) NOEXCEPT;
char PieceToChar(Piece) NOEXCEPT;
std::ostream& operator<<(std::ostream&, Piece);

class Square
{
    int id;

  public:
    Square(int file, int rank);
    int File() const NOEXCEPT;
    int Rank() const NOEXCEPT;
    void File(int);
    void Rank(int);
    friend bool operator<(Square, Square) NOEXCEPT; // for sorted vectors
    friend bool operator==(Square, Square) NOEXCEPT;
    friend std::size_t std::hash<Square>::operator()(const Square&) const;
};

bool operator<(Square, Square) NOEXCEPT;
bool operator==(Square, Square) NOEXCEPT;
bool operator!=(Square, Square) NOEXCEPT;
std::string getPrettyFile(Square);
std::string ToString(Square);
std::ostream& operator<<(std::ostream&, Square);
bool PutsOutOfRange(Square, std::pair<int, int>) NOEXCEPT;
boost::optional<Square> operator+(Square, std::pair<int, int>) NOEXCEPT;
boost::optional<Square>& operator+=(boost::optional<Square>&,
                                    std::pair<int, int>) NOEXCEPT;
Square& operator+=(Square&, std::pair<int, int>);
boost::optional<Square> operator-(Square, std::pair<int, int>) NOEXCEPT;
boost::optional<Square>& operator-=(boost::optional<Square>&,
                                    std::pair<int, int>) NOEXCEPT;
Square& operator-=(Square&, std::pair<int, int>);
std::pair<int, int> operator-(Square, Square) NOEXCEPT;
Square StringToSquare(const std::string&);

struct Move
{
    Square from;
    Square to;
    Move(Square sOld, Square sNew) NOEXCEPT : from(sOld), to(sNew)
    {
    }
};

bool operator==(Move, Move) NOEXCEPT;
bool operator!=(Move, Move) NOEXCEPT;
std::ostream& operator<<(std::ostream&, Move);
Move StringToMove(const std::string&);

struct Board
{
    std::array<std::array<boost::optional<Piece>, NumRanks>, NumFiles> squares;
    boost::optional<Square> enPassantable;
    Colour toMove;
    // these mark the squares of castleable rooks
    boost::optional<Square> wLeftCastleRook;
    boost::optional<Square> wRightCastleRook;
    boost::optional<Square> bLeftCastleRook;
    boost::optional<Square> bRightCastleRook;
    static Board HackyMakeDefaultStart();
    boost::optional<Piece>& operator[](Square)NOEXCEPT;
    const boost::optional<Piece>& operator[](Square) const NOEXCEPT;
};

bool operator==(const Board&, const Board&) NOEXCEPT;
bool operator!=(const Board&, const Board&) NOEXCEPT;
std::ostream& operator<<(std::ostream&, const Board&);
boost::optional<std::string> CannotMove(const Board&, Move);
void MakeMove(Board&, Move,
              bool switchTurn = true); // will perform move even if not legal
void MakeMoveChecked(Board&, Move, bool switchTurn = true); // invalid_argument
                                                            // thrown on
                                                            // illegal move
Board NextBoard(const Board&, Move, bool switchTurn = true);
Board NextBoardChecked(const Board&, Move, bool switchTurn = true);

std::vector<Square> ThreatenedSquares(const Board&, Colour);
bool InCheck(const Board&, bool UseToMoveColour = true);
bool NoMoves(const Board&);
bool InCheckmate(const Board&);
bool MoveIsReversible(const Board&, Move);

std::vector<Move> GeneratePawnMoves(const Board&, Square, Colour,
                                    bool onlyLegalMoves = true);
std::vector<Move> GenerateKnightMoves(const Board&, Square, Colour,
                                      bool onlyLegalMoves = true);
std::vector<Move> GenerateBishopMoves(const Board&, Square, Colour,
                                      bool onlyLegalMoves = true);
std::vector<Move> GenerateRookMoves(const Board&, Square, Colour,
                                    bool onlyLegalMoves = true);
std::vector<Move> GenerateQueenMoves(const Board&, Square, Colour,
                                     bool onlyLegalMoves = true);
std::vector<Move> GenerateKingMoves(const Board&, Square, Colour,
                                    bool onlyLegalMoves = true);
std::vector<Move> GeneratePieceMoves(const Board&, Square, Piece,
                                     bool onlyLegalMoves = true);

std::vector<Move> GenerateMoves(const Board&, Colour,
                                bool onlyLegalMoves = true);

} // namespace Chess
