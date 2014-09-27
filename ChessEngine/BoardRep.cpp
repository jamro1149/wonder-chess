#include <algorithm>
#include <cctype>
#include <iterator>
#if STDREGEX_SUPPORTED
#include <regex>
#else
#include <boost/regex.hpp>
using boost::regex;
using boost::smatch;
#endif
#include <set>
#include <stdexcept>
#include <vector>
#include "BoardRep.h"

using namespace Chess;
using namespace Chess::Utility;
using namespace std;
using boost::optional;
using boost::none;

static size_t CombineHash(size_t OldHash, size_t NewHash)
{
    return (OldHash << 1) ^ NewHash;
}

size_t hash<optional<Piece>>::operator()(const optional<Piece>& ot) const
{
    hash<bool> hb;
    hash<int> hi;
    return CombineHash(hb(ot.is_initialized()),
                       !ot ? 0 : CombineHash(hi(static_cast<int>(ot->type)),
                                             hi(static_cast<int>(ot->colour))));
}
size_t hash<Square>::operator()(const Square& s) const
{
    return hash<int>()(s.id);
}

size_t hash<optional<Square>>::operator()(const optional<Square>& os) const
{
    if (os)
    {
        return hash<Square>()(*os);
    }
    else
    {
        return hash<int>()(-1);
    }
}

size_t hash<Move>::operator()(const Move& m) const
{
    size_t h1 = hash<Square>()(m.from);
    size_t h2 = hash<Square>()(m.to);
    return CombineHash(h1, h2);
}

size_t hash<Board>::operator()(const Board& b) const
{
    size_t ret = 0u;

    for (int file = 0; file < NumFiles; ++file)
    {
        for (int rank = 0; rank < NumRanks; ++rank)
        {
            ret = CombineHash(ret,
                              hash<optional<Piece>>()(b.squares[file][rank]));
        }
    }

    hash<optional<Square>> hos;

    ret = CombineHash(ret, hos(b.enPassantable));
    ret = CombineHash(ret, hash<int>()(static_cast<int>(b.toMove)));
    ret = CombineHash(ret, hos(b.wLeftCastleRook));
    ret = CombineHash(ret, hos(b.wRightCastleRook));
    ret = CombineHash(ret, hos(b.bLeftCastleRook));
    ret = CombineHash(ret, hos(b.wRightCastleRook));

    return ret;
}

string Chess::ToString(const Type t)
{
    switch (t)
    {
    default:
        assert(false);
    case Type::Pawn:
        return "pawn";
    case Type::Knight:
        return "knight";
    case Type::Bishop:
        return "bishop";
    case Type::Rook:
        return "rook";
    case Type::Queen:
        return "queen";
    case Type::King:
        return "king";
    }
}

char Chess::TypeToChar(const Type t) NOEXCEPT
{
    switch (t)
    {
    default:
        assert(false);
    case Type::Pawn:
        return 'P';
    case Type::Knight:
        return 'N';
    case Type::Bishop:
        return 'B';
    case Type::Rook:
        return 'R';
    case Type::Queen:
        return 'Q';
    case Type::King:
        return 'K';
    }
}

Colour Chess::operator!(const Colour c)NOEXCEPT
{
    return c == Colour::White ? Colour::Black : Colour::White;
}

string Chess::ToString(const Colour c)
{
    switch (c)
    {
    default:
        assert(false);
    case Colour::White:
        return "white";
    case Colour::Black:
        return "black";
    }
}

ostream& Chess::operator<<(ostream& os, const Colour c)
{
    return os << ToString(c);
}

bool Chess::operator==(const Piece left, const Piece right) NOEXCEPT
{
    return left.type == right.type && left.colour == right.colour;
}

bool Chess::operator!=(const Piece left, const Piece right) NOEXCEPT
{
    return !(left == right);
}

char Chess::PieceToChar(const Piece p) NOEXCEPT
{
    const char typeChar = TypeToChar(p.type);
    return p.colour == Colour::White ? typeChar
                                     : static_cast<char>(tolower(typeChar));
}

ostream& Chess::operator<<(ostream& os, const Piece p)
{
    return os << PieceToChar(p);
}

static void CheckFile(int file)
{
    if (file < 0 || file >= NumFiles)
    {
        throw out_of_range("file out of range");
    }
}

static void CheckRank(int rank)
{
    if (rank < 0 || rank >= NumRanks)
    {
        throw out_of_range("rank out of range");
    }
}

Square::Square(const int file, const int rank)
    : id(rank * NumFiles + file) // first square on second rank == NumFiles
{
    CheckFile(file);
    CheckRank(rank);
}

int Square::File() const NOEXCEPT
{
    return id % NumFiles;
}

int Square::Rank() const NOEXCEPT
{
    return id / NumFiles;
}

void Square::File(const int file)
{
    CheckFile(file);
    id += file - File();
    assert(File() >= 0 && File() < NumFiles);
}

void Square::Rank(const int rank)
{
    CheckRank(rank);
    id += NumFiles * (rank - Rank());
    assert(Rank() >= 0 && Rank() < NumRanks);
}

bool Chess::operator<(const Square left, const Square right) NOEXCEPT
{
    return left.id < right.id;
}

bool Chess::operator==(const Square left, const Square right) NOEXCEPT
{
    return left.id == right.id;
}

bool Chess::operator!=(const Square left, const Square right) NOEXCEPT
{
    return !(left == right);
}

string Chess::getPrettyFile(const Square s)
{
    string ret;

    int workingFile = s.File();

    if (workingFile < 26)
    {
        ret += 'a' + static_cast<char>(workingFile);
        return ret;
    }

    // build ret backwards and reverse later
    // here, 'a' denotes '0' in base 26
    for (int nextFile = workingFile / 26; nextFile > 0;
         workingFile = nextFile, nextFile /= 26)
    {
        const char nextLetter = 'a' + workingFile % 26;
        ret += nextLetter;
    }
    // the first letter is special - 'a' denotes '1' in base 27, rather than '0'
    // in base 26
    const char firstLetter = 'a' + workingFile % 26 - 1;
    ret += firstLetter;

    reverse(ret.begin(), ret.end());

    return ret;
}

string Chess::ToString(const Square s)
{
    return getPrettyFile(s) + to_string(s.Rank() + 1);
}

ostream& Chess::operator<<(ostream& os, const Square s)
{
    return os << ToString(s);
}

bool Chess::PutsOutOfRange(const Square s, const pair<int, int> vec) NOEXCEPT
{
    const int newFile = s.File() + vec.first;
    const int newRank = s.Rank() + vec.second;
    return newFile < 0 || newFile >= NumFiles || newRank < 0 ||
           newRank >= NumRanks;
}

optional<Square> Chess::operator+(const Square left,
                                  const pair<int, int> right) NOEXCEPT
{
    if (PutsOutOfRange(left, right))
    {
        return none;
    }
    else
    {
        return Square(left.File() + right.first, left.Rank() + right.second);
    }
}

optional<Square>& Chess::operator+=(optional<Square>& left,
                                    const pair<int, int> right) NOEXCEPT
{
    return left = (left ? *left + right : left);
}

Square& Chess::operator+=(Square& left, const pair<int, int> right)
{
    if (auto s = left + right)
    {
        left = *s;
    }
    throw out_of_range("operator+= puts out of range");
}

optional<Square> Chess::operator-(const Square left,
                                  const pair<int, int> right) NOEXCEPT
{
    return left + make_pair(-right.first, -right.second);
}

optional<Square>& Chess::operator-=(optional<Square>& left,
                                    const pair<int, int> right) NOEXCEPT
{
    return left = (left ? *left - right : left);
}

Square& Chess::operator-=(Square& left, const pair<int, int> right)
{
    if (auto s = left - right)
    {
        left = *s;
    }
    throw out_of_range("operator-= puts out of range");
}

static int StringToFile(const string& s)
{
    if (s.size() == 0)
    {
        throw invalid_argument("empty file string not permitted");
    }

    if (s.size() == 1)
    {
        return s[0] - 'a';
    }

    int ret = 0, factor = 1;
    for (unsigned index = 0; index < s.size() - 1; ++index, factor *= 26)
    {
        if (!islower(s[index]))
        {
            throw invalid_argument("file string '" + s +
                                   "' may only contain lower case letters");
        }
        ret += (s[index] - 'a') * factor;
    }
    ret += (s[s.size() - 1] - 'a' + 1) * factor;
    return ret;
}

pair<int, int> Chess::operator-(const Square left, const Square right) NOEXCEPT
{
    return make_pair(left.File() - right.File(), left.Rank() - right.Rank());
}

Square Chess::StringToSquare(const string& s)
{
    static const regex squareRegex(R"(([[:alpha:]]+)(\d+))");

    smatch results;
    if (!regex_match(s, results, squareRegex))
    {
        throw invalid_argument("square string '" + s +
                               "' improperly formatted");
    }

    const int file = StringToFile(results[1]);
    const int rank = stoi(results[2]) - 1;
    return Square(file, rank);
}

bool Chess::operator==(const Move left, const Move right) NOEXCEPT
{
    return left.from == right.from && left.to == right.to;
}

bool Chess::operator!=(const Move left, const Move right) NOEXCEPT
{
    return !(left == right);
}

ostream& Chess::operator<<(ostream& os, const Move m)
{
    return os << "(" << m.from << ", " << m.to << ")";
}

Move Chess::StringToMove(const string& s)
{
    static const regex moveRegex(
        R"(\s*\(?\s*([[:alpha:]]+\d+)\s*,\s*([[:alpha:]]+\d+)\s*\)?\s*)");

    smatch results;
    if (!regex_match(s, results, moveRegex))
    {
        throw invalid_argument("move string '" + s + "' improperly formatted");
    }

    return Move(StringToSquare(results[1]), StringToSquare(results[2]));
}

// HACK: board assumed to be 8-by-8
Board Board::HackyMakeDefaultStart()
{
    Board ret;
    for (int file = 0; file < NumFiles; ++file)
    {
        // Pick type of piece based on file
        Type t;
        switch (file)
        {
        case 0:
        case 7:
            t = Type::Rook;
            break;

        case 1:
        case 6:
            t = Type::Knight;
            break;

        case 2:
        case 5:
            t = Type::Bishop;
            break;

        case 3:
            t = Type::Queen;
            break;

        case 4:
            t = Type::King;
            break;

        default:
            assert(false);
            t = Type::Pawn;
        }

        for (int rank = 0; rank < NumRanks; ++rank)
        {
            // Pick colour of piece based on rank
            switch (rank)
            {
            case 0:
            case 1:
                ret.squares[file][rank] =
                    Piece(rank == 0 ? t : Type::Pawn, Colour::White);
                break;

            case 6:
            case 7:
                ret.squares[file][rank] =
                    Piece(rank == 7 ? t : Type::Pawn, Colour::Black);
                break;

            default:
                assert(ret.squares[file][rank] == none);
            }
        }
    }

    assert(ret.enPassantable == none);
    ret.toMove = Colour::White;
    ret.wRightCastleRook = Square(7, 0);
    ret.wLeftCastleRook = Square(0, 0);
    ret.bRightCastleRook = Square(7, 7);
    ret.bLeftCastleRook = Square(0, 7);
    return ret;
}

optional<Piece>& Board::operator[](const Square s)NOEXCEPT
{
    return squares[s.File()][s.Rank()];
}

const optional<Piece>& Board::operator[](const Square s) const NOEXCEPT
{
    return squares[s.File()][s.Rank()];
}

bool Chess::operator==(const Board& left, const Board& right) NOEXCEPT
{
    return left.squares == right.squares &&
           left.enPassantable == right.enPassantable &&
           left.toMove == right.toMove &&
           left.wLeftCastleRook == right.wLeftCastleRook &&
           left.wRightCastleRook == right.wRightCastleRook &&
           left.bLeftCastleRook == right.bLeftCastleRook &&
           left.bRightCastleRook == right.bRightCastleRook;
}

bool Chess::operator!=(const Board& left, const Board& right) NOEXCEPT
{
    return !(left == right);
}

static void PrintBorder(ostream& os, const bool printMoveMarker,
                        const set<int>& castleMarkers)
{
    os << (printMoveMarker ? "=>" : "  ");
    os << "+";
    for (int file = 0; file < NumFiles; ++file)
    {
        os << "-";
        os << (castleMarkers.count(file) ? '*' : '-');
        os << "-";
    }
    os << "+\n";
}

static void PrintHackyBorder(ostream& os, const bool printMoveMarker,
                             const optional<Square> castleKings,
                             const optional<Square> castleQueens)
{
    set<int> castleMarkers;
    // HACK: starting position of king assumed
    if (castleKings)
    {
        castleMarkers.insert(castleKings->File());
        castleMarkers.insert(4);
    }
    if (castleQueens)
    {
        castleMarkers.insert(castleQueens->File());
        castleMarkers.insert(4);
    }
    PrintBorder(os, printMoveMarker, castleMarkers);
}

static void PrintRank(ostream& os, const Board& b, const int rank)
{
    os << rank + 1 << " |";
    for (int file = 0; file < NumFiles; ++file)
    {
        const bool isWhiteSquare = (rank % 2 != file % 2);
        const char squareChar = (isWhiteSquare ? ' ' : ':');

        os << squareChar;
        os << (b.squares[file][rank] ? PieceToChar(*b.squares[file][rank])
                                     : squareChar);
        os << squareChar;
    }
    os << "|";
}

ostream& Chess::operator<<(ostream& os, const Board& b)
{
    PrintHackyBorder(os, b.toMove == Colour::Black, b.bRightCastleRook,
                     b.bLeftCastleRook);

    for (int rank = NumRanks - 1; rank >= 0; --rank)
    {
        PrintRank(os, b, rank);
        os << "\n";
    }

    PrintHackyBorder(os, b.toMove == Colour::White, b.wRightCastleRook,
                     b.wLeftCastleRook);

    os << "  ";
    for (char file = 'a'; file < 'a' + static_cast<char>(NumFiles); ++file)
    {
        os << "  " << file;
    }

    return os;
}

optional<string> Chess::CannotMove(const Board& b, const Move m)
{
    auto piece = b[m.from];
    if (!piece)
    {
        return "no piece at " + ToString(m.from);
    }

    if (piece->colour != b.toMove)
    {
        return "it is not " + ToString(!b.toMove) + "'s turn to move";
    }

    bool isMoveLegal = false;
    switch (piece->type)
    {
    case Type::Pawn:
        isMoveLegal =
            LinearSearch(GeneratePawnMoves(b, m.from, piece->colour), m);
        break;
    case Type::Knight:
        isMoveLegal =
            LinearSearch(GenerateKnightMoves(b, m.from, piece->colour), m);
        break;
    case Type::Bishop:
        isMoveLegal =
            LinearSearch(GenerateBishopMoves(b, m.from, piece->colour), m);
        break;
    case Type::Rook:
        isMoveLegal =
            LinearSearch(GenerateRookMoves(b, m.from, piece->colour), m);
        break;
    case Type::Queen:
        isMoveLegal =
            LinearSearch(GenerateQueenMoves(b, m.from, piece->colour), m);
        break;
    case Type::King:
        isMoveLegal =
            LinearSearch(GenerateKingMoves(b, m.from, piece->colour), m);
    }

    return isMoveLegal ? none : boost::make_optional("cannot move " +
                                                     ToString(piece->type) +
                                                     " to " + ToString(m.to));
}

static bool HandlePawnMove(Board& b, const Move m, const Colour c,
                           optional<Square> enPassantable) NOEXCEPT
{
    // check en passent
    const auto moveVec = m.to - m.from;
    if (abs(moveVec.second) == 2)
    {
        // this piece moved double, so can be captured en passent on opponent's
        // turn
        b.enPassantable = m.to;
    }
    else if (moveVec.first != 0 && !b[m.to] && enPassantable)
    {
        // the move is an en passant capture!
        b[*enPassantable] = none;
    }

    // check pawn promotion
    return (m.to.Rank() == 0 && c == Colour::Black) ||
           (m.to.Rank() == NumRanks - 1 && c == Colour::White);
}

static void MarkNotCastlable(Board& b, const Square s, const Colour c)
{
    if (c == Colour::White)
    {
        if (b.wLeftCastleRook == s)
        {
            b.wLeftCastleRook = none;
        }
        else if (b.wRightCastleRook == s)
        {
            b.wRightCastleRook = none;
        }
    }
    else
    {
        if (b.bLeftCastleRook == s)
        {
            b.bLeftCastleRook = none;
        }
        else if (b.bRightCastleRook == s)
        {
            b.bRightCastleRook = none;
        }
    }
}

static void HandleRookMove(Board& b, const Move m, const Colour c) NOEXCEPT
{
    MarkNotCastlable(b, m.from, c);
}

static void HandleKingMove(Board& b, const Move m, const Colour c) NOEXCEPT
{
    const int fileDifference = (m.to - m.from).first;
    optional<Square>& leftCastleRook =
        c == Colour::White ? b.wLeftCastleRook : b.bLeftCastleRook;
    optional<Square>& rightCastleRook =
        c == Colour::White ? b.wRightCastleRook : b.bRightCastleRook;

    // if this move is castling, move the rook into place
    if (fileDifference < -1 && leftCastleRook)
    {
        MakeMove(b, Move(*leftCastleRook, *(m.to + make_pair(1, 0))), false);
    }
    else if (fileDifference > 1 && rightCastleRook)
    {
        MakeMove(b, Move(*rightCastleRook, *(m.to - make_pair(1, 0))), false);
    }

    // as the king has moved, it can't castle for the rest of the game
    leftCastleRook = none;
    rightCastleRook = none;
}

void Chess::MakeMove(Board& b, const Move m, const bool switchTurn)
{
    auto piece = b[m.from];
    if (!piece.is_initialized())
    {
        throw invalid_argument("no piece at " + ToString(m.from));
    }

    if (switchTurn)
    {
        b.toMove = !b.toMove;
    }

    const auto currEnPassentable = b.enPassantable;
    b.enPassantable =
        none; // this will be overwritten if player double-moves a pawn

    bool promotion = false;

    // handle special moves (castling, pawn double and en passent)
    switch (piece->type)
    {
    case Type::Pawn:
        promotion = HandlePawnMove(b, m, piece->colour, currEnPassentable);
        break;
    case Type::Rook:
        HandleRookMove(b, m, piece->colour);
        break;
    case Type::King:
        HandleKingMove(b, m, piece->colour);
        break;
    default:
        break;
    }

    // make the move
    b[m.from] = none;

    // if the move captures a castlable rook, null out the castlable
    if (b[m.to] && b[m.to]->type == Type::Rook)
    {
        MarkNotCastlable(b, m.to, b[m.to]->colour);
    }

    b[m.to] = promotion ? Piece(Type::Queen, piece->colour) : *piece;
}

void Chess::MakeMoveChecked(Board& b, const Move m, const bool switchTurn)
{
    if (auto reason = CannotMove(b, m))
    {
        throw invalid_argument(*reason);
    }
    MakeMove(b, m, switchTurn);
}

Board Chess::NextBoard(const Board& b, const Move m, const bool switchTurn)
{
    Board ret(b);
    MakeMove(ret, m, switchTurn);
    return ret;
}

Board Chess::NextBoardChecked(const Board& b, const Move m,
                              const bool switchTurn)
{
    Board ret(b);
    MakeMoveChecked(ret, m, switchTurn);
    return ret;
}

template <typename Pred, typename Cont, typename Iter>
static void CheckAttacks(const Board& b, const Square s, const Colour c,
                         const Pred& p, const Cont& vecs, Iter it,
                         const int range = -1)
{
    for (const auto vec : vecs)
    {
        bool newDir = true;

        int remainingRange = range;
        for (auto toSquare = s + vec; toSquare && remainingRange != 0;
             toSquare = *toSquare + vec, --remainingRange)
        {
            if (p(b, *toSquare, c, newDir))
            {
                *it = *toSquare;
                ++it;
                newDir = false;
            }
            else
            {
                break;
            }
        }
    }
}

// collection of functors to be used with CheckAttacks
namespace
{
// used to determine threatened squares
class IsThreatening
{
    mutable bool isBlocked;

  public:
    IsThreatening() NOEXCEPT : isBlocked(false)
    {
    }
    bool operator()(const Board& b, const Square s, Colour,
                    const bool newDir) const NOEXCEPT
    {
        if (newDir)
        {
            isBlocked = false;
        }

        bool ret = false;
        if (!isBlocked)
        {
            ret = true;
            if (b[s])
            {
                isBlocked = true;
            }
        }
        return ret;
    }
};

// suitable for pawn movement
struct IsEmpty
{
    bool operator()(const Board& b, const Square s, const Colour,
                    bool = false) const NOEXCEPT
    {
        return !b[s];
    }
};

// suitable for pawn capture
struct IsCapturable
{
    bool operator()(const Board& b, const Square s, const Colour c,
                    bool = false) const NOEXCEPT
    {
        return b[s] && b[s]->colour != c;
    }
};

// suitable for non-pawn movement and capture
class IsEmptyOrCapturable
{
    mutable bool isBlocked;
  public:
    IsEmptyOrCapturable() NOEXCEPT : isBlocked(false)
    {
    }
    bool operator()(const Board& b, const Square s, const Colour c,
                    bool newDir) const NOEXCEPT
    {
        if (newDir)
        {
            isBlocked = false;
        }

        if (isBlocked)
        {
            return false;
        }
        else if (IsEmpty()(b, s, c))
        {
            return true;
        }
        
        isBlocked = true;
        return IsCapturable()(b, s, c);
    }
};
}

static const array<pair<int, int>, 2> WhitePawnAttacks = {
    {make_pair(-1, 1), make_pair(1, 1)}};

static const array<pair<int, int>, 2> BlackPawnAttacks = {
    {make_pair(-1, -1), make_pair(1, -1)}};

static const array<pair<int, int>, 8> LShapes = {
    {make_pair(-2, -1), make_pair(-2, 1), make_pair(-1, -2), make_pair(-1, 2),
     make_pair(1, -2), make_pair(1, 2), make_pair(2, -1), make_pair(2, 1)}};

static const array<pair<int, int>, 4> Diagonals = {
    {make_pair(-1, -1), make_pair(-1, 1), make_pair(1, -1), make_pair(1, 1)}};

static const array<pair<int, int>, 4> Cardinals = {
    {make_pair(0, -1), make_pair(0, 1), make_pair(-1, 0), make_pair(1, 0)}};

static const array<pair<int, int>, 8> CardinalsAndDiagonals = {
    {make_pair(0, -1), make_pair(0, 1), make_pair(-1, 0), make_pair(1, 0),
     make_pair(-1, -1), make_pair(-1, 1), make_pair(1, -1), make_pair(1, 1)}};

// Board irrelevant for pawn, knight and king, and colour only relevant to pawn

static vector<Square> PawnThreatening(const Square s, const Colour c)
{
    vector<Square> ret;
    CheckAttacks(Board(), s, c, IsThreatening(),
                 c == Colour::White ? WhitePawnAttacks : BlackPawnAttacks,
                 back_inserter(ret), 1);
    return ret;
}

static vector<Square> KnightThreatening(const Square s)
{
    vector<Square> ret;
    ret.reserve(8);
    CheckAttacks(Board(), s, Colour(), IsThreatening(), LShapes,
                 back_inserter(ret), 1);
    return ret;
}

static vector<Square> BishopThreatening(const Board& b, const Square s)
{
    vector<Square> ret;
    ret.reserve(2 * (min(NumRanks, NumFiles) - 1));
    CheckAttacks(b, s, Colour(), IsThreatening(), Diagonals,
                 back_inserter(ret));
    return ret;
}

static vector<Square> RookThreatening(const Board& b, const Square s)
{
    vector<Square> ret;
    ret.reserve(2 * (min(NumRanks, NumFiles) - 1));
    CheckAttacks(b, s, Colour(), IsThreatening(), Cardinals,
                 back_inserter(ret));
    return ret;
}

static vector<Square> QueenThreatening(const Board& b, const Square s)
{
    vector<Square> ret;
    ret.reserve(4 * (min(NumRanks, NumFiles) - 1));
    CheckAttacks(b, s, Colour(), IsThreatening(), CardinalsAndDiagonals,
                 back_inserter(ret));
    return ret;
}

static vector<Square> KingThreatening(const Square s)
{
    vector<Square> ret;
    ret.reserve(8);
    CheckAttacks(Board(), s, Colour(), IsThreatening(), CardinalsAndDiagonals,
                 back_inserter(ret), 1);
    return ret;
}

static vector<Square> ThreateningSquares(const Board& b, const Square s,
                                                const Piece p)
{
    switch (p.type)
    {
    default:
        assert(false);
    case Type::Pawn:
        return PawnThreatening(s, p.colour);
    case Type::Knight:
        return KnightThreatening(s);
    case Type::Bishop:
        return BishopThreatening(b, s);
    case Type::Rook:
        return RookThreatening(b, s);
    case Type::Queen:
        return QueenThreatening(b, s);
    case Type::King:
        return KingThreatening(s);
    }
}

vector<Square> Chess::ThreatenedSquares(const Board& b, const Colour c)
{
    vector<Square> ret;

    for (int file = 0; file < NumFiles; ++file)
    {
        for (int rank = 0; rank < NumRanks; ++rank)
        {
            const auto piece = b.squares[file][rank];
            if (piece && piece->colour == c)
            {
                const auto squares =
                    ThreateningSquares(b, Square(file, rank), *piece);
                ret.insert(ret.end(), squares.begin(), squares.end());
            }
        }
    }

    return ret;
}

bool Chess::InCheck(const Board& b, const bool UseToMoveColour)
{
    const Colour c = UseToMoveColour ? b.toMove : !b.toMove;

    for (const Square s : ThreatenedSquares(b, !c))
    {
        const auto optPiece = b[s];
        if (optPiece && optPiece->colour == c && optPiece->type == Type::King)
        {
            return true;
        }
    }
    return false;
}

bool Chess::NoMoves(const Board& b)
{
    for (int file = 0; file < NumFiles; ++file)
    {
        for (int rank = 0; rank < NumRanks; ++rank)
        {
            const Square sq(file, rank);
            if (!b[sq] || b[sq]->colour != b.toMove)
            {
                continue;
            }

            if (GeneratePieceMoves(b, sq, *b[sq]).size() > 0)
            {
                return false;
            }
        }
    }

    return true;
}

bool Chess::InCheckmate(const Board& b)
{
    return InCheck(b) && NoMoves(b);
}

bool Chess::MoveIsReversible(const Board& b, const Move m)
{
    if (!b[m.from])
    {
        throw invalid_argument("No piece at " + ToString(m.from));
    }
    return b[m.from]->type != Type::Pawn && !b[m.to];
}

static vector<Move> LegalMoves(const Board& b, const Square from,
                               const vector<Square>& tos)
{
    vector<Move> ret;
    ret.reserve(tos.size());

    for (const Square to : tos)
    {
        const Move m(from, to);
        const auto next = NextBoard(b, m);
        if (!InCheck(next, false))
        {
            ret.push_back(m);
        }
    }

    return ret;
}

vector<Move> Chess::GeneratePawnMoves(const Board& b, const Square s,
                                             const Colour c)
{
    const bool isWhite = c == Colour::White;

    // insert the non-attacks
    const array<pair<int, int>, 1> moveVec = {
        {isWhite ? make_pair(0, 1) : make_pair(0, -1)}};
    const int doubleMoveRank = isWhite ? 1 : NumRanks - 2;
    vector<Square> squares;
    CheckAttacks(b, s, c, IsEmpty(), moveVec, back_inserter(squares),
                 s.Rank() == doubleMoveRank ? 2 : 1);
    auto ret = LegalMoves(b, s, squares);

    // insert the attacks
    squares.clear();
    CheckAttacks(b, s, c, IsCapturable(),
                 isWhite ? WhitePawnAttacks : BlackPawnAttacks,
                 back_inserter(squares), 1);
    const auto attacks = LegalMoves(b, s, squares);
    ret.insert(ret.end(), attacks.begin(), attacks.end());

    return ret;
}

vector<Move> Chess::GenerateKnightMoves(const Board& b, const Square s,
                                               const Colour c)
{
    vector<Square> squares;
    squares.reserve(8);
    CheckAttacks(b, s, c, IsEmptyOrCapturable(), LShapes,
                 back_inserter(squares), 1);
    return LegalMoves(b, s, squares);
}

vector<Move> Chess::GenerateBishopMoves(const Board& b, const Square s,
                                               const Colour c)
{
    vector<Square> squares;
    squares.reserve(2 * (min(NumFiles, NumRanks) - 1));
    CheckAttacks(b, s, c, IsEmptyOrCapturable(), Diagonals,
                 back_inserter(squares));
    return LegalMoves(b, s, squares);
}

vector<Move> Chess::GenerateRookMoves(const Board& b, const Square s,
                                             const Colour c)
{
    vector<Square> squares;
    squares.reserve(2 * (min(NumFiles, NumRanks) - 1));
    CheckAttacks(b, s, c, IsEmptyOrCapturable(), Cardinals,
                 back_inserter(squares));
    return LegalMoves(b, s, squares);
}

vector<Move> Chess::GenerateQueenMoves(const Board& b, const Square s,
                                              const Colour c)
{
    vector<Square> squares;
    squares.reserve(4 * (min(NumFiles, NumRanks) - 1));
    CheckAttacks(b, s, c, IsEmptyOrCapturable(), CardinalsAndDiagonals,
                 back_inserter(squares));
    return LegalMoves(b, s, squares);
}

static bool CanCastle(const Board& b, const Square kingSq, const Square rookSq,
                      const pair<int, int> dir)
{
    optional<Square> sweep = kingSq;
    while (sweep += dir)
    {
        if (*sweep == rookSq)
        {
            // there's nothing between the rook and king
            return true;
        }
        else if (b[*sweep])
        {
            // a piece stands between the rook and king
            return false;
        }
    }
    assert(false && "CanCastle() rookSq and kingSq not connected by dir!");
    return false;
}

vector<Move> Chess::GenerateKingMoves(const Board& b, const Square s,
                                             const Colour c)
{
    vector<Square> squares;
    squares.reserve(8);
    CheckAttacks(b, s, c, IsEmptyOrCapturable(), CardinalsAndDiagonals,
                 back_inserter(squares), 1);
    auto moves = LegalMoves(b, s, squares);

    const optional<Square>& leftCastleRook =
        c == Colour::White ? b.wLeftCastleRook : b.bLeftCastleRook;
    const optional<Square>& rightCastleRook =
        c == Colour::White ? b.wRightCastleRook : b.bRightCastleRook;

    if (leftCastleRook && CanCastle(b, s, *leftCastleRook, make_pair(-1, 0)))
    {
        moves.push_back(Move(s, *(s + make_pair(-2, 0))));
    }

    if (rightCastleRook && CanCastle(b, s, *rightCastleRook, make_pair(1, 0)))
    {
        moves.push_back(Move(s, *(s + make_pair(2, 0))));
    }

    return moves;
}

vector<Move> Chess::GeneratePieceMoves(const Board& b, const Square sq,
                                              const Piece p)
{
    switch (p.type)
    {
    case Type::Pawn:
        return GeneratePawnMoves(b, sq, p.colour);
    case Type::Knight:
        return GenerateKnightMoves(b, sq, p.colour);
    case Type::Bishop:
        return GenerateBishopMoves(b, sq, p.colour);
    case Type::Rook:
        return GenerateRookMoves(b, sq, p.colour);
    case Type::Queen:
        return GenerateQueenMoves(b, sq, p.colour);
    case Type::King:
        return GenerateKingMoves(b, sq, p.colour);
    }
}

vector<Move> Chess::GenerateMoves(const Board& b, const Colour c)
{
    vector<Move> ret;

    for (int file = 0; file < NumFiles; ++file)
    {
        for (int rank = 0; rank < NumRanks; ++rank)
        {
            const Square sq(file, rank);

            if (!b[sq] || b[sq]->colour != c)
            {
                continue;
            }

            auto results = GeneratePieceMoves(b, sq, *b[sq]);

            ret.insert(ret.end(), results.begin(), results.end());
        }
    }

    return ret;
}
