#include <algorithm>
#include <cctype>
#if STDREGEX_SUPPORTED
#include <regex>
#else
#include <boost/regex.hpp>
using boost::regex;
using boost::smatch;
#endif
#include <stdexcept>
#include "BoardRep.h"

using namespace Chess;
using namespace std;

size_t hash<Square>::operator()(const Square &s) const
{
    return hash<int>()(s.id);
}

size_t hash<Move>::operator()(const Move &m) const
{
    size_t h1 = hash<Square>()(m.from);
    size_t h2 = hash<Square>()(m.to);
    return h1 ^ (h2 << 1);
}

std::string Chess::ToString(const Type t)
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

std::string Chess::ToString(const Colour c)
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

std::ostream &Chess::operator<<(std::ostream &os, const Colour c)
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

ostream &Chess::operator<<(ostream &os, const Piece p)
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

ostream &Chess::operator<<(ostream &os, const Square s)
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

boost::optional<Square> Chess::operator+(const Square left,
                                         const pair<int, int> right) NOEXCEPT
{
    if (PutsOutOfRange(left, right))
    {
        return boost::none;
    }
    else
    {
        return Square(left.File() + right.first, left.Rank() + right.second);
    }
}

Square &Chess::operator+=(Square &left, const pair<int, int> right)
{
    if (auto s = left + right)
    {
        left = *s;
    }
    throw out_of_range("operator+= puts out of range");
}

boost::optional<Square> Chess::operator-(const Square left,
                                         const pair<int, int> right) NOEXCEPT
{
    return left + make_pair(-right.first, -right.second);
}

Square &Chess::operator-=(Square &left, const pair<int, int> right)
{
    if (auto s = left - right)
    {
        left = *s;
    }
    throw out_of_range("operator-= puts out of range");
}

static int StringToFile(const string &s)
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
    return make_pair(left.Rank() - right.Rank(), left.File() - right.File());
}

Square Chess::StringToSquare(const string &s)
{
    static const regex squareRegex(R "(([[:alpha:]]+)(\d+))");

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

std::ostream &Chess::operator<<(std::ostream &os, const Move m)
{
    return os << "(" << m.from << ", " << m.to << ")";
}

Move Chess::StringToMove(const string &s)
{
    static const regex moveRegex(
        R "(\s*\(?\s*([[:alpha:]]+\d+)\s*,\s*([[:alpha:]]+\d+)\s*\)?\s*)");

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
                assert(ret.squares[file][rank] == boost::none);
            }
        }
    }

    assert(ret.enPassantable == boost::none);
    ret.toMove = Colour::White;
    ret.wRightCastleRook = Square(7, 0);
    ret.wLeftCastleRook = Square(0, 0);
    ret.bRightCastleRook = Square(7, 7);
    ret.bLeftCastleRook = Square(0, 7);
    return ret;
}

boost::optional<Piece> &Board::operator[](const Square s)NOEXCEPT
{
    return squares[s.File()][s.Rank()];
}

const boost::optional<Piece> &Board::operator[](const Square s) const NOEXCEPT
{
    return squares[s.File()][s.Rank()];
}

bool Chess::operator==(const Board &left, const Board &right) NOEXCEPT
{
    return left.squares == right.squares &&
           left.enPassantable == right.enPassantable &&
           left.toMove == right.toMove &&
           left.wLeftCastleRook == right.wLeftCastleRook &&
           left.wRightCastleRook == right.wRightCastleRook &&
           left.bLeftCastleRook == right.bLeftCastleRook &&
           left.bRightCastleRook == right.bRightCastleRook;
}

bool Chess::operator!=(const Board &left, const Board &right) NOEXCEPT
{
    return !(left == right);
}

static void PrintBorder(ostream &os, const bool printMoveMarker,
                        const unordered_set<int> &castleMarkers)
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

static void PrintHackyBorder(ostream &os, const bool printMoveMarker,
                             const boost::optional<Square> castleKings,
                             const boost::optional<Square> castleQueens)
{
    unordered_set<int> castleMarkers;
    // HACK: starting position of rooks and king assumed
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

static void PrintRank(ostream &os, const Board &b, const int rank)
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

ostream &Chess::operator<<(ostream &os, const Board &b)
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

boost::optional<string> Chess::CannotMove(const Board &b, const Move m)
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
        isMoveLegal = GeneratePawnMoves(b, m.from, piece->colour).count(m) > 0;
        break;
    case Type::Knight:
        isMoveLegal =
            GenerateKnightMoves(b, m.from, piece->colour).count(m) > 0;
        break;
    case Type::Bishop:
        isMoveLegal =
            GenerateBishopMoves(b, m.from, piece->colour).count(m) > 0;
        break;
    case Type::Rook:
        isMoveLegal = GenerateRookMoves(b, m.from, piece->colour).count(m) > 0;
        break;
    case Type::Queen:
        isMoveLegal = GenerateQueenMoves(b, m.from, piece->colour).count(m) > 0;
        break;
    case Type::King:
        isMoveLegal = GenerateKingMoves(b, m.from, piece->colour).count(m) > 0;
    }

    return isMoveLegal
               ? boost::none
               : boost::make_optional("cannot move " + ToString(piece->type) +
                                      " to " + ToString(m.to));
}

static void HandlePawnMove(Board &b, const Move m,
                           boost::optional<Square> enPassantable) NOEXCEPT
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
        b[*enPassantable] = boost::none;
    }
}

static void HandleRookMove(Board &b, const Move m, const Colour c) NOEXCEPT
{
    if (c == Colour::White)
    {
        if (b.wLeftCastleRook == m.from)
        {
            b.wLeftCastleRook = boost::none;
        }
        else if (b.wRightCastleRook == m.from)
        {
            b.wRightCastleRook = boost::none;
        }
    }
    else
    {
        if (b.bLeftCastleRook == m.from)
        {
            b.bLeftCastleRook = boost::none;
        }
        else if (b.bRightCastleRook == m.from)
        {
            b.bRightCastleRook = boost::none;
        }
    }
}

static void HandleKingMove(Board &b, const Move m, const Colour c) NOEXCEPT
{
    const int fileDifference = (m.to - m.from).first;
    if (c == Colour::White)
    {
        if (fileDifference > 1 && b.wRightCastleRook)
        {
            MakeMove(b, Move(*b.wRightCastleRook, *(m.to - make_pair(1, 0))));
        }
        else if (fileDifference < -1 && b.wLeftCastleRook)
        {
            MakeMove(b, Move(*b.wLeftCastleRook, *(m.to + make_pair(1, 0))));
        }
        b.wLeftCastleRook = boost::none;
        b.wRightCastleRook = boost::none;
    }
    else
    {
        if (fileDifference > 1 && b.bRightCastleRook)
        {
            MakeMove(b, Move(*b.bRightCastleRook, *(m.to - make_pair(1, 0))),
                     false);
        }
        else if (fileDifference < -1 && b.bLeftCastleRook)
        {
            MakeMove(b, Move(*b.bLeftCastleRook, *(m.to + make_pair(1, 0))),
                     false);
        }
        b.bLeftCastleRook = boost::none;
        b.bRightCastleRook = boost::none;
    }
}

void Chess::MakeMove(Board &b, const Move m, const bool switchTurn)
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
        boost::none; // this will be overwritten if player double-moves a pawn

    // handle special moves (castling, pawn double and en passent)
    switch (piece->type)
    {
    case Type::Pawn:
        HandlePawnMove(b, m, currEnPassentable);
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
    b[m.from] = boost::none;
    b[m.to] = *piece;
}

void Chess::MakeMoveChecked(Board &b, const Move m, const bool switchTurn)
{
    if (auto reason = CannotMove(b, m))
    {
        throw invalid_argument(*reason);
    }
    MakeMove(b, m, switchTurn);
}

Board Chess::NextBoard(const Board &b, const Move m, const bool switchTurn)
{
    Board ret(b);
    MakeMove(ret, m, switchTurn);
    return ret;
}

Board Chess::NextBoardChecked(const Board &b, const Move m,
                              const bool switchTurn)
{
    Board ret(b);
    MakeMoveChecked(ret, m, switchTurn);
    return ret;
}

template <typename predicate, typename container>
static unordered_set<Square>
CheckAttacks(const Board &b, const Square s, const Colour c, const predicate &p,
             const container &vecs, const int range = -1)
{
    unordered_set<Square> ret;
    ret.reserve(vecs.size());

    for (const auto vec : vecs)
    {
        int remainingRange = range;
        for (auto toSquare = s + vec; toSquare && remainingRange != 0;
             toSquare = *toSquare + vec, --remainingRange)
        {
            if (p(b, *toSquare, c))
            {
                ret.insert(*toSquare);
            }
            else
            {
                break;
            }
        }
    }

    return ret;
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
    bool operator()(const Board &b, const Square s, Colour) const NOEXCEPT
    {
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
    bool operator()(const Board &b, const Square s, const Colour) const NOEXCEPT
    {
        return !b[s];
    }
};

// suitable for pawn capture
struct IsCapturable
{
    bool operator()(const Board &b, const Square s, const Colour c) const
        NOEXCEPT
    {
        return b[s] && b[s]->colour != c;
    }
};

// suitable for non-pawn movement and capture
struct IsEmptyOrCapturable
{
    bool operator()(const Board &b, const Square s, const Colour c) const
        NOEXCEPT
    {
        return IsEmpty()(b, s, c) || IsCapturable()(b, s, c);
    }
};
}

static const array<pair<int, int>, 2> WhitePawnAttacks = {
    { make_pair(-1, 1), make_pair(1, 1) }
};

static const array<pair<int, int>, 2> BlackPawnAttacks = {
    { make_pair(-1, -1), make_pair(1, -1) }
};

static const array<pair<int, int>, 8> LShapes = {
    { make_pair(-2, -1), make_pair(-2, 1), make_pair(-1, -2), make_pair(-1, 2),
      make_pair(1, -2),  make_pair(1, 2),  make_pair(2, -1),  make_pair(2, 1) }
};

static const array<pair<int, int>, 4> Diagonals = {
    { make_pair(-1, -1), make_pair(-1, 1), make_pair(1, -1), make_pair(1, 1) }
};

static const array<pair<int, int>, 4> Cardinals = {
    { make_pair(0, -1), make_pair(0, 1), make_pair(-1, 0), make_pair(1, 0) }
};

static const array<pair<int, int>, 8> CardinalsAndDiagonals = {
    { make_pair(0, -1),  make_pair(0, 1),  make_pair(-1, 0), make_pair(1, 0),
      make_pair(-1, -1), make_pair(-1, 1), make_pair(1, -1), make_pair(1, 1) }
};

// Board irrelevant for pawn, knight and king, and colour only relevant to pawn

static unordered_set<Square> PawnThreatening(const Square s, const Colour c)
{
    return CheckAttacks(
        Board(), s, c, IsThreatening(),
        c == Colour::White ? WhitePawnAttacks : BlackPawnAttacks, 1);
}

static unordered_set<Square> KnightThreatening(const Square s)
{
    return CheckAttacks(Board(), s, Colour(), IsThreatening(), LShapes, 1);
}

static unordered_set<Square> BishopThreatening(const Board &b, const Square s)
{
    return CheckAttacks(b, s, Colour(), IsThreatening(), Diagonals);
}

static unordered_set<Square> RookThreatening(const Board &b, const Square s)
{
    return CheckAttacks(b, s, Colour(), IsThreatening(), Cardinals);
}

static unordered_set<Square> QueenThreatening(const Board &b, const Square s)
{
    return CheckAttacks(b, s, Colour(), IsThreatening(), CardinalsAndDiagonals);
}

static unordered_set<Square> KingThreatening(const Square s)
{
    return CheckAttacks(Board(), s, Colour(), IsThreatening(),
                        CardinalsAndDiagonals, 1);
}

static unordered_set<Square> ThreateningSquares(const Board &b, const Square s,
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

unordered_set<Square> Chess::ThreatenedSquares(const Board &b, const Colour c)
{
    unordered_set<Square> ret;

    for (int file = 0; file < NumFiles; ++file)
    {
        for (int rank = 0; rank < NumRanks; ++rank)
        {
            const auto piece = b.squares[file][rank];
            if (piece && piece->colour == c)
            {
                const auto squares =
                    ThreateningSquares(b, Square(file, rank), *piece);
                ret.insert(squares.begin(), squares.end());
            }
        }
    }

    return ret;
}

bool Chess::InCheck(const Board &b)
{
    for (const Square s : ThreatenedSquares(b, !b.toMove))
    {
        const auto piece = b[s];
        if (piece && piece->colour == b.toMove && piece->type == Type::King)
        {
            return true;
        }
    }
    return false;
}

static unordered_set<Move> LegalMoves(const Board &b, const Square from,
                                      const unordered_set<Square> &tos)
{
    unordered_set<Move> ret;
    ret.reserve(tos.size());

    for (const Square to : tos)
    {
        const Move m(from, to);
        const auto next = NextBoard(b, m);
        if (!InCheck(next))
        {
            ret.insert(m);
        }
    }

    return ret;
}

unordered_set<Move> Chess::GeneratePawnMoves(const Board &b, const Square s,
                                             const Colour c)
{
    const bool isWhite = c == Colour::White;

    // insert the non-attacks
    const array<pair<int, int>, 1> moveVec = { { isWhite ? make_pair(0, 1)
                                                         : make_pair(0, -1) } };
    const int doubleMoveRank = isWhite ? 1 : NumRanks - 2;
    auto ret =
        LegalMoves(b, s, CheckAttacks(b, s, c, IsEmpty(), moveVec,
                                      s.Rank() == doubleMoveRank ? 2 : 1));

    // insert the attacks
    const auto attacks = LegalMoves(
        b, s, CheckAttacks(b, s, c, IsCapturable(),
                           isWhite ? WhitePawnAttacks : BlackPawnAttacks, 1));
    ret.insert(attacks.begin(), attacks.end());

    return ret;
}

unordered_set<Move> Chess::GenerateKnightMoves(const Board &b, const Square s,
                                               const Colour c)
{
    return LegalMoves(b, s,
                      CheckAttacks(b, s, c, IsEmptyOrCapturable(), LShapes, 1));
}

unordered_set<Move> Chess::GenerateBishopMoves(const Board &b, const Square s,
                                               const Colour c)
{
    return LegalMoves(b, s,
                      CheckAttacks(b, s, c, IsEmptyOrCapturable(), Diagonals));
}

unordered_set<Move> Chess::GenerateRookMoves(const Board &b, const Square s,
                                             const Colour c)
{
    return LegalMoves(b, s,
                      CheckAttacks(b, s, c, IsEmptyOrCapturable(), Cardinals));
}

unordered_set<Move> Chess::GenerateQueenMoves(const Board &b, const Square s,
                                              const Colour c)
{
    return LegalMoves(b, s, CheckAttacks(b, s, c, IsEmptyOrCapturable(),
                                         CardinalsAndDiagonals));
}

unordered_set<Move> Chess::GenerateKingMoves(const Board &b, const Square s,
                                             const Colour c)
{
    return LegalMoves(b, s, CheckAttacks(b, s, c, IsEmptyOrCapturable(),
                                         CardinalsAndDiagonals, 1));
}
