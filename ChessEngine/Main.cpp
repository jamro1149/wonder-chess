#include <iostream>
#include <map>
#include <regex>
#include <string>
#include <unordered_set>
#include <boost/scope_exit.hpp>
#include "BoardRep.h"
#include "Evaluation.h"
#include "Search.h"

using namespace std;
using namespace Chess;

int main()
{
    Board current = Board::HackyMakeDefaultStart();
    cout << current << endl;

    unordered_multiset<Board> history = {current};
    int reversibleHalfMoves = 0;
    //int numHalfMoves = 0;

    for (string s; getline(cin, s);)
    {
        BOOST_SCOPE_EXIT(void)
        {
            cout << endl;
        }
        BOOST_SCOPE_EXIT_END;

        try
        {
            if (s == "flush")
            {
                extern map<size_t, bool> CachedResults;
                CachedResults.clear();
                continue;
            }
            auto m = (s != "ai-turn"
                          ? StringToMove(s)
                          : NegaMaxSearch(current, &BasicShannonEvaluation, 4));

            //auto m = NegaMaxSearch(current, &BasicShannonEvaluation, 4);

            reversibleHalfMoves =
                MoveIsReversible(current, m) ? reversibleHalfMoves + 1 : 0;
            //++numHalfMoves;

            MakeMoveChecked(current, m);
            cout << current
                 << "\nThis board has a basic Shannon evaluation score of "
                 << BasicShannonEvaluation(current) << " centipawns\n";

            const bool inCheck = InCheck(current);
            const bool noMoves = NoMoves(current);

            if (inCheck && !noMoves)
            {
                cout << current.toMove << " is in check!\n";
            }
            else if (inCheck && noMoves)
            {
                cout << !current.toMove << " wins by checkmate!\n";
                return 0;
            }
            else if (!inCheck && noMoves)
            {
                cout << "The game is a draw by stalemate\n";
                return 0;
            }

            history.insert(current);
            if (history.count(current) >= 3)
            {
                cout << "The game is a draw by threefold repetition\n";
                return 0;
            }

            if (reversibleHalfMoves >= 100)
            {
                cout << "The game is a draw by the fifty-move rule\n";
                return 0;
            }

            //if (numHalfMoves > 20)
            //{
            //    cout << "That's all folks!\n";
            //    return 0;
            //}
        }
        catch (logic_error e)
        {
            cout << e.what() << "\n";
        }
    }
}
