#include <iostream>
#include <string>
#include <unordered_set>
#include <boost/scope_exit.hpp>
#include "BoardRep.h"

using namespace std;
using namespace Chess;

int main()
{
    Board current = Board::HackyMakeDefaultStart();
    cout << current << endl;

    unordered_multiset<Board> history = {current};

    for (string s; getline(cin, s);)
    {
        BOOST_SCOPE_EXIT(void)
        {
            cout << endl;
        }
        BOOST_SCOPE_EXIT_END;

        try
        {
            auto m = StringToMove(s);
            MakeMoveChecked(current, m);
            cout << current << "\n";
            if (InCheck(current))
            {
                cout << current.toMove << " is in check!\n";
            }
            
            history.insert(current);
            if (history.count(current) >= 3)
            {
                cout << "The game is a draw by threefold repetition\n";
                return 0;
            }
        }
        catch (logic_error e)
        {
            cout << e.what() << "\n";
        }
    }
}
