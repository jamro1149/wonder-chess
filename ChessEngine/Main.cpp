#include <iostream>
#include <string>
#include <boost/scope_exit.hpp>
#include "BoardRep.h"

using namespace std;
using namespace Chess;

int main()
{
	cout << boolalpha;

	Board b = Board::HackyMakeDefaultStart();
	cout << b << endl;

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
			cout << (CannotMove(b, m) == boost::none) << "\n";
			MakeMove(b, m);
			cout << b << "\n";
		}
		catch (logic_error e)
		{
			cout << e.what() << "\n";
		}
	}
}
