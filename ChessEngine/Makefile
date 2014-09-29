CXX=clang++
CXXFLAGS=-g -c -std=c++11 -Wall -Wextra -pedantic -DNOEXCEPT_SUPPORTED \
  -DSTDREGEX_SUPPORTED
LDFLAGS=-g

all: chess.out

chess.out: Main.o BoardRep.o Evaluation.o
	$(CXX) -o chess.out Main.o BoardRep.o Evaluation.o $(LDFLAGS)

Main.o: BoardRep.h Utility.h Search.h Search.inl

BoardRep.o: BoardRep.h Utility.h

Evaluation.o: BoardRep.h
