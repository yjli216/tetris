Jessica Li and Brian Hirsh

To run: type make
then type ./parser input.txt and run on codeworld

This game takes in an input.txt and builds tetris blocks from it.

Change the symbols in input.txt to the shape you want it to be
- make sure that the format in input.txt is correct
- there are 4 blocks, each block has 4 lines with 4 characters
- each character is either a space or a '#'
- each block is separated by a newline

files included:
parser.hs: parses the input file
tetris.hs: game logic

What we've learned:
We've integrated knowledge from the start of the course to the end.
We've used basic Haskell to build the interactive game and monads to build the parser. A random generator is used to generate a random block from the set of blocks that already exists.
