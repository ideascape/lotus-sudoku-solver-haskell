# lotus-sudoku-solver-haskell

The challenge is, given a partially filled puzzle board, your program must fill in empty spaces according to the rules of the game. 

The game board is a lotus shape with three defined segment types: rings, clockwise arcs, and counter-clockwise arcs. 

This is an illustration of an empty board with the rings highlighted (each color represents a different ring):

<img src="https://github.com/ideascape/lotus-sudoku-solver-haskell/blob/master/readme-pics/lss1.jpg" width="400"/>

This is an illustration of an empty board with the counter-clockwise arcs highlighted (each blue or white swath represents a different arc):

<img src="https://github.com/ideascape/lotus-sudoku-solver-haskell/blob/master/readme-pics/lss2.jpg" width="400"/>

Clockwise arcs are the same, except in the opposite direction. The rules of the game: 

1. Every arc contains the numbers 1 to 7

2. Every ring contains the numbers 1 to 7

3. No number can be repeated in any ring or arc.

Like so:

<img src="https://github.com/ideascape/lotus-sudoku-solver-haskell/blob/master/readme-pics/lss8.gif" width="400"/>

There are 49 spaces on the board. For purposes of program input, the spaces will be numbered like this:

<img src="https://github.com/ideascape/lotus-sudoku-solver-haskell/blob/master/readme-pics/lss3.jpg" width="400"/>
<img src="https://github.com/ideascape/lotus-sudoku-solver-haskell/blob/master/readme-pics/lss4.jpg" width="400"/>
<img src="https://github.com/ideascape/lotus-sudoku-solver-haskell/blob/master/readme-pics/lss5.jpg" width="400"/>

This is an example of an partially filled board, waiting for a solution:

<img src="https://github.com/ideascape/lotus-sudoku-solver-haskell/blob/master/readme-pics/lss6.jpg" width="400"/>

The input for the program would be this: [5,0,0,0,1,6,0,0,0,0,3,0,0,0,7,0,6,2,1,0,0,0,1,7,0,0,6,0,0,5,0,3,6,7,2,0,0,2,1,0,0,4,0,0,4,0,0,1,0]

This is a legal solution for the same board:

<img src="https://github.com/ideascape/lotus-sudoku-solver-haskell/blob/master/readme-pics/lss7.jpg" width="400"/>

The correct output for the program would be this: [5,4,7,2,1,6,3,6,5,4,3,7,2,1,7,3,6,2,1,5,4,2,1,7,5,4,6,3,1,5,4,3,6,7,2,7,6,2,1,3,5,4,3,5,4,7,2,1,6]

If there is no valid solution, the program will output an empty array.

The code in lotusSolverFinal.hs has been commented pretty thoroughly, if you're curious how the program works. 

To test the program, download Haskell at https://www.haskell.org/platform/ and clone this repo. Then just type 'runhaskell lotusSolverFinal.hs'. This will ask the program for to solve several several given puzzles (see testSolsValid in main) and print the found solutions for each.

Credit to Dain Vermaak for most of the above images and for the idea for the lotus soduku solver challenge.
