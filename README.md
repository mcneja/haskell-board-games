haskell-board-games
===================

Board game experiments written in Haskell. Using the
[Gloss] (https://hackage.haskell.org/package/gloss) library for interface.

## TicTacToe

A basic tic-tac-toe game. No AI.

## XiangHex

The beginnings of a version of [Xiang Hex] (http://www.chessvariants.org/index/msdisplay.php?itemid=MSxianghex),
a hex-grid interpretation of [Xiangqi] (http://www.chessvariants.org/xiangqi.html) (Chinese chess). It isn't very far along yet.

## HexDame

A complete (but still pretty rough) implementation of [HexDame] (http://en.wikipedia.org/wiki/Hexdame),
a hex-grid interpretation of international draughts.

Use the mouse to drag pieces. Captures are the roughest part of the UI; you drag to the terminal position
of the capture. In rare cases there can be multiple paths from the same start to the same end; in this case
the game will pick one.

Additional Keys:

  * Esc: Quit
  * N: New game
  * Space: Have AI take a turn using minimax searching
  * R: Have AI take a turn by choosing an available move randomly
  * K: Debug toggle a piece between pawn and king
  * D: Debug drop; hold while dropping a piece to put it anywhere
  
The AI is using very basic [minimax] (http://en.wikipedia.org/wiki/Minimax) searching, a few levels deep.
There's a debug display of the score of each move.
The scoring function is a very basic net change in the difference between the number of pieces on each side.
