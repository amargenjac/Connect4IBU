object Connect4 {
  val rows = 6
  val cols = 7
  val win = 4
  val empty = '.'

  val board = Array.fill(rows, cols)(empty)
 
 
  def main(args: Array[String]): Unit = {
    var player = 'O'
   
    while (true) {
      for (row <- board) {
      for (cell <- row) {
        print(cell + " ")
      }
      println()
    }
  
    println(s"Player $player's turn. Which column do you want to play in (1-$cols)?")
     val col = scala.io.StdIn.readInt() - 1
      if (!nextMove(player, col)) {
        println("Invalid move. Try again.")
      } else {     
        player = if (player == 'X') 'O' else 'X'
  }
}
}


def nextMove(player: Char, col: Int): Boolean = {
    if (col < 0 || col >= cols) {
      return false
    }
    for (row <- rows - 1 to 0 by -1) {
      if (board(row)(col) == empty) {
        board(row)(col) = player
        return true
      }
    }
    return false
  }
}
 

 
  