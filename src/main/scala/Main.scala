import scala.util.control.Breaks._
object Connect4 {
  val win = 4
  val empty = '.'
  println("Please input dimensions of the board")
  println("-------------------------------------")
  val rows = getRows()
  val cols = getCols()

  //check for invalid board size needs to be implemented here, loop until the size is good

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

// check if the board is too small or if the difference between rows and cols is greater than 2
def isValidSize(row: Int, col: Int): Boolean ={
  if (row < 6 || col < 7){
    println("Minimum size of the board should be 6x7. Please change your inputs")
    false
  }
  else if(row - col >= 2 || rows - cols <= -3){
    println("Difference between rows and columns can't be greater than 2")
    false
  }
  true

}

//helper function to input rows
def getRows(): Int ={
  println("Please input number of rows you want to have in your game.")
  val rows = scala.io.StdIn.readInt()
  rows
}

//helper function to input cols
def getCols(): Int ={
  println("Please input number of columns you want to have in your game.")
  val cols = scala.io.StdIn.readInt()
  cols
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
 

 
  