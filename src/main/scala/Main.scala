import scala.util.control.Breaks._
object Connect4 {
  println("Please input dimensions of the board")
  println("-------------------------------------")
  var rows = getRows()
  var cols = getCols()
  var movesCounter = 0
  var playerOneMoves = 0
  var playerTwoMoves = 0


  //check for invalid board size needs to be implemented here, loop until the size is good
  while(!isValidSize(rows,cols)){
    rows = getRows()
    cols = getCols()
  }
  var maxMoves = rows * cols
  val board = Array.fill(rows, cols)('*')
  var moveHistoryOne = Array.fill(maxMoves/2)("")
  var moveHistoryTwo = Array.fill(maxMoves/2)("")
 
 
  def main(args: Array[String]): Unit = {
    while(true){
    
    var player = 'O'
    while (true) {
      formatedOutput()
      print(s"Player $player's turn. Which column do you want to play in (1-$cols)?")
      val col = scala.io.StdIn.readInt() - 1
      if (!nextMove(player, col)) {
        println("Invalid move. Try again.")
      } else {     
        if (isFourInRow(player)) {
          return
        }
        if(isDraw()){
          return
        }
        player = if (player == 'X') 'O' else 'X'
  }
}
}
}


def printBoard(): Unit = {
  for (row <- board) {
      for (cell <- row) {
        print(cell + " ")
      }
      println()
    }
}

def formatedOutput(): Unit ={
    printBoard()
    println()
    movesPrinter(moveHistoryOne, "O")
    println()
    movesPrinter(moveHistoryTwo, "X")
    println()
}

def isDraw(): Boolean = {
  if(movesCounter == maxMoves){
    formatedOutput()
    println("The game is draw!")
    return true}
  return false
}

def movesPrinter(movesHistory: Array[String],player: String): Unit ={
    print(s"Player $player history: ")
    for(cell <- movesHistory){
    print(cell+" ")
  }

}
// check if the board is too small or if the difference between rows and cols is greater than 2
def isValidSize(row: Int, col: Int): Boolean ={
  if (row < 6 || col < 7){
    println("Board size can't be less than 6x7. Please change your input")
    return false
  }
  else if(row - col >= 2 || rows - cols <= -3){
    println("Difference between rows and columns in the board can't be greater than 2. Please change your input")
    return false
  }
  return true
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
      if (board(row)(col) == '*') {
        board(row)(col) = player
        if(player == 'O'){
          moveHistoryOne(playerOneMoves) = (col + 1).toString
          playerOneMoves += 1
        }
        else{
          moveHistoryTwo(playerTwoMoves) = (col + 1).toString
          playerTwoMoves += 1
        }
        movesCounter += 1
        print("\u001b[2J")
        return true
      }
    }
    return false
  }
   def isFourInRow(player: Char): Boolean = {
    for (row <- 0 until rows) {
      for (col <- 0 until cols) {
        if (board(row)(col) == player) {
          if (checkFour(player, row, col, 1, 0)) {
            formatedOutput()
            print(s"Player $player won!" )
            println()
            return true
          }
          if (checkFour(player, row, col, 0, 1)) {
            formatedOutput()
            print(s"Player $player won!" )
            println()
            return true
          }
          if (checkFour(player, row, col, 1, 1)) {
            formatedOutput()
            print(s"Player $player won!" )
            println()
            return true
          }
          if (checkFour(player, row, col, -1, 1)) {
            formatedOutput()
            print(s"Player $player won!" )
            println()
            return true
          }
        }
      }
    }
    return false
  }
 
  def checkFour(player: Char, row: Int, col: Int, rowdif: Int, coldif: Int): Boolean = {
    var count = 0
    var r = row
    var c = col
    while (r >= 0 && r < rows && c >= 0 && c < cols && board(r)(c) == player) {
      count += 1
      if (count == 4) {
        return true
      }
      r += rowdif
      c += coldif
    }
    return false
  }
}
 

 
  