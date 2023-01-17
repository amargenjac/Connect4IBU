import java.io._
import scala.io._
object Connect4 {
  var rows = 0
  var cols = 0
  var movesCounter = 0
  var player = "O"  
  var maxMoves = rows * cols
  var board = Array.fill(rows, cols)("*")
  var moveHistoryOne = ""
  var moveHistoryTwo = ""
  val boardFile = "Saved_Board_State.txt"
  val gameFile = "Saved_Game_State.txt"

 
 
  def main(args: Array[String]): Unit = {
    
    while(true){
      print("\u001b[2J")
      printMenu()
      var optionSelected = scala.io.StdIn.readInt()
      if(optionSelected==0){
        return
      }
      else if(optionSelected==1){
        println("\u001b[2J")
        setBoardConfig()
        play()
        println("Press enter to continue")
        var enter = scala.io.StdIn.readLine()
        print("\u001b[2J")
      }
      else if(optionSelected==2){
        println("\u001b[2J")
        loadGame()
        play()
        println("Press enter to continue")
        var enter = scala.io.StdIn.readLine()
        print("\u001b[2J")
      }
      else{
        print("Invalid option, try again!")
        Thread.sleep(1000)
      }
}
}

def printMenu(): Unit = {
  println("Welcome to IBU Connect 4!")
  println("Press 1 to start new game!")
  println("Press 2 to continue game!")
  println("Press 0 to exit the game!")

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

def movesPrinter(movesHistory: String,player: String): Unit ={
    print(s"Player $player history: ")
    print(movesHistory)
}

def getRows(): Int ={
  println("Please input number of rows you want to have in your game.")
  rows = scala.io.StdIn.readInt()
  rows
}

def getCols(): Int ={
  println("Please input number of columns you want to have in your game.")
  cols = scala.io.StdIn.readInt()
  cols
}

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

def nextMove(player: String, col: Int): Boolean = {
    if (col < 0 || col >= cols) {
      return false
    }
    for (row <- rows - 1 to 0 by -1) {
      if (board(row)(col) == "*") {
        board(row)(col) = player
        if(player == "O"){
          moveHistoryOne = moveHistoryOne + (col + 1).toString()
        }
        else{
          moveHistoryTwo = moveHistoryTwo + (col + 1).toString()
        }
        movesCounter += 1
        print("\u001b[2J")
        return true
      }
    }
    return false
}

def isFourInRow(player: String): Boolean = {
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
 
def checkFour(player: String, row: Int, col: Int, rowdif: Int, coldif: Int): Boolean = {
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

def isDraw(): Boolean = {
  if(movesCounter == maxMoves){
    formatedOutput()
    println("The game is draw!")
    return true}
  return false
}

def play(): Unit = {
  while (true) {
      formatedOutput()
      print(s"Player $player's turn. Which column do you want to play in (1-$cols)?")
      val col = scala.io.StdIn.readLine()
      if(col.equals("s")){
        saveGame()
        println("Game saved")
        Thread.sleep(1000)
        print("\u001b[2J")
      }
      if (col.forall(Character.isDigit)){
        if (!nextMove(player, col.toInt-1)) {
        println("Invalid move. Try again.")
      } else {     
        if (isFourInRow(player)) {
          return
        }
        if(isDraw()){
          return
        }
        player = if (player == "X") "O" else "X"
  }

      }
      
}
}

def setBoardConfig(): Unit = {
  println("Please input dimensions of the board")
  println("-------------------------------------")
  rows = getRows()
  cols = getCols()
  while(!isValidSize(rows,cols)){
    rows = getRows()
    cols = getCols()
  }
  player = "O"
  maxMoves = rows * cols
  moveHistoryOne = ""
  moveHistoryTwo = ""
  board = Array.fill(rows, cols)("*")
  print("\u001b[2J")
}

def saveGame(): Unit = {
  boardSave()
  movesSave()
}

def boardSave(): Unit = {
  val boardWriter = new PrintWriter(new File(boardFile))
  for(row <- board){
  for(cell<-row){
    boardWriter.write(cell)
  }
}
boardWriter.close()
}

def movesSave(): Unit = {
  val moveWriter = new PrintWriter(new File(gameFile))
  moveWriter.write(rows+"\n")
  moveWriter.write(cols+"\n")
  for(i <- moveHistoryOne){
    moveWriter.write(i)
  }
  moveWriter.write("\n")
  for(i <- moveHistoryTwo){
    moveWriter.write(i)
  }
  moveWriter.close()

}

def loadGame(): Unit = {
  var row = 0
  var col = 0
  var lineCounter = 0
  for (line <- Source.fromFile(gameFile).getLines){
    if(lineCounter == 0){rows = line.toInt}
    if(lineCounter == 1){cols = line.toInt}
    if(lineCounter == 2){moveHistoryOne = line}
    if(lineCounter == 3){moveHistoryTwo = line}
    lineCounter +=1
  }
  player = if(moveHistoryOne.size == moveHistoryTwo.size)  "O" else "X"
  maxMoves = rows * cols
  board = Array.fill(rows, cols)("*")
  for(line <- Source.fromFile(boardFile).getLines){
    for(char <- line){
      board(row)(col) = char.toString()
      col += 1
      if(col == cols){
        row += 1
        col = 0
      }
    }
  }
}

}
 

 
  