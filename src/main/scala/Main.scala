import java.io._
import scala.io._
object Connect4 {
  var rows = 0
  var cols = 0
  var movesCounter = 0
  var player1 = ""
  var player2 = ""
  var player = ""  
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
        chooseGameSymbol()
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
        print(cell +" ")
      }
      println()
    }
}

def formatedOutput(): Unit ={
    printBoard()
    println()
    movesPrinter(moveHistoryOne, player1)
    println()
    movesPrinter(moveHistoryTwo, player2)
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

def chooseGameSymbol(): Unit = {
  println("Press 1 to choose your game symbols")
  println("Press 0 to stick to the default symbols")
  val input = scala.io.StdIn.readLine()
   if(input.isEmpty){
    println("Please input valid number")
    Thread.sleep(1500)
    println("\u001b[2J")
    chooseGameSymbol()
  }
  else if(input.forall(Character.isDigit)){
    
    if(input.toInt == 1){
      println("Size limit for player simbols is 1")
      print("Player 1: ")
      player1 = scala.io.StdIn.readLine()
      print("Player 2: ")
      player2 = scala.io.StdIn.readLine()
      
      if(player1.isEmpty || player2.isEmpty){
        println("Symbols cannot be empty")
        Thread.sleep(1500)
        println("\u001b[2J")
        chooseGameSymbol()
      }
    }
    else if(input.toInt == 0){
      player1 = "O"
      player2 = "X"
    }
    else{
      println("Please input valid number")
      Thread.sleep(1500)
      println("\u001b[2J")
      chooseGameSymbol()

    }
  }
  else{
    println("Please input valid number")
    Thread.sleep(1500)
    println("\u001b[2J")
    chooseGameSymbol()
  }

  if(player1.size > 1 || player2.size > 1){
    println("Please keep the symbols smaller than 2 characters")
    Thread.sleep(1500)
    println("\u001b[2J")
    chooseGameSymbol()
  }

  if(player1.equals(player2)){
    println("Symbols cannot be the same")
    Thread.sleep(1500)
    println("\u001b[2J")
    chooseGameSymbol()

  }
  println("\u001b[2J")
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
        if(player == player1){
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
        player = if (player.equals(player2)) player1 else player2
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
  player = player1
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
  moveWriter.write("\n")
  moveWriter.write(player1+"\n")
  moveWriter.write(player2+"\n")
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
    if(lineCounter == 4){player1 = line}
    if(lineCounter == 5){player2 = line}
    lineCounter +=1
  }
  player = if(moveHistoryOne.size == moveHistoryTwo.size)  player1 else player2
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
 

 
  