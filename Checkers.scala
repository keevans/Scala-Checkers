/*
Kate Evans
Project 6 - Scala
*/

object Driver{
  //main function controls how the checkers are moved
  def main(args: Array[String]){
  	val b = new Board(8) //board created
  	b.setBoard //board set
    b.printBoard //initial board is printed
    println("\nEnter the next move or Q to quit:")
    var input = scala.io.StdIn.readLine() //user input accepted from the command line
    processInput(input) // input process and the board altered
    while(input != "Q"){ //while loop that continues to accept user input. Ends when Q is entered
      println("\nEnter the next move or Q to quit:")
      input = scala.io.StdIn.readLine()
      processInput(input)
    }
    //function that processes the user input and makes changes to the board
    def processInput(input: String){
       if(input != "Q"){ //checking if the input is a coordinate
          //breaking up the input string to place the coordinates in tuples
         try { 
            var split = input.split(", ")
            var split1 = split(0).toCharArray
            var split2 = split(1).toCharArray
            var origin = (split1(0),split1(1).asDigit)
            var destination = (split2(0),split2(1).asDigit)
            b.movePiece(origin, destination) //piece moved
            b.printBoard //edited board is printed
            b.flipBoard // board fliped
         } 
         catch {
           case e: Exception => println("Invalid Input.\nInput should be formatted A1, B2.\nDon't forget the space.")
         }
        }
    }
  }
  //Board class that allows a board to be createed
  //takes an Int for size as a parameter
  class Board(val size: Int) {
    //maps for converting coordinates
    val decodeLet = Map('A' -> 1, 'B' -> 2, 'C' -> 3, 'D' -> 4, 'E' -> 5, 'F' -> 6, 'G' -> 7, 'H' -> 8)
    val decodeNum = Map(1 -> 'A', 2 -> 'B', 3 -> 'C', 4 -> 'D', 5 -> 'E', 6 -> 'F', 7 -> 'G', 8 -> 'H')
    var flip = false //orientation of the board
    //the board is created, an array of checkered pieces
    var board = Array.ofDim[CheckerPiece](size+1,size+1)
	  for(a <- 1 until board.length; b <- 1 until board.length){
        board(a)(b) = new CheckerPiece((decodeNum(a), b), '-','-')
	  }
    //function for setting the initial board
    //loops through the board placing the appropriate pieces in their places
    //creates CheckerPieces as it does this
    def setBoard(){
      for(x <- 1 until board.length){
    	  for(y <- 1 until board.length){
          if(x < board.length/2){
    			  if(x%2 == 1 && y%2 == 1){
    				  setCell((decodeNum(x),y), new CheckerPiece((decodeNum(x),y), 'X', 'O'))
    			  }
            else if(x%2 == 0 && y%2 == 0){
              setCell((decodeNum(x),y), new CheckerPiece((decodeNum(x),y), 'X','O'))
            }
          }
          else if(x > board.length/2+1){
            if(x%2 == 1 && y%2 == 1){
              setCell((decodeNum(x),y), new CheckerPiece((decodeNum(x),y), 'O','X'))
            }
            else if(x%2 == 0 && y%2 == 0){
              setCell((decodeNum(x),y), new CheckerPiece((decodeNum(x),y), 'O','X'))
            }
          }
    	  }
      }
    }
    
    //function for moving CheckerPieces
    //takes a Tuple for the coordinate of the piece to be moved and a Tuple for the destination
    def movePiece(origin:(Char,Int), destination:(Char,Int)){
       //checks if the origin holds a piece and if the destination is empty
       if(board(decodeLet(origin._1))(origin._2).displayPiece != '-' && board(decodeLet(destination._1))(destination._2).displayPiece == '-'){
          var piece = board(decodeLet(origin._1))(origin._2)
          //case for a regular none jumping move
          if(decodeLet(origin._1) - decodeLet(destination._1) == 1 || decodeLet(origin._1) - decodeLet(destination._1) == -1){
            //moves the pieces around the board
            board(decodeLet(origin._1))(origin._2) = new CheckerPiece(origin,'-', '-')
            piece.setCoord(destination)
            board(decodeLet(destination._1))(destination._2) = piece
          }
          else if(decodeLet(origin._1) - decodeLet(destination._1) == 2){ //the first case for a jumping move
            if(origin._2 - destination._2 == 2){//checks the direction of the jump
              var jump = board(decodeLet(origin._1)-1)(origin._2-1)
              if(jump.displayPiece == piece.getOpponent){//checks that the piece being jumped is an opponents piece
                //moves the pieces around the board
                board(decodeLet(origin._1))(origin._2) = new CheckerPiece(origin,'-', '-')
                board(decodeLet(origin._1)-1)(origin._2-1) = new CheckerPiece((decodeNum(decodeLet(origin._1)-1),(origin._2-1)),'-', '-')
                piece.setCoord(destination)
                board(decodeLet(destination._1))(destination._2) = piece
              }
              else{
                println("Invalid move - not jumping over opponent")
              }
            }
            else if(origin._2 - destination._2 == -2){//second check for jumping direction
              var jump = board(decodeLet(origin._1)-1)(origin._2+1)
              if(jump.displayPiece == piece.getOpponent){//checks that the piece being jumped is an opponents piece
                //moves the pieces
                board(decodeLet(origin._1))(origin._2) = new CheckerPiece(origin,'-', '-')
                board(decodeLet(origin._1)-1)(origin._2+1) = new CheckerPiece((decodeNum(decodeLet(origin._1)-1),(origin._2+1)),'-', '-')
                piece.setCoord(destination)
                board(decodeLet(destination._1))(destination._2) = piece
              }
              else{
                println("Invalid move - not jumping over opponent")
              }
            }
            else{
              println("Invalid move - direction")
            }
          }
          else if(decodeLet(origin._1) - decodeLet(destination._1) == -2){//second check for a jumping move
            if(origin._2 - destination._2 == 2){//checks for the jumping direction
              var jump = board(decodeLet(origin._1)+1)(origin._2-1)
              if(jump.displayPiece == piece.getOpponent){//checks that the piece being jumped is an opponents piece
                //moves the pieces
                board(decodeLet(origin._1))(origin._2) = new CheckerPiece(origin,'-', '-')
                board(decodeLet(origin._1)+1)(origin._2-1) = new CheckerPiece((decodeNum(decodeLet(origin._1)+1),(origin._2-1)),'-', '-')
                piece.setCoord(destination)
                board(decodeLet(destination._1))(destination._2) = piece
              }
              else{
                println("Invalid move - not jumping over opponent")
              }
            }
            else if(origin._2 - destination._2 == -2){//second check for jumping direction
              var jump = board(decodeLet(origin._1)+1)(origin._2+1)
              if(jump.displayPiece == piece.getOpponent){//checks that the piece being jumped is an opponents piece
                //moves the pieces
                board(decodeLet(origin._1))(origin._2) = new CheckerPiece(origin,'-', '-')
                board(decodeLet(origin._1)+1)(origin._2+1) = new CheckerPiece((decodeNum(decodeLet(origin._1)+1),(origin._2+1)),'-', '-')
                piece.setCoord(destination)
                board(decodeLet(destination._1))(destination._2) = piece
              }
              else{
                println("Invalid move - not jumping over opponent")
              }
            }
            else{
              println("Invalid move - wrong direction")
            }
          }
          else{
            println("Invalid move - moving too far")
          }
       }
       else{
         println("Invalid move - there is a piece in that square")
       }
    }
    //function to flip the board
    def flipBoard(){
      if(flip == false){
        flip = true
      }
      else{
        flip = false
      }
    }

    //function to set a piece in a cell
    //takes a Tuple for a coordinate and a CheckerPiece
	  def setCell(coord:(Char,Int), piece:CheckerPiece){
		  var x = decodeLet(coord._1)
      piece.setCoord(coord)
		  board(x)(coord._2) = piece
	  }
    //function to print the board based on its orientation
	  def printBoard(){
      println()
      if(flip){
        for(a <- board.length-1 to 1 by -1){
          for(b <- board.length-1 to 1 by -1){
            print(board(a)(b).displayPiece + " ")
          }
          println()
        }
      }
      else{
  	    for(a <- 1 until board.length){
          for(b <- 1 until board.length){
           	print(board(a)(b).displayPiece + " ")
          }
          println()
  	    }
      }
	  }
  }//end Board
  
  //class for CheckerPiece
  //takes a Tuple for the coordinate, a Char for the color of the piece, and a Char for the color of the opponent
  class CheckerPiece(val coord: (Char, Int), val _color: Char, val _opponent: Char){
    var coords = coord
    val color = _color
    val opponent = _opponent
    //set the coordinates of the piece
    def setCoord(coord:(Char,Int)){
        coords = coord
    }
    //get the piece's opponent
    def getOpponent() : Char = {
      return opponent
    }
    //display the piece for printing
    def displayPiece(): Char = {
    	return color
    }
  }//end Piece

}//end Driver


