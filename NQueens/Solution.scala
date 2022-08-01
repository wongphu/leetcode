object Solution {
  import math.abs
  
  def solveNQueens(n: Int): List[List[String]] = {
    
    def canPlace(nextQueen: (Int,Int), queens: List[(Int,Int)]): Boolean = {
      val (nextRow, nextCol) = nextQueen
      queens.forall {
        case (row, col) => col != nextCol && abs(row - nextRow) != abs(col - nextCol)
      }
    }
    
    def neighbors(row: Int, board: List[(Int,Int)]): List[List[(Int,Int)]] = {
      (1 to n).toList.collect {
        case col if canPlace((row, col), board) =>
          (row, col) :: board
      }
    }
    
    def rec(row: Int, boards: List[List[(Int,Int)]] = List(List())): List[List[(Int,Int)]] = {
      if (row == n) {
        boards
      } else {
        boards.flatMap { board => rec(row+1, neighbors(row+1, board)) }
      }
    }
    
    def render(board: List[(Int,Int)]): List[String] = {
      board.reverse.map {
        case (row, col) =>
          ("." * (col-1) + "Q" + "." * (n-col))
      }
    }
    
    rec(0).map(render)
  }
}

