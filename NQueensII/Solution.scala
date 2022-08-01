object Solution {
  import math.abs
  
  def totalNQueens(n: Int): Int = {
    
    def canPlace(newRow: Int, newCol: Int, queens: List[(Int,Int)]): Boolean = {
      queens.forall {
        case (row, col) => col != newCol && abs(row - newRow) != abs(col - newCol)
      }
    }
    
    def next(row: Int, queens: List[(Int,Int)]): List[List[(Int,Int)]] = {
      (1 to n).collect {
        case col if canPlace(row, col, queens) => (row, col) :: queens
      }.toList
    }
    
    def count(row: Int, lists: List[List[(Int,Int)]]): Int = {
      if (row == n) {
        lists.size
      } else {
        lists.map(list => count(row+1, next(row+1, list))).sum
      }
    }
    
    count(0, List(List()))
  }
}
