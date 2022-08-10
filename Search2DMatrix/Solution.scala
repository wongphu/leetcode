object Solution {
  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    val m = matrix.size
    val n = matrix(0).size
    
    @scala.annotation.tailrec
    def loop(left: Int, right: Int): Boolean = {
      if (left > right) {
        false
      } else {
        val mid = (left + right) / 2
        val value = matrix(mid / n)(mid % n)
        if (target == value) {
          true
        } else if (target < value) {
          loop(left, mid-1)
        } else {
          loop(mid+1, right)
        }
      }
    }
    
    loop(0, m*n-1)
  }
}
