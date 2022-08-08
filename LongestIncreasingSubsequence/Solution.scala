object Solution {
  def lengthOfLIS(nums: Array[Int]): Int = {
    
    @scala.annotation.tailrec
    def loop(i: Int, acc: Vector[Int] = Vector()): Vector[Int] = {
      if (i == nums.size) {
        acc
      } else {
        val len = Range(0, i)
          .filter(nums(_) < nums(i))
          .map(acc(_))
          .maxOption
          .getOrElse(0) + 1
        loop(i+1, acc :+ len)
      }
    }
    
    val lens = loop(0)
    
    lens.maxOption.getOrElse(0)
  }
}
