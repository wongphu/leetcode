object Solution {
  
  def firstMissingPositive(nums: Array[Int]): Int = {
    val positives = nums.view.filter(_ > 0).toSet
    (1 to positives.size + 1).find(n => !positives.contains(n)).getOrElse(1)
  }
}
