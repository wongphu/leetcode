/**
 * Definition for a binary tree node.
 * class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
 *   var value: Int = _value
 *   var left: TreeNode = _left
 *   var right: TreeNode = _right
 * }
 */
object Solution {
  def isValidBST(root: TreeNode): Boolean = {
    
    def valid(node: TreeNode, minval: Long, maxval: Long): Boolean = {
      node == null || (
        node.value < maxval &&
        node.value > minval &&
        valid(node.left, minval, node.value) &&
        valid(node.right, node.value, maxval)
      )
    }
    
    valid(root, Long.MinValue, Long.MaxValue)
  }
}
