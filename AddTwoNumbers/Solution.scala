/**
 * Definition for singly-linked list.
 * class ListNode(_x: Int = 0, _next: ListNode = null) {
 * var next: ListNode = _next
 * var x: Int = _x
 * }
 */
object Solution {
  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    
    def helper(l1: ListNode, l2: ListNode, carry: Int): ListNode = {
      (l1, l2) match {
        case (null, null) =>
          if (carry == 0) null else ListNode(carry, null)
        case (a, null) =>
          val sum = (a.x + carry) % 10
          val newCarry = (a.x + carry) / 10
          new ListNode(sum, helper(a.next, null, newCarry))
        case (null, b) =>
          val sum = (b.x + carry) % 10
          val newCarry = (b.x + carry) / 10
          new ListNode(sum, helper(b.next, null, newCarry))
        case (a, b) =>
          val sum = (a.x + b.x + carry) % 10
          val newCarry = (a.x + b.x + carry) / 10
          new ListNode(sum, helper(a.next, b.next, newCarry))
      }
    }
    
    helper(l1, l2, 0)
  }
}
