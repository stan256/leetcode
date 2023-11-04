
object LinkedList extends App {

  // 206. Reverse Linked List
  case class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }
  def reverseList(head: ListNode): ListNode = {
    val stack = scala.collection.mutable.Stack.empty[Int]
    var next = head
    while (next != null) {
      stack.push(next.x)
      next = next.next
    }

    var start: ListNode = null
    var prev: ListNode = null
    while (stack.nonEmpty) {
      if (start == null) {
        start = ListNode(stack.pop())
      } else {
        if (prev == null) {
          start.next = ListNode(stack.pop())
          prev = start.next
        } else {
          prev.next = ListNode(stack.pop())
          prev = prev.next
        }
      }
    }
    start
  }


  def reverseList2(head: ListNode): ListNode = {
    var current = head
    var reverse: ListNode = null

    while (current != null) {
      reverse = ListNode(current.x, reverse)
      current = current.next
    }

    reverse
  }
//  private val node: ListNode = reverseList2(ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(5))))))
//  println(node)

  // 21. Merge Two Sorted Lists
  def mergeTwoLists(list1: ListNode, list2: ListNode): ListNode = {

    if (list1 == null)
      return list2
    else if (list2 == null)
      return list1

    val (first, second) = if (list1.x < list2.x) (list1, list2) else (list2, list1)

    var k = first
    var iterator1 = first.next
    var iterator2 = second

    while (iterator1 != null || iterator2 != null) {
      (iterator1, iterator2) match {
        case (a, b) if ((a != null && b != null ) && a.x < b.x) || b == null  =>
          iterator1 = a.next
          k.next = a
          k.next.next = iterator2
          k = k.next
        case (a, b) if ((a != null && b != null ) && a.x >= b.x) || a == null =>
          iterator2 = b.next
          k.next = b
          k.next.next = iterator1
          k = k.next
        case _ => throw new RuntimeException()
      }
    }


    first
  }
//  val node: ListNode = mergeTwoLists(ListNode(1, ListNode(2, ListNode(4))), ListNode(1, ListNode(3, ListNode(4))))
//  val node: ListNode = mergeTwoLists(ListNode(2), ListNode(1))
//  println(node)



}
