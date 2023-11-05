
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


  def reverseListTwoPointers(head: ListNode): ListNode = {
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
        case (a, b) if ((a != null && b != null) && a.x < b.x) || b == null =>
          iterator1 = a.next
          k.next = a
          k.next.next = iterator2
          k = k.next
        case (a, b) if ((a != null && b != null) && a.x >= b.x) || a == null =>
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

  // 143. Reorder List
  def reorderList(head: ListNode): Unit = {
    var iterator = head
    var stack = scala.collection.mutable.Stack.empty[Int]
    while (iterator != null) {
      stack.push(iterator.x)
      iterator = iterator.next
    }
    val length = stack.length
    stack = stack.take(length / 2)

    iterator = head
    var change = true
    while (stack.nonEmpty) {
      if (change) {
        iterator.next = ListNode(stack.pop(), iterator.next)
        change = false
      } else
        change = true
      iterator = iterator.next
    }

    if (length > 1) {
      if (length % 2 == 0)
        iterator.next = null
      else
        iterator.next.next = null
    }
  }

  //    private val node: ListNode = ListNode(1, ListNode(2, ListNode(3, ListNode(4))))
  //  private val node: ListNode = ListNode(1)
  //  private val node: ListNode = ListNode(1, ListNode(2))
  //  private val node: ListNode = ListNode(1, ListNode(2, ListNode(3)))
  //  reorderList(node)

  // 19. Remove Nth Node From End of List
  def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
    def reverse(list: ListNode) = {
      var h = list
      var res: ListNode = null

      while (h != null) {
        res = ListNode(h.x, res)
        h = h.next
      }
      res
    }

    def dropNthElement(list: ListNode, n: Int): ListNode = {
      if (n == 1) {
        list.next
      } else {
        var counter = 1
        var temp: ListNode = null

        while (counter < n) {
          counter += 1
          temp = if (temp == null) list else temp.next
        }

        if (temp.next.next != null) {
          temp.next = temp.next.next
        } else {
          temp.next = null
        }

        list
      }
    }

    reverse(dropNthElement(reverse(head), n))
  }
  //  private val node: ListNode = ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(5)))))
  //  println(removeNthFromEnd(node, 2))

  // 138. Copy List with Random Pointer
  class Node(var _value: Int) {
    var value: Int = _value
    var next: Node = null
    var random: Node = null
  }

  def copyRandomList(head: Node): Node = {
    var temp = head
    val oldNewMap = scala.collection.mutable.Map.empty[Node, Node]
    while (temp != null) {
      oldNewMap += (temp -> new Node(temp.value))
      temp = temp.next
    }

    temp = head
    while (temp != null) {
      val copy = oldNewMap(temp)
      copy.next = if (temp.next != null) oldNewMap(temp.next) else null
      copy.random = if (temp.random != null) oldNewMap(temp.random) else null
      temp = temp.next
    }

    if (head != null) oldNewMap(head) else null
  }

  val node = new Node(7)
  node.next = new Node(13)
  node.next.next = new Node(11)
  node.next.next.next = new Node(10)
  node.next.next.next.next = new Node(1)
  copyRandomList(node)
}
