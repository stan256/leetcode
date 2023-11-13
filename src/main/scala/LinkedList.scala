
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
  //  copyRandomList(node)

  // 2. Add Two Numbers
  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    var i1 = l1
    var i2 = l2
    var prevElement = ListNode(Int.MinValue, i1)
    var addOne = false

    while (i1 != null || i2 != null) {
      def calculateAndUpdateAddOne(a: Int, b: Int): Int = {
        var i = a + b + (if (addOne) 1 else 0)
        if (i >= 10) {
          addOne = true
          i = i % 10
        } else addOne = false
        i
      }

      (i1, i2) match {
        case (a, b) if a != null && b != null =>
          i1.x = calculateAndUpdateAddOne(a.x, b.x)
          prevElement = i1
          i1 = i1.next
          i2 = i2.next
        case (a, null) if a != null =>
          i1.x = calculateAndUpdateAddOne(a.x, 0)
          prevElement = i1
          i1 = a.next
        case (null, b) if b != null =>
          prevElement.next = ListNode(calculateAndUpdateAddOne(0, b.x))
          prevElement = prevElement.next
          i2 = b.next
        case (null, null) => throw new RuntimeException()
      }
    }

    if (i1 != null && addOne)
      i1.next = ListNode(1)

    if (prevElement != null && addOne)
      prevElement.next = ListNode(1)

    l1
  }

  //  addTwoNumbers(ListNode(2, ListNode(4, ListNode(3))), ListNode(5, ListNode(6, ListNode(4))))
  //  addTwoNumbers(ListNode(9), ListNode(9,ListNode(9,ListNode(9,ListNode(9)))))

  // 141. Linked List Cycle
  def hasCycle(head: ListNode): Boolean = {
    val map = scala.collection.mutable.Map.empty[ListNode, ListNode]

    var k = head
    var counter = 0
    while (k != null) {
      map += (k -> k.next)
      k = k.next
      counter += 1

      if (map.size != counter)
        return true
    }

    false
  }

  def hasCycle2(head: ListNode): Boolean = {
    var slow = head
    var fast = head
    while (fast != null) {
      slow = slow.next
      fast = if (fast.next != null) fast.next.next else
        return false
      if (slow == fast)
        return true
    }
    false
  }


  // 287. Find the Duplicate Number
  def findDuplicate(nums: Array[Int]): Int = {
    var slow = nums(0)
    var fast = nums(nums(0))

    do {
      slow = nums(slow)
      fast = nums(nums(fast))
    } while (slow != fast)

    var slow2 = 0
    while (slow != slow2) {
      slow = nums(slow)
      slow2 = nums(slow2)
    }

    slow2
  }
  //  println(findDuplicate(Array(1,1, 2)))
  //  println(findDuplicate(Array(1,2, 2)))
  //  println(findDuplicate(Array(1,1,1,1,1)))
  //  println(findDuplicate(Array(2,2,2,2,2)))
  //  println(findDuplicate(Array(3,3,3,3,3)))
  //  println(findDuplicate(Array(1,3,4,2,2)))
  //  println(findDuplicate(Array(3,1,3,4,2)))
  //  println(findDuplicate(Array(1,3,4,2,4)))

  class LRUCache(_capacity: Int) {
    case class DoubleLinkedList[T](x: T,
                                   key: Int,
                                   var previous: Option[DoubleLinkedList[T]] = None,
                                   var next: Option[DoubleLinkedList[T]] = None) {
      override def toString: String = {
        x.toString + " ; next: " + next.flatMap(_.next.map(_.x)) + "; previous: " + previous.flatMap(_.next.map(_.x))
      }
    }

    private val map = scala.collection.mutable.HashMap.empty[Int, DoubleLinkedList[Int]]
    private var mostRecentlyUsed: DoubleLinkedList[Int] = null
    private var leastRecentlyUsed: DoubleLinkedList[Int] = null

    def get(key: Int): Int = {
      val i = map.get(key) match {
        case Some(value) =>
          (value.previous, value.next) match {
            case (Some(a), Some(b)) =>
              a.next = Some(b)
              b.previous = Some(a)
              value.previous = None
              value.next = Some(mostRecentlyUsed)
            case (Some(a), None) =>
              leastRecentlyUsed = a
              a.next = None
              value.previous = None
              value.next = Some(mostRecentlyUsed)
              mostRecentlyUsed = value
            case _ =>
          }
          value.x
        case None => -1
      }
      println(i)
      i
    }

    def put(key: Int, value: Int): Unit = {
      if (mostRecentlyUsed == null) {
        mostRecentlyUsed = DoubleLinkedList(value, key)
        leastRecentlyUsed = mostRecentlyUsed
      } else {
        val temp = DoubleLinkedList(value, key, next = Some(mostRecentlyUsed))
        mostRecentlyUsed.previous = Some(temp)
        mostRecentlyUsed = temp
      }

      map.put(key, mostRecentlyUsed)

      if (map.size > _capacity) {
        leastRecentlyUsed = leastRecentlyUsed.previous.orNull
      }
    }

    // get (returns value + changes the order in queue)
    // create (changes the order in queue + removeLast?)
    // update (changes the order in queue + removeLast?)
    // removeLast (remove the tail node if needed)
  }

  private val cache = new LRUCache(2)
  cache.put(1, 1)
  cache.put(2, 2)
  cache.get(1) //     1
  cache.put(3, 3) //
  cache.get(2) //    -1
  cache.put(4, 4) //
  cache.get(1) //    -1
  cache.get(3) //     3
  cache.get(4) //     4
}
