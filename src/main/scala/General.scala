object General extends App {
  // 704. Binary Search
  def binarySearch(arr: Array[Int], n: Int): Int = {
    var left = 0
    var right = arr.length - 1

    while (left <= right) {
      val middleIndex = (left + right)/2

      val middle = arr(middleIndex)
      if (n == middle)
        return middleIndex
      else if (n > middle)
        left = middleIndex + 1
      else
        right = middleIndex - 1
    }

    -1
  }

  // 2043. Simple Bank System
  class Bank(balances: Array[Long]) {
    def addMoney(n: Int, money: Long): Unit = {
      balances(n) += money
    }

    def subtractMoney(n: Int, money: Long): Unit = {
      balances(n) -= money
    }

    def accountExists(account: Int): Boolean = account <= balances.length

    def transfer(account1: Int, account2: Int, money: Long): Boolean = {
      if (accountExists(account1) && accountExists(account2) && balances(account1 - 1) >= money) {
        addMoney(account2 - 1, money)
        subtractMoney(account1 - 1, money)
        true
      } else false
    }

    def deposit(account: Int, money: Long): Boolean = {
      if (accountExists(account)) {
        addMoney(account - 1, money)
        true
      } else false
    }

    def withdraw(account: Int, money: Long): Boolean = {
      if (accountExists(account) && balances(account - 1) >= money) {
        subtractMoney(account - 1, money)
        true
      } else false
    }
  }

  // 1166. Design File System
  class FileSystem() {
    case class Node(var value: Int, val map: collection.mutable.HashMap[String, Node] = collection.mutable.HashMap.empty[String, Node])

    private val map = collection.mutable.HashMap.empty[String, Node]

    def createPath(path: String, value: Int): Boolean = {
      val parts = path.drop(1).split("/")
      var search = map
      parts.dropRight(1).foreach(part => {
        if (search != null) {
          val maybeNode = search.get(part)
          search = maybeNode.map(_.map).orNull
        }
      })
      if (search == null) return false
      val maybeNode = search.get(parts.last)
      if (maybeNode.nonEmpty) return false
      else search(parts.last) = Node(value)
      true
    }

    def get(path: String): Int = {
      val parts = path.drop(1).split("/")
      var search = map
      parts.dropRight(1).foreach(part => {
        if (search != null) {
          val maybeNode = search.get(part)
          search = maybeNode.map(_.map).orNull
        }
      })
      if (search == null || !search.contains(parts.last)) return -1
      else search(parts.last).value
    }
  }

  // 1166. Design File System
  class FileSystem2() {
    val map = collection.mutable.HashMap.empty[String, Int]

    def createPath(path: String, value: Int): Boolean = {
      val parts = path.drop(1).split("/")
      if (parts.length == 1) {
        if (map.contains(path)) return false
        else {
          map(path) = value
          return true
        }
      }

      val parent = path.substring(0, path.lastIndexOf("/"))
      if (map.contains(parent) && !map.contains(path)) {
        map(path) = value
        true
      } else false
    }

    def get(path: String): Int = map.getOrElse(path, -1)
  }


  // 146. LRU Cache
  class LRUCache(capacity: Int) {
    case class ListNode(var key: Int, var value: Int, var prev: ListNode, var next: ListNode)

    val map = collection.mutable.Map.empty[Int, ListNode]

    val head = ListNode(-1, -1, null, null)
    val tail = ListNode(-1, -1, head, null)
    head.next = tail

    def get(key: Int): Int = {
      if (!map.contains(key)) return -1

      remove(map(key))
      add(map(key))
      map(key).value
    }

    def add(node: ListNode): Unit = {
      val prevEnd = tail.prev
      node.prev = prevEnd
      prevEnd.next = node
      node.next = tail
      tail.prev = node
    }

    def remove(node: ListNode): Unit = {
      node.prev.next = node.next
      node.next.prev = node.prev
    }

    def put(key: Int, value: Int): Unit = {
      if (map.contains(key)) {
        remove(map(key))
      }

      val node = ListNode(key, value, null, null)
      map.put(key, node)
      add(node)

      if (map.size > capacity) {
        val rm = head.next
        remove(rm)
        map.remove(rm.key)
      }

    }
  }

}
