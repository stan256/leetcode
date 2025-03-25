object Trie extends App {
  // 208. Implement Trie (Prefix Tree)
  class Trie() {
    case class Node(var isPresent: Boolean = false, val children: Array[Node] = Array.fill[Node](26)(null))

    val topLevel = Node()

    def insert(word: String): Unit = {
      var currentLevel = topLevel
      for (c <- word) {
        if (currentLevel.children(c - 'a') == null) currentLevel.children(c - 'a') = Node()
        currentLevel = currentLevel.children(c - 'a')
      }
      currentLevel.isPresent = true
    }

    def search(word: String): Boolean = {
      var currentLevel = topLevel
      for (c <- word) {
        if (currentLevel != null) currentLevel = currentLevel.children(c - 'a')
      }
      currentLevel != null && currentLevel.isPresent
    }

    def startsWith(prefix: String): Boolean = {
      var currentLevel = topLevel
      for (c <- prefix) {
        if (currentLevel != null) currentLevel = currentLevel.children(c - 'a')
      }
      currentLevel != null
    }
  }

  // // 208. Implement Trie (Prefix Tree)
  class Trie_2() {
    val parent = TrieNode()

    def insert(word: String): Unit = {
      var level = parent
      for (c <- word) {
        if (level.arr(c) == null) level.arr(c) = TrieNode()
        level = level.arr(c)
      }
      level.end = true
    }

    def search(word: String): Boolean = {
      var level = parent
      val result = word.find(c => {
        if (level.arr(c) != null) {
          level = level.arr(c)
          false
        } else true
      })
      result.isEmpty && level.end
    }

    def startsWith(prefix: String): Boolean = {
      var level = parent
      val result = prefix.find(c => {
        if (level.arr(c) != null) {
          level = level.arr(c)
          false
        } else true
      })
      result.isEmpty
    }

    case class TrieNode(val arr: Array[TrieNode] = Array.fill[TrieNode]('a' + 26)(null), var end: Boolean = false)
  }

}
