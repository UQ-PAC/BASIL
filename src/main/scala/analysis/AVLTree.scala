package analysis

/**
 * Node of the AVL tree.
 * @param key
 * @param value
 * @param height
 * @param left
 * @param right
 * @tparam K key type
 * @tparam V value type
 */
case class Node[K, V](var key: K, var value: V, var height: Int, var left: Option[Node[K, V]], var right: Option[Node[K, V]])

/**
 * AVL tree implementation. Ref. https://cs.indstate.edu/~kbalaraman/anew.pdf
 * @param ordering
 * @tparam K key type
 * @tparam V value type
 */
class AVLTree[K, V](ordering: Ordering[K]) {
  private var root: Option[Node[K, V]] = None

  // Get the height of the node
  private def height(node: Option[Node[K, V]]): Int = node.map(_.height).getOrElse(0)

  // Rotate right
  private def rotateRight(y: Node[K, V]): Node[K, V] = {
    val x = y.left.get
    val T2 = x.right
    x.right = Some(y)
    y.left = T2
    y.height = Math.max(height(y.left), height(y.right)) + 1
    x.height = Math.max(height(x.left), height(x.right)) + 1
    x
  }

  // Rotate left
  private def rotateLeft(x: Node[K, V]): Node[K, V] = {
    val y = x.right.get
    val T2 = y.left
    y.left = Some(x)
    x.right = T2
    x.height = Math.max(height(x.left), height(x.right)) + 1
    y.height = Math.max(height(y.left), height(y.right)) + 1
    y
  }

  // Get balance factor of node N
  private def getBalance(node: Option[Node[K, V]]): Int = node.map(n => height(n.left) - height(n.right)).getOrElse(0)

  // Insert a key-value pair
  def insert(key: K, value: V): Unit = {
    def insertNode(node: Option[Node[K, V]], key: K, value: V): Node[K, V] = {
      if (node.isEmpty) return Node(key, value, 1, None, None)

      val n = node.get

      if (ordering.lt(key, n.key)) n.left = Some(insertNode(n.left, key, value))
      else if (ordering.gt(key, n.key)) n.right = Some(insertNode(n.right, key, value))
      else {
        n.value = value
        return n
      }

      n.height = 1 + Math.max(height(n.left), height(n.right))
      val balance = getBalance(Some(n))

      // Left Left Case
      if (balance > 1 && ordering.lt(key, n.left.get.key)) return rotateRight(n)

      // Right Right Case
      if (balance < -1 && ordering.gt(key, n.right.get.key)) return rotateLeft(n)

      // Left Right Case
      if (balance > 1 && ordering.gt(key, n.left.get.key)) {
        n.left = Some(rotateLeft(n.left.get))
        return rotateRight(n)
      }

      // Right Left Case
      if (balance < -1 && ordering.lt(key, n.right.get.key)) {
        n.right = Some(rotateRight(n.right.get))
        return rotateLeft(n)
      }

      n
    }

    root = Some(insertNode(root, key, value))
  }

  // Search for a value by key
  def search(key: K): Option[V] = {
    def searchNode(node: Option[Node[K, V]], key: K): Option[V] = {
      if (node.isEmpty) return None

      val n = node.get

      if (ordering.equiv(key, n.key)) Some(n.value)
      else if (ordering.lt(key, n.key)) searchNode(n.left, key)
      else searchNode(n.right, key)
    }

    searchNode(root, key)
  }

  // Delete a key-value pair
  def delete(key: K): Unit = {
    def minValueNode(node: Node[K, V]): Node[K, V] = {
      var current = node
      while (current.left.isDefined) current = current.left.get
      current
    }

    def deleteNode(node: Option[Node[K, V]], key: K): Option[Node[K, V]] = {
      if (node.isEmpty) return None

      val n = node.get

      if (ordering.lt(key, n.key)) n.left = deleteNode(n.left, key)
      else if (ordering.gt(key, n.key)) n.right = deleteNode(n.right, key)
      else {
        if (n.left.isEmpty || n.right.isEmpty) {
          val temp = if (n.left.isDefined) n.left else n.right
          if (temp.isEmpty) return None
          else return temp
        } else {
          val temp = minValueNode(n.right.get)
          n.key = temp.key
          n.value = temp.value
          n.right = deleteNode(n.right, temp.key)
        }
      }

      n.height = Math.max(height(n.left), height(n.right)) + 1
      val balance = getBalance(Some(n))

      // Left Left Case
      if (balance > 1 && getBalance(n.left) >= 0) return Some(rotateRight(n))

      // Left Right Case
      if (balance > 1 && getBalance(n.left) < 0) {
        n.left = Some(rotateLeft(n.left.get))
        return Some(rotateRight(n))
      }

      // Right Right Case
      if (balance < -1 && getBalance(n.right) <= 0) return Some(rotateLeft(n))

      // Right Left Case
      if (balance < -1 && getBalance(n.right) > 0) {
        n.right = Some(rotateRight(n.right.get))
        return Some(rotateLeft(n))
      }

      Some(n)
    }

    root = deleteNode(root, key)
  }
}

// Example usage
object AVLTreeExample extends App {
  val avl = new AVLTree[Int, String](Ordering.Int)
  avl.insert(10, "Value10")
  avl.insert(20, "Value20")
  avl.insert(30, "Value30")
  avl.insert(40, "Value40")
  avl.insert(50, "Value50")
  avl.insert(25, "Value25")

  println(avl.search(25)) // Some(Value25)
  println(avl.search(100)) // None

  avl.delete(25)
  println(avl.search(25)) // None
}
