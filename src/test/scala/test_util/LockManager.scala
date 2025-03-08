package test_util

import scala.collection.mutable.HashMap
import java.util.concurrent.locks.ReentrantReadWriteLock

/**
 * Manages a number of mutex locks, each identified by a key.
 * Users should construct an instance of this class and place
 * it in a shared location. The `withLock` method takes a lock
 * name and executes the body while holding the specified lock.
 *
 * The key values will be used as HashMap keys, so they must be
 * hashable with sensible equality.
 */
class LockManager[T] {

  /**
   * The map containing the locks. The "locks" are simply AnyRef
   * objects which act as locks through the `synchronized` method.
   * Additionally, the map object itself is used to synchronise
   * reads/writes to the map.
   *
   * This is a mutable map.
   */
  private val locksMap: HashMap[T, AnyRef] = HashMap()

  /**
   * Executes the given body while holding the given lock.
   * The named lock will be created if needed.
   *
   * Usage:
   *
   *     val manager = LockManager[String]()
   *
   *     // ...
   *
   *     manager.withLock("lock name") {
   *       println("this code will hold the lock!")
   *     }
   */
  def withLock(key: T)(body: => Unit): Unit = {
    val lock = locksMap.synchronized {
      locksMap.getOrElseUpdate(key, Object())
    }

    lock.synchronized {
      body
    }
  }
}
