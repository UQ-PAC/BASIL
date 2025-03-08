package test_util

import scala.collection.immutable.HashMap
import java.util.concurrent.locks.ReentrantReadWriteLock

/**
 * A manager of mutually-exclusive locks keyed by strings.
 * Users should construct an instance and place it in a
 * shared location. The `withLock` method takes a lock name
 * and executes the body while holding the specified lock.
 */
class LockManager {
  /** An internal-use lock around the map of keyed locks. */
  private val mapLock = ReentrantReadWriteLock()
  private val read = mapLock.readLock
  private val write = mapLock.writeLock

  /**
   * The map containing the locks keyed by strings. The
   * "locks" are simply AnyRef objects which act as locks
   * through the `synchronized` method.
   */
  private var locksMap: HashMap[String, AnyRef] = HashMap()

  /**
   * Executes the given body while holding the given lock.
   * The named lock will be created if needed.
   *
   * Usage:
   *
   *     manager.withLock("lock name") {
   *       println("this code will hold the lock!")
   *     }
   */
  def withLock(key: String)(body: => Unit): Unit = {

    /*
     * This code uses read-write locks to synchronise
     * access to the hashmap of locks.
     *
     * First, the read lock is acquired. If the key has an
     * existing lock, that is used. Otherwise, the lock
     * is upgraded to a write lock and a new lock is created
     * and inserted for that key.
     *
     * There is an additional consideration when upgrading
     * the lock from read to write. The read lock must be dropped
     * before the write lock is obtained. Thus, it is possible for
     * another thread to modify the map in the intervening time.
     * After acquiring the write lock, we must check again that
     * the key does not exist before creating a new lock.
     */

    read.lock
    var namedLock = locksMap.get(key)
    if (namedLock.isEmpty) {
      // upgrade the lock, checking for possible modifications
      // that occured after `read` was dropped.
      read.unlock
      write.lock
      namedLock = locksMap.get(key)
      if (namedLock.isEmpty) {
        locksMap = locksMap + (key -> Object())
        namedLock = Some(locksMap(key))
      }
      assert(namedLock.nonEmpty)
      // downgrade the write lock to a read lock and continue
      read.lock
      write.unlock
    }
    // it is safe to drop the read lock here, as locks are
    // never modified or deleted once inserted into the map.
    read.unlock

    namedLock.get.synchronized {
      body
    }
  }
}
