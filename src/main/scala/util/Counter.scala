package util

import java.util.concurrent.atomic.AtomicLong

/**
 * A thread-safe incrementing counter. The first number returned
 * will be 1, and each successive call to next() will return an
 * incremented value.
 *
 * An instance of this class may be safely shared across threads.
 * The counter value will be shared.
 *
 * This counter cannot be reset. If you have an application which
 * needs to reset its counters, you must create a new Counter as
 * needed and pass this to functions which need it.
 *
 * NOTE: Please do not roll your own global counter class! This
 *       will cause problems when the test suites are run concurrently
 *       with multiple threads.
 */
class Counter {
  private val n = AtomicLong(0)

  /**
   * Increment the counter, then get and return it.
   * This method may be safely called by multiple threads.
   */
  def next() = n.incrementAndGet()
}
