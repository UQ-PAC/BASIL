package util

import java.util.concurrent.ThreadLocalRandom

/**
 * This mixin helps a class ensure that it is only accessed from a single
 * thread. This can be useful for debugging concurrency issues.
 */
case class ThreadLocalCheck private (thread: Thread) {
  def this() = this(Thread.currentThread)

  /**
   * Asserts that the caller is the same thread as the one which
   * originally constructed this object. This is intended to be called
   * within methods of classes which mix in this class.
   */
  def assertUniqueThread() = {
    lazy val cls = this.getClass.getCanonicalName
    val current = Thread.currentThread
    assert(
      current eq thread,
      s"unexpected multi-threaded usage of class '$cls'.\n"
        + s"object created by thread $thread but now accessed by $current"
    )
  }
}

/**
 * Sleeps for a randomised length of time. While debugging, this can be placed
 * between computations where there is a suspected race condition. Hopefully,
 * this will make race conditions more obvious.
 *
 * Optionally, an argument can be given. If given, it will compute the
 * argument, then return its result after the randomised length of time. This
 * simulates a computation which takes a longer length of time.
 */
def magicBugRevealingSleep[T](e: => T): T = {
  val result = e
  Thread.sleep(ThreadLocalRandom.current.nextInt(10, 1000))
  // Thread.`yield`()
  result
}

def magicBugRevealingSleep(): Unit = magicBugRevealingSleep {}
