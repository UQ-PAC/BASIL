package util.assertion

import basil.compileConstants.debugMode

/*
 * Conditionally inlined assertion that should not allocate anything
 */
inline def assert(inline assertion: Boolean, inline message: Option[String] = None): Unit = {
  inline if (!debugMode) then ()
  else {
    if assertion then
      throw (message match {
        case Some(x) => AssertionError(x)
        case None => AssertionError()
      })
  }
}
