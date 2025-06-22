package util.assertion

import basil.compileConstants.debugMode

/*
 * Conditionally inlined assertion that should not allocate anything
 */
inline def assert(inline assertion: Boolean, inline message: String*): Unit = {
  inline if (debugMode) then ()
  else {
    if (!assertion) then
      if (message.nonEmpty) then {
        throw AssertionError(message.mkString(""))
      } else {
        throw AssertionError()
      }
  }
}
