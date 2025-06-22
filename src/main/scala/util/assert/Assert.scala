package util.assertion

import basil.compileConstants.debugMode

var disableAssertions = false

/*
 * An assertion that can be disabled at run-time --nodebug flag, or compile-time.
 */
inline def debugAssert(inline assertion: Boolean, inline message: String*): Unit = {
  // Conditionally inlined assertion and should not allocate anything except in evaluation or failure of [[assertion]]
  inline if (debugMode) then {
    if (!disableAssertions && !assertion) then
      if (message.nonEmpty) then {
        throw AssertionError(message.mkString(""))
      } else {
        throw AssertionError()
      }
  } else {
    ()
  }
}


/*
 * Like (assert) but with a slightly different message. Shadows's scalas built-in require function..
 */
inline def require(inline assertion: Boolean, inline message: String*): Unit = {
  inline if (debugMode) then {
    if (!disableAssertions && !assertion) then
      if (message.nonEmpty) then {
        throw AssertionError("Requirement failed: " + message.mkString(""))
      } else {
        throw AssertionError("Requirement failed.")
      }
  } else {
    ()
  }
}
