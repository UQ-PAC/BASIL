package util.assertion

import basil.compileConstants.debugMode

var disableAssetions = false

/*
 * Conditionally inlined assertion that should not allocate anything
 */
inline def assert(inline assertion: Boolean, inline message: String*): Unit = {
  inline if (debugMode) then {
    if (!disableAssetions && !assertion) then
      if (message.nonEmpty) then {
        throw AssertionError(message.mkString(""))
      } else {
        throw AssertionError()
      }
  } else {
    ()
  }
}
