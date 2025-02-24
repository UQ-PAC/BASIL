
import munit.FunSuite
import ir.*
import ir.dsl.*


class BeansTest extends FunSuite {

 test ("crash") {
    val p = prog(proc("main", block("lmain", Assert(FalseLiteral), ret)))
    assertEquals(p , null)
  }

}

