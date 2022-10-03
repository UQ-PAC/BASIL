import org.scalatest.funsuite.AnyFunSuite

/*
In future, we could have a predefined structure for test file names and locations for a cleaner and
more manageable test suite.
*/


class SystemTests extends AnyFunSuite {
  val globalTestDirectory = "examples/"

  test("secret_write") {
    val directory = globalTestDirectory + "secret_write/"
    main(directory + "secret_write.adt",
      directory + "secret_write.relf",
      directory + "secret_write.spec")
  }
}
