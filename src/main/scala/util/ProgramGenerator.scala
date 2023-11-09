package util

import scala.collection.mutable.ArrayBuffer

object ProgramGenerator {

  private val operatorList = Array("+", "-", "*", "/", "|", "^", "&")
  def generateProgram(): String = {
    "#include <stdio.h>\n" +
      "void computeValue() {\n" +
      generateBody() +
      "\n}\n\n" +
      "void main() {\n" +
      "\tcomputeValue()\n" +
      "}\n"


  }
  def generateBody(): String = {
    var programBody = ""
    val rand = new scala.util.Random
    val variables = rand.between(1, 100)

    for (countVars <- 1 to variables) {

      val terms: ArrayBuffer[String] = ArrayBuffer()
      for (generatedVar <- 1 until countVars) {
        terms.addOne("variable" + generatedVar)
      }

      val numberOfConstants = rand.between(1, 100)
      for (i <- 1 to numberOfConstants) {
        terms.addOne(rand.between(1,500).toString)
      }
      val shuffledTerms = rand.shuffle(terms)
      var generatedLine = "\tint variable" + countVars + " ="
      for ((t, i) <- shuffledTerms.zipWithIndex) {
        if (i != 0) {
          generatedLine += operatorList(rand.nextInt(operatorList.length))
        }
        generatedLine += " " + t + " "
      }

      programBody += generatedLine + "\n"
    }
    programBody + "\n\tprintf(\"%d\", variable" + variables + ")"
  }
}
