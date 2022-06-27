/*
package utils

import util.RunUtils

import scala.util.Random
import scala.language.postfixOps

import scala.util.matching.Regex
import sys.process._


object TestUtils{
  val errorRegex: Regex =".*\\.bpl\\((\\d+),\\d+\\): Error: This assertion might not hold.".r
  val finalLineRegex: Regex ="Boogie program verifier finished with (\\d+) verified, (\\d+) error".r

  def processBoogieFile(filePath:String,elfFilePath:Option[String]=None):BoogieReturnResult = {
    val state = RunUtils.generateVCs(s"samples/that_compile/$filePath.bil",if(elfFilePath.isEmpty)s"samples/that_compile/$filePath.elf" else s"samples/that_compile/${elfFilePath.get}")
    val outputFilePath = s"/tmp/${Random.alphanumeric.take(20).mkString("")}.bpl"
    RunUtils.writeToFile(state,outputFilePath)

    val output = s"boogie $outputFilePath"!!



    val errorLines = errorRegex.findAllMatchIn(output).map(x=>x.group(1))
    val finalLine = finalLineRegex.findFirstMatchIn(output).get

    BoogieReturnResult(errorLines.map(_.toInt).toList, finalLine.group(1).toInt, finalLine.group(2).toInt)
  }
}
*/