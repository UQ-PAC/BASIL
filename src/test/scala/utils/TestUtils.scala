import scala.util.Random
import scala.language.postfixOps
import utils.BoogieReturnResut
import sys.process._


object TestUtils{
  val errorRegex=".*\\.bpl\\((\\d+),\\d+\\): Error: This assertion might not hold.".r
  val finalLineRegex="Boogie program verifier finished with (\\d+) verified, (\\d+) error".r

  def processBoogieFile(filePath:String,elfFilePath:Option[String]=None):BoogieReturnResut = {
    val state = RunUtils.generateVCs(s"samples/that_compile/$filePath.bil",if(elfFilePath==None)s"samples/that_compile/$filePath.elf" else s"samples/that_compile/${elfFilePath.get}")
    val outputFilePath = s"/tmp/${Random.alphanumeric.take(20).mkString("")}.bpl"
    RunUtils.writeToFile(state,outputFilePath)

    val output = (s"boogie $outputFilePath"!!)



    val errorLines = errorRegex.findAllMatchIn(output).map(x=>x.group(1))
    val finalLine = finalLineRegex.findFirstMatchIn(output).get

    BoogieReturnResut(errorLines.map(_.toInt).toList, finalLine.group(1).toInt, finalLine.group(2).toInt)
  }
}
