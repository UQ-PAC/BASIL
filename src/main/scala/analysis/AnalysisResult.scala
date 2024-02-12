package analysis

trait AnalysisResult[T] {

  def encodeAnalysisResults(result: T) : String

  def parseAnalysisResults(input: String): String
}
