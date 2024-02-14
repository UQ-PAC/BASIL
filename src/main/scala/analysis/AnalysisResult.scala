package analysis

/**
 * Analysis Results String utility trait
 * @tparam T type of analysis results
 */
trait AnalysisResult[T] {

  def encodeAnalysisResults(result: T) : String

  def parseAnalysisResults(input: String): String
}
