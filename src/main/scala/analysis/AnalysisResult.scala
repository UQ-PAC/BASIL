package analysis

/**
 * Analysis Results String utility used for testing analyses
 * parseAnalysisResults(encodeAnalysisResults(x)) == parseAnalysisResults(encodeAnalysisResults(y))
 * if analysis results input x and y are equivalent
 * @tparam T type of analysis results
 */
trait AnalysisResult[T] {

  /**
   * Encodes analysis results to a string
   * @param result Analysis result to be encoded
   * @return string encoding
   */
  def encodeAnalysisResults(result: T) : String

  /**
   * parses the output of encodeAnalysisResults function
   * the outgoing string should be exactly the same for two different input
   * strings if they are the encoding of two equivalent analysis results
   * @param input encoded analysis result string
   * @return
   */
  def parseAnalysisResults(input: String): String
}
