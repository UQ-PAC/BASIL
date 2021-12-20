package utils

case class BoogieReturnResut (failures: List[Int], numSuccess: Int, numFailure: Int)
case object BoogieReturnResut {
  def apply(failures: List[Int], numSuccess: Int, numFailure: Int): BoogieReturnResut = {
    if (failures.size != numFailure) throw new Exception()
    new BoogieReturnResut(failures, numSuccess, numFailure)
  }
}
