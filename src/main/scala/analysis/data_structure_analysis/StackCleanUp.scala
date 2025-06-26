package analysis.data_structure_analysis

enum StackMaintained {
  case unsure
  case maintained
  case clobbered
}

enum StackStatus(maintained: Boolean = false, size: Option[Int] = None) {
  case Unmaintained extends StackStatus
  case MaintainedNoSize extends StackStatus(true)
  case MaintainedWithSize(size: Int) extends StackStatus(true, Some(size))
  case Bot extends StackStatus(true, Some(0))

  def join(other: StackStatus): StackStatus = {
    (this, other) match
      case (Unmaintained, _) => Unmaintained
      case (_, Unmaintained) => Unmaintained
      case (MaintainedNoSize, _) => MaintainedNoSize
      case (_, MaintainedNoSize) => MaintainedNoSize
      case (MaintainedWithSize(size1), MaintainedWithSize(size2)) =>
        MaintainedWithSize(math.max(size1, size2))
      case (Bot, x) => x
      case (x, Bot) => x
  }

  def move(f: Int => Int): StackStatus = {
    this match
      case StackStatus.MaintainedWithSize(size) => MaintainedWithSize(f(size))
      case ss => ss
  }
}

object StackStatus {
  def join(a: StackStatus, b: StackStatus): StackStatus = a.join(b)
}
