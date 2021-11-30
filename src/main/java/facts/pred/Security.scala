package facts.pred

sealed trait Security extends Ordered[Security] {
  def compare(that: Security): Int =
    if (this == that) 0
    else if (this == High) 1
    else -1

  def toTruth = if (this == Low) Bool.True else Bool.False

  def &&(that: Security): Security = if (this == High && that == High) High else Low
}

// Standardise this and bool
case object High extends Security

case object Low extends Security
