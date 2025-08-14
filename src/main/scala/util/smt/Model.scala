package util.SMT

import org.sosy_lab.java_smt.api.{BitvectorFormula, BooleanFormula, Formula, Model as JavaSMTModel}

import java.math.BigInteger
import scala.jdk.CollectionConverters.CollectionHasAsScala

enum ModelValue {
  case Boolean(x: Boolean)

  // It seems that java_smt doesn't tell us the bitvector size :( This could maybe be obtained from when the query was
  // made but that sounds annoying
  case Bitvector(x: BigInt)

  case Unknown
}

object ModelValue {
  def apply(f: Formula, x: Object): ModelValue = (f, x) match {
    case (f: BitvectorFormula, x: BigInteger) => ModelValue.Bitvector(x)
    case (f: BooleanFormula, x: Boolean) => ModelValue.Boolean(x)
    case _ => ModelValue.Unknown
  }
}

/** Wrapper around a model of a SAT smt query result */
case class Model(a: Map[String, ModelValue])

object Model {
  def apply(m: JavaSMTModel): Model = {
    val vs = m.asList().asScala
    m.close()
    Model(vs.map(v => (v.getName, ModelValue(v.getValueAsFormula, v.getValue))).toMap)
  }
}
