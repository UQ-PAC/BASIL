import analysis.*
import boogie.*
import java.io.File
import ir.*
import ir.dsl.*
import org.scalatest.funsuite.AnyFunSuite
import util._
import scala.concurrent.*
import scala.concurrent.blocking
import scala.concurrent.duration._
import java.io.{BufferedWriter, File, FileWriter}
import ExecutionContext.Implicits.global
import ir.cilvisitor.*
import scala.sys.process.*


class TimeStaticAnalysis extends AnyFunSuite {

  class ProgStats extends CILVisitor {
    var procedures = 0
    var blocks = 0
    var statements = 0
    var goto_targets = 0
    var expressions = 0

    override def toString = {
      s" Program totals: ${statements} statements ; ${blocks} blocks ; ${procedures} procedures ; ${goto_targets}"
    }

    override def vproc(e: Procedure): VisitAction[List[Procedure]] = {
      procedures += 1
      DoChildren()
    }

    override def vstmt(s: Statement) = {
      statements += 1
      DoChildren()
    }

    override def vjump(j: Jump): VisitAction[Jump] = {
      j match {
        case GoTo(targets) => goto_targets += targets.size
        case _ => ()
      }
      DoChildren()
    }


    override def vblock(e: Block): VisitAction[Block] = {
      blocks += 1
      DoChildren()
    }

    override def vexpr(e: Expr): VisitAction[Expr] = {
      expressions += 1
      DoChildren()
    }
  }

  object ProgStats {
    def get(p:Program) : ProgStats = {
      val s = ProgStats()
      visit_prog(s, p)
      s
    }
  }


  def examples() = {
    def getFiles(directoryName: String): Array[String] = {
      Option(File(directoryName).listFiles(_.isFile)) match {
        case None => throw java.io.IOException(s"failed to read file '$directoryName'")
        case Some(subdirs) => subdirs.map(_.getName)
      }
    }

    val examples = getFiles("examples/csmith").filter(c => c.endsWith(".adt")).map(_.stripSuffix(".adt"))
    Logger.setLevel(LogLevel.INFO)



    info("Config")
    def config(name: String) = ILLoadingConfig("examples/csmith/" + name + ".adt", "examples/csmith/" + name + ".relf")
    def map2nd[A,B,C](a: Iterable[(C, A)], f: A => B) = a.map((x: (C, A)) => (x._1, f(x._2)))
    val loads = examples.map(c => (c, config(c))).toList
    val ctx = map2nd(loads, IRLoading.load)
    val cleanup = map2nd(ctx, IRTransform.doCleanup)

    val complexity = map2nd(cleanup, c => (c, ProgStats.get(c.program))).toMap
    val sorted = complexity.toList.sortBy(c => c._2._2.blocks)
    Logger.warn(sorted.map(c => s"${c._1}, ${c._2._2}").mkString("\n"))
    val sortedcontexts : List[(String, IRContext)] = sorted.map(c => (c._1, c._2._1))

    def doAnalysis(ctx: IRContext) : Future[List[(String, String, Long)]] = Future {
      blocking {
        StaticAnalysis.analyse(ctx, StaticAnalysisConfig(), 0).timer.checkPoints()
      }
    }
    var stop = false
    val result : List[(String, List[(String, String, Long)])] = sortedcontexts.map(v => {
      val (testn,ctx) = v
      try {
        if stop then {
          (testn, List((s"Timeout triggered stop", "", 0 : Long)))
        } else {
          Logger.warn(s"TESTING $testn")
          val comp = complexity(testn)._2
          Logger.warn(comp)
          val r = Await.result(doAnalysis(ctx), 30000.millis)
          Logger.warn("CHECKPOINTS:")
          Logger.warn(r.map(c => s"${c._1},${c._2},${c._3}").mkString("\n"))
          (testn, r)
        }
      } catch {
        case e : scala.concurrent.TimeoutException => {
          Logger.error(e)
          stop = true
          (testn, List((s"$e", "", 0 : Long)))
        }
        case e => {
          Logger.error(e, e.getStackTrace.take(10).mkString("\n"))
          (testn, List((s"$e", "", 0 : Long)))
        }
      }
    }).toList
    // test filename ,statements, blocks, procedures, checkpoint name, checkpoint loc , time delta 
    val times = result.flatMap(x => x._2.map((checkpoint : (String,  String, Long)) =>  {
      val comp = complexity(x._1)._2
      (x._1, comp.statements,comp.blocks,comp.procedures,
      checkpoint._1,
      checkpoint._3
      )
    })
    )

    times
  }
  
  def log(path: String, text: String) = {
    val writer = BufferedWriter(FileWriter(path, false))
    writer.write(text)
    writer.flush()
    writer.close()
  }

  test("Getexamples") {

    val r = examples().sortBy(x => x._5)
    val grouped = r.groupBy(x => x._5).filter(i => !i._1.contains("Timeout") && !i._1.contains("Exception"))

    var plotfile = "set terminal 'svg'; set output 'analysisres.svg' ; set xlabel \"statement count\" ; set ylabel \"analysis time (ms)\""

    var plotcmds = List[String]()

    for ((n, vs) <- grouped) {
      val table = (vs.sortBy(_._2).map(vs => {
        val x = vs._2 // statements
        val y = vs._6 // time
        s"$x $y"
      })).mkString("\n")
      val pname = s"dat/${n}.dat"
      log(pname, table)
      val plotcmd = s"'${pname}' title \"${n}\" with lines" 
      plotcmds = plotcmd::plotcmds
    }
    val pl = s"plot ${plotcmds.mkString(", ")}"
    val gp = plotfile + "\n" + pl
    println(gp)
    log("dat/plot.gp", gp)
    Seq("gnuplot", "dat/plot.gp").!!


    val csv = r.map(c => c.toList.mkString(",")).mkString("\n")
    info(csv)

    val header = "test filename,statements,blocks,procedures,interval name,interval call loc,timedelta (ms)\n"
    val text = header + csv
    val path = "analysis-times.csv"
  }
}
