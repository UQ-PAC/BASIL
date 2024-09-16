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
    // use filesize to approximate size of program and sort tests by size
    def map2nd[A,B,C](a: Iterable[(C, A)], f: A => B) = a.map((x: (C, A)) => (x._1, f(x._2)))
    val loads = examples.map(c => (c, config(c))).toList

    val loads2 = loads.map(x => {
      val (c,cfg) = x
      val chars = (File(cfg.inputFile)).length
      (c,cfg,chars)
    })
    val loads3 = loads2.sortBy(_._3).take(50).map(c => (c._1, c._2))
    Logger.info(loads2.map(c => s"${c._2.inputFile} ${c._3}").mkString("\n"))

    val sorted = loads3

    Logger.warn(sorted.map(c => s"${c._1}, ${c._2}").mkString("\n"))

    def doAnalysis(ex: String) : Future[(ProgStats, List[(String, String, Long)])] = Future {
      blocking {
        // load again and analyse
        val c = IRLoading.load(config(ex))
        val ctx = IRTransform.doCleanup(c)
        val comp = ProgStats.get(c.program)
        (comp, StaticAnalysis.analyse(ctx, StaticAnalysisConfig(), 0).timer.checkPoints())
      }
    }
    // give up after \timeout_thresh consecutive timeouts
    val timeout_thresh = 3
    var timeouts = 0
    val result : List[(String, ProgStats, List[(String, String, Long)])] = sorted.map(v => {
      val (testn,cfg) = v
      try {
        if (timeouts >= timeout_thresh) then {
          None
        } else {
          Logger.warn(s"TESTING $testn")
          val (comp,r) = Await.result(doAnalysis(testn), 240000.millis)
          Logger.warn(comp)
          Logger.warn("CHECKPOINTS:")
          Logger.warn(r.map(c => s"${c._1},${c._2},${c._3}").mkString("\n"))
          timeouts = 0
          Some((testn, comp, r))
        }
      } catch {
        case e : scala.concurrent.TimeoutException => {
          timeouts += 1
          Logger.error(e)
          None
        }
        case e => {
          Logger.error(e, e.getStackTrace.take(10).mkString("\n"))
          None
        }
      }
    }).collect {
      case Some(x) => x
    }.toList
    // test filename , statements, blocks, procedures, checkpoint name, checkpoint loc , time delta 
    val times = result.flatMap(x => x._3.map((checkpoint : (String,  String, Long)) =>  {
      val comp = x._2
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
    Logger.setLevel(LogLevel.WARN)

    val r = examples().sortBy(x => x._5)
    val grouped = r.groupBy(x => x._5).filter(i => !i._1.contains("Timeout") && !i._1.contains("Exception"))

    val outputPathPrefix = "src/test/analysisTiming"
    var plotfile = s"set terminal 'svg' enhanced background rgb 'white'; set output '${outputPathPrefix}/analysisres.svg' ; set xlabel \"statement count (log scale)\" ; set ylabel \"analysis time (ms) (log scale)\"\nset logscale x 2\nset logscale y 2"


    var plotcmds = List[String]()

    for ((n, vs) <- grouped) {
      val table = (vs.sortBy(_._2).map(vs => {
        val x = vs._2 // statements
        val y = vs._6 // time
        s"$x $y"
      })).mkString("\n")
      val pname = s"${outputPathPrefix}/${n}.dat"
      log(pname, table)
      val plotcmd = s"'${pname}' title \"${n}\" with lines" 
      plotcmds = plotcmd::plotcmds
    }
    val pl = s"plot ${plotcmds.mkString(", ")}"
    val gp = plotfile + "\n" + pl
    println(gp)
    log(outputPathPrefix + "/plot.gp", gp)
    Seq("gnuplot", outputPathPrefix + "/plot.gp").!!


    val csv = r.map(c => c.toList.mkString(",")).mkString("\n")
    info(csv)

    val header = "test filename,statements,blocks,procedures,interval name,interval call loc,timedelta (ms)\n"
    val text = header + csv
    val path = "analysis-times.csv"
  }
}
