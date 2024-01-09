import mill._, mill.define._, scalalib._

// https://index.scala-lang.org/ml86/mill-antlr
import scalafmt._
import $file.antlr


object basil extends RootModule with ScalaModule with antlr.AntlrModule {
  def scalaVersion = "3.3.1"

  val javaTests = ivy"com.novocode:junit-interface:0.11"
  val scalaTests = ivy"org.scalatest::scalatest:3.2.10"
  val scalactic = ivy"org.scalactic::scalactic:3.2.10"
  val antlrRuntime = ivy"org.antlr:antlr4-runtime:4.9"
  val sourceCode = ivy"com.lihaoyi::sourcecode:0.3.0"
  val mainArgs = ivy"com.lihaoyi::mainargs:0.5.1"

  def mainClass = Some("Main")


  def millSourcePath = super.millSourcePath / "src" / "main"
  def ivyDeps = Agg(scalactic, antlrRuntime, sourceCode, mainArgs)
  def sources = T.sources {Seq(PathRef(this.millSourcePath /  "scala" ))}


  override def antlrPackage: Option[String] = Some("Parsers")
  override def antlrGenerateVisitor = true
  override def antlrGrammarSources = T.sources {
    Seq(PathRef(millSourcePath / "antlr4"))
  }

  object test extends ScalaTests with TestModule.ScalaTest  {
    def ivyDeps = Agg(scalaTests, javaTests)
    def sources = T.sources {Seq(PathRef(this.millSourcePath / "scala" ))}

    //def millSourcePath = super.millSourcePath / "src" / "test"  
  }


  // antlr
  
  //object test extends JavaModuleTests {
  //  def ivyDeps = Agg(javaTests, scalaTests)
  //}


}
