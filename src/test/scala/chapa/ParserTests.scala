package chapa

import org.scalatest.FunSuite

class ParserTests extends FunSuite {
  //todo: add actual tests
  test("test parsing") {
//    run("agg by")
    run("agg ent")
//    run("ent")
  }

  private def run(str: String): Unit = {
    val res = ParsePipeline.parse(TestGrammar, str, true)
    println("\"" + str + "\"")
    println(res.map(deepPrint).mkString("\n"))
    println()
  }
}

