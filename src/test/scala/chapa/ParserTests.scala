package chapa

import org.scalatest.FunSuite

class ParserTests extends FunSuite {
  //todo: add actual tests
  test("test parsing") {
    run("agg by")    // returns result with empty "WHERE" - probably not good
    run("agg by by") // fails - expects results for both "by"
    run("agg entity1 entity2")
    run("agg entity1 entity2 entity3") // fails due to logic to handle recursive rules
  }

  private def run(str: String): Unit = {
    val res = ParsePipeline.parse(TestGrammar, str, true)
    println("\"" + str + "\"")
    println(res.map(deepPrint).mkString("\n"))
    println()
  }
}

