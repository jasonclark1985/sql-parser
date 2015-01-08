import org.parboiled2.ParseError

object ParseSql {
  val statement = """SELECT foo f, bar b, fizz a
      |FROM tableA a, tableB b,tableC c
      |WHERE a.x = b.y""".stripMargin

  def main(args: Array[String]) {
    val parseResult = new SqlParser(statement).Statement.run()
    println(parseResult)
    parseResult.map { r =>
      println(r)
    }.recover { case e: ParseError => println(e.formatTraces)}
  }
}

