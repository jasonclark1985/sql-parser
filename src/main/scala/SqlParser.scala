import org.parboiled2.CharPredicate._
import org.parboiled2.{CharPredicate, ParserInput, Parser}

object AST {
  case class Statement(select: AST.Select, from: AST.From, where: Option[AST.Where] = None)

  abstract class Column(name: String)

  case class SimpleColumn(name: String) extends Column(name)
  // TODO - Track qualifier in AliasedColumn
  case class AliasedColumn(name: String, alias: Option[String]) extends Column(name)
  case class QualifiedColumn(qualifier: String, name: String) extends Column(name)
  case class ColumnList(columns: Seq[Column]) extends Projection

  sealed trait Projection
  case object AllColumns extends Projection

  case class Select(projection: Projection)

  sealed trait Set
  case class Table(name: String, alias: Option[String] = None) extends Set
  case class TableList(tables: Seq[Set])
  case class SubSelect(statement: Statement) extends Set

  case class From(tableList: TableList)

  sealed trait Predicate
  case class ColumnEquality(left: Column, right: Column) extends Predicate
  case class InList(left: String, values: List[String]) extends Predicate
  case class Predicates(p: Seq[Predicate])

  case class Where(p: Predicates)

  case class GroupBy(columns: ColumnList)
}

trait AlphaNumeric { this: Parser =>

  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")

  def CharacterString = rule { oneOrMore(AlphaNum) }

  def Whitespace = rule { zeroOrMore(WhiteSpaceChar) }

  def comma = rule { ch(',') }

}

trait ColumnSupport extends AlphaNumeric { this: Parser =>

  def ColumnList = rule { oneOrMore(Whitespace ~ Column ~ Whitespace).separatedBy(comma) ~> AST.ColumnList }

  def Column = rule { capture(CharacterString) ~ Whitespace ~ optional(capture(CharacterString)) ~> AST.AliasedColumn }

  def SimpleColumn = rule { capture(CharacterString) ~> AST.SimpleColumn }

  def QualifiedColumn = rule { capture(CharacterString) ~ ch('.') ~ capture(CharacterString) ~> AST.QualifiedColumn }

}

// TODO - handle columns using table prefix - e.g. table1.foo
trait SelectClause extends AlphaNumeric with ColumnSupport { this: Parser =>

  def Select = rule { ignoreCase("select") ~ Whitespace ~ Projection ~ Whitespace ~> AST.Select }

  def Projection = rule { ColumnList | All }

  def All = rule { capture(ch('*')) ~> (v => AST.AllColumns) }

}

trait FromClause extends AlphaNumeric { this: Parser =>

  def From = rule { ignoreCase("from") ~ Whitespace ~ TableList ~> AST.From }

  def TableList = rule { oneOrMore(Whitespace ~ Table ~ Whitespace).separatedBy(comma) ~> AST.TableList }

  def Table = rule { capture(oneOrMore(CharacterString)) ~ Whitespace ~ optional(capture(CharacterString)) ~> AST.Table }

}

// column = column
// table1.column = table2.column
// column in ('foo', 'bar')
// table.column in ('foo', 'bar')
trait WhereClause extends AlphaNumeric with ColumnSupport { this: Parser =>

  def Where = rule { ignoreCase("where") ~ Whitespace ~ Predicates ~ Whitespace ~> AST.Where }

  def Predicates = rule { oneOrMore(ColumnEquality) ~> AST.Predicates }

  def FilterColumn = rule { QualifiedColumn | SimpleColumn }

  def ColumnEquality = rule { FilterColumn ~ Equals ~ FilterColumn ~> AST.ColumnEquality }

  def Equals = rule { Whitespace ~ ch('=') ~ Whitespace }

}

class SqlParser(val input: ParserInput) extends Parser
  with SelectClause with FromClause with WhereClause {

  def Statement = rule { Select ~ Whitespace ~ From ~ Whitespace ~ optional(Where) ~ EOI ~> AST.Statement }

}
