package dev.jtrim777.cmm
package parse

import org.parboiled2._
import lang.{DataType, Primitive, Expression => exp, Statement => stmt}

class ParserImpl(val input: ParserInput) extends Parser {
  import ParserImpl._

  implicit class StringRuleGen(value:String) {
    def ws: Rule0 = rule {
      str(value) ~ oneOrMore(' ')
    }

    def capKey: Rule1[String] = rule {
      capture(atomic(str(value))) ~ !(CharPredicate.AlphaNum | '_') ~ WSLOp
    }

    def sym: Rule0 = rule {
      str(value) ~ zeroOrMore(' ')
    }

    def wsl: Rule0 = rule {
      str(value) ~ zeroOrMore(anyOf(" \n"))
    }

    def atom: Rule0 = rule {
      atomic(str(value))
    }

    def atomWS: Rule0 = rule {
      atomic(str(value)) ~ WSOp
    }

    def keyword: Rule0 = rule {
      atomic(str(value)) ~ WSReq
    }

    def keywordL: Rule0 = rule {
      atomic(str(value)) ~ WSLReq
    }
  }

  def WSOp: Rule0 = rule { zeroOrMore(' ') }
  def WSReq: Rule0 = rule { oneOrMore(' ') }
  def WSLOp: Rule0 = rule { zeroOrMore(anyOf(" \n")) }
  def WSLReq: Rule0 = rule { oneOrMore(anyOf(" \n")) }

  def LP: Rule0 = rule { "(".wsl }
  def RP: Rule0 = rule { ")".wsl }
  def LB: Rule0 = rule { "[".wsl }
  def RB: Rule0 = rule { "]".wsl }
  def LC: Rule0 = rule { "{".wsl }
  def RC: Rule0 = rule { "}".wsl }

  def CoreID: Rule0 = rule {
    ("_" | CharPredicate.Alpha) ~ zeroOrMore(CharPredicate.AlphaNum | ch('_') | ch('.') | ch('$'))
  }
  def CappedID: Rule1[exp.ID] = rule {
    (capture(CoreID) ~> exp.ID.fromSource) ~ !(CharPredicate.AlphaNum | '_')
  }
  def ID: Rule1[exp.ID] = rule {
    CappedID ~ WSLOp
  }
  def IDNoWS: Rule1[exp.ID] = CappedID
  def IDNoRet: Rule1[exp.ID] = rule {
    CappedID ~ WSOp
  }

  def DecLiteral: Rule1[String] = rule {
    capture(("-".? ~ CharPredicate.Digit19 ~ zeroOrMore(CharPredicate.Digit)) | '0')
  }
  def HexLiteral: Rule1[String] = rule {
    capture("-".? ~ "0x" ~ oneOrMore(CharPredicate.HexDigit))
  }
  def BinLiteral: Rule1[String] = rule {
    capture("-".? ~ "0b" ~ oneOrMore(ch('0') | ch('1')))
  }

  def IntLiteral: Rule1[Long] = rule {
    ((HexLiteral | BinLiteral | DecLiteral) ~> exp.CInt.parse) ~ WSLOp
  }
  def IntConstant: ExprRule = rule {
    IntLiteral ~> exp.CInt.apply
  }
  // TODO: Floating point support

  def Operator: Rule1[Primitive] = rule {
    capture(oneOrMore("!%^&*+_/><~")) ~ WSLOp ~> Primitive.parseOperator
  }

  def TypeName: Rule1[DataType] = rule {
    (capture(CoreID) ~> DataType.fromName) ~ !(CharPredicate.AlphaNum | '_')
  }

  def ReadExpr: ExprRule = rule {
    TypeName ~ (LC ~ IntLiteral ~ RC).? ~ LB ~ Expr ~ RB ~> {(t:DataType, a:Option[Long], p:exp) => exp.Read(t, p, a)}
  }

  def PrimApp: ExprRule = rule {
    IDNoRet ~ LP ~ Expr.* ~ RP ~> {(f:exp.ID, args: Seq[exp]) => exp.PrimOp(Primitive.parse(f.name), args)}
  }

  def Operation: ExprRule = rule {
    Expr ~ Operator ~ Expr ~> {(lhs:exp, op:Primitive, rhs:exp) => exp.PrimOp(op, Seq(lhs, rhs))}
  }

  def Expr: ExprRule = rule {
    ReadExpr | PrimApp | Operation | ID | IntConstant
  }

  def End: Rule0 = rule {
    ";" ~ WSLOp
  }

  def Skip: StmtRule = rule {
    atomic("skip") ~ WSOp ~ End ~ push(stmt.Skip)
  }

  def Stmt: StmtRule = ???
}

object ParserImpl {
  type ExprRule = Rule1[exp]
  type StmtRule = Rule1[stmt]
}
