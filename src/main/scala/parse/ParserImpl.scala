package dev.jtrim777.cmm
package parse

import org.parboiled2._
import lang.{DataType, Primitive, Expression => exp, Statement => stmt, ProgramSegment => seg}
import collection.immutable.Seq

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
      atomic(str(value)) ~ zeroOrMore(' ')
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
    (capture(CoreID) ~> exp.ID.fromSource _) ~ !(CharPredicate.AlphaNum | '_')
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
    ((HexLiteral | BinLiteral | DecLiteral) ~> exp.CInt.parse _) ~ WSLOp
  }
  def IntConstant: ExprRule = rule {
    IntLiteral ~> exp.CInt.apply _
  }
  // TODO: Floating point support

  def Operator: Rule1[Primitive] = rule {
    (capture(oneOrMore(anyOf("!%^&*+_/><~=-"))) ~ WSLOp ~> Primitive.parseOperator _) ~>
      {opt:Option[Primitive] => test(opt.isDefined).named("valid operator") ~ push(opt.get)}
  }

  def TypeNameNS: Rule1[DataType] = rule {
    ((capture(CoreID) ~> DataType.fromName _) ~ !(CharPredicate.AlphaNum | '_')) ~>
      {opt:Option[DataType] => test(opt.isDefined).named("valid type name") ~ push(opt.get)}
  }

  def TypeName: Rule1[DataType] = rule {
    ID ~> {id:exp.ID => DataType.fromName(id.name)} ~> {opt:Option[DataType] => test(opt.isDefined).named("valid type name") ~ push(opt.get)}
  }

  def ReadExpr: ExprRule = rule {
    TypeNameNS ~ (LC ~ IntLiteral ~ RC).? ~ LB ~ Expr ~ RB ~> {(t:DataType, a:Option[Long], p:exp) => exp.Read(t, p, a)}
  }

  def PrimApp: ExprRule = rule {
    IDNoRet ~> {(f:exp.ID) => Primitive.parse(f.name)} ~ LP ~ Expr.+ ~ RP ~>
      {(f:Option[Primitive], args: Seq[exp]) => test(f.isDefined).named("valid primitive") ~ push(exp.Operation(f.get, args))}
  }

  def Operation: ExprRule = rule {
    EnclExpr ~ Operator ~ Expr ~> {(lhs:exp, op:Primitive, rhs:exp) => exp.Operation(op, Seq(lhs, rhs))}
  }

  def Expr: ExprRule = rule {
    ReadExpr | PrimApp | Operation | ID | IntConstant | ParenExpr
  }

  def ParenExpr: ExprRule = rule {
    LP ~ Expr ~ RP
  }

  def EnclExpr: ExprRule = rule {
    ID | IntConstant | ParenExpr
  }

  def End: Rule0 = rule {
    ";" ~ WSLOp
  }

  def Skip: StmtRule = rule {
    atomic("skip") ~ WSOp ~ End ~ push(stmt.Skip)
  }

  def VarDecl: StmtRule = rule {
    TypeName ~ ID.+(",".sym) ~ End ~> {(typ:DataType, names:Seq[exp.ID]) => stmt.VarDecl(typ, names.map(_.name))}
  }

  def Assn: StmtRule = rule {
    ID ~ "=".sym ~ Expr ~ End ~> {(varn:exp.ID, value:exp) => stmt.Assn(varn.name, value)}
  }

  def Write: StmtRule = rule {
    TypeName ~ LB ~ Expr ~ RB ~ "=".sym ~ Expr ~ End ~> {(typ:DataType, pos:exp, value:exp) => stmt.Write(typ, pos, value)}
  }

  def WriteAlign: StmtRule = rule {
    (TypeName ~ LC ~ IntLiteral ~ RC ~ LB ~ Expr ~ RB ~ "=".sym ~ Expr ~ End ) ~>
      { (typ: DataType, align: Long, pos: exp, value: exp) => stmt.Write(typ, pos, value, Some(align.toInt)) }
  }

  def IfStmt: StmtRule = rule {
    "if".sym ~ LP ~ Expr ~ RP ~ Block ~ ("else".sym ~ Block).? ~>
      {(cond:exp, exec: stmt.Block, eexec: Option[stmt.Block]) => stmt.IfStmt(cond, exec, eexec)}
  }

  def LocalLabel: StmtRule = rule {
    ID ~ ":".wsl ~> {(id: exp.ID) => stmt.LocalLabel(id.name)}
  }

  def Goto: StmtRule = rule {
    "goto".keyword ~ ID ~ End ~> {(id: exp.ID) => stmt.Goto(id.name)}
  }

  def Jump: StmtRule = rule {
    "jump".keyword ~ EnclExpr ~ End ~> {(proc: exp) => stmt.Jump(proc, Seq.empty)}
  }

  def JumpArgs: StmtRule = rule {
    "jump".keyword ~ EnclExpr ~ LP ~ Expr.*(",".sym) ~ RP ~ End ~> { (proc: exp, args: Seq[exp]) => stmt.Jump(proc, args) }
  }

  def Call: StmtRule = rule {
    EnclExpr ~ LP ~ Expr.*(",".sym) ~ RP ~ End ~> { (proc: exp, args: Seq[exp]) => stmt.Call(Seq.empty, proc, args) }
  }

  def CallAndSet: StmtRule = rule {
    ID.+(",".sym) ~ "=".sym ~ EnclExpr ~ LP ~ Expr.*(",".sym) ~ RP ~ End ~>
      { (rets: Seq[exp.ID], proc: exp, args: Seq[exp]) => stmt.Call(rets.map(_.name), proc, args) }
  }

  def Return: StmtRule = rule {
    "return".sym ~ End ~ push(stmt.Return(Seq.empty))
  }

  def ReturnValues: StmtRule = rule {
    "return".sym ~ Expr.*(",".sym) ~ End ~> {(vals: Seq[exp]) => stmt.Return(vals)}
  }

  def Stmt: StmtRule = rule {
    Block | Skip | Goto | JumpArgs | Jump | ReturnValues | Return | IfStmt | Assn | CallAndSet | Call |
      WriteAlign | Write | VarDecl | LocalLabel
  }

  def Block: Rule1[stmt.Block] = rule {
    LC ~ Stmt.* ~ RC ~> {(stms: Seq[stmt]) => stmt.Block(stms)}
  }

  def Parameter: Rule1[(String, DataType)] = rule {
    TypeName ~ ID ~> {(dt:DataType, id: exp.ID) => (id.name, dt)}
  }

  def ProcDefn: Rule1[seg.ProcDefn] = rule {
    ID ~ LP ~ Parameter.*(",".sym) ~ RP ~ Block ~>
      {(name:exp.ID, params:Seq[(String,DataType)], exec:stmt.Block) => seg.ProcDefn(name.name, params, exec)}
  }

  def DDCount: Rule1[Option[Int]] = rule {
    optional(LB ~ (IntLiteral ~> {i:Long => i.toInt}) ~ RB)
  }

  def DDInit: Rule1[Option[Seq[exp]]] = rule {
    (LC ~ Expr.*(",".sym) ~ RC).?
  }

  def DataDecl: Rule1[lang.DataDecl] = rule {
    ID ~ ":".sym ~ TypeName ~ DDCount ~ DDInit ~ End ~>
      {(name:exp.ID, typ:DataType, count:Option[Int], init:Option[Seq[exp]]) =>
        lang.DataDecl(name.name, typ, count.getOrElse(1), init.getOrElse(Seq.empty))}
  }

  def DataBlock: Rule1[seg.DataBlock] = rule {
    "data".sym ~ LC ~ DataDecl.* ~ RC ~> {(decls:Seq[lang.DataDecl]) => seg.DataBlock(decls)}
  }

  def SymImp: Rule1[seg.SymbolImport] = rule {
    "import".keyword ~ ID ~ End ~> {(key:exp.ID) => seg.SymbolImport(key.name)}
  }

  def SymExp: Rule1[seg.SymbolExport] = rule {
    "export".keyword ~ ID ~ End ~> { (key: exp.ID) => seg.SymbolExport(key.name) }
  }

  def Segment: Rule1[seg] = rule {
    SymImp | SymExp | DataBlock | ProcDefn
  }

  def Prog: Rule1[lang.Program] = rule {
    Segment.+ ~ EOI ~> {(segs: Seq[seg]) => lang.Program.fromMix(segs)}
  }
}

object ParserImpl {
  type ExprRule = Rule1[exp]
  type StmtRule = Rule1[stmt]
}
