package dev.jtrim777.cmm
package lang

import scala.reflect.ClassTag
import DataType._

case class Primitive(name: String, arity: Int, typing: List[DataType] => Either[String, DataType]) {
  def toCode: String = name
}

object Primitive {
  def biCombinator[D <: DataType : ClassTag](name: String): Primitive = Primitive(name, 2, {
    case (a:D) :: (b:D) :: Nil if a == b => Right(a)
    case (a:D) :: (b:D) :: Nil if a != b => Left(s"Inputs to $name must be of the same size")
    case _ =>
      val tnm = implicitly[ClassTag[D]].runtimeClass.getSimpleName.stripSuffix("$").toLowerCase
      Left(s"Primitive '$name' only accepts $tnm-type arguments")
  })
  def unaryOp[D <: DataType : ClassTag](name: String): Primitive = Primitive(name, 1, {
    case (a:D) :: Nil => Right(a)
    case _ =>
      val tnm = implicitly[ClassTag[D]].runtimeClass.getSimpleName.stripSuffix("$").toLowerCase
      Left(s"Primitive '$name' only accepts a $tnm-type argument")
  })
  def comparator[D <: DataType : ClassTag](name: String): Primitive = Primitive(name, 2, {
    case (a:D) :: (b:D) :: Nil if a == b => Right(DataType.fromName("word" + a.bytes))
    case (a:D) :: (b:D) :: Nil if a != b => Left(s"Inputs to $name must be of the same size")
    case _ =>
      val tnm = implicitly[ClassTag[D]].runtimeClass.getSimpleName.stripSuffix("$").toLowerCase
      Left(s"Comparator '$name' only accepts $tnm-type arguments")
  })

  val Add: Primitive = biCombinator[Word]("add")
  val Sub: Primitive = biCombinator[Word]("sub")
  val Mul: Primitive = biCombinator[Word]("mul")
  val Div: Primitive = biCombinator[Word]("div")
  val Mod: Primitive = biCombinator[Word]("mod")

  val UAdd: Primitive = biCombinator[Word]("uadd")
  val USub: Primitive = biCombinator[Word]("usub")
  val UMul: Primitive = biCombinator[Word]("umul")
  val UDiv: Primitive = biCombinator[Word]("udiv")
  val UMod: Primitive = biCombinator[Word]("umod")

  val FAdd: Primitive = biCombinator[Flot]("fadd")
  val FSub: Primitive = biCombinator[Flot]("fsub")
  val FMul: Primitive = biCombinator[Flot]("fmul")
  val FDiv: Primitive = biCombinator[Flot]("fdiv")

  val And: Primitive = biCombinator[Word]("and")
  val Or: Primitive = biCombinator[Word]("or")
  val Xor: Primitive = biCombinator[Word]("xor")
  val Not: Primitive = unaryOp[Word]("not")

  def shift(name: String): Primitive = Primitive(name, 2, {
    case (a:Word) :: (_:Word) :: Nil => Right(a)
    case _ => Left(s"Primitive '$name' only accepts word-type arguments")
  })
  val ShiftL: Primitive = shift("shiftl")
  val ShiftR: Primitive = shift("shiftr")
  val ShiftRA: Primitive = shift("shiftra")

  val Abs: Primitive = unaryOp[Word]("abs")
  val Neg: Primitive = unaryOp[Word]("neg")
  val Sign: Primitive = unaryOp[Word]("sign")

  val FAbs: Primitive = unaryOp[Flot]("fabs")
  val FNeg: Primitive = unaryOp[Flot]("fneg")
  val FSign: Primitive = unaryOp[Flot]("fsign")

  val Round: Primitive = Primitive("round", 2, {
    case (a:Flot) :: (_:Word) :: Nil => Right(a)
    case _ => Left("Primitive 'round' requires a float and word argument")
  })
  val Trunc: Primitive = Primitive("trunc", 2, {
    case (a:Flot) :: (_:Word) :: Nil => Right(a)
    case _ => Left("Primitive 'trunc' requires a float and word argument")
  })

  def resize[S <: DataType : ClassTag](name: String, target: _ <: S): Primitive = Primitive(name, 1, {
    case (_:S) :: Nil => Right(target)
    case _ =>
      val tnm = implicitly[ClassTag[S]].runtimeClass.getSimpleName.stripSuffix("$").toLowerCase
      Left(s"Primitive '$name' only accepts a $tnm-type argument")
  })
  val ToWord1: Primitive = resize[Word]("word1", Word1)
  val ToWord2: Primitive = resize[Word]("word2", Word2)
  val ToWord4: Primitive = resize[Word]("word4", Word4)
  val ToWord8: Primitive = resize[Word]("word8", Word8)

  val ToFloat4: Primitive = resize[Flot]("float4", Flot4)
  val ToFloat8: Primitive = resize[Flot]("float8", Flot8)

  val CastWord: Primitive = Primitive("asWord", 1, {
    case (_:Flot) :: Nil => Right(Word8)
    case _ => Left("Primitive 'asWord' only accepts a float-type argument")
  })
  val CastFlot: Primitive = Primitive("asFloat", 1, {
    case (_:Word) :: Nil => Right(Flot8)
    case _ => Left("Primitive 'asFloat' only accepts a word-type argument")
  })

  val Eq: Primitive = comparator[Word]("eq")
  val NEq: Primitive = comparator[Word]("ne")
  val LT: Primitive = comparator[Word]("lt")
  val LTE: Primitive = comparator[Word]("lte")
  val GT: Primitive = comparator[Word]("gt")
  val GTE: Primitive = comparator[Word]("gte")

  val FEq: Primitive = comparator[Flot]("feq")
  val FNEq: Primitive = comparator[Flot]("fne")
  val FLT: Primitive = comparator[Flot]("flt")
  val FLTE: Primitive = comparator[Flot]("flte")
  val FGT: Primitive = comparator[Flot]("fgt")
  val FGTE: Primitive = comparator[Flot]("fgte")

  val All: List[Primitive] = List(
    Add, Sub, Mul, Div, Mod,
    UAdd, USub, UMul, UDiv, UMod,
    FAdd, FSub, FMul, FDiv,
    And, Or, Xor, Not,
    ShiftL, ShiftR, ShiftRA,
    Abs, Neg, Sign,
    FAbs, FNeg, FSign,
    Round, Trunc,
    ToWord1, ToWord2, ToWord4, ToWord8,
    ToFloat4, ToFloat8,
    CastWord, CastFlot,
    Eq, NEq, LT, LTE, GT, GTE,
    FEq, FNEq, FLT, FLTE, FGT, FGTE
  )

  def parse(raw: String): Primitive = {
    All.find(_.name == raw)
      .getOrElse(throw new Exception(s"No such primitive $raw"))
  }

  def parseOperator(raw: String): Primitive = raw match {
    case "+" => Add
    case "-" => Sub
    case "*" => Mul
    case "/" => Div
    case "%" => Mod
    case "~+" => FAdd
    case "~-" => FSub
    case "~*" => FMul
    case "~/" => FDiv
    case "&" => And
    case "|" => Or
    case "^" => Xor
    case "<<" => ShiftL
    case ">>" => ShiftR
    case ">>>" => ShiftRA
    case "==" => Eq
    case "!=" => NEq
    case "<" => LT
    case "<=" => LTE
    case ">" => GT
    case ">=" => GTE
    case "~=" => FEq
    case "!~" => FNEq
    case "~<" => FLT
    case "~<=" => FLTE
    case "~>" => FGT
    case "~>=" => FGTE
    case _ => throw new Exception(s"No such operator '$raw'")
  }
}
