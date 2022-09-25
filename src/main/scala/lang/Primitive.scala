package dev.jtrim777.cmm
package lang

import scala.reflect.ClassTag
import DataType._
import collection.immutable.Seq

case class Primitive(name: String, arity: Int, typing: List[DataType] => Either[String, DataType],
                     floating: Boolean = false, unsigned: Boolean = false) {
  def toCode: String = name

  def getOutput(inputs: Seq[DataType]): Either[String, DataType] = typing(inputs.toList)

  def markFloating: Primitive = this.copy(floating = true)
  def markUnsigned: Primitive = this.copy(unsigned = true)
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

  val UAdd: Primitive = biCombinator[Word]("uadd").markUnsigned
  val USub: Primitive = biCombinator[Word]("usub").markUnsigned
  val UMul: Primitive = biCombinator[Word]("umul").markUnsigned
  val UDiv: Primitive = biCombinator[Word]("udiv").markUnsigned
  val UMod: Primitive = biCombinator[Word]("umod").markUnsigned

  val FAdd: Primitive = biCombinator[Flot]("fadd").markFloating
  val FSub: Primitive = biCombinator[Flot]("fsub").markFloating
  val FMul: Primitive = biCombinator[Flot]("fmul").markFloating
  val FDiv: Primitive = biCombinator[Flot]("fdiv").markFloating

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
  val ShiftRA: Primitive = shift("shiftra").markUnsigned

  val Abs: Primitive = unaryOp[Word]("abs")
  val Neg: Primitive = unaryOp[Word]("neg")
  val Sign: Primitive = unaryOp[Word]("sign")

  val FAbs: Primitive = unaryOp[Flot]("fabs").markFloating
  val FNeg: Primitive = unaryOp[Flot]("fneg").markFloating
  val FSign: Primitive = unaryOp[Flot]("fsign").markFloating

  val Round: Primitive = Primitive("round", 2, {
    case (a:Flot) :: (_:Word) :: Nil => Right(a)
    case _ => Left("Primitive 'round' requires a float and word argument")
  }, floating = true)
  val Trunc: Primitive = Primitive("trunc", 2, {
    case (a:Flot) :: (_:Word) :: Nil => Right(a)
    case _ => Left("Primitive 'trunc' requires a float and word argument")
  }, floating = true)

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

  val ToFloat4: Primitive = resize[Flot]("float4", Flot4).markFloating
  val ToFloat8: Primitive = resize[Flot]("float8", Flot8).markFloating

  val CastWord: Primitive = Primitive("asWord", 1, {
    case (_:Flot) :: Nil => Right(Word8)
    case _ => Left("Primitive 'asWord' only accepts a float-type argument")
  }).markFloating
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

  val ULT: Primitive = comparator[Word]("ult").markUnsigned
  val ULTE: Primitive = comparator[Word]("ulte").markUnsigned
  val UGT: Primitive = comparator[Word]("ugt").markUnsigned
  val UGTE: Primitive = comparator[Word]("ugte").markUnsigned

  val FEq: Primitive = comparator[Flot]("feq").markFloating
  val FNEq: Primitive = comparator[Flot]("fne").markFloating
  val FLT: Primitive = comparator[Flot]("flt").markFloating
  val FLTE: Primitive = comparator[Flot]("flte").markFloating
  val FGT: Primitive = comparator[Flot]("fgt").markFloating
  val FGTE: Primitive = comparator[Flot]("fgte").markFloating

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
    ULT, ULTE, UGT, UGTE,
    FEq, FNEq, FLT, FLTE, FGT, FGTE
  )

  val Comparators: List[Primitive] = List(
    Eq, NEq, LT, LTE, GT, GTE,
    ULT, ULTE, UGT, UGTE,
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
