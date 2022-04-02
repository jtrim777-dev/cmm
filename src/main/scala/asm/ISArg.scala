package com.github.jtrim777.cmm.asm

sealed trait ISArg {

}

object ISArg {
  sealed trait Const extends ISArg
  sealed trait Callable extends ISArg

  case class Register(index: Int) extends ISArg with Callable
  case class Constant(value: Long) extends ISArg with Const
  case class UConstant(value: Long) extends ISArg with Const
  case class LabelRef(name: String) extends ISArg with Callable
  case class Offset(base: Register, off: Const) extends ISArg
}
