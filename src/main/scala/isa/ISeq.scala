package dev.jtrim777.cmm
package isa

import collection.immutable.Seq

case class ISeq[Arch <: ISA](instrs: Seq[Instruction[Arch]], labels: Map[String, Int]) {
  def +(i: Instruction[Arch]): ISeq[Arch] = this.copy(instrs = instrs :+ i)

  def ++(is: ISeq[Arch]): ISeq[Arch] = this.copy(instrs = instrs ++ is.instrs, labels = labels ++ is.labelsOffset(this.instrs.length))

  def ++(is: Seq[Instruction[Arch]]): ISeq[Arch] = this.copy(instrs = instrs ++ is)

  def +:(i: Instruction[Arch]): ISeq[Arch] = this.copy(instrs = i +: instrs, labels = labelsOffset(1))

  def addLabeled(instr: Instruction[Arch], label: String): ISeq[Arch] =
    this.copy(instrs = instrs :+ instr, labels = labels.updated(label, instrs.length))

  private def labelsOffset(i: Int): Map[String, Int] = labels.map(t => (t._1, t._2 + i))
}

object ISeq {
  def apply[Arch <: ISA](is: Arch#Instr*): ISeq[Arch] = new ISeq(is, Map.empty)
}
