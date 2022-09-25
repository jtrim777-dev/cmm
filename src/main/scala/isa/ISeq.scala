package dev.jtrim777.cmm
package isa

import collection.immutable.Seq

case class ISeq[Arch <: ISA](instrs: Seq[Instruction[Arch]], labels: Map[String, Int]) {
  def add(i: Instruction[Arch]): ISeq[Arch] = this.copy(instrs = instrs :+ i)
  def +(i: Instruction[Arch]): ISeq[Arch] = this.add(i)

  def join(is: ISeq[Arch]): ISeq[Arch] = this.copy(instrs = instrs ++ is.instrs, labels = labels ++ is.labelsOffset(this.instrs.length))
  def ++(is: ISeq[Arch]): ISeq[Arch] = this.join(is)

  def ++(is: Seq[Instruction[Arch]]): ISeq[Arch] = this.copy(instrs = instrs ++ is)

  def +:(i: Instruction[Arch]): ISeq[Arch] = this.copy(instrs = i +: instrs, labels = labelsOffset(1))

  def addLabeled(instr: Instruction[Arch], label: String): ISeq[Arch] =
    this.copy(instrs = instrs :+ instr, labels = labels.updated(label, instrs.length))

  def label(key: String): ISeq[Arch] = this.copy(labels = labels.updated(key, 0))

  def labelEnd(key: String): ISeq[Arch] = this.copy(labels = labels.updated(key, instrs.length))

  private def labelsOffset(i: Int): Map[String, Int] = labels.map(t => (t._1, t._2 + i))
}

object ISeq {
  def labeled[Arch <: ISA](label: String, is: Arch#Instr): ISeq[Arch] = ISeq.empty[Arch].addLabeled(is, label)

  def apply[Arch <: ISA](is: Arch#Instr*): ISeq[Arch] = new ISeq(is, Map.empty)

  def empty[Arch <: ISA]: ISeq[Arch] = new ISeq[Arch](Seq.empty, Map.empty)

  def flatten[Arch <: ISA](ss: Seq[ISeq[Arch]]): ISeq[Arch] = {
    val base = ISeq.empty[Arch]
    ss.foldLeft(base) { (acc, next) =>
      acc ++ next
    }
  }
}
