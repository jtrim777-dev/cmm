package dev.jtrim777.cmm
package isa

object Label {
  def proc(name: String): String = s"cmm$$proc$$$name"
  def procBody(name: String): String = s"cmm$$proc_body$$$name"
  def elseCase(id: String): String = s"cmm$$mark_else$$$id"
  def endIf(id: String): String = s"cmm$$mark_endif$$$id"
  def localMark(blockId: String, name: String): String = s"cmm$$block$blockId$$$name"
}
