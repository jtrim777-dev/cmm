package dev.jtrim777.cmm
package isa

package object x86_64 {
  type ArchX64 = X64.type

  trait InstrSuffix {
    def key: String
  }
  case object BlankSuffix extends InstrSuffix {
    override def key: String = ""
  }
}
