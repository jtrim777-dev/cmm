package dev.jtrim777.cmm.lang

case class DataDecl(name: String, kind: DataType, count: Int, value: Seq[Expression]) {
  def toCode: String = {
    val sl = if (count > 1 || count != value.length) s"[$count]" else ""

    val el: String = if (value.isEmpty) "" else {
      value.map(_.toCode).mkString("{", ", ", "}")
    }

    s"$name: ${kind.toCode}$sl$el;"
  }

  def realCount: Int = if (count >= value.length || value.isEmpty) count else value.length
}
