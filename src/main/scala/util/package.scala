package dev.jtrim777.cmm

package object util {
  implicit class SEOps[L, R](seq: Seq[Either[L, R]]) {
    def sequence: Either[L, Seq[R]] = {
      val bc: Either[L, Seq[R]] = Right(List.empty)
      seq.foldLeft(bc) { (acc, elem: Either[L, R]) =>
        acc match {
          case Left(value) => Left(value)
          case Right(value) => elem match {
            case Left(iv) => Left(iv)
            case Right(iv) => Right(value :+ iv)
          }
        }
      }
    }
  }
}
