import $cp.`bazel-bin`.`test-sources_deploy.jar`
import dev.jtrim777.cmm.parse._
import org.parboiled2.{Rule, ParseError}
import scala.util.{Try, Success, Failure}

def testRule(input: String, ruleGet: ParserImpl => Try[_]): Unit = {
  val impl = new ParserImpl(input)

  val exec = ruleGet(impl)

  exec match {
    case Success(value) => pprint.pprintln(value)
    case Failure(ex:ParseError) => println(impl.formatError(ex))
    case Failure(o) => throw o
  }
}