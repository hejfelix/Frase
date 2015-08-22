import it.vigtig.lambda.ParserLike
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import it.vigtig.lambda.AST
import it.vigtig.lambda.InterpreterLike
import org.scalatest.PropSpec
import it.vigtig.lambda.HindleyMilnerLike
/*
 * @author Hargreaves
 */
object TypeTest extends PropSpec
with InterpreterLike 
with ParserLike
with HindleyMilnerLike
with GeneratorDrivenPropertyChecks {
  
}