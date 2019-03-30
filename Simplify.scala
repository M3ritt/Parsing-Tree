import scala.util.parsing.combinator._

abstract class Tree
case class E(m: Tree) extends Tree
case class And(left: Tree, right: Tree) extends Tree
case class Or(left: Tree, right: Tree) extends Tree
case class Not(right: Tree) extends Tree
case class C(n: Char) extends Tree
case class TrueOrFalse(b: Boolean) extends Tree
 
object Tree extends CombinatorsV1 {
  type Envr = String => String
  
def main(args: Array[String]){
    //print("expression? ")
    //var input : String = scala.io.StdIn.readLine()
  var input : String = "(a || !false) && c"
    //while(!input.equalsIgnoreCase("exit")){
      val output:Tree = parseAll(e, input).get
      val enviro: Envr = {
        case "a" => "a"
      }
      //if(simplifyTree(output, enviro) == input) {
      //  var result = input
      //  println("result: "+result) 
      //} else 
        println("result: " + simplifyTree(output, enviro))
      //print("expression? ")
      //input = scala.io.StdIn.readLine()
    //}  
    print("closing project")
  }
  
  def simplifyTree(t: Tree, envr: Envr): String = t match {   
    //character to value
    case C(c) => "" + c
    
    //boolean to value
    case TrueOrFalse(b) => "" + b
    
    case E(e) if(simplifyTree(e, envr) == "true") => "true" 
    case E(e) if(simplifyTree(e, envr) == "false") => "false"
    case E(e) => "(" + simplifyTree(e, envr) + ")"

    case And(left, right) if(simplifyTree(left, envr) == "false") => "false"
    case And(left, right) if(simplifyTree(right, envr) == "false") => "false"
    case And(left, right) if(simplifyTree(left,envr) == "true") => simplifyTree(right, envr)
    case And(left, right) if(simplifyTree (right, envr) == "true") => simplifyTree(left, envr)
    case And(left, right) => simplifyTree(left, envr) + "&&" + simplifyTree(right, envr)
    
    case Or(left, right) if(simplifyTree(left, envr) == "true") => "true"
    case Or(left, right) if(simplifyTree(right, envr) == "true") => "true"
    case Or(left, right) if(simplifyTree(left, envr) == "false")=> simplifyTree(right, envr)
    case Or(left, right) if(simplifyTree(right, envr) == "false") => simplifyTree(left, envr)
    case Or(left, right) => simplifyTree(left, envr) + "||" + simplifyTree(right, envr)
    //Need Not Cases still
    case Not(right) if(simplifyTree(right, envr) == "true") => "false"
    case Not(right) if(simplifyTree(right, envr) == "false") => "true"
    case Not(right) => "!" + (simplifyTree(right, envr))
}
} 
class CombinatorsV1 extends JavaTokenParsers {
  //Cases
  //E -> T '||' E | T
  //T -> F '&&' T | F
  //F -> '!' A | A
  //A -> '(' E ')' | C
  //C -> 'true' | 'false' | 'c'
  //c -> anyChar
  
  def e: Parser[Tree] = t ~ or ~ e ^^ {case l ~ o ~ r => Or(l, r)} | t
  def t: Parser[Tree] = f ~ and ~ t ^^ {case l ~ a ~ r => And(l, r)} | f
  def f: Parser[Tree] = not ~ a ^^ {case n ~ r => Not(r)} | a
  def a: Parser[Tree] = parab ~ e ~ parae ^^ {case l ~ m ~ r => E(m)} | c
  def c: Parser[Tree] = bool | chr
  def chr: Parser[C] = "[A-Za-z]".r ^^ {str => C(str.charAt(0))}
  def bool: Parser[TrueOrFalse] = "true".r ^^ {str => TrueOrFalse(true)} | "false".r ^^ {str => TrueOrFalse(false)}
  def parab[Tree] = "("
  def parae[Tree] = ")"
  def and[Tree] = "&&"
  def not[Tree] = "!" 
  def or[Tree] = "||"
}