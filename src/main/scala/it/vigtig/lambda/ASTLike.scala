package it.vigtig.lambda

/**
 * @author Hargreaves
 */
trait ASTLike extends AST{

  abstract trait Term

  case object Empty extends Term

  case class Abstr(id: Term, body: Term) extends Term
  case class Applic(left: Term, right: Term) extends Term
  case class Named(id: Id, body: Term) extends Term

  abstract trait Atom extends Term //"Normal Form", term cannot be reduced further
  case class Id(id: String) extends Atom
  case class Integer(i: Int) extends Atom
  case class Floating(f: Float) extends Atom
  case class Bit(b: Boolean) extends Atom

  case class SetType(id:Id,vars:List[Id],cons:List[Constructor]) extends Term
  case class Constructor(id:Id,args:List[(String,String)]) extends Term
  
  def prettyStr(t: Term): String = t match {
    case Constructor(Id(id),args) => s"$id ${args.mkString}"
    case SetType(Id(id),vars,cons) => s"set $id ${vars.mkString} = ${cons.map(prettyStr).mkString}"
    case Applic(a @ Id(_), b) => s"${prettyStr(a)} ${prettyStr(b)}"
    case Applic(a, b)         => s"(${prettyStr(a)}) (${prettyStr(b)})"
    case Abstr(Id(x), b)      => s"$x . ${prettyStr(b)}"
    case Id(x)                => x
    case Named(Id(x), term)   => s"$x = ${prettyStr(term)}"
    case Empty                => "< >"
    case Integer(i)           => i.toString
    case Floating(f)          => f.toString
    case Bit(b)               => b.toString
  }

}