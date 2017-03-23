package it.vigtig.lambda

/**
  * @author Hargreaves
  */
trait ASTLike extends AST {

  sealed trait Term

  case object Empty extends Term

  case class Abstr(id: Term, body: Term) extends Term

  case class Applic(left: Term, right: Term) extends Term

  case class Named(id: Id, body: Term) extends Term

  sealed trait Atom extends Term

  //"Normal Form", term cannot be reduced further
  case class Id(id: String) extends Atom

  case class Integer(i: Int) extends Atom

  case class Floating(f: Float) extends Atom

  case class Bit(b: Boolean) extends Atom

  case class SetId(id: String) extends Atom

  case class SetType(id: Id, vars: List[Id], cons: List[ConstructorDef]) extends Term

  case class ConstructorDef(id: Id, args: List[(String, String)]) extends Term

  def prettyList(t: Term): List[String] = t match {
    case Applic(Applic(SetId("Cons"), head), Id(x)) => List(prettyStr(head) + "|" + x)
    case Applic(Applic(SetId("Cons"), head), tail)  => prettyStr(head) :: prettyList(tail)
    case SetId("Nil")                               => Nil
    case x                                          => List(prettyStr(x))
  }

  def prettyStr(t: Term): String = t match {
    case Applic(Applic(SetId("Cons"), head), tail) => "[" + prettyList(t).mkString(",") + "]"
    case SetId("Nil")                              => ""
    case ConstructorDef(Id(id), args)              => s"$id ${args.mkString}"
    case SetId(sid)                                => sid
    case SetType(Id(id), vars, cons)               => s"set $id ${vars.mkString} = ${cons.map(prettyStr).mkString}"
    case Applic(a @ Id(_), b)                      => s"${prettyStr(a)} ${prettyStr(b)}"
    case Applic(a, b)                              => s"(${prettyStr(a)}) (${prettyStr(b)})"
    case Abstr(Id(x), b)                           => s"$x . ${prettyStr(b)}"
    case Abstr(x, b)                               => s"${prettyStr(x)} . ${prettyStr(b)}"
    case Id(x)                                     => x
    case Named(Id(x), term)                        => s"$x = ${prettyStr(term)}"
    case Empty                                     => "< >"
    case Integer(i)                                => i.toString
    case Floating(f)                               => f.toString
    case Bit(b)                                    => b.toString
  }

}
