package it.vigtig.lambda

sealed trait Token

// literals
case class INTEGER(str: String) extends Token
case class FLOAT(str: String)   extends Token

// key words
case object TRUE  extends Token
case object FALSE extends Token
case object `=`   extends Token
case object `+`   extends Token
case object `-`   extends Token
case object `/`   extends Token
case object `*`   extends Token
case object `.`   extends Token

// separators
case object COMMA   extends Token
case object SPACE   extends Token
case object NEWLINE extends Token
case object `(` extends Token
case object `)` extends Token

// others
case class IDENTIFIER(str: String) extends Token
