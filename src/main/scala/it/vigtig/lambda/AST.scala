package it.vigtig.lambda

/**
 * @author Felix
 */
abstract trait AST {
  type Term
  type Id
  type Abstr
  type Applic
}