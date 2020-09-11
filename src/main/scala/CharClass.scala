/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Activity 06 - CharClass
 * Student: Larsen Close
 */

object CharClass extends Enumeration {
  val EOF        = Value
  val LETTER     = Value
  val DIGIT      = Value
  val PAREN      = Value
  val OPERATOR   = Value
  val PUNCTUATOR = Value
  val QUOTE      = Value
  val BLANK      = Value
  val OTHER      = Value
}
