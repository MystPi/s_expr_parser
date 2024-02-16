import gleam/string
import gleeunit
import birdie
import s_expr_parser.{lex_and_parse}

pub fn main() {
  gleeunit.main()
}

pub fn literals_test() {
  "
  123
  42.3
  'hello\\'there!'
  foo-bar
  :foo-bar
  +
  "
  |> lex_and_parse
  |> string.inspect
  |> birdie.snap("literal values can be parsed")
}

pub fn s_expr_test() {
  "
  (* (+ a b) 20)
  "
  |> lex_and_parse
  |> string.inspect
  |> birdie.snap("s-expressions can be parsed")
}

pub fn bad_s_expr_test() {
  "
  (:blah :blah :blah)
  "
  |> lex_and_parse
  |> string.inspect
  |> birdie.snap("s-expressions must begin with an identifier")
}
