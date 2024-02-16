//// Parser grammar:
////
//// ```
//// File : Expr+
////
//// Expr : LParenT IdentT Expr* RParenT -> SExpr
////      | StrT -> Str
////      | NumT -> Num
////      | IdentT -> Ident
////      | SymbolT -> Symbol
//// ```

import gleam/int
import gleam/set
import gleam/result
import gleam/string
import gleam/function
import gleam/option.{None, Some}
import nibble/lexer.{type Lexer}
import nibble.{type Parser, do, return}

// --- TYPES -------------------------------------------------------------------

pub type Token {
  LParenT
  RParenT
  StrT(String)
  NumT(Float)
  IdentT(String)
  SymbolT(String)
}

pub type File {
  File(exprs: List(Expr))
}

pub type Expr {
  SExpr(ident: String, body: List(Expr))
  Str(String)
  Num(Float)
  Ident(String)
  Symbol(String)
}

pub type Error {
  LexError(row: Int, col: Int)
  ParseError(List(nibble.DeadEnd(Token, Nil)))
}

// --- LEXER -------------------------------------------------------------------

pub fn lex(input: String) -> Result(List(lexer.Token(Token)), Error) {
  input
  |> lexer.run(lexer())
  |> result.map_error(from_lex_error)
}

fn from_lex_error(error: lexer.Error) -> Error {
  case error {
    lexer.NoMatchFound(row, col, ..) -> LexError(row, col)
  }
}

fn lexer() -> Lexer(Token, Nil) {
  lexer.simple([
    lexer.token("(", LParenT),
    lexer.token(")", RParenT),
    lexer.string("'", StrT),
    lexer.number(int.to_float, function.identity)
      |> lexer.map(NumT),
    lexer.identifier(
      "[a-z_+\\-*/=~<>@%$]",
      "[a-zA-Z0-9_+\\-*/=~<>@%$?!]",
      set.new(),
      IdentT,
    ),
    lexer.identifier(
      ":",
      "[a-zA-Z0-9\\-_]",
      set.new(),
      function.compose(string.drop_left(_, 1), SymbolT),
    ),
    lexer.whitespace(Nil)
      |> lexer.ignore,
  ])
}

// --- PARSER ------------------------------------------------------------------

pub fn parse(tokens: List(lexer.Token(Token))) -> Result(File, Error) {
  tokens
  |> nibble.run(parser())
  |> result.map_error(ParseError)
}

fn parser() -> Parser(File, Token, Nil) {
  use exprs <- do(nibble.many1(expr_parser()))
  use _ <- do(nibble.eof())
  return(File(exprs))
}

fn expr_parser() -> Parser(Expr, Token, Nil) {
  nibble.one_of([literal_parser(), s_expr_parser()])
}

fn literal_parser() -> Parser(Expr, Token, Nil) {
  use token <- nibble.take_map("a literal value")

  case token {
    StrT(s) -> Some(Str(s))
    NumT(n) -> Some(Num(n))
    IdentT(i) -> Some(Ident(i))
    SymbolT(s) -> Some(Symbol(s))
    _ -> None
  }
}

fn s_expr_parser() -> Parser(Expr, Token, Nil) {
  use _ <- do(nibble.token(LParenT))
  use ident <- do(ident_parser())
  use body <- do(nibble.many(expr_parser()))
  use _ <- do(nibble.token(RParenT))
  return(SExpr(ident, body))
}

fn ident_parser() -> Parser(String, Token, Nil) {
  use token <- nibble.take_map("an identifier")

  case token {
    IdentT(i) -> Some(i)
    _ -> None
  }
}

// --- UTILS -------------------------------------------------------------------

pub fn lex_and_parse(input: String) -> Result(File, Error) {
  input
  |> lex
  |> result.then(parse)
}
