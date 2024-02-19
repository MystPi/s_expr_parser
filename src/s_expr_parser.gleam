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
import nibble/lexer.{type Lexer, type Span, Span}
import nibble.{type Parser, do, return, fail}

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
  SExpr(ident: String, body: List(Expr), location: Span)
  Str(String, location: Span)
  Num(Float, location: Span)
  Ident(String, location: Span)
  Symbol(String, location: Span)
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
  use token <- nibble.do(nibble.any())
  use location <- nibble.do(nibble.span())

  case token {
    StrT(s) -> return(Str(s, location))
    NumT(n) -> return(Num(n, location))
    IdentT(i) -> return(Ident(i, location))
    SymbolT(s) -> return(Symbol(s, location))
    _ -> nibble.fail("expected a literal value")
  }
}

fn s_expr_parser() -> Parser(Expr, Token, Nil) {
  use _ <- do(nibble.token(LParenT))
  use start <- do(nibble.span())
  use ident <- do(ident_parser())
  use body <- do(nibble.many(expr_parser()))
  use _ <- do(nibble.token(RParenT))
  use end <- do(nibble.span())

  let location = combine_spans(start, end)

  return(SExpr(ident, body, location))
}

fn ident_parser() -> Parser(String, Token, Nil) {
  use token <- nibble.do(nibble.any())

  case token {
    IdentT(i) -> return(i)
    _ -> fail("expected an identifier")
  }
}

// --- UTILS -------------------------------------------------------------------

pub fn lex_and_parse(input: String) -> Result(File, Error) {
  input
  |> lex
  |> result.then(parse)
}

fn combine_spans(from: Span, to: Span) -> Span {
  Span(from.row_start, from.col_start, to.row_end, to.col_end)
}
