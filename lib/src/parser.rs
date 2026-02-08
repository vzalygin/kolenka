use nom::{
    Finish, IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::{is_alphanumeric, is_digit},
    combinator::{all_consuming, map},
    error::{ParseError, VerboseError, convert_error},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded},
};

use crate::{context::Context, error::CompilerError};

/// EBNF
///
/// <Identifier> ::= identifier ;
/// <Integer>    ::= integer ;
/// <S>          ::= { space | tab | newline } ;
/// <TypeExpr>      ::= <TypeArray>
///                  |  <TypeFunc>
///                  |  <TypeVar>
///                  |  <TypeConstant> ;
/// <TypeArray>     ::= "[" <S> { <TypeExpr> <S> } "]" ;
/// <FuncInput>     ::= { <TypeExpr> <S> } ;
/// <FuncOutput>    ::= { <TypeExpr> <S> } ;
/// <TypeFunc>      ::= "(" <S> <FuncInput> "->" <S> <FuncOutput> ")" ;
/// <TypeVar>       ::= "'" <Identifier> ;
/// <TypeConstant>  ::= <Identifier> ;
/// <Term>          ::= <Quotation>
///                  |  <Integer>
///                  |  "true"
///                  |  "false"
///                  |  <Identifier> ;
/// <Quotation>     ::= "[" <S> { <Term> <S> } "]" ;
/// <Terms>         ::= <S> { <Term> <S> } ;
/// <DefinedName>   ::= <Identifier> ;
/// <TypeSig>       ::= ":" <S> <TypeExpr> ;
/// <Extern>        ::= "extern" <S> <DefinedName> <S> <TypeSig> ;
/// <Definition>    ::= "{" { <Term> } "}" ;
/// <Define>        ::= "define" <S> <DefinedName> <S> [ <TypeSig> ] <Definition> ;
/// <Program>       ::= { <Define> | <Extern> | <Term> } ;

#[derive(Debug, Clone)]
pub(crate) struct Ast {
    pub(crate) program: Program,
}

pub(crate) type Program = Vec<AstNode>;

#[derive(Debug, Clone)]
pub(crate) enum AstNode {
    // types
    Int { value: i32 },
    Bool { value: bool },

    // prog
    BuiltinIdentifier { value: Builtin },
    Identifier { value: String },
    Quote { value: Program },
    Define { id: String, value: Program },
    // types ?
}

#[derive(Debug, Clone)]
pub(crate) enum Builtin {
    // control
    Apply,
    If,

    // math ops
    Add,
    Sub,
    Mul,
    Div,

    // stack ops
    Pop,
    Dup,
    Swap,
}

pub(crate) fn parse_source(ctx: &mut Context, input: &str) -> Result<Ast, ()> {
    match program::<VerboseError<&str>>(input).finish() {
        Ok((_, ast)) => {
            ctx.emit_debug("parsed");
            ctx.emit_debug(format!("parser result: {:#?}", ast));
            Ok(ast)
        }
        Err(error) => {
            ctx.emit_err(CompilerError::ParserError {
                description: convert_error(input, error),
            });
            Err(())
        }
    }
}

fn program<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Ast, E> {
    map(
        all_consuming(delimited(multispace0, terms, multispace0)),
        |terms| Ast { program: terms },
    )(input)
}

fn terms<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Vec<AstNode>, E> {
    separated_list0(multispace1, term)(input)
}

fn term<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AstNode, E> {
    alt((define, quotation, num, bool, builtin, identifier))(input)
}

fn quotation<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AstNode, E> {
    map(
        delimited(tag("[").and(multispace0), terms, multispace0.and(tag("]"))),
        |inner| AstNode::Quote { value: inner },
    )(input)
}

fn define<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AstNode, E> {
    map(
        preceded(
            tag("define").and(multispace1),
            pair(
                string,
                delimited(tag("{").and(multispace0), terms, multispace0.and(tag("}"))),
            ),
        ),
        |(name, definition)| AstNode::Define {
            id: name,
            value: definition,
        },
    )(input)
}

fn identifier<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AstNode, E> {
    map(string, |id: String| AstNode::Identifier { value: id })(input)
}

fn string<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, String, E> {
    map(
        take_while1(|c| is_alphanumeric(c as u8) || c == '_'),
        |s: &str| s.to_string(),
    )(input)
}

fn num<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AstNode, E> {
    map(take_while1(|c: char| is_digit(c as u8)), |number: &str| {
        let number = number.parse::<i32>().unwrap();
        AstNode::Int { value: number }
    })(input)
}

fn bool<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AstNode, E> {
    alt((
        map(tag("true"), |_| AstNode::Bool { value: true }),
        map(tag("false"), |_| AstNode::Bool { value: false }),
    ))(input)
}

fn builtin<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AstNode, E> {
    map(
        alt((
            map(tag("apply"), |_| Builtin::Apply),
            map(tag("if"), |_| Builtin::If),
            map(tag("add"), |_| Builtin::Add),
            map(tag("sub"), |_| Builtin::Sub),
            map(tag("mul"), |_| Builtin::Mul),
            map(tag("div"), |_| Builtin::Div),
            map(tag("pop"), |_| Builtin::Pop),
            map(tag("dup"), |_| Builtin::Dup),
            map(tag("swap"), |_| Builtin::Swap),
        )),
        |builtin| AstNode::BuiltinIdentifier { value: builtin },
    )(input)
}

fn multispace0<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Vec<&'a str>, E> {
    many0(alt((
        nom::character::complete::multispace1::<&'a str, E>,
        comment,
    )))(input)
}

fn multispace1<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Vec<&'a str>, E> {
    many1(alt((
        nom::character::complete::multispace1::<&'a str, E>,
        comment,
    )))(input)
}

fn comment<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    map(preceded(tag("#"), take_while(|c| c != '\n')), |comm| comm)(input)
}
