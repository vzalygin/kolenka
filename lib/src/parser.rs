use derived_deref::{Deref, DerefMut};
use nom::{
    Finish, IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::{is_alphanumeric, is_digit},
    combinator::{all_consuming, map},
    error::{ParseError, VerboseError, convert_error},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, terminated},
};

use crate::{ProgramId, context::Context, error::CompilerError, id::OpId};

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
pub struct Ast {
    pub(crate) program: Program,
}

#[derive(Debug, Clone, Deref, DerefMut)]
pub(crate) struct Program {
    pub(crate) id: ProgramId,
    #[target]
    pub(crate) terms: Vec<AstNode>,
}

#[derive(Debug, Clone)]
pub(crate) enum AstNode {
    // types
    Int { id: OpId, value: i32 },
    Bool { id: OpId, value: bool },

    // prog
    BuiltinIdentifier { id: OpId, value: Builtin },
    Identifier { id: OpId, value: String },
    Quote { id: OpId, value: Program },
    Define { id: OpId, name: String, value: Program },
    // types ?
}

#[derive(Debug, Clone)]
pub(crate) enum Builtin {
    // control
    Eval,
    If,
    While,

    // math ops
    Add,
    Sub,
    Mul,
    Div,
    Less,
    LessOrEq,
    Great,
    GreatOrEq,

    // stack ops
    Pop,
    Dup,
    Swap,
    Quote,
    Compose,
}

pub fn parse_source(input: &str, ctx: &mut Context) -> Result<Ast, CompilerError> {
    match program::<VerboseError<&str>>(input).finish() {
        Ok((_, ast)) => {
            ctx.emit_debug(format!("parsed {:?}", ast));
            Ok(ast)
        }
        Err(e) => {
            let e = CompilerError::ParserError {
                description: convert_error(input, e),
            };
            ctx.emit_err(&e);
            Err(e)
        }
    }
}

fn program<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Ast, E> {
    map(
        all_consuming(delimited(multispace0, terms, multispace0)),
        |terms| Ast { program: Program { id: ProgramId::new(), terms } },
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
        |inner| AstNode::Quote { id: OpId::new(), value: Program { id: ProgramId::new(), terms: inner } },
    )(input)
}

fn define<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AstNode, E> {
    map(
        preceded(
            tag("define").and(multispace1),
            pair(
                terminated(string, multispace0),
                delimited(tag("{").and(multispace0), terms, multispace0.and(tag("}"))),
            ),
        ),
        |(name, definition)| AstNode::Define {
            id: OpId::new(),
            name,
            value: Program { id: ProgramId::new(), terms: definition },
        },
    )(input)
}

fn identifier<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AstNode, E> {
    map(string, |id: String| AstNode::Identifier { id: OpId::new(), value: id })(input)
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
        AstNode::Int { id: OpId::new(), value: number }
    })(input)
}

fn bool<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AstNode, E> {
    alt((
        map(tag("true"), |_| AstNode::Bool { id: OpId::new(), value: true }),
        map(tag("false"), |_| AstNode::Bool { id: OpId::new(), value: false }),
    ))(input)
}

fn builtin<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AstNode, E> {
    map(
        alt((
            map(tag("eval"), |_| Builtin::Eval),
            map(tag("if"), |_| Builtin::If),
            map(tag("while"), |_| Builtin::While),
            map(tag("quote"), |_| Builtin::Quote),
            map(tag("compose"), |_| Builtin::Compose),
            map(tag("+"), |_| Builtin::Add),
            map(tag("-"), |_| Builtin::Sub),
            map(tag("*"), |_| Builtin::Mul),
            map(tag("/"), |_| Builtin::Div),
            map(tag("<"), |_| Builtin::Less),
            map(tag("<="), |_| Builtin::LessOrEq),
            map(tag(">"), |_| Builtin::Great),
            map(tag(">="), |_| Builtin::GreatOrEq),
            map(tag("pop"), |_| Builtin::Pop),
            map(tag("dup"), |_| Builtin::Dup),
            map(tag("swap"), |_| Builtin::Swap),
        )),
        |builtin| AstNode::BuiltinIdentifier { id: OpId::new(), value: builtin },
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
