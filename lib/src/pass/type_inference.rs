//! Модуль с алгоритмами выводов типов для Ast

use thiserror::Error;

use crate::{
    parser::{Ast, AstNode, Builtin},
    pass::typing::{StackCfg, Term, Type},
};

#[derive(Error, Debug)]
enum TypeError {
    #[error("type error")]
    Error,
}

/// Вывод типа для всей программы
fn infer_ast(ast: &Ast) -> Result<Type, TypeError> {
    infer(&ast.program)
}

/// Вывод типа для последовательности команд
fn infer(nodes: &Vec<AstNode>) -> Result<Type, TypeError> {
    let mut prog_type = Type::new([], []);

    for node in nodes {
        let node_type = get_node_type(node)?;
        prog_type = chain(&prog_type, &node_type)?;
    }

    Ok(prog_type)
}

/// Типы для встроенных конструкций
///
/// ```text
/// apply   : (('S -> 'R) 'S -> 'R)
/// quote   : ('a 'S -> ('R -> 'a 'R) 'S)
/// compose : (('B -> 'C) ('A -> 'B) 'S -> ('A -> 'C) 'S)
/// dup     : ('a 'S -> 'a 'a 'S)
/// pop     : ('a 'S -> 'S)
/// swap    : ('a 'b 'S -> 'b 'a 'S)
/// cond    : (Bool 'a 'a 'S -> 'a 'S)
/// while   : (('S -> Bool 'R) ('R -> 'S) 'S -> 'S)
/// ```
fn get_node_type(node: &AstNode) -> Result<Type, TypeError> {
    match node {
        AstNode::BuiltinIdentifier { value } => match value {
            Builtin::Apply => {
                let tail = Term::stack();
                let new_tail = Term::stack();
                let quote = Term::quote(Type::new([tail.clone()], [new_tail.clone()]));
                Ok(Type::new([tail, quote], [new_tail]))
            }
            Builtin::If => {
                let tail = Term::stack();
                let var = Term::var();
                let bool = Term::bool();
                Ok(Type::new(
                    [tail.clone(), var.clone(), var.clone(), bool],
                    [tail, var.clone()],
                ))
            }
            Builtin::Add | Builtin::Sub | Builtin::Mul | Builtin::Div => {
                let tail = Term::stack();
                let int = Term::bool();
                Ok(Type::new(
                    [tail.clone(), int.clone(), int.clone()],
                    [tail, int.clone()],
                ))
            }
            Builtin::Pop => {
                let tail = Term::stack();
                let var = Term::var();
                Ok(Type::new([tail.clone(), var.clone()], [tail.clone()]))
            }
            Builtin::Dup => {
                let tail = Term::stack();
                let var = Term::var();
                Ok(Type::new(
                    [tail.clone(), var.clone()],
                    [tail, var.clone(), var],
                ))
            }
            Builtin::Swap => {
                let tail = Term::stack();
                let lhs = Term::var();
                let rhs = Term::var();
                Ok(Type::new(
                    [tail.clone(), lhs.clone(), rhs.clone()],
                    [tail, rhs, lhs],
                ))
            }
        },
        AstNode::Define { id: _, value: _ } => {
            let tail = Term::stack();
            Ok(Type::new([tail.clone()], [tail]))
        }
        AstNode::Int { value: _ } => {
            let tail = Term::stack();
            let int = Term::bool();
            Ok(Type::new([tail.clone()], [tail, int]))
        }
        AstNode::Bool { value: _ } => {
            // TODO можно упростить с Int
            let tail = Term::stack();
            let bool = Term::bool();
            Ok(Type::new([tail.clone()], [tail, bool]))
        }
        AstNode::Identifier { value } => {
            todo!("Тип идентификатора зависит от того, что под этим идентификатором определено")
        }
        AstNode::Quote { value } => {
            let tail = Term::stack();
            let quote = Term::quote(infer(value)?);

            Ok(Type::new([tail.clone()], [tail, quote])) // T-QUOTE rule
        }
    }
}

/// Вывод (сцепка, chaining) общего типа для двух последовательных типов
///
/// Выходная конфигурация `lhs` должна быть сопоставлена с входной конфигурацией `rhs` (T-COMPOSE rule).
/// Сопоставление конфигураций -- попарное сопоставление переменных на верхушках стеков конфигураций. Сопоставление для цитат -- сопоставление их входных и выходных конфигураций.
/// В процессе сопоставления генерируются ограничения, для которых затем ищется наиболее общее решение -- унификация. Если решение не существует, то имеет место ошибка типизации.
fn chain(lhs: &Type, rhs: &Type) -> Result<Type, TypeError> {
    let compose_restriction_pair = (&lhs.out, &rhs.inp);

    todo!()
}

/// Представление ограничения
///
/// Ограничение -- два кусочка конфигураций, для которых необходимо подобрать общую подстановку.
struct Restriction {
    lhs: StackCfg,
    rhs: StackCfg,
}

impl Restriction {
    fn new(lhs: StackCfg, rhs: StackCfg) -> Restriction {
        Restriction { lhs, rhs }
    }

    /// Поиск подстановки. Подстановка -- сведение двух конфигураций в один согласно правилам:
    ///
    /// 1. Если два типа, то выбирается наиболее общий (пример, Int и Var -> Int)
    /// 2. Если конфигурации разного размера, то выбирается наиболее длинная. По сути -- сводится к п.1, если считать, что наиболее общий == наиболее длинный.
    ///
    /// Если подобрать наиболее общий тип нельзя (например, пустота и Int или Quote и Bool), то возникает ошибка типизации. TODO уточнить различные ошибки
    ///
    /// После получения подстановки -- она применяется путем замены во всех ограничениях. TODO важен ли порядок?
    fn solve(&self) -> Result<StackCfg, TypeError> {
        todo!()
    }
}
