//! Модуль с алгоритмами выводов типов для Ast

use thiserror::Error;

use crate::{
    parser::{Ast, AstNode, Builtin},
    pass::typing::{StackCfg, Term, Type},
};

#[derive(Error, Debug)]
enum TypeError {
    #[error("Incompatible types {0:?} and {1:?}")]
    IncompatibleTypes(Term, Term),
}

/// Представление ограничения
///
/// Вывод типов связан согласованием конфигураций стека между командами. Данный тип описывает такие требования согласования.
enum Restriction {
    /// Требование унификации типов
    Unification(Term, Term),
    /// Требование согласования размеров стека
    StackExtension(StackCfg, StackCfg),
}

enum Replacement {
    Stack(StackCfg, StackCfg),
    Identity
}

impl Replacement {
    fn stack(from: StackCfg, to: StackCfg) -> Replacement {
        Replacement::Stack(from, to)
    }

    fn term(from: Term, to: Term) -> Replacement {
        Replacement::Stack(vec![from], vec![to])
    }
}

impl Type {
    fn apply_replacement(self, replacement: &Replacement) -> Type {
        match replacement {
            Replacement::Stack(from, to) => {
                Type::new(
                    stack_cfg_apply_replacement(self.inp, from, to),
                    stack_cfg_apply_replacement(self.out, from, to),
                )
            },
            Replacement::Identity => self,
        }
    }
}

fn stack_cfg_apply_replacement(old: StackCfg, from: &Vec<Term>, to: &Vec<Term>) -> StackCfg {
    let mut new: StackCfg = vec![];
    let mut i = 0;

    while i < old.len() {
        if old[i..].starts_with(from) {
            let mut to = to.clone();
            new.append(&mut to);
            i += from.len();
        } else {
            new.push(old[i].clone());
            i += 1;
        }
    }

    new
}

/// Вывод типа для всей программы
fn infer_ast(ast: &Ast) -> Result<Type, TypeError> {
    infer(&ast.program)
}

/// Вывод типа для последовательности команд
fn infer(nodes: &Vec<AstNode>) -> Result<Type, TypeError> {
    let mut prog_type = Type::trivial();

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
    let (mut lhs, mut rhs) = (lhs.clone(), rhs.clone());

    let mut restrictions = restrict(&lhs, &rhs);

    while restrictions.len() != 0 {
        for restriction in restrictions {
            let (lhs_replacement, rhs_replacement) = unification(restriction)?; 
            lhs = lhs.apply_replacement(&lhs_replacement);
            rhs = rhs.apply_replacement(&rhs_replacement);
        }

        restrictions = restrict(&lhs, &rhs);        
    }

    Ok(Type::new(lhs.inp, rhs.out))
}

/// Поиск ограничений.
fn restrict(lhs: &Type, rhs: &Type) -> Vec<Restriction> {
    let mut lhs_iter = lhs.out.iter().rev().into_iter().peekable();
    let mut rhs_iter = rhs.inp.iter().rev().into_iter().peekable();
    let mut restrictions: Vec<Restriction> = vec![];

    // TODO рекурсия
    while lhs_iter.peek().is_some() || rhs_iter.peek().is_some() {
        let lhs = lhs_iter.next().unwrap();
        let lhs_has_next = lhs_iter.peek().is_some();

        let rhs = rhs_iter.next().unwrap();
        let rhs_has_next = rhs_iter.peek().is_some();

        if lhs_has_next && rhs_has_next {
            restrictions.push(Restriction::Unification(lhs.clone(), rhs.clone()));
        } else if !rhs_has_next {
            let lhs: Vec<Term> = lhs_iter.map(|term| term.clone()).collect();
            let rhs: Vec<Term> = vec![rhs.clone()];
            restrictions.push(Restriction::StackExtension(lhs, rhs));
            break;
        } else if !lhs_has_next {
            let lhs: Vec<Term> = vec![lhs.clone()];
            let rhs: Vec<Term> = rhs_iter.map(|term| term.clone()).collect();
            restrictions.push(Restriction::StackExtension(lhs, rhs));
            break;
        }
    }

    restrictions
}

/// Поиск подстановки. Подстановка -- сведение двух конфигураций в одну согласно правилам:
///
/// 1. Если два типа, то выбирается наиболее конкретный (пример, Int и Var -> Int)
/// 2. Если конфигурации разного размера, то выбирается наиболее длинная. По сути -- сводится к п.1, если считать, что наиболее общий == наиболее длинный.
fn unification(restriction: Restriction) -> Result<(Replacement, Replacement), TypeError> {
    match restriction {
        Restriction::Unification(lhs, rhs) => {
            // Пока правила достаточно простые, reduce всегда возвращает `Ok(to)`, если сведение возможно
            let reduce_lhs = reduce(&lhs, &rhs).is_some();
            let reduce_rhs = reduce(&rhs, &lhs).is_some();
            if reduce_lhs {
                Ok((
                    Replacement::term(lhs, rhs),
                    Replacement::Identity
                ))
            } else if reduce_rhs {
                Ok((
                    Replacement::Identity,
                    Replacement::term(rhs, lhs)
                ))
            } else {
                Err(TypeError::IncompatibleTypes(lhs, rhs))
            }
        },
        Restriction::StackExtension(lhs, rhs) => {
            if lhs.len() < rhs.len() {
                Ok((
                    Replacement::stack(lhs, rhs),
                    Replacement::Identity,
                ))
            } else if lhs.len() > rhs.len() {
                Ok((
                    Replacement::Identity,
                    Replacement::stack(rhs, lhs)
                ))
            } else {
                Ok((
                    Replacement::Identity,
                    Replacement::Identity
                ))
            }
        },
    }
}

/// Если переменную `from` можно свести к `to`, то функция возвращает рельтат сведения.
fn reduce<'t>(from: &'t Term, to: &'t Term) -> Option<&'t Term> {
    match (from, to) {
        // Стек можно свести только к другому стеку
        (Term::Stack(_), Term::Stack(_)) => Option::Some(to),   

        // Переменную можно свести к любому более конкретному типу
        (Term::Var(_), Term::Stack(_)) => Option::Some(to),
        (Term::Var(_), Term::Var(_)) => Option::Some(to),
        (Term::Var(_), Term::Quote { inner: _ }) => Option::Some(to),
        (Term::Var(_), Term::Int) => Option::Some(to),
        (Term::Var(_), Term::Bool) => Option::Some(to),

        // Цитату можно свести только к другой цитате
        (Term::Quote { inner: _ }, Term::Quote { inner: _ }) => Option::Some(to),

        // Число можно свести к числу и булю
        (Term::Int, Term::Int) => Option::Some(to),
        (Term::Int, Term::Bool) => Option::Some(to),

        // Буль можно свести к булю и числу
        (Term::Bool, Term::Int) => Option::Some(to),
        (Term::Bool, Term::Bool) => Option::Some(to),

        // В остальных случаях свести нельзя
        _ => Option::None,
    }
}
