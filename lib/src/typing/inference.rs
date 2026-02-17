//! Модуль с алгоритмами выводов типов для Ast

use thiserror::Error;

use crate::{
    CompilerError, Context,
    parser::{Ast, AstNode, Builtin},
    typing::types::{StackCfg, Term, Type},
};

#[derive(Error, Debug)]
pub enum TypingError {
    #[error("Incompatible types {0} and {1}")]
    IncompatibleTypes(Term, Term),
}

/// Представление ограничения
///
/// Вывод типов связан согласованием конфигураций стека между командами. Данный тип описывает такие требования согласования.
#[derive(Debug)]
enum Constraint {
    /// Требование унификации типов
    Unification(Term, Term),
    /// Требование согласования размеров стека
    TailExtension(StackCfg, StackCfg),
}

#[derive(Debug)]
enum Replacement {
    Stack(StackCfg, StackCfg),
    Identity,
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
        Type::new(
            stack_cfg_apply_replacement(self.inp, replacement),
            stack_cfg_apply_replacement(self.out, replacement),
        )
    }
}

fn stack_cfg_apply_replacement(old: StackCfg, replacement: &Replacement) -> StackCfg {
    match replacement {
        Replacement::Stack(from, to) => {
            let mut new: StackCfg = vec![];
            let mut i = 0;

            while i < old.len() {
                if old[i..].starts_with(from) {
                    let mut to = to.clone();
                    new.append(&mut to);
                    i += from.len();
                } else {
                    let old = old[i].clone();
                    if let Term::Quote { inner } = old {
                        new.push(Term::quote(inner.apply_replacement(replacement)));
                    } else {
                        new.push(old);
                    }
                    i += 1;
                }
            }

            new
        }
        Replacement::Identity => old,
    }
}

/// Вывод типа для всей программы
pub fn infer_ast(ast: &Ast, ctx: &mut Context) -> Result<Type, CompilerError> {
    infer(&ast.program, ctx).map_err(CompilerError::TypingError)
}

/// Вывод типа для последовательности команд
fn infer(nodes: &Vec<AstNode>, ctx: &mut Context) -> Result<Type, TypingError> {
    let mut prog_type = Type::trivial();

    for node in nodes {
        ctx.emit_debug(format!("chaining {:?}", node));
        let node_type = get_node_type(node, &mut ctx.step())?;
        ctx.emit_debug(format!("chain node type: {}", node_type));
        prog_type = chain(&prog_type, &node_type, &mut ctx.step())?;
        ctx.emit_debug(format!("chained type {}", prog_type));
    }

    Ok(prog_type)
}

/// Типы для встроенных конструкций
///
/// ```text
/// eval    : (('S -> 'R) 'S -> 'R)
/// quote   : ('a 'S -> ('R -> 'a 'R) 'S)
/// compose : (('B -> 'C) ('A -> 'B) 'S -> ('A -> 'C) 'S)
/// dup     : ('a 'S -> 'a 'a 'S)
/// pop     : ('a 'S -> 'S)
/// swap    : ('a 'b 'S -> 'b 'a 'S)
/// cond    : ('a 'a Bool 'S -> 'a 'S)
/// while   : (('S -> Bool 'R) ('R -> 'S) 'S -> 'S)
/// ```
fn get_node_type(node: &AstNode, ctx: &mut Context) -> Result<Type, TypingError> {
    match node {
        AstNode::BuiltinIdentifier { value } => match value {
            Builtin::Eval => {
                let tail = Term::tail();
                let new_tail = Term::tail();
                let quote = Term::quote(Type::new([tail.clone()], [new_tail.clone()]));
                Ok(Type::new([tail, quote], [new_tail]))
            }
            Builtin::If => {
                let tail = Term::tail();
                let var = Term::var();
                let bool = Term::bool();
                Ok(Type::new(
                    [tail.clone(), bool, var.clone(), var.clone()],
                    [tail, var.clone()],
                ))
            }
            Builtin::Add | Builtin::Sub | Builtin::Mul | Builtin::Div => {
                let tail = Term::tail();
                let int = Term::bool();
                Ok(Type::new(
                    [tail.clone(), int.clone(), int.clone()],
                    [tail, int.clone()],
                ))
            }
            Builtin::Pop => {
                let tail = Term::tail();
                let var = Term::var();
                Ok(Type::new([tail.clone(), var.clone()], [tail.clone()]))
            }
            Builtin::Dup => {
                let tail = Term::tail();
                let var = Term::var();
                Ok(Type::new(
                    [tail.clone(), var.clone()],
                    [tail, var.clone(), var],
                ))
            }
            Builtin::Swap => {
                let tail = Term::tail();
                let lhs = Term::var();
                let rhs = Term::var();
                Ok(Type::new(
                    [tail.clone(), lhs.clone(), rhs.clone()],
                    [tail, rhs, lhs],
                ))
            }
        },
        AstNode::Define { id: _, value: _ } => {
            let tail = Term::tail();
            Ok(Type::new([tail.clone()], [tail]))
        }
        AstNode::Int { value: _ } => {
            let tail = Term::tail();
            let int = Term::int();
            Ok(Type::new([tail.clone()], [tail, int]))
        }
        AstNode::Bool { value: _ } => {
            // TODO можно упростить с Int
            let tail = Term::tail();
            let bool = Term::bool();
            Ok(Type::new([tail.clone()], [tail, bool]))
        }
        AstNode::Identifier { value } => {
            todo!("Тип идентификатора зависит от того, что под этим идентификатором определено")
        }
        AstNode::Quote { value } => {
            let tail = Term::tail();
            ctx.emit_debug(format!("infer quote {:?}", value));
            let quote = Term::quote(infer(value, &mut ctx.step())?);

            Ok(Type::new([tail.clone()], [tail, quote])) // T-QUOTE rule
        }
    }
}

/// Вывод (сцепка, chaining) общего типа для двух последовательных типов
///
/// Выходная конфигурация `lhs` должна быть сопоставлена с входной конфигурацией `rhs` (T-COMPOSE rule).
/// Сопоставление конфигураций -- попарное сопоставление переменных на верхушках стеков конфигураций. Сопоставление для цитат -- сопоставление их входных и выходных конфигураций.
/// В процессе сопоставления генерируются ограничения, для которых затем ищется наиболее общее решение -- унификация. Если решение не существует, то имеет место ошибка типизации.
fn chain(lhs: &Type, rhs: &Type, ctx: &mut Context) -> Result<Type, TypingError> {
    let (mut lhs, mut rhs) = (lhs.clone(), rhs.clone());

    let mut restrictions = constrain_chain(&lhs, &rhs, &mut ctx.step());

    while !restrictions.is_empty() {
        ctx.emit_debug(format!("types lhs {} rhs {}", lhs, rhs));
        ctx.emit_debug(format!("restrictions {:?}", restrictions));
        for restriction in restrictions {
            let replacement = chain_solve(restriction, &mut ctx.step())?;
            ctx.emit_debug(format!("replacement {:?}", replacement));
            lhs = lhs.apply_replacement(&replacement);
            rhs = rhs.apply_replacement(&replacement);
        }
        ctx.emit_debug(format!("applied types lhs {} rhs {}", lhs, rhs));
        restrictions = constrain_chain(&lhs, &rhs, &mut ctx.step());
    }

    Ok(Type::new(lhs.inp, rhs.out))
}

/// Поиск ограничений сцепки
///
/// Сцепка -- выход первого типа должен совпадать с входом второй типа.
fn constrain_chain(lhs: &Type, rhs: &Type, ctx: &mut Context) -> Vec<Constraint> {
    constrain(&lhs.out, &rhs.inp, ctx)
}

/// Поиск ограничений эквивалентности
///
/// Эквивалетность типов -- вход и выход первого типа совпадают с входом и выходом второго типа.
fn constrain_equivalence(lhs: &Type, rhs: &Type, ctx: &mut Context) -> Vec<Constraint> {
    let mut constraints: Vec<Constraint> = vec![];

    constraints.append(&mut constrain(&lhs.inp, &rhs.inp, ctx));
    constraints.append(&mut constrain(&lhs.out, &rhs.out, ctx));

    constraints
}

/// Поиск ограничений для двух стековых конфигураций
fn constrain(lhs: &StackCfg, rhs: &StackCfg, ctx: &mut Context) -> Vec<Constraint> {
    let mut lhs_iter = lhs.iter().rev().peekable();
    let mut rhs_iter = rhs.iter().rev().peekable();
    let mut constraints: Vec<Constraint> = vec![];

    while lhs_iter.peek().is_some() || rhs_iter.peek().is_some() {
        let lhs = lhs_iter.next().unwrap();
        let lhs_has_next = lhs_iter.peek().is_some();
        let rhs = rhs_iter.next().unwrap();
        let rhs_has_next = rhs_iter.peek().is_some();

        if lhs_has_next == rhs_has_next {
            if lhs != rhs {
                if let Term::Quote { inner: lhs } = lhs
                    && let Term::Quote { inner: rhs } = rhs
                {
                    constraints.append(&mut constrain_equivalence(lhs, rhs, &mut ctx.step()));
                } else {
                    constraints.push(Constraint::Unification(lhs.clone(), rhs.clone()));
                }
            }
        } else if !rhs_has_next {
            let lhs: Vec<Term> = vec![lhs.clone()]
                .into_iter()
                .chain(lhs_iter.cloned())
                .rev()
                .collect();
            let rhs: Vec<Term> = vec![rhs.clone()];
            constraints.push(Constraint::TailExtension(lhs, rhs));
            break;
        } else if !lhs_has_next {
            let lhs: Vec<Term> = vec![lhs.clone()];
            let rhs: Vec<Term> = vec![rhs.clone()]
                .into_iter()
                .chain(rhs_iter.cloned())
                .rev()
                .collect();
            constraints.push(Constraint::TailExtension(lhs, rhs));
            break;
        }
    }

    constraints
}

/// Поиск решения для ограничения. Подстановка -- сведение двух конфигураций в одну согласно правилам:
///
/// 1. Если два типа, то выбирается наиболее конкретный (пример, Int и Var -> Int)
/// 2. Если конфигурации разного размера, то выбирается наиболее длинная. По сути -- сводится к п.1, если считать, что наиболее общий == наиболее длинный.
fn chain_solve(restriction: Constraint, ctx: &mut Context) -> Result<Replacement, TypingError> {
    ctx.emit_debug(format!("unification for {:?}", restriction));
    match restriction {
        Constraint::Unification(lhs, rhs) => {
            // Пока правила достаточно простые, reduce всегда возвращает `Ok(to)`, если сведение возможно
            let r_lhs = chain_reduce(&lhs, &rhs).is_some();
            let r_rhs = chain_reduce(&rhs, &lhs).is_some();
            ctx.emit_debug(format!("reduce_lhs {} reduce_rhs {}", r_lhs, r_rhs));
            // Приоритетно менять правую часть, чтобы не возникло циклов
            // Но вообще надо бы подумать, действительно ли никак не решить цикл без этого странного необходимого порядка
            if r_rhs {
                Ok(Replacement::term(rhs, lhs))
            } else if r_lhs {
                Ok(Replacement::term(lhs, rhs))
            } else {
                Err(TypingError::IncompatibleTypes(lhs, rhs))
            }
        }
        Constraint::TailExtension(lhs, rhs) => {
            if lhs.len() < rhs.len() {
                Ok(Replacement::stack(lhs, rhs))
            } else if lhs.len() > rhs.len() {
                Ok(Replacement::stack(rhs, lhs))
            } else {
                Ok(Replacement::Identity)
            }
        }
    }
}

/// Если переменную `from` можно свести к `to` в контексте сцепки типов, то функция возвращает рельтат сведения
fn chain_reduce<'t>(from: &'t Term, to: &'t Term) -> Option<&'t Term> {
    match (from, to) {
        // Стек можно свести только к другому стеку
        (Term::Tail(_), Term::Tail(_)) => Option::Some(to),

        // Переменную можно свести к любому более конкретному типу
        (Term::Var(_), Term::Tail(_)) => Option::Some(to),
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
