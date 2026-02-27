//! Модуль с алгоритмами выводов типов для [`crate::parser::Ast`].

use std::collections::HashMap;

use thiserror::Error;

use crate::{
    CompilerError, Context,
    parser::{Ast, AstNode, Builtin, Program},
    typing::types::{StackCfg, Term, Type},
};

#[derive(Error, Debug)]
pub enum TypingError {
    #[error("Incompatible types {0} and {1}")]
    IncompatibleTypes(Term, Term),
    #[error("Unknown id {0}")]
    UnknownIdentifier(String),
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
    Identity, // TODO убрать этот инвариант, тк он бесполезен
}

type DefMap = HashMap<String, Program>;

type DefTypes = HashMap<String, Type>;

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
            self.seq
                .into_iter()
                .map(|stack_cfg| Type::stack_cfg_apply_replacement(stack_cfg, replacement))
                .collect::<Vec<_>>(),
        )
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
}

/// Вывод типа для всей программы
pub fn infer_ast(ast: &Ast, ctx: &mut Context) -> Result<Type, CompilerError> {
    let def_map: DefMap = init_definitions(ast, ctx);
    let mut def_tps: DefTypes = HashMap::new();

    infer(&ast.program, &def_map, &mut def_tps, ctx).map_err(CompilerError::TypingError)
}

fn init_definitions(ast: &Ast, ctx: &mut Context) -> DefMap {
    let mut defs = HashMap::new();

    for node in &ast.program {
        if let AstNode::Define { id, value } = node {
            ctx.emit_debug(format!("infer definition {}", id));
            defs.insert(id.clone(), value.clone());
        }
    }

    defs
}

/// Вывод типа для последовательности команд
fn infer<'d>(
    nodes: &'d Vec<AstNode>,
    def_map: &'d DefMap,
    def_tps: &'d mut DefTypes,
    ctx: &mut Context,
) -> Result<Type, TypingError> {
    let mut prog_type = match nodes.first() {
        Some(first) => get_node_type(first, def_map, def_tps, &mut ctx.step()),
        None => Ok(Type::trivial()),
    }?;

    for node in nodes.iter().skip(1) {
        ctx.emit_debug(format!("chaining {:?}", node));
        let node_type = get_node_type(node, def_map, def_tps, &mut ctx.step())?;
        ctx.emit_debug(format!("chain node type: {}", node_type));
        prog_type = chain(&prog_type, &node_type, &mut ctx.step())?;
    }

    ctx.emit_debug(format!("resulted type {}", prog_type));

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
fn get_node_type<'d>(
    node: &'d AstNode,
    def_map: &'d DefMap,
    def_tps: &'d mut DefTypes,
    ctx: &mut Context,
) -> Result<Type, TypingError> {
    match node {
        AstNode::BuiltinIdentifier { value } => match value {
            Builtin::Eval => {
                let tail = Term::tail();
                let new_tail = Term::tail();
                let quote = Term::quote(Type::from_inp_out([tail.clone()], [new_tail.clone()]));
                Ok(Type::from_inp_out([tail, quote], [new_tail]))
            }
            Builtin::If => {
                let tail = Term::tail();
                let bool = Term::bool();
                let new_tail = Term::tail();
                let var = Term::var();
                Ok(Type::from_inp_out(
                    [tail, bool, var.clone(), var.clone()],
                    [new_tail],
                ))
            }
            Builtin::Add | Builtin::Sub | Builtin::Mul | Builtin::Div => {
                let tail = Term::tail();
                let a = Term::int();
                let b = Term::int();
                let c = Term::int();
                Ok(Type::from_inp_out([tail.clone(), a, b], [tail, c]))
            }
            Builtin::Less | Builtin::LessOrEq | Builtin::Great | Builtin::GreatOrEq => {
                let tail = Term::tail();
                let a = Term::int();
                let b = Term::int();
                let c = Term::bool();
                Ok(Type::from_inp_out([tail.clone(), a, b], [tail, c]))
            }
            Builtin::Pop => {
                let tail = Term::tail();
                let var = Term::var();
                Ok(Type::from_inp_out([tail.clone(), var], [tail.clone()]))
            }
            Builtin::Dup => {
                let tail = Term::tail();
                let var = Term::var();
                Ok(Type::from_inp_out(
                    [tail.clone(), var.clone()],
                    [tail, var.clone(), var],
                ))
            }
            Builtin::Swap => {
                let tail = Term::tail();
                let lhs = Term::var();
                let rhs = Term::var();
                Ok(Type::from_inp_out(
                    [tail.clone(), lhs.clone(), rhs.clone()],
                    [tail, rhs, lhs],
                ))
            }
        },
        AstNode::Define { id: _, value: _ } => Ok(Type::trivial()),
        AstNode::Int { value: _ } => {
            let tail = Term::tail();
            let int = Term::int();
            Ok(Type::from_inp_out([tail.clone()], [tail, int]))
        }
        AstNode::Bool { value: _ } => {
            // TODO можно упростить с Int
            let tail = Term::tail();
            let bool = Term::bool();
            Ok(Type::from_inp_out([tail.clone()], [tail, bool]))
        }
        AstNode::Identifier { value } => {
            let t = def_tps.get(value);

            if let Some(t) = t {
                Ok(t.clone_id())
            } else {
                let prog = def_map
                    .get(value)
                    .ok_or(TypingError::UnknownIdentifier(value.clone()))?;

                let t = infer(prog, def_map, def_tps, &mut ctx.step())?.clone_inp_out();
                let t_return = t.clone_id();
                def_tps.insert(value.clone(), t);
                Ok(t_return)
            }
        }
        AstNode::Quote { value } => {
            let tail = Term::tail();
            ctx.emit_debug(format!("infer quote {:?}", value));
            let quote = Term::quote(infer(value, def_map, def_tps, &mut ctx.step())?);

            Ok(Type::from_inp_out([tail.clone()], [tail, quote])) // T-QUOTE rule
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

    Ok(lhs.append(rhs.seq.into_iter().skip(1)))
}

/// Поиск ограничений сцепки
///
/// Сцепка -- выход первого типа должен совпадать с входом второй типа.
fn constrain_chain(lhs: &Type, rhs: &Type, ctx: &mut Context) -> Vec<Constraint> {
    let (_, lhs_out) = lhs.inp_out();
    let (rhs_inp, _) = rhs.inp_out();
    constrain(lhs_out, rhs_inp, ctx)
}

/// Поиск ограничений эквивалентности
///
/// Эквивалетность типов -- вход и выход первого типа совпадают с входом и выходом второго типа.
fn constrain_equivalence(lhs: &Type, rhs: &Type, ctx: &mut Context) -> Vec<Constraint> {
    let (lhs_inp, lhs_out) = lhs.inp_out();
    let (rhs_inp, rhs_out) = rhs.inp_out();
    let mut constraints: Vec<Constraint> = vec![];

    constraints.append(&mut constrain(lhs_inp, rhs_inp, ctx));
    constraints.append(&mut constrain(lhs_out, rhs_out, ctx));

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
        (Term::Var(_), Term::Int(_)) => Option::Some(to),
        (Term::Var(_), Term::Bool(_)) => Option::Some(to),

        // Цитату можно свести только к другой цитате
        (Term::Quote { inner: _ }, Term::Quote { inner: _ }) => Option::Some(to),

        // Число можно свести к числу и булю
        (Term::Int(_), Term::Int(_)) => Option::Some(to),
        (Term::Int(_), Term::Bool(_)) => Option::Some(to),

        // Буль можно свести к булю и числу
        (Term::Bool(_), Term::Int(_)) => Option::Some(to),
        (Term::Bool(_), Term::Bool(_)) => Option::Some(to),

        // В остальных случаях свести нельзя
        _ => Option::None,
    }
}
