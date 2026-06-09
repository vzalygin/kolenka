use colored::Colorize;
use itertools::Itertools;

use crate::hir::{
    DeclMap, DefMap, ExprCall, ExprGoto, ExprGotoIf, dataflow::{DataFlowNode, Signature}, structs::{Expr, ExprInstr, Hir, HirFunction, InstrKind, Var, VarKind}
};

impl std::fmt::Display for DeclMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.0
                .iter()
                .map(|(k, v)| format!("{}: {}", k, v))
                .join(", ")
        )
    }
}

impl<'a> std::fmt::Display for DefMap<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.0
                .iter()
                .map(|(k, v)| format!("{}", k))
                .join(", ")
        )
    }
}

impl std::fmt::Display for Hir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (_, function) in self.iter().sorted_by_key(|(id, _)| **id) {
            write!(f, "{}", function)?;
        }

        Ok(())
    }
}

impl std::fmt::Display for HirFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args = self
            .dataflow
            .signature
            .args
            .iter()
            .map(|var| format!("{}", var))
            .join(", ");
        let returns = self
            .dataflow
            .signature
            .rets
            .iter()
            .map(|var| format!("{}", var))
            .join(", ");
        let name = if let Some(name) = &self.name {
            name
        } else {
            &"anon".to_string()
        };
        writeln!(f, "{} {}[{}]({}):", "fn".truecolor(255, 170, 0), format!("{}", *self.id).green(), name.as_str().green(), args)?;

        for block in self.iter() {
            write!(f, "{}{}", format!("{}", block.id).truecolor(250, 250, 250), ":".truecolor(250, 250, 250))?;
            for expr in block.iter() {
                write!(f, "\t")?;
                expr.fmt_inner(f, &returns)?;
                write!(f, "\n")?;
            }
        }

        Ok(())
    }
}

impl std::fmt::Display for ExprInstr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ret = self.produces;
        let (lhs, rhs) = self.consumes;
        match self.kind {
            InstrKind::ConstInt(i) => {
                write!(f, "{} {} {}", format!("{}", ret).truecolor(240, 240, 240), "=", format!("{}", i).truecolor(150, 255, 250))
            }
            InstrKind::ConstBool(b) => {
                write!(f, "{} {} {}", format!("{}", ret).truecolor(240, 240, 240), "=", format!("{}", b).truecolor(210, 255, 250))
            }
            InstrKind::Add => {
                write!(f, "{} {} {} + {}", format!("{}", ret).truecolor(240, 240, 240), "=", lhs, rhs)
            }
            InstrKind::Sub => {
                write!(f, "{} {} {} - {}", format!("{}", ret).truecolor(240, 240, 240), "=", lhs, rhs)
            }
            InstrKind::Mul => {
                write!(f, "{} {} {} * {}", format!("{}", ret).truecolor(240, 240, 240), "=", lhs, rhs)
            }
            InstrKind::Div => {
                write!(f, "{} {} {} / {}", format!("{}", ret).truecolor(240, 240, 240), "=", lhs, rhs)
            }
            InstrKind::Less => {
                write!(f, "{} {} {} < {}", format!("{}", ret).truecolor(240, 240, 240), "=", lhs, rhs)
            }
            InstrKind::LessOrEq => {
                write!(f, "{} {} {} <= {}", format!("{}", ret).truecolor(240, 240, 240), "=", lhs, rhs)
            }
            InstrKind::Great => {
                write!(f, "{} {} {} > {}", format!("{}", ret).truecolor(240, 240, 240), "=", lhs, rhs)
            }
            InstrKind::GreatOrEq => {
                write!(f, "{} {} {} >= {}", format!("{}", ret).truecolor(240, 240, 240), "=", lhs, rhs)
            }
            InstrKind::Phi => {
                write!(f, "{} {} {}({}, {})", format!("{}", ret).truecolor(240, 240, 240), "=", "phi".green(), format!("{}", lhs).truecolor(240, 240, 240), format!("{}", rhs).truecolor(240, 240, 240))
            },
        }
    }
}

impl std::fmt::Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            VarKind::Int => {
                write!(f, "{}", format!("i{}", *self.id).truecolor(240, 240, 240))
            }
            VarKind::Bool => {
                write!(f, "{}", format!("b{}", *self.id).truecolor(240, 240, 240))
            }
            VarKind::Any => {
                write!(f, "{}", format!("a{}", *self.id).truecolor(240, 240, 240))
            }
            VarKind::AnonFn(program_id) => {
                write!(f, "{}", format!("fn{}", program_id).truecolor(240, 240, 240))
            }
            VarKind::Nothing => {
                write!(f, "NOTHING{}", *self.id)
            }
        }
    }
}

impl std::fmt::Display for Signature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args = self.args.iter().map(|var| format!("{}", var)).join(", ");
        let rets = self.rets.iter().map(|var| format!("{}", var)).join(", ");

        write!(f, "fn({})->({})", args, rets)
    }
}

impl std::fmt::Display for DataFlowNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataFlowNode::Producer(var) => write!(f, "{{ producer {} }}", var),
            DataFlowNode::Triple(var_triple) => write!(
                f,
                "{{ triple {} {} -> {} }}",
                var_triple.args.0, var_triple.args.1, var_triple.ret
            ),
            DataFlowNode::Call(signature) => write!(f, "{{ signature {} }}", signature),
            DataFlowNode::CallVar(var, signature) => write!(f, "{{ call {} {} }}", var, signature),
            DataFlowNode::If(var_if) => {
                write!(f, "{{ if {} {} {} }}", var_if.condition, var_if.th.0, var_if.el.0)
            }
            DataFlowNode::Loop(var_loop) => {
                write!(f, "{{ loop {} {} }}", var_loop.condition.0, var_loop.body.0)
            },
        }
    }
}

impl Expr {
    fn fmt_inner(
        &self,
        f: &mut std::fmt::Formatter,
        returns: &String,
    ) -> std::fmt::Result {
        match self {
            Expr::Goto(ExprGoto { next }) => {
                write!(f, "{} {};", "goto".truecolor(255, 170, 0), format!("{}", next).truecolor(250, 250, 250))
            }
            Expr::GotoIf(ExprGotoIf { cond, then_block, else_block }) => {
                write!(
                    f,
                    "{} {} {} {}; {} {};",
                    "if".truecolor(255, 170, 0), cond, "then goto".truecolor(255, 170, 0), format!("{}", then_block).truecolor(250, 250, 250), "else goto".truecolor(255, 170, 0), format!("{}", *else_block).truecolor(250, 250, 250)
                )
            }
            Expr::Instr(instr) => {
                write!(f, "{};", instr)
            }
            Expr::Call(ExprCall { prog_id, args: consumes, rets: produces }) => {
                let args = consumes.iter().map(|var| format!("{}", var)).join(", ");
                let rets = produces.iter().map(|var| format!("{}", var)).join(", ");
                if rets.is_empty() {
                    write!(f, "{} {}({});", "call fn".truecolor(255, 170, 0), format!("{}", prog_id).green(), args.truecolor(240, 240, 240))
                } else {
                    write!(f, "{} {} {} {}({});", rets.truecolor(240, 240, 240), "=", "call fn".truecolor(255, 170, 0), format!("{}", prog_id).green(), args.truecolor(240, 240, 240))
                }
            }
            Expr::Return => {
                if !returns.is_empty() {
                    write!(f, "{} {};", "return".truecolor(255, 170, 0), returns)
                } else {
                    write!(f, "{};", "return".truecolor(255, 170, 0))
                }
            }
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_inner(f, &String::new())
    }
}
