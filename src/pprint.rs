use crate::ast::{Clause, Expr, Pattern};

fn indent(spaces: usize, indent: usize) -> String {
    " ".to_string().repeat(spaces * indent)
}

fn pprint_pat(pat: &Pattern, spaces: usize, indent_size: usize) {
    match pat {
        Pattern::Ident(id) => println!("{}Ident({})", indent(spaces, indent_size), id),
        Pattern::EmptyList => println!("{}EmptyList", indent(spaces, indent_size)),
        Pattern::Wildcard => println!("{}Wildcard", indent(spaces, indent_size)),
        Pattern::Cons(p1, p2) => {
            println!("{}Cons(", indent(spaces, indent_size));
            pprint_pat(p1, spaces, indent_size + 1);
            pprint_pat(p2, spaces, indent_size + 1);
            println!("{})", indent(spaces, indent_size));
        }
    }
}

fn pprint_clause(clause: &Clause, spaces: usize, indent_size: usize) {
    match clause {
        Clause::Clause(p, e) => {
            println!("{}Clause(", indent(spaces, indent_size));
            pprint_pat(&p, spaces, indent_size + 1);
            pprint_expr(&e, spaces, indent_size + 1);
            println!("{})", indent(spaces, indent_size));
        }
        Clause::Cont(c1, c2) => {
            println!("{}Cont(", indent(spaces, indent_size));
            pprint_clause(c1, spaces, indent_size + 1);
            pprint_clause(c2, spaces, indent_size + 1);
            println!("{})", indent(spaces, indent_size));
        }
    }
}

fn pprint_expr(expr: &Expr, spaces: usize, indent_size: usize) {
    match expr {
        Expr::Ident(id) => println!("{}Ident({})", indent(spaces, indent_size), id),
        Expr::Integer(i) => println!("{}Integer({})", indent(spaces, indent_size), i),
        Expr::Decimal(d) => println!("{}Decimal({})", indent(spaces, indent_size), d),
        Expr::Boolean(b) => println!("{}Boolean({}", indent(spaces, indent_size), b),
        Expr::EmptyList => println!("{}EmptyList", indent(spaces, indent_size)),
        Expr::UnaryOp(op, e) => {
            println!("{}UnaryOp(", indent(spaces, indent_size));
            println!("{}{:?}", indent(spaces, indent_size + 1), op);
            pprint_expr(e, spaces, indent_size + 1);
            println!("{})", indent(spaces, indent_size));
        }
        Expr::BinaryOp(op, l, r) => {
            println!("{}BinaryOp(", indent(spaces, indent_size));
            println!("{}{:?}", indent(spaces, indent_size + 1), op);
            pprint_expr(l, spaces, indent_size + 1);
            pprint_expr(r, spaces, indent_size + 1);
            println!("{})", indent(spaces, indent_size));
        }
        Expr::Cons(hd, tl) => {
            println!("{}Cons(", indent(spaces, indent_size));
            pprint_expr(hd, spaces, indent_size + 1);
            pprint_expr(tl, spaces, indent_size + 1);
            println!("{})", indent(spaces, indent_size));
        }
        Expr::IfThenElse(cond, if_expr, else_expr) => {
            println!("{}IfThenElse(", indent(spaces, indent_size));
            pprint_expr(cond, spaces, indent_size + 1);
            pprint_expr(if_expr, spaces, indent_size + 1);
            pprint_expr(else_expr, spaces, indent_size + 1);
            println!("{})", indent(spaces, indent_size));
        }
        Expr::MatchWith(e, c) => {
            println!("{}MatchWith(", indent(spaces, indent_size));
            pprint_expr(e, spaces, indent_size + 1);
            pprint_clause(&*c, spaces, indent_size + 1);
            println!("{})", indent(spaces, indent_size));
        }
        Expr::Fun(id, body) => {
            println!("{}Fun(", indent(spaces, indent_size));
            println!("{}Ident({})", indent(spaces, indent_size + 1), id);
            pprint_expr(body, spaces, indent_size + 1);
            println!("{})", indent(spaces, indent_size));
        }
        Expr::FunApp(f, e) => {
            println!("{}FunApp(", indent(spaces, indent_size));
            pprint_expr(f, spaces, indent_size + 1);
            pprint_expr(e, spaces, indent_size + 1);
            println!("{})", indent(spaces, indent_size));
        }
        Expr::LetIn(id, bound, body) => {
            println!("{}LetIn(", indent(spaces, indent_size));
            println!("{}Ident({})", indent(spaces, indent_size + 1), id);
            pprint_expr(bound, spaces, indent_size + 1);
            pprint_expr(body, spaces, indent_size + 1);
            println!("{})", indent(spaces, indent_size));
        }
        Expr::LetRecFunIn(_, _, _, _) => todo!(),
        Expr::LetRecIn(id, bound, body) => {
            println!("{}LetIn(", indent(spaces, indent_size));
            println!("{}Ident({})", indent(spaces, indent_size + 1), id);
            pprint_expr(bound, spaces, indent_size + 1);
            pprint_expr(body, spaces, indent_size + 1);
            println!("{})", indent(spaces, indent_size));
        }
        Expr::Paren(e) => {
            println!("{}Paren(", indent(spaces, indent_size));
            pprint_expr(e, spaces, indent_size + 1);
            println!("{})", indent(spaces, indent_size));
        }
    }
}

pub fn pprint(expr: &Expr, spaces: usize) {
    pprint_expr(expr, spaces, 0)
}
