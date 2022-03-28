mod ast;

use crate::lexer::{Lexer, Token};

use self::ast::{BinaryOperator, Clause, Expr, Ident, Pattern, Program};

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
}

impl<'a> Parser {
    pub fn new(input: &'a str) -> Self {
        Self {
            tokens: Lexer::new(input).collect(),
        }
    }

    fn literal(&self, cur: usize) -> Result<(Expr, usize), String> {
        match self.tokens.get(cur) {
            Some(&Token::Integer(i)) => Ok((Expr::Integer(i), cur + 1)),
            Some(&Token::Decimal(d)) => Ok((Expr::Decimal(d), cur + 1)),
            Some(&Token::Boolean(b)) => Ok((Expr::Boolean(b), cur + 1)),
            Some(&Token::EmptyList) => Ok((Expr::EmptyList, cur + 1)),
            v => Err(format!("Expected literal, got {:?}", v)),
        }
    }

    fn ident(&self, cur: usize) -> Result<(Ident, usize), String> {
        let tok = self.tokens.get(cur).unwrap_or(&Token::Unknown).clone();
        match tok {
            Token::Ident(id) => Ok((id, cur + 1)),
            v => Err(format!("Expected ident, got {:?}", v)),
        }
    }

    fn paren_expr(&self, cur: usize) -> Result<(Expr, usize), String> {
        let mut new_cur = cur;
        {
            let lparen_tok = self.tokens.get(new_cur);
            match lparen_tok {
                Some(&Token::Lparen) => (),
                _ => return Err(format!("Expected Token::Lparen, got {:?}", lparen_tok)),
            };
        }
        new_cur += 1;
        let (expr, mut new_cur) = self.expr(new_cur)?;
        {
            let rparen_tok = self.tokens.get(new_cur);
            match rparen_tok {
                Some(&Token::Rparen) => (),
                _ => return Err(format!("Expected Token::Rparen, got {:?}", rparen_tok)),
            };
        }
        new_cur += 1;
        Ok((expr, new_cur))
    }

    fn atom(&self, cur: usize) -> Result<(Expr, usize), String> {
        if let lit @ Ok(_) = self.literal(cur) {
            return lit;
        }
        if let Ok((id, new_cur)) = self.ident(cur) {
            return Ok((Expr::Ident(id), new_cur));
        }
        if let e@Ok(_) = self.paren_expr(cur) {
            return e;
        }

        Err("Error parsing atom".to_string())
    }

    fn if_expr(&self, cur: usize) -> Result<(Expr, usize), String> {
        let mut new_cur = cur;
        {
            let if_tok = self.tokens.get(new_cur);
            match if_tok {
                Some(&Token::If) => (),
                _ => return Err(format!("Expected Token::If, got {:?}", if_tok)),
            };
        }
        new_cur += 1;
        let (cond_expr, mut new_cur) = self.expr(new_cur)?;
        {
            let then_tok = self.tokens.get(new_cur);
            match then_tok {
                Some(&Token::Then) => (),
                _ => return Err(format!("Expected Token::If, got {:?}", then_tok)),
            };
        }
        new_cur += 1;
        let (then_expr, mut new_cur) = self.expr(new_cur)?;
        {
            let else_tok = self.tokens.get(new_cur);
            match else_tok {
                Some(&Token::Else) => (),
                _ => return Err(format!("Expected Token::If, got {:?}", else_tok)),
            };
        }
        new_cur += 1;
        let (else_expr, new_cur) = self.expr(new_cur)?;
        Ok((
            Expr::IfThenElse(
                Box::new(cond_expr),
                Box::new(then_expr),
                Box::new(else_expr),
            ),
            new_cur,
        ))
    }

    fn match_expr(&self, cur: usize) -> Result<(Expr, usize), String> {
        let mut new_cur = cur;
        {
            let match_tok = self.tokens.get(new_cur);
            match match_tok {
                Some(&Token::Match) => (),
                _ => return Err(format!("Expected Token::Match, got {:?}", match_tok)),
            };
        }
        new_cur += 1;
        let (expr, mut new_cur) = self.expr(new_cur)?;
        {
            let with_tok = self.tokens.get(new_cur);
            match with_tok {
                Some(&Token::With) => (),
                _ => return Err(format!("Expected Token::With, got {:?}", with_tok)),
            };
            new_cur += 1;
        }
        let (clause, new_cur) = self.clause(new_cur)?;
        Ok((Expr::MatchWith(Box::new(expr), Box::new(clause)), new_cur))
    }

    fn fun_expr(&self, cur: usize) -> Result<(Expr, usize), String> {
        let mut new_cur = cur;
        {
            let fun_tok = self.tokens.get(new_cur);
            match fun_tok {
                Some(&Token::Fun) => (),
                _ => return Err(format!("Expected Token::Fun, got {:?}", fun_tok)),
            };
        }
        new_cur += 1;
        let (param, mut new_cur) = self.ident(new_cur)?;
        {
            let arrow_tok = self.tokens.get(new_cur);
            match arrow_tok {
                Some(&Token::Arrow) => (),
                _ => return Err(format!("Expected Token::Arrow, got {:?}", arrow_tok)),
            };
        }
        new_cur += 1;
        let (body, new_cur) = self.expr(new_cur)?;
        Ok((Expr::Fun(param, Box::new(body)), new_cur))
    }

    fn letrec_expr(&self, cur: usize) -> Result<(Expr, usize), String> {
        todo!()
    }

    fn let_expr(&self, cur: usize) -> Result<(Expr, usize), String> {
        let mut new_cur = cur;
        {
            let let_tok = self.tokens.get(new_cur);
            match let_tok {
                Some(&Token::Let) => (),
                _ => return Err(format!("Expected Token::Fun, got {:?}", let_tok)),
            };
        }
        new_cur += 1;
        match self.tokens.get(new_cur) {
            Some(&Token::Rec) => return self.letrec_expr(cur),
            _ => (),
        }
        let (id, mut new_cur) = self.ident(new_cur)?;
        {
            let assign_tok = self.tokens.get(new_cur);
            match assign_tok {
                Some(&Token::Assign) => (),
                _ => return Err(format!("Expected Token::Assign, got {:?}", assign_tok)),
            };
        }
        new_cur += 1;
        let (bound_expr, mut new_cur) = self.expr(new_cur)?;
        {
            let in_tok = self.tokens.get(new_cur);
            match in_tok {
                Some(&Token::In) => (),
                _ => return Err(format!("Expected Token::In, got {:?}", in_tok)),
            };
        }
        new_cur += 1;
        let (body_expr, new_cur) = self.expr(new_cur)?;
        Ok((
            Expr::LetIn(id, Box::new(bound_expr), Box::new(body_expr)),
            new_cur,
        ))
    }

    fn clause(&self, cur: usize) -> Result<(Clause, usize), String> {
        let (left, mut new_cur) = self.clause_start(cur)?;
        match self.tokens.get(new_cur) {
            Some(&Token::Or) => (),
            _ => return Ok((left, new_cur)),
        };
        new_cur += 1;
        let (right, new_cur) = self.clause(new_cur)?;
        Ok((Clause::Cont(Box::new(left), Box::new(right)), new_cur))
    }

    fn clause_start(&self, cur: usize) -> Result<(Clause, usize), String> {
        let (pat, mut new_cur) = self.pat(cur)?;
        match self.tokens.get(new_cur) {
            Some(&Token::Arrow) => (),
            _ => return Err("Error parsing clause".to_string()),
        };
        new_cur += 1;
        let (expr, new_cur) = self.expr(new_cur)?;
        Ok((Clause::Clause(pat, expr), new_cur))
    }

    fn pat(&self, cur: usize) -> Result<(Pattern, usize), String> {
        let (left, mut new_cur) = self.pat_start(cur)?;
        match self.tokens.get(new_cur) {
            Some(&Token::Cons) => (),
            _ => return Ok((left, new_cur)),
        };
        new_cur += 1;
        let (right, new_cur) = self.pat(new_cur)?;
        Ok((Pattern::Cons(Box::new(left), Box::new(right)), new_cur))
    }

    fn pat_start(&self, cur: usize) -> Result<(Pattern, usize), String> {
        if let Ok((id, _)) = self.ident(cur) {
            return Ok((Pattern::Ident(id), cur + 1));
        }

        match self.tokens.get(cur) {
            Some(&Token::EmptyList) => Ok((Pattern::EmptyList, cur + 1)),
            Some(&Token::Wildcard) => Ok((Pattern::Wildcard, cur + 1)),
            _ => Err("Error parsing pattern".to_string()),
        }
    }

    fn expr(&self, cur: usize) -> Result<(Expr, usize), String> {
        let (left, new_cur) = self.expr_start(cur)?;
        if let Ok((right, new_cur)) = self.expr(new_cur) {
            Ok((Expr::FunApp(Box::new(left), Box::new(right)), new_cur))
        } else {
            Ok((left, new_cur))
        }
    }

    fn expr_start(&self, cur: usize) -> Result<(Expr, usize), String> {
        match self.tokens.get(cur) {
            Some(&Token::If) => self.if_expr(cur),
            Some(&Token::Match) => self.match_expr(cur),
            Some(&Token::Fun) => self.fun_expr(cur),
            Some(&Token::Let) => self.let_expr(cur),
            _ => self.cons_expr(cur),
        }
    }

    fn cons_expr(&self, cur: usize) -> Result<(Expr, usize), String> {
        let (left, mut new_cur) = self.eq_expr(cur)?;
        if let _ = self.tokens.get(new_cur) {
            return Ok((left, new_cur));
        };
        new_cur += 1;
        let (right, new_cur) = self.cons_expr(new_cur)?;
        Ok((Expr::Cons(Box::new(left), Box::new(right)), new_cur))
    }

    fn eq_expr(&self, cur: usize) -> Result<(Expr, usize), String> {
        let (left, mut new_cur) = self.ineq_expr(cur)?;
        let op = self.tokens.get(new_cur);
        let bin_op = match op {
            Some(&Token::Eq) => BinaryOperator::Eq,
            Some(&Token::Neq) => BinaryOperator::Neq,
            _ => return Ok((left, new_cur)),
        };
        new_cur += 1;
        let (right, new_cur) = self.eq_expr(new_cur)?;
        Ok((
            Expr::BinaryOp(bin_op, Box::new(left), Box::new(right)),
            new_cur,
        ))
    }

    fn ineq_expr(&self, cur: usize) -> Result<(Expr, usize), String> {
        let (left, mut new_cur) = self.bool_expr(cur)?;
        let bin_op = match self.tokens.get(new_cur) {
            Some(&Token::Lt) => BinaryOperator::Lt,
            Some(&Token::Le) => BinaryOperator::Le,
            Some(&Token::Gt) => BinaryOperator::Gt,
            Some(&Token::Ge) => BinaryOperator::Ge,
            _ => return Ok((left, new_cur)),
        };
        new_cur += 1;
        let (right, new_cur) = self.ineq_expr(new_cur)?;
        Ok((
            Expr::BinaryOp(bin_op, Box::new(left), Box::new(right)),
            new_cur,
        ))
    }

    fn bool_expr(&self, cur: usize) -> Result<(Expr, usize), String> {
        let (left, mut new_cur) = self.addsub_expr(cur)?;
        let op = self.tokens.get(new_cur);
        let bin_op = match op {
            Some(&Token::Add) => BinaryOperator::Plus,
            Some(&Token::Sub) => BinaryOperator::Minus,
            _ => return Ok((left, new_cur)),
        };
        new_cur += 1;
        let (right, new_cur) = self.bool_expr(new_cur)?;
        Ok((
            Expr::BinaryOp(bin_op, Box::new(left), Box::new(right)),
            new_cur,
        ))
    }

    fn addsub_expr(&self, cur: usize) -> Result<(Expr, usize), String> {
        let (left, mut new_cur) = self.muldiv_expr(cur)?;
        let op = self.tokens.get(new_cur);
        let bin_op = match op {
            Some(&Token::Add) => BinaryOperator::Plus,
            Some(&Token::Sub) => BinaryOperator::Minus,
            _ => return Ok((left, new_cur)),
        };
        new_cur += 1;
        let (right, new_cur) = self.addsub_expr(new_cur)?;
        Ok((
            Expr::BinaryOp(bin_op, Box::new(left), Box::new(right)),
            new_cur,
        ))
    }

    fn muldiv_expr(&self, cur: usize) -> Result<(Expr, usize), String> {
        let (left, mut new_cur) = self.atom(cur)?;
        let op = self.tokens.get(new_cur);
        let bin_op = match op {
            Some(&Token::Mul) => BinaryOperator::Mul,
            Some(&Token::Div) => BinaryOperator::Div,
            _ => return Ok((left, new_cur)),
        };
        new_cur += 1;
        let (right, new_cur) = self.muldiv_expr(new_cur)?;
        Ok((
            Expr::BinaryOp(bin_op, Box::new(left), Box::new(right)),
            new_cur,
        ))
    }

    pub fn check_prog(self) {
        let cur = 0;
        if let Ok((expr, new_cur)) = self.expr(cur) {
            println!("expr: {:?}\nnew_cur: {}", expr, new_cur);
        } else {
            println!("Failed");
        };
    }

    pub fn program(self) -> Option<Program> {
        todo!();
    }
}
