use rand::{Rng, seq::SliceRandom};
use std::fmt;

#[derive(Clone, Copy)]
pub struct UnaryOp {
    pub func: fn(f64) -> f64,
    pub name: &'static str,
}

impl fmt::Debug for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("UnaryOp").field("name", &self.name).finish()
    }
}

#[derive(Clone, Copy)]
pub struct BinaryOp {
    pub func: fn(f64, f64) -> f64,
    pub name: &'static str,
}

impl fmt::Debug for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BinaryOp")
            .field("name", &self.name)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Constant(f64),
    Variable(char),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
}

const UNARY_OPS: &[UnaryOp] = &[
    UnaryOp {
        func: f64::sin,
        name: "sin",
    },
    UnaryOp {
        func: f64::cos,
        name: "cos",
    },
];

const BINARY_OPS: &[BinaryOp] = &[
    BinaryOp {
        func: |a, b| a + b,
        name: "+",
    },
    BinaryOp {
        func: |a, b| a - b,
        name: "-",
    },
    BinaryOp {
        func: |a, b| a * b,
        name: "*",
    },
    BinaryOp {
        func: |a, b| if b.abs() < 1e-6 { 1.0 } else { a / b },
        name: "/",
    },
];

impl Expr {
    pub fn equal(&self, other: &Expr) -> bool {
        match (self, other) {
            (Expr::Constant(c1), Expr::Constant(c2)) => (c1 - c2).abs() < 1e-9,
            (Expr::Variable(v1), Expr::Variable(v2)) => v1 == v2,
            (Expr::Unary(op1, a1), Expr::Unary(op2, a2)) => op1.name == op2.name && a1.equal(a2),
            (Expr::Binary(op1, l1, r1), Expr::Binary(op2, l2, r2)) => {
                op1.name == op2.name && l1.equal(l2) && r1.equal(r2)
            }
            _ => false,
        }
    }
    pub fn simplify(&self) -> Expr {
        let simplified = match self {
            Expr::Unary(op, a) => Expr::Unary(*op, Box::new(a.simplify())),
            Expr::Binary(op, a, b) => {
                Expr::Binary(*op, Box::new(a.simplify()), Box::new(b.simplify()))
            }
            _ => self.clone(),
        };

        match &simplified {
            Expr::Binary(op, a, b) => {
                if let (Expr::Constant(c1), Expr::Constant(c2)) = (&**a, &**b) {
                    return Expr::Constant((op.func)(*c1, *c2));
                }
                match (op.name, &**a, &**b) {
                    ("+", _, Expr::Constant(c)) if c.abs() < 1e-9 => *a.clone(),
                    ("+", Expr::Constant(c), _) if c.abs() < 1e-9 => *b.clone(),
                    ("-", _, Expr::Constant(c)) if c.abs() < 1e-9 => *a.clone(),
                    ("*", _, Expr::Constant(c)) if (c - 1.0).abs() < 1e-9 => *a.clone(),
                    ("*", Expr::Constant(c), _) if (c - 1.0).abs() < 1e-9 => *b.clone(),
                    ("*", _, Expr::Constant(c)) if c.abs() < 1e-9 => Expr::Constant(0.0),
                    ("*", Expr::Constant(c), _) if c.abs() < 1e-9 => Expr::Constant(0.0),
                    ("/", _, Expr::Constant(c)) if (c - 1.0).abs() < 1e-9 => *a.clone(),
                    ("/", Expr::Constant(c), _) if c.abs() < 1e-9 => Expr::Constant(0.0),
                    ("/", Expr::Variable(_), Expr::Variable(_))
                        if a.to_string() == b.to_string() =>
                    {
                        Expr::Constant(1.0)
                    }
                    ("-", Expr::Variable(_), Expr::Variable(_))
                        if a.to_string() == b.to_string() =>
                    {
                        Expr::Constant(0.0)
                    }
                    ("/", _, _) if a.to_string() == b.to_string() => Expr::Constant(1.0),
                    ("-", _, Expr::Constant(c)) if *c < 0.0_f64 => Expr::Binary(
                        BINARY_OPS[0], // +
                        Box::new(*a.clone()),
                        Box::new(Expr::Constant(-c)),
                    ),
                    ("+", _, Expr::Constant(c)) if *c < 0.0_f64 => Expr::Binary(
                        BINARY_OPS[1], // +
                        Box::new(*a.clone()),
                        Box::new(Expr::Constant(-c)),
                    ),
                    _ => simplified,
                }
            }
            Expr::Unary(op, a) => {
                if let Expr::Constant(c) = **a {
                    return Expr::Constant((op.func)(c));
                }
                match (op.name, &**a) {
                    ("sin", Expr::Constant(c)) if c.abs() < 1e-9 => Expr::Constant(0.0),
                    ("cos", Expr::Constant(c)) if c.abs() < 1e-9 => Expr::Constant(1.0),
                    _ => simplified,
                }
            }
            _ => simplified,
        }
    }

    pub fn evaluate(&self, x: f64) -> f64 {
        match self {
            Expr::Constant(c) => *c,
            Expr::Variable(_) => x,
            Expr::Unary(op, a) => (op.func)(a.evaluate(x)),
            Expr::Binary(op, a, b) => (op.func)(a.evaluate(x), b.evaluate(x)),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Expr::Constant(c) => format!("{:.2}", c),
            Expr::Variable(v) => v.to_string(),
            Expr::Unary(op, a) => format!("{}({})", op.name, a.to_string()),
            Expr::Binary(op, a, b) => format!("({} {} {})", a.to_string(), op.name, b.to_string()),
        }
    }
}

pub fn generate_random_expr(depth: u32) -> Expr {
    let mut rng = rand::thread_rng();
    if depth == 0 || rng.gen_bool(0.3) {
        if rng.gen_bool(0.5) {
            Expr::Constant(rng.gen_range(-5.0..5.0))
        } else {
            Expr::Variable('x')
        }
    } else {
        let op_type: u8 = rng.gen_range(0..=2);
        match op_type {
            0 => {
                // Unary
                let op = UNARY_OPS.choose(&mut rng).unwrap();
                let operand = Box::new(generate_random_expr(depth - 1));
                Expr::Unary(*op, operand)
            }
            _ => {
                // Binary
                let op = BINARY_OPS.choose(&mut rng).unwrap();
                let left = Box::new(generate_random_expr(depth - 1));
                let right = Box::new(generate_random_expr(depth - 1));
                Expr::Binary(*op, left, right)
            }
        }
    }
}

pub fn fitness(expr: &Expr, data: &[(f64, f64)]) -> f64 {
    let mut mse = 0.0;
    for (x, y_true) in data {
        let y_pred = expr.evaluate(*x);
        mse += (y_true - y_pred).powi(2);
    }
    mse / data.len() as f64
}

pub fn fitness_max_abs(expr: &Expr, data: &[(f64, f64)]) -> f64 {
    let mut max_abs_error = 0.0;
    for (x, y_true) in data {
        let y_pred = expr.evaluate(*x);
        let abs_error = (y_true - y_pred).abs();
        if abs_error > max_abs_error {
            max_abs_error = abs_error;
        }
    }
    max_abs_error
}

pub fn crossover(parent1: &Expr, parent2: &Expr) -> Expr {
    let mut rng = rand::thread_rng();
    let mut p1 = parent1.clone();
    let p2 = parent2.clone();

    // For simplicity, we'll just swap a random subtree.
    // A more robust implementation would traverse the tree and pick a node.
    if rng.gen_bool(0.5) {
        if let Expr::Binary(_, ref mut l, _) = p1 {
            if let Expr::Binary(_, _, r2) = p2 {
                *l = r2;
            }
        }
    } else {
        if let Expr::Binary(_, _, ref mut r) = p1 {
            if let Expr::Binary(_, l2, _) = p2 {
                *r = l2;
            }
        }
    }
    p1
}

pub fn mutate(expr: &Expr) -> Expr {
    let mut rng = rand::thread_rng();
    let mut new_expr = expr.clone();

    if rng.gen_bool(0.2) {
        // 20% chance to mutate a node
        // Replace a random node with a new random expression
        let depth = rng.gen_range(1..3);
        return generate_random_expr(depth);
    }

    match &mut new_expr {
        Expr::Unary(_, a) => {
            **a = mutate(a);
        }
        Expr::Binary(_, l, r) => {
            if rng.gen_bool(0.5) {
                **l = mutate(l);
            } else {
                **r = mutate(r);
            }
        }
        _ => {}
    }
    new_expr
}


pub fn print_expr_as_tree(expr: &Expr, indent: usize) {
    print!("|");
    for _ in 0..indent {
        print!("-");
    }
    match expr {
        Expr::Constant(c) => println!("Constant({:.2})", c),
        Expr::Variable(v) => println!("Variable({})", v),
        Expr::Unary(op, a) => {
            println!("Unary({})", op.name);
            print_expr_as_tree(a, indent + 1);
        }
        Expr::Binary(op, a, b) => {
            println!("Binary({})", op.name);
            print_expr_as_tree(a, indent + 4);
            print_expr_as_tree(b, indent + 4);
        }
    }
}
