use rand::Rng;
use rand::seq::SliceRandom;
use symbolic_regression::*;

fn main() {
    // Target function: y = x^2 + x + 1
    let target_fn = |x: f64| x.powi(2) + x + 1.0 + x * x.cos();
    let data: Vec<(f64, f64)> = (0..100)
        .map(|i| {
            let x = i as f64 / 10.0;
            (x, target_fn(x))
        })
        .collect();

    let population_size = 100;
    let generations = 5000;
    let elite_size = 5;
    let mutation_rate = 0.3;
    let simpify_rate = 0.5;

    let mut population: Vec<Expr> = (0..population_size)
        .map(|_| generate_random_expr(3))
        .collect();

    for generation in 0..generations {
        let mut fitness_scores: Vec<(f64, &Expr)> = population
            .iter()
            .map(|expr| (fitness(expr, &data), expr))
            .collect();

        fitness_scores.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());

        let best_expr = fitness_scores[0].1;
        println!(
            "Generation {}: Best fitness = {:.4}, Best expr = {}",
            generation,
            fitness_scores[0].0,
            best_expr.to_string()
        );

        if fitness_scores[0].0 < 1e-4 {
            println!("\nFound a good solution!");
            break;
        }

        let mut new_population = Vec::new();

        // Elitism
        for i in 0..elite_size {
            new_population.push(fitness_scores[i].1.clone());
        }

        // Crossover and mutation
        while new_population.len() < population_size {
            let mut rng = rand::thread_rng();
            let parents: Vec<&Expr> = fitness_scores
                .iter()
                .take(population_size / 2) // Select from top 50%
                .map(|(_, expr)| *expr)
                .collect();

            let parent1 = parents.choose(&mut rng).unwrap();
            let parent2 = parents.choose(&mut rng).unwrap();

            let mut offspring = crossover(parent1, parent2);
            if rng.gen_bool(mutation_rate) {
                offspring = mutate(&offspring);
                if rng.gen_bool(simpify_rate) {
                    offspring = offspring.simplify();
                }
            }
            new_population.push(offspring);
        }
        population = new_population;
    }

    let mut final_scores: Vec<(f64, &Expr)> = population
        .iter()
        .map(|expr| (fitness(expr, &data), expr))
        .collect();
    final_scores.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());

    println!(
        "\nFinal best expression: {}",
        final_scores[0].1.simplify().to_string()
    );
    println!("Final fitness: {:.4}", final_scores[0].0);
}
