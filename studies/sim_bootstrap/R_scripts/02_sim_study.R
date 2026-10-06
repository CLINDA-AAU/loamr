out_path <- here::here("studies", "sim_bootstrap", "Output")

## -----------------------------------------------------------------------------
## Simulation setup
## -----------------------------------------------------------------------------
source(here::here("studies", "sim_bootstrap", "R_scripts", "01_functions.R"))

a <- 15; b <- 10; h <- 3

SigmaA_H0  <- matrix(c(2, 0.5, 0.5, 2), 2, 2)
SigmaB_H0  <- matrix(c(1, 0.2, 0.2, 1), 2, 2)
SigmaE_H0  <- matrix(c(0.5, 0, 0, 0.5), 2, 2)

## Under an alternative: method 2 has larger sigma_B^2
SigmaB_H1 <- matrix(c(1, 0.5, 0.5, 2.5), 2, 2)

## True interaction present. Tested under both interaction = F (worst case;
## wrong model) and interaction = T (best case; correct model)
SigmaAB_inter <- matrix(c(0.3, 0.15, 0.15, 0.3), 2, 2)

## No true interaction
SigmaAB_null <- matrix(0, 2, 2)

## Set n_cores explicitly if you don't want run_sim_study() to use
## (detectCores() - 1) by default

# Helper function for loading or running simulation results
load_or_run <- function(path, expr) {
  if (!file.exists(path)) {
    obj <- eval.parent(substitute(expr))
    saveRDS(obj, path)
  } else {
    obj <- readRDS(path)
  }
  obj
}

## Note: 5-ish minutes run time per case below

## -----------------------------------------------------------------------------
## Case: under H0; true interaction, model correctly specified (interaction = TRUE)
## -----------------------------------------------------------------------------
cat("=== H0, interaction = TRUE (true interaction present) ===\n")

sim_H0_int <- load_or_run(
  file.path(out_path, "sim_H0_interaction.rds"),
  run_sim_study(
    a, b, h, SigmaA_H0, SigmaB_H0, SigmaAB_inter, SigmaE_H0,
    M = 1000, R = 10000, seed = 1, interaction = TRUE
  )
)

summarise_sim(sim_H0_int)
plot_sim(sim_H0_int, file = file.path(out_path, "sim_H0_interaction.pdf"))


## -----------------------------------------------------------------------------
## Case: under H0; true interaction, model misspecified (interaction = FALSE)
## -----------------------------------------------------------------------------
cat("=== H0, interaction = FALSE, MISSPECIFIED (true interaction present but not modelled) ===\n")

sim_H0_mis <- load_or_run(
  file.path(out_path, "sim_H0_no_interaction_misspecified.rds"),
  run_sim_study(
    a, b, h, SigmaA_H0, SigmaB_H0, SigmaAB_inter, SigmaE_H0,
    M = 1000, R = 10000, seed = 1, interaction = FALSE
  )
)

summarise_sim(sim_H0_mis)
plot_sim(sim_H0_mis, file = file.path(out_path, "sim_H0_no_interaction_misspecified.pdf"))

## -----------------------------------------------------------------------------
## Case: under H0; no true interaction, model correctly specified
## -----------------------------------------------------------------------------
cat("=== H0, interaction = FALSE, WELL-SPECIFIED (no true interaction) ===\n")

sim_H0_wellspec <- load_or_run(
  file.path(out_path, "sim_H0_no_interaction_wellspecified.rds"),
  run_sim_study(
    a, b, h, SigmaA_H0, SigmaB_H0, SigmaAB_null, SigmaE_H0,
    M = 1000, R = 10000, seed = 2, interaction = FALSE
  )
)

summarise_sim(sim_H0_wellspec)
plot_sim(sim_H0_wellspec, file = file.path(out_path, "sim_H0_no_interaction_wellspecified.pdf"))

## -----------------------------------------------------------------------------
## Case: under H1; true interaction, model correctly specified
## -----------------------------------------------------------------------------
cat("=== H1 (difference in sigma_B), interaction = TRUE ===\n")

sim_H1 <- load_or_run(
  file.path(out_path, "sim_H1.rds"),
  run_sim_study(
    a, b, h, SigmaA_H0, SigmaB_H1, SigmaAB_inter, SigmaE_H0,
    M = 1000, R = 10000, seed = 3, interaction = TRUE
  )
)

summarise_sim(sim_H1)
plot_sim(sim_H1, file = file.path(out_path, "sim_H1.pdf"))

## -----------------------------------------------------------------------------
## h = 1: repeatability not available
## -----------------------------------------------------------------------------
cat("=== h = 1 (single measurement per cell) ===\n")

sim_H1_norep <- load_or_run(
  file.path(out_path, "sim_H1_norep.rds"),
  run_sim_study(
    a, b, h = 1,
    SigmaA_H0, SigmaB_H0, SigmaAB_null, SigmaE_H0,
    M = 1000, R = 10000, seed = 4, interaction = FALSE
  )
)

summarise_sim(sim_H1_norep)
plot_sim(sim_H1_norep, file = file.path(out_path, "sim_H1_norep.pdf"))


## -----------------------------------------------------------------------------
## Case: under H0; true interaction, model correctly specified (interaction = TRUE); b increased
## -----------------------------------------------------------------------------
cat("=== H0, interaction = TRUE (true interaction present), b increased ===\n")

sim_H0_int_largeb <- load_or_run(
  file.path(out_path, "sim_H0_interaction_largeb.rds"),
  run_sim_study(
    a, b = 30, h, SigmaA_H0, SigmaB_H0, SigmaAB_inter, SigmaE_H0,
    M = 1000, R = 10000, seed = 1, interaction = TRUE
  )
)

summarise_sim(sim_H0_int_largeb)
plot_sim(sim_H0_int, file = file.path(out_path, "sim_H0_interaction_largeb.pdf"))










