## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA
)

# This vignette draws random numbers in several places -- simulated ages in the
# data-format example, and every bootstrap below. Seeding once here makes the
# whole document reproducible, so the numbers quoted in the prose match the
# numbers in the output.
set.seed(2026)

## ----install, eval = FALSE----------------------------------------------------
# install.packages("r4lineups",
#                  repos = c("https://cgtza2.r-universe.dev",
#                            "https://cloud.r-project.org"))

## -----------------------------------------------------------------------------
library(r4lineups)

## -----------------------------------------------------------------------------
lineup_vec_demo <- c(3, 2, 5, 6, 1, 3, 3, 5, 6, 4)

## -----------------------------------------------------------------------------
table(lineup_vec_demo)

## -----------------------------------------------------------------------------
participant_num <- paste("Partic_", 1:10, sep = "")
age             <- round(runif(10, 18, 80), 2)
line_df         <- data.frame(participant_num, age,
                              lineup_vec = lineup_vec_demo)
line_df

## -----------------------------------------------------------------------------
a_lineup_vec <- line_df$lineup_vec

## -----------------------------------------------------------------------------
a_lineup_table <- table(line_df$lineup_vec)

## -----------------------------------------------------------------------------
null_foil_vec <- rep(c(5, 3, 2, 1, 8, 6, 7), each = 4)

# Position 4 was never chosen, so this table has 7 entries, not 8
table(null_foil_vec)

## -----------------------------------------------------------------------------
lineup_table(null_foil_vec, k = 8, suspect_pos = 3)

## -----------------------------------------------------------------------------
data(nortje2012)

## ----echo=FALSE, results='asis'-----------------------------------------------
pander::pandoc.table(nortje2012[1:3, ], "nortje2012 data (first 3 rows of 133)")

## -----------------------------------------------------------------------------
lineup_vec   <- nortje2012$lineup_1
lineup_list  <- as.list(nortje2012)
lineup_tab   <- lineup_table(lineup_vec, k = 8)

## -----------------------------------------------------------------------------
lineup_prop_vec(lineup_vec, target_pos = 3, k = 8)

## ----warning=FALSE------------------------------------------------------------
bootobject <- boot::boot(lineup_vec, lineup_prop_boot, target_pos = 3, R = 1000)
bootobject

## ----warning=FALSE------------------------------------------------------------
cis <- boot::boot.ci(bootobject, conf = 0.95, type = "bca", target_pos = 3)
cis

## -----------------------------------------------------------------------------
n_chose_3 <- sum(lineup_vec == 3)
n_total   <- length(lineup_vec)

# Exact (Clopper-Pearson) interval
binom.test(n_chose_3, n_total)$conf.int

# Normal-theory interval
prop.test(n_chose_3, n_total)$conf.int

## ----boot_boundary------------------------------------------------------------
never_chosen <- rep(2:6, 6)   # 30 mock witnesses; member 1 never chosen
b0 <- boot::boot(never_chosen, lineup_prop_boot, target_pos = 1, R = 1000)
range(b0$t)                              # every resample gives 0
binom.test(0, length(never_chosen))$conf.int

## -----------------------------------------------------------------------------
allprop(lineup_vec, k = 8)

## -----------------------------------------------------------------------------
lineuprops_ci <- lineup_boot_allprop(lineup_vec, k = 8, R = 1000)
lineuprops_ci

## -----------------------------------------------------------------------------
lineup_boot_allprop(lineup_vec, k = 8, conf = 1 - 0.05 / 8, R = 1000)

## ----fig.width = 6, fig.height = 4--------------------------------------------
bias_dist <- lineup_bias_boot_dist(lineup_vec, target_pos = 3, k = 8, R = 1000)
plot_lineup_bias_distribution(bias_dist, target_pos = 3)

## -----------------------------------------------------------------------------
func_size_report(lineup_vec, target_pos = 3, k = 8, R = 1000)

## -----------------------------------------------------------------------------
esize_m(lineup_tab, k = 8, both = TRUE)

## -----------------------------------------------------------------------------
# Position 4 was never chosen
esize_m(lineup_table(null_foil_vec, k = 8), k = 8, both = TRUE)

## -----------------------------------------------------------------------------
# Create a dataframe of bootstrapped lineup data
bootdata <- gen_boot_samples(lineup_vec, 1000)

# Calculate effective size for each lineup in bootdata.
# Nominal size is again declared by the user.
lineupsizes <- gen_esize_m(bootdata, k = 8)

# Pass the vector of bootstrapped effective sizes to gen_esize_m_ci
# to calculate lower and upper CIs with the desired level of alpha
gen_esize_m_ci(lineupsizes, perc = .025)
gen_esize_m_ci(lineupsizes, perc = .975)

## -----------------------------------------------------------------------------
gen_boot_propmean_se(lineupsizes)

## -----------------------------------------------------------------------------
# Compute effective size from a lineup table
esize_T(lineup_tab)

# Compute bootstrapped effective size
esize_boot <- boot::boot(lineup_vec, esize_T_boot, R = 1000)
esize_boot

## -----------------------------------------------------------------------------
esize_boot_ci <- boot::boot.ci(esize_boot, type = c("perc", "bca"))
esize_boot_ci

## ----fig.width = 6, fig.height = 4--------------------------------------------
esize_dist <- esize_boot_dist(lineup_vec, k = 8, metric = "tredoux", R = 1000)
plot_esize_distribution(esize_dist, metric = "E' (Tredoux, 1998)")

## ----esize_bias_demo----------------------------------------------------------
set.seed(4242)
k <- 6L

bias_demo <- do.call(rbind, lapply(c(20, 50, 100, 500), function(N) {
  est <- replicate(2000, {
    counts <- tabulate(sample.int(k, N, replace = TRUE), nbins = k)
    c(plug_in = esize_T(counts), corrected = esize_T(counts, unbiased = TRUE))
  })
  data.frame(
    `N choices`  = N,
    `Plug-in E'` = round(mean(est["plug_in", ]), 3),
    `Predicted`  = round(N * k / (N + k - 1), 3),
    `Corrected`  = round(mean(est["corrected", ]), 3),
    check.names  = FALSE
  )
}))

knitr::kable(
  bias_demo, row.names = FALSE,
  caption = "Mean E' from simulated fair six-person lineups, where the true effective size is exactly 6. Simulated data, 2000 replications per row."
)

## ----esize_unbiased-----------------------------------------------------------
c(plug_in = esize_T(lineup_tab),
  corrected = esize_T(lineup_tab, unbiased = TRUE))

## ----ta_effective_size--------------------------------------------------------
data(lineup_example)

ta <- lineup_example[!lineup_example$target_present, ]
chosen <- ta$lineup_member[ta$lineup_member > 0]

c(plug_in   = esize_T(table(factor(chosen, levels = 1:6))),
  corrected = esize_T(table(factor(chosen, levels = 1:6)), unbiased = TRUE))

## -----------------------------------------------------------------------------
eff_size_per_foils(lineup_vec, target_pos = seq_len(8), k = 8,
                   conf = 0.95, R = 1000)

## -----------------------------------------------------------------------------
for (conf in c(0.5, 0.7, 0.95, 0.99)) {
  cat("conf =", conf, "->",
      eff_size_per_foils(lineup_vec, target_pos = seq_len(8), k = 8,
                         conf = conf, R = 1000), "\n")
}

## -----------------------------------------------------------------------------
# Get data
linedf <- nortje2012[1:2]

# Compare effective size
effsize_compare(linedf)

## -----------------------------------------------------------------------------
# Target present & target absent lineup data for 1 lineup pair
TP_lineup <- nortje2012$lineup_1
TA_lineup <- nortje2012$lineup_3

# Compute diagnosticity ratio
diag_ratio_W(TP_lineup, TA_lineup, pos_pres = 3, pos_abs = 7, k1 = 8, k2 = 8)
diag_ratio_T(TP_lineup, TA_lineup, pos_pres = 3, pos_abs = 7, k1 = 8, k2 = 8)

## -----------------------------------------------------------------------------
var_diag_ratio(TP_lineup, TA_lineup, 3, 7, 8, 8)

## -----------------------------------------------------------------------------
# Target present data:
TP_lineup1 <- rep(1:6, length.out = 100)
TP_lineup2 <- rep(1:5, length.out = 70)
TP_lineup3 <- rep(1:4, length.out = 20)
lineup_pres_list <- list(TP_lineup1, TP_lineup2, TP_lineup3)

# Target absent data:
TA_lineup1 <- rep(6:1, length.out = 100)
TA_lineup2 <- rep(5:1, length.out = 70)
TA_lineup3 <- rep(4:1, length.out = 20)
lineup_abs_list <- list(TA_lineup1, TA_lineup2, TA_lineup3)

## -----------------------------------------------------------------------------
pos_list <- c(3, 2, 1)

## -----------------------------------------------------------------------------
k <- c(6, 5, 4)

## ----warning=FALSE------------------------------------------------------------
homog_diag(lineup_pres_list, lineup_abs_list, pos_list, k)

## ----warning=FALSE------------------------------------------------------------
# A helper: n1 witnesses choose position 1, the rest spread over positions 2-6
choose_pos1 <- function(n1) c(rep(1, n1), rep(2:6, length.out = 100 - n1))

# Suspect ID rate falls from .60 to .32 across the three TP lineups,
# and rises from .10 to .20 across the three TA lineups
het_pres <- list(choose_pos1(60), choose_pos1(45), choose_pos1(32))
het_abs  <- list(choose_pos1(10), choose_pos1(15), choose_pos1(20))

homog_diag(het_pres, het_abs, pos_list = c(1, 1, 1), k = c(6, 6, 6))

## ----warning=FALSE------------------------------------------------------------
mixed_pres <- list(choose_pos1(60), choose_pos1(30), choose_pos1(20))
mixed_abs  <- list(choose_pos1(10), choose_pos1(30), choose_pos1(45))

homog_diag(mixed_pres, mixed_abs, pos_list = c(1, 1, 1), k = c(6, 6, 6))

## ----warning=FALSE------------------------------------------------------------
homog_diag_boot(lineup_pres_list = het_pres,
                lineup_abs_list = het_abs,
                pos_list = c(1, 1, 1),
                k = c(6, 6, 6),
                R = 200,
                seed = 1)

## ----warning=FALSE------------------------------------------------------------
homog_diag_boot(lineup_pres_list = mixed_pres,
                lineup_abs_list = mixed_abs,
                pos_list = c(1, 1, 1),
                k = c(6, 6, 6),
                R = 200,
                seed = 1)

## -----------------------------------------------------------------------------
# Raw data as it should be collected: the member chosen (0 = rejected the lineup)
raw <- data.frame(
  target_present = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
  chosen_member  = c(3, 5, 0, 3, 2, 0),
  confidence     = c(90, 70, 40, 80, 60, 50)
)

# The suspect occupies position 3 in both the TP and TA lineups
suspect_pos <- 3

raw$identification <- ifelse(raw$chosen_member == 0, "reject",
                      ifelse(raw$chosen_member == suspect_pos, "suspect",
                             "filler"))
raw

## ----echo = FALSE, results='asis', warning=FALSE------------------------------
data(lineup_example)
pander::pandoc.table(lineup_example[1:8, ],
                     paste0("lineup_example (first 8 rows of ",
                            nrow(lineup_example), ")"))

## ----fig.cap = "ROC Curve for Confidence ~ Accuracy", fig.width = 6, fig.height = 5, warning=FALSE----
roc1 <- make_rocdata(lineup_example, lineup_size = 6)
plot(roc1)

## ----warning=FALSE------------------------------------------------------------
roc_result <- make_roc(lineup_example, lineup_size = 6, show_plot = FALSE)
roc_result

## ----fig.width = 6, fig.height = 5, warning=FALSE-----------------------------
plot(roc1) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::labs(title = "Partial ROC, Nortje & Tredoux mock-witness data")

## ----fig.width = 6, fig.height = 5, warning=FALSE-----------------------------
full_roc1 <- make_fullroc(lineup_example, lineup_size = 6, show_plot = FALSE)
plot(full_roc1)

