## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      warning = FALSE, message = FALSE,
                      fig.width = 7, fig.height = 5)

## ----libraries----------------------------------------------------------------
library(r4lineups)

## ----simulate-----------------------------------------------------------------
set.seed(123)
n_subj  <- 40
n_items <- 60

df <- data.frame(
  subject_id = rep(1:n_subj, each = n_items),
  item_id    = rep(1:n_items, times = n_subj)
)
df$is_old <- rbinom(nrow(df), 1, 0.5)

# Average d' = 1.2. Participants and items differ in their tendency to say
# "old" (shifts, which enter on old and new trials alike), and participants
# also differ in their own d' (slope deviations, which enter on old trials only)
subj_shift <- rnorm(n_subj,  0, 0.4)
item_shift <- rnorm(n_items, 0, 0.4)
subj_slope <- rnorm(n_subj,  0, 0.5)

df$said_old <- rbinom(
  nrow(df), 1,
  stats::pnorm((1.2 + subj_slope[df$subject_id]) * df$is_old +
                 subj_shift[df$subject_id] + item_shift[df$item_id])
)

## ----glm_fit------------------------------------------------------------------
glm_fit <- fit_sdt_glm(df, is_old = "is_old", said_old = "said_old",
                       link = "probit")
extract_sdt_metrics(glm_fit)

## ----glmm_fit-----------------------------------------------------------------
glmm_fit <- fit_sdt_glmm(df,
                         is_old       = "is_old",
                         said_old     = "said_old",
                         subject_id   = "subject_id",
                         item_id      = "item_id",
                         link         = "probit",
                         random_slope = TRUE)
extract_sdt_metrics(glmm_fit)

## ----crossed_effects----------------------------------------------------------
structures <- list(
  "GLM: no clustering"        = fit_sdt_glm(df, "is_old", "said_old",
                                            link = "probit"),
  "GLMM: subject only"        = fit_sdt_glmm(df, "is_old", "said_old",
                                             subject_id = "subject_id",
                                             link = "probit",
                                             random_slope = FALSE),
  "GLMM: subject and item"    = fit_sdt_glmm(df, "is_old", "said_old",
                                             subject_id = "subject_id",
                                             item_id = "item_id",
                                             link = "probit",
                                             random_slope = FALSE),
  "GLMM: subject slope and item" = glmm_fit
)

comparison <- do.call(rbind, lapply(names(structures), function(nm) {
  m  <- structures[[nm]]
  co <- summary(m)$coefficients
  term <- attr(m, "sdt_is_old_term")
  if (is.null(term)) term <- ".is_old_c"
  data.frame(
    `Random effects` = nm,
    `d'`             = round(co[term, "Estimate"], 3),
    `SE`             = round(co[term, "Std. Error"], 4),
    `z`              = round(co[term, "z value"], 1),
    check.names = FALSE
  )
}))

knitr::kable(
  comparison, row.names = FALSE,
  caption = "Estimated d', its standard error and z under four random-effect structures. Simulated data, 40 participants by 60 items, average d' = 1.2."
)

## ----attenuation--------------------------------------------------------------
fit_re <- structures[["GLMM: subject slope and item"]]
vc     <- lme4::VarCorr(fit_re)
v_subj <- vc[[".subject_id"]]
v_item <- vc[[".item_id"]][1, 1]
v_x    <- function(x) v_subj[1, 1] + 2 * x * v_subj[1, 2] +
  x^2 * v_subj[2, 2] + v_item

b    <- lme4::fixef(fit_re)
cond <- b[[".is_old_c"]]
pred <- (b[["(Intercept)"]] + 0.5 * cond) / sqrt(1 + v_x(0.5)) -
  (b[["(Intercept)"]] - 0.5 * cond) / sqrt(1 + v_x(-0.5))
marg <- stats::coef(structures[["GLM: no clustering"]])[[".is_old_c"]]

knitr::kable(
  data.frame(
    Quantity = c("Conditional d' (GLMM with random slope)",
                 "Random-effect variance, new items",
                 "Random-effect variance, old items",
                 "Predicted marginal d'",
                 "Marginal d' (GLM), observed"),
    Value = round(c(cond, v_x(-0.5), v_x(0.5), pred, marg), 3)
  ),
  row.names = FALSE,
  caption = "Marginal and conditional d' are different quantities related by a known attenuation. Simulated data."
)

## ----covariates---------------------------------------------------------------
df$delay <- rnorm(nrow(df), 0, 1)

glm_fit_cov <- fit_sdt_glm(df,
                           is_old       = "is_old",
                           said_old     = "said_old",
                           covariates   = "delay",
                           link         = "probit",
                           interactions = TRUE)
summary(glm_fit_cov)

