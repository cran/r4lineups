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

# True d' = 1.2, with both participants and items varying in difficulty
subj_shift <- rnorm(n_subj,  0, 0.4)
item_shift <- rnorm(n_items, 0, 0.4)

df$said_old <- rbinom(
  nrow(df), 1,
  stats::pnorm(1.2 * df$is_old + subj_shift[df$subject_id] +
                 item_shift[df$item_id])
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
                         random_slope = FALSE)
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
                                             random_slope = FALSE)
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
    check.names = FALSE
  )
}))

knitr::kable(
  comparison, row.names = FALSE,
  caption = "Estimated d' and its standard error under three random-effect structures. Simulated data, 40 participants by 60 items, true d' = 1.2."
)

## ----attenuation--------------------------------------------------------------
vc <- as.data.frame(lme4::VarCorr(structures[["GLMM: subject and item"]]))
total_var <- sum(vc$vcov)

cond <- lme4::fixef(structures[["GLMM: subject and item"]])[[".is_old_c"]]
marg <- stats::coef(structures[["GLM: no clustering"]])[[".is_old_c"]]

knitr::kable(
  data.frame(
    Quantity = c("Conditional d' (GLMM)",
                 "Random-effect variance, total",
                 "Attenuation factor",
                 "Conditional / attenuation factor",
                 "Marginal d' (GLM), observed"),
    Value = round(c(cond, total_var, sqrt(1 + total_var),
                    cond / sqrt(1 + total_var), marg), 3)
  ),
  row.names = FALSE,
  caption = "Marginal and conditional d' are different quantities related by a known factor. Simulated data."
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

