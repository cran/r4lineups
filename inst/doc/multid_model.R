## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      warning = FALSE, message = FALSE,
                      fig.width = 7, fig.height = 5)

## ----libraries----------------------------------------------------------------
library(r4lineups)

## ----table4-------------------------------------------------------------------
md <- make_multid(counts = c(G = 45, Fp = 24, Rp = 31, I = 17, Fa = 26, Ra = 57),
                  correction = "none")
md

## ----decomposition-plot-------------------------------------------------------
plot(md)

## ----fair-reference-----------------------------------------------------------
md$indices[md$indices$index %in% c("IFa", "fair_reference", "ifa_vs_fair"),
           c("index", "estimate")]
make_multid(counts = c(G = 45, Fp = 24, Rp = 31, I = 17, Fa = 26, Ra = 57),
            filler_rate = "per_filler", correction = "none")$indices[3, ]

## ----trial-level--------------------------------------------------------------
data(lineup_example)
md2 <- make_multid(lineup_example, by_confidence = TRUE,
                   n_bootstrap = 500, seed = 2026)
summary(md2)

## ----indices-plot-------------------------------------------------------------
plot(md2, type = "indices")

## ----confidence-plot----------------------------------------------------------
plot(md2, type = "confidence")

## ----compare------------------------------------------------------------------
sim <- make_multid(counts = c(G = 52, Fp = 25, Rp = 24, I = 28, Fa = 26, Ra = 46),
                   n_bootstrap = 500, seed = 1)
sqn <- make_multid(counts = c(G = 44, Fp = 19, Rp = 39, I = 15, Fa = 17, Ra = 68),
                   n_bootstrap = 500, seed = 2)
compare_multid(sim, sqn, labels = c("simultaneous", "sequential"))

