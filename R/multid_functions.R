#' Multi-d' Model of Lineup Performance (Lee & Penrod, 2019)
#'
#' Computes the multiple empirical discriminability (d') indices of Lee and
#' Penrod's (2019) Multi-d' model from target-present (TP) and target-absent
#' (TA) lineup data. Unlike the conventional 2 x 2 signal detection treatment,
#' which folds filler identifications into "negative" responses, the Multi-d'
#' model uses the full 2 x 3 response table (suspect ID, filler ID, rejection)
#' in each lineup type.
#'
#' @param data A data frame with columns \code{target_present} (logical) and
#'   \code{identification} (\code{"suspect"}, \code{"filler"} or
#'   \code{"reject"}). A \code{confidence} column is needed only when
#'   \code{by_confidence = TRUE}. TA rows with \code{identification ==
#'   "suspect"} mark a designated innocent suspect; if there are none, the
#'   innocent suspect is estimated from filler choices (see Details). Supply
#'   either \code{data} or \code{counts}.
#' @param counts A named numeric vector of response frequencies with elements
#'   \code{G} (guilty-suspect IDs), \code{Fp} (TP filler IDs), \code{Rp} (TP
#'   rejections), \code{I} (innocent-suspect IDs), \code{Fa} (TA filler IDs) and
#'   \code{Ra} (TA rejections). Omit \code{I} (or set it to \code{NA}) when the
#'   TA lineups have no designated innocent suspect; \code{Fa} is then the total
#'   number of TA filler IDs and is split as described in Details. Proportions
#'   may be supplied in place of frequencies, provided \code{n_bootstrap = 0}.
#' @param lineup_size Nominal lineup size (suspect plus fillers). Default 6.
#' @param filler_rate Either \code{"total"} (default), which uses the total
#'   filler ID rate as Lee and Penrod (2019) do, or \code{"per_filler"}, which
#'   divides each filler ID rate by the number of fillers (\code{lineup_size - 1})
#'   so that the filler indices refer to an average single filler.
#' @param correction Correction for rates of 0 or 1: \code{"loglinear"}
#'   (default), \code{"half"} or \code{"none"}. See
#'   \code{\link{sdt_summary_from_counts}}. Use \code{"none"} to reproduce
#'   published values computed from raw proportions.
#' @param by_confidence Logical. If \code{TRUE}, the indices are also computed
#'   cumulatively by confidence, as in Lee and Penrod's (2019) Table 9.
#'   Requires \code{data} with a \code{confidence} column.
#' @param conf_thresholds Optional numeric vector of confidence thresholds for
#'   \code{by_confidence}. Defaults to every observed confidence value.
#' @param n_bootstrap Number of bootstrap resamples for percentile confidence
#'   intervals (default 0, no bootstrap). Resampling is done within the TP and
#'   TA arms separately.
#' @param conf_level Confidence level for bootstrap intervals. Default 0.95.
#' @param seed Optional random seed for the bootstrap.
#' @param innocent_suspect For trial-level \code{data}, how innocent-suspect
#'   identifications are obtained; see \code{\link{make_rocdata}}. Ignored
#'   for \code{counts}, where the presence of \code{I} decides.
#'
#' @details
#' With each d' computed as the difference between two z-transformed
#' (inverse-normal) response rates, the model defines:
#' \itemize{
#'   \item \eqn{d'(GI) = z(G) - z(I)}: guilty versus innocent suspect, the usual
#'     empirical discriminability;
#'   \item \eqn{d'(GFp) = z(G) - z(Fp)}: guilty suspect versus TP fillers;
#'   \item \eqn{d'(IFa) = z(I) - z(Fa)}: innocent suspect versus TA fillers,
#'     which Lee and Penrod treat as TA lineup bias;
#'   \item \eqn{d'(FaFp) = z(Fa) - z(Fp)}: the greater appeal of TA than TP
#'     fillers (differential filler siphoning);
#'   \item \eqn{d'(RaRp) = z(Ra) - z(Rp)}: discrimination of culprit presence
#'     from culprit absence (detection).
#' }
#' Lee and Penrod also propose \eqn{d'(GFp) - d'(RaRp)} as an index of TP
#' lineup bias net of memory strength, and \eqn{d'(GFp) - d'(IFa)} as an index
#' of differential filler siphoning. The decomposition
#' \deqn{d'(GI) = d'(GFp) - d'(IFa) - d'(FaFp)}
#' holds exactly for any data, because the filler terms cancel. It is an
#' accounting identity that shows where d'(GI) comes from, not an empirical
#' result.
#'
#' When TA lineups have no designated innocent suspect, the innocent-suspect
#' count is estimated as the TA filler count divided by \code{lineup_size}, and
#' the remaining \eqn{(k - 1)/k} of filler choices are kept as TA filler IDs.
#' Lee and Penrod call this the "average-filler" practice. It forces the TA
#' lineup to be fair, so d'(IFa) then reflects the rejection rate rather than
#' lineup bias.
#'
#' With \code{filler_rate = "total"}, a perfectly fair TA lineup does not give
#' \eqn{d'(IFa) = 0}: the innocent suspect is compared with all fillers
#' together. The returned \code{fair_reference} is the d'(IFa) a perfectly
#' fair lineup would give at the observed TA rejection rate (for example,
#' about -1.93 when no one rejects a 6-person lineup), and \code{ifa_vs_fair}
#' is the observed d'(IFa) minus that reference. These two quantities are not
#' part of Lee and Penrod's model; they are provided to help interpret d'(IFa).
#' With \code{filler_rate = "per_filler"}, the fair reference is 0, and the
#' average-filler practice gives d'(IFa) = 0 exactly when \code{correction =
#' "none"} (a rate correction shifts the two rates slightly differently).
#'
#' The criterion \code{c} is \eqn{-(z(G) + z(I))/2} and \code{dr} is the
#' diagnosticity ratio G/I, both as reported by Lee and Penrod (2019).
#'
#' @return An object of class \code{"multid"}, a list with:
#' \describe{
#'   \item{indices}{Data frame of indices with estimates and, if bootstrapped,
#'     standard errors and percentile confidence limits.}
#'   \item{rates}{Named vector of the six response rates used (after
#'     correction and, if requested, per-filler scaling).}
#'   \item{counts}{Named vector of the six response counts (after any
#'     average-filler split).}
#'   \item{n_tp, n_ta}{Number of TP and TA lineups.}
#'   \item{innocent_method}{\code{"designated"} or \code{"estimated_from_fillers"}.}
#'   \item{by_confidence}{Data frame of cumulative indices by confidence
#'     threshold, or \code{NULL}.}
#'   \item{boot}{Matrix of bootstrap draws, or \code{NULL}.}
#' }
#' plus the settings used.
#'
#' @references
#' Lee, J., & Penrod, S. D. (2019). New signal detection theory-based framework
#' for eyewitness performance in lineups. \emph{Law and Human Behavior}.
#' \doi{10.1037/lhb0000343}
#'
#' Lee, J., Mansour, J. K., & Penrod, S. D. (2022). Validity of mock-witness
#' measures for assessing lineup fairness. \emph{Psychology, Crime & Law}.
#' \doi{10.1080/1068316X.2021.1905811}
#'
#' Lee, J., Mansour, J. K., & Penrod, S. D. (2024). How to measure lineup
#' fairness: Concurrent and predictive validity of lineup-fairness measures.
#' \emph{Psychology, Crime & Law}. \doi{10.1080/1068316X.2024.2307358}
#'
#' @seealso \code{\link{compare_multid}}, \code{\link{plot.multid}},
#'   \code{\link{sdt_summary_from_counts}}
#'
#' @examples
#' # Lineup column of Lee & Penrod's (2019) Table 4 (Steblay et al., 2003)
#' md <- make_multid(counts = c(G = 45, Fp = 24, Rp = 31, I = 17, Fa = 26, Ra = 57),
#'                   correction = "none")
#' md
#'
#' # From trial-level data, with bootstrap CIs and confidence breakdown
#' data(lineup_example)
#' md2 <- make_multid(lineup_example, by_confidence = TRUE,
#'                    n_bootstrap = 200, seed = 1)
#' summary(md2)
#' plot(md2)
#' @export
make_multid <- function(data = NULL,
                        counts = NULL,
                        lineup_size = 6,
                        filler_rate = c("total", "per_filler"),
                        correction = c("loglinear", "half", "none"),
                        by_confidence = FALSE,
                        conf_thresholds = NULL,
                        n_bootstrap = 0,
                        conf_level = 0.95,
                        seed = NULL,
                        innocent_suspect = "auto") {
  filler_rate <- match.arg(filler_rate)
  correction <- match.arg(correction)
  if (!is.numeric(lineup_size) || length(lineup_size) != 1L ||
      is.na(lineup_size) || lineup_size < 2 || lineup_size != round(lineup_size)) {
    stop("lineup_size must be a whole number of at least 2.", call. = FALSE)
  }
  if (!is.numeric(n_bootstrap) || length(n_bootstrap) != 1L ||
      is.na(n_bootstrap) || n_bootstrap < 0) {
    stop("n_bootstrap must be a non-negative number.", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      conf_level <= 0 || conf_level >= 1) {
    stop("conf_level must be between 0 and 1.", call. = FALSE)
  }
  if (is.null(data) == is.null(counts)) {
    stop("Supply exactly one of 'data' or 'counts'.", call. = FALSE)
  }

  if (!is.null(data)) {
    .validate_lineup_analysis(data, require_confidence = isTRUE(by_confidence), lineup_size = lineup_size)
    innocent_suspect <- .resolve_innocent_suspect(data, innocent_suspect)
    raw <- .multid_counts_from_data(data, innocent_suspect)
  } else {
    if (isTRUE(by_confidence)) {
      stop("by_confidence = TRUE requires trial-level 'data'.", call. = FALSE)
    }
    raw <- .multid_validate_counts(counts)
  }
  innocent_method <- if (is.na(raw[["I"]])) "estimated_from_fillers" else "designated"
  split <- .multid_split_innocent(raw, lineup_size)
  n_tp <- sum(raw[c("G", "Fp", "Rp")])
  n_ta <- sum(raw[c("I", "Fa", "Ra")], na.rm = TRUE)

  est <- .multid_compute(split, n_tp, n_ta, lineup_size, filler_rate, correction)

  boot_draws <- NULL
  if (n_bootstrap > 0) {
    if (any(raw != round(raw), na.rm = TRUE)) {
      stop("Bootstrapping requires integer counts.", call. = FALSE)
    }
    if (!is.null(seed)) set.seed(seed)
    boot_draws <- .multid_bootstrap(raw, n_tp, n_ta, lineup_size, filler_rate,
                                    correction, n_bootstrap)
  }

  by_conf <- NULL
  if (isTRUE(by_confidence)) {
    by_conf <- .multid_by_confidence(data, lineup_size, filler_rate,
                                     correction, conf_thresholds,
                                     innocent_suspect)
  }

  structure(
    list(
      indices = .multid_index_table(est$values, boot_draws, conf_level),
      rates = est$rates,
      counts = split,
      n_tp = n_tp,
      n_ta = n_ta,
      lineup_size = lineup_size,
      filler_rate = filler_rate,
      correction = correction,
      innocent_method = innocent_method,
      conf_level = conf_level,
      n_bootstrap = n_bootstrap,
      boot = boot_draws,
      by_confidence = by_conf
    ),
    class = "multid"
  )
}

# Index names, in display order, with short descriptions.
.multid_labels <- c(
  GI = "d'(GI): guilty vs innocent suspect",
  GFp = "d'(GFp): guilty suspect vs TP fillers",
  IFa = "d'(IFa): innocent suspect vs TA fillers (TA bias)",
  FaFp = "d'(FaFp): TA vs TP filler appeal",
  RaRp = "d'(RaRp): culprit presence vs absence",
  tp_bias = "d'(GFp) - d'(RaRp): TP lineup bias",
  siphoning = "d'(GFp) - d'(IFa): differential filler siphoning",
  fair_reference = "d'(IFa) expected for a perfectly fair lineup",
  ifa_vs_fair = "d'(IFa) minus fair reference",
  c = "Criterion c",
  dr = "Diagnosticity ratio G/I"
)

.multid_counts_from_data <- function(data, innocent_suspect = "auto") {
  tp <- as.logical(data$target_present)
  id <- as.character(data$identification)
  tally <- function(mask, what) sum(mask & id == what, na.rm = TRUE)
  ta <- data[!tp, , drop = FALSE]
  designated <- .innocent_suspect_method(ta, innocent_suspect) == "designated"
  c(
    G = tally(tp, "suspect"),
    Fp = tally(tp, "filler"),
    Rp = tally(tp, "reject"),
    I = if (designated) tally(!tp, "suspect") else NA_real_,
    Fa = tally(!tp, "filler"),
    Ra = tally(!tp, "reject")
  )
}

.multid_validate_counts <- function(counts) {
  needed <- c("G", "Fp", "Rp", "Fa", "Ra")
  if (!is.numeric(counts) || is.null(names(counts)) ||
      !all(needed %in% names(counts))) {
    stop("counts must be a named numeric vector with elements G, Fp, Rp, Fa, Ra ",
         "(and I, unless the innocent suspect is to be estimated).", call. = FALSE)
  }
  I <- if ("I" %in% names(counts)) counts[["I"]] else NA_real_
  out <- c(G = counts[["G"]], Fp = counts[["Fp"]], Rp = counts[["Rp"]],
           I = I, Fa = counts[["Fa"]], Ra = counts[["Ra"]])
  vals <- out[!is.na(out) | names(out) != "I"]
  if (anyNA(vals) || any(!is.finite(vals)) || any(vals < 0)) {
    stop("counts must be finite and non-negative.", call. = FALSE)
  }
  if (sum(out[c("G", "Fp", "Rp")]) <= 0 || sum(out[c("I", "Fa", "Ra")], na.rm = TRUE) <= 0) {
    stop("Both target-present and target-absent counts must be positive.", call. = FALSE)
  }
  out
}

# Apply the average-filler split when there is no designated innocent suspect.
.multid_split_innocent <- function(raw, lineup_size) {
  out <- raw
  if (is.na(raw[["I"]])) {
    out[["I"]] <- raw[["Fa"]] / lineup_size
    out[["Fa"]] <- raw[["Fa"]] * (lineup_size - 1) / lineup_size
  }
  out
}

.multid_compute <- function(counts, n_tp, n_ta, lineup_size, filler_rate, correction) {
  n <- c(G = n_tp, Fp = n_tp, Rp = n_tp, I = n_ta, Fa = n_ta, Ra = n_ta)
  rates <- vapply(names(n), function(nm) {
    sdt_adjust_rate(counts[[nm]], n[[nm]], correction)
  }, numeric(1))
  n_fillers <- lineup_size - 1
  if (filler_rate == "per_filler") {
    rates[c("Fp", "Fa")] <- rates[c("Fp", "Fa")] / n_fillers
  }
  z <- stats::qnorm(rates)

  # d'(IFa) for a perfectly fair TA lineup at the observed rejection rate
  p_choose <- min(max(1 - rates[["Ra"]], 1e-6), 1 - 1e-6)
  fair_fa <- if (filler_rate == "total") p_choose * n_fillers / lineup_size else p_choose / lineup_size
  fair_reference <- stats::qnorm(p_choose / lineup_size) - stats::qnorm(fair_fa)

  GFp <- z[["G"]] - z[["Fp"]]
  IFa <- z[["I"]] - z[["Fa"]]
  RaRp <- z[["Ra"]] - z[["Rp"]]
  values <- c(
    GI = z[["G"]] - z[["I"]],
    GFp = GFp,
    IFa = IFa,
    FaFp = z[["Fa"]] - z[["Fp"]],
    RaRp = RaRp,
    tp_bias = GFp - RaRp,
    siphoning = GFp - IFa,
    fair_reference = fair_reference,
    ifa_vs_fair = IFa - fair_reference,
    c = -(z[["G"]] + z[["I"]]) / 2,
    dr = rates[["G"]] / rates[["I"]]
  )
  list(values = values, rates = rates)
}

.multid_bootstrap <- function(raw, n_tp, n_ta, lineup_size, filler_rate,
                              correction, n_bootstrap) {
  tp_p <- raw[c("G", "Fp", "Rp")] / n_tp
  ta_names <- if (is.na(raw[["I"]])) c("Fa", "Ra") else c("I", "Fa", "Ra")
  ta_p <- raw[ta_names] / n_ta
  draws <- vapply(seq_len(n_bootstrap), function(b) {
    resample <- raw
    resample[c("G", "Fp", "Rp")] <- stats::rmultinom(1, n_tp, tp_p)[, 1]
    resample[ta_names] <- stats::rmultinom(1, n_ta, ta_p)[, 1]
    split <- .multid_split_innocent(resample, lineup_size)
    .multid_compute(split, n_tp, n_ta, lineup_size, filler_rate, correction)$values
  }, numeric(length(.multid_labels)))
  t(draws)
}

.multid_index_table <- function(values, boot_draws, conf_level) {
  out <- data.frame(
    index = names(values),
    description = unname(.multid_labels[names(values)]),
    estimate = unname(values),
    stringsAsFactors = FALSE
  )
  if (!is.null(boot_draws)) {
    alpha <- (1 - conf_level) / 2
    out$se <- apply(boot_draws, 2, stats::sd, na.rm = TRUE)
    out$ci_lower <- apply(boot_draws, 2, stats::quantile, probs = alpha, na.rm = TRUE)
    out$ci_upper <- apply(boot_draws, 2, stats::quantile, probs = 1 - alpha, na.rm = TRUE)
  }
  out
}

.multid_by_confidence <- function(data, lineup_size, filler_rate, correction,
                                  conf_thresholds, innocent_suspect) {
  conf <- as.numeric(data$confidence)
  if (is.null(conf_thresholds)) {
    conf_thresholds <- sort(unique(conf[!is.na(conf)]), decreasing = TRUE)
  } else {
    conf_thresholds <- sort(unique(conf_thresholds), decreasing = TRUE)
  }
  full <- .multid_counts_from_data(data, innocent_suspect)
  n_tp <- sum(full[c("G", "Fp", "Rp")])
  n_ta <- sum(full[c("I", "Fa", "Ra")], na.rm = TRUE)
  rows <- lapply(conf_thresholds, function(thr) {
    keep <- !is.na(conf) & conf >= thr
    raw <- .multid_counts_from_data(data[keep, , drop = FALSE], innocent_suspect)
    if (!is.na(full[["I"]]) && is.na(raw[["I"]])) raw[["I"]] <- 0
    split <- .multid_split_innocent(raw, lineup_size)
    est <- .multid_compute(split, n_tp, n_ta, lineup_size, filler_rate, correction)
    data.frame(
      threshold = thr,
      t(est$rates),
      t(est$values[c("GI", "GFp", "IFa", "FaFp", "RaRp", "c", "dr")]),
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

#' @export
print.multid <- function(x, digits = 2, ...) {
  cat("Multi-d' model (Lee & Penrod, 2019)\n")
  cat(sprintf("  TP lineups: %g   TA lineups: %g   Lineup size: %d\n",
              x$n_tp, x$n_ta, as.integer(x$lineup_size)))
  cat(sprintf("  Innocent suspect: %s   Filler rate: %s   Correction: %s\n\n",
              x$innocent_method, x$filler_rate, x$correction))
  cat("Response rates:\n")
  print(round(x$rates, 3))
  cat("\n")
  tab <- x$indices
  show <- tab[tab$index %in% c("GI", "GFp", "IFa", "FaFp", "RaRp", "tp_bias"), ]
  num <- vapply(show, is.numeric, logical(1))
  show[num] <- lapply(show[num], round, digits = digits)
  print(show[, setdiff(names(show), "index")], row.names = FALSE)
  est <- stats::setNames(tab$estimate, tab$index)
  cat(sprintf(
    "\nDecomposition: d'(GI) = d'(GFp) - d'(IFa) - d'(FaFp): %.*f = %.*f - (%.*f) - (%.*f)\n",
    digits, est[["GI"]], digits, est[["GFp"]], digits, est[["IFa"]],
    digits, est[["FaFp"]]
  ))
  cat(sprintf("Fair-lineup reference for d'(IFa): %.*f (observed minus reference: %.*f)\n",
              digits, est[["fair_reference"]], digits, est[["ifa_vs_fair"]]))
  invisible(x)
}

#' @export
summary.multid <- function(object, digits = 3, ...) {
  print(object, digits = digits)
  cat("\nAll indices:\n")
  tab <- object$indices
  num <- vapply(tab, is.numeric, logical(1))
  tab[num] <- lapply(tab[num], round, digits = digits)
  print(tab[, setdiff(names(tab), "description")], row.names = FALSE)
  if (!is.null(object$by_confidence)) {
    cat("\nCumulative indices by confidence (responses at or above threshold):\n")
    bc <- object$by_confidence
    bc[] <- lapply(bc, round, digits = digits)
    print(bc, row.names = FALSE)
  }
  invisible(object$indices)
}

#' Plot a Multi-d' Analysis
#'
#' @param x An object of class \code{"multid"} from \code{\link{make_multid}}.
#' @param type \code{"decomposition"} (default) shows d'(GFp), -d'(IFa) and
#'   -d'(FaFp) as a waterfall summing to d'(GI). \code{"indices"} plots each
#'   index, with bootstrap intervals when available. \code{"confidence"} plots
#'   the cumulative indices against confidence threshold (requires
#'   \code{by_confidence = TRUE}).
#' @param ... Unused.
#' @return A ggplot object.
#' @examples
#' md <- make_multid(counts = c(G = 45, Fp = 24, Rp = 31, I = 17, Fa = 26, Ra = 57))
#' plot(md)
#' plot(md, type = "indices")
#' @export
plot.multid <- function(x, type = c("decomposition", "indices", "confidence"), ...) {
  type <- match.arg(type)
  est <- stats::setNames(x$indices$estimate, x$indices$index)

  if (type == "decomposition") {
    parts <- c(est[["GFp"]], -est[["IFa"]], -est[["FaFp"]])
    end <- cumsum(parts)
    start <- c(0, utils::head(end, -1))
    df <- data.frame(
      label = factor(c("d'(GFp)", "-d'(IFa)", "-d'(FaFp)", "d'(GI)"),
                     levels = c("d'(GFp)", "-d'(IFa)", "-d'(FaFp)", "d'(GI)")),
      start = c(start, 0),
      end = c(end, est[["GI"]]),
      kind = c(ifelse(parts >= 0, "adds", "subtracts"), "total"),
      pos = 1:4
    )
    return(
      ggplot2::ggplot(df) +
        ggplot2::geom_rect(ggplot2::aes(xmin = pos - 0.4, xmax = pos + 0.4,
                                        ymin = start, ymax = end, fill = kind)) +
        ggplot2::geom_hline(yintercept = 0, colour = "grey40") +
        ggplot2::scale_x_continuous(breaks = df$pos, labels = levels(df$label)) +
        ggplot2::scale_fill_manual(values = c(adds = "steelblue", subtracts = "firebrick",
                                              total = "grey30"), name = NULL) +
        ggplot2::labs(x = NULL, y = "d'",
                      title = "Decomposition of d'(GI) (Lee & Penrod, 2019)") +
        ggplot2::theme_minimal()
    )
  }

  if (type == "indices") {
    keep <- c("GI", "GFp", "IFa", "FaFp", "RaRp", "tp_bias")
    df <- x$indices[x$indices$index %in% keep, ]
    df$label <- factor(df$index, levels = rev(keep),
                       labels = rev(c("d'(GI)", "d'(GFp)", "d'(IFa)", "d'(FaFp)",
                                      "d'(RaRp)", "d'(GFp) - d'(RaRp)")))
    p <- ggplot2::ggplot(df, ggplot2::aes(x = estimate, y = label)) +
      ggplot2::geom_vline(xintercept = 0, colour = "grey60") +
      ggplot2::geom_point(size = 2.5, colour = "steelblue")
    if (!is.null(df$ci_lower)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
                                      width = 0.2, colour = "steelblue")
    }
    return(p + ggplot2::labs(x = "d'", y = NULL, title = "Multi-d' indices") +
             ggplot2::theme_minimal())
  }

  if (is.null(x$by_confidence)) {
    stop("Refit with by_confidence = TRUE to plot by confidence.", call. = FALSE)
  }
  bc <- x$by_confidence
  long <- data.frame(
    threshold = rep(bc$threshold, 4),
    component = factor(rep(c("d'(GI)", "d'(GFp)", "-d'(IFa)", "-d'(FaFp)"), each = nrow(bc)),
                       levels = c("d'(GI)", "d'(GFp)", "-d'(IFa)", "-d'(FaFp)")),
    value = c(bc$GI, bc$GFp, -bc$IFa, -bc$FaFp)
  )
  ggplot2::ggplot(long, ggplot2::aes(x = threshold, y = value, colour = component)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60") +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_x_reverse() +
    ggplot2::labs(x = "Confidence threshold (responses at or above)", y = "d'",
                  colour = NULL, title = "Components of d'(GI) by confidence") +
    ggplot2::theme_minimal()
}

#' Compare Multi-d' Indices Between Two Conditions
#'
#' Computes the difference (\code{x} minus \code{y}) on every Multi-d' index,
#' for example simultaneous versus sequential lineups, or high- versus
#' low-similarity lineups. When both objects carry bootstrap draws, standard
#' errors, percentile intervals and normal-approximation p-values are given
#' for each difference. The two conditions are treated as independent samples:
#' the standard error is the square root of the sum of the two bootstrap
#' variances, and the interval is taken from differences between randomly
#' re-paired draws, so the result does not depend on whether the two fits used
#' the same seed.
#'
#' @param x,y Objects of class \code{"multid"} from \code{\link{make_multid}},
#'   fitted with the same \code{filler_rate}.
#' @param labels Names for the two conditions.
#' @param seed Optional random seed for re-pairing the bootstrap draws.
#' @return A data frame of class \code{"multid_comparison"}.
#' @examples
#' # Simultaneous vs sequential lineups, Lee & Penrod's (2019) Table 3
#' sim <- make_multid(counts = c(G = 52, Fp = 25, Rp = 24, I = 28, Fa = 26, Ra = 46),
#'                    correction = "none")
#' sqn <- make_multid(counts = c(G = 44, Fp = 19, Rp = 39, I = 15, Fa = 17, Ra = 68),
#'                    correction = "none")
#' compare_multid(sim, sqn, labels = c("simultaneous", "sequential"))
#' @export
compare_multid <- function(x, y, labels = c("x", "y"), seed = NULL) {
  if (!inherits(x, "multid") || !inherits(y, "multid")) {
    stop("x and y must be objects from make_multid().", call. = FALSE)
  }
  if (x$filler_rate != y$filler_rate) {
    stop("x and y must use the same filler_rate.", call. = FALSE)
  }
  keep <- c("GI", "GFp", "IFa", "FaFp", "RaRp", "tp_bias", "siphoning", "ifa_vs_fair")
  ex <- stats::setNames(x$indices$estimate, x$indices$index)[keep]
  ey <- stats::setNames(y$indices$estimate, y$indices$index)[keep]
  out <- data.frame(
    index = keep,
    description = unname(.multid_labels[keep]),
    x = unname(ex),
    y = unname(ey),
    difference = unname(ex - ey),
    stringsAsFactors = FALSE
  )
  names(out)[3:4] <- labels
  if (!is.null(x$boot) && !is.null(y$boot)) {
    # The conditions are independent samples, but draws from fits that shared
    # a seed are correlated row by row, so re-pair y's draws at random.
    restore_rng <- .local_seed(seed)
    on.exit(restore_rng(), add = TRUE)
    b <- min(nrow(x$boot), nrow(y$boot))
    xb <- x$boot[seq_len(b), keep, drop = FALSE]
    yb <- y$boot[sample.int(nrow(y$boot), b), keep, drop = FALSE]
    diffs <- xb - yb
    alpha <- (1 - x$conf_level) / 2
    out$se <- sqrt(apply(x$boot[, keep, drop = FALSE], 2, stats::var, na.rm = TRUE) +
                     apply(y$boot[, keep, drop = FALSE], 2, stats::var, na.rm = TRUE))
    out$ci_lower <- apply(diffs, 2, stats::quantile, probs = alpha, na.rm = TRUE)
    out$ci_upper <- apply(diffs, 2, stats::quantile, probs = 1 - alpha, na.rm = TRUE)
    out$p_value <- 2 * stats::pnorm(-abs(out$difference / out$se))
  }
  attr(out, "labels") <- labels
  class(out) <- c("multid_comparison", "data.frame")
  out
}

#' @export
print.multid_comparison <- function(x, digits = 3, ...) {
  labels <- attr(x, "labels")
  cat(sprintf("Multi-d' comparison: %s minus %s\n\n", labels[1], labels[2]))
  df <- as.data.frame(unclass(x), stringsAsFactors = FALSE)
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], round, digits = digits)
  print(df[, setdiff(names(df), "description")], row.names = FALSE)
  invisible(x)
}
