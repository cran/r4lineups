#' @section Authorship and acknowledgements:
#' Colin Tredoux and Tamsyn Naylor are the key authors of `r4lineups` and were
#' jointly responsible for the original package's design and development.
#'
#' Development used multiple dynamically updated frontier models: Anthropic
#' Claude Sonnet 4 and 5 and Claude Opus 4.6 and 4.8, and OpenAI Codex with
#' GPT-5-family models including GPT-5.5 and GPT-5.6 Sol. Additional Claude and
#' Codex sessions were used, but exact session-level model identifiers were not
#' consistently recorded. These tools assisted with code review and drafting,
#' debugging, test scaffolding, documentation, package-check remediation, and
#' manuscript editing. Colin Tredoux and Tamsyn Naylor reviewed, modified, and
#' validated the AI-assisted outputs, made the statistical and software-design
#' decisions, and retain full responsibility for accuracy, originality,
#' licensing, and scholarly claims.
#'
#' The package implements computational approaches developed by many
#' researchers. Their publications are cited in the documentation for the
#' relevant functions and vignettes, and the package authors gratefully
#' acknowledge this foundational work. Any error in translating a published
#' method into code is an error in this package, not in the cited work. Please
#' report suspected errors through the
#' \href{https://github.com/CGTZA2/r4lineups/issues}{GitHub issue tracker}.
#'
#' @importFrom stats complete.cases
#' @keywords internal
"_PACKAGE"

# Declare ggplot2 NSE variables
utils::globalVariables(c(
  "parameter",
  "estimate",
  "lower",
  "upper",
  "outcome",
  "count",
  "type",
  "value",
  "original"
))
