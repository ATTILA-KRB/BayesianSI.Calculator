#' Build Interval Header Strings
#'
#' Internal helper that constructs the CI / PI / TI header label strings used by
#' \code{format_intervals_table()}, optionally prefixed with a sidedness label.
#'
#' @param ci_percent Numeric/character CI percentage.
#' @param pi_percent Numeric/character PI percentage.
#' @param ti_percent Numeric/character TI percentage.
#' @param conf_percent Numeric/character TI credibility/confidence percentage.
#' @param side Optional logical indicating sidedness. \code{NULL} adds no prefix,
#'   \code{TRUE} prefixes with "One-Sided ", \code{FALSE} prefixes with
#'   "Two-Sided ".
#'
#' @return A named list with character elements \code{ci}, \code{pi} and
#'   \code{ti}.
#' @keywords internal
#' @noRd
build_interval_headers <- function(ci_percent, pi_percent, ti_percent,
                                   conf_percent, side = NULL) {
  prefix <- if (is.null(side)) {
    ""
  } else if (isTRUE(side)) {
    "One-Sided "
  } else {
    "Two-Sided "
  }

  list(
    ci = sprintf("%s%s%% CI", prefix, ci_percent),
    pi = sprintf("%s%s%% PI", prefix, pi_percent),
    ti = sprintf("%s%s%% TI (%s%% Credibility)", prefix, ti_percent, conf_percent)
  )
}

#' Format Intervals Table with Double Header
#'
#' Creates a formatted DT table with two header levels showing statistical intervals
#' in the same format as the Shiny app.
#'
#' @param result Output list from perform_calculations() containing data and summary
#' @param show_reducible Logical, whether to include reducible uncertainty columns (default = FALSE)
#' @param ci_percent Optional CI percentage. When \code{NULL} the value is parsed
#'   from \code{result$summary}.
#' @param pi_percent Optional PI percentage. When \code{NULL} the value is parsed
#'   from \code{result$summary}.
#' @param ti_percent Optional TI percentage. When \code{NULL} the value is parsed
#'   from \code{result$summary}.
#' @param conf_percent Optional TI credibility/confidence percentage. When
#'   \code{NULL} the value is parsed from \code{result$summary}.
#' @param is_one_sided Optional logical controlling header sidedness labels.
#'   \code{NULL} (default) adds no prefix; \code{TRUE} adds "One-Sided ";
#'   \code{FALSE} adds "Two-Sided ". When \code{FALSE} and \code{include_pi} is
#'   not explicitly set, the prediction-interval (PI) columns are dropped.
#' @param by_label Character label for the grouping ("By") column header
#'   (default = "By Identifier").
#' @param include_pi Optional logical controlling whether the PI columns are
#'   shown. When \code{NULL} it is derived: explicit two-sided
#'   (\code{is_one_sided == FALSE}) drops PI, otherwise PI is included.
#'
#' @return A DT::datatable object with formatted headers showing interval percentages
#' @import DT
#' @export
format_intervals_table <- function(result, show_reducible = FALSE,
                                   ci_percent = NULL, pi_percent = NULL,
                                   ti_percent = NULL, conf_percent = NULL,
                                   is_one_sided = NULL,
                                   by_label = "By Identifier",
                                   include_pi = NULL) {

  # Extract data
  data <- result$data
  summary_text <- result$summary

  # Extract percentages from summary text using regex when not supplied.
  if (is.null(ci_percent)) {
    ci_percent <- as.numeric(regmatches(summary_text, regexpr("\\d+(?=% credibility)", summary_text, perl = TRUE)))
  }
  if (is.null(pi_percent)) {
    pi_percent <- as.numeric(regmatches(summary_text, regexpr("\\d+(?=% prediction)", summary_text, perl = TRUE)))
  }
  if (is.null(ti_percent)) {
    ti_percent <- as.numeric(regmatches(summary_text, regexpr("\\d+(?=% tolerance)", summary_text, perl = TRUE)))
  }
  if (is.null(conf_percent)) {
    conf_percent <- as.numeric(regmatches(summary_text, regexpr("\\d+(?=% confidence)", summary_text, perl = TRUE)))
  }

  # Derive whether PI columns are included.
  if (is.null(include_pi)) {
    include_pi <- !(identical(is_one_sided, FALSE))
  }

  # Check if single chain
  is_single_chain <- length(unique(data$By)) == 1 &&
    (unique(data$By) == "SingleChain" || unique(data$By) == 1 || identical(unique(data$By), ""))

  # Prepare display data
  display_data <- data
  if (is_single_chain) {
    display_data$By <- "" # Empty the By column for display purposes
  }

  # Build header label strings (parameterized by sidedness).
  headers <- build_interval_headers(ci_percent, pi_percent, ti_percent,
                                    conf_percent, side = is_one_sided)

  # Assemble the top-level interval header names, their colspans and the
  # corresponding lower/upper sub-headers, depending on PI inclusion and the
  # reducible columns.
  display_columns <- c("By", "Median", "CI_Lower", "CI_Upper")
  interval_headers <- c(headers$ci)
  interval_colspans <- c(2)

  if (include_pi) {
    display_columns <- c(display_columns, "PI_Lower", "PI_Upper")
    interval_headers <- c(interval_headers, headers$pi)
    interval_colspans <- c(interval_colspans, 2)
  }

  display_columns <- c(display_columns, "TI_Lower", "TI_Upper")
  interval_headers <- c(interval_headers, headers$ti)
  interval_colspans <- c(interval_colspans, 2)

  if (show_reducible) {
    display_columns <- c(display_columns, "ReducibleLower", "ReducibleUpper")
    interval_headers <- c(interval_headers, "Reducible Imprecision (%)")
    interval_colspans <- c(interval_colspans, 2)
  }

  # Top-level header row: By, Point Estimate (each spanning both rows), then the
  # interval groups.
  top_interval_ths <- do.call(
    shiny::tagList,
    mapply(
      function(name, colspan) shiny::tags$th(colspan = colspan, name),
      interval_headers,
      interval_colspans,
      SIMPLIFY = FALSE
    )
  )

  # Second header row: a Lower/Upper pair per interval group.
  sub_ths <- do.call(
    shiny::tagList,
    unlist(
      lapply(seq_along(interval_headers), function(i) {
        list(shiny::tags$th("Lower"), shiny::tags$th("Upper"))
      }),
      recursive = FALSE
    )
  )

  container <- shiny::tags$table(
    class = 'display',
    shiny::tags$thead(
      shiny::tags$tr(
        shiny::tags$th(rowspan = 2, if (is_single_chain) "" else by_label),
        shiny::tags$th(rowspan = 2, "Point Estimate"),
        top_interval_ths
      ),
      shiny::tags$tr(sub_ths)
    )
  )

  # Create the DT table
  dt <- DT::datatable(
    display_data[, display_columns],
    options = list(
      pageLength = 50,
      dom = 't',
      scrollX = TRUE,
      scrollY = "400px",
      fixedHeader = TRUE,
      scrollCollapse = TRUE,
      paging = FALSE,
      ordering = TRUE,
      order = list(),
      columnDefs = list(
        list(className = 'dt-center', targets = '_all'),
        list(className = 'dt-left', targets = 0)
      )
    ),
    style = 'bootstrap4',
    container = container,
    rownames = FALSE
  ) %>%
    DT::formatRound(columns = setdiff(display_columns, "By"), digits = 4)

  return(dt)
}
