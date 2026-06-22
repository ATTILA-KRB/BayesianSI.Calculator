#' Load and Inspect Data
#'
#' This function attempts to load data using various methods and performs data quality checks.
#' It tries different reading methods in order of efficiency and flexibility.
#'
#' @param file_path Character string specifying the path to the data file.
#' @param data Optional data frame. If provided, only inspection is performed.
#' @param silent Logical, if TRUE suppresses messages (but not warnings). Default is FALSE.
#'
#' @return A data frame containing the loaded data.
#'
#' @importFrom data.table fread
#' @importFrom readr read_csv
#' @importFrom utils read.csv read.table
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # From a file
#' data <- load_n_inspect_data("path/to/your/data.csv")
#'
#' # From existing data frame
#' df <- data.frame(x = 1:10, y = letters[1:10])
#' data <- load_n_inspect_data(data = df)
#' }
load_n_inspect_data <- function(file_path = NULL, data = NULL, silent = FALSE) {
  # Input validation
  if (is.null(file_path) && is.null(data)) {
    stop("Either file_path or data must be provided")
  }

  if (!is.null(file_path) && !is.null(data)) {
    warning("Both file_path and data provided. Using data and ignoring file_path.")
    return(inspect_loaded_data(data))
  }

  # If data is provided directly, only perform inspection
  if (!is.null(data)) {
    return(inspect_loaded_data(data))
  }

  # Check if file exists
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  # Try different loading methods
  loaded_data <- try_loading_methods(file_path, silent)

  # Inspect the loaded data
  inspect_loaded_data(loaded_data)

  return(loaded_data)
}

#' Try Different Loading Methods
#'
#' @param file_path Path to the data file
#' @param silent Logical, whether to suppress messages
#'
#' @return A data frame containing the loaded data
#'
#' @keywords internal
try_loading_methods <- function(file_path, silent) {
  # Initialize result
  loaded_data <- NULL
  success <- FALSE

  # 1. Try fread first (fastest)
  if (!success) {
    tryCatch({
      if (!silent) message("Attempting to load with data.table::fread...")
      loaded_data <- data.table::fread(file_path, data.table = FALSE)
      success <- TRUE
      if (!silent) message("Successfully loaded with fread")
    }, error = function(e) {
      if (!silent) message("fread failed: ", e$message)
    })
  }

  # 2. Try readr::read_csv
  if (!success) {
    tryCatch({
      if (!silent) message("Attempting to load with readr::read_csv...")
      loaded_data <- readr::read_csv(file_path, show_col_types = FALSE)
      loaded_data <- as.data.frame(loaded_data)  # Convert to standard data.frame
      success <- TRUE
      if (!silent) message("Successfully loaded with read_csv")
    }, error = function(e) {
      if (!silent) message("read_csv failed: ", e$message)
    })
  }

  # 3. Try base R read.csv
  if (!success) {
    tryCatch({
      if (!silent) message("Attempting to load with base::read.csv...")
      loaded_data <- read.csv(file_path, stringsAsFactors = FALSE)
      success <- TRUE
      if (!silent) message("Successfully loaded with read.csv")
    }, error = function(e) {
      if (!silent) message("read.csv failed: ", e$message)
    })
  }

  # 4. Try read.table with tab separator
  if (!success) {
    tryCatch({
      if (!silent) message("Attempting to load with read.table (tab separator)...")
      loaded_data <- read.table(file_path, header = TRUE, sep = "\t",
                                stringsAsFactors = FALSE)
      success <- TRUE
      if (!silent) message("Successfully loaded with read.table (tab separator)")
    }, error = function(e) {
      if (!silent) message("read.table with tab separator failed: ", e$message)
    })
  }

  # 5. Try read.table with various separators
  if (!success) {
    separators <- c(",", ";", "|", "/")
    for (sep in separators) {
      tryCatch({
        if (!silent) message("Attempting to load with read.table (separator: ", sep, ")...")
        loaded_data <- read.table(file_path, header = TRUE, sep = sep,
                                  stringsAsFactors = FALSE)
        success <- TRUE
        if (!silent) message("Successfully loaded with read.table (separator: ", sep, ")")
        break
      }, error = function(e) {
        if (!silent) message("read.table with separator ", sep, " failed: ", e$message)
      })
    }
  }

  # If all methods failed, stop with error
  if (!success) {
    stop("All loading methods failed. Please check the file format and try again.")
  }

  return(loaded_data)
}

#' Inspect Data for Quality Issues
#'
#' Performs a set of non-destructive data quality checks on a data frame (or
#' data.table) and returns the findings as a named list of human-readable
#' character messages. This function performs no I/O and emits no conditions
#' (no \code{warning()} / \code{message()} calls); it simply returns its
#' findings so callers can decide how to surface them.
#'
#' The following checks are performed:
#' \itemize{
#'   \item list-type columns (\code{list_warnings})
#'   \item columns containing NA/NaN values (\code{na_warnings})
#'   \item character columns mixing numeric and string values
#'     (\code{mixed_type_warnings})
#'   \item numeric columns containing only integer values (\code{integer_warnings})
#'   \item numeric columns containing only zero values (\code{zero_warnings})
#' }
#'
#' @param data A data frame or \code{data.table} to inspect.
#'
#' @return A named \code{list} of character vectors describing any issues
#'   found. Possible names are \code{list_warnings}, \code{na_warnings},
#'   \code{mixed_type_warnings}, \code{integer_warnings} and
#'   \code{zero_warnings}; each entry holds one or more \code{sprintf}-formatted
#'   messages. If inspection fails, the list contains a single \code{warning}
#'   element with the error message. An empty list is returned when no issues
#'   are detected.
#'
#' @export
#'
#' @examples
#' # Clean data returns an empty list
#' inspect_data(data.frame(x = c(1.5, 2.5), y = c(3.5, 4.5)))
#'
#' # NA values are reported under na_warnings
#' inspect_data(data.frame(x = c(1, NA, 3), y = 1:3))
inspect_data <- function(data) {
  results <- list()

  tryCatch({
    # Convert to data.frame temporarily to avoid data.table subsetting issues
    is_dt <- data.table::is.data.table(data)
    if (is_dt) {
      data_df <- as.data.frame(data)
    } else {
      data_df <- data
    }

    # Check column types
    col_types <- sapply(data_df, class)
    list_cols <- names(which(sapply(col_types, function(x) "list" %in% x)))

    if (length(list_cols) > 0) {
      results$list_warnings <- sprintf("Column '%s' contains list data which may not be suitable for analysis.", list_cols)
    }

    # Check for NA and NaN values
    na_counts <- sapply(data_df, function(col) {
      if (is.list(col)) {
        return(0)  # Skip list columns
      } else {
        return(sum(is.na(col) | is.nan(col)))
      }
    })

    if (sum(na_counts) > 0) {
      results$na_warnings <- sapply(names(na_counts[na_counts > 0]), function(col) {
        sprintf("Column '%s' contains %d NA/NaN values.", col, na_counts[col])
      })
    }

    # Check for columns with both numeric and string data
    mixed_type_cols <- sapply(data_df, function(col) {
      if (is.list(col)) {
        return(FALSE)  # Skip list columns
      } else if (is.character(col)) {
        return(any(grepl("^\\s*-?\\d*\\.?\\d+\\s*$", col)) &&
                 !all(grepl("^\\s*-?\\d*\\.?\\d+\\s*$", col)))
      } else {
        return(FALSE)
      }
    })

    if (any(mixed_type_cols)) {
      results$mixed_type_warnings <- sprintf("Column '%s' contains both numeric and string data.",
                                             names(which(mixed_type_cols)))
    }

    # Check for columns with only integers
    integer_cols <- sapply(data_df, function(col) {
      if (is.list(col) || !is.numeric(col)) {
        return(FALSE)  # Skip list columns and non-numeric columns
      } else {
        return(all(col == floor(col), na.rm = TRUE))
      }
    })

    if (any(integer_cols)) {
      results$integer_warnings <- sprintf("Column '%s' contains only integer values.",
                                          names(which(integer_cols)))
    }

    # Check for columns with only zeros
    zero_cols <- sapply(data_df, function(col) {
      if (is.list(col) || !is.numeric(col)) {
        return(FALSE)  # Skip list columns and non-numeric columns
      } else {
        return(all(col == 0, na.rm = TRUE))
      }
    })

    if (any(zero_cols)) {
      results$zero_warnings <- sprintf("Column '%s' contains only zero values.",
                                       names(which(zero_cols)))
    }

  }, error = function(e) {
    results$warning <- paste("Error during data inspection:", e$message)
  })

  return(results)
}

#' Inspect Loaded Data
#'
#' @param data A data frame to inspect
#'
#' @return The input data frame, invisibly
#'
#' @keywords internal
inspect_loaded_data <- function(data) {
  # Check for empty data. These checks are intentionally kept here (and not in
  # inspect_data()) to preserve existing behaviour for both the package and the
  # Shiny app.
  if (nrow(data) == 0) {
    warning("The data has 0 rows")
  }
  if (ncol(data) == 0) {
    warning("The data has 0 columns")
  }

  # Delegate the detection logic to inspect_data(), then translate its named
  # list of findings back into the warning()/message() calls this function has
  # historically emitted, preserving the exact existing text.
  inspection <- inspect_data(data)

  # List-type columns -> warning
  list_cols <- names(which(sapply(sapply(data, class),
                                  function(x) "list" %in% x)))
  if (length(list_cols) > 0) {
    warning("The following columns contain list data which may not be suitable for analysis: ",
            paste(list_cols, collapse = ", "))
  }

  # NA / NaN values -> warning
  na_counts <- sapply(data, function(col) {
    if (is.list(col)) return(0)
    sum(is.na(col) | is.nan(col))
  })
  cols_with_na <- names(na_counts[na_counts > 0])
  if (length(cols_with_na) > 0) {
    warning("The following columns contain NA/NaN values: \n",
            paste(" - ", cols_with_na, ": ", na_counts[cols_with_na], " missing values",
                  collapse = "\n"))
  }

  # Mixed numeric/string character columns -> warning
  mixed_type_cols <- sapply(data, function(col) {
    if (is.list(col)) return(FALSE)
    if (is.character(col)) {
      return(any(grepl("^\\s*-?\\d*\\.?\\d+\\s*$", col)) &&
               !all(grepl("^\\s*-?\\d*\\.?\\d+\\s*$", col)))
    }
    return(FALSE)
  })
  if (any(mixed_type_cols)) {
    warning("The following columns contain both numeric and string data: ",
            paste(names(which(mixed_type_cols)), collapse = ", "))
  }

  # Integer-only columns -> message
  integer_cols <- sapply(data, function(col) {
    if (is.list(col) || !is.numeric(col)) return(FALSE)
    all(col == floor(col), na.rm = TRUE)
  })
  if (any(integer_cols)) {
    message("The following columns contain only integer values: ",
            paste(names(which(integer_cols)), collapse = ", "))
  }

  # Zero-only columns -> warning
  zero_cols <- sapply(data, function(col) {
    if (is.list(col) || !is.numeric(col)) return(FALSE)
    all(col == 0, na.rm = TRUE)
  })
  if (any(zero_cols)) {
    warning("The following columns contain only zero values: ",
            paste(names(which(zero_cols)), collapse = ", "))
  }

  # Return the data invisibly
  invisible(data)
}
