#' Convert Dataframe Numerical Columns to Time Series Format
#'This function converts the numerical columns of a dataframe into time series objects and combines them into a new dataframe.
#' In addition, a date variable is created based on the specified frequency and start time, which is then appended to the output.
#' @param df A dataframe containing data. Non-numeric columns are ignored.
#' @param frequency An integer indicating the number of observations per unit time. Use:
#' \itemize{
#'     \item \code{1} for yearly data,
#'     \item \code{4} for quarterly data,
#'     \item \code{12} for monthly data.
#'   }
#'   The default is \code{1}
#' @param start A numeric vector of length 2 indicating the start time of the series (e.g., \code{c(1, 1)} for the first year).
#'   For monthly or quarterly data, the second element represents the starting period.
#'
#' @returns A dataframe in which each numerical column from \code{df} has been converted to a time series object. An additional
#'   \code{time} column is added that contains date values (of class \code{Date}) corresponding to each observation.
#' @export
#'@details
#' The function first checks that the input is a dataframe and that it contains at least one numerical column.
#' It then converts each numerical column into a time series object using the base R \code{\link{ts}} function.
#' A time variable is created in decimal format and is converted into a proper date format according to the frequency:
#' \itemize{
#'   \item For quarterly data (\code{frequency = 4}), the month is determined by \code{(quarter - 1) * 3 + 1}.
#'   \item For monthly data (\code{frequency = 12}), the day is set to the first day of the month.
#'   \item For yearly data (\code{frequency = 1}), the date is set to January 1st of the given year.
#' }
#' @examples
#'  \dontrun{
#' # Example with monthly data:
#' df <- data.frame(
#'   sales = rnorm(24, mean = 100, sd = 10),
#'   profit = rnorm(24, mean = 20, sd = 5),
#'   category = letters[1:24]
#' )
#'
#' # Convert numerical columns to a monthly time series starting in January 2020
#' ts_df <- tsset(df, frequency = 12, start = c(2020, 1))
#' head(ts_df)
#' }
#' #' @export
tsset <- function(df, frequency = 1, start = c(1, 1)) {
  # Ensure the input is a dataframe
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe.")
  }

  # Extract numerical columns
  numerical_cols <- sapply(df, is.numeric)

  # Check if there are any numerical columns
  if (sum(numerical_cols) == 0) {
    stop("No numerical variables found in the dataframe.")
  }

  # Convert numerical columns to time series
  ts_list <- lapply(df[numerical_cols], function(x) ts(x, frequency = frequency, start = start))

  # Combine the time series into a dataframe
  ts_df <- as.data.frame(ts_list)

  # Create a time variable in decimal format
  time_var <- time(ts(1:nrow(df), frequency = frequency, start = start))

  # Convert decimal time to proper date format
  if (frequency == 4) {
    # Quarterly data
    dates <- sapply(time_var, function(t) {
      year <- floor(t)
      quarter <- (t - year) * 4 + 1
      month <- (quarter - 1) * 3 + 1
      as.Date(paste(year, month, "01", sep = "-"))
    })
  } else if (frequency == 12) {
    # Monthly data
    dates <- sapply(time_var, function(t) {
      year <- floor(t)
      month <- round((t - year) * 12) + 1
      as.Date(paste(year, month, "01", sep = "-"))
    })
  } else if (frequency == 1) {
    # Yearly data
    dates <- sapply(time_var, function(t) {
      year <- floor(t)
      as.Date(paste(year, "01", "01", sep = "-"))
    })
  } else {
    stop("Unsupported frequency. Please use 1 (yearly), 4 (quarterly), or 12 (monthly).")
  }

  # Add the date variable to the dataframe
  ts_df$time <- as.Date(dates, origin = "1970-01-01")

  return(ts_df)
}



#' Time Series Plot with ggplot2 and ggthemes
#'
#' This function creates a time series line plot from a data frame using
#' ggplot2. It expects a time column and one or more numeric variables, reshapes the
#' data into long format, and then plots the data with a line for each variable.
#' The user can choose from several themes available in ggthemes or ggplot2.
#'
#' @param data A data frame containing the time series data.
#' @param time_col A string indicating the name of the time column in \code{data}.
#' @param vars A character vector of column names in \code{data} representing the variables to plot.
#' @param theme_choice A string specifying the theme to apply. Options include:
#'   \itemize{
#'     \item \code{"economist"}: Applies the Economist theme from ggthemes.
#'     \item \code{"fivethirtyeight"}: Applies the FiveThirtyEight theme from ggthemes.
#'     \item \code{"minimal"}: Applies the minimal theme from ggplot2.
#'   }
#'   The default is to match one of these choices.
#' @param title A string for the title of the plot. Defaults to "Time Series Plot".
#'
#' @return A \code{ggplot} object representing the time series line plot.
#'
#' @details
#' The function first checks that the required packages (\code{ggplot2}, \code{ggthemes},
#' \code{tidyr}, and \code{dplyr}) are installed. It then ensures that the time column is
#' in a proper date/time format (either \code{POSIXct} or \code{Date}). The data is subset
#' to include only the specified time and variable columns and reshaped to long format using
#' \code{tidyr::pivot_longer}. A ggplot line plot is then created with the specified theme.
#'
#'
#' @examples
#' \dontrun{
#' # Create example data
#' set.seed(123)
#' dates <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "month")
#' data <- data.frame(
#'   date = dates,
#'   variable1 = cumsum(rnorm(length(dates))),
#'   variable2 = cumsum(rnorm(length(dates)))
#' )
#'
#' # Create a time series plot using the economist theme
#' plot <- tsplot1(
#'   data,
#'   time_col = "date",
#'   vars = c("variable1", "variable2"),
#'   theme_choice = "economist",
#'   title = "Sample Time Series Plot"
#' )
#' print(plot)
#' }
#'
#' @export
tsplot1 <- function(data, time_col, vars,
                    theme_choice = c("economist", "fivethirtyeight", "minimal"),
                    title = "Time Series Plot") {
  # Check that required packages are available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  if (!requireNamespace("ggthemes", quietly = TRUE)) {
    stop("Package 'ggthemes' is required but not installed.")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }

  # Match the theme_choice argument
  theme_choice <- match.arg(theme_choice)

  # Ensure the time column is in POSIXct or Date format
  if (!inherits(data[[time_col]], "POSIXct") && !inherits(data[[time_col]], "Date")) {
    data[[time_col]] <- as.POSIXct(data[[time_col]])
  }

  # Select only the necessary columns and reshape to long format
  data_long <- dplyr::select(data, dplyr::all_of(c(time_col, vars))) %>%
    tidyr::pivot_longer(
      cols = tidyr::all_of(vars),
      names_to = "variable",
      values_to = "value"
    )

  # Create the base plot
  p <- ggplot2::ggplot(data_long, ggplot2::aes_string(x = time_col, y = "value", color = "variable")) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(title = title, x = time_col, y = "Value", color = "Variable") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # Apply the selected theme from ggthemes or ggplot2
  if (theme_choice == "economist") {
    p <- p + ggthemes::theme_economist() + ggthemes::scale_color_economist()
  } else if (theme_choice == "fivethirtyeight") {
    p <- p + ggthemes::theme_fivethirtyeight()
  } else if (theme_choice == "minimal") {
    p <- p + ggplot2::theme_minimal()
  }

  return(p)
}


#' Create a Time Series Plot with the Stata Theme
#'
#' This function creates a time series line plot from a data frame using ggplot2.
#' It expects a time column and one or more numeric variables, reshapes the data into
#' long format, and then plots the data with a line for each variable. The Stata theme
#' and color scale from ggthemes are applied to the plot.
#'
#' @param data A data frame containing the time series data.
#' @param time_col A string indicating the name of the time column in \code{data}.
#' @param vars A character vector of column names in \code{data} representing the variables to plot.
#' @param title A string for the title of the plot. Defaults to "Time Series Plot".
#'
#' @return A \code{ggplot} object representing the time series line plot.
#'
#' @details
#' The function first checks that the required packages (\code{ggplot2}, \code{ggthemes},
#' \code{tidyr}, and \code{dplyr}) are installed. It then ensures that the time column is in a proper date
#' or date-time format (either \code{Date} or \code{POSIXct}); if not, it converts the column using \code{as.POSIXct}.
#' Next, it selects the time column and the specified variables, reshaping the data into long format using
#' \code{tidyr::pivot_longer}. Finally, it builds the time series plot using \code{ggplot2::geom_line()} and applies
#' the Stata theme from \code{ggthemes}.
#'
#' @examples
#' \dontrun{
#' # Create example data
#' set.seed(123)
#' dates <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "month")
#' data <- data.frame(
#'   date = dates,
#'   sales = cumsum(rnorm(length(dates))),
#'   profit = cumsum(rnorm(length(dates)))
#' )
#'
#' # Create a time series plot using the Stata theme
#' p <- plot_time_series1(
#'   data,
#'   time_col = "date",
#'   vars = c("sales", "profit"),
#'   title = "Monthly Sales and Profit"
#' )
#' print(p)
#' }
#'
#' @export
tsplot2 <- function(data, time_col, vars, title = "Time Series Plot") {
  # Check that required packages are available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  if (!requireNamespace("ggthemes", quietly = TRUE)) {
    stop("Package 'ggthemes' is required but not installed.")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }

  # Ensure the time column is in Date or POSIXct format; convert if necessary.
  if (!inherits(data[[time_col]], "Date") && !inherits(data[[time_col]], "POSIXct")) {
    data[[time_col]] <- as.POSIXct(data[[time_col]])
  }

  # Select only the time column and specified variables, then reshape into long format.
  data_long <- dplyr::select(data, dplyr::all_of(c(time_col, vars))) %>%
    tidyr::pivot_longer(
      cols = tidyr::all_of(vars),
      names_to = "variable",
      values_to = "value"
    )

  # Build the time series plot using ggplot2 and apply the Stata theme from ggthemes.
  p <- ggplot2::ggplot(data_long, ggplot2::aes_string(x = time_col, y = "value", color = "variable")) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(
      title = title,
      x = time_col,
      y = "Value",
      color = "Variable"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggthemes::theme_stata() +
    ggthemes::scale_colour_stata()

  return(p)
}




#' Create a Faceted Time Series Plot with the Stata Theme
#'
#' This function creates a faceted time series line plot from a data frame using ggplot2.
#' It expects a time column and one or more numeric variables, reshapes the data into long format,
#' and then plots each variable in its own facet using \code{ggplot2::facet_wrap()}. The built-in
#' Stata theme and corresponding color scale from ggthemes are applied.
#'
#' @param data A data frame containing the time series data.
#' @param time_col A string indicating the name of the time column in \code{data}.
#' @param vars A character vector of column names in \code{data} representing the variables to plot.
#' @param title A string for the title of the plot. Defaults to "Time Series Plot".
#'
#' @return A \code{ggplot} object representing the faceted time series plot.
#'
#' @details
#' The function first checks that the required packages (\code{ggplot2}, \code{ggthemes}, \code{tidyr},
#' and \code{dplyr}) are installed. It ensures that the time column is in a proper date or date-time format
#' (either \code{Date} or \code{POSIXct}); if not, it converts the column using \code{as.POSIXct}.
#' Next, it selects the time column and specified variables, reshaping the data into long format using
#' \code{tidyr::pivot_longer}. Finally, it builds the faceted time series plot with separate facets for each
#' variable using \code{ggplot2::facet_wrap()} and applies the Stata theme and color scale from \code{ggthemes}.
#'
#' @examples
#' \dontrun{
#' # Create example data
#' set.seed(123)
#' dates <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "month")
#' data <- data.frame(
#'   date = dates,
#'   sales = cumsum(rnorm(length(dates))),
#'   profit = cumsum(rnorm(length(dates)))
#' )
#'
#' # Create a faceted time series plot using the Stata theme
#' p <- plot_time_series2(
#'   data,
#'   time_col = "date",
#'   vars = c("sales", "profit"),
#'   title = "Monthly Sales and Profit"
#' )
#' print(p)
#' }
#'
#' @export
tsplot3 <- function(data, time_col, vars, title = "Time Series Plot") {
  # Check that required packages are available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  if (!requireNamespace("ggthemes", quietly = TRUE)) {
    stop("Package 'ggthemes' is required but not installed.")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }

  # Ensure the time column is in Date or POSIXct format; convert if necessary.
  if (!inherits(data[[time_col]], "Date") && !inherits(data[[time_col]], "POSIXct")) {
    data[[time_col]] <- as.POSIXct(data[[time_col]])
  }

  # Select the time column and the specified variables, then reshape to long format.
  data_long <- dplyr::select(data, dplyr::all_of(c(time_col, vars))) %>%
    tidyr::pivot_longer(
      cols = tidyr::all_of(vars),
      names_to = "variable",
      values_to = "value"
    )

  # Build the time series plot with separate facets for each variable
  p <- ggplot2::ggplot(data_long, ggplot2::aes_string(x = time_col, y = "value", color = "variable")) +
    ggplot2::geom_line(size = 1) +
    ggplot2::facet_wrap(~ variable, scales = "free_y") +
    ggplot2::labs(
      title = title,
      x = time_col,
      y = "Value"
    ) +
    # Apply the built-in Stata theme and corresponding color scale from ggthemes
    ggthemes::theme_stata() +
    ggthemes::scale_colour_stata()

  return(p)
}


#' Plot Autocorrelation Function with the Stata Theme
#'
#' This function computes and plots the autocorrelation function (ACF) for one or more numeric variables
#' from a data frame. For each variable specified in \code{vars}, the ACF is calculated using \code{stats::acf}
#' (with \code{plot = FALSE}) and the results are combined into a single data frame. The ACF values are then plotted
#' using \code{ggplot2} with each variable shown in its own facet. The plot is styled using the Stata theme from
#' \code{ggthemes}.
#'
#' @param data A data frame containing the numeric variables.
#' @param vars A character vector of column names in \code{data} for which the ACF should be computed.
#' @param lag.max An integer specifying the maximum lag to be used in the ACF computation. The default is 20.
#' @param title A character string for the title of the plot. Defaults to "Autocorrelation Function".
#'
#' @return A \code{ggplot} object displaying the ACF for each variable in separate facets.
#'
#' @details
#' The function begins by verifying that each variable in \code{vars} exists in \code{data} and is numeric.
#' It then calculates the ACF for each variable using the base R function \code{acf}. The lags and ACF values
#' are extracted from the \code{acf} object and combined into a data frame. Finally, a faceted plot is created using
#' \code{ggplot2::geom_segment} and \code{ggplot2::facet_wrap}, with a horizontal dashed line at y = 0 for reference.
#' The Stata theme from \code{ggthemes} is applied to the plot.
#'
#' @examples
#' \dontrun{
#' # Create example data
#' set.seed(42)
#' data <- data.frame(
#'   var1 = rnorm(100),
#'   var2 = rnorm(100)
#' )
#'
#' # Plot the ACF for var1 and var2 with a maximum lag of 20
#' p <- plot_acf_stata(data, vars = c("var1", "var2"), lag.max = 20, title = "ACF Plot")
#' print(p)
#' }
#'
#' @export
acfplot <- function(data, vars, lag.max = 20, title = "Autocorrelation Function") {
  # Check that each selected variable exists in the data frame and is numeric.
  for (v in vars) {
    if (!v %in% names(data))
      stop(paste("Variable", v, "not found in the data frame."))
    if (!is.numeric(data[[v]]))
      stop(paste("Variable", v, "must be numeric."))
  }

  # Calculate the ACF for each variable and store the results in a list.
  acf_list <- lapply(vars, function(v) {
    x <- data[[v]]
    acf_obj <- stats::acf(x, plot = FALSE, lag.max = lag.max)
    # Convert the lag and acf arrays to vectors.
    lags <- as.vector(acf_obj$lag)
    acf_values <- as.vector(acf_obj$acf)
    # Create a data frame with the variable name, lag, and acf values.
    data.frame(variable = v, lag = lags, acf = acf_values)
  })

  # Combine the list of data frames into one.
  acf_df <- do.call(rbind, acf_list)

  # Create the ACF plot using ggplot2 and facet by variable.
  p <- ggplot2::ggplot(acf_df, ggplot2::aes(x = lag, y = acf)) +
    ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0), color = "steelblue") +
    ggplot2::geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    ggplot2::labs(title = title, x = "Lag", y = "ACF") +
    ggplot2::facet_wrap(~ variable, scales = "free_y") +
    ggthemes::theme_stata()

  return(p)
}

#' Partial Autocorrelation Function Plot
#'
#' @param data
#' @param value_vars
#' @param lag.max
#' @param plot_title
#'
#' @returns
#' @export
pacfplot <- function(data, value_vars, lag.max = NULL, plot_title = "Faceted PACF Plot") {
  # Create a list to store PACF data for each variable.
  pacf_list <- list()

  # Loop over each variable to compute its PACF.
  for (var in value_vars) {
    # Remove missing values.
    ts_data <- stats::na.omit(data[[var]])
    n <- length(ts_data)

    # Compute PACF (without plotting) using stats::pacf.
    pacf_res <- stats::pacf(ts_data, lag.max = lag.max, plot = FALSE)
    # Extract lags and PACF values.
    lags <- as.vector(pacf_res$lag)
    pacf_vals <- as.vector(pacf_res$acf)

    # Create a data frame for this variable.
    df_pacf <- data.frame(
      Variable = var,
      lag = lags,
      pacf = pacf_vals,
      n = n,
      ci = 1.96 / sqrt(n)  # 95% confidence interval for white noise
    )

    pacf_list[[var]] <- df_pacf
  }

  # Combine the individual data frames into one.
  df_pacf_all <- do.call(rbind, pacf_list)

  # Create a data frame for the confidence interval lines
  ci_data <- unique(df_pacf_all[, c("Variable", "ci")])
  ci_data$ci_pos <- ci_data$ci
  ci_data$ci_neg <- -ci_data$ci

  # Build the faceted PACF plot
  p <- ggplot2::ggplot(df_pacf_all, ggplot2::aes(x = lag, y = pacf)) +
    ggplot2::geom_segment(ggplot2::aes(xend = lag, y = 0, yend = pacf),
                          color = "blue", size = 3.8) +
    ggplot2::geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
    ggplot2::geom_hline(data = ci_data, ggplot2::aes(yintercept = ci_pos),
                        color = "red", linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_hline(data = ci_data, ggplot2::aes(yintercept = ci_neg),
                        color = "red", linetype = "dashed", linewidth = 0.5) +
    ggplot2::facet_wrap(ggplot2::vars(Variable), scales = "free_y") +
    ggplot2::labs(title = plot_title, x = "Lag", y = "PACF") +
    ggthemes::theme_stata() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  print(p)
}

#' Compute Differenced Time Series Variable
#'
#' This function computes the differences for a specified time series variable in a data frame using a given lag.
#' It removes the first \code{lag} rows from the data frame (to account for the differencing) and adds a new column
#' with the computed differences. If a name for the new variable is not provided, it defaults to \code{"d_"}
#' concatenated with the original variable name.
#'
#' @param data A data frame containing the time series data.
#' @param var A string indicating the name of the variable (column) for which the difference should be computed.
#' @param lag An integer specifying the lag to use for the differencing. The default is 1.
#' @param new_var An optional string for the name of the new differenced variable. If \code{NULL} (the default),
#'   the new variable is named by prepending \code{"d_"} to \code{var}.
#'
#' @return A data frame with the first \code{lag} rows removed and a new column containing the differenced values.
#'
#' @details
#' The function uses the base R \code{diff()} function to compute the differences for the specified variable.
#' Because differencing produces a vector that is shorter than the original data by \code{lag} observations,
#' the first \code{lag} rows are removed from the original data frame to align the new variable with the remaining rows.
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' set.seed(123)
#' df <- data.frame(
#'   time = seq.Date(from = as.Date("2020-01-01"), by = "day", length.out = 10),
#'   value = cumsum(rnorm(10))
#' )
#'
#' # Compute the first difference of the 'value' column
#' df_diff <- diff_time_series(df, var = "value", lag = 1)
#' head(df_diff)
#' }
#'
#' @export
tsdifference <- function(data, var, lag = 1, new_var = NULL) {
  # Check if the specified variable exists in the data frame
  if (!var %in% names(data)) {
    stop("Variable '", var, "' not found in the data frame.")
  }

  # Define the name for the new variable if not provided
  if (is.null(new_var)) {
    new_var <- paste0("d_", var)
  }

  # Compute the differences using the built-in diff() function
  diff_values <- diff(data[[var]], lag = lag)

  # Remove the first 'lag' rows from the data to align with the computed differences
  data_diff <- data[-seq_len(lag), , drop = FALSE]

  # Add the new differenced variable as a column in the modified data frame
  data_diff[[new_var]] <- diff_values

  return(data_diff)
}


#' Take the Natural Logarithm of Numeric Variables in a Data Frame
#'
#' This function identifies all numeric columns in a data frame and replaces their values with
#' the natural logarithm of the original values. If no numeric columns are found, the function
#' issues a warning and returns the original data frame. In addition, if any numeric column contains
#' non-positive values (i.e., \code{<= 0}), a warning is issued since \code{log()} is undefined for
#' these values.
#'
#' @param df A data frame containing one or more columns. Only numeric columns will be transformed.
#'
#' @return A data frame where all numeric columns have been transformed by taking their natural logarithm.
#'   If there are no numeric columns, the original data frame is returned with a warning.
#'
#' @details
#' The function uses \code{sapply} to detect numeric columns. It then iterates through these columns and
#' applies the \code{log} function to each one. If any numeric column contains non-positive values, a warning
#' is produced because \code{log()} will return \code{-Inf} or \code{NaN} for such entries.
#'
#' @examples
#' \dontrun{
#' # Create a sample data frame with numeric and non-numeric columns
#' df <- data.frame(
#'   id = 1:5,
#'   value = c(10, 20, 30, 40, 50),
#'   category = letters[1:5]
#' )
#'
#' # Transform the numeric columns by taking their natural logarithm
#' df_log <- log_numeric_vars(df)
#' head(df_log)
#' }
#'
#' @export
logcaculator <- function(df) {
  # Identify which columns are numeric
  numeric_cols <- sapply(df, is.numeric)

  # If no numeric columns exist, warn the user and return the original data frame
  if (!any(numeric_cols)) {
    warning("No numeric columns found in the data frame.")
    return(df)
  }

  # Create a copy of the data frame so the original is not modified
  df_log <- df

  # Loop through the numeric columns and take the natural log
  for (col in names(df)[numeric_cols]) {
    # Optionally, warn if there are non-positive values because log() is undefined for them.
    if (any(df[[col]] <= 0, na.rm = TRUE)) {
      warning(paste("Column", col, "contains non-positive values. log() will produce -Inf or NaN for these entries."))
    }
    df_log[[col]] <- log(df[[col]])
  }

  return(df_log)
}


#' Perform Unit Root Tests using tseries and urca
#'
#' This function applies several unit root tests on each numeric variable in a data frame.
#' For each numeric variable, it conducts the Augmented Dickey-Fuller (ADF) test, Phillips-Perron (PP) test,
#' and KPSS test using functions from the **tseries** package, and the DF-GLS test using \code{ur.ers} from the **urca** package.
#'
#' @param x A data frame containing time series data. Only numeric columns are tested.
#'
#' @return A data frame summarizing the test statistics and p-values (or critical values) for each test for each variable.
#'
#' @details
#' For each numeric variable in \code{x}, the function performs:
#' \itemize{
#'   \item **ADF test**: using \code{tseries::adf.test}, with the test statistic and p-value rounded to two decimals.
#'   \item **PP test**: using \code{tseries::pp.test}, with the test statistic and p-value rounded to two decimals.
#'   \item **KPSS test**: using \code{tseries::kpss.test}, with the test statistic and p-value rounded to two decimals.
#'   \item **DF-GLS test**: using \code{urca::ur.ers} (with a constant model and a maximum lag of 4), with the test statistic rounded to two decimals and the critical values combined into a comma-separated string.
#' }
#' The results for each variable are combined into a single data frame.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' set.seed(123)
#' data <- data.frame(
#'   series1 = rnorm(100),
#'   series2 = rnorm(100)
#' )
#' results <- urootp2(data)
#' print(results)
#' }
#'
#' @export
unitroottest <- function(x) {
  # Initialize an empty list to store the results
  results <- list()

  # Loop through each variable in the dataframe
  for (var_name in names(x)) {
    # Get the current variable
    var_data <- x[[var_name]]

    # Check if the variable is numeric (unit root tests apply to numeric data)
    if (is.numeric(var_data)) {

      # Conduct tests using the tseries package

      # ADF Test
      adf_test <- tseries::adf.test(var_data)
      adf_stat <- round(adf_test$statistic, 2)
      adf_pval <- round(adf_test$p.value, 2)

      # PP Test
      pp_test <- tseries::pp.test(var_data)
      pp_stat <- round(pp_test$statistic, 2)
      pp_pval <- round(pp_test$p.value, 2)

      # KPSS Test
      kpss_test <- tseries::kpss.test(var_data)
      kpss_stat <- round(kpss_test$statistic, 2)
      kpss_pval <- round(kpss_test$p.value, 2)

      # DF-GLS Test using urca::ur.ers
      # (use model = "constant" for an intercept or "trend" if a trend is desired)
      dfgls_test <- urca::ur.ers(var_data, model = "constant", lag.max = 4)
      dfgls_stat <- round(dfgls_test@teststat, 2)
      dfgls_crit_val <- paste(round(dfgls_test@cval, 2), collapse = ", ")

      # Combine the results in a data frame
      test_results <- data.frame(
        Variable = var_name,
        ADF = adf_stat,
        ADF.P = adf_pval,
        PP = pp_stat,
        PP.P = pp_pval,
        KPSS = kpss_stat,
        KPSS.P = kpss_pval,
        DFGLS = dfgls_stat,
        DFGLS.C = dfgls_crit_val,
        stringsAsFactors = FALSE
      )

      # Append the results to the list
      results[[var_name]] <- test_results
    }
  }

  # Combine all results into a single dataframe
  final_results <- do.call(rbind, results)

  return(suppressWarnings(final_results))
}


urootc <- function(x) {
  # Initialize an empty list to store the results
  results <- list()

  # Loop through each variable in the dataframe
  for (var_name in names(x)) {
    # Get the current variable
    var_data <- x[[var_name]]

    # Check if the variable is numeric (unit root tests apply to numeric data)
    if (is.numeric(var_data)) {

      # Conduct tests using the urca package

      # ADF Test (using ur.df from urca)
      adf_test <- urca::ur.df(var_data, type = "drift", selectlags = "AIC")
      adf_stat <- round(adf_test@teststat[1], 2)
      adf_crit_val <- paste(round(adf_test@cval[1, ], 2), collapse = ", ")

      # PP Test (using ur.pp from urca)
      pp_test <- urca::ur.pp(var_data, type = "Z-tau", model = "constant")
      pp_stat <- round(pp_test@teststat, 2)
      pp_crit_val <- paste(round(pp_test@cval, 2), collapse = ", ")

      # KPSS Test (using ur.kpss from urca)
      kpss_test <- urca::ur.kpss(var_data, type = "mu")
      kpss_stat <- round(kpss_test@teststat, 2)
      kpss_crit_val <- paste(round(kpss_test@cval, 2), collapse = ", ")

      # DF-GLS Test using urca::ur.ers
      # (use model = "constant" for an intercept or "trend" if a trend is desired)
      dfgls_test <- urca::ur.ers(var_data, model = "constant", lag.max = 4)
      dfgls_stat <- round(dfgls_test@teststat, 2)
      dfgls_crit_val <- paste(round(dfgls_test@cval, 2), collapse = ", ")

      # Combine the results in a data frame
      test_results <- data.frame(
        Variable = var_name,
        ADF = adf_stat,
        ADF.C = adf_crit_val,
        PP = pp_stat,
        PP.C = pp_crit_val,
        KPSS = kpss_stat,
        KPSS.C = kpss_crit_val,
        DFGLS = dfgls_stat,
        DFGLS.C = dfgls_crit_val,
        stringsAsFactors = FALSE
      )

      # Append the results to the list
      results[[var_name]] <- test_results
    }
  }

  # Combine all results into a single dataframe
  final_results <- do.call(rbind, results)

  return(suppressWarnings(final_results))
}
