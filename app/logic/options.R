box::use(
  glue[glue],
  dplyr[...],
  graphics[...],
  purrr
)

# 1 Tree --------------------------------------------------------------------------------------

#' Calculate the matrix of the underlying asset value at each point in time
#'
#' This function creates a matrix of the underlying asset value at each point in time,
#' based on the number of steps provided. It is used in the calculations of a
#' Cox, Ross and Rubinstein binomial model.
#'
#' @param s_0 \code{numeric} The initial price of the underlying asset
#' @param u \code{numeric} factor by which the underlying asset's price increases when it goes up
#' @param d \code{numeric} factor by which the underlying asset's price decreases when it goes down
#' @param n \code{integer} The number of time steps in the binomial model
#'
#' @return \code{data.frame} The matrix of the underlying asset value at each point in time
#'
#' @family Binomial Model Functions
#'
#' @export
#'
#' @examples
#' underlying_asset_value_matrix(100, 1.1, 0.9, 3)
underlying_asset_value_matrix <- function(s_0, u, d, n) {
  scenarios <- as.data.frame(
    matrix(rep(NA, (n + 1) * (n + 1)), nrow = n + 1, ncol = n + 1)
  )

  # calculate scenarios
  step <- 0
  for (j in 0:n) {
    for (i in 0:step) {
      scenarios[(i + 1), (j + 1)] <- u^i * d^(step - i) * s_0
    }
    step <- step + 1
  }

  # Name the columns
  nms <- purrr$map(
    .x = c("step", "successes"),
    .f = \(.x, .y) {
      glue("{word}_{n}",
        n    = seq(0, n, 1),
        word = .x
      )
    }
  )

  colnames(scenarios) <- nms[[1]]
  rownames(scenarios) <- nms[[2]]

  return(scenarios)
}

#' Plot the tree of the underlying asset price at each point in time
#'
#' This function creates a plot of the tree of the underlying asset price at each point in time,
#' based on the provided matrix. It is used in the calculations of a
#' Cox, Ross and Rubinstein binomial model.
#'
#' @param data \code{data.frame} The matrix of the underlying asset value at each point in time
#' @param n \code{integer} The number of time steps in the binomial model
#'
#' @family Binomial Model Functions
#'
#' @export
#'
#' @examples
#' underlying_matrix <- underlying_asset_value_matrix(100, 1.1, 0.9, 3)
#' plot_tree(underlying_matrix, 3, "No")
plot_tree <- function(data, n) {
  # Initialize the plot with the initial value of s_0
  # and use the maximum and minimum values of the last step as limits
  plot(
    x    = 0,
    y    = data[1, 1],
    xlim = c(0, n),
    main = "Tree",
    xlab = "Steps",
    ylab = "Underlying Asset",
    col  = "black",
    pch  = 15,
    ylim = c(min(data[, ncol(data)] * 0.98, na.rm = TRUE), max(data[, ncol(data)], na.rm = TRUE))
  )

  # Draw the tree lines
  x_ <- 0
  k <- 2
  for (j in 1:(ncol(data) - 1)) {
    for (i in 1:k) {
      lines(
        x   = c(x_, (x_ + 1)),
        y   = c(data[i, j], data[i, j + 1]),
        col = "orange"
      )
      lines(
        x   = c(x_, (x_ + 1)),
        y   = c(data[i, j], data[i + 1, j + 1]),
        col = "orange"
      )
    }
    x_ <- x_ + 1
    k <- k + 1
  }

  # Plot the remaining points and a text indicating the underlying asset price in each scenario
  step <- 0
  for (j in 2:ncol(data)) {
    step <- step + 1

    for (i in 1:(step + 1)) {
      points(
        x   = step,
        y   = data[i, j],
        col = "black",
        pch = 16
      )

      text(
        x      = step,
        y      = data[i, j],
        adj    = 0,
        labels = as.character(round(data[i, j], digits = 2)),
        pos    = 1
      )
    }
  }
}

# 2 European ----------------------------------------------------------------------------------

#' Calculate the price of a European call or put option
#'
#' This function calculates the price of a European call or put option using the binomial model.
#' European options can only be exercised at the end of the option's life.
#'
#' @param rf \code{numeric} The risk-free interest rate, expressed as a decimal (e.g., 0.05 for 5%)
#' @param time \code{numeric} Time to maturity, expressed in years (e.g., 1 for one year)
#' @param n \code{integer} The number of time steps in the binomial model
#' @param p \code{numeric} probability of the underlying asset's price increasing at each time step
#' @param u \code{numeric} factor by which the underlying asset's price increases when it goes up
#' @param d \code{numeric} factor by which the underlying asset's price decreases when it goes down
#' @param s_0 \code{numeric} The initial price of the underlying asset
#' @param k \code{numeric} The strike price of the option
#' @param type \code{character} indicating the option type: either "Call" or "Put"
#'
#' @return \code{numeric} The price of the European call or put option
#'
#' @family Binomial Model Functions
#'
#' @export
#'
#' @examples
#' price_option(0.05, 1, 10, 0.6, 1.1, 0.9, 100, 110, "Call")
#' price_option(0.05, 1, 10, 0.6, 1.1, 0.9, 100, 110, "Put")
price_option <- function(rf, time, n, p, u, d, s_0, k, type) {
  res <- vector("numeric", n + 1)

  for (i in 0:n) {
    res[i + 1] <-
      (factorial(n) / (factorial(i) * factorial(n - i))) *
        p^i * (1 - p)^(n - i) *
        switch(
          EXPR = type,
          Call = max(c((u^i * d^(n - i) * s_0 - k), 0)),
          Put  = max(c((k - u^i * d^(n - i) * s_0), 0))
        )
  }
  # calculate present value
  sum(res) * exp(-(rf * time))
}

#' Calculate intermediate nodes of the option price matrix
#'
#' This function computes the intermediate nodes of the option price matrix based on the
#' output of the underlying_asset_value_matrix function. It uses the generic price_option
#' function to value options at each node.
#'
#' @param data \code{matrix} The output of the underlying_asset_value_matrix function
#' @param rf \code{numeric} The risk-free interest rate
#' @param time \code{numeric} The time to maturity of the option
#' @param steps \code{integer} The number of time steps in the binomial model
#' @param d \code{numeric} factor by which the underlying asset's price decreases when it goes down
#' @param u \code{numeric} factor by which the underlying asset's price increases when it goes up
#' @param p \code{numeric} The risk-neutral probability of an upward movement in the underlying
#' asset price
#' @param k \code{numeric} The strike price of the option
#' @param poc \code{character} The type of option, either "Call" or "Put"
#'
#' @return \code{data.frame} The matrix of intermediate nodes of the option price
#'
#' @family Binomial Model Functions
#'
#' @export
#'
#' @examples
#' # First, create the underlying asset value matrix
#' asset_value_matrix <- underlying_asset_value_matrix(100, 1.1, 0.9, 3)
#' # Then, calculate the intermediate nodes of the option price matrix
#' intermediate_nodes(asset_value_matrix, 0.05, 1, 3, 0.9, 1.1, 0.5, 100, "Call")
intermediate_nodes <- function(data, rf, time, steps, d, u, p, k, type) {
  alpha <- time / steps # to discount at intermediate steps

  limit <- 1 # in each column of the `data` argument, a scenario is added
  for (j in seq_along(data[1, ])) {
    for (i in 1:limit) {
      # as each element different from NA in data is a node => we can use the generic function
      # to value options in each of them
      data[i, j] <- price_option(
        rf       = rf,
        s_0      = data[i, j],
        time     = time,
        d        = d,
        u        = u,
        p        = p,
        n        = steps,
        k        = k,
        type     = type
      )
    }
    limit <- limit + 1 # I start from the zero moment where there is a single node
    time <- time - alpha # as there are fewer steps left, r is multiplied by a fraction of T
    steps <- steps - 1 # as we change columns, the remaining steps decrease
  }

  return(as.data.frame(data))
}

# 3 American ----------------------------------------------------------------------------------

#' Calculate the payoff of an American option
#'
#' This function calculates the payoff of an American call or put option using the binomial model.
#' American options can be exercised at any time before the option's expiration.
#'
#' @param data \code{matrix} Output of the price_option function, representing the option prices at
#' different time steps
#' @param k \code{numeric} The strike price of the option
#' @param type \code{character} indicating the option type: either "Call" or "Put"
#'
#' @return \code{matrix} The matrix of American call or put option payoffs at different time steps
#'
#' @family Binomial Model Functions
#'
#' @export
#'
#' @examples
#' # First, create the underlying asset value matrix
#' asset_value_matrix <- underlying_asset_value_matrix(100, 1.1, 0.9, 3)
#' # Then calculate the payoffs
#' exercise_option(asset_value_matrix, 110, "Call")
exercise_option <- function(data, k, type) {
  data <- switch(type,
    "Call" = data - k,
    "Put"  = k - data
  )

  as.data.frame(data) |>
    mutate(
      across(
        .cols = everything(),
        .fns = \(.x) case_when(
          is.na(.x) ~ .x,
          .x > 0 ~ .x,
          .default = 0
        )
      )
    )
}

#' Calculate the price of an American option
#'
#' This function calculates the price of an American call or put option using the binomial model.
#' American options can be exercised at any time before the option's expiration.
#'
#' @param data \code{data.frame} The output of the exercise_option function,
#' representing option payoffs at different time steps
#' @param p \code{numeric} probability of the underlying asset's price increasing at each time step
#' @param r \code{numeric} The risk-free interest rate, expressed as a decimal (e.g., 0.05 for 5%)
#' @param n \code{integer} The number of time steps in the binomial model
#' @param time \code{numeric} Time to maturity, expressed in years (e.g., 1 for one year)
#'
#' @return \code{data.frame} The matrix of American option prices at different time steps
#'
#' @family Binomial Model Functions
#'
#' @export
#'
#' @examples
#' payoff_data <- underlying_asset_value_matrix(100, 1.1, 0.9, 3) |>
#'   exercise_option(price_data, 110, "Call")
#' price_american_option(payoff_data, 0.6, 0.05, 10, 1)
price_american_option <- function(data, p, r, n, time) {
  alpha <- time / n
  # Start from the end
  limit <- n
  for (column in (ncol(data) - 1):1) {
    for (row in 1:limit) { # repeat the operation for each node within the step
      data[row, column] <- max(
        data[row, column], # maximum between exercising and continuing to wait
        exp(-r * alpha) * ((1 - p) * data[row, (column + 1)] + p * data[(row + 1), (column + 1)])
      )
    }

    limit <- limit - 1
  }

  return(data)
}
