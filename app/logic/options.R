box::use(
  glue[glue],
  purrr
)

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
