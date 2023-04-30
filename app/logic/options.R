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