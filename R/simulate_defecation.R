#' Simulate defecation events
#'
#' @rdname simulate_defecation
#'
#' @param rho_0 the rho (shape) parameter for the time to first defecation event
#' @param loghaz_0 the log hazard of first defecation event (see details for relationship to the mean and scale parameter)
#' @param rho_1 the rho (shape) parameter for the time between subsequent defecation events
#' @param loghaz_1 the log hazard of subsequent defecation events (see details for relationship to the mean and scale parameter)
#' @param N the number of animals to simulate
#' @param end_time the time period for the simulation (presumed to be in minutes, although this is implicitly on the same scale as the rho/lambda parameters above)
#'
#' @returns a tibble with Animal (factor, with levels giving all animals including those with implicitly zero defecation events), Time (time of defecation event), and Number (order of defecation events for that animal)
#'
#' @details
#' Different rho/loghaz parameters for each animal can be supplied with a vector of length equal to N (length-1 inputs will be recycled, but no other recycling is performed).
#'
#' Note that the inputs for the time-to-event distributions are provided as rho and lambda:  this matches the parameterisation used by JAGS, but not that used by R.  The relationship between parameters is:
#' shape = rho
#' lambda = exp(log_hazard)
#' scale = lambda^(-1/rho)
#' mean = scale * gamma(1 + 1/shape)
#' lambda = (log(mean) – loggam(1 + 1/rho)) * -rho
#' (Where gamma() denotes the Gamma function, and loggam() is the Log Gamma function)
#'
#' The reason for using this parameterisation is that animal-level effects on log-hazard can be more easily specified by adding e.g. normally distributed random effects to loghaz_0 and loghaz_1
#'
#' @examples
#' defecations <- simulate_defecation(N=1000)
#'
#' @importFrom stringr str_c str_replace_all
#' @importFrom purrr map
#' @importFrom dplyr mutate bind_rows row_number count group_by ungroup rename
#' @importFrom tidyr complete
#' @importFrom forcats fct
#'
#' @export
simulate_defecation <- function(rho_0=1.2, loghaz_0=-5.6, rho_1=2.6, loghaz_1=-13.0, N = 1L, end_time = 60*3){

  scale_0 <- exp(loghaz_0) ^ (-1.0 / rho_0)
  scale_1 <- exp(loghaz_1) ^ (-1.0 / rho_1)

  animals <- str_c("A", seq_len(N) |> format() |> str_replace_all(" ", "0"))

  map(animals,
    \(i){
      times=time <- rweibull(1, rho_0, scale_0)
      while (time < end_time)
      {
        time <- time + rweibull(1, rho_1, scale_1)
        times <- c(times, time)
      }
      tibble(
        Animal = i,
        Time = times[times <= end_time]
      ) |>
        mutate(Number = row_number())
    },
    .progress=TRUE
  ) |>
    bind_rows() |>
    mutate(Animal = fct(Animal, levels=animals))
}

#' @rdname simulate_defecation
#' @export
group_defecation <- function(defecations, breaks = seq(0,60*3,by=60)){

  defecations |>
    mutate(Period = cut(Time, breaks)) |>
    count(Animal, Period) |>
    complete(Animal, Period, fill=list(n=0)) |>
    count(Period, n, name="nn") |>
    group_by(Period) |>
    mutate(Probability = nn / sum(nn)) |>
    ungroup() |>
    rename(Number = n, Tally = nn)

}
