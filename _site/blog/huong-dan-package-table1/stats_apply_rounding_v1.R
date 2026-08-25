stats_apply_rounding_v1 <- function (x, digits = 4, 
                                     digits.pct = 1, 
                                     round.median.min.max = TRUE, 
          round.integers = TRUE, round5up = TRUE, rounding.fn = signif_pad_v1, 
          ...) 
{
  mindig <- function(x, digits) {
    cx <- format(x)
    ndig <- nchar(gsub("\\D", "", cx))
    ifelse(ndig > digits, cx, rounding.fn(x, digits = digits, 
                                          round.integers = round.integers, 
                                          round5up = round5up, 
                                          ...))
  }
  format.percent <- function(x, digits) {
    if (x == 0) 
      "0"
    else if (x == 100) 
      "100"
    else round_pad(x, digits = digits.pct, ...)
  }
  if (!is.list(x)) {
    stop("Expecting a list")
  }
  if (is.list(x[[1]])) {
    lapply(x, stats.apply.rounding, digits = digits, digits.pct = digits.pct, 
           round.integers = round.integers, round5up = round5up, 
           ...)
  }
  else {
    r <- lapply(x, rounding.fn, digits = digits, round.integers = round.integers, 
                round5up = round5up, ...)
    nr <- c("N", "FREQ", "NMISS")
    nr <- nr[nr %in% names(x)]
    nr <- nr[!is.na(x[nr])]
    r[nr] <- lapply(x[nr], format_n, ...)
    if (!round.median.min.max) {
      sr <- c("MEDIAN", "MIN", "MAX")
      sr <- sr[sr %in% names(x)]
      r[sr] <- lapply(x[sr], mindig, digits = digits)
    }
    pr <- c("PCT", "PCTnoNA", "CV", "GCV")
    pr <- pr[pr %in% names(x)]
    pr <- pr[!is.na(x[pr])]
    r[pr] <- lapply(as.numeric(x[pr]), format.percent, digits = digits.pct)
    r
  }
}