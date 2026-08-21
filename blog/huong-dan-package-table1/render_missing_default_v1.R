render_missing_default_v1 <- function (x, ...) 
{
  if (!is.null(w <- table1:::weights.weighted(x))) {
    missingx <- weighted.default(is.na(x), w)
  }
  else {
    missingx <- is.na(x)
  }
  with(stats_apply_rounding_v1(stats.default(missingx, ...), ...)$Yes, 
       c(Missing = sprintf("%s (%s%%)", 
                           FREQ, 
                           PCT)))
}