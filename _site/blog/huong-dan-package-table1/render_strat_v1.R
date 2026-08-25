render_strat_v1 <- function (strata, ..., transpose = F) 
{
  get_n <- function(x) {
    if (!is.null(w <- table1:::weights.weighted(x))) {
      sum(w)
    }
    else {
      nrow(x)
    }
  }
  stratn <- table1:::format_n(sapply(strata, get_n), ...)
  html <- ifelse(is.na(stratn), names(strata), sprintf("%s<br/><span class='stratn'>(n = %s)</span>", 
                                                       names(strata), stratn))
  nohtml <- ifelse(is.na(stratn), names(strata), sprintf("%s\n(n = %s)", 
                                                         names(strata), stratn))
  setNames(html, nohtml)
}