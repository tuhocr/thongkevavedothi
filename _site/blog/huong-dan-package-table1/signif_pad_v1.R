signif_pad_v1 <- function (x, digits = 3, round.integers = TRUE, round5up = TRUE, 
          dec, ...) 
{
  args <- list(...)
  if (!missing(dec)) {
    warning("argument dec is deprecated; please use decimal.mark instead.", 
            call. = FALSE)
    args$decimal.mark <- dec
  }
  eps <- if (round5up) 
    x * (10^(-(digits + 3)))
  else 0
  rx <- ifelse(!is.na(x) & x >= 10^digits & .isFALSE(round.integers), 
               round(x), signif(x + eps, digits))
  args1 <- c(list(x = rx, digits = digits, format = "fg", flag = "#"), 
             args[names(args) %in% names(formals(formatC))])
  args1 <- args1[!duplicated(names(args1))]
  cx <- do.call(formatC, args1)
  cx[is.na(x)] <- "0"
  cx <- gsub("[^0-9]*$", "", cx)
  ifelse(is.na(x), NA, cx)
}