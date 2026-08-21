table1_internal_v1 <- function (x, labels, groupspan = NULL, rowlabelhead = "", transpose = FALSE, 
          topclass = "Rtable1", footnote = NULL, caption = NULL, render = render.default, 
          render.strat = table1::render.strat, extra.col = NULL, extra.col.pos = NULL, 
          ...) 
{
  if (is.null(labels$strata)) {
    labels$strata <- names(x)
  }
  if (is.null(names(labels$strata))) {
    names(labels$strata) <- names(x)
  }
  names(x) <- labels$strata
  if (is.character(render)) {
    render <- parse.abbrev.render.code(code = render, ...)
  }
  char2factor <- function(df) {
    df[, sapply(df, is.character)] <- lapply(df[, sapply(df, 
                                                         is.character), drop = FALSE], factorp)
    df
  }
  x <- lapply(x, char2factor)
  any.missing <- sapply(names(labels$variables), function(v) do.call(sum, 
                                                                     lapply(x, function(s) sum(is.na(s[[v]])))) > 0)
  if ("..." %in% formalArgs(args(render.strat))) {
    headings <- render.strat(x, ..., transpose = transpose)
  }
  else {
    headings <- render.strat(x)
  }
  if (is.null(names(headings))) {
    names(headings) <- headings
  }
  if (transpose) {
    ncolumns <- length(labels$variables)
    if (ncolumns > 12) {
      warning(sprintf("Table has %d columns. Are you sure this is what you want?", 
                      ncolumns))
    }
    contents <- lapply(names(x), function(s) {
      do.call(cbind, lapply(names(labels$variables), function(v) {
        lvls <- unique(do.call(c, lapply(x, function(s) levels(s[[v]]))))
        z <- x[[s]][[v]]
        if (!is.null(lvls)) {
          z <- factorp(z, levels = lvls)
        }
        y <- render(x = z, name = v, missing = any.missing[v], 
                    transpose = T, ...)
        y <- paste0(y, collapse = "<br/>")
        names(y) <- if (!is.null(names(labels$variables[[v]]))) {
          names(labels$variables[[v]])
        }
        else {
          labels$variables[[v]]
        }
        y <- t(y)
        rownames(y) <- s
        y
      }))
    })
  }
  else {
    if (!is.null(extra.col)) {
      headings <- c(headings, names(extra.col))
      if (!is.null(groupspan)) {
        groupspan <- c(groupspan, rep(1, length(extra.col)))
        labels$groups <- c(labels$groups, rep("", length(extra.col)))
      }
      if (!is.null(extra.col.pos)) {
        if (!is.numeric(extra.col.pos) || any(extra.col.pos > 
                                              length(headings))) {
          stop("extra.col.pos should be a vector of column positions")
        }
        if (length(extra.col.pos) > length(extra.col)) {
          stop("length of extra.col.pos should not exceed that of extra.col")
        }
        s1 <- seq(length(x) + 1, length.out = length(extra.col.pos))
        s2 <- setdiff(1:length(headings), s1)
        colpermute <- rep(0, length(headings))
        colpermute[extra.col.pos] <- s1
        colpermute[-extra.col.pos] <- s2
        headings <- headings[colpermute, drop = F]
        if (!is.null(groupspan)) {
          grpermute <- rep(1:length(groupspan), times = groupspan)[colpermute]
          grpermute <- grpermute[!duplicated(grpermute)]
          groupspan <- groupspan[grpermute]
          labels$groups <- labels$groups[grpermute]
        }
      }
    }
    ncolumns <- length(headings)
    if (ncolumns > 12) {
      warning(sprintf("Table has %d columns. Are you sure this is what you want?", 
                      ncolumns))
    }
    contents <- lapply(names(labels$variables), function(v) {
      lvls <- unique(do.call(c, lapply(x, function(s) levels(s[[v]]))))
      y <- do.call(cbind, lapply(x, function(s) {
        z <- s[[v]]
        if (!is.null(lvls)) {
          z <- factorp(z, levels = lvls)
        }
        render(x = z, name = v, missing = any.missing[v], 
               ...)
      }))
      if (!is.null(extra.col)) {
        pad_with_empty <- function(w, n) {
          rep(c(as.character(w), rep("", n)), length.out = n)
        }
        y2 <- do.call(cbind, lapply(extra.col, function(f) {
          pad_with_empty(f(lapply(x, getElement, name = v), 
                           v, ...), nrow(y))
        }))
        y <- cbind(y, y2)
        if (!is.null(extra.col.pos)) {
          y <- y[, colpermute, drop = F]
        }
      }
      rownames(y) <- paste(rownames(y), sep = "")
      rownames(y)[1] <- if (!is.null(names(labels$variables[[v]]))) {
        names(labels$variables[[v]])
      }
      else {
        labels$variables[[v]]
      }
      y
    })
  }
  obj <- list(contents = contents, headings = headings, labels = labels, 
              topclass = topclass, ncolumns = ncolumns, groupspan = groupspan, 
              transpose = transpose, rowlabelhead = rowlabelhead, caption = caption, 
              footnote = footnote)
  update_html(structure("", obj = obj))
}