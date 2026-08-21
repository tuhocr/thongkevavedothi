table1_formula_v1 <- function (x, data, overall = "Overall", rowlabelhead = "", transpose = FALSE, 
          droplevels = TRUE, topclass = "Rtable1", footnote = NULL, 
          caption = NULL, render = render.default, render.strat = table1::render.strat, 
          render.varlabel = table1::render.varlabel, extra.col = NULL, 
          extra.col.pos = NULL, ...) 
{
  f <- Formula(x)
  if (length(length(f)) != 2 || length(f)[2] < 1 || length(f)[2] > 
      2) {
    stop(paste0("Invalid formula: ", paste0(x, collapse = "")))
  }
  if (!is.null(overall) && length(overall) != 1) {
    stop("overall should have length 1 (unless NULL)")
  }
  if (length(f)[1] > 0) {
    warning("Unexpected LHS in formula ignored (table1 expects a 1-sided formula)")
  }
  if (length(f)[2] == 2) {
    f2 <- formula(f)
    f2[[2]][[3]] <- f[[2]][[2]]
    f2[[2]][[2]] <- f[[2]][[3]]
    f2 <- Formula(f2)
    dot <- !is.null(attr(terms(Formula(formula(f, rhs = 2)), 
                               data = data), "Formula_without_dot", exact = TRUE))
    dot2 <- !is.null(attr(terms(Formula(formula(f2, rhs = 2)), 
                                data = data), "Formula_without_dot", exact = TRUE))
    if (dot && dot2) {
      stop("Cannot have . in both parts of the formula")
    }
    if (dot || dot2) {
      f <- attr(terms(f, data = data, dot = "sequential"), 
                "Formula_without_dot", exact = TRUE)
      f2 <- attr(terms(f2, data = data, dot = "sequential"), 
                 "Formula_without_dot", exact = TRUE)
    }
    m1 <- model.frame(formula(f2, rhs = 2), data = data, 
                      na.action = na.pass)
    if (inherits(data, "weighted") && !is.null(w <- weights.weighted(data))) {
      m1 <- weighted.default(m1, w = w)
    }
    if (inherits(data, "indexed") && !is.null(i <- indices.indexed(data))) {
      m1 <- indexed.default(m1, i = i)
    }
    m2 <- model.frame(formula(f, rhs = 2), data = data, na.action = na.pass)
    if (!all(sapply(m2, is.factor) | sapply(m2, is.character))) {
      warning("Terms to the right of '|' in formula 'x' define table columns and are expected to be factors with meaningful labels.")
    }
    if (any(sapply(m2, function(xx) any(is.na(xx))))) {
      stop("Stratification variable(s) should not contain missing values.")
    }
    m2 <- lapply(m2, factorp)
    if (droplevels) {
      m2 <- lapply(m2, droplevels)
    }
    if (length(m2) > 1) {
      if (length(m2) > 2) {
        stop("Only 1 level of nesting is supported")
      }
      collabels <- tapply(m2[[2]], m2[[1]], levels, simplify = F)
      if (droplevels) {
        coln <- tapply(m2[[2]], m2[[1]], table, simplify = F)
        collabels <- mapply(function(x, y) x[y > 0], 
                            collabels, coln, SIMPLIFY = F)
      }
      grouplabel <- names(collabels)
      groupspan <- sapply(collabels, length)
      stratlabel <- unlist(collabels)
      if (!is.null(overall) && overall != FALSE) {
        if (!is.null(names(overall)) && names(overall) == 
            "left") {
          grouplabel <- c(overall, grouplabel)
        }
        else {
          grouplabel <- c(grouplabel, overall)
        }
        groupspan <- c(groupspan, nlevels(m2[[2]]))
        stratlabel <- c(stratlabel, levels(m2[[2]]))
      }
    }
    else {
      stratlabel <- levels(m2[[1]])
      if (!is.null(overall) && overall != FALSE) {
        if (!is.null(names(overall)) && names(overall) == 
            "left") {
          stratlabel <- c(overall, stratlabel)
        }
        else {
          stratlabel <- c(stratlabel, overall)
        }
      }
    }
  }
  else {
    m1 <- model.frame(formula(f, rhs = 1), data = data, na.action = na.pass)
    m2 <- NULL
    if (is.null(overall) || (is.logical(overall) && overall == 
                             FALSE)) {
      stop("Table has no columns?!")
    }
    stratlabel <- overall
  }
  for (i in 1:ncol(m1)) {
    if (!has.label(m1[[i]])) {
      label(m1[[i]]) <- names(m1)[i]
    }
  }
  if (!is.null(m2)) {
    strata <- split(m1, rev(m2))
    if (droplevels) {
      stratn <- sapply(strata, nrow)
      strata[stratn == 0] <- NULL
    }
    if (!is.null(overall) && overall != FALSE) {
      if (length(m2) > 1) {
        overall.strata <- split(m1, data.frame(m2[[2]], 
                                               overall = "overall"))
      }
      else {
        overall.strata <- list(overall = m1)
      }
      if (!is.null(names(overall)) && names(overall) == 
          "left") {
        strata <- c(overall.strata, strata)
      }
      else {
        strata <- c(strata, overall.strata)
      }
    }
  }
  else {
    strata <- list(overall = m1)
  }
  labels <- list(strata = stratlabel, variables = lapply(m1, 
                                                         render.varlabel, html = T, transpose = transpose))
  names(labels$strata) <- names(strata)
  if (!is.null(m2) && length(m2) > 1) {
    labels$groups <- grouplabel
    table_default_v1(x = strata, labels = labels, groupspan = groupspan, 
                   rowlabelhead = rowlabelhead, transpose = transpose, 
                   topclass = topclass, footnote = footnote, caption = caption, 
                   render = render, render.strat = render.strat, extra.col = extra.col, 
                   extra.col.pos = extra.col.pos, ...)
  }
  else {
    table_default_v1(x = strata, labels = labels, rowlabelhead = rowlabelhead, 
                   transpose = transpose, topclass = topclass, footnote = footnote, 
                   caption = caption, render = render, render.strat = render.strat, 
                   extra.col = extra.col, extra.col.pos = extra.col.pos, 
                   ...)
  }
}