table_default_v1 <- function (x, labels, groupspan = NULL, rowlabelhead = "", transpose = FALSE, 
          topclass = "Rtable1", footnote = NULL, caption = NULL, render = render.default, 
          render.strat = table1::render.strat, extra.col = NULL, extra.col.pos = NULL, 
          ...) 
{
  table1_internal_v1(x = x, labels = labels, groupspan = groupspan, 
                   rowlabelhead = rowlabelhead, transpose = transpose, topclass = topclass, 
                   footnote = footnote, caption = caption, render = render, 
                   render.strat = render.strat, extra.col = extra.col, extra.col.pos = extra.col.pos, 
                   ...)
}