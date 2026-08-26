library(tidyr)
library(dplyr)

df_a <- data.frame(
  subject = paste0("S", 1:10),
  week_0 = c(31,31,32,30,34,35,36,36,32,33),
  week_2 = c(36,34,31,32,33,34,31,35,31,32),
  week_4 = c(35,34,37,36,37,36,31,30,35,34),
  week_6 = c(37,35,35,35,37,38,38,40,36,36)
)


df$subject <- as.character(df$subject)

df_a

identical(df, df_a)

class(df)
class(df_a)
attributes(df)
attributes(df_a)
df$week_0
class(df$week_0)
class(df_a$week_0)








df_long_a <- df_a %>%
  pivot_longer(cols = starts_with("week"),
               names_to = "time",
               values_to = "score") %>%
  mutate(time = factor(time, levels = c("week_0","week_2","week_4","week_6")))

library(rstatix)

res_aov <- anova_test(
  data = df_long_a,
  dv = score,
  wid = subject,
  within = time
)

get_anova_table(res_aov)




##########

library(tidyr)
library(dplyr)

df <- data.frame(
  subject = paste0("S", 1:10),
  week_0 = c(31,31,32,30,34,35,36,36,32,33),
  week_2 = c(36,34,31,32,33,34,31,35,31,32),
  week_4 = c(35,34,37,36,37,36,31,30,35,34),
  week_6 = c(37,35,35,35,37,38,38,40,36,36)
)

df_long <- df %>%
  pivot_longer(cols = starts_with("week"),
               names_to = "time",
               values_to = "score") %>%
  mutate(time = factor(time, levels = c("week_0","week_2","week_4","week_6")))

library(rstatix)
options(scipen = 999)
options(digits = 10)
options(pillar.sigfig = 8)
res_aov <- anova_test(
  data = df_long,
  dv = score,
  wid = subject,
  within = time
)



get_anova_table(res_aov) |> print(digits = 8)

res_aov$ANOVA

res_aov$Mauchly


result_table <- get_anova_table(res_aov)

result_table[] <- lapply(
  result_table,
  function(x) {
    if (is.numeric(x)) {
      format(x, digits = 8, nsmall = 8)
    } else {
      x
    }
  }
)

result_table



?rstatix:::`.anova_test`


trace("anova_summary", edit = T)



