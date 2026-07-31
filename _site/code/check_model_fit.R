check_model_fit <- function(model_lavaan){
  
  
  fit_ok <- c("chisq", "df", "cfi", "tli", "nfi", "ifi", "rfi", "rmsea", "srmr", "aic", "bic")
  fitINDEX <- lavaan:::fitMeasures(object = model_lavaan, 
                                   fit.measures = fit_ok)
  
  
  kq_fit_1F <- data.frame(
    FitIndex = fit_ok, 
    Value = round(fitINDEX, 3)
  )
  
  row.names(kq_fit_1F) <- NULL
  
  chisq_df <- kq_fit_1F$Value[kq_fit_1F$FitIndex == "chisq"] / kq_fit_1F$Value[kq_fit_1F$FitIndex == "df"]
  
  chisq_df_ok <- data.frame(FitIndex = "chisq / df",
                            Value = chisq_df)
  
  kq_fit_1F_ok <- rbind(kq_fit_1F,
                        chisq_df_ok)
  
  kq_fit_1F_ok <- kq_fit_1F_ok[ c(1:2, 
                                  nrow(kq_fit_1F_ok),
                                  3:(nrow(kq_fit_1F_ok)-1)) , ]
  
  row.names(kq_fit_1F_ok) <- NULL
  
  kq_fit_1F_ok |> dplyr:::mutate(cutoff = case_when(
    
    FitIndex == "chisq" ~ "",
    
    FitIndex == "df" ~ "",
    
    FitIndex == "chisq / df" & Value <= 2  ~ "Good",
    
    FitIndex == "chisq / df" & Value > 2  & Value <= 3 ~ "Acceptable",
    
    FitIndex == "cfi" & Value > 0.95 ~ "Good",
    
    FitIndex == "cfi" & Value > 0.9 & Value <= 0.95 ~ "Acceptable",
    
    FitIndex == "tli" & Value > 0.95 ~ "Good",
    
    FitIndex == "tli" & Value > 0.9 & Value <= 0.95 ~ "Acceptable",
    
    FitIndex == "nfi" & Value > 0.95 ~ "Good",
    
    FitIndex == "ifi" & Value > 0.95 ~ "Good",
    
    FitIndex == "rfi" & Value > 0.95 ~ "Good",
    
    FitIndex == "rmsea" & Value < 0.08 ~ "Good",
    
    FitIndex == "srmr" & Value > 0.05 & Value < 0.08 ~ "Acceptable",
    
    FitIndex == "srmr" & Value <= 0.05 ~ "Good",
    
    .default = ""
  ))
  
}