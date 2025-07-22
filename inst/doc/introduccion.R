## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(metacor)

## -----------------------------------------------------------------------------
library(metacor)

# Example dataset (for pre/post design only)
df <- data.frame(
  study_name = c("Study1", "Study2", "Study3", "Study4","Study5", "Study6", "Study7", "Study8", "Study9"),
  p_value_Int = c(1.038814e-07, NA, NA, NA, NA, 2.100000e-02, NA, NA, NA),
  n_Int = c(10, 10, 10, 10, 15, 15, 10, 10, 10),
  meanPre_Int = c(8.17, 10.09, 10.18, 9.85, 9.51,7.70, 10.00,  11.53, 11.20),
  meanPost_Int = c(10.12, 12.50, 12.56,10.41, 10.88, 9.20, 10.80,13.42,12.00),
  sd_pre_Int = c(1.83,0.67,0.66,0.90,0.62, 0.90, 0.70, 0.60, 1.90),
  sd_post_Int = c(1.85, 0.72, 0.97, 0.67, 0.76, 1.10, 0.70,0.80,1.80),
  upperCI_Int = c(NA, NA,NA, NA,NA, NA,NA, NA, NA),
  lowerCI_Int = c(NA, NA,NA, NA,NA, NA,NA, NA, NA))

results <- metacor_dual(df,
                        digits = 3,
                        method = "both",
                        apply_hedges = TRUE,
                        add_to_df = TRUE,
                        SMD_method = "SMDpre",
                        MeanDifferences = TRUE,
                        impute_method = "cv",
                        verbose = TRUE,
                        report_imputations = TRUE,
                        custom_sd_diff_int = NULL,
                        custom_sd_diff_con = NULL,
                        single_group = TRUE)

## -----------------------------------------------------------------------------
result <- metacor_dual(df, report_imputations = TRUE)

## -----------------------------------------------------------------------------
?metacor_dual


## -----------------------------------------------------------------------------
sessionInfo()

