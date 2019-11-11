rm(list = ls())
library(tidyverse)
library(data.table)
library(countrycode)
library(here)
library(labelled)

source(here("code/fig_10_data_functions.R"))

# 1. Specify country groups and load data--------------------------------------
source(here("code/setup_country_classification.R"))

raw_data <- fread("data/exports_harv_hs_red.csv.bz2", 
                  colClasses = c("double", rep("character", 2),
                                 rep("double", 13)
                                 )
                  )

raw_data[, exp_share_sect:=exp_val/exp_world_product]
raw_data[, exp_country_total:=sum(exp_val, na.rm=T), .(year, exporter)]
raw_data[, exp_share_sect_reg:=exp_val/exp_country_total]

# raw_data <- raw_data[year<2017 & year>1994] # was:  year<2016, year>1994

# 2. Process data -------------------------------------------------------------

# 95-99 vs. 03-07
time_frame <- list()
time_frame[["old_low"]] <- 1995
time_frame[["old_up"]] <- 1999
time_frame[["new_low"]] <- 2003
time_frame[["new_up"]] <- 2007

long_data_1 <- make_plot_data(raw_data, 
                              period_considered = "long", 
                              gap_used = 5,
                              fuck_list=time_frame)

# 95-99 vs. 10-14
time_frame <- list()
time_frame[["old_low"]] <- 1995
time_frame[["old_up"]] <- 1999
time_frame[["new_low"]] <- 2010
time_frame[["new_up"]] <- 2014

long_data_2 <- make_plot_data(raw_data, 
                              period_considered = "long", 
                              gap_used = 5, 
                              fuck_list=time_frame)

# 03-07 vs. 10-14
time_frame <- list()
time_frame[["old_low"]] <- 2003
time_frame[["old_up"]] <- 2007
time_frame[["new_low"]] <- 2003
time_frame[["new_up"]] <- 2007

long_data_3 <- make_plot_data(raw_data, 
                              period_considered = "long", 
                              gap_used = 5, 
                              fuck_list=time_frame)

# 3. Make regression data -----------------------------------------------------
depend_var <- "diff_exp_val_total_log"
indep_vars <- c("av_pci_w")
weight_var <- c("late_exp_share_reg")
reg_formula <- set_up_reg_formula(depend_var, indep_vars)

reg_dat_long_1 <- make_regression_data(depend_var, 
                                       c(indep_vars, weight_var), long_data_1)
reg_dat_long_2 <- make_regression_data(depend_var, 
                                       c(indep_vars, weight_var), long_data_2)
reg_dat_long_3 <- make_regression_data(depend_var, 
                                       c(indep_vars, weight_var), long_data_3)

reg_dat_long_countries <- unique(reg_dat_long_3$pos$exporter)

# 4. Make estimates: 95-99 vs. 03-07 ------------------------------------------

reg_frame_long_wls_pos_1 <- make_reg_frame(reg_dat_long_1[["pos"]],
                                           long_data_1,
                                           period_cons = "long",
                                           change_cons = "positive", 
                                           dep_var = depend_var, 
                                           idep_vars = indep_vars, 
                                           countries_cons = setdiff(
                                             reg_dat_long_countries, "LUX"),
                                           weights=weight_var, 
                                           gap_cons = "Yes")

reg_frame_long_wls_neg_1 <- make_reg_frame(reg_dat_long_1[["neg"]],
                                           long_data_1,
                                           period_cons = "long",
                                           change_cons = "negative", 
                                           dep_var = depend_var, 
                                           idep_vars = indep_vars, 
                                           countries_cons =setdiff(
                                             reg_dat_long_countries, "LUX"), 
                                           weights=weight_var, 
                                           gap_cons = "Yes")

reg_total_frame_wls_1 <- do.call(rbind, list(reg_frame_long_wls_pos_1,
                                             reg_frame_long_wls_neg_1)) %>%
  select(-contains("late_share"))

wls_diff_reg_gap_1 <- reg_total_frame_wls_1 %>%
  select(one_of(
    c("country", "dep_var", "change_sign", "period", "int_coef", "pci_w_coef", 
      "gap", "positive_v_sh", "positive_n_sh", "negative_v_sh", "negative_n_sh"
    ))) %>%
  mutate(
    int_coef_n = UQ(
      as.name("int_coef")) * ifelse(change_sign=="positive", 
                                    UQ(as.name("positive_n_sh")), 
                                    UQ(as.name("negative_n_sh"))),
    int_coef_v = UQ(
      as.name("int_coef")) * ifelse(change_sign=="positive", 
                                    UQ(as.name("positive_v_sh")), 
                                    UQ(as.name("negative_v_sh"))),
    pci_w_coef_n = UQ(
      as.name("pci_w_coef")) * ifelse(change_sign=="positive",
                                      UQ(as.name("positive_n_sh")), 
                                      UQ(as.name("negative_n_sh"))),
    pci_w_coef_v = UQ(
      as.name("pci_w_coef")) * ifelse(change_sign=="positive", 
                                      UQ(as.name("positive_v_sh")), 
                                      UQ(as.name("negative_v_sh")))
  ) %>%
  select(
    -one_of(c("int_coef", "pci_w_coef"))
    ) %>%
  gather(parameter, estimate, 
         -country, -dep_var, -change_sign, -period, -gap, 
         -positive_v_sh, -positive_n_sh, -negative_v_sh, -negative_n_sh
         ) %>%
  spread(change_sign, estimate) %>%
  mutate(weighted_estimate=negative+positive) %>%
  select(-negative, -positive) %>%
  spread(parameter, weighted_estimate)

var_label(wls_diff_reg_gap_1) <- list(
  country = "The country, as given by Jakob's classification.",
  dep_var = "The dependent variable used in the regression.",
  period = "The period used for the early/late distinction. 'long' refers 
  to 1980-1999 vs. 2000-2016, and 'crisis' to 2000-2007 vs 2008-2016.",
  gap = "Has a gap been used? If 'yes' there was a ten year gap in the 
  long period (1995-2005) and a six year gap in the crisis period (2005-2011).",
  positive_v_sh = "The share of positive changes in terms of value 
  (i.e. sum of positive changes divided by sum of total changes 
  (in absolute values).",
  positive_n_sh = "The share of positive changes in terms of frequency 
  (i.e. the number of positive changes divided by the number of all changes).",
  negative_v_sh = "The share of negative changes in terms of value 
  (i.e. sum of negative changes divided by sum of total changes 
  (in absolute values).",
  negative_n_sh = "The share of negative changes in terms of frequency 
  (i.e. the negative of positive changes divided by the number of all changes).",
  int_coef_n = "They weighted estimate for the intercept: 
  the estimates for positive and negative changes were summed, 
  and weighted with their shares in terms of frequency.",
  int_coef_v = "They weighted estimate for the intercept: 
  the estimates for positive and negative changes were summed, 
  and weighted with their shares in terms of value.",
  pci_w_coef_n = "They weighted estimate for the parameter for weighted 
  complexity: the estimates for positive and negative changes were summed, 
  and weighted with their shares in terms of frequency.",
  pci_w_coef_v = "They weighted estimate for the parameter for weighted
  complexity: the estimates for positive and negative changes were summed, 
  and weighted with their shares in terms of value."
)
head(wls_diff_reg_gap_1)

fwrite(wls_diff_reg_gap_1, here("data/wls_95_99_vs_03_07.csv"))

# 5. Make estimates: 95-99 vs. 10-14 ------------------------------------------

reg_frame_long_wls_pos_2 <- make_reg_frame(reg_dat_long_2[["pos"]],
                                           long_data_2,
                                           period_cons = "long",
                                           change_cons = "positive", 
                                           dep_var = depend_var, 
                                           idep_vars = indep_vars, 
                                           countries_cons = setdiff(
                                             reg_dat_long_countries, "LUX"),
                                           weights=weight_var, 
                                           gap_cons = "Yes")

reg_frame_long_wls_neg_2 <- make_reg_frame(reg_dat_long_2[["neg"]],
                                           long_data_2,
                                           period_cons = "long",
                                           change_cons = "negative", 
                                           dep_var = depend_var, 
                                           idep_vars = indep_vars, 
                                           countries_cons =setdiff(
                                             reg_dat_long_countries, "LUX"), 
                                           weights=weight_var, 
                                           gap_cons = "Yes")

reg_total_frame_wls_2 <- do.call(rbind, list(reg_frame_long_wls_pos_2,
                                             reg_frame_long_wls_neg_2)) %>%
  select(-contains("late_share"))

wls_diff_reg_gap_2 <- reg_total_frame_wls_2 %>%
  select(one_of(c("country", "dep_var", "change_sign", "period", "int_coef", 
                  "pci_w_coef", "gap", "positive_v_sh", "positive_n_sh", 
                  "negative_v_sh", "negative_n_sh"))
  ) %>%
  mutate(int_coef_n = UQ(
    as.name("int_coef")) * ifelse(change_sign=="positive", 
                                  UQ(as.name("positive_n_sh")), 
                                  UQ(as.name("negative_n_sh"))),
    int_coef_v = UQ(
      as.name("int_coef")) * ifelse(change_sign=="positive", 
                                    UQ(as.name("positive_v_sh")), 
                                    UQ(as.name("negative_v_sh"))),
    pci_w_coef_n = UQ(
      as.name("pci_w_coef")) * ifelse(change_sign=="positive", 
                                      UQ(as.name("positive_n_sh")), 
                                      UQ(as.name("negative_n_sh"))),
    pci_w_coef_v = UQ(
      as.name("pci_w_coef")) * ifelse(change_sign=="positive", 
                                      UQ(as.name("positive_v_sh")), 
                                      UQ(as.name("negative_v_sh")))
  ) %>%
  select(-one_of(c("int_coef", "pci_w_coef"))) %>%
  gather(parameter, estimate, -country, -dep_var, -change_sign, -period, 
         -gap, -positive_v_sh, -positive_n_sh, -negative_v_sh, -negative_n_sh
  ) %>%
  spread(change_sign, estimate) %>%
  mutate(weighted_estimate=negative+positive) %>%
  select(-negative, -positive) %>%
  spread(parameter, weighted_estimate)
var_label(wls_diff_reg_gap_2) <- list(
  country = "The country, as given by Jakob's classification.",
  dep_var = "The dependent variable used in the regression.",
  period = "The period used for the early/late distinction. 'long' refers to 
  1980-1999 vs. 2000-2016, and 'crisis' to 2000-2007 vs 2008-2016.",
  gap = "Has a gap been used? If 'yes' there was a ten year gap in the long 
  period (1995-2005) and a six year gap in the crisis period (2005-2011).",
  positive_v_sh = "The share of positive changes in terms of value 
  (i.e. sum of positive changes divided by sum of total changes 
  (in absolute values).",
  positive_n_sh = "The share of positive changes in terms of frequency 
  (i.e. the number of positive changes divided by the number of all changes).",
  negative_v_sh = "The share of negative changes in terms of value 
  (i.e. sum of negative changes divided by sum of total changes 
  (in absolute values).",
  negative_n_sh = "The share of negative changes in terms of frequency 
  (i.e. the negative of positive changes divided by the number of all changes).",
  int_coef_n = "They weighted estimate for the intercept: 
  the estimates for positive and negative changes were summed, 
  and weighted with their shares in terms of frequency.",
  int_coef_v = "They weighted estimate for the intercept: 
  the estimates for positive and negative changes were summed, 
  and weighted with their shares in terms of value.",
  pci_w_coef_n = "They weighted estimate for the parameter for weighted complexity: 
  the estimates for positive and negative changes were summed, 
  and weighted with their shares in terms of frequency.",
  pci_w_coef_v = "They weighted estimate for the parameter for weighted complexity: 
  the estimates for positive and negative changes were summed, 
  and weighted with their shares in terms of value.")
head(wls_diff_reg_gap_2)

fwrite(wls_diff_reg_gap_2, here("data/wls_95_99_vs_10_14.csv"))


# 5. Make estimates: 03-07 vs. 10-14 ------------------------------------------
reg_frame_long_wls_pos_3 <- make_reg_frame(reg_dat_long_3[["pos"]],
                                           long_data_3,
                                           period_cons = "long",
                                           change_cons = "positive", 
                                           dep_var = depend_var, 
                                           idep_vars = indep_vars, 
                                           countries_cons = setdiff(
                                             reg_dat_long_countries, "LUX"),
                                           weights=weight_var, 
                                           gap_cons = "Yes")

reg_frame_long_wls_neg_3 <- make_reg_frame(reg_dat_long_3[["neg"]],
                                           long_data_3,
                                           period_cons = "long",
                                           change_cons = "negative", 
                                           dep_var = depend_var, 
                                           idep_vars = indep_vars, 
                                           countries_cons =setdiff(
                                             reg_dat_long_countries, "LUX"), 
                                           weights=weight_var, 
                                           gap_cons = "Yes")

reg_total_frame_wls_3 <- do.call(rbind, list(reg_frame_long_wls_pos_3,
                                             reg_frame_long_wls_neg_3)) %>%
  select(-contains("late_share"))



wls_diff_reg_gap_3 <- reg_total_frame_wls_3 %>%
  select(one_of(c("country", "dep_var", "change_sign", "period", "int_coef", 
                  "pci_w_coef", "gap", "positive_v_sh", "positive_n_sh", 
                  "negative_v_sh", "negative_n_sh"))
  ) %>%
  mutate(int_coef_n = UQ(
    as.name("int_coef")) * ifelse(change_sign=="positive", 
                                  UQ(as.name("positive_n_sh")), 
                                  UQ(as.name("negative_n_sh"))),
    int_coef_v = UQ(
      as.name("int_coef")) * ifelse(change_sign=="positive", 
                                    UQ(as.name("positive_v_sh")), 
                                    UQ(as.name("negative_v_sh"))),
    pci_w_coef_n = UQ(
      as.name("pci_w_coef")) * ifelse(change_sign=="positive", 
                                      UQ(as.name("positive_n_sh")), 
                                      UQ(as.name("negative_n_sh"))),
    pci_w_coef_v = UQ(
      as.name("pci_w_coef")) * ifelse(change_sign=="positive", 
                                      UQ(as.name("positive_v_sh")), 
                                      UQ(as.name("negative_v_sh")))) %>%
  select(-one_of(c("int_coef", "pci_w_coef"))) %>%
  gather(parameter, estimate, -country, -dep_var, -change_sign, -period, -gap, 
         -positive_v_sh, -positive_n_sh, -negative_v_sh, -negative_n_sh) %>%
  spread(change_sign, estimate) %>%
  mutate(weighted_estimate=negative+positive) %>%
  select(-negative, -positive) %>%
  spread(parameter, weighted_estimate)
var_label(wls_diff_reg_gap_3) <- list(
  country = "The country, as given by Jakob's classification.",
  dep_var = "The dependent variable used in the regression.",
  period = "The period used for the early/late distinction. 'long' refers to 
  1980-1999 vs. 2000-2016, and 'crisis' to 2000-2007 vs 2008-2016.",
  gap = "Has a gap been used? If 'yes' there was a ten year gap in the long 
  period (1995-2005) and a six year gap in the crisis period (2005-2011).",
  positive_v_sh = "The share of positive changes in terms of value 
  (i.e. sum of positive changes divided by sum of total changes (in absolute values).",
  positive_n_sh = "The share of positive changes in terms of frequency 
  (i.e. the number of positive changes divided by the number of all changes).",
  negative_v_sh = "The share of negative changes in terms of value 
  (i.e. sum of negative changes divided by sum of total changes (in absolute values).",
  negative_n_sh = "The share of negative changes in terms of frequency 
  (i.e. the negative of positive changes divided by the number of all changes).",
  int_coef_n = "They weighted estimate for the intercept: 
  the estimates for positive and negative changes were summed, 
  and weighted with their shares in terms of frequency.",
  int_coef_v = "They weighted estimate for the intercept: 
  the estimates for positive and negative changes were summed, 
  and weighted with their shares in terms of value.",
  pci_w_coef_n = "They weighted estimate for the parameter for weighted complexity: 
  the estimates for positive and negative changes were summed, 
  and weighted with their shares in terms of frequency.",
  pci_w_coef_v = "They weighted estimate for the parameter for weighted complexity:
  the estimates for positive and negative changes were summed, 
  and weighted with their shares in terms of value.")
head(wls_diff_reg_gap_3)

fwrite(wls_diff_reg_gap_3, here("data/wls_03_07_vs_10_14.csv"))
