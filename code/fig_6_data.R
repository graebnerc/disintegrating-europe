rm(list=ls())
library(countrycode)
library(data.table)
library(tidyverse)
library(here)
library(IMFData)

source(here("code/setup_country_classification.R"))
countries_of_interest <- unlist(countries)

# Figures 2 and 3: Debt data---------------------------------------------------

debt_data <- fread(here("data/macro_data_fig1-2_4.csv"))
debt_data <- filter(debt_data, 
                    iso3c %in% countries_of_interest,
                    year>=1999, 
                    year<2018) %>%
  select(one_of("iso3c", "year", 
                "debt_corp_nf_percGDP", "debt_corp_f_percGDP", 
                "debt_gen_gov_percGDP", "debt_hh_npish_percGDI",
                "total_debt_percGDP"
  )
  ) %>%
  mutate(
    hh_resid_data=total_debt_percGDP-(
      debt_corp_nf_percGDP+debt_corp_f_percGDP+debt_gen_gov_percGDP),
    countryname=countrycode(iso3c, "iso3c", "country.name")
  )

fwrite(debt_data, "data/fig_6_debt_data.csv")
