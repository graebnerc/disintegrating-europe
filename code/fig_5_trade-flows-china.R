rm(list = ls())
library(data.table)
library(here)
library(tidyverse)
if (!require("icaeDesign")){
  devtools::install_github("graebnerc/icaeDesign")
}
library(ggalluvial)
source("code/bilat_analysis_functions.R")

plots_title_size <- 12
plots_axis_title_size <- 11
plots_axis_ticks_size <- 10

# Set country list-------------------------------------------------------------
source(here("code/setup_country_classification.R"))
countries[["china"]] <- countrycode::countrycode(
  c("China"), "country.name", "iso3c")
countries[["france"]] <- NULL
countries_all <- unlist(countries)

country_frame <- as_tibble(
  as.data.frame(countries_all, stringsAsFactors = FALSE), 
  rownames = "cluster")

# Get the price deflator-------------------------------------------------------
raw_file_name <- here("data/deflator.csv")

if (!file.exists(raw_file_name)) {
  deflator_usd <-  WDI::WDI(country = "US",
                            start = 1962,
                            indicator = c("deflator"="NY.GDP.DEFL.ZS")
  ) %>%
    filter(!is.na(deflator)
    ) %>%
    select(year, deflator)
  write_csv(x = deflator_usd, path = raw_file_name, col_names = T)
} else(
  deflator_usd <- read_csv(file = raw_file_name)
)

# Process bilateral trade data-------------------------------------------------
hs_2_stats <- fread(here("data/hs_2_stats.csv"))
load(file = here("data/bilat_hs2.rda"))
head(bilat_hs2)

bilat_hs2[as.data.table(deflator_usd), on = "year", deflator := (i.deflator/100)]
bilat_hs2[, export_value:=export_value/deflator]
bilat_hs2[, deflator:=NULL]

bilat_hs2_raw <- copy(bilat_hs2)

bilat_hs2[, location_code:=ifelse(
  !location_code %in% countries_all, "RoW",
  ifelse(location_code %in% countries[["core"]], "core",
         ifelse(location_code %in% countries[["peri"]], "peri",
                ifelse(location_code %in% countries[["china"]], "china", NA))))
  ]
bilat_hs2[, partner_code:=ifelse(
  !partner_code %in% countries_all, "RoW",
  ifelse(partner_code %in% countries[["core"]], "core",
         ifelse(partner_code %in% countries[["peri"]], "peri",
                ifelse(partner_code %in% countries[["china"]], "china", NA))))
  ]
bilat_hs2 <- unique(bilat_hs2[, .(export_value=sum(export_value, na.rm = T),
                                  import_value=sum(import_value, na.rm = T)),
                              .(year, location_code, partner_code, hs_product_code)
                              ])

# Figure 6A: sources of imports to the core------------------------------------
bilat_hs2_rel_imp <- data.table::copy(bilat_hs2)
bilat_hs2_rel_imp_data <- bilat_hs2_rel_imp[location_code=="core" & 
                                              partner_code!="RoW", 
                                            .(import_value=sum(import_value)), 
                                            .(year, partner_code)] %>%
  mutate(
    partner_code=ifelse(partner_code=="core", "1core", 
                        ifelse(partner_code=="china", "2china", "3peri"))
  ) %>%
  group_by(year) %>%
  mutate(
    total_imports=sum(import_value)
  ) %>%
  ungroup() %>%
  mutate(
    rel_import_value=(import_value/total_imports)*100
  )
bilat_hs2_rel_imp_plot <- strat_plot(bilat_hs2_rel_imp_data, 
                                     "partner_code", "rel_import_value", 
                                     c(2002, 2015), 
                                     "Sources for imports of the core countries",
                                     "Import share") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0), 
    sec.axis = sec_axis(~., 
                        labels = scales::percent_format(accuracy = 1, 
                                                        scale = 1)
    )
  ) +
  theme(
    axis.title = element_text(color="black", 
                              size=plots_axis_title_size),
    plot.title = element_text(margin = margin(b = 8),
                              color="black", 
                              size=plots_title_size),
    axis.text = element_text(color="black", 
                             size=plots_axis_ticks_size))
bilat_hs2_rel_imp_plot


# Figure 6B: destinations for exports of the core------------------------------

bilat_hs2_rel_exp <- data.table::copy(bilat_hs2)

bilat_hs2_rel_exp_data <- bilat_hs2_rel_exp[location_code=="core" & 
                                              partner_code!="RoW", 
                                            .(export_value=sum(export_value)), 
                                            .(year, partner_code)] %>%
  mutate(
    partner_code=ifelse(partner_code=="core", "1core", 
                        ifelse(partner_code=="china", "2china", "3peri"))
  ) %>%
  group_by(year) %>%
  mutate(
    total_exports=sum(export_value)
  ) %>%
  ungroup() %>%
  mutate(
    rel_export_value=(export_value/total_exports)*100
  )

bilat_hs2_rel_exp_plot <- strat_plot(bilat_hs2_rel_exp_data, 
                                     "partner_code", "rel_export_value", 
                                     c(2002, 2015), 
                                     "Destinations for exports from the core countries",
                                     "Export share") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    expand = c(0, 0), 
    sec.axis = sec_axis(
      ~., 
      labels = scales::percent_format(accuracy = 1, scale = 1)
    )
  ) +
  theme(
    axis.title = element_text(color="black", 
                              size=plots_axis_title_size),
    plot.title = element_text(margin = margin(b = 8),
                              color="black", 
                              size=plots_title_size),
    axis.text = element_text(color="black", 
                             size=plots_axis_ticks_size)
  )
bilat_hs2_rel_exp_plot

# Combine 6A and 6B:
fig_6_AB <- ggpubr::ggarrange(
  bilat_hs2_rel_imp_plot, bilat_hs2_rel_exp_plot, ncol = 2,
  labels = c("a)", "b)"), font.label = list(font="bold", size=10)
)

# Figure 6C and D: Exports between groups at different time points-------------

fig_6_alluvias <- list()
for (y in c(2000, 2017)){
  fig_6_alluvias[[as.character(y)]] <- make_alluvium_plot_yearly_complete(y) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(axis.title.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.line.y = element_blank(), 
          axis.text.y = element_blank(), 
          panel.grid = element_blank(),
          plot.title = element_text(margin = margin(b = 5)))
}
fig_6_CD_alluvia_part <- ggpubr::ggarrange(plotlist = fig_6_alluvias, 
                                   ncol = 2, legend = "none", 
                                   labels = c("c)", "d)"), 
                                   font.label = list(font="bold", size=10)
)
fig_6_CD_alluvia_part

# Setup complete figure 6------------------------------------------------------
fig_6_complete <- ggpubr::ggarrange(fig_6_AB, 
                                    fig_6_CD_alluvia_part, 
                               nrow = 2)

ggsave(plot = fig_6_complete, 
       filename = "output/fig_5_trade-with-china.pdf", 
       width = 9, height = 6)

ggsave(plot = fig_6_complete, 
       filename = "output/fig_5_trade-with-china.png", 
       width = 9, height = 6)

# Calculations for the text----------------------------------------------------
base_year <- 2000
comparison_year <- 2016

# Imports from the core:
bilat_hs2_rel_imp_core <- data.table::copy(bilat_hs2)
bilat_hs2_rel_imp_core <- bilat_hs2_rel_imp_core[location_code=="core" & 
                                                     year<=comparison_year]
bilat_hs2_rel_imp_core[, total_imports:=sum(import_value), .(year)]
bilat_hs2_rel_imp_core[, rel_imports:=sum(import_value), .(year, partner_code)]
bilat_hs2_rel_imp_core <- unique(bilat_hs2_rel_imp_core[, .(year, partner_code, 
                                                              total_imports, 
                                                              rel_imports)])
bilat_hs2_rel_imp_core[, share_imports:=(rel_imports/total_imports)*100]

paste0(
  "More precisely, in ", base_year, ", ", 
  round(bilat_hs2_rel_imp_core[partner_code=="peri" & year==base_year][[5]], 2), 
  "% of the imports of the Core countries came from Periphery countries; in ",
  comparison_year,
  ", this has decreased by ",
  abs(round(
    (((bilat_hs2_rel_imp_core[partner_code=="peri" & year==comparison_year][[5]]-
        bilat_hs2_rel_imp_core[partner_code=="peri" & year==base_year][[5]])/
       bilat_hs2_rel_imp_core[partner_code=="peri" & year==base_year][[5]])*100), 
    2)), 
  "% to ", 
  round(bilat_hs2_rel_imp_core[partner_code=="peri" & year==comparison_year][[5]], 2), 
  "%. At the same time imports from China more than tripled from ",
  round(bilat_hs2_rel_imp_core[partner_code=="china" & year==base_year][[5]], 2), 
  "% in ", base_year, 
  "% to ",
  round(bilat_hs2_rel_imp_core[partner_code=="china" & year==comparison_year][[5]], 2), 
  "% in ", comparison_year, 
  " (calculations based on data from the Atlas of Economic Complexity)."
  )

# Imports from China
bilat_hs2_rel_imp_china <- data.table::copy(bilat_hs2)
bilat_hs2_rel_imp_china <- bilat_hs2_rel_imp_china[location_code=="china" & 
                                                     year<=comparison_year]
bilat_hs2_rel_imp_china[, total_imports:=sum(import_value), .(year)]
bilat_hs2_rel_imp_china[, rel_imports:=sum(import_value), .(year, partner_code)]
bilat_hs2_rel_imp_china <- unique(bilat_hs2_rel_imp_china[, .(year, partner_code, 
                                                              total_imports, 
                                                              rel_imports)])
bilat_hs2_rel_imp_china[, share_imports:=(rel_imports/total_imports)*100]

paste0(
  "in ", base_year, ", core and periphery countries were responsible for ",
  round(bilat_hs2_rel_imp_china[partner_code=="core" & year==base_year][[5]], 2), 
  "% and ",
  round(bilat_hs2_rel_imp_china[partner_code=="peri" & year==base_year][[5]], 2), 
  "% of China’s imports, respectively. ", 
  "In ", comparison_year, ", for the periphery countries, this value has increased by ", 
  abs(round(
    ((bilat_hs2_rel_imp_china[partner_code=="peri" & year==comparison_year][[5]]-
        bilat_hs2_rel_imp_china[partner_code=="peri" & year==base_year][[5]])/
       bilat_hs2_rel_imp_china[partner_code=="peri" & year==base_year][[5]])*100, 2)),
  "% to ",
  round(bilat_hs2_rel_imp_china[partner_code=="peri" & year==comparison_year][[5]], 2), 
  "%. Yet, the relative increase for the core countries was almost three times higher: they managed to increase their share in Chinese imports by ",
  abs(round(
    ((bilat_hs2_rel_imp_china[partner_code=="core" & year==comparison_year][[5]] -
        bilat_hs2_rel_imp_china[partner_code=="core" & year==base_year][[5]])/
       bilat_hs2_rel_imp_china[partner_code=="core" & year==base_year][[5]])*100, 2)),
  "% up to ",
  round(bilat_hs2_rel_imp_china[partner_code=="core" & year==comparison_year][[5]], 2),
  "% (calculations based on data from the Atlas of Economic Complexity)."
)
