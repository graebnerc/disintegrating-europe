rm(list=ls())
library(countrycode)
library(data.table)
library(eurostat)
library(tidyverse)
library(grid)
library(gridExtra)
library(ggrepel)
library(ggpubr)
library(here)
library(reldist)
library(latex2exp)
if (!require("icaeDesign")){
  devtools::install_github("graebnerc/icaeDesign")
}

source(here("code/setup_country_classification.R"))

plots_title_size <- 12
plots_axis_title_size <- 11
plots_axis_ticks_size <- 10

macro_data <- fread(here("data/macro_data_fig1-2_4.csv"))

start_year <- 1995
end_year <- 2018

# Plot function----------------------------------------------------------------
make_weighted_plot <- function(data_used, variable_name, y_lab, var_mean, 
                               var_sd, time_span, breaks_x, label_x){
  fig <- ggplot(data = data_used, 
                aes(x = year, 
                    y = get(var_mean), 
                    group=is.core, 
                    colour=is.core)) +  
    geom_path()  + 
    geom_ribbon(data=data_used,
                aes(ymin=get(var_mean)-get(var_sd), 
                    ymax=get(var_mean)+get(var_sd),
                    linetype=NA, fill=is.core), 
                alpha=0.25) +
    scale_color_icae(palette = "main", reverse = F, guide=FALSE) + 
    scale_fill_icae(palette = "main", reverse = F) + 
    scale_y_continuous(name = y_lab,
                       labels = scales::percent_format(scale = 1,
                                                       accuracy = 1)
    ) +
    scale_x_continuous(breaks=breaks_x, 
                       expand = expand_scale(mult = c(0, 0), 
                                             add = c(0, 2))
    ) + 
    xlab(label_x) +
    labs(
      title = paste(variable_name), 
      x = "Years", 
      y = y_lab, 
      color = "Core"
    ) +
    geom_vline(xintercept=2007, 
               color="#798ba0"
    ) +
    coord_cartesian(
      xlim = time_span, 
      expand = FALSE
    ) +
    theme_icae()
  return(fig)
}

#' Test uniqueness of data table
#'
#' Tests whether a data.table has unique rows.
#'
#' @param data_table A data frame of data table of which uniqueness should
#'  be tested.
#' @param index_vars Vector of strings, which specify the columns of
#'  data_table according to which uniqueness should be tested
#'  (e.g. country and year).
#' @return TRUE if data_table is unique, FALSE and a warning if it is not.
test_uniqueness <- function(data_table, index_vars, print_pos=TRUE){
  data_table <- data.table::as.data.table(data_table)
  if (nrow(data_table)!=data.table::uniqueN(data_table, by = index_vars)){
    warning(paste0("Rows in the data.table: ", nrow(data_table),
                   ", rows in the unique data.table:",
                   data.table::uniqueN(data_table, by = index_vars)))
    return(FALSE)
  } else {
    if (print_pos){
      print(paste0("No duplicates in ", as.list(sys.call()[[2]])))
    }
    return(TRUE)
  }
}

# Data preparation-------------------------------------------------------------
fig_4_data_weighted <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "population_ameco", "current_account_GDP_ameco")
  ) %>%
  dplyr::rename(population=population_ameco) %>%
  dplyr::mutate(is.core=ifelse(iso3c %in% countries[["core"]], "Core countries", 
                               ifelse(iso3c %in% countries[["peri"]], 
                                      "Periphery countries", NA))
  ) %>%
  dplyr::mutate(is.core=ifelse(iso3c=="FRA", "France", is.core)
  ) %>%
  dplyr::filter(!is.na(is.core)
  ) %>%
  dplyr::mutate(is.core=as.factor(is.core)
  ) %>%
  dplyr::filter(year>=start_year & year <= end_year) %>%
  dplyr::group_by(year, is.core) %>%
  dplyr::mutate(population_group=sum(population)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(pop_rel_group=population / population_group) %>%
  dplyr::group_by(year, is.core) %>%
  dplyr::mutate(test_pop=sum(pop_rel_group)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year, is.core) %>%
  dplyr::summarise(
    current_account_mean=weighted.mean(current_account_GDP_ameco, 
                                       pop_rel_group),
    current_account_sd=sd(current_account_GDP_ameco*pop_rel_group)
  ) %>%
  dplyr::ungroup()
head(fig_4_data_weighted)

# Figure 4 plot creation-------------------------------------------------------
x_axis_breaks <- c(1995, 2000, 2005, 2007, 2010, 2014, 2018)

fig_4_titles <- c(
  "Current account balance to GDP (population-weighted)" = "current_account"
)

fig_4_CA <- make_weighted_plot(
  fig_4_data_weighted, 
  names(fig_4_titles),
  "Current account balance to GDP",
  paste0(fig_4_titles, "_mean"), 
  paste0(fig_4_titles, "_sd"), 
  c(start_year, end_year),
  x_axis_breaks, paste0("(", letters[1],")")) +
  ylab("Current account balance to GDP") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))

ggsave(plot = fig_4_CA, 
       filename = "output/fig_4_current-account.pdf",
       width = 6, height = 4)

ggsave(plot = fig_4_CA, 
       filename = "output/fig_4_current-account.png",
       width = 6, height = 4)

# Numbers for the text in first paragraph of 2.3-------------------------------

cat("while the population-weighted average of the current account in the core ", 
    "countries rose from about ",
    round(filter(fig_4_data_weighted, 
                 year==2000, 
                 is.core=="Core countries")[["current_account_mean"]], 2), 
    "% in 2000 to more than ",
    round(filter(fig_4_data_weighted, 
                 year==2008, 
                 is.core=="Core countries")[["current_account_mean"]], 1),
    "% of GDP in 2008, the weighted average of current account deficits in the ",
    "periphery more than doubled from ",
    round(filter(fig_4_data_weighted, 
                 year==2000, 
                 is.core=="Periphery countries")[["current_account_mean"]], 2),
    "% at the start of the Euro project to ",
    round(filter(fig_4_data_weighted, 
                 year==2008, 
                 is.core=="Periphery countries")[["current_account_mean"]], 2),
    "% before the financial crisis.", sep = "")
