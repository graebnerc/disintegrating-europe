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

plots_title_size <- 12
plots_axis_title_size <- 11
plots_axis_ticks_size <- 10

# Figure 3: Export and import Ginis============================================
prepare_gini_data_from_scratch <- FALSE # The full data must be downloaded from 
# the Harvard complexity atlas and is not provided alongside this repository. 
# See the README.

# Specify country groups-------------------------------------------------------
source("code/setup_country_classification.R")

# Read in raw data-------------------------------------------------------------
if (prepare_gini_data_from_scratch){
  raw_data <- fread("data/exports_harv_hs_red.csv.bz2", 
                    colClasses = c("double", rep("character", 2),
                                   rep("double", 13))
  )
  
  set_of_countries <- sort(countries_of_interest)
  set_of_years <- 2000:2016
  
  new_base_data_final <- raw_data[
    exporter %in% countries_of_interest & 
      year %in% set_of_years & !is.na(pci), .(
        year,
        country=exporter,
        export_value=exp_val,
        import_value,
        commoditycode,
        PCI=pci
      )]
  
  # Set up the complexity corridor-----------------------------------------------
  
  complexity_corridor_num_2 <- seq(-2, 2, by = 1.0)
  complexity_corridor_var_2 <- rep(NA, length(complexity_corridor_num_2))
  for (i in 1:(length(complexity_corridor_num_2)-1)){
    complexity_corridor_var_2[i] <- paste("X", as.character(
      complexity_corridor_num_2[i]),
      ".sm.PCI.smeq.",
      as.character(complexity_corridor_num_2[i+1]),
      sep = "")
  }
  
  complexity_corridor_var_2 <- c("smeq.-2", complexity_corridor_var_2)
  complexity_corridor_var_2[length(complexity_corridor_var_2)] <- "above.2"
  
  # Aggregate exports for corridors----------------------------------------------
  
  pb <-txtProgressBar(min = 1, 
                      max = length(set_of_years), 
                      style = 3)
  for (year_index in 1:length(set_of_years)){
    setTxtProgressBar(pb, year_index)
    year_cons <- set_of_years[year_index]
    for (country_index in 1:length(set_of_countries)){
      country_cons <- set_of_countries[country_index]
      current_data <- new_base_data_final[year==year_cons & country==country_cons]
      
      country_frame_var <- rep(country_cons, length(complexity_corridor_var_2))
      year_frame_var <- rep(year_cons, length(complexity_corridor_var_2))
      complexity_corridor <- complexity_corridor_var_2
      exports_sum_frame_var <- rep(NA, length(complexity_corridor_var_2))
      imports_sum_frame_var <- rep(NA, length(complexity_corridor_var_2))
      
      for (f in 1:(length(complexity_corridor_var_2)-2)) {
        exports_sum_frame_var[f+1] <- sum(
          current_data[
            PCI>complexity_corridor_num_2[f] & 
              PCI<=complexity_corridor_num_2[f+1]]$export_val
        )
        
        imports_sum_frame_var[f+1] <- sum(
          current_data[
            PCI>complexity_corridor_num_2[f] & 
              PCI<=complexity_corridor_num_2[f+1]]$import_val
        )
      }
      exports_sum_frame_var[1] <- sum(
        current_data[PCI<=complexity_corridor_num_2[1]]$export_val)
      exports_sum_frame_var[length(exports_sum_frame_var)] <- sum(
        current_data[
          PCI>complexity_corridor_num_2[length(complexity_corridor_num_2)]
          ]$export_val)
      
      exports_total_frame_var <- rep(sum(exports_sum_frame_var, na.rm=T), 
                                     length(complexity_corridor_var_2))
      
      imports_sum_frame_var[1] <- sum(
        current_data[PCI<=complexity_corridor_num_2[1]]$import_val)
      imports_sum_frame_var[length(imports_sum_frame_var)] <- sum(
        current_data[
          PCI>complexity_corridor_num_2[length(complexity_corridor_num_2)]
          ]$import_val)
      
      imports_total_frame_var <- rep(sum(imports_sum_frame_var, na.rm=T), 
                                     length(complexity_corridor_var_2))
      
      if (country_index==1){
        country_data_current <- data.frame(
          year = year_frame_var, 
          country = country_frame_var,
          complexity.corridor = complexity_corridor, 
          exports.sum = exports_sum_frame_var,
          total.exports.year = exports_total_frame_var,
          imports.sum = imports_sum_frame_var,
          total.imports.year = imports_total_frame_var
        )
      } else {
        country_data_current <- rbind(
          country_data_current, 
          data.frame(
            year = year_frame_var, 
            country = country_frame_var,
            complexity.corridor = complexity_corridor, 
            exports.sum = exports_sum_frame_var,
            total.exports.year = exports_total_frame_var,
            imports.sum = imports_sum_frame_var,
            total.imports.year = imports_total_frame_var)
        )
      }
    }
    if (year_index == 1) {
      split_data <- country_data_current
    } else{
      split_data <- rbind(split_data, country_data_current)
    }
  }
  close(pb)
  
  split_data_file <- "data/trade_gini_categories.csv"
  fwrite(split_data, split_data_file)
  R.utils::bzip2(paste0(split_data_file),
                 destname=paste0(split_data_file, ".bz2"), 
                 overwrite = TRUE)
}

# Make figure------------------------------------------------------------------

make_gini_plot <- function(data_used, time_interval){
  
  gini_trade_data <- data_used %>%
    filter(year>=time_interval[1],
           year<=time_interval[2]) %>%
    group_by(year, complexity.corridor) %>%
    summarise(ginis.exp = gini(exports.sum, weights=total.exports.year),
              ginis.imp = gini(imports.sum, weights=total.imports.year)) %>%
    ungroup() %>%
    group_by(complexity.corridor) %>%
    summarise(mean.gini.ex = mean(ginis.exp, na.rm=TRUE),
              sd.gini.ex = sd(ginis.exp, na.rm=TRUE),
              q25.gini.ex = quantile(ginis.exp, .25, na.rm=TRUE),
              q75.gini.ex = quantile(ginis.exp, .75, na.rm=TRUE),
              mean.gini.im = mean(ginis.imp, na.rm=TRUE),
              sd.gini.im = sd(ginis.imp, na.rm=TRUE),
              q25.gini.im = quantile(ginis.imp, .25, na.rm=TRUE),
              q75.gini.im = quantile(ginis.imp, .75, na.rm=TRUE)) %>%
    ungroup()
  
  sequence_corridor <- unique(data_categories_trade$complexity.corridor)
  x_ax <- seq(-2.5, 2.5, length.out = length(sequence_corridor))
  
  gini_trade_data <- gini_trade_data[
    match(sequence_corridor, gini_trade_data$complexity.corridor),
    ]
  
  sequence_corridor_labels_tex <- c("$PCI\\leq -2$",
                                    "$-2 < PCI\\leq -1$",
                                    "$-1 < PCI\\leq 0$",
                                    "$0 < PCI\\leq 1$",
                                    "$1 < PCI\\leq 2$",
                                    "$PCI > 2")
  
  export_gini_final <- ggplot(gini_trade_data) + 
    geom_line(aes(x=x_ax, 
                  y=mean.gini.im, 
                  colour="imp") 
              ) +
    geom_ribbon(aes(x=x_ax, 
                    ymin=q75.gini.im, 
                    ymax=q25.gini.im), 
                alpha=0.2, 
                fill="#006600") +
    geom_line(aes(x=x_ax, 
                  y=mean.gini.ex,
                  colour="exp") 
              ) + 
    geom_ribbon(aes(x=x_ax, 
                    ymin=q75.gini.ex, 
                    ymax=q25.gini.ex), 
                alpha=0.2, 
                fill="#8600B3") +
    scale_x_continuous(limits = c(-3.05, 2.55), 
                       breaks=x_ax, 
                       labels=TeX(sequence_corridor_labels_tex)) +
    scale_y_continuous(limits = c(0.0, 0.6), 
                       breaks = seq(0.0, 0.5, by=0.1)) +
    coord_cartesian(xlim = x_ax, 
                    expand = FALSE) +
    scale_color_hue(labels = c("Imports", "Exports")) +
    scale_color_manual(
      values = c("imp"="#006600", "exp"="#8600B3"), 
      labels = c("Exports", "Imports")
    ) +
    ggtitle(paste0("Inequality of import/export complexity: ", 
                   time_interval[1], "-", time_interval[2])) +
    ylab("Average Gini") + 
    xlab("Product complexity (PCI)") +
    theme_icae() +
    theme(axis.title.x = element_text(),
          axis.text.x = element_text(
            angle = 30, 
            hjust = 1, 
            vjust = 1),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.y = element_line(colour = "grey"), 
          panel.grid.minor.y = element_blank()
    )

  return(export_gini_final)
}

data_categories_trade <- fread("data/trade_gini_categories.csv.bz2", 
                               colClasses = c("double", 
                                              rep("character", 2), 
                                              rep("double", 4)
                                              )
                               )

gini_plot_full <- make_gini_plot(data_categories_trade, c(2000, 2016)) +
  theme(
    legend.position = c(0.85, 0.2),
    legend.background = element_rect(linetype = "solid", 
                                     colour = "grey"), 
    legend.margin = margin(t = -4, r = 8, l = 8, b = 0),
    axis.title = element_text(color="black", size=plots_axis_title_size),
    plot.title = element_text(color="black", size=plots_title_size),
    axis.text = element_text(color="black", size=plots_axis_ticks_size)
  )
gini_plot_full

ggsave(plot = gini_plot_full, 
       here(filename = "output/fig_3_exp-imp-gini.pdf"),
       width = 6, height = 4)

ggsave(plot = gini_plot_full, 
       filename = here("output/fig_3_exp-imp-gini.png"),
       width = 6, height = 4)
