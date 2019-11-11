rm(list=ls())
library(countrycode)
library(data.table)
library(tidyverse)
library(here)
library(ggpubr)
library(ggrepel)
library(latex2exp)
if (!require("icaeDesign")){
  devtools::install_github("graebnerc/icaeDesign")
}

plots_title_size <- 14
plots_axis_title_size <- 13
plots_axis_ticks_size <- 12

# 0. General setup-------------------------------------------------------------
# Use the sample as a reference for shares, or the whole world?
use_sample_basis <- F # If true, only uses sample countries as basis for shares
# Use the adjusted values, i.e. without NA-PCI values?
use_adjusted <- T # If true uses data that removes NA pci values

# 1. Specify country groups----------------------------------------------------

source(here("code/setup_country_classification.R"))
countries_all <- c(unlist(countries), "RdW")

# 2. Read in raw data----------------------------------------------------------

raw_data <- fread(here("data/exports_harv_hs_red.csv.bz2"), 
                  colClasses = c("double", rep("character", 2), 
                                 rep("double", 13))
                  )

# 3. Set up new data according to case considered----------------------------

sample_indicator <- ifelse(use_sample_basis, "sample", "world")

new_var_names <- c("exporter", "year", "commoditycode", "pci", "exp_val", 
                   "total.exp.world", "total.exp.country", "total.exp.product", 
                   "share.product.country.basket", "share.product.world.total", 
                   "share.product.world.product")

unproblematic <- c("exporter", "year", "commoditycode", "pci", "exp_val")
new_totals <- c("total.exp.world", 
                "total.exp.country",
                "total.exp.product")

old_totals <- c(paste0("exp_", sample_indicator,"_total"), 
                "exp_country_total", 
                paste0("exp_", sample_indicator,"_product")
                )

if (use_adjusted){
  old_totals <- paste0(old_totals, "_adj")
}

dat_5_new <- raw_data[year>=2000, mget(c(unproblematic, old_totals))]
setnames(dat_5_new,
         old = old_totals,
         new = new_totals)

dat_5_new[, share.product.country.basket:=exp_val/total.exp.country
          ][, share.product.world.total:=exp_val/total.exp.world
            ][, share.product.world.product:=exp_val/total.exp.product]
if (use_sample_basis){
  dat_5_new <- dat_5_new[exporter!="RdW"]
}

# 4. Add complexity categories-------------------------------------------------

dat_6_new <- dat_5_new %>%
  mutate(complexity.corridor=ifelse(
    pci< -8, -8.5,
    ifelse(
      pci>=-8 & pci < -7.0, -8,
      ifelse(
        pci>=-7 & pci< -6.0, -7,
        ifelse(
          pci>=-6 & pci < -5.0, -6,
          ifelse(
            pci>=-5 & pci < -4.0, -5,
            ifelse(
              pci>=-4 & pci< -3, -4,
              ifelse(
                pci>=-3 & pci < -2, -3,
                ifelse(
                  pci>=-2 & pci < -1, -2,
                  ifelse(
                    pci>=-1 & pci < 0, -1,
                    ifelse(
                      pci>=0 & pci < 1, 0,
                      ifelse(
                        pci>=1 & pci < 2, 1,
                        ifelse(
                          pci>=2 & pci< 3, 2,
                          ifelse(
                            pci>=3 & pci < 4, 3,
                            ifelse(
                              pci>=4 & pci < 5, 4,
                              ifelse(
                                pci>=5 & pci < 6, 5,
                                ifelse(
                                  pci>=6 & pci < 7, 6,
                                  ifelse(pci >= 7, 7, NA
                                  ))))))))))))))))))

dat_6_new <- dat_5_new %>%
  mutate(complexity.corridor=ifelse(
    pci< -4, -4.5,
    ifelse(
      pci>=-4 & pci < -3.5, -4,
      ifelse(
        pci>=-3.5 & pci< -3.0, -3.5,
        ifelse(
          pci>=-3.0 & pci < -2.5, -3.0,
          ifelse(
            pci>=-2.5 & pci < -2.0, -2.5,
            ifelse(
              pci>=-2.0 & pci< -1.5, -2.0,
              ifelse(
                pci>=-1.5 & pci < -1, -1.5,
                ifelse(
                  pci>=-1 & pci < -0.5, -1,
                  ifelse(
                    pci>=-0.5 & pci < 0, -0.5,
                    ifelse(
                      pci>=0 & pci < 0.5, 0,
                      ifelse(
                        pci>=0.5 & pci < 1, 0.5,
                        ifelse(
                          pci>=1 & pci< 1.5, 1,
                          ifelse(
                            pci>=1.5 & pci < 2, 1.5,
                            ifelse(
                              pci>=2 & pci < 2.5, 2,
                              ifelse(
                                pci>=2.5 & pci < 3, 2.5,
                                ifelse(
                                  pci>=3 & pci < 3.5, 3,
                                  ifelse(pci >= 3.5 & pci < 4, 3.5, 
                                         ifelse(pci>4.0, 4.0, NA
                                                )))))))))))))))))))


head(filter(dat_6_new, is.na(complexity.corridor)))

# 5. Get means, sums and shares for the corridors------------------------------

dat_7_new <- dat_6_new %>%
  group_by(exporter, year, complexity.corridor) %>%
  summarise(pci_mean=mean(pci, na.rm=T),
            pci_weig=weighted.mean(pci, exp_val, na.rm=T),
            exp_val=sum(exp_val, na.rm=T),
            total.exp.world=mean(total.exp.world),
            total.exp.country=mean(total.exp.country),
            total.exp.product=sum(total.exp.product),
            share.product.country.basket=sum(share.product.country.basket),
            share.product.world.total=sum(share.product.world.total)) %>%
  ungroup() %>% # Anteil Produkt exportiert von diesem Land am Weltmarkt
  mutate(share.product.world.product=exp_val/total.exp.product, 
         share.country.world.total=total.exp.country/total.exp.world)
head(dat_7_new)


# 6. Aggregate the groups------------------------------------------------------

frame_list_new <- list()
for (th in sort(unique(dat_7_new$complexity.corridor))) {
  print(th)
  frame_list_new[[as.character(th)]] <- filter(dat_7_new, 
                                           complexity.corridor>=th) %>% 
    mutate(greq_than=th)
}
frames_all_new <- rbindlist(frame_list_new)

dat_8_new <- frames_all_new %>%
  group_by(exporter, year, greq_than) %>%
  summarise(pci_mean=mean(pci_mean, na.rm=T),
            pci_weig=weighted.mean(pci_weig, exp_val),
            exp_val=sum(exp_val, na.rm=T)) %>%
  ungroup()
head(dat_8_new)

# dat_9 <- dat_8 %>%
#   left_join(dat_3_world, by="year") %>%
#   left_join(dat_3_country, by=c("year", "exporter")) %>% 
#   group_by(year, greq_than) %>%
#   # Gesamtexporte von allen Laendern fuer dieses Produkt
#   mutate(product.exp.world=sum(exp_val, na.rm=T)) %>% 
#   ungroup() %>%
#   mutate(share.country.world.total=total.exp.country/total.exp.world,
#          share.product.country.total=exp_val/total.exp.country,
#          share.product.world.total=product.exp.world/total.exp.world)
# head(dat_9)

dat_5_new_merge <-  unique(dat_5_new[, .(exporter, year, total.exp.country, total.exp.world)])

dat_9_new <- dat_8_new %>%
  left_join(dat_5_new_merge, by=c("year", "exporter")) %>%
  group_by(year, greq_than) %>%
  # Gesamtexporte von allen Laendern fuer dieses Produkt
  mutate(product.exp.world=sum(exp_val, na.rm=T)) %>% 
  ungroup() %>%
  mutate(share.country.world.total=total.exp.country/total.exp.world,
         share.product.country.total=exp_val/total.exp.country,
         share.product.world.total=product.exp.world/total.exp.world)
head(dat_9_new)

# 7. Add expected exports------------------------------------------------------

dat_10_new <- dat_9_new %>%
  mutate(expected.export=share.country.world.total*product.exp.world,
         diff.abs=exp_val-expected.export,
         diff.rel=(diff.abs/expected.export)*100)
head(dat_10_new)

# 8. Get statistics used in the paper text-------------------------------------
paper_info_1 <- dat_10_new %>%
  filter(exporter %in% c("DEU", "ESP", "PRT")) %>%
  select(exporter, share.country.world.total, year) %>%
  distinct(.keep_all=TRUE) %>%
  group_by(exporter) %>%
  summarise(mean_share=mean(share.country.world.total)*100) %>%
  ungroup()
head(paper_info_1)

paper_info_2 <- dat_10_new %>%
  filter(exporter %in% c("DEU", "ESP", "PRT"),
         greq_than==1.0) %>%
  select(exporter, diff.rel, year) %>%
  distinct(.keep_all=TRUE) %>%
  group_by(exporter) %>%
  summarise(mean_diff=mean(diff.rel)) %>%
  ungroup()
head(paper_info_2)

# 9. Visualization-------------------------------------------------------------

even_ceiled_seq <- function(min_val, max_val, by, basis=10, include_zero=F){
  base_seq <- seq(from = min_val, to = max_val, by = by)
  round_seq <- ceiling(base_seq/basis)*basis
  if (round_seq[1] %% 2 == 0){
    even_round_seq <- round_seq
  } else {
    even_round_seq <- round_seq - basis
    even_round_seq <- c(even_round_seq, even_round_seq[-1]+basis)
  }
  if (include_zero & !(0 %in% even_round_seq)){
    even_round_seq <- sort(c(even_round_seq, 0), decreasing = F)
  }
  return(even_round_seq)
}

make_fig8_plot <- function(data_used, time_period, pci_max=1, remove_RdW=T){
  if (remove_RdW){
    data_used <- filter(data_used, exporter!="RdW")
  }
  full_period_dat_mean <- filter(
    data_used, 
    year>=time_period[1] & year <=time_period[2], 
    exporter %in% countries_all,
    greq_than<=pci_max,
    greq_than>-3.5) %>%
    group_by(exporter, greq_than) %>%
    summarise(diff.rel=mean(diff.rel, na.rm=T)) %>%
    ungroup()
  
  x_min <- min(full_period_dat_mean[["greq_than"]])
  x_max <- max(full_period_dat_mean[["greq_than"]])
  x_lims <- c(x_min, x_max + 0.5)
  x_breaks <- seq(x_min, x_max, by = 0.5)
  x_labels <- paste0("$PCI \\geq ", x_breaks, "$")
  
  full_period_plot_mean <- ggplot(full_period_dat_mean) + 
    geom_line(
      aes(x=greq_than, 
          y=diff.rel, 
          colour=exporter)
    ) + 
    geom_point(
      aes(x=greq_than, 
          y=diff.rel, 
          colour=exporter)
    ) +
    scale_x_continuous(
      limits = x_lims,
      breaks = x_breaks,
      labels = TeX(x_labels)
      ) +
    scale_y_continuous(labels = scales::percent_format(scale = 1,
                                                       accuracy = 2),
                       breaks = even_ceiled_seq(
                         min(full_period_dat_mean[["diff.rel"]]), 
                         max(full_period_dat_mean[["diff.rel"]]), 
                         by = 30, basis = 10, include_zero=T)
                       ) +
    geom_label_repel(
      aes(
        x=greq_than, 
        y=diff.rel, 
        label=exporter, 
        colour=exporter), 
      data = filter(full_period_dat_mean, greq_than==as.character(pci_max)),  
      nudge_x = 0.5, 
      segment.alpha = 0.75, 
      segment.color = "grey", 
      max.iter = 4000
    ) +
    scale_color_icae(palette = "mixed", reverse = F) + 
    ggtitle(
      paste0("Polarization in terms of exported products: ",
             time_period[1], "-", time_period[2])
    ) +
    ylab("Relative mean divergence of expected value") + 
    xlab("PCI threshold") +
    theme(plot.title = element_text(size=16),
          legend.position = "none", 
          panel.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_line(colour="black"),
          axis.title.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(colour="grey")
    )
  
  return(full_period_plot_mean)  
}
pci_upper_limit <- 1.5
exp_devs_full_new <- make_fig8_plot(dat_10_new, 
                                    c(2000, 2016), 
                                    pci_upper_limit) +
  theme(plot.title = element_text(size=20),
        axis.title.y = element_text(size=17),
        axis.text = element_text(color="black", 
                                    size=17))
exp_devs_early_new <- make_fig8_plot(dat_10_new, 
                                     c(2000, 2006), 
                                     pci_upper_limit) +
  theme(plot.title = element_text(size=16),
        axis.title.y = element_text(color="black", size = 14),
        axis.text.y = element_text(color="black", size = 13),
        axis.text.x = element_text(angle = 30, vjust = 0.625, 
                                   color="black", size = 14))
exp_devs_late_new <- make_fig8_plot(dat_10_new, 
                                    c(2006, 2016), 
                                    pci_upper_limit) +
  theme(plot.title = element_text(size=16),
        axis.title.y = element_text(color="black", size = 14),
        axis.text.y = element_text(color="black", size = 13),
        axis.text.x = element_text(angle = 30, vjust = 0.625, 
                                   color="black", size = 14))

exp_devs_complete_new <- ggpubr::ggarrange(
  exp_devs_full_new,
  ggpubr::ggarrange(exp_devs_early_new, exp_devs_late_new, 
                    ncol = 2, nrow = 1, legend = "none", 
                    labels = c("b)", "c)"), font.label = list(font="bold")),
  ncol = 1, nrow = 2, legend = "none",
  labels = c("a)"), font.label = list(font="bold")
)

new_plot_name <- paste0("output/fig_8_polarized-exports",
                        ".pdf")

ggsave(filename =here(new_plot_name), 
       plot = exp_devs_complete_new,  width = 14, height = 11)

new_plot_name <- paste0("output/fig_8_polarized-exports", 
                        ".png")

ggsave(filename = here(new_plot_name), 
       plot = exp_devs_complete_new,  width = 14, height = 11)
