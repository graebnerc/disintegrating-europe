rm(list = ls())
library(data.table)
library(countrycode)
library(here)
library(tidyverse)
if (!require("icaeDesign")){
  devtools::install_github("graebnerc/icaeDesign")
}

harv_orig <- fread(here("data/harv_hs.csv.bz2"),
                   colClasses = c("character", "double", "character", 
                                  rep("double", 8), rep("character", 4))
)

harv_red <- harv_orig[!is.na(pci), .(year, export_value, pci, hs_product_code)]
harv_red[, export_value:=sum(export_value, na.rm=T), .(year, hs_product_code)]
harv_red <- unique(harv_red)

dist_pci <- ggplot(harv_red, aes(x=pci, color=year, group=year)) +
  geom_density(alpha=0.2) +
  ggtitle("Density of products in total world trade") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0), add = c(0, 0.025))) +
  theme_icae() + 
  scale_color_icae() +
  theme(legend.position = "none")
dist_pci

ecdf_pci <- ggplot(harv_red, aes(x=pci, color=year, group=year)) +
  stat_ecdf() +
  ggtitle("ECDF of products in total world trade") +
  ylab("Share of products in world trade with pci<x") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0), add = c(0, 0.025))) +
  theme_icae() + 
  scale_color_icae() +
  theme(legend.position = "none")
ecdf_pci

fig_a1 <- ggpubr::ggarrange(dist_pci, ecdf_pci, 
                            ncol = 2, nrow = 1, 
                            legend = "none", labels = c("a)", "b)"), 
                            font.label = list(font="bold", size=10))
ggsave(plot = fig_a1, 
       filename = here("output/fig_A1_dist-pci.pdf"), 
       width = 9, height = 4)

ggsave(plot = fig_a1, 
       filename = here("output/fig_A1_dist-pci.png"), 
       width = 9, height = 4)
