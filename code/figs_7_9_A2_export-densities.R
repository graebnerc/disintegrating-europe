rm(list = ls())
library(countrycode)
library(data.table)
library(tidyverse)
if (!require("icaeDesign")){
  devtools::install_github("graebnerc/icaeDesign")
}
library(here)
library(reldist)
library(latex2exp)
source(here("code/setup_country_classification.R"))
add_china <- FALSE
start_year <- 2000

plots_title_size <- 12
plots_axis_title_size <- 11
plots_axis_ticks_size <- 10

# Setup data===================================================================
exp_data <- fread(here("data/exports_harv_hs_red.csv.bz2"), 
                    colClasses = c("double", rep("character", 2), 
                                   rep("double", 13)
                                   )
                    )
exp_data_2 <- exp_data[exporter!="RdW" & year>=start_year, .(year, 
                                                             commoditycode, 
                                                             exporter, pci, 
                                                             exp_val)]
exp_data_2[, exp_total_country:=sum(exp_val, na.rm = T), .(year, exporter)]
exp_data_2[, exp_share:=exp_val/exp_total_country]

exp_shares <- exp_data_2[, .(pci=mean(pci, na.rm = T), 
                             exp_share=mean(exp_share, na.rm = T)), 
                         .(exporter, commoditycode)]

exp_shares_groups <- exp_shares[
  , .(commoditycode, pci, exp_share, 
      cgroup=ifelse(
        exporter %in% countries[["core"]], "core", 
        ifelse(
          exporter %in% countries[["peri"]], "peri", 
          "france"))
      )]
exp_shares_groups <- unique(exp_shares_groups[, .(pci=mean(pci, na.rm = T), 
                                                  exp_share=mean(exp_share, 
                                                                 na.rm = T)), 
                                              .(cgroup, commoditycode)])
if (add_china){
  # Add export data on China
  exp_shares_china <- fread(here("data/china_exports.csv.gz"), 
                            colClasses = c("double", "character", 
                                           rep("double", 5)))
  exp_shares_china <- exp_shares_china[!is.na(pci)]
  exp_shares_china[, exp_share:=export_value/exp_country_total_adj]
  
  exp_shares_china <- unique(exp_shares_china[, .(pci=mean(pci), 
                                           exp_share=mean(exp_share, na.rm = T),
                                           cgroup="china"), 
                                       .(commoditycode)])
  exp_shares_groups <- rbindlist(list(exp_shares_groups, exp_shares_china), 
                                 use.names = TRUE)
  
  exp_shares <- rbindlist(list(exp_shares, 
                               mutate(exp_shares_china, exporter="CHN") %>% 
                                 select(-cgroup)),
                          use.names = TRUE)
}

# Figure 7: export basket densities in the core and the periphery==============
# Figure 7A: Export basket densities for core and periphery--------------------
if (add_china){
  groups_considered <- c("core", "peri", "china")
} else {
  groups_considered <- c("core", "peri")
}

fig_7a_core_peri_exp_densities <- exp_shares_groups %>%
  filter(cgroup %in% c("core", "peri")) %>% 
  mutate(cgroup=ifelse(cgroup=="core", "Core countries", 
                       ifelse(cgroup=="peri", "Periphery countries", "China"))
         ) %>%
  ggplot(., 
            aes(x=pci, color=cgroup, fill=cgroup)) + 
  geom_density(aes(weight=exp_share), alpha=0.5) + 
  ggtitle(paste0(
    "Distribution of PCI for exported products (average ", start_year, "-2017)"
    )) + 
  ylab("Density for products in country group export basket") +
  scale_y_continuous(limits = c(0, 0.62), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-3, 3), expand = c(0, 0)) +
  scale_color_icae(palette = "mixed", aesthetics = c("color", "fill")) +
  theme_icae() +
  theme(legend.position = c(0.175, 0.8))

# Figure 7B: Export basket densities for selected core countries---------------
fill_cols <- c(
  "DEU" = unname(icaeDesign::get_icae_colors("purple")),
  "FIN" = unname(icaeDesign::get_icae_colors("sand")),
  "CHN" = unname(icaeDesign::get_icae_colors("dark blue"))
)
if (add_china){
  cntrys <- c("DEU", "FIN", "CHN")
} else{
  cntrys <- c("DEU", "FIN")
}

figs_8b <- list()
for (cntry in cntrys){
  print(cntry)
  figs_8b[[cntry]] <- exp_shares %>%
    filter(exporter==cntry) %>%
    mutate(exporter:=countrycode(exporter, "iso3c", "country.name")) %>%
    ggplot(., 
           aes(x=pci)
    ) +
    ggtitle(paste0(
      "Distribution of PCI for exported products (average ", start_year, "-2017)"
      )) + 
    ylab("Density for share in export basket") +
    scale_y_continuous(limits = c(0, 0.75), expand = c(0, 0)) +
    scale_x_continuous(limits = c(-3, 3), expand = c(0, 0)) +
    geom_density(aes(weight=exp_share/sum(exp_share)), 
                 alpha=0.5, color=fill_cols[cntry], fill=fill_cols[cntry]) +
    facet_wrap(~exporter) + 
    theme_icae() + 
    theme(legend.position = "none")
}

# Piece together figure 7------------------------------------------------------
fig_7b_ger_fin_density <- ggpubr::ggarrange(plotlist = figs_8b, 
                                            nrow = length(cntrys))
oldw <- getOption("warn")
options(warn=-1)
exp_dist_full_plot <- ggpubr::ggarrange(
  fig_7a_core_peri_exp_densities, 
  fig_7b_ger_fin_density,
  ncol = 2, labels = c("a)", "b)"), font.label = list(face="bold"))
options(warn=oldw)
ggsave(plot = exp_dist_full_plot, 
       filename = here("output/fig_7_exp_dist_core_peri.pdf"),
       height = 7, width = 12)

ggsave(plot = exp_dist_full_plot, 
       filename = here("output/fig_7_exp_dist_core_peri.png"),
       height = 7, width = 12)


# Figure A2: Export densities for all countries--------------------------------
pci_dist_countries <- ggplot(
  mutate(
    exp_shares, exporter:=countrycode(exporter, "iso3c", "country.name")
    ), 
  aes(
    x=pci, group=exporter, fill=exporter, color=exporter)
  ) +
  ggtitle(
    "Distribution of PCI for exported products (average 2000-2017)"
    ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(limits = c(-3, 3), expand = c(0, 0)) +
  geom_density(aes(weight=exp_share), alpha=0.5) +
  facet_wrap(~exporter) +
  ylab("Density for products in country group export basket") +
  xlab("Product Complexity Index (PCI)") +
  theme_icae() +
  scale_color_icae(palette="mixed", aesthetics=c("fill", "color")) +
  theme(legend.position = "none",
        axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
options(warn=-1)

ggsave(plot = pci_dist_countries, 
       filename = here("output/fig_A2_exp_dists.pdf"),
       height = 7, width = 12)

ggsave(plot = pci_dist_countries, 
       filename = here("output/fig_A2_exp_dists.png"),
       height = 7, width = 12)
options(warn=oldw)

# Figure 9: shifts in export densities========================================
dynamic_data <- copy(exp_data_2)
dynamic_data[, cgroup:=ifelse(exporter %in% countries[["core"]], 
                              "Core countries", ifelse(
                                exporter %in% countries[["france"]],
                                "France", ifelse(
                                  exporter %in% countries[["peri"]],
                                  "Periphery countries", NA)
                              ))]
years_considered <- c(2000, 2016)

# Core vs. periphery-----------------------------------------------------------
dynamic_data_groups <- copy(dynamic_data)
dynamic_data_groups <- dynamic_data_groups[year%in%years_considered, 
                                           .(exp_val=sum(exp_val)), 
                                           .(year, cgroup, commoditycode, pci)]
dynamic_data_groups[, exp_total:=sum(exp_val), .(year, cgroup)]
dynamic_data_groups[, exp_share:=exp_val/exp_total]
dynamic_data_groups[, year:=as.factor(year)]

fig_9_core_peri <- ggplot(dynamic_data_groups[cgroup!="France"], 
                           aes(x=pci, color=year, fill=year)
) +
  scale_y_continuous(limits = c(0, 0.7), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-4, 4), expand = c(0, 0)) +
  ggtitle("Changes in the composition of average export baskets") +
  ylab("Density for share in export basket") +
  xlab("Product Complexity Index (PCI)") +
  geom_density(aes(weight=exp_share), 
               alpha=0.5) +
  facet_wrap(~cgroup) + 
  theme_icae() + 
  scale_color_icae(palette = "main", 
                   aesthetics=c("color", "fill")) +
  theme(legend.position = c(0.2, 0.87))

fig_9_core_peri_new <- ggplot(dynamic_data_groups[cgroup!="France"], 
                           aes(x=pci, color=cgroup, fill=cgroup)
) +
  scale_y_continuous(limits = c(0, 0.7), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-4, 4), expand = c(0, 0)) +
  ggtitle("Changes in the composition of average export baskets") +
  ylab("Density for share in export basket") +
  xlab("Product Complexity Index (PCI)") +
  geom_density(aes(weight=exp_share), 
               alpha=0.5) +
  facet_wrap(~year) + 
  theme_icae() + 
  scale_color_icae(palette = "main", 
                   aesthetics=c("color", "fill")) +
  theme(legend.position = c(0.12, 0.8675),
        axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))

ggsave(plot = fig_9_core_peri_new, 
       filename = here("output/fig_9_shifting-export-dist.pdf"),
       width = 7, height = 4)

ggsave(plot = fig_9_core_peri_new, 
       filename = here("output/fig_9_shifting-export-dist.png"),
       width = 7, height = 4)
