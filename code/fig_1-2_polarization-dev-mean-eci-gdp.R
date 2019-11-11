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
library(scales)
if (!require("icaeDesign")){
  devtools::install_github("graebnerc/icaeDesign")
}
source(here("code/setup_country_classification.R"))

download_bond_data <- F
download_eurostat_debt <- F
macro_data <- fread(here("data/macro_data_fig1-2_4.csv"))

start_year <- 1995
end_year <- 2018

plots_title_size <- 12
plots_axis_title_size <- 11
plots_axis_ticks_size <- 10

# Figure 1: Diverging development trajectories=================================
# 1.1. Deviation from mean income----------------------------------------------
dev_mean_data <- data.table::copy(macro_data)
dev_mean_plot <- dev_mean_data[!is.na(population) & !is.na(gdp_real_pc_ppp), 
                               .(year, iso3c, gdp_real_pc_ppp, population,
                  cluster=ifelse(iso3c %in% countries[["core"]], 
                                 "Core countries", ifelse(
                                   iso3c %in% countries[["peri"]], 
                                        "Periphery countries", ifelse(
                                          iso3c %in% countries[["france"]], 
                                               "France", NA))))
                  ][!is.na(cluster) & year>=start_year & year <= end_year] %>%
  group_by(year, cluster) %>%
  summarise(gdp_real_pc_ppp_sd=sd(gdp_real_pc_ppp),
            gdp_real_pc_ppp=weighted.mean(gdp_real_pc_ppp, w = population)
            ) %>% 
  ungroup() %>%
  group_by(year) %>%
  dplyr::mutate(mean.GDP_pc_PPP=mean(gdp_real_pc_ppp, na.rm = T)) %>%
  ungroup() %>%
  dplyr::mutate(dev.mean.GDP_pc_PPP = (gdp_real_pc_ppp - mean.GDP_pc_PPP)/1000
                ) %>%
  ggplot(., 
         aes(x=year, y=dev.mean.GDP_pc_PPP, color=cluster)) +
  geom_line() + geom_point() +
  ggtitle("Deviation of average income (population-weighted)") + 
  ylab("Deviation avg. GDPpc (PPP)") +
  scale_y_continuous(labels = scales::number_format(suffix = "k")) +
  xlab("(a)") +
  theme_icae() +
  scale_color_icae(palette = "mixed") + 
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
dev_mean_plot

# 1.2 Unemployment rate--------------------------------------------------------
x_axis_breaks <- c(1995, 2000, 2005, 2007, 2010, 2014, 2018)
unemp_plot <- macro_data %>%
  dplyr::select(dplyr::one_of(
    "year", "iso3c", "unemp_rate", "population_ameco")
  ) %>%
  dplyr::rename(population=population_ameco) %>%
  dplyr::mutate(is.core=ifelse(iso3c %in% countries[["core"]],
                               "Core countries", ifelse(
                                 iso3c %in% countries[["peri"]], 
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
    unemp_rate_mean=weighted.mean(unemp_rate, 
                                  pop_rel_group),
    unemp_rate_sd=sd(unemp_rate*pop_rel_group)
  ) %>%
  dplyr::ungroup() %>%
  ggplot(., aes(x=year, y=unemp_rate_mean, color=is.core)) +
  geom_point() + 
  geom_line() +
  geom_ribbon(aes(ymin=get("unemp_rate_mean")-get("unemp_rate_sd"), 
                  ymax=get("unemp_rate_mean")+get("unemp_rate_sd"),
                  linetype=NA, fill=is.core), 
              alpha=0.25, show.legend = FALSE) +
  xlab("(b)") +
  ylab("Weighted unemployment rate") +
  scale_color_icae(palette = "mixed", 
                                  aesthetics=c("color", "fill")
                                  ) +
  ggtitle("Weighted unemployment rate") + 
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expand_scale(
      mult = c(0, 0), add = c(0, 0.5)
      )
    ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
    ) +
  theme_icae() + 
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
unemp_plot 

# Final figure-----------------------------------------------------------------
deviation_fig <- ggarrange(
  dev_mean_plot, unemp_plot,
  ncol = 2, nrow = 1, 
  labels = c("a)", "b)"), 
  font.label = list(face="bold")
)
ggsave(plot = deviation_fig, 
       filename = here("output/fig_1_deviation-core-peri.pdf"),
       width = 10, height = 4)

ggsave(plot = deviation_fig, 
       filename = here("output/fig_1_deviation-core-peri.png"),
       width = 10, height = 4)

# Figure 2: ECI and income=====================================================
eci_income_data <- data.table::copy(macro_data) %>%
  select(iso3c, year, eci_harv_hs, gdp_real_pc_ppp) %>%
  mutate(cluster=ifelse(iso3c %in% countries[["core"]], 
                        "Core countries", ifelse(
                          iso3c %in% countries[["peri"]],
                          "Periphery countries", ifelse(
                            iso3c %in% countries[["france"]], 
                            "France", NA)))
         ) %>%
  dplyr::filter(!is.na(cluster), !is.na(gdp_real_pc_ppp), iso3c!="LUX") %>%
  group_by(iso3c, cluster) %>%
  summarise_all(mean, na.rm=T)%>%
  ungroup() 

l_reg <- lm(eci_harv_hs ~ gdp_real_pc_ppp, data = eci_income_data)
r_sq <- round(summary(l_reg)$r.squared, 2)
reg_eq <- as.character(as.expression(
  paste0("$GDP = \\beta_0 + \\beta_1 ECI,$ \\hspace{1cm} $R^2=", r_sq, "$")
))
reg_eq

eci_income <- ggplot(eci_income_data, 
                     aes(x=eci_harv_hs, 
                         y=gdp_real_pc_ppp, "k", 
                         color=cluster)) +
    geom_point(
      aes(x=eci_harv_hs, 
          y=gdp_real_pc_ppp, 
          color=cluster), 
      show.legend = T) +
    geom_smooth(
      aes(x=eci_harv_hs, 
          y=gdp_real_pc_ppp), 
      method = "lm", 
      color="#330080") +
    geom_label_repel(
      mapping = aes(
        x=eci_harv_hs, 
        y=gdp_real_pc_ppp, 
        label=iso3c, 
        color=cluster), 
      show.legend = F
    ) +
    theme_icae() + 
  scale_color_icae(palette = "mixed") +
  xlab("Economic complexity index (ECI)") + 
  ylab("Average GDP per capita (PPP)") +
  scale_x_continuous(expand = c(0, 0.01)) +
  scale_y_continuous(labels = scales::number_format(scale = 1/1000, suffix = "k")) +
  ggtitle("Complexity and per-capita income (average 1999-2016)") +
  annotate("text", x=1.75, y=25000, 
           label=TeX(
             paste0("$GDP = \\beta_0 + \\beta_1 ECI,$  ", " $R^2=", r_sq, "$"), 
             output = "character"), parse=TRUE) +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
eci_income

ggsave(plot = eci_income, 
       filename = here("output/fig_2_eci-income.pdf"),
       width = 8, height = 4)

ggsave(plot = eci_income, 
       filename = here("output/fig_2_eci-income.png"),
       width = 8, height = 4)
