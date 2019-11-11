#' Capitalize first character
#' 
#' @param x The string to be capitalized
#' @return The string, but with the first letter being capitalized
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#' Create an alluvia plot
#' 
#' Creates an alluvia plot that represents trade flows
#' 
#' @param year_used The year for which the plot is to be made
#' @return A ggplot2 object
make_alluvium_plot_yearly_complete <- function(year_used){
  year_cons <- year_used
  bilat_hs2_y_sub <- bilat_hs2[year==year_cons][, year:=NULL]
  bilat_hs2_y_sub <- bilat_hs2_y_sub[, .(export_value=sum(export_value)), 
                                     .(location_code, partner_code)]
  bilat_hs2_y_sub <- bilat_hs2_y_sub[
    !(partner_code  =="RoW" | location_code =="RoW")
    ]
  bilat_hs2_y_sub$partner_code <- paste(
    bilat_hs2_y_sub$partner_code, " ", sep="")
  
  print(head(as.data.frame(bilat_hs2_y_sub), n = 4))
  
  alluvium_plot <- ggplot(
    as.data.frame(bilat_hs2_y_sub),
    aes(y = export_value, 
        axis1 = location_code, 
        axis2 = partner_code)
  ) +
    geom_alluvium(
      aes(fill = export_value), width = 1/12
    ) +
    geom_stratum(
      width = 1/12, fill = "black", color = "grey"
    ) +
    geom_label(
      stat = "stratum", label.strata = TRUE, size=2
    ) +
    scale_x_discrete(
      limits = c("Exporter", "Importer"), 
      expand = c(.05, .05)
    ) +
    ggtitle(
      paste0("Total export flows in ", year_used)
    ) +
    theme_icae() +
    scale_fill_icae(
      palette = "mixed", 
      aesthetics=c("fill", "color"), 
      discrete = F
    )
  return(alluvium_plot)
}

#' Make stratum plot
#' 
#' Creates a stratum plot that visualizes shares (or sums of values) over time.
#' 
#' @param data_used The data set to be used
#' @param location_var The column name for the region under consideration
#' @param y_var The variable name for the y-axis
#' @param stratum_years The years for which a stratum is to be computed
#' @param plot_title The title of the plot
#' @param ylabel The label for the y-axis
strat_plot <- function(data_used, location_var, y_var, 
                       stratum_years, plot_title, ylabel){
  ggplot(data_used,
         aes(x = year, 
             stratum = UQ(as.name(location_var)), 
             alluvium = UQ(as.name(location_var)),
             y = UQ(as.name(y_var)),
             label=firstup(gsub('[[:digit:]]+', "", 
                        UQ(as.name(location_var)))))) +
    geom_alluvium(aes(fill=UQ(as.name(location_var))), width = 0.25) +
    geom_label(
      data = filter(data_used, year%in% stratum_years),
      aes(color=UQ(as.name(location_var))),
      stat = "stratum", size = 3) +
    theme(legend.position = "none") +
    ggtitle(plot_title) +
    theme_icae() +
    ylab(ylabel) +
    scale_fill_icae(palette = "mixed", aesthetics=c("fill", "color")) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(limits = c(2000, 2017), 
                       breaks= seq(2000, 2017, 5), 
                       labels = seq(2000, 2017, 5), 
                       expand = c(0, 0)) +
    theme(legend.position = "none", axis.title.x = element_blank())
}
