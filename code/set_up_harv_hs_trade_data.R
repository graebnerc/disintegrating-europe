rm(list = ls())
library(data.table)
library(countrycode)
library(here)

source(here("code/setup_country_classification.R"))

download_scratch <- F
recreate_reduced_data <- T
harv_hs_file_csv <- here("data/harv_hs.csv")
harv_hs_file_bz2 <- paste0(harv_hs_file_csv, ".bz2")

# Step 1 (optional): Download the data and save it in working directory--------
if (download_scratch){
  harv_hs_file_orig <- paste0(harv_hs_file_csv, ".zip")
  harv_hs_url <- 
    "https://intl-atlas-downloads.s3.amazonaws.com/country_hsproduct4digit_year.csv.zip"
  download.file(url = harv_hs_url, destfile = harv_hs_file_orig)
  
  harv_orig <- fread(
    cmd = paste0("unzip -p ", harv_hs_file_orig), 
    colClasses = c("character", "double", "character", 
                   rep("double", 8), rep("character", 4))
  )
  fwrite(harv_orig, harv_hs_file_csv)
  
  R.utils::bzip2(paste0(harv_hs_file_csv),
                 destname=paste0(harv_hs_file_csv, ".bz2"), 
                 overwrite = TRUE)
  
  unlink(harv_hs_file_orig)
}


# Step 2: Create reduced data set for the paper--------------------------------
if (download_scratch | recreate_reduced_data){
  harv_reduced_file <- here("data/exports_harv_hs_red.csv")
  if (!download_scratch){
    harv_orig <- fread(harv_hs_file_bz2,
                       colClasses = c("character", "double", "character", 
                                      rep("double", 8), rep("character", 4))
    )
  }

  harv_orig_red <- harv_orig[,  .(year, export_value=as.double(export_value), 
                                  import_value=as.double(import_value), 
                                  pci, location_code, hs_product_code)]
  
  harv_orig_red[, eu_sample_country := ifelse(
    location_code %in% countries_all, "yes", "no")]
  
  # Sums based on the whole data:
  harv_orig_red[
    , exp_world_total:=sum(export_value, na.rm=T), .(year)] 
  harv_orig_red[
    , exp_sample_total:=sum(export_value, na.rm=T), .(year, 
                                                      eu_sample_country)] 
  harv_orig_red[
    , exp_world_product:=sum(export_value, na.rm=T), .(year, 
                                                       hs_product_code)] 
  harv_orig_red[
    , exp_sample_product:=sum(export_value, na.rm=T), .(year, 
                                                        hs_product_code, 
                                                        eu_sample_country)] 
  harv_orig_red[
    , exp_country_total:=sum(export_value, na.rm=T), .(year, 
                                                       location_code)]
  
  # _adj Values: based only on clean data
  harv_orig_red <- harv_orig_red[!is.na(pci)] 
  
  harv_orig_red[
    , exp_world_total_adj:=sum(export_value, na.rm=T), .(year)] 
  harv_orig_red[
    , exp_sample_total_adj:=sum(export_value, na.rm=T), .(year, 
                                                          eu_sample_country)] 
  harv_orig_red[
    , exp_world_product_adj:=sum(export_value, na.rm=T), .(year, 
                                                           hs_product_code)] 
  harv_orig_red[
    , exp_sample_product_adj:=sum(export_value, na.rm=T), .(year, 
                                                            hs_product_code, 
                                                            eu_sample_country)] 
  harv_orig_red[
    , exp_country_total_adj:=sum(export_value, na.rm=T), .(year, 
                                                           location_code)]
  
  # Remove all countries not in the sample:
  harv_orig_red[, red_country_ind:=ifelse(eu_sample_country=="yes", location_code, "RdW")]
  harv_orig_red <- harv_orig_red[, .(export_value=sum(export_value, na.rm=T),
                                      import_value=sum(import_value, na.rm=T),
                                      pci=mean(pci, na.rm=T),
                                      exp_world_total=mean(exp_world_total, na.rm = T),
                                      exp_sample_total=mean(exp_sample_total, na.rm = T),
                                      exp_world_product=mean(exp_world_product, na.rm = T),
                                      exp_sample_product=mean(exp_sample_product, na.rm = T),
                                      exp_country_total=sum(exp_country_total, na.rm = T),
                                      exp_world_total_adj=mean(exp_world_total_adj, na.rm = T),
                                      exp_sample_total_adj=mean(exp_sample_total_adj, na.rm = T),
                                      exp_world_product_adj=mean(exp_world_product_adj, na.rm = T),
                                      exp_sample_product_adj=mean(exp_sample_product_adj, na.rm = T),
                                      exp_country_total_adj=sum(exp_country_total_adj, na.rm = T)
                                      ), 
                                      .(year, red_country_ind, hs_product_code)]
  harv_orig_red[, location_code:=red_country_ind][, red_country_ind:=NULL]

  harv_orig_red <- harv_orig_red[location_code %in% c(countries_all, "RdW")]
  
  # Save the reduced file:
  harv_orig_red <- harv_orig_red[, .(
    year, exporter=location_code, commoditycode=hs_product_code, pci, 
    exp_val=export_value,   
    exp_country_total, exp_country_total_adj,
    exp_world_total, exp_world_total_adj,
    exp_sample_total, exp_sample_total_adj, 
    exp_world_product, exp_world_product_adj,
    exp_sample_product, exp_sample_product_adj,
    import_value)
    ]
  fwrite(harv_orig_red, harv_reduced_file)
  
  R.utils::bzip2(paste0(harv_reduced_file),
                 destname=paste0(harv_reduced_file, ".bz2"), 
                 overwrite = TRUE)
  
  # Get ECI rankings
  harv_eci_ranking <- unique(harv_orig[, .(year, hs_eci, location_code)])
  harv_eci_ranking <- harv_eci_ranking[!is.na(hs_eci)]
  harv_eci_ranking[, complexity_harv_rank_simple:=rank(-hs_eci, 
                                                       ties.method = "min"), 
                   .(year)]
  harv_eci_ranking[, complexity_harv_rank_normed:=
                     ((complexity_harv_rank_simple-
                         min(complexity_harv_rank_simple)))/
                     (max(complexity_harv_rank_simple)-
                        min(complexity_harv_rank_simple)), .(year)]
  
  countries_in_sample <- list()
  for (y in min(harv_eci_ranking$year):max(harv_eci_ranking$year)){
    print(y)
    countries_in_sample[[as.character(y)]] <- unique(
      harv_eci_ranking[year==y]$location_code)
  }
  common_countries <- Reduce(intersect, countries_in_sample)
  
  harv_eci_ranking[location_code %in% common_countries, 
                   complexity_harv_rank_common:=rank(-hs_eci, 
                                                     ties.method = "min"), 
                   .(year)]
  harv_eci_ranking <- dplyr::rename(harv_eci_ranking, locationcode=location_code)
  fwrite(harv_eci_ranking, here("data/harv_eci_ranking.csv"))
}

# Add export data on China-----------------------------------------------------

if (recreate_reduced_data){
  full_data <- fread(here("data/harv_hs.csv.bz2"))
  dest_file <- here("data/china_exports.csv")
  china_data <- full_data[location_code=="CHN", .(year, 
                                                  commoditycode=hs_product_code,
                                                  export_value, import_value, 
                                                  pci)]
  china_data[, exp_country_total:=sum(export_value), .(year)]
  china_data[!is.na(pci), exp_country_total_adj:=sum(export_value), .(year)]
  china_data <- dplyr::rename(china_data, locationcode=location_code)
  fwrite(china_data, dest_file)
  R.utils::gzip(dest_file,
                 destname=paste0(dest_file, ".gz"), 
                 overwrite = TRUE)
}
