# Code to create data "bilat_hs2.rda"

library(data.table)
library(here)

download_url <- paste0(
  "https://intl-atlas-downloads.s3.amazonaws.com/country_partner_hsproduct2digit_year.zip")
tmp <- tempfile(fileext = ".zip")
download.file(download_url,
              tmp,
              quiet = FALSE)

bilat_hs2 <- fread(
  cmd = paste0("unzip -p ", tmp), 
  showProgress = TRUE, 
  select = c("year", "export_value", "import_value", 
             "location_code", "partner_code", 
             "hs_product_code"),
  colClasses = c(rep("character", 3), 
                 "integer", 
                 rep("double", 4), 
                 rep("character", 6)
  )
)
save(bilat_hs2, file = here("data/bilat_hs2.rda"), compress = "xz")
