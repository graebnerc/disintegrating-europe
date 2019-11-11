countries <- list()
countries[["core"]] <- countrycode::countrycode(
  c("Austria", "Belgium", "Finland", "Luxembourg", "Germany", "Netherlands"),
  "country.name", "iso3c")
countries[["peri"]] <- countrycode::countrycode(
  c("Greece", "Ireland", "Italy", "Portugal",  "Spain"),
  "country.name", "iso3c")
countries[["france"]] <- countrycode::countrycode(
  c("France"), "country.name", "iso3c")
countries_all <- unlist(countries)