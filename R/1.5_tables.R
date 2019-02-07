# Compute summaries for tables


source("R/0_utilities.R")

# Wettest year: 2011
# Dryest year: 2014

# ---------------------------------------------------------------------
#Read in processed data

data_annual <- read_csv("output/data_annual.csv",
                        col_types = list(watershed = col_factor(),
                                         biomass = col_factor()))

data_longterm <- data_annual %>% 
  dplyr::group_by(watershed, biomass) %>% 
  dplyr::summarise_all(mean) %>% 
  dplyr::select(-wy)


# ---------------------------------------------------------------------
# Long term averages

View(data_longterm)
  


