# Misc analysis code for publication


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Import data (from 1.1)


# A single data frame with each variable
data_annual <- read_csv("output/data_annual.csv",
                        col_types = list(wy = col_character(),
                                         watershed = col_factor(),
                                         biomass = col_factor()))

# A list of data frames for each flux that has been differenced
diff_flux_annual <- read_rds("output/diff_flux_annual.rds")
diff_storage_annual <- read_rds("output/diff_storage_annual.rds")


data_daily <- read_csv("output/data_daily.csv",
                       col_types = list(wy = col_character(),
                                        run = col_factor(),
                                        season = col_factor(levels = c("Jan-Mar",
                                                                       "Apr-Jun",
                                                                       "Jul-Sep",
                                                                       "Oct-Dec")),
                                        biomass = col_factor(),
                                        watershed = col_factor()))

diff_flux_daily <- read_rds("output/diff_flux_daily.rds")
diff_storage_daily <- read_rds("output/diff_storage_daily.rds")


# Read in standard water balance data
data_annual_stacked <- read_csv(file="output/data_annual_stacked.csv")
# Read in vegetation change water balance data
veg_change_water_balance <- read_csv(file="output/veg_change_water_balance.csv")



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Precipitation

# Mean Precip and range
data_annual %>% 
  dplyr::summarise(precip = mean(Precip))

# Order the years by wetness
a <- data_annual %>% 
  dplyr::filter(watershed == "p303", biomass == 100)
o <- order(a$Precip)

a$wy[o]
a$Precip[o]

# Low 3:    2014 2007 2013
# Middle 4: 2004 2012 2008 2009
# High 4:   2010 2005 2006 2011


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Temperature

data_annual %>% 
  dplyr::select(wy, watershed, biomass, Tmax, Tmin) %>% 
  dplyr::summarise(Tmin = mean(Tmin),
                   Tmax = mean(Tmax))

mean(c(5.46,14))


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Streamflow

data_annual %>% 
  dplyr::filter(biomass == 100) %>% 
  dplyr::group_by(watershed) %>% 
  dplyr::summarise(Streamflow = mean(Streamflow))


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Get mean flux and coefficient of variation for each component of water balance

mean_watershed <- data_annual_stacked %>% 
  group_by(flux) %>% 
  dplyr::summarise(
    mean = mean(`100`),
    sd = sd(`100`),
    plus_minus = sd*1.96,
    co_var = sd(`100`)/mean(`100`)
  )

print(mean_watershed)

# Note: mean +- sd * 1.96




# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Find mean values for water made available and partitioned by treatments
# Also serves a check on vegetation change water balance



veg_change_water_balance_mean <- function(veg_change_water_balance_input, scenario_input, balance_component_input){
  conserved_mean_by_flux <- veg_change_water_balance_input %>% 
    dplyr::filter(scenario == scenario_input, balance_component==balance_component_input) %>% 
    dplyr::group_by(flux) %>% 
    dplyr::summarise(value = mean(value))
  conserved_mean <- conserved_mean_by_flux %>% 
    dplyr::ungroup() %>% 
    dplyr::summarise(value = sum(value))    
  print(conserved_mean_by_flux)
  print(paste("Total flux:", as.numeric(conserved_mean)))
}


# -----
# Water made available (conserved)

# 20% thinning scenario
veg_change_water_balance_mean(veg_change_water_balance_input = veg_change_water_balance, scenario_input = 80, balance_component_input = "conserved")
# 50% thinning scenario
veg_change_water_balance_mean(veg_change_water_balance_input = veg_change_water_balance, scenario_input = 50, balance_component_input = "conserved")

# -----
# Water partitioned (allocated)

# 20% thinning scenario
veg_change_water_balance_mean(veg_change_water_balance_input = veg_change_water_balance, scenario_input = 80, balance_component_input = "allocated")
# 50% thinning scenario
veg_change_water_balance_mean(veg_change_water_balance_input = veg_change_water_balance, scenario_input = 50, balance_component_input = "allocated")


