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
# Find mean value for water made available by treatments

# 20% thinning scenario (by E and T)
aa <- veg_change_water_balance %>% 
  dplyr::filter(scenario == 80, balance_component=="conserved") %>% 
  dplyr::group_by(flux) %>% 
  dplyr::summarise(value = mean(value))

# 20% thinning scenario (by ET)
bb <- aa %>% 
  dplyr::ungroup() %>% 
  dplyr::summarise(value = sum(value))    
as.numeric(bb)
  

# 50% thinning scenario (by E and T)
aa <- veg_change_water_balance %>% 
  dplyr::filter(scenario == 50, balance_component=="conserved") %>% 
  dplyr::group_by(flux) %>% 
  dplyr::summarise(value = mean(value))

# 50% thinning scenario (by ET)
bb <- aa %>% 
  dplyr::ungroup() %>% 
  dplyr::summarise(value = sum(value))    
as.numeric(bb)
  

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Find mean value for partitioned water 

# 20% thinning scenario
aa <- veg_change_water_balance %>% 
  dplyr::filter(scenario == 80, balance_component=="allocated") %>% 
  dplyr::group_by(flux) %>% 
  dplyr::summarise(value = mean(value))
as.numeric(aa[4,2])

# 20% thinning scenario
bb <- aa %>% 
  dplyr::ungroup() %>% 
  dplyr::summarise(value = sum(value))    
as.numeric(bb)


# 50% thinning scenario
aa <- veg_change_water_balance %>% 
  dplyr::filter(scenario == 50, balance_component=="allocated") %>% 
  dplyr::group_by(flux) %>% 
  dplyr::summarise(value = mean(value))
as.numeric(aa[4,2])

# 50% thinning scenario
bb <- aa %>% 
  dplyr::ungroup() %>% 
  dplyr::summarise(value = sum(value))    
as.numeric(bb)




