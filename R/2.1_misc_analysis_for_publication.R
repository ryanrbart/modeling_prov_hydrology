# Misc analysis code for publication

# Includes code for Table 4 values

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


# Generate average change values for standard water balance
mean_80 <- data_annual_stacked %>% 
  group_by(flux) %>% 
  dplyr::summarise(
    mean = mean(absolute_80),
    sd = sd(absolute_80),
    plus_minus = sd*1.96,
    co_var = sd(absolute_80)/mean(absolute_80)
  )
print(mean_80)

mean_50 <- data_annual_stacked %>% 
  group_by(flux) %>% 
  dplyr::summarise(
    mean = mean(absolute_50),
    sd = sd(absolute_50),
    plus_minus = sd*1.96,
    co_var = sd(absolute_50)/mean(absolute_50)
  )
print(mean_50)



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
# Water made available

# 20% thinning scenario
veg_change_water_balance_mean(veg_change_water_balance_input = veg_change_water_balance, scenario_input = 80, balance_component_input = "leftside")
# 50% thinning scenario
veg_change_water_balance_mean(veg_change_water_balance_input = veg_change_water_balance, scenario_input = 50, balance_component_input = "leftside")

# -----
# Water partitioned

# 20% thinning scenario
veg_change_water_balance_mean(veg_change_water_balance_input = veg_change_water_balance, scenario_input = 80, balance_component_input = "rightside")
# 50% thinning scenario
veg_change_water_balance_mean(veg_change_water_balance_input = veg_change_water_balance, scenario_input = 50, balance_component_input = "rightside")



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Generate results for table 4 for publication

table_4_part1 <- data_annual_stacked %>% 
  # group_by(watershed, flux) %>% 
  group_by(flux) %>% 
  dplyr::summarise(
    mean = round(mean(`100`),0),
    plus_minus = round(1.96*(sd(`100`)/(length(`100`)^0.5)),0),
    co_var = round(sd(`100`)/mean(`100`),2))

print(table_4_part1)


table_4_part2 <- data_annual_stacked %>% 
  # group_by(watershed, flux) %>% 
  group_by(flux) %>% 
  dplyr::summarise(
    mean = round(mean(absolute_50),0),
    plus_minus = round(1.96*(sd(absolute_50)/(length(absolute_50)^0.5)),0),
    co_var = round(sd(absolute_50)/mean(absolute_50),2))

print(table_4_part2)

veg_change_water_balance_mean <- function(veg_change_water_balance_input, scenario_input, balance_component_input){
  conserved_mean_by_flux <- veg_change_water_balance_input %>% 
    dplyr::filter(scenario == scenario_input, balance_component==balance_component_input) %>% 
    # dplyr::group_by(watershed, flux) %>% 
    group_by(flux) %>% 
    dplyr::summarise(
      mean = round(mean(value),0),
      plus_minus = round(1.96*(sd(value)/(length(value)^0.5)),0),
      co_var = round(sd(value)/mean(value),2))
  print(conserved_mean_by_flux)
  return(conserved_mean_by_flux)
}

table_4_part3 <- veg_change_water_balance_mean(veg_change_water_balance_input = veg_change_water_balance, scenario_input = 50, balance_component_input = "rightside")

# Export table components
write_csv(table_4_part1, "output/manuscript_plots/table_4_part_1.csv")
write_csv(table_4_part2, "output/manuscript_plots/table_4_part_2.csv")
write_csv(table_4_part3, "output/manuscript_plots/table_4_part_3.csv")



