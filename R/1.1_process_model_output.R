# Import model output and process
# Data originally in SigmaPlot


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Import data

wy <- seq(2004,2014)

generate_data <- function(wy, shed, biomass){
  # Generate file names of data to be imported 
  names <- map(.x=wy, 
               function(x)file.path("model_data",
                                    shed,
                                    paste(shed,"_",biomass,"_",x,".csv",sep="")))
  
  output_data <- names %>% 
    # Import data to list
    map(.,function(x)read_csv(x, n_max = 365)) %>% 
    # Set name of list
    setNames(., wy) %>% 
    dplyr::bind_rows(.id="wy") %>% 
    # Add column identifying the biomass level and watershed
    tibble::add_column(biomass = biomass, watershed = shed)
  
  return(output_data)
}

# Run data import function
rhessys_data <- list()
rhessys_data$p301_50 <- generate_data(wy=wy, shed="p301", biomass=50)
rhessys_data$p301_80 <- generate_data(wy=wy, shed="p301", biomass=80)
rhessys_data$p301_100 <- generate_data(wy=wy, shed="p301", biomass=100)

rhessys_data$p303_50 <- generate_data(wy=wy, shed="p303", biomass=50)
rhessys_data$p303_80 <- generate_data(wy=wy, shed="p303", biomass=80)
rhessys_data$p303_100 <- generate_data(wy=wy, shed="p303", biomass=100)

rhessys_data$p304_50 <- generate_data(wy=wy, shed="p304", biomass=50)
rhessys_data$p304_80 <- generate_data(wy=wy, shed="p304", biomass=80)
rhessys_data$p304_100 <- generate_data(wy=wy, shed="p304", biomass=100)

# Bind data together
data_daily <- dplyr::bind_rows(rhessys_data, .id="run")
data_daily$biomass <- factor(data_daily$biomass, 
                           levels = c(unique(data_daily$biomass)))
data_daily$watershed <- factor(data_daily$watershed, 
                             levels = c(unique(data_daily$watershed)))
data_daily$run <- factor(data_daily$run, 
                               levels = c("p301_100", "p301_80", "p301_50",
                                          "p303_100", "p303_80", "p303_50",
                                          "p304_100", "p304_80", "p304_50"))

# Season counter
data_daily <- data_daily %>% 
  dplyr::mutate(season = factor(case_when(WYD >= 0 & WYD <= 92 ~ "Oct-Dec",
                                   WYD >= 93 & WYD <= 182 ~ "Jan-Mar",
                                   WYD >= 183 & WYD <= 273 ~ "Apr-Jun",
                                   WYD >= 274 & WYD <= 365 ~ "Jul-Sep"),
                                levels = c("Jan-Mar",
                                           "Apr-Jun",
                                           "Jul-Sep",
                                           "Oct-Dec"))) %>% 
  dplyr::rename(SatArea = `%SatArea`, SCA = `%SCA`)


# Compute additional variables
# Water balance residual
data_daily <- data_daily %>% 
  dplyr::mutate(WB_Residual = Precip-Streamflow-Evap-Transp,
                Evap_neg = -1*Evap,
                Transp_neg = -1*Transp,
                Sat_sto = (5000 - SatDef_dep)*0.5,  # SatDef_Vol (SatDef_dep*0.5) is volume of space that is not saturated. Should be equal or greater than unsat + rz
                Total_sto = Sat_sto + RZ_sto + Unsat_Sto)

# Subsurface storage by depth
# data_daily %>% 
#   dplyr::mutate(happy = SatDef_dep)

# Get rid of erroneous values (WYD365 for P304 has no data for WY2014 for all three scenarios)
data_daily <- data_daily %>% 
  dplyr::filter(!(wy == 2014 & WYD == 365 & watershed == "p304"))

# Calculate canopy and litter evaporation
data_daily <- data_daily %>% 
  dplyr::group_by(run) %>% 
  dplyr::mutate(canopy_evap = if_else(Precip > 0.000001, 0, lag(CanopySto) - CanopySto),
                litter_evap = if_else(Precip > 0.000001, 0, lag(LitterSto) - LitterSto)) %>% 
  dplyr::ungroup()


# ---------------------------------------------------------------------
# Summarize data by wateryear and season

data_annual <- data_daily %>% 
  dplyr::group_by(wy, watershed, biomass) %>% 
  dplyr::summarise(Precip = sum(Precip),
                   Rainfall = sum(Rainfall),
                   Snowfall = sum(Snowfall),
                   SnowSubl = sum(SnowSubl),
                   del_snowpack = sum(del_snowpack),
                   Evap = sum(Evap),
                   Transp = sum(Transp),
                   PET = sum(PET),
                   Streamflow = sum(Streamflow),
                   Baseflow = sum(Baseflow),
                   ReturnFlow = sum(ReturnFlow),
                   GW_out = sum(GW_out),
                   RZ_drain = sum(RZ_drain),
                   Unsat_drain = sum(Unsat_drain),
                   CapRise = sum(CapRise),
                   R_Tfall = sum(R_Tfall),
                   S_Tfall = sum(S_Tfall),
                   Photosyn = sum(Photosyn),
                   WB_Residual = sum(WB_Residual),
                   Evap_neg = sum(Evap_neg),
                   Transp_neg = sum(Transp_neg),
                   canopy_evap = sum(canopy_evap, na.rm=TRUE),
                   litter_evap = sum(litter_evap, na.rm=TRUE),
                   
                   Tmax = mean(Tmax),
                   Tmin = mean(Tmin),
                   Snowpack = mean(Snowpack),
                   LitterSto = mean(LitterSto),
                   CanopySto = mean(CanopySto),
                   RZ_sto = mean(RZ_sto),
                   Unsat_Sto = mean(Unsat_Sto),
                   SatDef_dep = mean(SatDef_dep),
                   SatDef_Vol = mean(SatDef_Vol),
                   GW_sto = mean(GW_sto),
                   Det_sto = mean(Det_sto),
                   SatArea = mean(SatArea),
                   SCA = mean(SCA),
                   LAI = mean(LAI),
                   Sat_sto = mean(Sat_sto),
                   Total_sto = mean(Total_sto)
  )


data_seasonal <- data_daily %>% 
  dplyr::group_by(wy, season, watershed, biomass) %>% 
  dplyr::summarise(Precip = sum(Precip),
                   Rainfall = sum(Rainfall),
                   Snowfall = sum(Snowfall),
                   SnowSubl = sum(SnowSubl),
                   del_snowpack = sum(del_snowpack),
                   Evap = sum(Evap),
                   Transp = sum(Transp),
                   PET = sum(PET),
                   Streamflow = sum(Streamflow),
                   Baseflow = sum(Baseflow),
                   ReturnFlow = sum(ReturnFlow),
                   GW_out = sum(GW_out),
                   RZ_drain = sum(RZ_drain),
                   Unsat_drain = sum(Unsat_drain),
                   CapRise = sum(CapRise),
                   R_Tfall = sum(R_Tfall),
                   S_Tfall = sum(S_Tfall),
                   Photosyn = sum(Photosyn),
                   WB_Residual = sum(WB_Residual),
                   Evap_neg = sum(Evap_neg),
                   Transp_neg = sum(Transp_neg),
                   canopy_evap = sum(canopy_evap, na.rm=TRUE),
                   litter_evap = sum(litter_evap, na.rm=TRUE),
                   
                   Tmax = mean(Tmax),
                   Tmin = mean(Tmin),
                   Snowpack = mean(Snowpack),
                   LitterSto = mean(LitterSto),
                   CanopySto = mean(CanopySto),
                   RZ_sto = mean(RZ_sto),
                   Unsat_Sto = mean(Unsat_Sto),
                   SatDef_dep = mean(SatDef_dep),
                   SatDef_Vol = mean(SatDef_Vol),
                   GW_sto = mean(GW_sto),
                   Det_sto = mean(Det_sto),
                   SatArea = mean(SatArea),
                   SCA = mean(SCA),
                   LAI = mean(LAI),
                   Sat_sto = mean(Sat_sto),
                   Total_sto = mean(Total_sto)
  )


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Compute difference between thinning and baseline


# Functions
# See https://dplyr.tidyverse.org/articles/programming.html

# Function to pass a dynamic variable to dplyr::select
select_dynamic <- function(df, ...){
  group_var <- enquos(...)
  dplyr::select(df, !!! group_var)
}

# Function to pass dynamic variables to tidyr::spread
spread_dynamic <- function(df, key, value){
  key <- enquo(key)
  value <- enquo(value)
  tidyr::spread(df, key=(!!key), value = (!!value))
}

# Function to compute absolute and relative changes
absol_rel_change <- function(df){
  df %>% 
    dplyr::mutate(absolute_80 = `80`-`100`,
                  relative_80 = (`80`-`100`)/`100`*100,
                  absolute_50 = `50`-`100`,
                  relative_50 = (`50`-`100`)/`100`*100)
}

# ---------------------------------------------------------------------
# Process data to a list that has absolute and relative differences for each variable


var_standard_daily <- c("wy", "WYD", "biomass", "watershed")
var_standard_seasonal <- c("wy", "season", "biomass", "watershed")
var_standard_annual <- c("wy", "biomass", "watershed")

diff_flux_daily <- list()
diff_flux_seasonal <- list()
diff_flux_annual <- list()

diff_storage_daily <- list()
diff_storage_seasonal <- list()
diff_storage_annual <- list()

# Process fluxes
for (aa in seq_along(flux_var)){
  
  # Daily
  diff_flux_daily[[aa]] <- data_daily %>% 
    select_dynamic(c(var_standard_daily, flux_var[aa])) %>% 
    spread_dynamic(key = biomass, value = flux_var[aa]) %>% 
    absol_rel_change()
  
  # Seasonal
  diff_flux_seasonal[[aa]] <- data_seasonal %>% 
    select_dynamic(c(var_standard_seasonal, flux_var[aa])) %>% 
    spread_dynamic(key = biomass, value = flux_var[aa]) %>% 
    absol_rel_change()
  
  # Annual
  diff_flux_annual[[aa]] <- data_annual %>% 
    select_dynamic(c(var_standard_annual, flux_var[aa])) %>% 
    spread_dynamic(key = biomass, value = flux_var[aa]) %>% 
    absol_rel_change()
}

# Set flux names in list
diff_flux_daily <- setNames(diff_flux_daily, flux_var)
diff_flux_seasonal <- setNames(diff_flux_seasonal, flux_var)
diff_flux_annual <- setNames(diff_flux_annual, flux_var)

# 'Fix' absolute change for negative fluxes (used in Roger's water balance plot)
diff_flux_daily$Transp_neg <- diff_flux_daily$Transp_neg %>% 
  dplyr::mutate(absolute_80 = -1*absolute_80,
                absolute_50 = -1*absolute_50)
diff_flux_seasonal$Transp_neg <- diff_flux_seasonal$Transp_neg %>% 
  dplyr::mutate(absolute_80 = -1*absolute_80,
                absolute_50 = -1*absolute_50)
diff_flux_annual$Transp_neg <- diff_flux_annual$Transp_neg %>% 
  dplyr::mutate(absolute_80 = -1*absolute_80,
                absolute_50 = -1*absolute_50)

diff_flux_daily$Evap_neg <- diff_flux_daily$Evap_neg %>% 
  dplyr::mutate(absolute_80 = -1*absolute_80,
                absolute_50 = -1*absolute_50)
diff_flux_seasonal$Evap_neg <- diff_flux_seasonal$Evap_neg %>% 
  dplyr::mutate(absolute_80 = -1*absolute_80,
                absolute_50 = -1*absolute_50)
diff_flux_annual$Evap_neg <- diff_flux_annual$Evap_neg %>% 
  dplyr::mutate(absolute_80 = -1*absolute_80,
                absolute_50 = -1*absolute_50)



# Process Storages
for (aa in seq_along(storage_var)){
  
  # Daily
  diff_storage_daily[[aa]] <- data_daily %>% 
    select_dynamic(c(var_standard_daily, storage_var[aa])) %>% 
    spread_dynamic(key = biomass, value = storage_var[aa]) %>% 
    absol_rel_change()
  
  # Seasonal
  diff_storage_seasonal[[aa]] <- data_seasonal %>% 
    select_dynamic(c(var_standard_seasonal, storage_var[aa])) %>% 
    spread_dynamic(key = biomass, value = storage_var[aa]) %>% 
    absol_rel_change()
  
  # Annual
  diff_storage_annual[[aa]] <- data_annual %>% 
    select_dynamic(c(var_standard_annual, storage_var[aa])) %>% 
    spread_dynamic(key = biomass, value = storage_var[aa]) %>% 
    absol_rel_change()
}

# Set flux names in list
diff_storage_daily <- setNames(diff_storage_daily, storage_var)
diff_storage_seasonal <- setNames(diff_storage_seasonal, storage_var)
diff_storage_annual <- setNames(diff_storage_annual, storage_var)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Export

write_csv(data_daily, path="output/data_daily.csv")
write_csv(data_seasonal, path="output/data_seasonal.csv")
write_csv(data_annual, path="output/data_annual.csv")


write_rds(diff_flux_daily, path="output/diff_flux_daily.rds")
write_rds(diff_flux_seasonal, path="output/diff_flux_seasonal.rds")
write_rds(diff_flux_annual, path="output/diff_flux_annual.rds")

write_rds(diff_storage_daily, path="output/diff_storage_daily.rds")
write_rds(diff_storage_seasonal, path="output/diff_storage_seasonal.rds")
write_rds(diff_storage_annual, path="output/diff_storage_annual.rds")

