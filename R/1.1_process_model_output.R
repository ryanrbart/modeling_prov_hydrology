# Import model output and process
# Data originally in SigmaPlot


source("R/0_utilities.R")

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

# Season counter
data_daily <- data_daily %>% 
  dplyr::mutate(season = factor(case_when(WYD >= 0 & WYD <= 92 ~ "Oct-Dec",
                                   WYD >= 93 & WYD <= 182 ~ "Jan-Mar",
                                   WYD >= 183 & WYD <= 273 ~ "Apr-Jun",
                                   WYD >= 274 & WYD <= 365 ~ "Jul-Sep")))

# ---------------------------------------------------------------------
# Summarize data by wateryear and season

data_annual <- data_daily %>% 
  dplyr::group_by(wy, watershed, biomass) %>% 
  dplyr::summarise(Snowfall = sum(Snowfall),
                   SnowSubl = sum(SnowSubl),
                   del_snowpack = sum(del_snowpack),
                   Streamflow = sum(Streamflow),
                   Evap = sum(Evap),
                   Transp = sum(Transp),
                   Precip = sum(Precip),
                   PET = sum(PET),
                   Baseflow = sum(Baseflow),
                   ReturnFlow = sum(ReturnFlow),
                   Rainfall = sum(Rainfall),
                   RZ_drain = sum(RZ_drain),
                   Unsat_drain = sum(Unsat_drain),
                   CapRise = sum(CapRise),
                   GW_out = sum(GW_out),
                   R_Tfall = sum(R_Tfall),
                   S_Tfall = sum(S_Tfall),
                   Photosyn = sum(Photosyn),
                   
                   Tmax = mean(Tmax),
                   Tmin = mean(Tmin),
                   GW_sto = mean(GW_sto),
                   Snowpack = mean(Snowpack),
                   SatDef_dep = mean(SatDef_dep),
                   SatDef_Vol = mean(SatDef_Vol),
                   Unsat_Sto = mean(Unsat_Sto),
                   RZ_sto = mean(RZ_sto),
                   LitterSto = mean(LitterSto),
                   CanopySto = mean(CanopySto),
                   RZ_sto = mean(RZ_sto),
                   Det_sto = mean(Det_sto),
                   `%SatArea` = mean(`%SatArea`),
                   `%SCA` = mean(`%SCA`),
                   LAI = mean(LAI)
  )


data_seasonal <- data_daily %>% 
  dplyr::group_by(wy, season, watershed, biomass) %>% 
  dplyr::summarise(Snowfall = sum(Snowfall),
                   SnowSubl = sum(SnowSubl),
                   del_snowpack = sum(del_snowpack),
                   Streamflow = sum(Streamflow),
                   Evap = sum(Evap),
                   Transp = sum(Transp),
                   Precip = sum(Precip),
                   PET = sum(PET),
                   Baseflow = sum(Baseflow),
                   ReturnFlow = sum(ReturnFlow),
                   Rainfall = sum(Rainfall),
                   RZ_drain = sum(RZ_drain),
                   Unsat_drain = sum(Unsat_drain),
                   CapRise = sum(CapRise),
                   GW_out = sum(GW_out),
                   R_Tfall = sum(R_Tfall),
                   S_Tfall = sum(S_Tfall),
                   Photosyn = sum(Photosyn),
                   
                   Tmax = mean(Tmax),
                   Tmin = mean(Tmin),
                   GW_sto = mean(GW_sto),
                   Snowpack = mean(Snowpack),
                   SatDef_dep = mean(SatDef_dep),
                   SatDef_Vol = mean(SatDef_Vol),
                   Unsat_Sto = mean(Unsat_Sto),
                   RZ_sto = mean(RZ_sto),
                   LitterSto = mean(LitterSto),
                   CanopySto = mean(CanopySto),
                   RZ_sto = mean(RZ_sto),
                   Det_sto = mean(Det_sto),
                   `%SatArea` = mean(`%SatArea`),
                   `%SCA` = mean(`%SCA`),
                   LAI = mean(LAI)
  )


# ---------------------------------------------------------------------
# Export

write_csv(data_daily, path="output/data_daily.csv")
write_csv(data_seasonal, path="output/data_seasonal.csv")
write_csv(data_annual, path="output/data_annual.csv")


