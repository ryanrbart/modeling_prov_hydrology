# Plot daily data


source("R/0_utilities.R")

# Wettest year: 2011
# Dryest year: 2014

# ---------------------------------------------------------------------
#Read in processed data

data_daily <- read_csv("output/data_daily.csv",
                       col_types = list(wy = col_character(),
                                        run = col_factor(),
                                        season = col_factor(),
                                        biomass = col_factor(),
                                        watershed = col_factor()))
data_seasonal <- read_csv("output/data_seasonal.csv",
                          col_types = list(wy = col_character(),
                                           season = col_factor(),
                                           biomass = col_factor(),
                                           watershed = col_factor()))
data_annual <- read_csv("output/data_annual.csv",
                        col_types = list(wy = col_character(),
                                         watershed = col_factor(),
                                         biomass = col_factor()))


# ---------------------------------------------------------------------
# Compare wettest and driest years

# Transpiration
x <- data_daily %>% 
  dplyr::filter(watershed == "p301", wy %in% c(2011,2014)) %>% 
  ggplot(.) +
  geom_line(aes(x=WYD, y=`Cum_trans`, col=biomass)) +
  facet_grid(wy~.) +
  NULL
plot(x)


# Evap
x <- data_daily %>% 
  dplyr::filter(watershed == "p301", wy %in% c(2011,2014)) %>% 
  ggplot(.) +
  geom_line(aes(x=WYD, y=Evap, col=biomass)) +
  facet_grid(wy~.) +
  NULL
plot(x)


# PET
x <- data_daily %>% 
  dplyr::filter(watershed == "p301", wy %in% c(2011,2014)) %>% 
  ggplot(.) +
  geom_line(aes(x=WYD, y=PET, col=biomass)) +
  facet_grid(wy~.) +
  NULL
plot(x)


# RZ_storage
x <- data_daily %>% 
  dplyr::filter(watershed == "p301", wy %in% c(2011,2014)) %>% 
  ggplot(.) +
  geom_line(aes(x=WYD, y=RZ_sto, col=biomass)) +
  facet_grid(wy~.) +
  NULL
plot(x)


# Streamflow
x <- data_daily %>% 
  dplyr::filter(wy %in% c(2011,2014)) %>% 
  ggplot(.) +
  geom_line(aes(x=WYD, y=Streamflow, col=biomass)) +
  facet_grid(wy~watershed) +
  NULL
plot(x)




# Make plot with the biomass differences!
  



