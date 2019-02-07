# Plot seasonal data


source("R/0_utilities.R")

# Wettest year: 2011
# Dryest year: 2014

# ---------------------------------------------------------------------
#Read in processed data

data_daily <- read_csv("output/data_daily.csv",
                       col_types = list(wy = col_character(),
                                        run = col_factor(),
                                        season = col_factor(levels = c("Jan-Mar",
                                                                       "Apr-Jun",
                                                                       "Jul-Sep",
                                                                       "Oct-Dec")),
                                        biomass = col_factor(),
                                        watershed = col_factor()))
data_seasonal <- read_csv("output/data_seasonal.csv",
                          col_types = list(wy = col_character(),
                                           season = col_factor(levels = c("Jan-Mar",
                                                                          "Apr-Jun",
                                                                          "Jul-Sep",
                                                                          "Oct-Dec")),
                                           biomass = col_factor(),
                                           watershed = col_factor()))
data_annual <- read_csv("output/data_annual.csv",
                        col_types = list(wy = col_character(),
                                         watershed = col_factor(),
                                         biomass = col_factor()))


# ---------------------------------------------------------------------
#

response_variable_lab <- c("prop_c_mort" = "prop_c_\nmort",
                           "prop_mort_consumed" = "prop_mort_\nconsumed",
                           "prop_c_consumed" = "prop_c_\nconsumed",
                           "prop_c_residual" = "prop_c_\nresidual")

# Transpiration
x <- data_seasonal %>% 
  dplyr::filter(watershed == "p301") %>% 
  ggplot(.) +
  geom_col(aes(x=wy, y=Transp, fill=biomass), position="dodge") +
  facet_grid(season~.) +
  NULL
plot(x)


# Evap
x <- data_seasonal %>% 
  dplyr::filter(watershed == "p301") %>% 
  ggplot(.) +
  geom_col(aes(x=wy, y=Evap, fill=biomass), position="dodge") +
  facet_grid(season~.) +
  NULL
plot(x)



# Streamflow
x <- data_seasonal %>% 
  dplyr::filter(watershed == "p301") %>% 
  ggplot(.) +
  geom_col(aes(x=wy, y=Streamflow, fill=biomass), position="dodge") +
  facet_grid(season~.) +
  NULL
plot(x)

