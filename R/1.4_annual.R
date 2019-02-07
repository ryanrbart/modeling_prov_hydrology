# Plot annual data


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
#


# Transpiration
x <- data_seasonal %>% 
  ggplot(.) +
  geom_col(aes(x=wy, y=Transp, fill=biomass), position="dodge") +
  facet_grid(watershed~.) +
  NULL
plot(x)


# Evap
x <- data_seasonal %>% 
  ggplot(.) +
  geom_col(aes(x=wy, y=Evap, fill=biomass), position="dodge") +
  facet_grid(watershed~.) +
  NULL
plot(x)


# Streamflow
x <- data_seasonal %>% 
  ggplot(.) +
  geom_col(aes(x=wy, y=Streamflow, fill=biomass), position="dodge") +
  facet_grid(watershed~.) +
  NULL
plot(x)





# ---------------------------------------------------------------------
# Stack the key flux variables

data_annual_stacked <- data_annual %>% 
  dplyr::mutate(Residual = Precip-Streamflow-Evap-Transp) %>% 
  tidyr::gather(Streamflow, Evap, Transp, Residual, key="flux", value="flux_value") %>% 
  mutate(x_label = factor(str_replace(interaction(wy, biomass), '\\.', ' / '), ordered=TRUE))

x <- data_annual_stacked %>% 
  #dplyr::filter(watershed == "p301") %>% 
  ggplot(.) +
  geom_col(aes(x=x_label,y=flux_value, fill=flux), width = .7, position = "stack") +
  #facet_grid(watershed~.) +
  theme(axis.text.x = element_text(angle = 270, hjust=0, vjust=0.6)) +
  NULL
plot(x)




