# Figures annual vegetation change water balance figures
# As of 2020-05-30, includes code for manuscript figures 3, 4 & 5



source("R/0_utilities.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Import data

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


# Change in detention store, canopy store, litter store, change in snowpack
ls(diff_flux_annual)
ls(diff_storage_annual)


diff_flux_annual_stacked <- diff_flux_annual %>% 
  dplyr::bind_rows(.id="flux")
diff_storage_annual_stacked <- diff_storage_annual %>% 
  dplyr::bind_rows(.id="flux")
diff_flux_daily_stacked <- diff_flux_daily %>% 
  dplyr::bind_rows(.id="flux")
diff_storage_daily_stacked <- diff_storage_daily %>% 
  dplyr::bind_rows(.id="flux")


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Quantify water made available from thinning


# Estimate of percent of total evaporation in RHESSys that is directly affected by thinning/vegetation change
# This number generated in 1.6
evap_coefficient <- 0.17
evap_coefficient <- 0.585    # Assumes all canopy evaporation and 1/2 soil, litter and snow evaporation is partitioned from thinned area

# Updated evap_coefficient
# Based on idea that adjustments from change in PET are evenly distributed over all components of E, including soil, litter, and snow E in the thinned area.
# Back calculates how much non-canopy E is in thinned area and reduces deltaE by that amount
# Need to do watershed accounting for thinned and unthinned areas separately.
# a=0.5 # Area
# (1 - (% flux from thinned area) / ((% flux from unthinned area) + (% flux from thinned area)))
# (1 - (a - (a * evap_coefficient)) / ((1 - a) + (a - (a * evap_coefficient))))
offset_80 <- (1-(0.2-(0.2*evap_coefficient)) / ((1-0.2) + (0.2-(0.2*evap_coefficient))))
offset_50 <- (1-(0.5-(0.5*evap_coefficient)) / ((1-0.5) + (0.5-(0.5*evap_coefficient))))

# New version: Not using yet, but actually may be useful.
evap_conserved_new <- diff_flux_annual_stacked %>% 
  dplyr::filter(flux == "Evap") %>% 
  dplyr::mutate(evap_80_conserved = (`100`*0.2*evap_coefficient)*offset_80,
                evap_50_conserved = (`100`*0.5*evap_coefficient)*offset_50)

# Old version: Without accounting for change in flux on thinned area due to PET
evap_conserved <- diff_flux_annual_stacked %>% 
  dplyr::filter(flux == "Evap") %>% 
  dplyr::mutate(evap_80_conserved = `100`*0.2*evap_coefficient,
                evap_50_conserved = `100`*0.5*evap_coefficient)

transp_conserved <- diff_flux_annual_stacked %>% 
  dplyr::filter(flux == "Transp") %>% 
  dplyr::mutate(transp_80_conserved = `100`*0.2,
                transp_50_conserved = `100`*0.5)



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Quantify how water is repartitioned after thinning


# New version: Not using. Posssible delete
evap_change_new <- diff_flux_annual_stacked %>% 
  dplyr::filter(flux == "Evap") %>% 
  dplyr::mutate(evap_80_allocated = (`80` - (`100`*(1 - evap_coefficient*0.2*offset_80))),
                evap_50_allocated = (`50` - (`100`*(1 - evap_coefficient*0.5*offset_50))))

# Old version: Calculate evap change 
evap_change <- diff_flux_annual_stacked %>% 
  dplyr::filter(flux == "Evap") %>% 
  dplyr::mutate(evap_80_allocated = `80` - (`100`*(1 - evap_coefficient*0.2)),
                evap_50_allocated = `50` - (`100`*(1 - evap_coefficient*0.5)))
                

# Calculate transpiration change
transp_change <- diff_flux_annual_stacked %>% 
  dplyr::filter(flux == "Transp") %>% 
  dplyr::mutate(transp_80_allocated = `80` - (`100`*0.8),
                transp_50_allocated = `50` - (`100`*0.5))


# Calculate streamflow change
streamflow_change <- diff_flux_annual_stacked %>% 
  dplyr::filter(flux == "Streamflow") %>% 
  dplyr::mutate(streamflow_80_allocated = `80` - `100`,
                streamflow_50_allocated = `50` - `100`)


# Calculate soil storage change
storage_change_soil <- diff_storage_daily$Total_sto %>% 
  dplyr::filter(WYD %in% c(1, 364)) %>%                  # Compare storage on the first and last data of wateryear (364 used because of some missing data with WYD365)
  dplyr::group_by(watershed) %>% 
  dplyr::mutate(lag_50 = lag(`50`),
                lag_80 = lag(`80`),
                lag_100 = lag(`100`),
                soilstorage_50_allocated = (`50`-lag_50) - (`100`-lag_100),
                soilstorage_80_allocated = (`80`-lag_80) - (`100`-lag_100)) %>% 
  dplyr::filter(WYD %in% c(364))


# Calculate gw storage change
storage_change_gw <- diff_storage_daily$GW_sto %>% 
  dplyr::filter(WYD %in% c(1, 364)) %>%                  # Compare storage on the first and last data of wateryear (364 used because of some missing data with WYD365)
  dplyr::group_by(watershed) %>% 
  dplyr::mutate(lag_50 = lag(`50`),
                lag_80 = lag(`80`),
                lag_100 = lag(`100`),
                gwstorage_50_allocated = (`50`-lag_50) - (`100`-lag_100),
                gwstorage_80_allocated = (`80`-lag_80) - (`100`-lag_100)) %>% 
  dplyr::filter(WYD %in% c(364))


# Calculate total storage change
storage_change <- storage_change_soil %>%
  dplyr::select(wy, WYD, watershed, soilstorage_50_allocated, soilstorage_80_allocated) %>% 
  dplyr::full_join(., dplyr::select(storage_change_gw, wy, WYD, watershed,
                gwstorage_50_allocated, gwstorage_80_allocated),
                by = c("wy", "WYD", "watershed")) %>% 
  dplyr::mutate(storage_50_allocated = gwstorage_50_allocated + soilstorage_50_allocated,
                storage_80_allocated = gwstorage_80_allocated + soilstorage_80_allocated)
  


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Bring all components of vegetation change water balance together 


# Conserved water
flux_conserved <- dplyr::full_join(dplyr::select(evap_conserved, c(wy, watershed, evap_80_conserved, evap_50_conserved)), 
                                   dplyr::select(transp_conserved, c(wy, watershed, transp_80_conserved, transp_50_conserved)), 
                                   by=c("wy", "watershed")) %>% 
  dplyr::mutate(total_80_conserved = evap_80_conserved + transp_80_conserved,
                total_50_conserved = evap_50_conserved + transp_50_conserved)

flux_conserved <- flux_conserved %>% 
  dplyr::left_join(., dplyr::distinct(dplyr::select(data_annual, wy, watershed, Precip)), by=c("wy", "watershed"))

flux_conserved_long <- flux_conserved %>% 
  dplyr::select(-c(total_80_conserved, total_50_conserved)) %>% 
  tidyr::pivot_longer(names_to = "flux_level", values_to = "value", -c(wy,watershed, Precip)) %>% 
  tidyr::separate(flux_level, into=c("flux", "scenario", "balance_component"))


# ----
# Allocated water
flux_allocated <- dplyr::full_join(dplyr::select(evap_change, wy, watershed, evap_80_allocated, evap_50_allocated), 
                                   dplyr::select(transp_change, wy, watershed, transp_80_allocated, transp_50_allocated), 
                                   by=c("wy", "watershed"))

flux_allocated <- dplyr::full_join(flux_allocated, 
                                   dplyr::select(streamflow_change, wy, watershed, streamflow_80_allocated, streamflow_50_allocated), 
                                   by=c("wy", "watershed"))

flux_allocated <- dplyr::full_join(flux_allocated,
                                   dplyr::select(storage_change, wy, watershed, storage_80_allocated, storage_50_allocated),
                                   by=c("wy", "watershed"))

# Create percentages
flux_allocated <- flux_allocated %>% 
  dplyr::mutate(evap_50_percent = evap_50_allocated/(evap_50_allocated + transp_50_allocated +
                                                           streamflow_50_allocated + storage_50_allocated),
                transp_50_percent = transp_50_allocated/(evap_50_allocated + transp_50_allocated +
                                                           streamflow_50_allocated + storage_50_allocated),
                streamflow_50_percent = streamflow_50_allocated/(evap_50_allocated + transp_50_allocated +
                                                           streamflow_50_allocated + storage_50_allocated),
                storage_50_percent = storage_50_allocated/(evap_50_allocated + transp_50_allocated +
                                                           streamflow_50_allocated + storage_50_allocated),
                evap_80_percent = evap_80_allocated/(evap_80_allocated + transp_80_allocated +
                                                         streamflow_80_allocated + storage_80_allocated),
                transp_80_percent = transp_80_allocated/(evap_80_allocated + transp_80_allocated +
                                                           streamflow_80_allocated + storage_80_allocated),
                streamflow_80_percent = streamflow_80_allocated/(evap_80_allocated + transp_80_allocated +
                                                               streamflow_80_allocated + storage_80_allocated),
                storage_80_percent = storage_80_allocated/(evap_80_allocated + transp_80_allocated +
                                                            streamflow_80_allocated + storage_80_allocated))


# Add precipitation to results
flux_allocated <- flux_allocated %>% 
  dplyr::left_join(., dplyr::distinct(dplyr::select(data_annual, wy, watershed, Precip)), by=c("wy", "watershed"))

# Separate the flux from the scenario
flux_allocated_long <- tidyr::pivot_longer(flux_allocated, names_to = "flux_level", values_to = "value", -c(wy,watershed, Precip)) %>% 
  tidyr::separate(flux_level, into=c("flux", "scenario", "balance_component"))


# ----
# Combine conserved and allocated water
veg_change_water_balance <- bind_rows(flux_conserved_long, flux_allocated_long)
veg_change_water_balance$flux <- factor(veg_change_water_balance$flux, levels = c("storage", "streamflow", "evap", "transp"))
veg_change_water_balance$scenario <- factor(veg_change_water_balance$scenario, levels = c("80", "50"))



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Make vegetation change water balance figure (Fig 3)

scenario_id <- c(
  "80" = "20% Thinning Scenario",
  "50" = "50% Thinning Scenario"
)

shed_id <- c(
  "p301" = "P301",
  "p303" = "P303",
  "p304" = "P304"
)

# Plot of the water conserved
x <- veg_change_water_balance %>% 
  dplyr::filter(balance_component=="conserved") %>% 
  ggplot() +
  geom_col(mapping=aes(x=wy,y=value, fill=flux), width = .7, position = "stack") +
  facet_grid(watershed~scenario) +
  scale_fill_manual(name = "Flux", values = c("#b2df8a","#1f78b4","#a6cee3","#33a02c"),
                    labels = c("Change in\nStorage/Leakage", "Streamflow", "Evaporation", "Transpiration")) +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 270, hjust=0, vjust=0.6),
        legend.position = "right") +
  NULL
#plot(x)


# Plot of the reallocated water
x <- veg_change_water_balance %>% 
  dplyr::filter(balance_component=="allocated") %>% 
  ggplot() +
  geom_col(mapping=aes(x=wy,y=value, fill=flux), width = .7, position = "stack") +
  facet_grid(watershed~scenario) +
  scale_fill_manual(name = "Flux", values = c("#b2df8a","#1f78b4","#a6cee3","#33a02c"),
                    labels = c("Change in\nStorage/Leakage", "Streamflow", "Evaporation", "Transpiration")) +
  labs(title = "Allocation of 'conserved' water from thinning", x="Water Year", y="Change in flux (mm)") +
  theme_bw(base_size = 11) +
  labs(title="Vegetation Change Water Balance (left side of equation)", x="Water Year", y="Change in Hydrologic Flux (mm)") +
  theme(axis.text.x = element_text(angle = 270, hjust=0, vjust=0.6),
        legend.position = "right") +
  NULL
#plot(x)



# Plot conserved and reallocated water together
veg_change_water_balance1 <- veg_change_water_balance
veg_change_water_balance1$wy <- as.numeric(veg_change_water_balance1$wy)
x <- ggplot() +
  geom_col(data=dplyr::filter(veg_change_water_balance1, balance_component=="allocated"),
           mapping=aes(x=wy+0.17,y=value, fill=flux), width = .3, position = "stack") +
  geom_col(data=dplyr::filter(veg_change_water_balance1, balance_component=="conserved"),
           mapping=aes(x=wy-0.17,y=value, fill=flux), width = .3, position = "stack") +
  facet_grid(watershed~scenario, labeller = labeller(scenario = scenario_id,
                                                     watershed = shed_id)) +
  scale_fill_manual(name = "Flux", values = c("#b2df8a","#1f78b4","#a6cee3","#33a02c"),
                    labels = c("Change in\nStorage/Leakage", "Streamflow", "Evaporation", "Transpiration")) +
  scale_x_continuous(breaks = seq(2004,2014), labels = seq(2004,2014)) +
  labs(title="Vegetation Change Water Balance", x="Water year", y="Change in hydrologic flux (mm)") +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 270, hjust=0, vjust=0.6),
        legend.position = "bottom") +
  NULL
plot(x)

ggsave("output/manuscript_plots/plot_veg_change_water_balance.jpg", plot=x, width = 8, height = 6)



# ----
# Plot conserved and reallocated water together (watersheds combined)

veg_change_water_balance1 <- veg_change_water_balance %>% 
  dplyr::filter(balance_component!="percent") %>% 
  dplyr::mutate(tmp = paste(flux,balance_component, sep="_")) %>% 
  dplyr::mutate(flux_comp = case_when(tmp == "evap_conserved" ~ paste("a1",tmp, sep="_"),
                                      tmp == "transp_conserved" ~ paste("a2",tmp, sep="_"),
                                      tmp == "storage_allocated" ~ paste("a3",tmp, sep="_"),
                                      tmp == "streamflow_allocated" ~ paste("a4",tmp, sep="_"),
                                      tmp == "evap_allocated" ~ paste("a5",tmp, sep="_"),
                                      tmp == "transp_allocated" ~ paste("a6",tmp, sep="_"))) %>% 
  dplyr::group_by(wy, flux, scenario, balance_component, flux_comp) %>% 
  dplyr::summarise(value = mean(value))
veg_change_water_balance1$wy <- as.numeric(veg_change_water_balance1$wy)
veg_change_water_balance1$flux_comp <- factor(veg_change_water_balance1$flux_comp,
                                              levels = c("a1_evap_conserved", "a2_transp_conserved",
                                                         "a3_storage_allocated", "a4_streamflow_allocated",
                                                         "a5_evap_allocated", "a6_transp_allocated"))    # The a1, a2, etc is because I couldn't get order correct below so renamed alphbetically.


x <- ggplot() +
  geom_col(data=dplyr::filter(veg_change_water_balance1, balance_component=="conserved"),
           mapping=aes(x=wy-0.17,y=value, fill=flux_comp), width = .3, position = "stack") +
  geom_col(data=dplyr::filter(veg_change_water_balance1, balance_component=="allocated"),
           mapping=aes(x=wy+0.17,y=value, fill=flux_comp), width = .3, position = "stack") +
  facet_grid(scenario~., labeller = labeller(scenario = scenario_id)) +
  scale_fill_manual(name = "Flux", 
                    # https://personal.sron.nl/~pault/#sec:qualitative 
                    # Bright: Red, blue, green, cyan, yellow, purple
                    #values = c("#EE6677","#4477AA", "#228833","#66CCEE","#CCBB44","#AA3377"),
                    # Bright: Cyan, purple, blue, red, yellow, green                    
                    #values = c("#66CCEE","#AA3377","#4477AA","#EE6677","#CCBB44","#228833"),
                    # Bright: Purple, cyan, red, blue, yellow, green                    
                    #values = c("#AA3377","#66CCEE","#EE6677","#4477AA","#CCBB44","#228833"),
                    # Bright: Purple, red, cyan, blue, yellow, green                    
                    #values = c("#AA3377","#EE6677","#66CCEE","#4477AA","#CCBB44","#228833"),
                    # Bright: Purple, red, cyan, blue, green, yellow                    
                    values = colors_bright_6,
                    
                    labels = c(expression('Affected Evaporation ('*E[a]*')'),
                               expression('Affected Transpiration ('*T[a]*')'),
                               expression('Change in Storage (d'*S[w]*')'), 
                               expression('Streamflow ('*Q[w]*')'),
                               expression('Unaffected Evaporation ('*E[u]*')'), 
                               expression('Unaffected Transpiration ('*T[u]*')'))) +
  scale_x_continuous(breaks = seq(2004,2014), labels = seq(2004,2014)) +
  labs(x="Water year", y="Change in post-treatment annual flux, mm") +
  theme_bw(base_size = 11) +
  theme(legend.text.align = 0,
        legend.position = "bottom") +
  NULL
#plot(x)

ggsave("output/manuscript_plots/plot_veg_change_water_balance_combined.jpg", plot=x, width = 6.5, height = 5)





# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Compare partitioning relative to precipitation (Fig 4)

flux_id <- c(
  "storage" = "Change in Storage",
  "streamflow" = "Streamflow",
  "evap" = "Evaporation",
  "transp" = "Transpiration"
)
shed_id <- c(
  "p301" = "P301",
  "p303" = "P303",
  "p304" = "P304"
)


# By percent for 50% scenario
x <- veg_change_water_balance %>% 
  dplyr::filter(balance_component=="percent", scenario=="50") %>% 
  ggplot(data=.) +
  geom_point(aes(x=Precip, y=value*100)) +
  geom_smooth(aes(x=Precip, y=value*100), method='lm') +
  facet_grid(watershed~flux,
             labeller = labeller(watershed = shed_id,
                                 flux = flux_id)) +
  labs(title = "Partitioning of Conserved Water: 50% Thinning Scenario",
       x="Annual Precipitation (mm)",
       y="Percent of Total Post-Treatment Flux Change (%)") +
  theme_bw(base_size = 11) +
  NULL
plot(x)

ggsave("output/manuscript_plots/plot_precip_vs_flux_change_50.jpg", plot=x, width = 8, height = 6)



# By amount for 50% scenario (watersheds combined)
veg_change_water_balance1 <- veg_change_water_balance
veg_change_water_balance1$flux <- factor(veg_change_water_balance1$flux,
                                         labels = c(
                                           "storage" = expression('Change in Storage (d'*S[w]*')'),
                                           "streamflow" = expression('Streamflow ('*Q[w]*')'),
                                           "evap" = expression('Evaporation ('*E[u]*')'),
                                           "transp" = expression('Transpiration ('*T[u]*')')
                                         ))

x <- veg_change_water_balance1 %>% 
  dplyr::group_by(wy, flux, scenario, balance_component) %>% 
  dplyr::summarise(value = mean(value),
                   Precip = mean(Precip)) %>% 
  dplyr::filter(balance_component=="allocated", scenario=="50") %>% 
  ggplot(data=.) +
  geom_point(aes(x=Precip, y=value)) +
  # geom_smooth(aes(x=Precip, y=value), method='lm') +
  geom_smooth(aes(x=Precip, y=value), method='glm', formula = y ~ I(x^2)) +
  # geom_smooth(aes(x=Precip, y=value), span=3) +
  facet_wrap(flux~.,
             labeller = labeller(flux = label_parsed)) +
  labs(x="Annual Precipitation (mm)",
       y="Change in Post-Treatment Annual Flux (mm)") +
  theme_bw(base_size = 11) +
  NULL
plot(x)

ggsave("output/manuscript_plots/plot_precip_vs_flux_change_50_combined.jpg", plot=x, width = 5, height = 4)




# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Relative benefit of doing 50% thinning vs 20% thinning (Bang for your buck) (Fig. 5)


veg_change_water_balance1 <- veg_change_water_balance
veg_change_water_balance1$flux <- factor(veg_change_water_balance1$flux,
                                         labels = c(
                                           "storage" = expression('Delta Storage (d'*S[w]*')'),
                                           "streamflow" = expression('Streamflow ('*Q[w]*')'),
                                           "evap" = expression('Evaporation ('*E[u]*')'),
                                           "transp" = expression('Transpiration ('*T[u]*')')
                                         ))

# Normalized to 1% treated area.
happy <- veg_change_water_balance %>% 
  dplyr::filter(balance_component=="allocated", flux %in% c("streamflow", "transp")) %>% 
  tidyr::pivot_wider(names_from = scenario, values_from = value) %>% 
  dplyr::mutate(`50_normalized` = `50`*(1/50),      # Flux yield per 1% treated area in watershed
                `80_normalized` = `80`*(1/20),      # Flux yield per 1% treated area in watershed
                flux_change_normalized1 = `50_normalized`/`80_normalized`*100-100,
                flux_change_normalized2 = `50_normalized` - `80_normalized`) %>% 
  tidyr::pivot_longer(cols = c(`50_normalized`, `80_normalized`), names_to = "normalized_names", values_to = "normalized_values")
happy$normalized_names <- factor(happy$normalized_names, levels = c("80_normalized", "50_normalized"))
happy$flux <- factor(happy$flux, levels = c("transp", "streamflow"),
                     labels = c("transp" = expression('Transpiration ('*T[u]*')'),"streamflow" = expression('Streamflow ('*Q[w]*')')))

happy_mean <- happy %>% 
  dplyr::group_by(watershed, flux, normalized_names) %>% 
  dplyr::summarize(flux_mean = mean(normalized_values))
  
x <- ggplot() +
  geom_col(data=happy, mapping=aes(x=wy,y=normalized_values, fill=normalized_names), width = .85,  position = "dodge") +
  geom_hline(data=happy_mean, aes(yintercept = flux_mean, color=normalized_names)) +
  facet_grid(watershed~flux,
             labeller = labeller(watershed = shed_id,
                                 flux = label_parsed)) +
  scale_fill_manual(name = "Scenario", values = colors_vibrant_2,
                    labels = c("20% Thinning", "50% Thinning")) +
  scale_color_manual(name = "Scenario", values = colors_vibrant_2,
                    labels = c("20% Thinning", "50% Thinning")) +
  labs(x="Water Year",
       y="Flux yield per 1% thinned (mm)") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom") +
  NULL
#plot(x)

ggsave("output/manuscript_plots/plot_bang_for_your_buck.jpg", plot=x, width = 8, height = 6)



# Normalized to 1% treated area (Watersheds combined).
happy <- veg_change_water_balance %>% 
  dplyr::group_by(wy, flux, scenario, balance_component) %>% 
  dplyr::summarise(value = mean(value), Precip = mean(Precip)) %>% 
  dplyr::filter(balance_component=="allocated", flux %in% c("streamflow", "transp")) %>% 
  tidyr::pivot_wider(names_from = scenario, values_from = value) %>% 
  dplyr::mutate(`50_normalized` = `50`*(1/50),      # Flux yield per 1% treated area in watershed
                `80_normalized` = `80`*(1/20),      # Flux yield per 1% treated area in watershed
                flux_change_normalized1 = `50_normalized`/`80_normalized`*100-100,
                flux_change_normalized2 = `50_normalized` - `80_normalized`) %>% 
  tidyr::pivot_longer(cols = c(`50_normalized`, `80_normalized`), names_to = "normalized_names", values_to = "normalized_values")
happy$normalized_names <- factor(happy$normalized_names, levels = c("80_normalized", "50_normalized"))
happy$flux <- factor(happy$flux, levels = c("transp", "streamflow"),
                     labels = c("transp" = expression('Transpiration ('*T[u]*')'),"streamflow" = expression('Streamflow ('*Q[w]*')')))

happy_mean <- happy %>% 
  dplyr::group_by(flux, normalized_names) %>% 
  dplyr::summarize(flux_mean = mean(normalized_values))
print(happy_mean)

x <- ggplot() +
  geom_col(data=happy, mapping=aes(x=wy,y=normalized_values, fill=normalized_names), width = .85,  position = "dodge") +
  geom_hline(data=happy_mean, aes(yintercept = flux_mean, color=normalized_names)) +
  facet_grid(flux~.,
             labeller = labeller(flux = label_parsed)) +
  scale_fill_manual(name = "Scenario", values = colors_vibrant_2,
                    labels = c("20% Thinning", "50% Thinning")) +
  scale_color_manual(name = "Scenario", values = colors_vibrant_2,
                     labels = c("20% Thinning", "50% Thinning")) +
  labs(x="Water Year",
       y="Annual Flux Change per 1% Thinned (mm)") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom") +
  NULL
#plot(x)

ggsave("output/manuscript_plots/plot_bang_for_your_buck_combined.jpg", plot=x, width = 6.5, height = 5)







# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Save outputs

write_csv(veg_change_water_balance, path="output/veg_change_water_balance.csv")





