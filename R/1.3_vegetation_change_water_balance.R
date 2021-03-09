# Figures for annual vegetation change water balance
# Includes code for manuscript figures 4, 5, & 6



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
# Quantify water made available from thinning aka left half of Equation 4

# Calculate evaporation removed
evap_rmv <- diff_flux_annual_stacked %>% 
  dplyr::filter(flux %in% c("Evap","canopy_evap")) %>% 
  dplyr::select(-c("absolute_80", "absolute_50", "relative_80", "relative_50")) %>% 
  pivot_wider(names_from = "flux", values_from = c(`50`, `80`, `100`)) %>% 
  dplyr::rename("evap_50" = `50_Evap`, "evap_80" = `80_Evap`, "evap_100" = `100_Evap`,
                "can_evap_50" = `50_canopy_evap`, "can_evap_80" = `80_canopy_evap`, "can_evap_100" = `100_canopy_evap`) %>% 
  # Calculate evaporation made available from canopy evaporation
  dplyr::mutate(Ec_80_rmv_pre = can_evap_100*0.2,
                Ec_50_rmv_pre = can_evap_100*0.5,
                delta_Ec_80_rmv = 0 - Ec_80_rmv_pre,
                delta_Ec_50_rmv = 0 - Ec_50_rmv_pre) %>% 
  # Calculate evaporation made available from litter, soil, and snow evaporation: 20% thinning
  dplyr::mutate(Enc_80_rmv_pre = (evap_100-can_evap_100) * (0.2),
                Enc_80_rmv_post = Enc_80_rmv_pre * (evap_80/(evap_100 - Ec_80_rmv_pre)),
                delta_Enc_80_rmv = Enc_80_rmv_post - Enc_80_rmv_pre) %>% 
  # Calculate evaporation made available from litter, soil, and snow evaporation: 50% thinning
  dplyr::mutate(Enc_50_rmv_pre = (evap_100-can_evap_100) * (0.5),
                Enc_50_rmv_post = Enc_50_rmv_pre * (evap_50/(evap_100 - Ec_50_rmv_pre)),
                delta_Enc_50_rmv = Enc_50_rmv_post - Enc_50_rmv_pre) %>% 
  dplyr::mutate(delta_evap_80_rmv = delta_Ec_80_rmv + delta_Enc_80_rmv,
                delta_evap_50_rmv = delta_Ec_50_rmv + delta_Enc_50_rmv)   

# Calculate transpiration removed
transp_rmv <- diff_flux_annual_stacked %>% 
  dplyr::filter(flux == "Transp") %>% 
  dplyr::select(-c("absolute_80", "absolute_50", "relative_80", "relative_50")) %>% 
  dplyr::mutate(delta_transp_80_rmv = 0 - `100`*0.2,
                delta_transp_50_rmv = 0 - `100`*0.5)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Quantify how water is repartitioned after thinning (right half of equation 4)

# Calculate evaporation remain
evap_rmn <- diff_flux_annual_stacked %>% 
  dplyr::filter(flux %in% c("Evap","canopy_evap")) %>% 
  dplyr::select(-c("absolute_80", "absolute_50", "relative_80", "relative_50")) %>% 
  pivot_wider(names_from = "flux", values_from = c(`50`, `80`, `100`)) %>% 
  dplyr::rename("evap_50" = `50_Evap`, "evap_80" = `80_Evap`, "evap_100" = `100_Evap`,
                "can_evap_50" = `50_canopy_evap`, "can_evap_80" = `80_canopy_evap`, "can_evap_100" = `100_canopy_evap`) %>% 
  # Calculate evaporation made available from litter, soil, and snow evaporation: 20% thinning
  dplyr::mutate(Ec_80_rmv_pre = can_evap_100*0.2,
                E_80_rmn_pre = (evap_100) * (1-0.2),
                E_80_rmn_post = E_80_rmn_pre * (evap_80/(evap_100 - Ec_80_rmv_pre)),
                delta_evap_80_rmn = E_80_rmn_post - E_80_rmn_pre) %>% 
  # Calculate evaporation made available from litter, soil, and snow evaporation: 50% thinning
  dplyr::mutate(Ec_50_rmv_pre = can_evap_100*0.5,
                E_50_rmn_pre = (evap_100) * (1-0.5),
                E_50_rmn_post = E_50_rmn_pre * (evap_50/(evap_100 - Ec_50_rmv_pre)),
                delta_evap_50_rmn = E_50_rmn_post - E_50_rmn_pre)


# Calculate transpiration remain
transp_rmn <- diff_flux_annual_stacked %>% 
  dplyr::filter(flux == "Transp") %>% 
  dplyr::select(-c("absolute_80", "absolute_50", "relative_80", "relative_50")) %>% 
  dplyr::mutate(delta_transp_80_rmn = (`80`-0) - (`100`* (1-0.2)),
                delta_transp_50_rmn = (`50`-0) - (`100`* (1-0.5)))


# Calculate streamflow remain
streamflow_w <- diff_flux_annual_stacked %>% 
  dplyr::filter(flux == "Streamflow") %>% 
  dplyr::mutate(delta_streamflow_80_w = `80` - `100`,
                delta_streamflow_50_w = `50` - `100`)


# Calculate soil storage remain (part 1 of storage term)
storage_w_soil <- diff_storage_daily$Total_sto %>% 
  dplyr::filter(WYD %in% c(1, 364)) %>%                  # Compare storage on the first and last data of wateryear (364 used because of some missing data with WYD365)
  dplyr::group_by(watershed) %>% 
  dplyr::mutate(lag_50 = lag(`50`),
                lag_80 = lag(`80`),
                lag_100 = lag(`100`),
                soilstorage_50_w = (`50`-lag_50) - (`100`-lag_100),
                soilstorage_80_w = (`80`-lag_80) - (`100`-lag_100)) %>% 
  dplyr::filter(WYD %in% c(364))


# Calculate gw storage remain  (part 2 of storage term)
storage_w_gw <- diff_storage_daily$GW_sto %>% 
  dplyr::filter(WYD %in% c(1, 364)) %>%                  # Compare storage on the first and last data of wateryear (364 used because of some missing data with WYD365)
  dplyr::group_by(watershed) %>% 
  dplyr::mutate(lag_50 = lag(`50`),
                lag_80 = lag(`80`),
                lag_100 = lag(`100`),
                gwstorage_50_w = (`50`-lag_50) - (`100`-lag_100),
                gwstorage_80_w = (`80`-lag_80) - (`100`-lag_100)) %>% 
  dplyr::filter(WYD %in% c(364))


# Calculate total storage remain
storage_w <- storage_w_soil %>%
  dplyr::select(wy, WYD, watershed, soilstorage_50_w, soilstorage_80_w) %>% 
  dplyr::full_join(., dplyr::select(storage_w_gw, wy, WYD, watershed,
                gwstorage_50_w, gwstorage_80_w),
                by = c("wy", "WYD", "watershed")) %>% 
  dplyr::mutate(delta_storage_50_w = gwstorage_50_w + soilstorage_50_w,
                delta_storage_80_w = gwstorage_80_w + soilstorage_80_w)
  


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Bring all components of vegetation change water balance together 


# Water made available
flux_rmv <- dplyr::full_join(dplyr::select(evap_rmv, c(wy, watershed, delta_evap_80_rmv, delta_evap_50_rmv)), 
                             dplyr::select(transp_rmv, c(wy, watershed, delta_transp_80_rmv, delta_transp_50_rmv)), 
                             by=c("wy", "watershed")) %>% 
  dplyr::mutate(total_80_rmv = delta_evap_80_rmv + delta_transp_80_rmv,
                total_50_rmv = delta_evap_50_rmv + delta_transp_50_rmv)

flux_rmv <- flux_rmv %>% 
  dplyr::left_join(., dplyr::distinct(dplyr::select(data_annual, wy, watershed, Precip)), by=c("wy", "watershed"))

flux_leftside_long <- flux_rmv %>% 
  dplyr::select(-c(total_80_rmv, total_50_rmv)) %>% 
  tidyr::pivot_longer(names_to = "flux_level", values_to = "value", -c(wy,watershed, Precip)) %>% 
  tidyr::separate(flux_level, into=c("delta", "flux", "scenario", "balance_component")) %>% 
  dplyr::select(-c(delta)) %>% 
  mutate(balance_component = if_else(balance_component == "rmv", "leftside", NULL))


# ----
# Allocated water
flux_allocated <- dplyr::full_join(dplyr::select(evap_rmn, wy, watershed, delta_evap_80_rmn, delta_evap_50_rmn), 
                                   dplyr::select(transp_rmn, wy, watershed, delta_transp_80_rmn, delta_transp_50_rmn), 
                                   by=c("wy", "watershed"))

flux_allocated <- dplyr::full_join(flux_allocated, 
                                   dplyr::select(streamflow_w, wy, watershed, delta_streamflow_80_w, delta_streamflow_50_w), 
                                   by=c("wy", "watershed"))

flux_allocated <- dplyr::full_join(flux_allocated,
                                   dplyr::select(storage_w, wy, watershed, delta_storage_80_w, delta_storage_50_w),
                                   by=c("wy", "watershed"))

# Create percentages
flux_allocated <- flux_allocated %>% 
  dplyr::mutate(delta_evap_50_percent = delta_evap_50_rmn/(delta_evap_50_rmn + delta_transp_50_rmn +
                                                           delta_streamflow_50_w + delta_storage_50_w),
                delta_transp_50_percent = delta_transp_50_rmn/(delta_evap_50_rmn + delta_transp_50_rmn +
                                                           delta_streamflow_50_w + delta_storage_50_w),
                delta_streamflow_50_percent = delta_streamflow_50_w/(delta_evap_50_rmn + delta_transp_50_rmn +
                                                           delta_streamflow_50_w + delta_storage_50_w),
                delta_storage_50_percent = delta_storage_50_w/(delta_evap_50_rmn + delta_transp_50_rmn +
                                                           delta_streamflow_50_w + delta_storage_50_w),
                delta_evap_80_percent = delta_evap_80_rmn/(delta_evap_80_rmn + delta_transp_80_rmn +
                                                         delta_streamflow_80_w + delta_storage_80_w),
                delta_transp_80_percent = delta_transp_80_rmn/(delta_evap_80_rmn + delta_transp_80_rmn +
                                                           delta_streamflow_80_w + delta_storage_80_w),
                delta_streamflow_80_percent = delta_streamflow_80_w/(delta_evap_80_rmn + delta_transp_80_rmn +
                                                               delta_streamflow_80_w + delta_storage_80_w),
                delta_storage_80_percent = delta_storage_80_w/(delta_evap_80_rmn + delta_transp_80_rmn +
                                                            delta_streamflow_80_w + delta_storage_80_w))


# Add precipitation to results
flux_allocated <- flux_allocated %>% 
  dplyr::left_join(., dplyr::distinct(dplyr::select(data_annual, wy, watershed, Precip)), by=c("wy", "watershed"))

# Separate the flux from the scenario
flux_rightside_long <- tidyr::pivot_longer(flux_allocated, names_to = "flux_level", values_to = "value", -c(wy,watershed, Precip)) %>% 
  tidyr::separate(flux_level, into=c("delta", "flux", "scenario", "balance_component")) %>% 
  dplyr::select(-c(delta)) %>% 
  mutate(balance_component = case_when(balance_component == "rmn" ~ "rightside",
                                       balance_component == "w" ~ "rightside",
                                       balance_component == "percent" ~ "percent"))


# ----
# Combine conserved and allocated water
veg_change_water_balance <- bind_rows(flux_leftside_long, flux_rightside_long)
veg_change_water_balance$flux <- factor(veg_change_water_balance$flux, levels = c("storage", "streamflow", "evap", "transp"))
veg_change_water_balance$scenario <- factor(veg_change_water_balance$scenario, levels = c("80", "50"))



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Make vegetation change water balance figure (Fig 4)

scenario_id <- c(
  "80" = "20% Biomass reduction",
  "50" = "50% Biomass reduction"
)

shed_id <- c(
  "p301" = "P301",
  "p303" = "P303",
  "p304" = "P304"
)


veg_change_water_balance1 <- veg_change_water_balance %>% 
  dplyr::filter(balance_component!="percent") %>% 
  dplyr::mutate(tmp = paste(flux,balance_component, sep="_")) %>% 
  dplyr::mutate(flux_comp = case_when(tmp == "evap_leftside" ~ paste("a1",tmp, sep="_"),
                                      tmp == "transp_leftside" ~ paste("a2",tmp, sep="_"),
                                      tmp == "storage_rightside" ~ paste("a3",tmp, sep="_"),
                                      tmp == "streamflow_rightside" ~ paste("a4",tmp, sep="_"),
                                      tmp == "evap_rightside" ~ paste("a5",tmp, sep="_"),
                                      tmp == "transp_rightside" ~ paste("a6",tmp, sep="_"))) %>% 
  dplyr::group_by(wy, flux, scenario, balance_component, flux_comp) %>% 
  dplyr::summarise(value = mean(value)) %>% 
  mutate(value = if_else(balance_component == "leftside", -value, value))
veg_change_water_balance1$wy <- as.numeric(veg_change_water_balance1$wy)
veg_change_water_balance1$flux_comp <- factor(veg_change_water_balance1$flux_comp,
                                              levels = c("a1_evap_leftside", "a2_transp_leftside",
                                                         "a3_storage_rightside", "a4_streamflow_rightside",
                                                         "a5_evap_rightside", "a6_transp_rightside"))    # The a1, a2, etc is because I couldn't get order correct below so renamed alphbetically.


x <- ggplot() +
  geom_col(data=dplyr::filter(veg_change_water_balance1, balance_component=="leftside"),
           mapping=aes(x=wy-0.17,y=value, fill=str_wrap(flux_comp)), width = .3, position = "stack") +
  geom_col(data=dplyr::filter(veg_change_water_balance1, balance_component=="rightside"),
           mapping=aes(x=wy+0.17,y=value, fill=str_wrap(flux_comp)), width = .3, position = "stack") +
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
                    
                    # See https://stackoverflow.com/questions/28810453/ for atop reasoning.
                    labels = c(expression(atop(x="",
                                               y = atop("Evaporation: Removed",
                                                        'vegetation (-'*Delta*E[rmv]*')     '))),
                               expression(atop(x="",
                                               y = atop("Transpiration: Removed",
                                                        'vegetation (-'*Delta*T[rmv]*')       '))),
                               expression(atop(x="",
                                               y = atop("Change in Storage",
                                               '('*Delta*'(d'*S[w]*'))               '))), 
                               expression(atop(x="",
                                               y = atop("Streamflow",
                                               '('*Delta*Q[w]*')        '))),
                               expression(atop(x="",
                                               y = atop("Evaporation: Remaining",
                                                        'vegetation ('*Delta*E[rmn]*')        '))), 
                               expression(atop(x="",
                                               y = atop("Transpiration: Remaining",
                                                        'vegetation ('*Delta*T[rmn]*')         '))))) +
  scale_x_continuous(breaks = seq(2004,2014), labels = seq(2004,2014)) +
  labs(x="Water year", y="Change in post-biomass-reduction annual flux, mm") +
  theme_bw(base_size = 11) +
  theme(legend.text.align = 0,
        legend.position = "bottom",
        #legend.key.height=unit(.7, "cm"),
        legend.text=element_text(size=11,
                                 margin = margin(t = -2, r = 0, b = 0, l = 0,
                                                 unit = "mm")),
        legend.box.margin=margin(-10,-10,-5,-10)) +
  NULL
#plot(x)

ggsave("output/manuscript_plots/plot_veg_change_water_balance.jpg", plot=x, width = 6.5, height = 5)
ggsave("output/manuscript_plots/plot_veg_change_water_balance.pdf", plot=x, width = 6.5, height = 5)




# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Compare partitioning relative to precipitation (Fig 5)

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


# By amount for 50% scenario (watersheds combined)
veg_change_water_balance1 <- veg_change_water_balance %>% 
  dplyr::group_by(wy, flux, scenario, balance_component) %>% 
  dplyr::summarise(value = mean(value),
                   Precip = mean(Precip)) %>% 
  dplyr::filter(balance_component=="rightside", scenario=="50") %>% 
  ungroup()
veg_change_water_balance1$flux <- factor(veg_change_water_balance1$flux,
                                         labels = c(
                                           "storage" = expression('Change in Storage (d'*S[w]*')'),
                                           "streamflow" = expression('Streamflow ('*Q[w]*')'),
                                           "evap" = expression('Evaporation ('*E[rmn]*')'),
                                           "transp" = expression('Transpiration ('*T[rmn]*')')
                                         ))


# Generate regression values
regr_results <- veg_change_water_balance1 %>% 
  group_by(flux, scenario,) %>% 
  nest() %>% 
#  mutate(regr = map(data, ~ nls(formula = value ~ Precip^2,  data = ., start = c(1))),
  mutate(regr = map(data, ~ lm(formula = value ~ Precip,  data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))

regr_terms <- regr_results %>% 
  unnest(results_terms)
regr_fit <- regr_results %>% 
  unnest(results_fit)
print(regr_terms$estimate)
print(regr_fit$p.value)

dat_text <- data.frame(
  # label2 = c(expression('y = -0.0040x + 15.6\n'~R^2~' = 0.06, p = 0.48'),
  #           expression('y = 0.11x - 58.4\nR2 = 0.85, p < 0.0001'),
  #           expression('y = -0.0082x - 4.8\nR2 = 0.70, p = 0.001'),
  #           expression('y = -0.047x + 240.8\nR2 = 0.49, p = 0.02')),
  label = c("y = -0.0040x + 15.6\nR2 = 0.06, p = 0.48",
            "y = 0.11x - 58.4\nR2 = 0.85, p < 0.0001",
            "y = -0.0082x - 4.8\nR2 = 0.70, p = 0.001",
            "y = -0.047x + 240.8\nR2 = 0.49, p = 0.02"),
  x = c(650,650,650,650),
  y = c(210,210,210,-16),
  flux = c("storage",
           "streamflow" ,
           "evap",
           "transp")
)
dat_text$flux <- factor(dat_text$flux,
                        levels = c("storage", "streamflow", "evap", "transp"),
                        labels = c(
                          "storage" = expression('Change in Storage (d'*S[w]*')'),
                          "streamflow" = expression('Streamflow ('*Q[w]*')'),
                          "evap" = expression('Evaporation ('*E[rmn]*')'),
                          "transp" = expression('Transpiration ('*T[rmn]*')')
                        ))

# Plot figure
x <- veg_change_water_balance1 %>% 
  ggplot(data=.) +
  geom_point(aes(x=Precip, y=value)) +
  stat_smooth(aes(x=Precip, y=value), method="lm", formula = y~x) +
  #geom_smooth(aes(x=Precip, y=value), method='glm', formula = y ~ I(x^2)) +
  geom_text(data = dat_text,
            mapping = aes(x = x, y = y, label = label),
            hjust = 0, vjust = 0, size = 2) +
  facet_wrap(flux~.,
             labeller = labeller(flux = label_parsed)) +
  labs(x="Annual precipitation, mm",
       y="Change in post-biomass-reduction annual flux, mm") +
  theme_bw(base_size = 11) +
  NULL
plot(x)

ggsave("output/manuscript_plots/plot_precip_vs_flux_change_50.jpg", plot=x, width = 5, height = 4)
ggsave("output/manuscript_plots/plot_precip_vs_flux_change_50.pdf", plot=x, width = 5, height = 4)




# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Relative benefit of doing 50% thinning vs 20% thinning (Bang for your buck) (Fig. 6)


veg_change_water_balance1 <- veg_change_water_balance
veg_change_water_balance1$flux <- factor(veg_change_water_balance1$flux,
                                         labels = c(
                                           "storage" = expression('Delta Storage (d'*S[w]*')'),
                                           "streamflow" = expression('Streamflow ('*Q[w]*')'),
                                           "evap" = expression('Evaporation ('*E[rmn]*')'),
                                           "transp" = expression('Transpiration ('*T[rmn]*')')
                                         ))


# Normalized to 1% treated area (Watersheds combined).
happy <- veg_change_water_balance %>% 
  dplyr::group_by(wy, flux, scenario, balance_component) %>% 
  dplyr::summarise(value = mean(value), Precip = mean(Precip)) %>% 
  dplyr::filter(balance_component=="rightside", flux %in% c("streamflow", "transp")) %>% 
  tidyr::pivot_wider(names_from = scenario, values_from = value) %>% 
  dplyr::mutate(`50_normalized` = `50`*(1/50),      # Flux yield per 1% treated area in watershed
                `80_normalized` = `80`*(1/20),      # Flux yield per 1% treated area in watershed
                flux_change_normalized1 = `50_normalized`/`80_normalized`*100-100,
                flux_change_normalized2 = `50_normalized` - `80_normalized`) %>% 
  tidyr::pivot_longer(cols = c(`50_normalized`, `80_normalized`), names_to = "normalized_names", values_to = "normalized_values")
happy$normalized_names <- factor(happy$normalized_names, levels = c("80_normalized", "50_normalized"))
happy$flux <- factor(happy$flux, levels = c("transp", "streamflow"),
                     labels = c("transp" = expression('Transpiration ('*T[rmn]*')'),"streamflow" = expression('Streamflow ('*Q[w]*')')))

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
                    labels = c("20% Biomass reduction", "50% Biomass reduction")) +
  scale_color_manual(name = "Scenario", values = colors_vibrant_2,
                     labels = c("20% Biomass reduction", "50% Biomass reduction")) +
  labs(x="Water year",
       y="Annual flux change per 1% biomass removed, mm") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        legend.box.margin=margin(-10,-10,-5,-10)) +
  NULL
#plot(x)

ggsave("output/manuscript_plots/plot_bang_for_your_buck_.jpg", plot=x, width = 6.5, height = 5)
ggsave("output/manuscript_plots/plot_bang_for_your_buck_.pdf", plot=x, width = 6.5, height = 5)


# ---------
# Test statistical difference between normalized changes
# Use 1-sample test. See if significantly different than zero.

happy %>% 
  group_by(flux, normalized_names) %>% 
  summarize(stat = t.test(normalized_values)$statistic,
            p = t.test(normalized_values)$p.value)

t.test(dplyr::filter(happy, flux == "\"Streamflow (\" * Q[w] * \")\"", normalized_names == "50_normalized")$normalized_values)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Save outputs

write_csv(veg_change_water_balance, file="output/veg_change_water_balance.csv")





