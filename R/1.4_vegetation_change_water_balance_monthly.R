# Figures for monthly vegetation reduction water balance
# Includes code for manuscript Figure 7 + Graphical Abstract


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


# Add month-day to daily data for computing monthly change in storage

# Change daily to monthly
diff_flux_monthly_stacked <- diff_flux_daily_stacked %>% 
  dplyr::mutate(yd = wyd_to_yd(wyd = WYD, y=2005),
                month = yd_to_month(yd = yd, y=2005)) %>%     # using non-leap year for all years since underlying data ignores leap year
  dplyr::select(-c(WYD,yd)) %>% 
  dplyr::group_by(flux, wy, month, watershed) %>% 
  dplyr::summarise(`50` = sum(`50`, na.rm = TRUE),
                   `80` = sum(`80`, na.rm = TRUE),
                   `100` = sum(`100`, na.rm = TRUE))
  

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Quantify water made available from thinning aka left half of Equation 4

# Calculate evaporation removed
evap_rmv <- diff_flux_monthly_stacked %>% 
  dplyr::filter(flux %in% c("Evap","canopy_evap")) %>% 
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
transp_rmv <- diff_flux_monthly_stacked %>% 
  dplyr::filter(flux == "Transp") %>% 
  dplyr::mutate(delta_transp_80_rmv = 0 - `100`*0.2,
                delta_transp_50_rmv = 0 - `100`*0.5)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Quantify how water is repartitioned after thinning (right half of equation 4)

# Calculate evaporation remain
evap_rmn <- diff_flux_monthly_stacked %>% 
  dplyr::filter(flux %in% c("Evap","canopy_evap")) %>% 
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
transp_rmn <- diff_flux_monthly_stacked %>% 
  dplyr::filter(flux == "Transp") %>% 
  dplyr::mutate(delta_transp_80_rmn = (`80`-0) - (`100`* (1-0.2)),
                delta_transp_50_rmn = (`50`-0) - (`100`* (1-0.5)))


# Calculate streamflow remain
streamflow_w <- diff_flux_monthly_stacked %>% 
  dplyr::filter(flux == "Streamflow") %>% 
  dplyr::mutate(delta_streamflow_80_w = `80` - `100`,
                delta_streamflow_50_w = `50` - `100`)


# Calculate soil storage remain (part 1 of storage term)
storage_w_soil <- diff_storage_daily_stacked %>% 
  dplyr::mutate(yd = wyd_to_yd(wyd = WYD, y=2005),
                month = yd_to_month(yd = yd, y=2005)) %>%     # using non-leap year for all years since underlying data ignores leap year
  dplyr::filter(flux == "Total_sto") %>% 
  dplyr::group_by(flux, watershed, wy, month) %>% 
  dplyr::mutate(month_rank = round(percent_rank(yd), 8)) %>% 
  dplyr::filter(month_rank %in% c(0,1)) %>%                  # Compare storage on the first and last data of month (using rank in the month to determine first and last day)
  dplyr::group_by(watershed) %>% 
  dplyr::mutate(lag_50 = lag(`50`),
                lag_80 = lag(`80`),
                lag_100 = lag(`100`),
                soilstorage_50_w = (`50`-lag_50) - (`100`-lag_100),
                soilstorage_80_w = (`80`-lag_80) - (`100`-lag_100)) %>% 
  dplyr::filter(month_rank %in% c(1))


# Calculate gw storage remain  (part 2 of storage term)
storage_w_gw <- diff_storage_daily_stacked %>% 
  dplyr::mutate(yd = wyd_to_yd(wyd = WYD, y=2005),
                month = yd_to_month(yd = yd, y=2005)) %>%     # using non-leap year for all years since underlying data ignores leap year
  dplyr::filter(flux == "GW_sto") %>% 
  dplyr::group_by(flux, watershed, wy, month) %>% 
  dplyr::mutate(month_rank = round(percent_rank(yd), 8)) %>% 
  dplyr::filter(month_rank %in% c(0,1)) %>%                  # Compare storage on the first and last data of month (using rank in the month to determine first and last day)
  dplyr::group_by(watershed) %>% 
  dplyr::mutate(lag_50 = lag(`50`),
                lag_80 = lag(`80`),
                lag_100 = lag(`100`),
                gwstorage_50_w = (`50`-lag_50) - (`100`-lag_100),
                gwstorage_80_w = (`80`-lag_80) - (`100`-lag_100)) %>% 
  dplyr::filter(month_rank %in% c(1))


# Calculate total storage remain
storage_w <- storage_w_soil %>%
  dplyr::select(wy, month, watershed, soilstorage_50_w, soilstorage_80_w) %>% 
  dplyr::full_join(., dplyr::select(storage_w_gw, wy, month, watershed,
                                    gwstorage_50_w, gwstorage_80_w),
                   by = c("wy", "month", "watershed")) %>% 
  dplyr::mutate(delta_storage_50_w = gwstorage_50_w + soilstorage_50_w,
                delta_storage_80_w = gwstorage_80_w + soilstorage_80_w)



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Bring all components of vegetation change water balance together 


# Water made available
flux_rmv <- dplyr::full_join(dplyr::select(ungroup(evap_rmv), c(wy, watershed, month, delta_evap_80_rmv, delta_evap_50_rmv)), 
                             dplyr::select(ungroup(transp_rmv), c(wy, watershed, month, delta_transp_80_rmv, delta_transp_50_rmv)), 
                             by=c("wy", "watershed", "month")) %>% 
  dplyr::mutate(total_80_rmv = delta_evap_80_rmv + delta_transp_80_rmv,
                total_50_rmv = delta_evap_50_rmv + delta_transp_50_rmv)

flux_rmv <- flux_rmv %>% 
  dplyr::left_join(., dplyr::distinct(dplyr::select(data_annual, wy, watershed, Precip)), by=c("wy", "watershed"))

flux_leftside_long <- flux_rmv %>% 
  dplyr::select(-c(total_80_rmv, total_50_rmv)) %>% 
  tidyr::pivot_longer(names_to = "flux_level", values_to = "value", -c(wy,watershed, month, Precip)) %>% 
  tidyr::separate(flux_level, into=c("delta", "flux", "scenario", "balance_component")) %>% 
  dplyr::select(-c(delta)) %>% 
  mutate(balance_component = if_else(balance_component == "rmv", "leftside", NULL))


# ----
# Allocated water
flux_allocated <- dplyr::full_join(dplyr::select(ungroup(evap_rmn), wy, month, watershed, delta_evap_80_rmn, delta_evap_50_rmn), 
                                   dplyr::select(ungroup(transp_rmn), wy, month, watershed, delta_transp_80_rmn, delta_transp_50_rmn), 
                                   by=c("wy", "watershed", "month"))

flux_allocated <- dplyr::full_join(flux_allocated, 
                                   dplyr::select(ungroup(streamflow_w), wy, month, watershed, delta_streamflow_80_w, delta_streamflow_50_w), 
                                   by=c("wy", "watershed", "month"))

flux_allocated <- dplyr::full_join(flux_allocated,
                                   dplyr::select(ungroup(storage_w), wy, month, watershed, delta_storage_80_w, delta_storage_50_w),
                                   by=c("wy", "watershed", "month"))

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
flux_rightside_long <- tidyr::pivot_longer(flux_allocated, names_to = "flux_level", values_to = "value", -c(wy,watershed, month, Precip)) %>% 
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
# Quantify water made available from thinning


# Calculate evaporation made available from canopy evaporation
# deltaE_a = Ecan_post_a - Ecan_pre_a (first component goes to 0 during post-treatment period)
evap_conserved1 <- diff_flux_monthly_stacked %>%
  dplyr::filter(flux == "canopy_evap") %>%
  dplyr::mutate(evap_80_conserved1 = `100`*0.2,                        # Note: sign in code is reversed from equations in manuscript
                evap_50_conserved1 = `100`*0.5)                        # Note: sign in code is reversed from equations in manuscript

# Calculate evaporation made available from litter, soil, and snow evaporation due to a reduction in PET  
# See equations in publication appendix
evap_conserved2 <- diff_flux_monthly_stacked %>% 
  dplyr::filter(flux %in% c("Evap","canopy_evap")) %>% 
  pivot_wider(names_from = "flux", values_from = c(`50`, `80`, `100`)) %>% 
  dplyr::rename("evap_50" = `50_Evap`, "evap_80" = `80_Evap`, "evap_100" = `100_Evap`,
                "can_evap_50" = `50_canopy_evap`, "can_evap_80" = `80_canopy_evap`, "can_evap_100" = `100_canopy_evap`) %>% 
  # 20% thinning
  dplyr::mutate(Eu_pre_80 = (1-0.2)*evap_100,
                Ea_pre_wo_canopy_80 = 0.2*evap_100 - 0.2*can_evap_100,
                percent_change_80 = evap_80/(Eu_pre_80 + Ea_pre_wo_canopy_80),
                Ea_post_80 = Ea_pre_wo_canopy_80 * percent_change_80,
                deltaEa_80 = Ea_post_80 - Ea_pre_wo_canopy_80) %>% 
  # 50% thinning
  dplyr::mutate(Eu_pre_50 = (1-0.5)*evap_100,
                Ea_pre_wo_canopy_50 = 0.5*evap_100 - 0.5*can_evap_100,
                percent_change_50 = evap_50/(Eu_pre_50 + Ea_pre_wo_canopy_50),
                Ea_post_50 = Ea_pre_wo_canopy_50 * percent_change_50,
                deltaEa_50 = Ea_post_50 - Ea_pre_wo_canopy_50) %>% 
  dplyr::mutate(evap_80_conserved2 = -deltaEa_80,                        # Note: sign in code is reversed from equations in manuscript
                evap_50_conserved2 = -deltaEa_50)                        # Note: sign in code is reversed from equations in manuscript

# Combine both evaporation components
evap_conserved1 <- dplyr::select(ungroup(evap_conserved1), wy, month, watershed, evap_80_conserved1, evap_50_conserved1)
evap_conserved2 <- dplyr::select(ungroup(evap_conserved2), wy, watershed, evap_80_conserved2, evap_50_conserved2)
evap_conserved <- evap_conserved1 %>% 
  dplyr::full_join(., evap_conserved2, by = c("wy", "watershed")) %>% 
  dplyr::mutate(evap_80_conserved = evap_80_conserved1 + evap_80_conserved2,
                evap_50_conserved = evap_50_conserved1 + evap_50_conserved2)


# Calculate transpiration made available
# deltaT_a = T_post_a - T_pre_a (first component goes to 0 during post-treatment period)
transp_conserved <- diff_flux_monthly_stacked %>% 
  dplyr::filter(flux == "Transp") %>% 
  dplyr::mutate(transp_80_conserved = `100`*0.2,
                transp_50_conserved = `100`*0.5)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Quantify how water is repartitioned after thinning


# Calculate evaporation partitioned
# See equations in publication appendix
evap_change <- diff_flux_monthly_stacked %>% 
  dplyr::filter(flux %in% c("Evap","canopy_evap")) %>% 
  pivot_wider(names_from = "flux", values_from = c(`50`, `80`, `100`)) %>% 
  dplyr::rename("evap_50" = `50_Evap`, "evap_80" = `80_Evap`, "evap_100" = `100_Evap`,
                "can_evap_50" = `50_canopy_evap`, "can_evap_80" = `80_canopy_evap`, "can_evap_100" = `100_canopy_evap`) %>% 
  # 20% thinning
  dplyr::mutate(Eu_pre_80 = (1-0.2)*evap_100,
                Ea_pre_wo_canopy_80 = 0.2*evap_100 - 0.2*can_evap_100,
                percent_change_80 = evap_80/(Eu_pre_80 + Ea_pre_wo_canopy_80),
                Eu_post_80 = Eu_pre_80 * percent_change_80,
                deltaEu_80 = Eu_post_80 - Eu_pre_80) %>% 
  # 50% thinning
  dplyr::mutate(Eu_pre_50 = (1-0.5)*evap_100,
                Ea_pre_wo_canopy_50 = 0.5*evap_100 - 0.5*can_evap_100,
                percent_change_50 = evap_50/(Eu_pre_50 + Ea_pre_wo_canopy_50),
                Eu_post_50 = Eu_pre_50 * percent_change_50,
                deltaEu_50 = Eu_post_50 - Eu_pre_50) %>% 
  dplyr::mutate(evap_80_allocated = deltaEu_80,
                evap_50_allocated = deltaEu_50)


# Calculate transpiration partitioned
transp_change <- diff_flux_monthly_stacked %>% 
  dplyr::filter(flux == "Transp") %>% 
  dplyr::mutate(transp_80_allocated = `80` - (`100`*0.8),
                transp_50_allocated = `50` - (`100`*0.5))


# Calculate streamflow partitioned
streamflow_change <- diff_flux_monthly_stacked %>% 
  dplyr::filter(flux == "Streamflow") %>% 
  dplyr::mutate(streamflow_80_allocated = `80` - `100`,
                streamflow_50_allocated = `50` - `100`)


# Calculate soil storage partitioned
storage_change_soil <- diff_storage_daily_stacked %>% 
  dplyr::mutate(yd = wyd_to_yd(wyd = WYD, y=2005),
                month = yd_to_month(yd = yd, y=2005)) %>%     # using non-leap year for all years since underlying data ignores leap year
  dplyr::filter(flux == "Total_sto") %>% 
  dplyr::group_by(flux, watershed, wy, month) %>% 
  dplyr::mutate(month_rank = round(percent_rank(yd), 8)) %>% 
  dplyr::filter(month_rank %in% c(0,1)) %>%                  # Compare storage on the first and last data of month (using rank in the month to determine first and last day)
  dplyr::group_by(watershed) %>% 
  dplyr::mutate(lag_50 = lag(`50`),
                lag_80 = lag(`80`),
                lag_100 = lag(`100`),
                soilstorage_50_allocated = (`50`-lag_50) - (`100`-lag_100),
                soilstorage_80_allocated = (`80`-lag_80) - (`100`-lag_100)) %>% 
  dplyr::filter(month_rank %in% c(1))


# Calculate gw storage partitioned
storage_change_gw <- diff_storage_daily_stacked %>% 
  dplyr::mutate(yd = wyd_to_yd(wyd = WYD, y=2005),
                month = yd_to_month(yd = yd, y=2005)) %>%     # using non-leap year for all years since underlying data ignores leap year
  dplyr::filter(flux == "GW_sto") %>% 
  dplyr::group_by(flux, watershed, wy, month) %>% 
  dplyr::mutate(month_rank = round(percent_rank(yd), 8)) %>% 
  dplyr::filter(month_rank %in% c(0,1)) %>%                  # Compare storage on the first and last data of month (using rank in the month to determine first and last day)
  dplyr::group_by(watershed) %>% 
  dplyr::mutate(lag_50 = lag(`50`),
                lag_80 = lag(`80`),
                lag_100 = lag(`100`),
                gwstorage_50_allocated = (`50`-lag_50) - (`100`-lag_100),
                gwstorage_80_allocated = (`80`-lag_80) - (`100`-lag_100)) %>% 
  dplyr::filter(month_rank %in% c(1))


# Calculate total storage partitioned
storage_change <- storage_change_soil %>%
  dplyr::select(wy, month, watershed, soilstorage_50_allocated, soilstorage_80_allocated) %>% 
  dplyr::full_join(., dplyr::select(storage_change_gw, wy, month, watershed,
                                    gwstorage_50_allocated, gwstorage_80_allocated),
                   by = c("wy", "month", "watershed")) %>% 
  dplyr::mutate(storage_50_allocated = gwstorage_50_allocated + soilstorage_50_allocated,
                storage_80_allocated = gwstorage_80_allocated + soilstorage_80_allocated)



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Bring all components of vegetation change water balance together 



# Conserved water
flux_conserved <- dplyr::full_join(dplyr::select(ungroup(evap_conserved), c(wy, watershed, month, evap_80_conserved, evap_50_conserved)), 
                                   dplyr::select(ungroup(transp_conserved), c(wy, watershed, month, transp_80_conserved, transp_50_conserved)), 
                                   by=c("wy", "watershed", "month")) %>% 
  dplyr::mutate(total_80_conserved = evap_80_conserved + transp_80_conserved,
                total_50_conserved = evap_50_conserved + transp_50_conserved)

flux_conserved <- flux_conserved %>% 
  dplyr::left_join(., dplyr::distinct(dplyr::select(data_annual, wy, watershed, Precip)), by=c("wy", "watershed"))

flux_conserved_long <- flux_conserved %>% 
  dplyr::select(-c(total_80_conserved, total_50_conserved)) %>% 
  tidyr::pivot_longer(names_to = "flux_level", values_to = "value", -c(wy,watershed, month, Precip)) %>% 
  tidyr::separate(flux_level, into=c("flux", "scenario", "balance_component"))


# ----
# Allocated water
flux_allocated <- dplyr::full_join(dplyr::select(ungroup(evap_change), wy, month, watershed, evap_80_allocated, evap_50_allocated), 
                                   dplyr::select(ungroup(transp_change), wy, month, watershed, transp_80_allocated, transp_50_allocated), 
                                   by=c("wy", "watershed", "month"))

flux_allocated <- dplyr::full_join(flux_allocated, 
                                   dplyr::select(ungroup(streamflow_change), wy, month, watershed, streamflow_80_allocated, streamflow_50_allocated), 
                                   by=c("wy", "watershed", "month"))

flux_allocated <- dplyr::full_join(flux_allocated,
                                   dplyr::select(ungroup(storage_change), wy, month, watershed, storage_80_allocated, storage_50_allocated),
                                   by=c("wy", "watershed", "month"))

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
flux_allocated_long <- tidyr::pivot_longer(flux_allocated, names_to = "flux_level", values_to = "value", -c(wy, watershed, month, Precip)) %>% 
  tidyr::separate(flux_level, into=c("flux", "scenario", "balance_component"))


# ----
# Combine conserved and allocated water
veg_change_water_balance <- bind_rows(flux_conserved_long, flux_allocated_long)
veg_change_water_balance$flux <- factor(veg_change_water_balance$flux, levels = c("storage", "streamflow", "evap", "transp"))
veg_change_water_balance$scenario <- factor(veg_change_water_balance$scenario, levels = c("80", "50"))


# QC
# veg_change_water_balance %>% 
#   dplyr::filter(flux == "evap",
#                 #wy == 2011,
#                 balance_component=="allocated",
#                 scenario==80,
#                 #watershed=="p301"
#                 ) %>% 
#   group_by(wy) %>% 
#   summarise(happy = sum(value))




# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Generate Figure 7
# Monthly water balance: Compare dry years (2014 2007 2013), normal year (2004 2012 2008 2009), and wet year (2010 2005 2006 2011)
# For P303 50% and 20% scenarios


wetness_id <- c(
  "low" = "Dry water years",
  "middle" = "Average water years",
  "high" = "Wet water years"
)

shed_id <- c(
  "p301" = "P301",
  "p303" = "P303",
  "p304" = "P304"
)

scenario_id <- c(
  "80" = "20% Biomass reduction",
  "50" = "50% Biomass reduction"
)



# Plot with all four fluxes/storages
tmp <- veg_change_water_balance %>% 
  dplyr::mutate(wetness = case_when(wy %in% c(2014, 2007, 2013) ~ "low",
                                    wy %in% c(2004, 2012, 2008, 2009) ~ "middle",
                                    wy %in% c(2010, 2005, 2006, 2011) ~ "high")) %>% 
  dplyr::filter(flux %in% c("storage", "streamflow", "evap", "transp"),
                balance_component=="rightside",
                watershed=="p303",
                scenario == 50) %>% 
  dplyr::group_by(wetness, flux, month, scenario, balance_component) %>% 
  dplyr::summarise(value = mean(value, na.rm = TRUE))
tmp$wetness <- factor(tmp$wetness, levels = c("low", "middle", "high"))
tmp$month <- factor(tmp$month, levels = c(10,11,12,1,2,3,4,5,6,7,8,9),
                    labels = c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep"))
tmp$flux <- factor(tmp$flux,
                   labels = c(
                     # "storage" = expression(atop(x="Change in Storage/",
                     #                 y =  'GW Loss ('*Delta*'(d'*S[w]*'))')), # Remove delta
                     "storage" = expression('Change in Storage (d'*S[w]*')'),
                     "streamflow" = expression('Streamflow ('*Q[w]*')'),
                     "evap" = expression('Evaporation ('*E[rmn]*')'),
                     "transp" = expression('Transpiration ('*T[rmn]*')')
                   ))

x <- ggplot(data=tmp) +
  geom_col(aes(x=month,y=value, fill = flux)) +
  facet_grid(flux~wetness, labeller = labeller(flux = label_parsed, wetness = wetness_id)) +
  labs(x="Month", y="Change in post-biomass-reduction monthly flux, mm") +
  scale_fill_manual(values = colors_bright_4) +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 270, hjust=0, vjust=0.6),
        legend.position = "none") +
  NULL
plot(x)

ggsave("output/manuscript_plots/plot_veg_change_water_balance_monthly.jpg", plot=x, width = 7.5, height = 7)
ggsave("output/manuscript_plots/plot_veg_change_water_balance_monthly.pdf", plot=x, width = 7.5, height = 7)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Average monthly values

tmp %>% 
  dplyr::group_by(wetness, flux) %>% 
  summarise(value = mean(value)*12)


# For to compute baseline streamflow for comparison with change value
tmp2 <- diff_flux_monthly_stacked %>% 
  dplyr::mutate(wetness = case_when(wy %in% c(2014, 2007, 2013) ~ "low",
                                    wy %in% c(2004, 2012, 2008, 2009) ~ "middle",
                                    wy %in% c(2010, 2005, 2006, 2011) ~ "high")) %>% 
  dplyr::filter(flux %in% c("Streamflow")) %>% 
  dplyr::group_by(wetness, month) %>% 
  dplyr::summarise(`100` = mean(`100`, na.rm = TRUE))
tmp2$wetness <- factor(tmp2$wetness, levels = c("low", "middle", "high"))

View(tmp2)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Graphical Abstract

x <- tmp %>% 
  dplyr::filter(wetness == "high") %>% 
  ggplot(data=.) +
  geom_col(aes(x=month,y=value, fill = flux)) +
  facet_wrap(flux~., labeller = labeller(flux = label_parsed)) +
  #facet_grid(flux~wetness, labeller = labeller(flux = label_parsed, wetness = wetness_id)) +
  labs(x="Month", y="Change in post-biomass-reduction monthly flux, mm") +
  scale_fill_manual(values = colors_bright_4) +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_text(angle = 270, hjust=0, vjust=0.6),
        legend.position = "none") +
  NULL
plot(x)

ggsave("output/manuscript_plots/plot_veg_balance_monthly_graphical.pdf", plot=x, width = 7.5, height = 7)


