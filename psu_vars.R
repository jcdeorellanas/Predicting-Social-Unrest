library(haven)
library(tidyverse)  # includes ggplot, dplyr
library(ggrepel)    # better text labels 
library(scales)     # customize plot scales
library(ggridges)   # ridge/joy plots
library(patchwork)  # combining plots
library(readxl)

source("tools/tools.r")

# load data -------------------------------------------------------------------
source("data/instability_data.r")
data_summary(instab_dataset)
# ------------------------------------------------------------------------------

# Social Instability Data Frame-------------------------------------------------
soc_ins <- instab_dataset |>
  select(Country = cname, Year = year,
         # corruption
         poltcl_corrptn_idx = vdem_corr,
         media_corrupt = vdem_mecorrpt,
         election_vote_buying = vdem_elvotbuy,
         control_of_corruption_estimate = wbgi_cce,

         # trust
         trust_in_people = wvs_trust,
         
         # social instability. Check the ones that have the most data
         polit_stab_absence_of_viol_terror = wbgi_pve, #1996-2022
         Viol_before_elect = nelda_vcdbe, #1946-2020
         Riot_n_prot_after_elect = nelda_rpae, #1946-2020
         # "Global Terrorism Index" = voh_gti, #2011 - 2022
         gd_ptsa, # Political Terror Scale Amnesty International 1976-2021
         gd_ptss, # Political Terror Scale State Department 1976-2021
           
         #education
         sch_enrlmnt_prmry_gross = wdi_gerp, # "School enrollment, primary (% gross)"
         ed_attnmnt_fem_15_24 = gea_ea1524f, #"Educational Attainment (15-24 years, Female)"
         ed_attnmnt_mal_15_24 = gea_ea1524m, #"Educational Attainment (15-24 years, Male)"
         
         # unemploy
         wdi_unempilo,     # Unemployment, total (% of total labor force) (modeled ILO)
         wdi_lfpilo15,     # Labor force participation rate (% of total ages 15+) (modeled ILO)
         wdi_unempmilo,    # total male unemp
         wdi_unempyilo,    # total youth unemployment (15-24)
         wdi_unempymilo,   # total male youth unemployment (15-24)
         # informal and self employment
         SEMP_p,
         Inf_Emp_Agr_Total_Thosnds,
         Inf_Emp_Non_Agr_Total_Thosnds,
         Inf_Emp_Employess_Total_Thosnds,
         Inf_Emp_Self_Emp_Total_Thosnds,
    
         # basic services
         gov_size  = fi_sog, #"Size of Government: Expenditures, Taxes and Enterprises"
         wdi_acelu, wdi_acelr,   # electricity urban rural
         wdi_chexppgdp,          # health expenditure
         wdi_hwf,                # hand washing
         who_sanittot, # Total population using basic sanitation services (%)
         ihme_hle_0104t, # Healthy life years 1-4 years total

         # inflation
         wdi_inflation, #Inflation, consumer prices
         
         # demography
         wdi_popgr, # Population growth (annual %)
         
         # econ ineq (unequal distribution of income and wealth within a society)
         #wdi_povgap215, #only 1991 observations
         income_share_high_10pct = wdi_incsh10h,
         income_share_low_10pct = wdi_incsh10l,
         wdi_gdpcapgr, #GDP per capita growth (annual %) 
         Gini_Index,
         Income_inequality,
         
         # social ineq
         egal_comp_idx = vdem_egal, # "Egalitarian component index"
         sch_enrlmnt_tertiary_gross = wdi_gert,
         Human_Inequality_Coefficient,
         human_cap_idx = egov_hci
         )|>
  mutate(pol_terr_scale_avg = if_else(!is.na(gd_ptsa) & !is.na(gd_ptss),
                                      (gd_ptsa + gd_ptss) / 2, NA_real_)) 


# __________________________________________________________________________


# ti_cpi_nm_df <- instab_dataset |>
#   select(Country = cname, Year = year,
#          ti_cpi)|>
#   drop_na(ti_cpi)
# 
# ti_cpi_om_df <- instab_dataset |>
#   select(Country = cname, Year = year, ti_cpi_om) |>
#   mutate(ti_cpi = round(ti_cpi_om * 10)) |>
#   drop_na(ti_cpi) |>
#   select(-ti_cpi_om)  # Drop ti_cpi_om column