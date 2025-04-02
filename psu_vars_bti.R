library(haven)
library(tidyverse)  # includes ggplot, dplyr
library(ggrepel)    # better text labels 
library(scales)     # customize plot scales
library(ggridges)   # ridge/joy plots
library(patchwork)  # combining plots
library(readxl)

source("tools/tools.r")

# Transform Excel files into Stata format
#df <- read_excel("data/informal_economy_database/informal-economy-database.xlsx")
#write_dta(df, "data/informal_economy_database/informal-economy-database.dta")

# load data -------------------------------------------------------------------
source("data/instability_data.r")
data_summary(instab_dataset)

# new_df <- instab_dataset |>
#   select(cname, year, starts_with("gcb_"))

# Social Instability Data Frame-------------------------------------------------
psu_df <- instab_dataset |>
  select(Country = cname, Year = year,
         # corruption
         #"Bayesian Corruption Indicator" = bci_bci,
         "Corruption Perceptions Index" = ti_cpi,
         "Political corruption index" = vdem_corr,
         "Media corrupt" = vdem_mecorrpt,
         #vdem_execorr, vdem_jucorrdc, vdem_pubcorr,
         "Election vote buying" = vdem_elvotbuy,
         "Control of Corruption, Estimate" = wbgi_cce,
         #"Prosecution of Office Abuse" = bti_poa,
         
         # trust
         "Most people can be trusted" = wvs_trust,
         
         # social instability Choose 3 or 4 at most. Check the ones that have the most data
         "Political Stability and Absence of Violence/Terrorism, Estimate" = wbgi_pve, #1996-2022
         "Violence and Civilian Deaths before Election" = nelda_vcdbe, #1946-2020
         "Riots and Protests after Election" = nelda_rpae, #1946-2020
         # "Global Terrorism Index" = voh_gti, #2011 - 2022
         "Political Terror Scale AI" = gd_ptsa, #Amnesty International 1976-2021
         "Political Terror Scale SD" = gd_ptss, #State Department 1976-2021
           
         #education
         "School enrollment, primary (% gross)" = wdi_gerp,
         "Educational Attainment (15-24 years, Female)" = gea_ea1524f,
         "Educational Attainment (15-24 years, Male)" = gea_ea1524m
         
         # unemploy
         wdi_unempilo,
         wdi_lfpilo15,     # maybe look for shadow economy
         wdi_unempmilo,    # total male unemp
         wdi_unempyilo,    # total youth
         wdi_unempymilo,   # total male youth
         wdi_empprne, 
         wdi_empprmne,
         
         # basic services
         "Size of Government: Expenditures, Taxes and Enterprises" = fi_sog,
         wdi_acelu, wdi_acelr,   # electricity urban rural
         wdi_chexppgdp,          # health expenditure
         wdi_hwf,                # hand washing
         who_sanittot, # Total population using basic sanitation services (%)
         ihme_hle_0104,
         # "Welfare Regime" = bti_wr,
         # "Social Safety Nets" = bti_ssn,
         
         # inflation
         "Inflation, consumer prices" = wdi_inflation,
         
         # econ ineq (unequal distribution of income and wealth within a society)
         #wdi_povgap215, #only 1991 observations
         # wdi_gini,
         "Income share held by highest 10%" = wdi_incsh10h,
         "Income share held by lowest 10%"=wdi_incsh10l,
         Gini_Index,
         Income_inequality,
         
         # social ineq
         "Egalitarian component index" = vdem_egal,
         #"Political and Social Integration" = bti_psi,
         "School enrollment, tertiary (% gross)" = wdi_gert,
         #"Equal Opportunity" = bti_eo,
         Human_Inequality_Coefficient,
         "Human Capital Index" = egov_hci
         
         )|>
  mutate(pol_terr_scale_avg = if_else(!is.na("Political Terror Scale AI") & !is.na("Political Terror Scale SD"),
                                      ("Political Terror Scale AI" + "Political Terror Scale SD") / 2, NA_real_))



# 2. Trust----------------------------------------------------------------------
# trust_df <- qog_data |>
#   select(cname, year, 
#          "Approval of Democracy" = bti_aod, 
#          "Commitment to Democratic Institutions" = bti_cdi, 
#          "Civil Rights" = bti_cr, 
#          "Civil Society Participation" = bti_csp,
#          "Free and Fair Elections" = bti_ffe,
#          "Freedom of Expression" = bti_foe, 
#          "Independent Judiciary" = bti_ij, bti_poa, 
#          bti_pp, bti_prp,
#          wbgi_gee,
#          wjp_civ_just
#          )

# 4. Social inequality----------------------------------------------------------
# Is:df <- qog_data |>
#   select(cname, year,
#          "Civil Rights Protection" = bti_cr, 
#          "Civil Society Participation" = bti_csp, 
#          "Civil Society Traditions" = bti_cst, 
#          "Interest Groups" = bti_ig,
#          "No Interference of Religious Dogmas" = bti_nird)

# --------------------------------------------------------------------------------
# Y New Variable? Country Political Situation
# ctry_pols_df <- qog_data |>
#   select(cname, year,
#          bmr_demtran, br_com, br_coup, br_scoup, br_dem, br_elecpost, br_elect, 
#          br_suff, br_fcoup, br_int, br_mon, br_newconst,br_pvote, br_regch,
#          bti_aar, bti_ba, bti_cdi, bti_cr, bti_csp, bti_ds, bti_epg, bti_ffe,
#          bti_foe, bti_gi, bti_gp, bti_ic, bti_ij, bti_muf, bti_nird, bti_pdi,
#          bti_poa, bti_pp, bti_prp, bti_ps,
#          "Voice and Accountability, Estimate" = wbgi_vae
#          )

social_instability <- instab_dataset |> 
  #filter(year==c(2021,2020))|>
  select(country   = cname,
         year,
         Polit_stab = wbgi_pve, #data from 1996-2022
         # Int_dsplcd_per = wdi_idpvp, #to include with bti_ analysis
         # Conf_intsty = bti_ci,    # developing and recent
         # Use_of_force_monop = bti_muf,
         Viol_before_elect = nelda_vcdbe, #1946-2020
         Riot_n_prot_after_elect = nelda_rpae, # 1946-2020
         # Soc_viol_scale_idx = svs_ind, #2013-2016 # does'n include the USA and China
         Global_terror_idx = voh_gti, #2011 - 2022
         Pol_terr_scale_AI = gd_ptsa,
         Pol_terr_scale_SD = gd_ptss) |>
  #mutate(Pol_terr_scale = (Pol_terr_scale_AI + Pol_terr_scale_SD)/2)|>
  drop_na(country, Riot_n_prot_after_elect,
          Viol_before_elect, Global_terror_idx) 
data_summary(social_instability)


ti_cpi_nm_df <- instab_dataset |>
  select(Country = cname, Year = year,
         ti_cpi)|>
  drop_na(ti_cpi)

ti_cpi_om_df <- instab_dataset |>
  select(Country = cname, Year = year, ti_cpi_om) |>
  mutate(ti_cpi = round(ti_cpi_om * 10)) |>
  drop_na(ti_cpi) |>
  select(-ti_cpi_om)  # Drop ti_cpi_om column