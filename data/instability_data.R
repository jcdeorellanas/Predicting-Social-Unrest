library(haven)
library(tidyverse)  # includes ggplot, dplyr, tidyr, readr, purrr, tibble
library(readxl)


load_instab_data <- function() {
  # load data -------------------------------------------------------------------
  qog_data <- rio::import("data/QoG/qog_std_ts_jan24_stata14.dta")
  
  iec_data <- rio::import("data/informal_economy_databases/informal_economy_df.dta") |>
    rename_with(~ gsub("Code", "ISO-3 Code", .x))|>
    rename_with(~ gsub("Economy", "Country or Area", .x)) |>
    mutate(Year = as.numeric(Year))
  #print(colnames(iec_data))
  coef_hum_ineq <- rio::import("data/UNDP/Coefficient_of_human_inequality.xlsx") |>
    mutate(Year = as.numeric(Year))
  #print(colnames(coef_hum_ineq))
  gini_index <- rio::import("data/UNDP/Gini_Index.xlsx") |>
    mutate(Year = as.numeric(Year))
  #print(colnames(gini_index))
  i_emp_df <- rio::import("data/UNDP/informal_employment_UNDP_df.dta") |>
    rename_with(~ gsub("ISO_3_Code", "ISO-3 Code", .x))|>
    rename_with(~ gsub("Country_or_Area", "Country or Area", .x)) |>
    mutate(Year = as.numeric(Year))
  #print(colnames(i_emp_df))
  #data_summary(i_emp_df)
  incom_ineq <- rio::import("data/UNDP/Inequality_in_income.xlsx") |>
    mutate(Year = as.numeric(Year))
  #print(colnames(incom_ineq))
  
  # list_of_dfs to join and modify
  list_of_dfs <- list(coef_hum_ineq, gini_index, incom_ineq, iec_data, i_emp_df)
  # Join data frames in the list_of_dfs by vector of common columns
  join_dfs <- Reduce(function(x, y) full_join(x, y, by = c("Country or Area", "ISO-3 Code", "Year")), list_of_dfs) |>
    rename_with(~ gsub("ISO-3 Code", "ccodealp", .x)) |>
    rename_with(~ gsub("Year", "year", .x)) |>
    rename_with(~ gsub("Coefficient of human inequality", "Human_Inequality_Coefficient", .x)) |>
    rename_with(~ gsub("Inequality in income", "Income_inequality", .x))
  #print(colnames(join_dfs))
  
  # selected_data <- join_dfs |> 
  #   dplyr::select(
  #     ccodealp, year, Human_Inequality_Coefficient, Gini_Index,
  #     Income_inequality, DGE_p, MIMIC_p, Pension_p, Infemp_p, Infsize_p, 
  #     SEMP_p, Inf_Emp_Agr_Total_Thosnds = iemp_sex_n_ec_act_thsnds_tot_Agr,
  #     Inf_Emp_Non_Agr_Total_Thosnds = iemp_sex_n_ec_act_thsnds_tot_Non,
  #     Inf_Emp_Employess_Total_Thosnds = iemp_sex_n_stus_em_thsnds_tot_Em,
  #     Inf_Emp_Self_Emp_Total_Thosnds = iemp_sex_n_stus_em_thsnds_tot_Se
  #   )
  
  instab_dataset <- qog_data |> 
    full_join(
      join_dfs |> 
        dplyr::select(
          ccodealp, year, 
          Human_Inequality_Coefficient, 
          Gini_Index, 
          Income_inequality, 
          DGE_p, MIMIC_p, Pension_p, Infemp_p, Infsize_p, SEMP_p,
          Inf_Emp_Agr_Total_Thosnds = iemp_sex_n_ec_act_thsnds_tot_Agr,
          Inf_Emp_Non_Agr_Total_Thosnds = iemp_sex_n_ec_act_thsnds_tot_Non,
          Inf_Emp_Employess_Total_Thosnds = iemp_sex_n_stus_em_thsnds_tot_Em,
          Inf_Emp_Self_Emp_Total_Thosnds = iemp_sex_n_stus_em_thsnds_tot_Se
        ),
      by = c("ccodealp", "year")
    )
  
  return(instab_dataset)
}

# Optionally load the dataset immediately
instab_dataset <- load_instab_data()


#data_summary(instab_dataset)


# undp_inf_emp <- undp_long |>
#   select("Country or Area", ccodealp, year, 
#          "Estimates of Informal Output, percentage of official gross domestic product (GDP)", 
#          "Informal employment by sex and economic activity (thousands), Total, Agriculture",
#          "Informal employment by sex and economic activity (thousands), Total, Non-agriculture", 
#          "Informal employment by sex and status in employment (thousands), Total, Employees",
#          "Informal employment by sex and status in employment (thousands), Total, Self-employed")