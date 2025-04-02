library(tidyverse)
library(ggrepel)
library(rstatix)    # for easy calculation of t-tests
library(broom)
library(parameters) # calculate the "optimal" number of factors

source("tools/tools.r")

# load data -------------------------------------------------------------------
source("data/instability_data.r")

social_instability <- instab_dataset |> 
  select(country   = cname,
         polit_stab_absence_of_viol_terror = wbgi_pve, #data from 1996-2022
         Viol_before_elect = nelda_vcdbe, #1946-2020
         Riot_n_prot_after_elect = nelda_rpae, # 1946-2020
         Global_terror_idx = voh_gti, #2011 - 2022
         gd_ptsa,
         gd_ptss) |> 
  mutate(pol_terr_scale_avg = if_else(!is.na(gd_ptsa) & !is.na(gd_ptss),
                                      (gd_ptsa + gd_ptss) / 2, NA_real_)) |> 
  drop_na(country, polit_stab_absence_of_viol_terror, Riot_n_prot_after_elect,
          Viol_before_elect, Global_terror_idx, pol_terr_scale_avg)

data_summary(social_instability)

# find the principal components -------------------------------------

# setting `scale. = TRUE` is essential!
pca <- social_instability |> 
  select(polit_stab_absence_of_viol_terror, Riot_n_prot_after_elect,
         Viol_before_elect, Global_terror_idx, pol_terr_scale_avg) |> 
  prcomp(scale. = TRUE)

pca

summary(pca)

# "optimal" number of factors
social_instability |> 
  select(polit_stab_absence_of_viol_terror, Riot_n_prot_after_elect,
         Viol_before_elect, Global_terror_idx, pol_terr_scale_avg) |> 
  parameters::n_factors() |>
  summary()

# plot the weights of the variables in each component
components <- pca$rotation |> 
  data.frame() |> 
  rownames_to_column("variable")

components |> 
  ggplot(aes(x = variable, y = -PC1)) +
  geom_col()

components |> 
  ggplot(aes(x = variable, y = -PC2)) +
  geom_col()

# Create a tibble/data frame with the standard deviations of the PCA----------
pca_variance <- tibble(PC = 1:length(pca$sdev),
                       SD = pca$sdev,
                       Variance = pca$sdev^2,
                       Proportion = pca$sdev^2 / sum(pca$sdev^2),
                       Cumulative = cumsum(pca$sdev^2) / sum(pca$sdev^2))

# Scree plot with the eigenvalues on the y-axis and the principal components on the x-axis
ggplot(pca_variance, aes(x = PC, y = Variance)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(x = "Principal Component",
       y = "Eigenvalue",
       title = "Scree Plot")

# Adding a layer for cumulative variance to the scree plot
ggplot(pca_variance, aes(x = PC)) +
  geom_line(aes(y = Cumulative), color = "blue") +
  geom_point(aes(y = Proportion), color = "red") +
  theme_minimal() +
  labs(x = "Principal Component",
       y = "Proportion of Variance Explained",
       title = "Scree Plot with Cumulative Variance")


# ----------------------------------------------------
data_summary(instab_dataset)

unemployment <- instab_dataset |> 
  select(country   = cname,
         wdi_unempilo,
         wdi_lfpilo15,     # maybe look for shadow economy
         wdi_unempmilo,    # total male unemp
         wdi_unempyilo,    # total youth
         wdi_unempymilo,   # total male youth
         wdi_empprne, 
         wdi_empprmne,
         "Inf Emp.Employess Total (Thosnds)",
         "Inf Emp.Non Agr. Total (Thosnds)",
         "Inf Emp.Agr. Total (Thosnds)",
         #MIMIC_p,
         #DGE_p,
         SEMP_p
         ) |>
  drop_na(country, wdi_unempilo, wdi_lfpilo15, wdi_unempmilo, wdi_unempyilo,
          wdi_unempymilo, wdi_empprne, wdi_empprmne,
          "Inf Emp.Employess Total (Thosnds)",
          "Inf Emp.Non Agr. Total (Thosnds)",
          "Inf Emp.Agr. Total (Thosnds)", SEMP_p)
data_summary(unemployment)

# find the principal components -------------------------------------

# setting `scale. = TRUE` is essential!
pca <- unemployment |> 
  select(wdi_unempilo,
         wdi_lfpilo15,     # maybe look for shadow economy
         wdi_unempmilo,    # total male unemp
         wdi_unempyilo,    # total youth
         wdi_unempymilo,   # total male youth
         "Inf Emp.Employess Total (Thosnds)",
         "Inf Emp.Non Agr. Total (Thosnds)",
         "Inf Emp.Agr. Total (Thosnds)",
         SEMP_p) |> 
  prcomp(scale. = TRUE)

pca

summary(pca)

# "optimal" number of factors
infemp |> 
  select("Inf Emp.Employess Total (Thosnds)",
         "Inf Emp.Non Agr. Total (Thosnds)",
         "Inf Emp.Agr. Total (Thosnds)") |> 
  parameters::n_factors() |>
  summary()

# plot the weights of the variables in each component
components <- pca$rotation |> 
  data.frame() |> 
  rownames_to_column("variable")

components |> 
  ggplot(aes(x = variable, y = -PC1)) +
  geom_col()

components |> 
  ggplot(aes(x = variable, y = -PC2)) +
  geom_col()

# Create a tibble/data frame with the standard deviations of the PCA----------
pca_variance <- tibble(PC = 1:length(pca$sdev),
                       SD = pca$sdev,
                       Variance = pca$sdev^2,
                       Proportion = pca$sdev^2 / sum(pca$sdev^2),
                       Cumulative = cumsum(pca$sdev^2) / sum(pca$sdev^2))

# Scree plot with the eigenvalues on the y-axis and the principal components on the x-axis
ggplot(pca_variance, aes(x = PC, y = Variance)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(x = "Principal Component",
       y = "Eigenvalue",
       title = "Scree Plot")

# Adding a layer for cumulative variance to the scree plot
ggplot(pca_variance, aes(x = PC)) +
  geom_line(aes(y = Cumulative), color = "blue") +
  geom_point(aes(y = Proportion), color = "red") +
  theme_minimal() +
  labs(x = "Principal Component",
       y = "Proportion of Variance Explained",
       title = "Scree Plot with Cumulative Variance")




# plot using the components ----------------------------------------

# add components to the dataset
soc_ins_pca <- broom::augment(pca, social_instability)

soc_ins_pca |> 
  ggplot(aes(x = -.fittedPC1, y = .fittedPC2, 
             color = Viol_before_elect, label = country)) +
  geom_point() +
  geom_text_repel() +
  scale_color_binned(type = "viridis") +
  labs(x = "Political Stability", y = "Post election turmoil")

soc_ins_pca |> 
  ggplot(aes(x = -.fittedPC1, y = .fittedPC2, 
             color = health, label = country)) +
  geom_point() +
  geom_text_repel() +
  scale_color_binned(type = "viridis") +
  labs(x = "Free market", y = "Welfare state")

inst_pca |> 
  ggplot(aes(x = -.fittedPC1, y = .fittedPC2, 
             color = education, label = country)) +
  geom_point() +
  geom_text_repel() +
  scale_color_binned(type = "viridis") +
  labs(x = "Free market", y = "Welfare state")

inst_pca |> 
  ggplot(aes(x = -.fittedPC1, y = .fittedPC2, 
             color = democracy, label = country)) +
  geom_point() +
  geom_text_repel() +
  scale_color_binned(type = "viridis") +
  labs(x = "Free market", y = "Welfare state")

# relations between PCAs and other variables

inst_pca |> 
  ggplot(aes(x = -.fittedPC1,
             y = income,
             label = country)) +
  geom_smooth(method = lm, se = FALSE) +
  geom_point() +
  geom_text_repel() +
  scale_y_log10(labels = scales::dollar) +
  labs(x = "Free market",
       y = "Real GDP per capita")

inst_pca |> 
  ggplot(aes(x = .fittedPC2,
             y = income,
             label = country)) +
  geom_smooth(method = lm, se = FALSE) +
  geom_point() +
  geom_text_repel() +
  scale_y_log10(labels = scales::dollar) +
  labs(x = "Size of welfare state",
       y = "Real GDP per capita")

inst_pca |> 
  ggplot(aes(x = -.fittedPC1,
             y = health,
             label = country)) +
  geom_smooth(method = lm, se = FALSE) +
  geom_point() +
  geom_text_repel() +
  labs(x = "Free market",
       y = "Health")

inst_pca |> 
  ggplot(aes(x = -.fittedPC1,
             y = education,
             label = country)) +
  geom_smooth(method = lm, se = FALSE) +
  geom_point() +
  geom_text_repel() +
  labs(x = "Free market",
       y = "Education")

inst_pca |> 
  ggplot(aes(x = -.fittedPC1,
             y = democracy,
             label = country)) +
  geom_smooth(method = lm, se = FALSE) +
  geom_point() +
  geom_text_repel() +
  labs(x = "Free market",
       y = "Democracy")

