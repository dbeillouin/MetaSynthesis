
#Packages
library(broom) # Pour utiliser la fonction tidy
library(broom.mixed) # pour utiliserr broom et metafor
library(magrittr)  # pour les pipes
library(brms)
library(rstan)
library(clubSandwich)

Outcome_to_be_included=c("SOC stock", "SOC concentration", "SOC stock / SOC concentration")
Outcome_to_be_included=c("Bulk soil")

Grouping_var         =c("Land_use", "Intervention")
chains =4
warmup = 8000
iter = 20000
thin = 10

options(warn=-1)
workers = parallel::detectCores()-1
future::plan(future::multisession(workers= parallel::detectCores()-1))

RATIO  <-readxl::read_excel(here::here("data","derived-data","RATIO.xlsx")) %>%
  dplyr::filter(Sub_cat_outcome %in% Outcome_to_be_included )

DATABASE <-RATIO                                            %>%
  dplyr::mutate(N_paired_data = as.numeric(N_paired_data))  %>%
  tidyr::nest(data= -all_of(Grouping_var))%>%
  dplyr::mutate(n_ES   = furrr::future_map_dbl(data, nrow),
                n_data = furrr::future_map_dbl(data, ~{sum(.x$N_paired_data, na.rm=TRUE)}),
                n_MA   = furrr::future_map_dbl(data, ~{length(unique(.x$ID))}))

DATABASE_plu<-DATABASE %>% dplyr::filter(n_ES>1)
DATABASE_un<-DATABASE %>% dplyr::filter(n_ES<=1)

######  For combination with >2 ES, we report the value ####

DATABASE_un <- DATABASE_un %>%
  dplyr::mutate(
    estimate_2lvl        = unlist(furrr::future_map_dbl(data, ~  .x$`Effect size`)),
    conf.low_2lvl         = unlist(furrr::future_map_dbl(data, ~  .x$`Effect size`- sqrt(.x$vi)*1.96)),
    conf.high_2lvl        = unlist(furrr::future_map_dbl(data, ~  .x$`Effect size`+sqrt(.x$vi)*1.96)))


### bayesian analysis

rstan_options(auto_write = TRUE)

priors <- c(prior(normal(0,1), class = Intercept),
            prior(cauchy(0,0.5), class = sd))

### Bayesian model 3 lvl #####
DATABASE_Bay_3lvl <- DATABASE_plu   %>%
  dplyr::ungroup() %>%
  dplyr::mutate(data = purrr::map(data, ~.x %>% dplyr::rename("ES" = `Effect size`)),
                fit_B =purrr::map(data, ~ brm(ES|se(vi, sigma=TRUE) ~ 1+ (1|ID)+ (1|NUM),
                                              data =  .x,
                                              prior = priors,
                                              chains = chains,
                                              warmup = warmup,
                                              iter = iter,
                                              cores= workers,
                                              future = TRUE,
                                              thin = thin,
                                              inits = 0)),
                tidied = purrr::map(fit_B, tidy,conf.int = TRUE),
                WAIC = purrr::map(fit_B, ~ WAIC(add_criterion(.x, "waic"))$p_waic),
                Rhat = purrr::map(fit_B, ~ summary(.x)$fixed$Rhat),
                neff = purrr::map(fit_B, ~  length(which(neff_ratio(.x)<0.1))),
                Nb_D = purrr::map(fit_B, ~ sum(subset(nuts_params(.x), Parameter == "divergent__")$Value))) %>%
  tidyr::unnest(tidied) %>%
  dplyr::rename_with(~ paste0(., "_Bay_3lvl"), -c(all_of(Grouping_var), "n_ES", "n_data", "n_MA","data")) %>%
  dplyr::select(-data)

saveRDS(DATABASE_Bay_3lvl, here::here("data","tmp","DATABASE_Bay_3lvl.rds"))


rm(DATABASE_Bay_3lvl)

              ### Bayesian model 2 lvl #####
              DATABASE_Bay_2lvl <- DATABASE_plu  %>%
                dplyr::ungroup() %>%
                dplyr::mutate(data = purrr::map(data, ~.x %>% dplyr::rename("ES" = `Effect size`)),
                              fit_B = purrr::map(data, ~ brm(ES|se(vi, sigma=TRUE) ~ 1+ (1|ID),
                                                             data =  .x,
                                                             prior = priors,
                                                             chains = chains,
                                                             warmup = warmup,
                                                             iter = iter,
                                                             cores= workers,
                                                             future = TRUE,
                                                             thin = thin,
                                                             inits = 0)),
                              tidied = purrr::map(fit_B, tidy,conf.int = TRUE),
                              WAIC = purrr::map(fit_B, ~ WAIC(add_criterion(.x, "waic"))$estimates[3,1]),
                              Rhat = purrr::map(fit_B, ~ summary(.x)$fixed$Rhat),
                              neff = purrr::map(fit_B, ~  length(which(neff_ratio(.x)<0.1))),
                              Nb_D = purrr::map(fit_B, ~ sum(subset(nuts_params(.x), Parameter == "divergent__")$Value))
                )%>%
                tidyr::unnest(tidied) %>%
                dplyr::rename_with(~ paste0(., "_Bay_2lvl"), -c(Grouping_var, "n_ES", "n_data", "n_MA","data"))


              ### Bayesian model 2 lvl and quality #####
              DATABASE_Bay_2lvlQ <- DATABASE_plu   %>%
                dplyr::ungroup() %>%
                dplyr::mutate(data = purrr::map(data, ~.x %>% dplyr::rename("ES" = `Effect size`)),
                              fit_B = purrr::map(data, ~ brm(ES|se(QUALITY_DOI(SCORE =SCORE, vi= vi), sigma=TRUE) ~ 1+ (1|ID),
                                                             data =  .x,
                                                             prior = priors,
                                                             chains = chains,
                                                             warmup = warmup,
                                                             iter = iter,
                                                             cores= workers,
                                                             future = TRUE,
                                                             thin = thin,
                                                             inits = 0)),
                              tidied = purrr::map(fit_B, tidy,conf.int = TRUE),
                              WAIC = purrr::map(fit_B, ~ WAIC(add_criterion(.x, "waic"))$estimates[3,1]),
                              Rhat = purrr::map(fit_B, ~ summary(.x)$fixed$Rhat),
                              neff = purrr::map(fit_B, ~  length(which(neff_ratio(.x)<0.1))),
                              Nb_D = purrr::map(fit_B, ~ sum(subset(nuts_params(.x), Parameter == "divergent__")$Value))
                )%>%
                tidyr::unnest(tidied) %>%
                dplyr::rename_with(~ paste0(., "_2lvlQ"), -c(Grouping_var, "n_ES", "n_data", "n_MA","data"))


              ### Bayesian model 3 lvl and quality #####
              DATABASE_Bay_3lvlQ <- DATABASE_plu  %>%
                dplyr::ungroup() %>%
                dplyr::mutate(data = purrr::map(data, ~.x %>% dplyr::rename("ES" = `Effect size`)),
                              fit_B = purrr::map(data, ~ brm(ES|se(QUALITY_DOI(SCORE =SCORE, vi= vi), sigma=TRUE) ~ 1+ (1|ID) + (1|NUM),
                                                             data =  .x,
                                                             prior = priors,
                                                             chains = chains,
                                                             warmup = warmup,
                                                             iter = iter,
                                                             cores= workers,
                                                             future = TRUE,
                                                             thin = thin,
                                                             inits = 0)),
                              tidied = purrr::map(fit_B, tidy,conf.int = TRUE),
                              WAIC = purrr::map(fit_B, ~ WAIC(add_criterion(.x, "waic"))$estimates[3,1]),
                              Rhat = purrr::map(fit_B, ~ summary(.x)$fixed$Rhat),
                              neff = purrr::map(fit_B, ~  length(which(neff_ratio(.x)<0.1))),
                              Nb_D = purrr::map(fit_B, ~ sum(subset(nuts_params(.x), Parameter == "divergent__")$Value))
                )%>%
                tidyr::unnest(tidied) %>%
                dplyr::rename_with(~ paste0(., "_3lvlQ"), -c(Grouping_var, "n_ES", "n_data", "n_MA","data"))



              FINAL_Bay<-plyr::join_all(list(DATABASE_Bay_3lvl,DATABASE_Bay_2lvl,DATABASE_Bay_2lvlQ,DATABASE_Bay_3lvlQ),
                                        by=Grouping_var, type='left')

