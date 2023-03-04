
#Packages
library(broom) # Pour utiliser la fonction tidy
library(broom.mixed) # pour utiliserr broom et metafor
library(magrittr)  # pour les pipes
library(brms)
library(rstan)
library(clubSandwich)

Outcome_to_be_included=c("SOC stock", "SOC concentration", "SOC stock / SOC concentration")
Grouping_var         =c("details_outcome", "Land_use", "Intervention")
chains =4
warmup = 8000
iter = 25000
thin = 10

   workers = parallel::detectCores()-1
   future::plan(future::multisession(workers= parallel::detectCores()-1))

   RATIO  <-readxl::read_excel(here::here("data","derived-data","RATIO.xlsx")) %>%
           dplyr::filter(details_outcome %in% Outcome_to_be_included )

  DATABASE <-RATIO                                            %>%
    dplyr::mutate(N_paired_data = as.numeric(N_paired_data))  %>%
    tidyr::nest(data= -Grouping_var)%>%
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


  ###### Two level meta-analytical model. ####
  DATABASE_two <- DATABASE_plu %>%
    dplyr::mutate(
      fit_2_lvl = furrr::future_map(data, ~
                  tryCatch({
                    mod  <-metafor::rma.mv(`Effect size`,
                                  vi,data=.x,
                                  random=~1|ID,
                                  method="REML",
                                  control=list(optimizer="optim",
                                               optmethod="BFGS"))
                  }, error=function(e){
                    mod  <- metafor::rma.mv(`Effect size`,
                                   vi,
                                   data=.x,
                                   random=~1|ID,
                                   method="REML")
                  }),.progress = TRUE),
      tidied = purrr::map(fit_2_lvl, broom.mixed::tidy,conf.int = TRUE),
      Weights = furrr::future_map(fit_2_lvl, ~ weights(.x)),
      AIC = furrr::future_map(fit_2_lvl, ~ AIC(.x))) %>%
    tidyr::unnest(tidied) %>%
    dplyr::select(-c("term","type" )) %>%
    dplyr::rename_with(~ paste0(., "_2lvl"), -c(Grouping_var, "n_ES", "n_data", "n_MA","data"))

  ###### Classical three level meta-analytical model. ####
  DATABASE_Three <- DATABASE_plu %>%
    dplyr::mutate(
      fit_3_lvl = furrr::future_map(data, ~
                                        tryCatch({
                                          mod  <-metafor::rma.mv(`Effect size`,
                                                                 vi,data=.x,
                                                                 random=~1|ID/NUM,
                                                                 method="REML",
                                                                 control=list(optimizer="optim",
                                                                              optmethod="BFGS"))
                                        }, error=function(e){
                                          mod  <- metafor::rma.mv(`Effect size`,
                                                                  vi,
                                                                  data=.x,
                                                                  random=~1|ID/NUM,
                                                                  method="REML")}),.progress = TRUE),
      tidied = purrr::map(fit_3_lvl, tidy,conf.int = TRUE),
      Weights = furrr::future_map(fit_3_lvl, ~ weights(.x)),
      AIC = furrr::future_map(fit_3_lvl, ~ AIC(.x))) %>%
    tidyr::unnest(tidied) %>%
    dplyr::select(-c("term","type" )) %>%
    dplyr::rename_with(~ paste0(., "_3lvl"), -c(Grouping_var, "n_ES", "n_data", "n_MA","data"))


  ###### Quality meta-analytical model. ####

  DATABASE_Three <- DATABASE_plu %>%
    dplyr::mutate(
      fit_3_lvl = furrr::future_map(data, ~
                                      tryCatch({
                                        mod  <-metafor::rma.mv(`Effect size`,
                                                               QUALITY_DOI(SCORE =.$SCORE, vi= .$vi),
                                                               data=.x,
                                                               random=~1|ID/NUM,
                                                               method="REML",
                                                               control=list(optimizer="optim",
                                                                            optmethod="BFGS"))
                                      }, error=function(e){
                                        mod  <- metafor::rma.mv(`Effect size`,
                                                                vi,
                                                                data=.x,
                                                                random=~1|ID/NUM,
                                                                method="REML")}),.progress = TRUE),
      tidied = purrr::map(fit_3_lvl, tidy,conf.int = TRUE),
      Weights = furrr::future_map(fit_3_lvl, ~ weights(.x)),
      AIC = furrr::future_map(fit_3_lvl, ~ AIC(.x))) %>%
    tidyr::unnest(tidied) %>%
    dplyr::select(-c("term","type" )) %>%
    dplyr::rename_with(~ paste0(., "_3lvl"), -c(Grouping_var, "n_ES", "n_data", "n_MA","data"))






  DATABASE_Quality <- DATABASE_plu %>%
    dplyr::mutate(
      fit_Q = furrr::future_map(data, ~
                  tryCatch({
                    mod  <-metafor::rma.mv(`Effect size`,
                                 QUALITY_DOI(SCORE =.$SCORE, vi= .$vi),
                                  data=.x,
                                  random=~1|NUM/ID,
                                  method="REML",
                                  control=list(optimizer="optim",
                                               optmethod="BFGS"))
                  }, error=function(e){
                    mod  <-metafor::rma.mv(`Effect size`,
                                  QUALITY_DOI(SCORE =.$SCORE, vi= .$vi),
                                  data=.x,
                                  random=~1|NUM/ID,
                                  method="REML")
                  }) ,.progress = TRUE),
      tidied_Q = purrr::map(fit_Q, tidy,conf.int = TRUE),
      Weights = furrr::future_map(fit_Q, ~ weights(.x)),
      AIC = furrr::future_map(fit_Q, ~ AIC(.x)))
  %>%
    tidyr::unnest(tidied_Q) %>%
    dplyr::select(-c("term","type" )) %>%
    dplyr::rename_with(~ paste0(., "_Q"), -c(Grouping_var, "n_ES", "n_data", "n_MA","data"))


  ######   Quality and redundancy meta-analytical model ####
  # We first create the global Redundancy matrix

  Global_Redundancy_matrix(File_name   = "Data_Base_C_Sol_2023-02-18.xlsx",
                           sheet_name  = "Primary_studies",
                           ID          = "ID",
                           DOI         = "DOI",
                           FACTOR      = 1)

  DATABASE_Quality_Red <- DATABASE_plu %>%
    dplyr::mutate(
      fit_QR = furrr::future_map(data, ~
                           tryCatch({
                             mod  <-metafor::rma.mv(`Effect size`,
                                           V=
                                             FUN_MAT_RED(
                                               Matrix_name        = "Global_Redundancy_matrix.xlsx",
                                               Selected_rows_ID   = .$ID ,
                                               Variance           =  QUALITY_DOI(SCORE =.$SCORE, vi= .$vi)),
                                           data=.x,
                                           random=~1|NUM/ID,
                                           method="REML",
                                           control=list(optimizer="optim",
                                                        optmethod="BFGS"))
                           }, error=function(e){
                             mod  <-metafor::rma.mv(`Effect size`,
                                                vi,
                                               data=.x,
                                               random=~1|NUM/ID,
                                               method="REML")
                           }),.progress = TRUE),
      tidied_QR = purrr::map(fit_QR, tidy,conf.int = TRUE),
      Weights = furrr::future_map(fit_QR, ~ weights(.x)),
      AIC = furrr::future_map(fit_QR, ~ AIC(.x))) %>%
    tidyr::unnest(tidied_QR) %>%
    dplyr::select(-c("term","type" )) %>%
    dplyr::rename_with(~ paste0(., "_QR"), -c(Grouping_var, "n_ES", "n_data", "n_MA","data"))



  ######   linear meta-analytical model ####
  # We first create the global Redundancy matrix

  DATABASE_Linear <- DATABASE_plu %>%
    dplyr::mutate(
      fit_L = furrr::future_map(data, ~
                             tryCatch({
                               mod  <-metafor::rma.uni(`Effect size`,
                                             vi,
                                             data=.x,
                                             method="REML",
                                             control=list(optimizer="optim",
                                                          optmethod="BFGS"))
                             }, error=function(e){
                               mod  <- metafor::rma.uni(`Effect size`,
                                              vi,
                                              data=.x,
                                              method="REML")
                             }) ,.progress = TRUE),
      tidied = purrr::map(fit_L, tidy,conf.int = TRUE),
      Weights = furrr::future_map(fit_L, ~ weights(.x)),
      AIC = furrr::future_map(fit_L, ~ AIC(.x))) %>%
      tidyr::unnest(tidied) %>%
    dplyr::select(-c("term","type" )) %>%
    dplyr::rename_with(~ paste0(., "_Lin"), -c(Grouping_var, "n_ES", "n_data", "n_MA","data"))


###### We add the rosenfail safe number and Trim and fill model #####
  for (i in 1 : dim(DATABASE_Linear)[1]){
    DATABASE_Linear$Rosen[i]<-metafor::fsn(`Effect size`, vi, data=DATABASE_plu[[length(Grouping_var)+1]][[i]],type="Rosenberg")$fsnum
    DATABASE_Linear$TF<-tryCatch({tidy(trimfill(DATABASE_Linear[[11]][[i]], control=list(stepadj=0.5)))},
                    error=function(e){'NA'})
    }

 # On assemble les effect sizes calculés (quand n_ES>2) et ceux quand n_ES=1:
  DATABASE_two<-dplyr::bind_rows(DATABASE_two,DATABASE_un)

  FINAL_freq<-plyr::join_all(list(DATABASE_two,DATABASE_Three,DATABASE_Quality,DATABASE_Quality_Red,DATABASE_Linear),
                         by=Grouping_var, type='left')

  return(FINAL_freq)

  # ### bayesian analysis
  #
  # rstan_options(auto_write = TRUE)
  #
  # priors <- c(prior(normal(0,1), class = Intercept),
  #             prior(cauchy(0,0.5), class = sd))
  #
  #
  #
  # ### Bayesian model 3 lvl #####
  # DATABASE_Bay_3lvl <- DATABASE_plu   %>%
  #   dplyr::mutate(data = furrr::future_map(data, ~.x %>% dplyr::rename("ES" = `Effect size`)),
  #                 fit_B = map_progress(data, ~ brm(ES|se(vi, sigma=TRUE) ~ 1+ (1|ID)+ (1|NUM),
  #                                    data =  .x,
  #                                    prior = priors,
  #                                    chains = chains,
  #                                    warmup = warmup,
  #                                    iter = iter,
  #                                    cores= workers,
  #                                    future = TRUE,
  #                                    thin = thin,
  #                                    inits = 0)),
  #                  tidied = purrr::map(fit_B, tidy,conf.int = TRUE),
  #                    WAIC = furrr::future_map(fit_B, ~ WAIC(add_criterion(.x, "waic"))$p_waic),
  #                    Rhat = furrr::future_map(fit_B, ~ summary(.x)$fixed$Rhat),
  #                    neff = furrr::future_map(fit_B, ~  length(which(neff_ratio(.x)<0.1))),
  #                    Nb_D = furrr::future_map(fit_B, ~ sum(subset(nuts_params(.x), Parameter == "divergent__")$Value))
  #                 ) %>%
  #   tidyr::unnest(tidied) %>%
  #   dplyr::rename_with(~ paste0(., "_Bay_3lvl"), -c(Grouping_var, "n_ES", "n_data", "n_MA","data"))
  #
  #
  #
  # ### Bayesian model 2 lvl #####
  # DATABASE_Bay_2lvl <- DATABASE_plu  %>%
  #   dplyr::mutate(data = furrr::future_map(data, ~.x %>% dplyr::rename("ES" = `Effect size`)),
  #                 fit_B = map_progress(data, ~ brm(ES|se(vi, sigma=TRUE) ~ 1+ (1|ID),
  #                                                data =  .x,
  #                                                prior = priors,
  #                                                chains = chains,
  #                                                warmup = warmup,
  #                                                iter = iter,
  #                                                cores= workers,
  #                                                future = TRUE,
  #                                                thin = thin,
  #                                                inits = 0)),
  #                 tidied = purrr::map(fit_B, tidy,conf.int = TRUE),
  #                 WAIC = furrr::future_map(fit_B, ~ WAIC(add_criterion(.x, "waic"))$estimates[3,1]),
  #                 Rhat = furrr::future_map(fit_B, ~ summary(.x)$fixed$Rhat),
  #                 neff = furrr::future_map(fit_B, ~  length(which(neff_ratio(.x)<0.1))),
  #                 Nb_D = furrr::future_map(fit_B, ~ sum(subset(nuts_params(.x), Parameter == "divergent__")$Value))
  #   )%>%
  #   tidyr::unnest(tidied) %>%
  #   dplyr::rename_with(~ paste0(., "_Bay_2lvl"), -c(Grouping_var, "n_ES", "n_data", "n_MA","data"))
  #
  #
  # ### Bayesian model 2 lvl and quality #####
  # DATABASE_Bay_2lvlQ <- DATABASE_plu   %>%
  #   dplyr::mutate(data = furrr::future_map(data, ~.x %>% dplyr::rename("ES" = `Effect size`)),
  #                 fit_B = map_progress(data, ~ brm(ES|se(QUALITY_DOI(SCORE =SCORE, vi= vi), sigma=TRUE) ~ 1+ (1|ID),
  #                                                data =  .x,
  #                                                prior = priors,
  #                                                chains = chains,
  #                                                warmup = warmup,
  #                                                iter = iter,
  #                                                cores= workers,
  #                                                future = TRUE,
  #                                                thin = thin,
  #                                                inits = 0)),
  #                 tidied = furrr::future_map(fit_B, tidy,conf.int = TRUE),
  #                 WAIC = furrr::future_map(fit_B, ~ WAIC(add_criterion(.x, "waic"))$estimates[3,1]),
  #                 Rhat = furrr::future_map(fit_B, ~ summary(.x)$fixed$Rhat),
  #                 neff = furrr::future_map(fit_B, ~  length(which(neff_ratio(.x)<0.1))),
  #                 Nb_D = furrr::future_map(fit_B, ~ sum(subset(nuts_params(.x), Parameter == "divergent__")$Value))
  #   )%>%
  #   tidyr::unnest(tidied) %>%
  #   dplyr::rename_with(~ paste0(., "_2lvlQ"), -c(Grouping_var, "n_ES", "n_data", "n_MA","data"))
  #
  #
  # ### Bayesian model 3 lvl and quality #####
  # DATABASE_Bay_3lvlQ <- DATABASE_plu  %>%
  #   dplyr::mutate(data = furrr::future_map(data, ~.x %>% dplyr::rename("ES" = `Effect size`)),
  #                 fit_B = map_progress(data, ~ brm(ES|se(QUALITY_DOI(SCORE =SCORE, vi= vi), sigma=TRUE) ~ 1+ (1|ID) + (1|NUM),
  #                                                data =  .x,
  #                                                prior = priors,
  #                                                chains = chains,
  #                                                warmup = warmup,
  #                                                iter = iter,
  #                                                cores= workers,
  #                                                future = TRUE,
  #                                                thin = thin,
  #                                                inits = 0)),
  #                 tidied = furrr::future_map(fit_B, tidy,conf.int = TRUE),
  #                 WAIC = furrr::future_map(fit_B, ~ WAIC(add_criterion(.x, "waic"))$estimates[3,1]),
  #                 Rhat = furrr::future_map(fit_B, ~ summary(.x)$fixed$Rhat),
  #                 neff = furrr::future_map(fit_B, ~  length(which(neff_ratio(.x)<0.1))),
  #                 Nb_D = furrr::future_map(fit_B, ~ sum(subset(nuts_params(.x), Parameter == "divergent__")$Value))
  #   )%>%
  #   tidyr::unnest(tidied) %>%
  #   dplyr::rename_with(~ paste0(., "_3lvlQ"), -c(Grouping_var, "n_ES", "n_data", "n_MA","data"))
  #
  #
  #
  # FINAL_Bay<-plyr::join_all(list(DATABASE_Bay_3lvl,DATABASE_Bay_2lvl,DATABASE_Bay_2lvlQ,DATABASE_Bay_3lvlQ),
  #                            by=Grouping_var, type='left')
  # return(FINAL_Bay)

                      }

