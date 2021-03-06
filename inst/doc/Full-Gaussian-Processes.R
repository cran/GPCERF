## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load---------------------------------------------------------------------
library(GPCERF)
library(ggplot2)

## -----------------------------------------------------------------------------
set.seed(134)
# Generate dataset with a normally distributed exposure given covariates
data_sim_normal <- generate_synthetic_data(sample_size = 400, 
                                          outcome_sd = 10, 
                                          gps_spec = 1)
# Generate dataset with a t-distributed with 2df exposure given covariates
data_sim_t <- generate_synthetic_data(sample_size = 400, 
                                     outcome_sd = 10, 
                                     gps_spec = 2)

tru_R<-function(w, sim_data){
  design_mt <- model.matrix(~cf1+cf2+cf3+cf4+cf5+cf6-1, data = sim_data)
  mean(apply(design_mt, 1, function(x){
    -10 - sum(c(2, 2, 3, -1,2,2)*x) -
      w*(0.1 - 0.1*x[1] + 0.1*x[4] + 0.1*x[5] + 0.1*x[3]^2) + 0.13^2*w^3
  }))
}

erf_tru_normal <- sapply(seq(0,20,0.1), function(w) tru_R(w, data_sim_normal))
erf_tru_t <- sapply(seq(0,20,0.1), function(w) tru_R(w, data_sim_t))

## -----------------------------------------------------------------------------
GPS_m_normal <- train_GPS(cov_mt = as.matrix(data_sim_normal[,-(1:2)]),
                          w_all = as.matrix(data_sim_normal$treat))
GPS_m_t <- train_GPS(cov_mt = as.matrix(data_sim_t[,-(1:2)]),
                     w_all = as.matrix(data_sim_t$treat))

## -----------------------------------------------------------------------------
w_all = seq(0,20,0.1)
data.table::setDT(data_sim_normal)

gp_res_normal <- estimate_cerf_gp(data_sim_normal,
                                  w_all,
                                  GPS_m_normal,
                                  params = list(alpha = c(0.1),
                                                beta=0.2,
                                                g_sigma = 1,
                                                tune_app = "all"),
                                  nthread = 1)
plot(gp_res_normal) + 
  geom_line(data = data.frame(w = w_all, y = erf_tru_normal), 
            aes(x = w, y = y, color = "True"), size = 1.5)

## -----------------------------------------------------------------------------
data.table::setDT(data_sim_t)

gp_res_t <- estimate_cerf_gp(data_sim_t,
                                  w_all,
                                  GPS_m_t,
                                  params = list(alpha = c(0.1),
                                                beta=0.2,
                                                g_sigma = 1,
                                                tune_app = "all"),
                                  nthread = 1)
plot(gp_res_t) + 
  geom_line(data = data.frame(w = w_all, y = erf_tru_t), 
            aes(x = w, y = y, color = "True"), size = 1.5)

