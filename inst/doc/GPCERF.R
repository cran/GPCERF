## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  library("devtools")
#  install_github("NSAPH-Software/GPCERF", ref="main")

## -----------------------------------------------------------------------------
library("GPCERF")

## -----------------------------------------------------------------------------
mydata <- generate_synthetic_data()
GPS_m <- train_gps(cov_mt = mydata[,c("cf1", "cf2", "cf3", "cf4",
                                       "cf5", "cf6")],
                  w_all = mydata$treat,
                  sl_lib = c("SL.xgboost"),
                  dnorm_log = FALSE)
head(GPS_m)

## -----------------------------------------------------------------------------
set.seed(129)

sim_data <- generate_synthetic_data(sample_size = 200, gps_spec = 3)

# Estimate GPS function
GPS_m <- train_gps(cov_mt = sim_data[,-(1:2)],
                   w_all = sim_data$treat,
                   sl_lib = c("SL.xgboost"),
                   dnorm_log = FALSE)

# exposure values
w_all = seq(0,20,0.1)
cerf_gp_obj <- estimate_cerf_gp(sim_data,
                               w_all,
                               GPS_m,
                               params = list(alpha = c(0.1,0.2,0.4),
                                             beta=0.2,
                                             g_sigma = 1,
                                             tune_app = "all"),
                               nthread = 1)

plot(cerf_gp_obj)

## -----------------------------------------------------------------------------
set.seed(19)
sim_data <- generate_synthetic_data(sample_size = 200, gps_spec = 3)

# Estimate GPS function
GPS_m <- train_gps(cov_mt = sim_data[,-(1:2)],
                   w_all = sim_data$treat,
                   sl_lib = c("SL.xgboost"),
                   dnorm_log = FALSE)
# exposure values
w_all <- seq(0, 20, 0.5)
cerf_nngp_obj <- estimate_cerf_nngp(sim_data,
                                    w_all,
                                    GPS_m,
                                    params = list(alpha = c(0.1, 0.2),
                                                  beta = 0.2,
                                                  g_sigma = 1,
                                                  tune_app = "all",
                                                  n_neighbor = 20,
                                                  expand = 1,
                                                  block_size = 1e4),
                                    formula = ~ . - 1 - Y - treat,
                                    nthread = 1)

plot(cerf_nngp_obj)

