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
GPS_m <- train_GPS(as.matrix(mydata[,c("cf1", "cf2", "cf3", "cf4",
                                       "cf5", "cf6")]),
                   as.matrix(mydata$treat))
head(GPS_m)

## -----------------------------------------------------------------------------
set.seed(129)

sim_data <- generate_synthetic_data(sample_size = 200, gps_spec = 3)

# Estimate GPS function
GPS_m <- train_GPS(cov_mt = as.matrix(sim_data[,-(1:2)]),
                  w_all = as.matrix(sim_data$treat))
# exposure values
w_all = seq(0,20,0.1)
data.table::setDT(sim_data)
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
sim.data <- generate_synthetic_data(sample_size = 200, gps_spec = 3)
# Estimate GPS function
GPS_m <- train_GPS(cov_mt = as.matrix(sim_data[,-(1:2)]),
                  w_all = as.matrix(sim_data$treat))
# exposure values
w.all <- seq(0,20,0.5)
data.table::setDT(sim_data)
cerf_nngp_obj <- estimate_cerf_nngp(sim_data,
                                   w_all,
                                   GPS_m,
                                   params = list(alpha = c(0.1,0.2),
                                                 beta = 0.2,
                                                 g_sigma = 1,
                                                 tune_app = "all",
                                                 n_neighbor = 20,
                                                 expand = 1,
                                                 block_size = 1e4),
                                   nthread = 1)

plot(cerf_nngp_obj)

