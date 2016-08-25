library(dplyr)
#library(magrittr)
library(tibble)
library(phytools)
source("./cumsum_bm_paths.R")
tree <- phytools::pbtree(b=0.5, d=0.1, t=10, scale=1000, nsim=1,type="discrete")
plot(tree)

sigma <- 0.01

x_traj <- trajectories_bm(tree,sigma)
y_traj <- trajectories_bm(tree,sigma)
radius <- radius_bm(tree,sigma,min_rad=0.01,trend=0.01)


full_data <-  x_traj %>%
  inner_join(y_traj,by=c("times","from","to")) %>%
  inner_join(radius,by=c("times","from","to")) %>%
  arrange(times) %>%
  as.tbl()