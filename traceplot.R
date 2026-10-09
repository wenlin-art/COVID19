
library(coda)
library(MCMCvis)

samplelist<-readRDS("C:/Users/willi/OneDrive/Desktop/research/simulation result/prior_sensitivity/weak_info_3chain.rds")


mcmc_list <- mcmc.list( lapply(samplelist, as.mcmc))

new_names <- c(
  a  = "alpha",
  h  = "h",
  pi = "phi_I",
  rh = "r",
  ph = "phi_H",
  ds = "p_S",
  di = "p_I",
  dh = "p_H",
  dr = "p_R"
)


for(i in seq_along(mcmc_list)) {
  
  old_names <- colnames(mcmc_list[[i]])
  
  colnames(mcmc_list[[i]]) <-
    unname(new_names[old_names])
  
}

MCMCtrace(
  mcmc_list,
  params = c(
    "phi_H",
    "p_S",
    "p_I",
    "p_H",
    "p_R",
    "phi_I",
    "r",
    "h",
    "alpha"
  ),
  iter = 200000,
  ISB = FALSE,
  pdf = TRUE,
  filename = "MCMCtrace_renamed.pdf"
)



MCMCtrace(
  mcmc_list,
  params = c(
    "phi_H",
    "p_S",
    "p_I",
    "p_H",
    "p_R",
    "phi_I",
    "r",
    "h",
    "alpha"
  ),
  iter = 200000,
  Rhat = TRUE,
  n.eff = TRUE,
  pdf = TRUE,
  filename = "simulation_F5ph4.pdf"
)
