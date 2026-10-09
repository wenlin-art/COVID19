
library(coda)
library(MCMCvis)

samplelist<-readRDS("C:/Users/willi/OneDrive/Desktop/research/simulation result/prior_sensitivity/weak_info_3chain.rds")

#final_partly_5000n_100rep_time 15 ,  a=0.03
#file name=allS_bigah_iter50k_n 10000,a=0.7


folder<-"C:/Users/willi/OneDrive/Desktop/research/simulation result/prior_sensitivity"
#C:\Users\willi\OneDrive\Desktop\research\simulation result\big_a\F1ph1 3000
files <- list.files(
  path = folder,
  pattern = "\\.rds$",
  full.names = TRUE
)
final_results <- lapply(files, readRDS)

results[[1]]


#a dh di dr ds h ph  pi rh

mcmc_list <- mcmc.list( lapply(samplelist, as.mcmc))

MCMCtrace(
  mcmc_list,
  params = c("ph","ds","di","dh","dr","pi","rh","h","a"),
  iter = 200000,
  ISB = FALSE,
  pdf = TRUE,
  filename = "MCMCtrace1.pdf"
)

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
