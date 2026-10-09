
library(dplyr)
library(nimble)
library(parallel)
#for my function states_matrix()
#install.packages("tidyr")
library(tidyr)
#library(stringr)
#for traceplot
library(MCMCvis)
library(coda)

###########################################################################
###########################################################################
######whole simulation code on digital canada
###########################################################################
###########################################################################

.libPaths(c("~/Rlibs", .libPaths()))
args<-commandArgs(trailingOnly = TRUE)
library(nimble)
library(dplyr)
i<- as.numeric(args[1]) 
#i<-1
set.seed(i)

#for my function states_matrix
library(tidyr)

###########################################################################
######prepare the data
###########################################################################
#I need xobs,part_known,initial
###first, generate xobs
######simulation data=F5PH4
n<-18265
infected_n<-8620
TIME <- 15 #18,20,21,15
true_a<-0.24207517

a<-true_a*infected_n/n  #infected rate, true*n/N
dh<- 0.50 #pro of detected as hospitolized
di<- 0.09 #pro of detected as infected
dr<-0.99
ds<- 0.07 #pro of detected as susceptible
h<-0.0163 #hospitalization rate when infected
ph<-0.96 #survival rate when hospitalized
pi<-0.99 #survival rate when unhospitalized
rh<-0.12  #released rate when hospitalized


bio<-matrix(
  c(
    1-a,a,0,0,0,
    0,(1-h)*pi,h*pi,0,1-pi,
    0,0,ph*(1-rh),ph*rh,1-ph,
    0,0,0,1,0,
    0,0,0,0,1
  ),nrow=5,byrow=TRUE
)

detect<-matrix(
  c(
    1-ds,ds,0,0,0,0,
    1-di,0,di,0,0,0,
    1-dh,0,0,dh,0,0,
    1-dr,0,0,0,dr,0,
    0,0,0,0,0,1
  ),nrow=5,byrow=TRUE
)

xhid <- matrix(NA, nrow = n, ncol = TIME)
xobs <- matrix(NA, nrow = n, ncol = TIME)
for (k in 1:n){
  xhid[k,1] <- sample(1:5, 1, prob = c(1,0,0,0,0))
  xobs[k,1] <- sample(1:6, 1, prob = detect[xhid[k,1],]) 
  
  for (t in 2:TIME) {
    xhid[k,t] <- sample(1:5, 1, prob = bio[xhid[k,t-1],])
    xobs[k,t] <- sample(1:6, 1, prob = detect[xhid[k,t],])
  }
}

##################################################################
# Force the first H state to be observed as H
for (k in 1:n) {
  first_H <- which(xhid[k, ] == 3)[1]
  
  if (!is.na(first_H)) {
    xobs[k, first_H] <- 4
  }
}

##minibatch,n=20,000
#shuffle<-unique(sample(1:n))
#xobs <- xobs[shuffle[1:200], ]


#################################################################

###consider only IH
has_IH <- apply(xobs, 1, function(x) any(x %in% c(3,4,5,6)))
obs_IH<-xobs[has_IH,]
n<-nrow(obs_IH)
##xobs is the observation
#to make simulation consistant with the true data, transfer numerical form into character form
xobs_df <- as.data.frame(obs_IH)

xobs_df$ID <- paste0("B", seq_len(nrow(xobs_df)))
xobs_df <- xobs_df[, c("ID", setdiff(names(xobs_df), "ID"))]

state_map <- c(
  "1" = NA,
  "2" = "S",
  "3" = "I",
  "4" = "H",
  "5" = "R",
  "6" = "D"
)

for (j in 2:ncol(xobs_df)) {
  xobs_df[[j]] <- state_map[as.character(xobs_df[[j]])]
}

colnames(xobs_df) <- c("ID", paste0("W", 1:TIME))

##to get part_known 
states_matrix<-function(observation){
  result<-observation
  
  for(i in 1:nrow(observation)){
    states<-unlist(observation[i,-1])
    n<-length(states)
    
    #all S before 1st S
    first_non_na<-which(!is.na(states))[1]
    if(!is.na(first_non_na)){
      if(states[first_non_na] == "S" && first_non_na>1){states[1:(first_non_na-1)]<-"S"}
    }else{states[1:n]<-"S"}
    
    #fill in NA with the same state as 2 same states next it
    status_sequence<-as.character(states)
    status_sequence<-replace_na(status_sequence,"NA")
    rle_result<-rle(status_sequence)
    modified<-FALSE
    if(length(rle_result$lengths)>=3){
      for(j in 2:(length(rle_result$values)-1)){
        if(rle_result$values[j]=="NA"){
          if(rle_result$values[j-1]=="S" && rle_result$values[j+1]=="S"){
            rle_result$values[j]<-"S"
            modified<-TRUE
          }else if(rle_result$values[j-1]=="H" && rle_result$values[j+1]=="H"){
            rle_result$values[j]<-"H"
            modified<-TRUE
          }
        }
      }
    }
    
    
    if(modified){
      new_sequence<-inverse.rle(rle_result)
      new_sequence[new_sequence=="NA"]<-NA
      states<-as.list(new_sequence)
    }
    
    result[i,-1]<-as.list(states)
  }
  
  return(result)
}
part_known<-states_matrix(xobs_df)

##to get all  known states
smart_init<-function(partly_known){
  init<-partly_known
  
  for(i in 1:nrow(partly_known)){
    status_sequence<-as.character(partly_known[i,-1])
    status_sequence<-replace_na(status_sequence,"NA")
    rle_result<-rle(status_sequence)
    ##everybody start from S
    modified<-FALSE
    if(rle_result$values[1]=="NA"){
      rle_result$values[1]<-"S"
      modified<-TRUE
    }
    ##fill in unknown as last status
    if(length(rle_result$lengths)>=2){
      for(j in 2:length(rle_result$values)){
        if(rle_result$values[j-1]!="NA" && rle_result$values[j]=="NA"){
          rle_result$values[j]<-rle_result$values[j-1]
          modified<-TRUE
        }
      }
    }
    
    if(modified){
      new_sequence<-inverse.rle(rle_result)
      new_sequence[new_sequence=="NA"]<-NA
      init[i,-1]<-as.list(new_sequence)
      
    }
  }
  
  #3loops smart adjustment
  for (iteration in 1:3){
    for(i in 1:nrow(init)){
      for(j in 2:(ncol(init)-1)){
        if (init[i,j]=="S" && init[i,(j+1)] %in% c("H","R","D")){
          init[i,j]<-"I"
        }else if (init[i,j]=="I" && init[i,(j+1)] == "R"){
          init[i,j]<-"H"
        }
      }
    }
  }
  
  return(init)
}  
all_states<-smart_init(xobs_df)


##to get initial
need_sample<-function(part_know,all_know){
  filled_only<-as.data.frame(matrix(NA,nrow=nrow(all_know),ncol=ncol(all_know)))
  names(filled_only)<-names(all_know)
  filled_only[,1]<-all_know[,1]
  
  for(i in 1:nrow(part_know)){
    for(j in 2:ncol(part_know)){
      if (is.na(part_know[i,j]) && !is.na(all_know[i,j])){
        filled_only[i,j]<-all_know[i,j]
      }
    }
  }
  return(filled_only)
}
initial<-need_sample(part_known,all_states)

###transfer back to numeric form metrix
xobs<-xobs_df%>%
  mutate(across(everything(),
                ~case_when(
                  .=="S"~2,
                  .=="I"~3,
                  .=="H"~4,
                  .=="R"~5,
                  .=="D"~6,
                  is.na(.)~1 )))%>%
  select(-1)

part_known<-part_known%>%
  mutate(across(everything(),
                ~case_when(
                  .=="S"~1,
                  .=="I"~2,
                  .=="H"~3,
                  .=="R"~4,
                  .=="D"~5 )))%>%
  select(-1)

all_states<-all_states%>%
  mutate(across(everything(),
                ~case_when(
                  .=="S"~1,
                  .=="I"~2,
                  .=="H"~3,
                  .=="R"~4,
                  .=="D"~5 )))%>%
  select(-1)

initial<-initial%>%
  mutate(across(everything(),
                ~case_when(
                  .=="S"~1,
                  .=="I"~2,
                  .=="H"~3,
                  .=="R"~4,
                  .=="D"~5 )))%>%
  select(-1)

###########################################################################
###nimble model
###########################################################################
model_code<-nimbleCode({
  a ~ dbeta(0.25,4.75) #0-0.1
  h ~ dbeta(0.25,4.75) #0-0.1
  pi ~ dbeta(3.75,1.25) #0.5-1
  rh ~ dbeta(1,1)
  ph~ dbeta(3.75,1.25) #0.5-1
  ds~ dbeta(0.25,4.75) #0-0.1
  di~ dbeta(0.5,4.5) #0.01-0.2
  dh~ dbeta(1,1)
  dr~ dbeta(1,1)
  
  phi2[1, 1] <- 1-a
  phi2[1, 2] <- a
  phi2[1, 3] <-0
  phi2[1, 4] <- 0
  phi2[1, 5] <- 0
  phi2[2, 1] <-0
  phi2[2, 2] <- (1-h)*pi
  phi2[2, 3] <-h*pi
  phi2[2, 4] <- 0
  phi2[2, 5] <- 1-pi
  phi2[3, 1] <- 0
  phi2[3, 2] <- 0
  phi2[3, 3] <-ph*(1-rh)
  phi2[3, 4] <- ph*rh
  phi2[3, 5] <- 1-ph
  phi2[4, 1] <- 0
  phi2[4, 2] <- 0
  phi2[4, 3] <-0
  phi2[4, 4] <- 1
  phi2[4, 5] <- 0
  phi2[5, 1] <- 0
  phi2[5, 2] <- 0
  phi2[5, 3] <-0
  phi2[5, 4] <- 0
  phi2[5, 5] <- 1
  
  
  phi3[1, 1] <-1-ds
  phi3[1, 2] <- ds
  phi3[1, 3] <-0
  phi3[1, 4] <- 0
  phi3[1, 5] <- 0
  phi3[1, 6] <-0
  phi3[2, 1] <-1-di 
  phi3[2, 2] <- 0
  phi3[2, 3] <-di
  phi3[2, 4] <- 0
  phi3[2, 5] <- 0
  phi3[2, 6] <- 0
  phi3[3, 1] <- 1-dh
  phi3[3, 2] <- 0
  phi3[3, 3] <-0
  phi3[3, 4] <-dh
  phi3[3, 5] <- 0
  phi3[3, 6] <-0
  phi3[4, 1] <- 1-dr
  phi3[4, 2] <- 0
  phi3[4, 3] <-0
  phi3[4, 4] <- 0
  phi3[4, 5] <- dr
  phi3[4, 6] <-0
  phi3[5, 1] <- 0
  phi3[5, 2] <- 0
  phi3[5, 3] <-0
  phi3[5, 4] <- 0
  phi3[5, 5] <- 0
  phi3[5, 6] <-1
  
  for (k in 1:n){
    # pi0[1:5] ~ ddirch(alpha[1:5])
    xhid[k,1] ~ dcat(pi0[1:5])
    xobs[k,1] ~ dcat(phi3[xhid[k,1],1:6])
    
    for(t in 2:TIME){
      xhid[k,t] ~ dcat(phi2[xhid[k,t-1], 1:5]) 
      xobs[k,t] ~ dcat(phi3[xhid[k,t], 1:6]) 
    }
  }
  
  
})


model_constants <-list(n = n, pi0 = c(1,0,0,0,0),TIME=TIME)
model_data <- list(xobs = xobs,xhid=part_known)
model_inits <- list(
  ph=0.9,
  ds=0.03,
  di=0.02,
  dh=0.5,
  dr=0.9,
  pi = 0.9,
  rh = 0.5,  
  h = 0.001, 
  a = 0.001,
  xhid = initial
)
model_dimensions <- list(
  xhid=c(n,TIME),xobs=c(n,TIME)
)

########### build model,single run######
#markov_model <- nimbleModel(
# code = model_code,
#  data = model_data,
#  constants = model_constants,
#  inits = model_inits,
#  dimensions = model_dimensions,
#)

# setup MCMC
#markov_mcmc_config <- configureMCMC(markov_model, monitors = c("ph","ds","di","dh","dr",
#                                                               "pi","rh","h","a" ))

#markov_mcmc <- buildMCMC(markov_mcmc_config)

# compile and run
#compiled_model <- compileNimble(markov_model)
#compiled_mcmc <- compileNimble(markov_mcmc, project = compiled_model)
#samples <- runMCMC(compiled_mcmc, niter = 50, nburnin = 10)

#########parallel,3 chains########################################
nchain<-3
startif_init<-list(
  list(
    ph=0.5,
    ds=0.5,
    di=0.5,
    dh=0.5,
    dr=0.5,
    pi = 0.5,
    rh = 0.5,  
    h = 0.5, 
    a = 0.5,
    xhid = initial
  ),
  list(
    ph=0.9,
    ds=0.03,
    di=0.02,
    dh=0.5,
    dr=0.9,
    pi = 0.9,
    rh = 0.5,  
    h = 0.001, 
    a = 0.001,
    xhid = initial
  ),
  list(
    ph=0.1,
    ds=0.1,
    di=0.1,
    dh=0.1,
    dr=0.1,
    pi = 0.1,
    rh = 0.1,  
    h = 0.1, 
    a = 0.1,
    xhid = initial
  )
)

cl<-makeCluster(nchain, type = "PSOCK")
clusterEvalQ(cl,{library(nimble)})
clusterExport(cl,varlist = c("model_code","model_data","model_constants","model_dimensions","model_inits",
                             "nchain","startif_init"),envir = .GlobalEnv)
samplelist<-parLapply(cl,seq_len(nchain),function(i){
  markov_model <- nimbleModel(
    code = model_code,
    data = model_data,
    constants = model_constants,
    inits = startif_init[i],
    dimensions = model_dimensions,
    
  )
  markov_mcmc_config <- configureMCMC(markov_model, monitors = c("ph","ds","di","dh","dr",
                                                                 "pi","rh","h","a" ))
  markov_mcmc <- buildMCMC(markov_mcmc_config)
  compiled_model <- compileNimble(markov_model)
  compiled_mcmc <- compileNimble(markov_mcmc, project = compiled_model)
  runMCMC(compiled_mcmc, niter = 50, nburnin = 0,inits = startif_init[i])
})
stopCluster(cl)
saveRDS(samplelist,
        file = "3chain.rds")
#mcmc_list<-mcmc.list(lapply(samplelist,as.mcmc))
#MCMCsummary(mcmc_list,probs = c(0.025,0.975))
#MCMCtrace(mcmc_list,params = c("ph","ds","di","dh","dr","pi","rh","h","a" ),iter = 50,ISB=FALSE,pdf = TRUE,
#          filename = "MCMCtrace1.pdf")
######
#######################################################################
##check results
########################################################################
results<-list(
  ph=c(mean=mean(samples[, "ph"]),
       coverage=as.numeric(quantile(samples[, "ph"],c(0.025,0.975))),
       true=ph),
  ds=c(mean=mean(samples[, "ds"]),
       coverage=as.numeric(quantile(samples[, "ds"],c(0.025,0.975))),
       true=ds),
  di=c(mean=mean(samples[, "di"]),
       coverage=as.numeric(quantile(samples[, "di"],c(0.025,0.975))),
       true=di),
  dh=c(mean=mean(samples[, "dh"]),
       coverage=as.numeric(quantile(samples[, "dh"],c(0.025,0.975))),
       true=dh),
  dr=c(mean=mean(samples[, "dr"]),
       coverage=as.numeric(quantile(samples[, "dr"],c(0.025,0.975))),
       true=dr),
  pi=c(mean=mean(samples[, "pi"]),
       coverage=as.numeric(quantile(samples[, "pi"],c(0.025,0.975))),
       true=pi), 
  rh=c(mean=mean(samples[, "rh"]),
       coverage=as.numeric(quantile(samples[, "rh"],c(0.025,0.975))),
       true=rh), 
  h=c(mean=mean(samples[, "h"]),
      coverage=as.numeric(quantile(samples[, "h"],c(0.025,0.975))),
      true=h), 
  a=c(mean=mean(samples[, "a"])*nrow(xobs)/18265,
      coverage=as.numeric(quantile(samples[, "a"],c(0.025,0.975)))*nrow(xobs)/18265,
      true=a),
  iteration=i
)

out_file <- "./MCMC_samples"
if (!dir.exists(out_file)) dir.create(out_file,recursive=TRUE)

saveRDS(results,file=file.path(out_file,paste0("iteration",i,".rds")))
#saveRDS(samples,file=file.path(out_file,paste0("weak_info_MCMCsamples.rds")))

# Print results with 5 decimal places
print(
  lapply(results, function(x) round(x, 5))
)

cat("completed iteration",i,"\n")
###trace plots
#mcmc<-as.mcmc(samples)
#MCMCtrace(mcmc,params = c("ph", "pi", "rh", "h","a", "dh","ds", "di", "dr"),iter=60,ISB=FALSE,pdf = FALSE)
#MCMCsummary(mcmc,probs=c(0.025,0.975))


###change the code
#sample size, time length, true value, true alpha value:true=a*22815/nrow(xobs), file name


###########################################################################
###########################################################################
######END whole simulation code on digital canada
###########################################################################
###########################################################################








