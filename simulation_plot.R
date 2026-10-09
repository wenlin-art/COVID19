install.packages("patchwork")
##################################################################################
#this is for simulation plot, to see whether simulation catch the true value
library(ggplot2)
library(dplyr)
library(patchwork)
#prepare data, include #I version, result has 2 lists
out_file <- "C:\\Users\\willi\\OneDrive\\Desktop\\research\\simulation result\\big_a\\F1ph1 8596"
all_files<-list.files(out_file,pattern = "iteration.*\\.rds",full.names = TRUE)
final_results<-lapply(all_files,readRDS)

0.2288*7565/39279

#####################################################################################
#########################MLE design############################################
out_file <- "C:\\Users\\willi\\OneDrive\\Desktop\\research\\simulation result\\MLE\\F5PH4_MLE"
all_files<-list.files(out_file,pattern = "iteration.*\\.rds",full.names = TRUE)
final_results<-lapply(all_files,readRDS)

params <- c("a", "h", "pi", "rh", "ph", "ds", "di", "dh", "dr")

summary_data <- data.frame(
  parameter = character(),
  iteration = integer(),
  mean = numeric(),
  lower_ci = numeric(),
  upper_ci = numeric(),
  true_value = numeric(),
  stringsAsFactors = FALSE
)

for (j in seq_along(final_results)) {
  
  res <- final_results[[j]]
  
  for (k in seq_along(res$parameter)) {
    
    param <- res$parameter[k]
    
    summary_data <- rbind(
      summary_data,
      data.frame(
        parameter = param,
        iteration = j,
        mean = res$MLE[param],
        lower_ci = res$lower[param],
        upper_ci = res$upper[param],
        true_value = res$true[k]
      )
    )
  }
}

#simulation summary table
table_data <- summary_data %>%
  group_by(parameter) %>%
  summarise(
    true_value = mean(true_value),
    estimate = mean(mean),
    coverage = mean(
      lower_ci <= true_value &
        upper_ci >= true_value
    ),
    bias = estimate - true_value,
    rmse = sqrt(mean((mean - true_value)^2)),
    .groups = "drop"
  )

##PLOT ESTIMATION:600*300
ggplot(summary_data, aes(x = parameter, y = mean)) +
  geom_boxplot(outlier.alpha = 0.4) +
  geom_point(
    data = summary_data %>% distinct(parameter, true_value),
    aes(x = parameter, y = true_value),
    color = "red",
    size = 2
  ) +
  labs(
    x = "Parameter",
    y = "Posterior mean",
    title = "Simulation results: Female,65+,phase4"
  )  +
  scale_x_discrete(
    labels = c(
      a  = expression(alpha),
      h  = expression(h),
      pi = expression(phi[I]),
      rh = expression(r),
      ph = expression(phi[H]),
      ds = expression(p^S),
      di = expression(p^I),
      dh = expression(p^H),
      dr = expression(p^r)
    )
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
  )

#########################MLE design end############################################
########################################################################################

#prepare data
out_file <- "C:\\Users\\willi\\OneDrive\\Desktop\\research\\simulation result\\final\\HI_PH4_20k"
#out_file <- "C:\\Users\\willi\\OneDrive\\Desktop\\research\\simulation result\\big_a\\F1ph1 3000"
all_files<-list.files(out_file,pattern = "iteration.*\\.rds",full.names = TRUE)
final_results<-lapply(all_files,readRDS)

params<-c("a","h","pi","rh","ph","ds","di","dh","dr")

summary_data<-data.frame(
  parameter=character(),
  iteration=integer(),
  mean=numeric(),
  lower_ci=numeric(),
  upper_ci=numeric(),
  true_value=numeric(),
  stringsAsFactors = FALSE
)

for (j in seq_along(final_results)) {
  res<-final_results[[j]]
  for (param in params) {
    summary_data<-rbind(summary_data,data.frame(
      parameter=param,
      iteration=j,
      mean=res[[param]]["mean"],
      lower_ci=res[[param]]["coverage1"],
      upper_ci=res[[param]]["coverage2"],
      true_value=res[[param]]["true"]
    ))
    
  }
  
}

#simulation summary table
table_data <- summary_data %>%
  group_by(parameter) %>%
  summarise(
    true_value = mean(true_value),
    estimate = mean(mean),
    coverage = mean(
      lower_ci <= true_value &
        upper_ci >= true_value
    ),
    bias = estimate - true_value,
    rmse = sqrt(mean((mean - true_value)^2)),
    .groups = "drop"
  )



##PLOT ESTIMATION:600*300
ggplot(summary_data, aes(x = parameter, y = mean)) +
  geom_boxplot(outlier.alpha = 0.4) +
  geom_point(
    data = summary_data %>% distinct(parameter, true_value),
    aes(x = parameter, y = true_value),
    color = "red",
    size = 2
  ) +
  labs(
    x = "Parameter",
    y = "Posterior mean",
    title = "Simulation results: Female,45-65,phase1"
  )  +
  scale_x_discrete(
    labels = c(
      a  = expression(alpha),
      h  = expression(h),
      pi = expression(phi[I]),
      rh = expression(r),
      ph = expression(phi[H]),
      ds = expression(p^S),
      di = expression(p^I),
      dh = expression(p^H),
      dr = expression(p^r)
    )
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
  )

##play again,start from preparing the data
out_file <- "C:\\Users\\willi\\OneDrive\\Desktop\\research\\simulation result\\big_a\\random_number 8000"
all_files<-list.files(out_file,pattern = "iteration.*\\.rds",full.names = TRUE)
final_results<-lapply(all_files,readRDS)

params<-c("a","h","pi","rh","ph","ds","di","dh","dr")

summary_data1<-data.frame(
  parameter=character(),
  iteration=integer(),
  mean=numeric(),
  lower_ci=numeric(),
  upper_ci=numeric(),
  true_value=numeric(),
  stringsAsFactors = FALSE
)

for (j in seq_along(final_results)) {
  
  res<-final_results[[j]]
  est <- res$estimation
  for (param in params) {
    summary_data1<-rbind(summary_data1,data.frame(
      parameter=param,
      iteration=j,
      mean=est[[param]]["mean"],
      lower_ci=est[[param]]["coverage1"],
      upper_ci=est[[param]]["coverage2"],
      true_value=est[[param]]["true"]
    ))
    
  }
  
}


#simulation summary table
table_data <- summary_data1 %>%
  group_by(parameter) %>%
  summarise(
    true_value = mean(true_value),
    estimate = mean(mean),
    coverage = mean(
      lower_ci <= true_value &
        upper_ci >= true_value
    ),
    bias = estimate - true_value,
    rmse = sqrt(mean((mean - true_value)^2)),
    .groups = "drop"
  )

#plot simulation,save plot:600*300

high<-ggplot(summary_data, aes(x = parameter, y = mean)) +
  geom_boxplot(outlier.alpha = 0.4) +
  geom_point(
    data = summary_data %>% distinct(parameter, true_value),
    aes(x = parameter, y = true_value),
    color = "red",
    size = 2
  ) +
  labs(
    x = "Parameter",
    y = "Posterior mean",
    title = "Simulation results: Female,18-24,phase2"
  )  +
  scale_x_discrete(
    labels = c(
      a  = expression(alpha),
      h  = expression(h),
      pi = expression(phi[I]),
      rh = expression(r),
      ph = expression(phi[H]),
      ds = expression(p^S),
      di = expression(p^I),
      dh = expression(p^H),
      dr = expression(p^r)
    )
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
  )

low<-ggplot(summary_data1, aes(x = parameter, y = mean)) +
  geom_boxplot(outlier.alpha = 0.4) +
  geom_point(
    data = summary_data1 %>% distinct(parameter, true_value),
    aes(x = parameter, y = true_value),
    color = "red",
    size = 2
  ) +
  labs(
    x = "Parameter",
    y = "Posterior mean",
    title = "Simulation results: low-information setting"
  ) +
  scale_x_discrete(
    labels = c(
      a  = expression(alpha),
      h  = expression(h),
      pi = expression(phi[I]),
      rh = expression(r),
      ph = expression(phi[H]),
      ds = expression(p^S),
      di = expression(p^I),
      dh = expression(p^H),
      dr = expression(p^r)
    )
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
  )
high+low


################################################################################
###true data, Does 50,000 posterior samples from MCMC cautch the true value?
#boxplot:50,000 samples
#true value:#observation/#sample size

library(coda)
library(tidyverse)

library(dplyr)
#sum_df has mean,95%CI
tidy_one_summary <- function(sum_df, group, stage) {
  sum_df %>%
    mutate(
      param = rownames(sum_df),
      group = group,
      stage = stage
    ) %>%
    select(param, group, stage,
           mean = Mean,
           q025 = `2.5%`,
           q50  = `50%`,
           q975 = `97.5%`)
}

tidy_list <- list(
  tidy_one_summary(sum_F1_s1, "F1", 1),
  tidy_one_summary(sum_F1_s2, "F1", 2),
  tidy_one_summary(sum_F1_s3, "F1", 3),
  tidy_one_summary(sum_F1_s4, "F1", 4),
  tidy_one_summary(sum_M1_s1, "M1", 1)
  # ...
)

final_df <- bind_rows(tidy_list)

inf_df <- final_df %>% filter(param == "inf_rate")

ggplot(inf_df, aes(x = stage, y = q50)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.15) +
  facet_wrap(~ group) +
  theme_bw()




low<-ggplot(summary_data1, aes(x = parameter, y = mean)) +
  geom_boxplot(outlier.alpha = 0.4) +
  geom_point(
    data = summary_data1 %>% distinct(parameter, true_value),
    aes(x = parameter, y = true_value),
    color = "red",
    size = 2
  ) +
  labs(
    x = "Parameter",
    y = "Posterior mean",
    title = "Simulation results: low-information setting"
  ) +
  scale_x_discrete(
    labels = c(
      a  = expression(alpha),
      h  = expression(h),
      pi = expression(phi[I]),
      rh = expression(r),
      ph = expression(phi[H]),
      ds = expression(p^S),
      di = expression(p^I),
      dh = expression(p^H),
      dr = expression(p^r)
    )
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
  )

##################################################################################
#for analysis true estimation result
#######generate some fake data
get_obs<-function(N,Time){
  a<-0.01  #infected rate=constant
  h<-0.002 #hospitalization rate when infected
  pi<-0.9 #survival rate when unhospitalized
  rh<-0.388  #released rate when hospitalized
  ph<-0.98 #survival rate when hospitalized
  ds<- 0.021 #pro of detected as susceptible
  di<- 0.012 #pro of detected as infected
  dh<- 0.79 #pro of detected as hospitolized
  dr<-0.95
  
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
  
  xhid <- matrix(NA, nrow = N, ncol = Time)
  xobs <- matrix(NA, nrow = N, ncol = Time)
  for (k in 1:N){
    xhid[k,1] <- sample(1:5, 1, prob = c(1,0,0,0,0))
    xobs[k,1] <- sample(1:6, 1, prob = detect[xhid[k,1],]) 
    
    for (t in 2:Time) {
      xhid[k,t] <- sample(1:5, 1, prob = bio[xhid[k,t-1],])
      xobs[k,t] <- sample(1:6, 1, prob = detect[xhid[k,t],])
    }
  }
  ##xobs is the observation
  #to make simulation consistant with the true data, transfer numerical form into character form
  xobs_df <- as.data.frame(xobs)
  
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
  
  colnames(xobs_df) <- c("ID", paste0("W", 1:Time))
  return(xobs_df)
  
}

F1<-get_obs(100,10)
F2<-get_obs(10,100)
F3<-get_obs(10,100)
F4<-get_obs(10,100)
F5<-get_obs(10,100)

############analys observation data, count states by week.
library(dplyr)
library(tidyr)

count_states_by_week <- function(xobs_df, dataset_name) {
  xobs_df %>%
    pivot_longer(
      cols = -1,
      names_to = "week",
      values_to = "state"
    ) %>%
    mutate(
      week = as.integer(sub("W", "", week)),
      dataset = dataset_name
    ) %>%
    filter(state %in% c("I", "H", "D")) %>%
    group_by(dataset, week, state) %>%
    summarise(count = n(), .groups = "drop")
}

all_counts <- bind_rows(
  count_states_by_week(F1, "F1"),
  count_states_by_week(F2, "F2"),
  count_states_by_week(F3, "F3"),
  count_states_by_week(F4, "F4"),
  count_states_by_week(F5, "F5")
)
F1week<-count_states_by_week(F1, "F1")

#I states change

ggplot(
  filter(all_counts, state == "I"),
  aes(x = week, y = count, color = dataset)
) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Weekly number of I individuals",
    x = "Week",
    y = "Count",
    color = "Dataset"
  ) +
  theme_bw()+ 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


#I,H,D count together
ggplot(
  all_counts,
  aes(x = week, y = count, color = dataset)
) +
  geom_line(linewidth = 1) +
  geom_point() +
  facet_wrap(~ state, scales = "free_y") +
  labs(
    x = "Week",
    y = "Count",
    color = "Dataset",
    title = "Weekly counts of I, H, and D states"
  ) +
  theme_bw()

###########haven't debug this one. this is for plot estimation across all phases and compare subpopulation
#fake data
simulate_parameter_estimates <- function(
    group_name = paste0("F", 1:5),
    parameters = c("a","h","di","ds","dh","dr","pi","ph","rh"),
    phases = c("phase1","phase2","phase3","phase4"),
    seed = 123
) {
  set.seed(seed)
  
  df <- expand.grid(
    phase = phases,
    group = group_name,
    parameter = parameters,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  
  # 给不同参数设定合理的基准值
  base_value <- dplyr::case_when(
    df$parameter == "a"  ~ 0.05,
    df$parameter == "h"  ~ 0.02,
    df$parameter == "di" ~ 0.30,
    df$parameter == "ds" ~ 0.60,
    df$parameter == "dh" ~ 0.15,
    df$parameter == "dr" ~ 0.10,
    df$parameter == "pi" ~ 0.95,
    df$parameter == "ph" ~ 0.20,
    df$parameter == "rh" ~ 0.08,
    TRUE ~ 0.1
  )
  
  # 模拟估计值（阶段效应 + 组别效应 + 随机噪声）
  df$estimate <- base_value +
    as.numeric(factor(df$phase)) * rnorm(nrow(df), 0, 0.004) +
    as.numeric(factor(df$group)) * rnorm(nrow(df), 0, 0.003)
  
  # 转为因子并设定顺序（方便作图）
  df$phase <- factor(df$phase, levels = phases)
  df$group <- factor(df$group, levels = group_name)
  df$parameter <- factor(df$parameter, levels = parameters)
  
  return(df)
}

 estimation_df<- simulate_parameter_estimates()

 estimation_df$phase <- factor(estimation_df$phase, levels = c("phase1","phase2","phase3","phase4"))
 estimation_df$group <- factor(estimation_df$group, levels = c("F1","F2","F3","F4","F5"))

ggplot(estimation_df, aes(x = phase, y = estimate,
               color = group, group = group)) +
  geom_line(size = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ parameter, scales = "free_y", ncol = 3) +
  labs(
    x = "Phase",
    y = "Estimate",
    color = "Age group"
  ) +
  theme_bw()



################################################################################
########################change a ##############################################
## adjustment for a
a_factor <- 8620 / 18265

a_results <- lapply(final_results, function(res) {
  
  a_mean  <- res$a["mean"] * a_factor
  a_lower <- res$a["coverage1"] * a_factor
  a_upper <- res$a["coverage2"] * a_factor
  a_true  <- 0.1142452
  
  c(
    mean = a_mean,
    lower_ci = a_lower,
    upper_ci = a_upper,
    true_value = a_true
  )
})

## convert to data frame
a_summary <- do.call(rbind, a_results)
a_summary <- as.data.frame(a_summary)

## calculate the four quantities
a_estimate <- mean(a_summary$mean)

a_coverage <- mean(
  a_summary$lower_ci <= a_summary$true_value &
    a_summary$upper_ci >= a_summary$true_value
)

a_bias <- a_estimate - a_summary$true_value[1]

a_rmse <- sqrt(
  mean(
    (a_summary$mean - a_summary$true_value)^2
  )
)

## final results
cat("Estimate =", a_estimate, "\n")
cat("Coverage =", a_coverage, "\n")
cat("Bias     =", a_bias, "\n")
cat("RMSE     =", a_rmse, "\n")
##############################reculculate a end###################################
################################################################################