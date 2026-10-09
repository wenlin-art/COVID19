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

