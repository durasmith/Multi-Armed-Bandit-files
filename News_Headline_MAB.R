library(Rlab)
library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(ggpubr)
library(gganimate)
library(ggridges)
library(tidyverse)
library(transformr)
library(RColorBrewer)
library(gt)

# Website Receives 150 visits per minute

# Three Headlines to be tested

# Three Arms

# Starting the Algo


# Refreshing each 3 min
#nrf <- 60 / 3


# Refreshing each 5 min
nrf <- 60 / 5

df_t <- list()
df_g_mt <- list()
df_para <- list()

#Just to get the same result
set.seed(1995)

#Simulating this scenario 500 times
for(r in 1:500)

{
  # Declaring the number of visitors 
  n_visit <- rpois(1,150)
  
  #Defining Non-informative priori for each Arm
  post_v1 <- rbeta(1,1,1)
  post_v2 <- rbeta(1,1,1)
  post_v3 <- rbeta(1,1,1)
  
  # Generating randomly real conversion rate of each arm for Clear Advantage scenario
  P_b_arm <- runif(1,0,1) 
  #P_2_arm <- runif(1,0.75,0.85) * P_b_arm  #Clear Advantage Scenario
  #P_3_arm <- runif(1,0.75,0.85) * P_b_arm  #Clear Advantage Scenario
  P_2_arm <- runif(1,0.85,0.95) * P_b_arm  #Non-Clear Advantage Scenario
  P_3_arm <- runif(1,0.85,0.95) * P_b_arm  #Non-Clear Advantage Scenario
  
  
  #Declaring count variables for each Arm corresponding to a number of visitor in each arm
  n_v1 <- 1
  n_v2 <- 1
  n_v3 <- 1
  
  #Declaring count variables for each Arm
  r_v1 <- 1
  r_v2 <- 1
  r_v3 <- 1
  
  #Declaring vectors to store important results
  tt <- c()
  tt_v1 <- c()
  tt_v2 <- c()
  tt_v3 <- c()
  tt_aux_max <- c()
  mc_pr <- 0
  mc_pr_ct <- 0
  j <- 0  
  
# Starting Batched Multi Armed Bandit
for( j in 1:nrf)
{
  
# Calculating the Traffic Proportion by MC
  
  tt[j] <- j
  hjk <- c()
  
  for( i in 1: 2000)
  {
    #Simulating sample of Posteroris 2000x
    post_v1 <- rbeta(1,r_v1 + 1, n_v1 + 1 - r_v1)
    post_v2 <- rbeta(1,r_v2 + 1, n_v2 + 1 - r_v2)
    post_v3 <- rbeta(1,r_v3 + 1, n_v3 + 1 - r_v3)
    
    #Getting the Arm with maximum value
    hjk[i] <- which.max(c(post_v1,post_v2,post_v3))
    
  }
  
  #Calculating the Proportion by Monte Carlo Simulations results
  
  tt_v1[j] <- length(which(hjk == 1)) / 2000
  
  tt_v2[j] <- length(which(hjk == 2)) / 2000
  
  tt_v3[j] <- length(which(hjk == 3)) / 2000
  
  #Grouping sample of each Posteriori
  s <- c(post_v1,post_v2,post_v3) 
  
  #Selecting the maximum sample value
  aux_max_s <- max(s)
  
  #Identifying the Arm with maximum sample value
  aux_s <- which.max(s) 
  
  #Generating random results associated with each Arm according with the Real probability
  r_b_arm <- rbinom(1,round(tt_v1[j] * n_visit),P_b_arm)
  r_2_arm <- rbinom(1,round(tt_v2[j] * n_visit),P_2_arm)
  r_3_arm <- rbinom(1,round(tt_v3[j] * n_visit),P_3_arm)
  
  #Refreshing first Parameter of each Beta Posteriori            
  n_v1 <- n_v1 + round(tt_v1[j] * n_visit)
  n_v2 <- n_v2 + round(tt_v2[j] * n_visit)
  n_v3 <- n_v3 + round(tt_v3[j] * n_visit)
  
  #Refreshing second Parameter of each Beta Posteriori
  r_v1 <- r_v1 + r_b_arm
  r_v2 <- r_v2 + r_2_arm
  r_v3 <- r_v3 + r_3_arm
  
  #Sampling data according new refreshed Beta Posterioris
  post_v1 <- rbeta(1,r_v1 + 1, n_v1 + 1 - r_v1)
  post_v2 <- rbeta(1,r_v2 + 1, n_v2 + 1 - r_v2)
  post_v3 <- rbeta(1,r_v3 + 1, n_v3 + 1 - r_v3)
  
 # print(j) 
  
}


# Number of visitors over 1h
n_v1 #Arm 1
n_v2 #Arm 2
n_v3 #Amr 3

# Estimated probability of each arm
est_1 <- (1+r_v1) / ( (1+r_v1) + (n_v1 + 1 - r_v1) )
est_2 <- (1+r_v2) / ( (1+r_v2) + (n_v2 + 1 - r_v2) )
est_3 <- (1+r_v3) / ( (1+r_v3) + (n_v3 + 1 - r_v3) )

# Saving Estimated Results
df_para[[r]] <- data.frame(Num_Visitors = n_visit,
                           Real_Best_Arm = P_b_arm,
                           Real_Arm_2 = P_2_arm ,
                           Real_Arm_3 = P_3_arm,
                           Esti_Best_Arm = est_1,
                           Esti_Arm_2 = est_2,
                           Esti_Arm_3 = est_3
                           )

df_t[[r]] <- data.frame(#Time_Minutes = tt * 3, # for 3 min refresh
                        Time_Minutes = tt * 5, # for 5 min refresh
                   Arm_Optimal = tt_v1,
                   Arm_2  = tt_v2,
                   Arm_3 = tt_v3)

df_g_mt[[r]] <- df_t[[r]] %>% select(Time_Minutes,Arm_Optimal,Arm_2,Arm_3) %>%
  pivot_longer(cols = !Time_Minutes,
               names_to = "Arms",
               values_to = "Probability")

# Just to follow the progress
print(r)

}

#Stacking results

df_t_stack <- bind_rows(df_t, .id = "Simulation" )

df_g_mt_stack <- bind_rows(df_g_mt, .id = "Simulation" )

df_para_stack <- bind_rows(df_para, .id = "Simulation" )

df_g_mt_stack_avg <- df_g_mt_stack %>% 
                     group_by(Arms,Time_Minutes) %>%
                     summarise(Prob_Med = mean(Probability))
   

#Ploting Results

#Monte Carlo Plot
g_mc <- ggplot(data = df_g_mt_stack_avg, aes(x = Time_Minutes, y = Prob_Med, color = Arms )) +
       geom_line(size = 1.1)+
       theme_classic() +
       #geom_hline(yintercept = 0.95, linetype = 'dotted', size = 0.8) +
       labs(title = "Monte Carlo Probability by Time" ,
            x = "Time (min)",
            y = "Probability Estimated") +
      scale_x_continuous(breaks = seq(3,60,3)) + 
      scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1)) + 
      scale_color_brewer(palette ="Set1")
  
#Table of Parameters

t_par <- df_para_stack %>%
         mutate(Error_Best_Arm = abs(Real_Best_Arm - Esti_Best_Arm) / Real_Best_Arm ,
                Error_Arm_2 = abs(Real_Arm_2 - Esti_Arm_2) / Real_Arm_2,
                Error_Arm_ = abs(Real_Arm_3 - Esti_Arm_3) / Real_Arm_3) %>%
         select(!Simulation) %>%
         summarise_all(list(Mean = mean, SD = sd))

f_t_par <- t_par %>% gt() %>% 
  tab_header(title = "Parameters Summary Table")
f_t_par

#Speed Convergence Table

t_nc <- df_t_stack %>%
        filter(Time_Minutes %% 15 == 0) %>%
        mutate(Max_Arm = ifelse(Arm_Optimal > Arm_2 & Arm_Optimal > Arm_3, 1, 0)) %>%
        group_by(Time_Minutes) %>%
        summarise(Num_Convergence = sum(Max_Arm)) %>%
        mutate(Percentual_Convergence = Num_Convergence / 500)


# Final Report

ggarrange(g_mc, tableGrob(t_nc, rows = NULL),
          nrow = 2, ncol = 1)

