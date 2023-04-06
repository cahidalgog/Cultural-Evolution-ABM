### Chapter 6: https://bookdown.org/amesoudi/ABMtutorial_bookdown/model6.html

# LIBRARIES AND LOAD DATA ----
# install.packages("pacman")
library(pacman)

## use function of pacman to install and load required libraries ##
p_load(here, ggraph, ggeasy, tidyverse, RColorBrewer, ggstatsplot, ggstatsplot, patchwork,
       gridExtra, esquisse, sjPlot)

## Set theme for plots ##
theme_set(theme_bw(base_family = 'serif'))



##  VERTICAL MODEL
VerticalTransmission <- function (N, p_0, s_v, t_max, r_max) {
  
  # create matrix with t_max rows and r_max columns, fill with NAs, convert to dataframe
  output <- as.data.frame(matrix(NA,t_max,r_max))  
  
  # purely cosmetic: rename the columns with run1, run2 etc.
  names(output) <- paste("run", 1:r_max, sep="")  
  
  for (r in 1:r_max) {
    
    # create first generation
    agent <- data.frame(trait = sample(c("A","B"), N, replace = TRUE, 
                                       prob = c(p_0,1-p_0)))  
    
    # add first generation's p to first row of column r
    output[1,r] <- sum(agent$trait == "A") / N  
    
    for (t in 2:t_max) {
      
      # create dataframe with a set of 2 randomly-picked parents for each agent
      parents <- data.frame(mother = sample(agent$trait, N, replace = TRUE), 
                            father = sample(agent$trait, N, replace = TRUE))
      
      prob <- runif(N)
      
      # if both parents have A, child has A
      agent$trait[parents$mother == "A" & parents$father == "A"] <- "A"
      
      # if both parents have B, child has B
      agent$trait[parents$mother == "B" & parents$father == "B"] <- "B"
      
      # if mother has A and father has B, child has A with prob (1/2 + s_v/2), otherwise B
      agent$trait[parents$mother == "A" & parents$father == "B" & 
                    prob < (1/2 + s_v/2)] <- "A"
      agent$trait[parents$mother == "A" & parents$father == "B" & 
                    prob >= (1/2 + s_v/2)] <- "B"
      
      # if mother has B and father has A, child has A with prob (1/2 + s_v/2), otherwise B
      agent$trait[parents$mother == "B" & parents$father == "A" & 
                    prob < (1/2 + s_v/2)] <- "A"
      agent$trait[parents$mother == "B" & parents$father == "A" & 
                    prob >= (1/2 + s_v/2)] <- "B"
      
      # get p and put it into output slot for this generation t and run r
      output[t,r] <- sum(agent$trait == "A") / N  
      
    }
    
  }
  
  # first plot a thick line for the mean p
 plot(rowMeans(output), 
       type = 'l', 
       ylab = "p, proportion of agents with trait A", 
       xlab = "generation", 
       ylim = c(0,1), 
       lwd = 3, 
       main = paste("N = ", N, ", s_v = ", s_v, sep = ""))
  
  for (r in 1:r_max) {  
    
    # add lines for each run, up to r_max
    lines(output[,r], type = 'l')  
    
  }
  
  output  # export data from function
}

## Run model

data_model6a <- VerticalTransmission(N = 10000, 
                                     p_0 = 0.1, 
                                     s_v = 0.1, 
                                     t_max = 150, 
                                     r_max = 5)



##  ASSORTATIVE MATING
VerticalAssortative <- function (N, p_0, s_v, a, t_max, r_max) {
  
  # create matrix with t_max rows and r_max columns, fill with NAs, convert to dataframe
  output <- as.data.frame(matrix(NA,t_max,r_max))  
  
  # purely cosmetic: rename the columns with run1, run2 etc.
  names(output) <- paste("run", 1:r_max, sep="")  
  
  for (r in 1:r_max) {
    
    # create first generation
    agent <- data.frame(trait = sample(c("A","B"), N, replace = TRUE, 
                                       prob = c(p_0,1-p_0)))  
    
    # add first generation's p to first row of column r
    output[1,r] <- sum(agent$trait == "A") / N  
    
    for (t in 2:t_max) {
      
      # 1. assortative mating: 
      
      # create dataframe with a set of 2 parents for each agent
      # mother is picked randomly, father is blank for now
      parents <- data.frame(mother = sample(agent$trait, N, replace = TRUE), 
                            father = rep(NA, N))
      
      # probabilities for a
      prob <- runif(N)
      
      # with prob a, make father identical
      parents$father[prob < a] <- parents$mother[prob < a]
      
      # with prob 1-a, pick random trait for father
      parents$father[prob >= a] <- sample(agent$trait, 
                                          sum(prob >= a), 
                                          replace = TRUE)
      
      # 2. vertical transmission:
      
      # new probabilities for s_v
      prob <- runif(N)
      
      # if both parents have A, child has A
      agent$trait[parents$mother == "A" & parents$father == "A"] <- "A"
      
      # if both parents have B, child has B
      agent$trait[parents$mother == "B" & parents$father == "B"] <- "B"
      
      # if mother has A and father has B, child has A with prob (1/2 + s_v/2), otherwise B
      agent$trait[parents$mother == "A" & parents$father == "B" & 
                    prob < (1/2 + s_v/2)] <- "A"
      agent$trait[parents$mother == "A" & parents$father == "B" & 
                    prob >= (1/2 + s_v/2)] <- "B"
      
      # if mother has B and father has A, child has A with prob (1/2 + s_v/2), otherwise B
      agent$trait[parents$mother == "B" & parents$father == "A" & 
                    prob < (1/2 + s_v/2)] <- "A"
      agent$trait[parents$mother == "B" & parents$father == "A" & 
                    prob >= (1/2 + s_v/2)] <- "B"
      
      # 3. store results:
      
      # get p and put it into output slot for this generation t and run r
      output[t,r] <- sum(agent$trait == "A") / N  
      
    }
    
  }
  
  # first plot a thick line for the mean p
  plot(rowMeans(output), 
       type = 'l', 
       ylab = "p, proportion of agents with trait A", 
       xlab = "generation", 
       ylim = c(0,1), 
       lwd = 3, 
       main = paste("N = ", N, ", s_v = ", s_v, ", a = ", a, sep = ""))
  
  for (r in 1:r_max) {  
    
    # add lines for each run, up to r_max
    lines(output[,r], type = 'l')  
    
  }
  
  output  # export data from function
}

# Run assortative
data_model6b <- VerticalAssortative(N = 10000, 
                                    p_0 = 0.01, 
                                    s_v = 0.1, 
                                    a = 0.5, 
                                    t_max = 150, 
                                    r_max = 5)


## Horizontal cultural transmission

VerticalHorizontal <- function (N, p_0, s_v, s_h, a, n, t_max, r_max, make_plot = TRUE) {
  
  # create matrix with t_max rows and r_max columns, fill with NAs, convert to dataframe
  output <- as.data.frame(matrix(NA,t_max,r_max))  
  
  # purely cosmetic: rename the columns with run1, run2 etc.
  names(output) <- paste("run", 1:r_max, sep="")  
  
  for (r in 1:r_max) {
    
    # create first generation
    agent <- data.frame(trait = sample(c("A","B"), N, replace = TRUE, 
                                       prob = c(p_0,1-p_0)))  
    
    # add first generation's p to first row of column r
    output[1,r] <- sum(agent$trait == "A") / N  
    
    for (t in 2:t_max) {
      
      # 1. assortative mating: 
      
      # create dataframe with a set of 2 parents for each agent
      # mother is picked randomly, father is blank for now
      parents <- data.frame(mother = sample(agent$trait, N, replace = TRUE), 
                            father = rep(NA, N))
      
      # probabilities for a
      prob <- runif(N)
      
      # with prob a, make father identical
      parents$father[prob < a] <- parents$mother[prob < a]
      
      # with prob 1-a, pick random trait for father
      parents$father[prob >= a] <- sample(agent$trait, 
                                          sum(prob >= a), 
                                          replace = TRUE)
      
      # 2. vertical transmission:
      
      # new probabilities for s_v
      prob <- runif(N)
      
      # if both parents have A, child has A
      agent$trait[parents$mother == "A" & parents$father == "A"] <- "A"
      
      # if both parents have B, child has B
      agent$trait[parents$mother == "B" & parents$father == "B"] <- "B"
      
      # if mother has A and father has B, child has A with prob (1/2 + s_v/2), otherwise B
      agent$trait[parents$mother == "A" & parents$father == "B" & 
                    prob < (1/2 + s_v/2)] <- "A"
      agent$trait[parents$mother == "A" & parents$father == "B" & 
                    prob >= (1/2 + s_v/2)] <- "B"
      
      # if mother has B and father has A, child has A with prob (1/2 + s_v/2), otherwise B
      agent$trait[parents$mother == "B" & parents$father == "A" & 
                    prob < (1/2 + s_v/2)] <- "A"
      agent$trait[parents$mother == "B" & parents$father == "A" & 
                    prob >= (1/2 + s_v/2)] <- "B"
      
      # 3. horizontal transmission:
      
      # create matrix for holding n demonstrators for N agents
      # fill with randomly selected agents from current gen
      demonstrators <- matrix(data = sample(agent$trait, N*n, replace = TRUE),
                              nrow = N, ncol = n)
      
      # record whether there is at least one A in each row
      oneA <- rowSums(demonstrators == "A") > 0
      
      # new probabilities for s_h
      prob <- runif(N)
      
      # adopt trait A if oneA is true and with prob s_h
      agent$trait[oneA & prob < s_h] <- "A"
      
      # 4. store results:
      
      # get p and put it into output slot for this generation t and run r
      output[t,r] <- sum(agent$trait == "A") / N  
      
    }
    
  }
  
  if (make_plot == TRUE) {
    
    # first plot a thick line for the mean p
    plot(rowMeans(output), 
         type = 'l', 
         ylab = "p, proportion of agents with trait A", 
         xlab = "generation", 
         ylim = c(0,1), 
         lwd = 3, 
         main = paste("N = ", N, ", s_v = ", s_v, ", s_h = ", s_h,
                      ", a = ", a, ", n = ", n, sep = ""))
    
    for (r in 1:r_max) {  
      
      # add lines for each run, up to r_max
      lines(output[,r], type = 'l')  
      
    }
    
  }
  
  output  # export data from function
}

model6c <- VerticalHorizontal(N = 10000, 
                              p_0 = 0.01, 
                              s_v = 0.1, 
                              s_h = 0, 
                              a = 0.4, 
                              n = 0, 
                              t_max = 150, 
                              r_max = 5)
