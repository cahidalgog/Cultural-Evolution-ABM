### Chapter 11: https://bookdown.org/amesoudi/ABMtutorial_bookdown/model11.html#model11

# LIBRARIES AND LOAD DATA ----
# install.packages("pacman")
library(pacman)

## use function of pacman to install and load required libraries ##
p_load(here, ggraph, ggeasy, tidyverse, RColorBrewer, ggstatsplot, ggstatsplot, patchwork,
       gridExtra, esquisse, sjPlot)

## Set theme for plots ##
theme_set(theme_bw(base_family = 'serif'))


## CGS Model ##

Cooperation <- function(N = 128, 
                        n = 4, 
                        t_max = 2000, 
                        e = 0.02, 
                        c = 0.2, 
                        m = 0.01, 
                        p = 0.8, 
                        k = 0.2, 
                        mu = 0.01, 
                        epsilon = 0.015, 
                        show_plot = TRUE) {
  
  # create agent matrix, each column is a group
  # can be C (cooperator), D (defector) or P (punisher)
  agent <- matrix(nrow = n, ncol = N)
  
  # initial conditions: group 1 all punishers, others are all defectors
  agent[,1] <- "P"
  agent[,-1] <- "D"
  
  # create output for freq of cooperation in each timestep
  # and freq of punishment
  output <- data.frame(PandC = rep(NA, t_max), 
                       P = rep(NA, t_max))
  
  # store for t = 1
  output$PandC[1] <- sum(agent == "C" | agent == "P") / (N*n)
  output$P[1] <- sum(agent == "P") / (N*n)
  
  for (t in 2:t_max) {
    
    # create/initialise payoff matrix, with baseline payoff 1
    payoff <- matrix(1, nrow = n, ncol = N)
    
    # 1. Cooperation
    
    # probs for contribution (1-e)
    contribute <- matrix(runif(n*N), nrow = n, ncol = N)
    
    # contributors are Ps or Cs with prob 1-e
    contributors <- (agent == "P" | agent == "C") & contribute > e
    
    # reduce payoffs of contributing Ps and Cs by c
    payoff[contributors] <- payoff[contributors] - c
    
    # 2. Punishment
    
    # columns/groups with at least one P
    Pgroups <- unique(which(agent=="P", arr.ind=TRUE)[,"col"])
    
    # defections (Ds and Ps/Cs with probability e)
    defections <- agent == "D" | ((agent == "P" | agent == "C") & contribute <= e)
    
    # cycle thru Pgroups (j) and agents (i)
    for (j in Pgroups) {
      
      for (i in 1:n) {
        
        # if the agent is P
        if (agent[i,j] == "P") {
          
          # reduce punisher's payoff by k/n per defection
          payoff[i,j] <- payoff[i,j] - sum(defections[-i,j]) * k / n
          
          # reduce each defector's payoff by p/n
          payoff[-i,j][defections[-i,j]] <- payoff[-i,j][defections[-i,j]] - p / n
          
        }
        
      }
      
    }
    
    # 3. Social learning
    
    # store agent in previous agent to avoid overlap
    previous_agent <- agent
    
    # cycle thru groups (j) and agents (i)
    for (j in 1:N) {
      
      for (i in 1:n) {
        
        # with prob 1-m, choose demonstrator from same group (excluding self)
        if (runif(1) > m) {
          
          dem <- c(sample((1:n)[(1:n)!=i], 1), j)
          
          # with prob m, choose demonstrator from different group
        } else {
          
          dem <- c(sample(1:n, 1), 
                   sample((1:N)[(1:N)!=j], 1))
          
        }
        
        # get W, relative payoff
        W <- payoff[dem[1],dem[2]] / (payoff[dem[1],dem[2]] + payoff[i,j])
        
        # copy dem's behaviour with prob W
        # use previous_agent to avoid copying an agent who has already copied
        if (runif(1) < W) {
          
          agent[i,j] <- previous_agent[dem[1],dem[2]]
          
        }
        
      }
      
    }
    
    # 4. Group selection
    
    # dataframe of randomly selected pairs of groups
    contests <- as.data.frame(matrix(sample(N), nrow = N/2, ncol = 2))
    
    # keep contests with prob epsilon
    contests <- contests[runif(N/2) < epsilon,]
    
    # recalculate defections (Ds and Ps/Cs with probability e)
    defections <- agent == "D" | ((agent == "P" | agent == "C") & contribute <= e)
    
    # if there are any contests left
    if (nrow(contests) > 0) {  
      
      # cycle thru pairs
      for (i in 1:nrow(contests)) {
        
        # prob group 1 beats group 2 in pair i
        d1 <- sum(defections[,contests[i,1]]) / n
        d2 <- sum(defections[,contests[i,2]]) / n
        d <- 0.5 + (d2 - d1)/2
        
        # group 1 wins
        if (runif(1) < d) {
          
          agent[,contests[i,2]] <- agent[,contests[i,1]]
          
          # group 2 wins  
        } else {
          
          agent[,contests[i,1]] <- agent[,contests[i,2]]
          
        }
        
      }
      
    }
    
    # 5. Mutation
    
    # probs for mutation
    mutate <- runif(N*n)
    
    # store agent in previous agent to avoid overlap
    previous_agent <- agent
    
    # mutating D agents
    agent[mutate < mu & previous_agent == "D"] <- 
      sample(c("P","C"), 
             sum(mutate < mu & previous_agent == "D"), 
             replace = TRUE)
    
    # mutating C agents
    agent[mutate < mu & previous_agent == "C"] <- 
      sample(c("P","D"), 
             sum(mutate < mu & previous_agent == "C"), 
             replace = TRUE)
    
    # mutating P agents
    agent[mutate < mu & previous_agent == "P"] <- 
      sample(c("D","C"), 
             sum(mutate < mu & previous_agent == "P"), 
             replace = TRUE)
    
    # 6. Record freq of cooperation
    
    output$PandC[t] <- sum(agent == "C" | agent == "P") / (N*n)
    output$P[t] <- sum(agent == "P") / (N*n)
    
  }
  
  if (show_plot == TRUE) {
    
    plot(x = 1:nrow(output), 
         y = output$PandC, 
         type = 'l',
         ylab = "frequency of cooperation", 
         xlab = "generation", 
         ylim = c(0,1))
    
    # dotted line for freq of Ps
    lines(x = 1:nrow(output), 
          y = output$P,
          type = 'l',
          lty = 3)
    
  }
  
  # output final agent, full output, and mean cooperation of last 50% of timesteps
  list(agent = agent, 
       output = output, 
       mean_coop = mean(output$PandC[(t_max/2):t_max]))
  
}


system.time(data_model11 <- Cooperation())


n <- c(4,8,16,32,64)
epsilon <- c(0.0075,0.015,0.03)
output <- data.frame(matrix(NA, ncol = length(epsilon), nrow = length(n)))
rownames(output) <- n
colnames(output) <- epsilon

for (j in 1:length(epsilon)) {
  
  for (i in 1:length(n)) {
    
    output[i,j] <- Cooperation(n = n[i], 
                               epsilon = epsilon[j],
                               show_plot = FALSE)$mean_coop
    
  }
  
}

plot(x = 1:length(n), y = output[,1], 
     type = 'o',
     ylab = "frequency of cooperation", 
     xlab = "group size", 
     col = "darkblue",
     pch = 15,
     ylim = c(0,1),
     xlim = c(1,length(n)),
     xaxt = "n")

axis(1, at=1:length(n), labels=n)

lines(x = 1:length(n), output[,2],
      type = 'o',
      col = "red",
      pch = 17)

lines(x = 1:length(n), output[,3],
      type = 'o',
      col = "plum",
      pch = 16)

legend("bottomleft",
       legend = epsilon,
       title = "freq of group conflict",
       lty = 1,
       lwd = 2,
       pch = c(15,17,16),
       col = c("darkblue", "red","plum"),
       bty = "n",
       cex = 0.9)
ggsave("Cprueba.png", width = 10, height = 10)
