
### Chapter 5: https://bookdown.org/amesoudi/ABMtutorial_bookdown/model5.html

# LIBRARIES AND LOAD DATA ----
# install.packages("pacman")
library(pacman)

## use function of pacman to install and load required libraries ##
p_load(here, ggraph, ggeasy, tidyverse, RColorBrewer, ggstatsplot, ggstatsplot, patchwork,
       gridExtra, esquisse, sjPlot)

## Set theme for plots ##
theme_set(theme_bw(base_family = 'serif'))



## Model

ConformistTransmission <- function (N, p_0, D, t_max, r_max) {
  
  # create a matrix with t_max rows and r_max columns, fill with NAs, convert to dataframe
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
      
      # create dataframe with a set of 3 randomly-picked demonstrators for each agent
      demonstrators <- data.frame(dem1 = sample(agent$trait, N, replace = TRUE), 
                                  dem2 = sample(agent$trait, N, replace = TRUE), 
                                  dem3 = sample(agent$trait, N, replace = TRUE))
      
      # get the number of As in each 3-dem combo
      numAs <- rowSums(demonstrators == "A")
      
      agent$trait[numAs == 3] <- "A"  # for dem combos with all As, set to A
      agent$trait[numAs == 0] <- "B"  # for dem combos with no As, set to B
      
      prob <- runif(N)
      
      # when A is a majority, 2/3
      agent$trait[numAs == 2 & prob < (2/3 + D/3)] <- "A"
      agent$trait[numAs == 2 & prob >= (2/3 + D/3)] <- "B"
      
      # when A is a minority, 1/3
      agent$trait[numAs == 1 & prob < (1/3 - D/3)] <- "A"
      agent$trait[numAs == 1 & prob >= (1/3 - D/3)] <- "B"
      
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
       main = paste("N = ", N, ", D = ", D, ", p_0 = ", p_0, sep = ""))
  
  for (r in 1:r_max) {  
    
    # add lines for each run, up to r_max
    lines(output[,r], type = 'l')  
    
  }
  
  output  # export data from function
}


data_model5 <- ConformistTransmission(N = 1000, p_0 = 0.5, D = 1, t_max = 50, r_max = 10)
