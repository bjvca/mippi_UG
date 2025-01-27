#run in /home/bjvca/data/projects/OneCG/MIPP/study_design/
library(simstudy)

# specify between site variation



path <- getwd()

library(plotly)

library(lmtest)
library(car)
library(clubSandwich)
set.seed(1492023) #set seed to date
(setVar <- iccRE(ICC = 0.10, dist = "binary"))
#run in /home/bjvca/data/projects/OneCG/MIPP/study_design/
N_villages <- seq(from=150, to=250, by=5)     # The sample sizes we'll be considering
n_farmers_per_village <- seq(from=10, to=10, by=10)
power <- matrix(NA, length(N_villages),length(n_farmers_per_village))           # Empty object to collect simulation estimates
power_H1 <- matrix(NA, length(N_villages),length(n_farmers_per_village))           # Empty object to collect simulation estimates
power_H2 <- matrix(NA, length(N_villages),length(n_farmers_per_village))           # Empty object to collect simulation estimates
power_H3 <- matrix(NA, length(N_villages),length(n_farmers_per_village))           # Empty object to collect simulation estimates
power_one <- matrix(NA, length(N_villages),length(n_farmers_per_village))           # Empty object to collect simulation estimates

alpha <- 0.05                                    # Standard significance level
sims <- 1000                                    # Number of simulations to conduct for each N

#### Outer loop to vary the number of subjects ####
for (j in 1:length(N_villages)){
  for (k in 1:length(n_farmers_per_village)){
    N <- N_villages[j]                              # Pick the jth value for N
    N1 <- n_farmers_per_village[k]
    
    significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
    significant.experiments_H1 <- rep(NA, sims)         # Empty object to count significant experiments
    significant.experiments_H2 <- rep(NA, sims)         # Empty object to count significant experiments
    significant.experiments_H3 <- rep(NA, sims)         # Empty object to count significant experiments
    significant.experiments_one <- rep(NA, sims)         # Empty object to count significant experiments
    store_coefs <- matrix(NA,sims,4) 
    
    #### Inner loop to conduct experiments "sims" times over for each N ####
    for (i in 1:sims){
   
      d <- defData(varname = "a", formula = 0, variance = 0.580565, id = "grp")
      d <- defData(d, varname = "size", formula = N1, dist = "nonrandom")
      
      a <- defDataAdd(varname = "y1", formula = "a", 
                      variance = 4, dist = "normal")
      
      
      dT <- genData(N, d)
      
      dta <- genCluster(dtClust = dT, cLevelVar = "grp", 
                        numIndsVar = "size", level1ID = "id")
      dta <- addColumns(a, dta)
   dta$y1 <- dta$y1-1
      dta$y2 <- dta$y1+.75
      dta$y3 <- dta$y1+.75
      dta$y4 <- dta$y1+(3*1)
      
      dta$y1 <- dta$y1 > 0
      dta$y2 <- dta$y2 > 0
      dta$y3 <- dta$y3 > 0
      dta$y4 <- dta$y4 > 0
      
      #put half of the groups in a treatment group
      dta$treat_seed <- 0
      dta$treat_cons <- 0
      all <- names(table(dta$grp))
      sampl_1 <- sample(names(table(dta$grp)), size = N/4)
      sampl_2 <-  sample(setdiff(all, sampl_1), size = N/4)
      sampl_3 <-  sample(setdiff(setdiff(all, sampl_1),sampl_2), size = N/4)
      dta$treat_seed[dta$grp %in% c(sampl_1,sampl_3)] <- 1
      dta$treat_cons[dta$grp %in% c(sampl_2,sampl_3)] <- 1
      
    
      dta$y <- dta$y2*(dta$treat_seed==1 & dta$treat_cons==0)+ dta$y3*(dta$treat_cons==1 & dta$treat_seed==0)+ (dta$y4)*(dta$treat_seed==1 & dta$treat_cons==1) + dta$y1*(dta$treat_seed==0 & dta$treat_cons==0)
      
      fit.sim <- lm(y~treat_seed*treat_cons,data=dta)
      summary(fit.sim)
      vcov_cluster <- vcovCR(fit.sim,cluster=dta$grp,type="CR3")
      coef_test( fit.sim , vcov_cluster)$p_Satt
      store_coefs[i,] <-  coef(fit.sim)
      p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
      p.value_1<-  coef_test( fit.sim , vcov_cluster)$p_Satt[2]  # Extract p-values
      p.value_2<-  coef_test( fit.sim , vcov_cluster)$p_Satt[3]  # Extract p-values
      p.value_3<-  coef_test( fit.sim , vcov_cluster)$p_Satt[4]  # Extract p-values
      significant.experiments[i] <- (p.value_1 < alpha) & (p.value_2 < alpha) & (p.value_3 < alpha)
      significant.experiments_H1[i] <- (p.value_1 < alpha)
      significant.experiments_H2[i] <- (p.value_2 < alpha)
      significant.experiments_H3[i] <- (p.value_3 < alpha)
      significant.experiments_one[i] <- (p.value_1 < alpha) | (p.value_2 < alpha) | (p.value_3 < alpha)
  
    }
    
    power[j,k] <- mean(significant.experiments)
    power_H1[j,k] <- mean(significant.experiments_H1)    
    power_H2[j,k] <- mean(significant.experiments_H2)    
    power_H3[j,k] <- mean(significant.experiments_H3)   
    power_one[j,k] <- mean(significant.experiments_one)  
    
  }
  # store average success rate (power) for each N
}

fig <- plot_ly(x=~n_farmers_per_village,y=~N_villages,z = ~power)

fig <- fig %>% add_surface(
  
  contours = list(
    
    z = list(
      
      show=TRUE,
      
      usecolormap=TRUE,
      
      highlightcolor="#ff0000",
      
      project=list(z=TRUE)
      
    )
    
  )
  
)

fig <- fig %>% layout(
  
  scene = list(
    
    camera=list(
      
      eye = list(x=1.87, y=0.88, z=-0.64)
      
    )
    
  )
  
)
