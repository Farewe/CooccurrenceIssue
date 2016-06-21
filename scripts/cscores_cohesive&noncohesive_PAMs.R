#### Boris' simulated data #### 

##re-analysis checking cohesive vs non-cohesive ranges PAMs
library(bipartite)
#loop over Boris's PAMs to calculate their C_scores individually
#Non-COHESIVE ranges
simulation <- c("100patches", "250patches", "500patches", "1000patches", "2500patches", "5000patches")

pam.coocc <- array(dim = c(length(simulation),
                           2,
                           2),
                   dimnames = list(simulation,
                                   c("cohesive", "uncohesive"),
                                   c("neutral", "temp.gradient")))
i <- 0
for(s in dimnames(pam.coocc)[[3]])
{
  for(sim in simulation)
  {
    i <- i + 1
    load(paste0("./outputs/", s, "_sim", sim, "_sp_patch_site_matrix"))
    pam <- speciespatch.sites.matrix
    pam <- pam[-which(colSums(pam) == 0), ]
    pam.coocc[sim, "uncohesive", s] <- C.score(pam)
    cat(paste0(round(i/prod(dim(pam.coocc)) * 100, 2), "% completed\n"))
    
    i <- i + 1
    load(paste0("./outputs/", s, "_sim", sim, "_sp_cohesive_site_matrix"))
    pam <- speciescohesive.sites.matrix
    pam <- pam[-which(colSums(pam) == 0), ]
    pam.coocc[sim, "cohesive", s] <- C.score(pam)
  }
}

source("removeCells&Spp.R")
boris.noncohpams <- list.files("uncohesive_R_files/")
setwd("C:/Fabricio/Hawkins-spurious_patterns/uncohesive_R_files")
boris.noncoh.pams.cscores <- matrix(NA,nrow=6,ncol=1)
rownames(boris.noncoh.pams.cscores) <- boris.noncohpams
colnames(boris.noncoh.pams.cscores) <- "C_score_normalised"

for (i in 1:length(boris.noncohpams)){
  print(i)
  load(boris.noncohpams[i])
  pam <- speciespatch.sites.matrix
  pam <- removeSp(pam)
  pam <- removeCells.2(pam)
  boris.noncoh.pams.cscores[i,] <- C.score(pam)
}

save.image("C:/Fabricio/Hawkins-spurious_patterns/noncohesive_PAMs_Cscores_.RData")


#COHESIVE ranges
rm(list=ls())
setwd("C:/Fabricio/Hawkins-spurious_patterns")
source("removeCells&Spp.R")
boris.cohevpams <- list.files("cohesive_R_files/")
setwd("C:/Fabricio/Hawkins-spurious_patterns/cohesive_R_files/")
boris.cohev.pams.cscores <- matrix(NA,nrow=6,ncol=1)
rownames(boris.cohev.pams.cscores) <- boris.cohevpams
colnames(boris.cohev.pams.cscores) <- "C_score_normalised"

for (i in 1:length(boris.cohevpams)){
  print(i)
  load(boris.cohevpams[i])
  pam <- speciescohesive.sites.matrix
  pam <- removeSp(pam)
  pam <- removeCells.2(pam)
  boris.cohev.pams.cscores[i,] <- C.score(pam)
}

save.image("C:/Fabricio/Hawkins-spurious_patterns/cohesive_PAMs_Cscores_.RData")