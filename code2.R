# checked on april 15 - 2021
###################################################################################################
################################### NEUTRAL SIMULATIONS ###########################################
#This code simulates neutral dynamics on the bci data
#uses the 7 census' exact number of dead and new recruits to add "new individuals" each time
#kills the individuals before calculating the abundance distribution of the plot
#uses the case1 of neutrality (no CNDD and replaced by whatever species based on relative abundances). 
#data used: MasterBCI_C1.txt , bci.clust.2020.Rdata , counts.Rdata (empirical deaths and new recruits per census)

rm(list = ls())

#load bci census data: cleaned census 1 from code 1
cen.data1 = readRDS(paste0("census.data", "/cen.data", 1, ".rds"))

##########################
######initialize variables: #CASE 1 - Pure stochasticity (234 species)
##########################

census1 = cen.data1
colnames(census1)[colnames(census1) == "X1"] = "hold"
load('bci.clust.aug.Rdata') #cluster file

#deads and new individuals each census
counts = readRDS("counts.rds") #deads and new

#starts with census1
census1$id = NULL
census1$dbh = NULL
head(census1)
temp=census1[,2:4] #quad, code, hold

#simulation for 7 time points (7 census) 500 times/replicates
dir.create("simulation_output")

set.seed(1234)

for (m in 1:500){
  print(paste("sim",m,sep=""))
  print(date())
  
  census=temp
  new.census1=list()
  new.census1[[1]]=census
  
  for (i in 1:6){ 
    
     death.indiv = counts[(i*2)-1]
     new.indiv = counts[(i*2)]
     
     dead.rows = sample(1:dim(census)[1], death.indiv, replace=F) 
     remove.kill = census[-c(dead.rows),]
     abund = tapply(remove.kill$hold, remove.kill$code, sum)/nrow(remove.kill)
     abund= abund[!is.na(abund)]

     new.spp=c()
     new.spp = names(sample(abund,new.indiv,replace=T,prob=abund))
     to.born=data.frame(rep(1,length(new.spp)), new.spp, rep(1,length(new.spp)))
     colnames(to.born)=c("quadrat","code","hold")
    
     census = census[-c(dead.rows),]
     census=rbind(census,to.born)
    
     rownames(census)=1:nrow(census) 
    
     new.census1[[i+1]]=census
     }
  
  for(j in seq_along(new.census1)){
    new.census1[[j]]$fg <- clust[as.character(new.census1[[j]]$code)]
  }
  
  output.path <- paste0("simulation_output", "/sim_", m, ".rds")
  saveRDS(new.census1, output.path)
}
