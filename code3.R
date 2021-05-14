# checked on april 15 - 2021
###################################################################################################
################################### graphs neutral sims ###########################################
#This code graphs the results from code2.R
setwd("C:/Users/Libraries/Desktop/A.Codes for guilds 2021")

table3 = list() #
 
for (i in 1:500){ 
  
  table3[[i]] <- list()
  
  input.path = paste0("simulation_output", "/sim_", i, ".rds")
  
  simulation = readRDS(input.path)
  

  for (j in 1:7){
    
    table3[[i]][[j]] = table(simulation[[j]]$fg)

  }
  
  print(i)
}

#### calculate the f.g. percentages and confidence intervals

all = table3
n.fg = 3
reps = 500
n.cen = 7

cn = list()
for (i in 1:n.cen){ 
  
  use = c()
  
  for (j in 1:(reps)){
    
    use = c(use,all[[j]][i])
    
  }
  cn[[i]] = use
  
}

cnp = list() #census percentage
for (i in 1:n.cen){  
  use = list()
  
  for (j in 1:(reps)){
    
    use[[j]]= cn[[i]][[j]]/sum(cn[[i]][[j]])
    
  }
  
  cnp[[i]] = use
  
}

##

cns = list()
f.g = 3
for (i in 1:n.cen){
  fg = list()
  
  for (k in 1:f.g){
    
    use = c()
    
    for (j in 1:(reps)){
      
      use = c(use,cnp[[i]][[j]][k])
      
    }
    
    use = sort(use)
    fg[[k]] = use
    
  }
  
  cns[[i]] = fg
  
}

### plotting
#calcualte medians and C.I.
medians = list()
ups = list()
lows = list()
p.low = 1 + (reps/2)-((1.96*sqrt(reps))/(2))
p.up = (reps/2)+((1.96*sqrt(reps))/(2))

for (i in 1:f.g){
  
  use1 = c()
  use2 = c()
  use3 = c()
  
  for (j in 1:n.cen){
    
    use1 = c(use1, median(cns[[j]][[i]]))
    
    use2 = c(use2, cns[[j]][[i]][round(p.up)])
    
    use3 = c(use3, cns[[j]][[i]][round(p.low)])
    
  }
  
  medians[[i]] = use1 #medians per functional group
  ups[[i]] = use2
  lows[[i]] = use3
}

## plot of simulated proportions

par(mfrow=c(1,3))

plot(medians[[1]],type="b",ylim=c(0.0607,0.0612),main="Guild 1",xlab="Census",ylab="Proportion",pch=19)
polygon(c(1:7,rev(1:7)),c(lows[[1]],rev(ups[[1]])),col="grey70",border=F)
lines(medians[[1]],type="b",pch=19)

plot(medians[[2]],type="b",ylim=c(0.6731,0.6738),main="Guild 2",xlab="Census",ylab="Proportion",pch=21)
polygon(c(1:7,rev(1:7)),c(lows[[2]],rev(ups[[2]])),col="grey70",border=F)
lines(medians[[2]],type="b",pch=21)

plot(medians[[3]],type="b",ylim=c(0.265,0.266),main="Guild 3",xlab="Census",ylab="Proportion",pch=22)
polygon(c(1:7,rev(1:7)),c(lows[[3]],rev(ups[[3]])),col="grey70",border=F)
lines(medians[[3]],type="b",pch=6)

### add the medians to the bci empirical data in code1.R