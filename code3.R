# checked on ocotber - 2021
###################################################################################################
################################### graphs neutral sims ###########################################
#This code graphs the results from code2.R
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

plot(cex.main = 2, cex = 2, cex.axis = 1.5, cex.lab = 1.5, medians[[1]],type="b",ylim=c(0.2715,0.2722),main="Guild 1",xlab="Census",ylab="Proportion",pch=16, col = adjustcolor("deepskyblue4",alpha.f = 0.5))
polygon(c(1:7,rev(1:7)),c(lows[[1]],rev(ups[[1]])),col=adjustcolor("grey70", alpha.f = 0.3),border=F)
lines(medians[[1]],type="b",pch=16, col = adjustcolor("deepskyblue4",alpha.f = 0.4), lwd = 2)

plot(cex.main = 2, cex = 2, cex.axis = 1.5, cex.lab = 1.5, medians[[2]],type="b",ylim=c(0.5737,0.5742),main="Guild 2",xlab="Census",ylab="Proportion",pch=15, col = adjustcolor("darkgreen",alpha.f = 0.5))
polygon(c(1:7,rev(1:7)),c(lows[[2]],rev(ups[[2]])),col=adjustcolor("grey70", alpha.f = 0.3),border=F)
lines(medians[[2]],type="b",pch=15, col = adjustcolor("darkgreen",alpha.f = 0.4),lwd = 2)

plot(cex.main = 2, cex = 2, cex.axis = 1.5, cex.lab = 1.5, medians[[3]],type="b",ylim=c(0.1541,0.1546),main="Guild 3",xlab="Census",ylab="Proportion",pch=17, col = adjustcolor("tan1",alpha.f = 0.5))
polygon(c(1:7,rev(1:7)),c(lows[[3]],rev(ups[[3]])),col=adjustcolor("grey70", alpha.f = 0.3),border=F)
lines(medians[[3]],type="b",pch=17, col = adjustcolor("tan1",alpha.f = 0.4), lwd = 2)

### add the medians to the bci empirical data in code1.R
