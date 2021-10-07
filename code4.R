# checked on october - 2021
#############################################################################################
################################### shannon index ###########################################
#This code calculates the shannon index of the simulations

rm(list = ls())
# 
### SHANNON INDEX 
library(vegan)
n.census = 7
n.fg = 3
by.guild = list()
shannon = list()
sims = 500

for (i in 1:sims){ 

  use2 <- list()
  use3 = list()
  
  input.path = paste0("simulation_output", "/sim_", i, ".rds")
  
  simulation = readRDS(input.path)
  

  for (k in 1:n.fg){
    
    use = list()
    shan = c()
    
    for (j in 1:n.census){
      
      use[[j]] = table(simulation[[j]][simulation[[j]]$fg==k,]$code)
      shan = c(shan,diversity(use[[j]],index = "shannon"))
      
    }

    use2[[k]] = use
    use3[[k]] = shan
    
  }
  by.guild[[i]]=use2 
  shannon[[i]] = use3
  
  
  print(i)
}

# output.path <- paste0("simulation_output", "/sim_", m, ".rds")
# saveRDS(by.guild, output.path)
saveRDS(shannon, "shannon.sims.rds")

### to calculate the median and C.I. of the simulations

p.low = round(1 + (sims/2)-((1.96*sqrt(sims))/(2)))
p.up = round((sims/2)+((1.96*sqrt(sims))/(2)))
medians_sh = list()
ups_sh = list()
downs_sh = list()

for (i in 1:n.fg){
  use2 = c()
  use3 = c()
  use4 = c()
  
  for (j in 1:n.census){
    
    use = c()  
    
    for (k in 1:sims){
      use = c(use,shannon[[k]][[i]][j])
    }
    use2 = c(use2,median(use))
    use3 = c(use3, sort(use)[p.up])
    use4 = c(use4, sort(use)[p.low])
  }
  medians_sh[[i]] = use2
  ups_sh[[i]] = use3
  downs_sh[[i]] = use4
  
  print(i)
}

## plot
par(mfrow=c(1,3))

plot(cex = 2, cex.axis = 1.5, cex.lab = 1.5,medians_sh[[1]],type="b",ylim = c(1.5326,1.536),main="Guild 1",xlab="Census",ylab="Shannon Index",pch=16, col = adjustcolor("deepskyblue4",alpha.f = 0.5))
polygon(c(1:7,rev(1:7)),c(downs_sh[[1]],rev(ups_sh[[1]])),col=adjustcolor("grey70", alpha.f = 0.3),border=F)
lines(medians_sh[[1]],type="b",pch=19, col = adjustcolor("deepskyblue4",alpha.f = 0.5), lwd = 2)

plot(cex = 2, cex.axis = 1.5, cex.lab = 1.5,medians_sh[[2]],type="b",ylim=c(3.492,3.495),main="Guild 2",xlab="Census",ylab="Shannon Index",pch=15, col = adjustcolor("darkgreen",alpha.f = 0.5))
polygon(c(1:7,rev(1:7)),c(downs_sh[[2]],rev(ups_sh[[2]])),col=adjustcolor("grey70", alpha.f = 0.3),border=F)
lines(medians_sh[[2]],type="b",pch=15, col = adjustcolor("darkgreen",alpha.f = 0.5),lwd = 2)

plot(cex = 2, cex.axis = 1.5, cex.lab = 1.5,medians_sh[[3]],type="b",ylim=c(3.437,3.4425),main="Guild 3",xlab="Census",ylab="Shannon Index",pch=17, col = adjustcolor("tan1",alpha.f = 0.5))
polygon(c(1:7,rev(1:7)),c(downs_sh[[3]],rev(ups_sh[[3]])),col=adjustcolor("grey70", alpha.f = 0.3),border=F)
lines(medians_sh[[3]],type="b",pch=17, col = adjustcolor("tan1",alpha.f = 0.5), lwd = 2)


