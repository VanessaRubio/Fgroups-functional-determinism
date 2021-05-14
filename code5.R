# checked on april 15 - 2021
#############################################################################################
################################### shannon index & rank empirical ##########################
#This code calculates the species rank distribution and shannon index of the empirical data
 
library(vegan)
n.census = 7
n.fg = 3

abundances = list()
richness = list()

for (j in 1:n.census){
  
  abundances[[j]] = list()
  richness[[j]] = list()
  
  data.cen = readRDS(paste0("census.data", "/cen.data", j, ".rds"))
  
  for (i in 1:n.fg){
    
    use = subset(data.cen,data.cen$fg == i)
    abundances[[j]][[i]] = sort(table(use$code),decr = T)
    richness[[j]][[i]] = length(unique(table(use$code)))
  }
  
  
}

#### code species rank distribution within guild
par(mfrow=c(1,3))
tmp1 = abundances
for (h in 1:n.fg){ 
  m.spp=mean(log(unlist(tmp1[[1]][[h]]))) 
  sd.spp=sd(log(unlist(tmp1[[1]][[h]]))) 
  R=length(unlist(tmp1[[1]][[h]]))
  ranks.lognormal = R * (1 - pnorm(log(unlist(tmp1[[1]][[h]])), m.spp, + sd.spp))
  if (h==1){
  plot(as.numeric(ranks.lognormal), as.numeric(log(unlist(tmp1[[1]][[h]]))), type = "l", ylim = c(0, 11), xlim=c(0,40), main = NULL,xlab = "Species Rank",ylab = "Log Density", cex.lab = 1.5, cex.axis = 1.2, lwd = 1.5, col = c("red"))
  title(paste("Guild",h), cex.main = 1.5)
  }else{
    plot(as.numeric(ranks.lognormal), as.numeric(log(unlist(tmp1[[1]][[h]]))), type = "l", ylim = c(0, 11), xlim=c(0,110), main = NULL,xlab = "Species Rank",ylab = "Log Density", cex.lab = 1.5, cex.axis = 1.2, lwd = 1.5, col = c("red"))
    title(paste("Guild",h), cex.main = 1.5)
  }
  
  
  for (m in 2:n.census){
    colors = c("red","blue","chartreuse4","coral3","goldenrod2")
    m2.spp=mean(log(unlist(tmp1[[m]][[h]]))) 
    sd2.spp=sd(log(unlist(tmp1[[m]][[h]]))) 
    R2=length(unlist(tmp1[[m]][[h]]))
    ranks.lognormal2 = R2 * (1 - pnorm(log(unlist(tmp1[[m]][[h]])), m2.spp, + sd2.spp))
    lines(as.numeric(ranks.lognormal2), as.numeric(log(unlist(tmp1[[m]][[h]]))),type="l", cex.lab = 1.5, cex.axis = 1.2, lwd = 1.5,col=c(colors[m]))
    # 5 x 15 inches
  }
}

#### Calculate Shannon
shannon = list()
for (i in 1:n.fg){
  
  use = c()
  shannon.index = c()
  shannon[[i]] = list()
  
  for (j in 1:n.census){
    data.cen = readRDS(paste0("census.data", "/cen.data", j, ".rds"))
    use = table(subset(data.cen, data.cen$fg == i)$code)
    shannon.index = c(shannon.index,diversity(use,index = "shannon"))
    
  }
  
  shannon[[i]] = shannon.index
}

# plots shannon
range(shannon)
plot(shannon[[1]],type = "b",ylim = c(1.4,3.6),ylab = "Shannon Index",xlab = "Census",pch = 19, cex.lab = 1, cex.axis = 1, cex = 1)
lines(shannon[[2]],type = "b",pch = 21, cex = 1)
lines(shannon[[3]],type = "b",pch = 6, cex = 1)

### shannon from the simulationsto graph together
all_sh = readRDS("shannon.sims.rds")
reps = 500
p.low = round(1 + (reps/2)-((1.96*sqrt(reps))/(2)))
p.up = round((reps/2)+((1.96*sqrt(reps))/(2)))
medians_sh = list()
ups_sh = list()
downs_sh = list()

n.fg = 3
for (i in 1:n.fg){
  use2 = c()
  use3 = c()
  use4 = c()
  
  for (j in 1:7){
    use = c()  
    for (k in 1:500){
      use = c(use,all_sh[[k]][[i]][j]) #all_sh[[k]][[i]][j], k=sims, i=fg, j=census
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

lw = 1
lines(medians_sh[[1]],type="l",ylim=c(0.315,0.3171),lty = 2, lwd = lw)
lines(medians_sh[[2]],lty = 2, lwd = lw)
lines(medians_sh[[3]],lty = 2, lwd = lw)
#5.5 x 6 inches
