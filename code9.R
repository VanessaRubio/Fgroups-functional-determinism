### this code looks at the individual 20x20 m plots for changes in relative proportions throught time
#by calculating the difference in the proportions in each quadrat per census 
#And also calculate trait densities

## plot spatial analysis


rm(list = ls())

#bci census data
cen.data1 = readRDS(paste0("census.data", "/cen.data", 1, ".rds"))
cen.data2 = readRDS(paste0("census.data", "/cen.data", 2, ".rds"))
cen.data3 = readRDS(paste0("census.data", "/cen.data", 3, ".rds"))
cen.data4 = readRDS(paste0("census.data", "/cen.data", 4, ".rds"))
cen.data5 = readRDS(paste0("census.data", "/cen.data", 5, ".rds"))
cen.data6 = readRDS(paste0("census.data", "/cen.data", 6, ".rds"))
cen.data7 = readRDS(paste0("census.data", "/cen.data", 7, ".rds"))


quadrat = sort(unique(cen.data1$quadrat))
all.census = list(cen.data1, cen.data2, cen.data3, cen.data4, cen.data5, cen.data6, cen.data7)

use2 = list()
for (i in 1:1250){
  
  use = quadrat[i]
  use.g1 = rep(0,7)
  use.g2 = rep(0,7)
  use.g3 = rep(0,7)
  
    for (j in 1:7){
    
    census = all.census[[j]]
    tmp = census[census$quadrat == use,]
    
    guild.count = table(tmp$fg)/sum(table(tmp$fg))  
    
    use.g1[j] = guild.count[1] 
    use.g2[j] = guild.count[2]
    use.g3[j] = guild.count[3]
    
  }
  
  use2[[i]] = list(use.g1,use.g2,use.g3)
  
  
  
}
 
### plot lines 

par(mfrow = c(1,3))
value = 0.1
colores = adjustcolor("grey70", alpha.f = value)
#guild 1

plot(c(1:7), use2[[1]][[1]],type= "l",col = colores, ylim = c(0,0.8))
for (i in 2:1250){
  
  lines(use2[[i]][[1]], col = colores)
  
}

#guild 2
plot(c(1:7), use2[[1]][[2]],type= "l",col = colores, ylim = c(0,0.8))
for (i in 2:1250){
  
  lines(use2[[i]][[2]], col = colores)
  
}

#guild 3
plot(c(1:7), use2[[1]][[3]],type= "l",col = colores, ylim = c(0,0.8))
for (i in 2:1250){
  
  lines(use2[[i]][[3]], col = colores)
  
}

### histograms plots with quantile 25% and 75%

c1.1 = c()
c2.1 = c()
c1.2 = c()
c2.2 = c()
c1.3 = c()
c2.3 = c()

for (i in 1:1250){
  #guild1
  c1.1[i] = unlist(use2[[i]][1])[1] #census 1, guild 1
  c2.1[i] = unlist(use2[[i]][1])[2] #census 2, guild 1
  c1.2[i] = unlist(use2[[i]][2])[1] #census 1, guild 2
  c2.2[i] = unlist(use2[[i]][2])[2] #census 2, guild 2
  c1.3[i] = unlist(use2[[i]][3])[1]
  c2.3[i] = unlist(use2[[i]][3])[2]
}

par(mfrow = c(1,3))
#census1 to census 2
#guild 1
x = c1.1 - c2.1
h = hist(x,main = "FG 1: Census 1 to Census 2", breaks=20, xlab = "Change (relative proportion)", border = "lightgray", xlim = c(-0.8,0.60)) 
abline(v = quantile(x)[2], col = "darkgreen",lwd = 2)
abline(v = quantile(x)[4], col = "blue",lwd = 2)
#guild2
x = c1.2 - c2.2
h = hist(x,main = "FG 2: Census 1 to Census 2", breaks=20, xlab = "Change (relative proportion)", border = "lightgray", xlim = c(-0.8,0.60)) 
abline(v = quantile(x)[2], col = "darkgreen",lwd = 2)
abline(v = quantile(x)[4], col = "blue",lwd = 2)
#guild3
x = na.omit(c1.3 - c2.3)
h = hist(x,main = "FG 3: Census 1 to Census 2", breaks=20, xlab = "Change (relative proportion)", border = "lightgray", xlim = c(-0.8,0.60)) 
abline(v = quantile(x)[2], col = "darkgreen",lwd = 2)
abline(v = quantile(x)[4], col = "blue",lwd = 2)

#### census 2 to census 3

c2.1 = c()
c3.1 = c()
c2.2 = c()
c3.2 = c()
c2.3 = c()
c3.3 = c()

for (i in 1:1250){
  #guild1
  c2.1[i] = unlist(use2[[i]][1])[2] #census 1, guild 1
  c3.1[i] = unlist(use2[[i]][1])[3] #census 2, guild 1
  c2.2[i] = unlist(use2[[i]][2])[2] #census 1, guild 2
  c3.2[i] = unlist(use2[[i]][2])[3] #census 2, guild 2
  c2.3[i] = unlist(use2[[i]][3])[2] #census 1, guild 3
  c3.3[i] = unlist(use2[[i]][3])[3] #census 2, guild 3
}

par(mfrow = c(1,3))
#census1 to census 2
#guild 1
x = c2.1 - c3.1
h = hist(x,main = "FG 1: Census 2 to Census 3", breaks=20, xlab = "Change (relative proportion)", border = "lightgray", xlim = c(-0.8,0.60)) 
abline(v = quantile(x)[2], col = "darkgreen",lwd = 2)
abline(v = quantile(x)[4], col = "blue",lwd = 2)
#guild2
x = c2.2 - c3.2
h = hist(x,main = "FG 2: Census 2 to Census 3", breaks=20, xlab = "Change (relative proportion)", border = "lightgray", xlim = c(-0.8,0.60)) 
abline(v = quantile(x)[2], col = "darkgreen",lwd = 2)
abline(v = quantile(x)[4], col = "blue",lwd = 2)
#guild3
x = na.omit(c2.3 - c3.3)
h = hist(x,main = "FG 3: Census 2 to Census 3", breaks=20, xlab = "Change (relative proportion)", border = "lightgray", xlim = c(-0.8,0.60)) 
abline(v = quantile(x)[2], col = "darkgreen",lwd = 2)
abline(v = quantile(x)[4], col = "blue",lwd = 2)
## 5x13
#### census 3 to census 4

c3.1 = c()
c4.1 = c()
c3.2 = c()
c4.2 = c()
c3.3 = c()
c4.3 = c()

for (i in 1:1250){
  #guild1
  c3.1[i] = unlist(use2[[i]][1])[3] #census 3, guild 1
  c4.1[i] = unlist(use2[[i]][1])[4] #census 4, guild 1
  c3.2[i] = unlist(use2[[i]][2])[3] #census 3, guild 2
  c4.2[i] = unlist(use2[[i]][2])[4] #census 4, guild 2
  c3.3[i] = unlist(use2[[i]][3])[3] #census 3, guild 3
  c4.3[i] = unlist(use2[[i]][3])[4] #census 4, guild 3
}

par(mfrow = c(1,3))
#census1 to census 2
#guild 1
x = c3.1 - c4.1
h = hist(x,main = "FG 1: Census 3 to Census 4", breaks=20, xlab = "Change (relative proportion)", border = "lightgray", xlim = c(-0.8,0.60)) 
abline(v = quantile(x)[2], col = "darkgreen",lwd = 2)
abline(v = quantile(x)[4], col = "blue",lwd = 2)
#guild2
x = c3.2 - c4.2
h = hist(x,main = "FG 2: Census 3 to Census 4", breaks=20, xlab = "Change (relative proportion)", border = "lightgray", xlim = c(-0.8,0.60)) 
abline(v = quantile(x)[2], col = "darkgreen",lwd = 2)
abline(v = quantile(x)[4], col = "blue",lwd = 2)
#guild3
x = na.omit(c3.3 - c4.3)
h = hist(x,main = "FG 3: Census 3 to Census 4", breaks=20, xlab = "Change (relative proportion)", border = "lightgray", xlim = c(-0.8,0.60)) 
abline(v = quantile(x)[2], col = "darkgreen",lwd = 2)
abline(v = quantile(x)[4], col = "blue",lwd = 2)

#### census 4 to census 5

c4.1 = c()
c5.1 = c()
c4.2 = c()
c5.2 = c()
c4.3 = c()
c5.3 = c()

for (i in 1:1250){
  #guild1
  c4.1[i] = unlist(use2[[i]][1])[4] #census 4, guild 1
  c5.1[i] = unlist(use2[[i]][1])[5] #census 5, guild 1
  c4.2[i] = unlist(use2[[i]][2])[4] #census 4, guild 2
  c5.2[i] = unlist(use2[[i]][2])[5] #census 5, guild 2
  c4.3[i] = unlist(use2[[i]][3])[4] #census 4, guild 3
  c5.3[i] = unlist(use2[[i]][3])[5] #census 5, guild 3
}

par(mfrow = c(1,3))
#census1 to census 2
#guild 1
x = c4.1 - c5.1
h = hist(x,main = "FG 1: Census 4 to Census 5", breaks=20, xlab = "Change (relative proportion)", border = "lightgray", xlim = c(-0.8,0.60)) 
abline(v = quantile(x)[2], col = "darkgreen",lwd = 2)
abline(v = quantile(x)[4], col = "blue",lwd = 2)
#guild2
x = c4.2 - c5.2
h = hist(x,main = "FG 2: Census 4 to Census 5", breaks=20, xlab = "Change (relative proportion)", border = "lightgray", xlim = c(-0.8,0.60)) 
abline(v = quantile(x)[2], col = "darkgreen",lwd = 2)
abline(v = quantile(x)[4], col = "blue",lwd = 2)
#guild3
x = na.omit(c4.3 - c5.3)
h = hist(x,main = "FG 3: Census 4 to Census 5", breaks=20, xlab = "Change (relative proportion)", border = "lightgray", xlim = c(-0.8,0.60)) 
abline(v = quantile(x)[2], col = "darkgreen",lwd = 2)
abline(v = quantile(x)[4], col = "blue",lwd = 2)


#### census 5 to census 6

c5.1 = c()
c6.1 = c()
c5.2 = c()
c6.2 = c()
c5.3 = c()
c6.3 = c()

for (i in 1:1250){
  #guild1
  c5.1[i] = unlist(use2[[i]][1])[5] #census 5, guild 1
  c6.1[i] = unlist(use2[[i]][1])[6] #census 6, guild 1
  c5.2[i] = unlist(use2[[i]][2])[5] #census 5, guild 2
  c6.2[i] = unlist(use2[[i]][2])[6] #census 6, guild 2
  c5.3[i] = unlist(use2[[i]][3])[5] #census 5, guild 3
  c6.3[i] = unlist(use2[[i]][3])[6] #census 6, guild 3
}

par(mfrow = c(1,3))
#census1 to census 2
#guild 1
x = c5.1 - c6.1
h = hist(x,main = "FG 1: Census 5 to Census 6", breaks=20, xlab = "Change (relative proportion)", border = "lightgray", xlim = c(-0.8,0.60)) 
abline(v = quantile(x)[2], col = "darkgreen",lwd = 2)
abline(v = quantile(x)[4], col = "blue",lwd = 2)
#guild2
x = c5.2 - c6.2
h = hist(x,main = "FG 2: Census 5 to Census 6", breaks=20, xlab = "Change (relative proportion)", border = "lightgray", xlim = c(-0.8,0.60)) 
abline(v = quantile(x)[2], col = "darkgreen",lwd = 2)
abline(v = quantile(x)[4], col = "blue",lwd = 2)
#guild3
x = na.omit(c5.3 - c6.3)
h = hist(x,main = "FG 3: Census 5 to Census 6", breaks=20, xlab = "Change (relative proportion)", border = "lightgray", xlim = c(-0.8,0.60)) 
abline(v = quantile(x)[2], col = "darkgreen",lwd = 2)
abline(v = quantile(x)[4], col = "blue",lwd = 2)

#### census 6 to census 7

c6.1 = c()
c7.1 = c()
c6.2 = c()
c7.2 = c()
c6.3 = c()
c7.3 = c()

for (i in 1:1250){
  #guild1
  c6.1[i] = unlist(use2[[i]][1])[6] #census 6, guild 1
  c7.1[i] = unlist(use2[[i]][1])[7] #census 7, guild 1
  c6.2[i] = unlist(use2[[i]][2])[6] #census 6, guild 2
  c7.2[i] = unlist(use2[[i]][2])[7] #census 7, guild 2
  c6.3[i] = unlist(use2[[i]][3])[6] #census 6, guild 3
  c7.3[i] = unlist(use2[[i]][3])[7] #census 7, guild 3
}

par(mfrow = c(1,3))
#census1 to census 2
#guild 1
x = c6.1 - c7.1
h = hist(x,main = "FG 1: Census 6 to Census 7", breaks=20, xlab = "Change (relative proportion)", border = "lightgray", xlim = c(-0.8,0.60)) 
abline(v = quantile(x)[2], col = "darkgreen",lwd = 2)
abline(v = quantile(x)[4], col = "blue",lwd = 2)
#guild2
x = c6.2 - c7.2
h = hist(x,main = "FG 2: Census 6 to Census 7", breaks=20, xlab = "Change (relative proportion)", border = "lightgray", xlim = c(-0.8,0.60)) 
abline(v = quantile(x)[2], col = "darkgreen",lwd = 2)
abline(v = quantile(x)[4], col = "blue",lwd = 2)
#guild3
x = na.omit(c6.3 - c7.3)
h = hist(x,main = "FG 3: Census 6 to Census 7", breaks=20, xlab = "Change (relative proportion)", border = "lightgray", xlim = c(-0.8,0.60))  
abline(v = quantile(x)[2], col = "darkgreen",lwd = 2)
abline(v = quantile(x)[4], col = "blue",lwd = 2)


####################################################################################################
####################################################################################################
####################################################################################################
############### plot of densities for traits in each guild
rm(list = ls())

census1=read.table("MasterBCI_C1.txt",h=T)
census1$fg=rep(0,nrow(census1))

traits2=read.table("traits.txt",h=T) #307 species
traits2$hmax=log10(traits2$hmax)
traits2$lma3=log10(traits2$lma3)
traits2$la3=log10(traits2$la3)
head(traits2)

load('bci.clust.aug.Rdata') #cluster file
funk = function(x){ #compare the cluster species to the data to assign FG
  
  sp=x
  positions=match(toString(sp),names(clust))  
  fg=clust[positions] 
  
  return(fg)
  
}
census1$fg = as.numeric(unlist(lapply(census1$code, funk))) 

funk2 = function(x){ 
  
  sp=x
  positions=match(toString(sp),traits2$code)  
  la=traits2$la3[positions] 
  
  return(la)
  
}
census1$la=as.numeric(unlist(lapply(census1$code, funk2))) 

funk3 = function(x){ #compare the cluster species to the data to assign FG
  
  sp=x
  positions=match(toString(sp),traits2$code)  
  lma=traits2$lma3[positions] 
  
  return(lma)
  
}
census1$lma=as.numeric(unlist(lapply(census1$code, funk3))) 

funk4 = function(x){ #compare the cluster species to the data to assign FG
  
  sp=x
  positions=match(toString(sp),traits2$code)  
  hmax=traits2$hmax[positions] 
  
  return(hmax)
  
}
census1$hmax=as.numeric(unlist(lapply(census1$code, funk4))) 

funk5 = function(x){ #compare the cluster species to the data to assign FG
  
  sp=x
  positions=match(toString(sp),traits2$code)  
  wsg=traits2$wsg[positions] 
  
  return(wsg)
  
}
census1$wsg=as.numeric(unlist(lapply(census1$code, funk5))) 

head(census1)
data = census1

data = data[!is.na(data$fg),] #remove the rows that do not have a functional g assigned
table(data$code)

#guild 1 : 3
g1 = data[data$fg == 1,]
g2 = data[data$fg == 2,]
g3 = data[data$fg == 3,]

##get the trait data
funk = function(x){ 
  
  sp=x
  positions=match(toString(sp),data$code)  
  traits=data[positions,11:14] 
  
  return(traits)
  
}

##data.frames per guild
df1 = data.frame("code" = unique(g1$code))
tr = do.call("rbind",lapply(df1$code, funk))
df1 = cbind(df1,tr) #binds census data with the variables data

df2 = data.frame("code" = unique(g2$code))
tr = do.call("rbind",lapply(df2$code, funk))
df2 = cbind(df2,tr) #binds census data with the variables data

df3 = data.frame("code" = unique(g3$code))
tr = do.call("rbind",lapply(df3$code, funk))
df3 = cbind(df3,tr) #binds census data with the variables data

data.new = data.frame("code" = unique(data$code)) ####data complete
tr = do.call("rbind",lapply(data.new$code, funk))
data.new = cbind(data.new,tr)

funk = function(x){ #compare the cluster species to the data to assign FG
  
  sp=x
  positions=match(toString(sp),names(clust))  
  fg=clust[positions] 
  
  return(fg)
  
}
data.new$fg = as.numeric(unlist(lapply(data.new$code, funk))) 

#####
### kernel plots
library("sm")
par(mfrow = c(2,2))
sm.density.compare(data.new$la, data.new$fg, xlab="log10(leaf area)", col = adjustcolor(colores, alpha.f = 0.4), lwd = 3, lty = c(1,1,1))
sm.density.compare(data.new$lma, data.new$fg, xlab="log10(leaf mass per area)", col = adjustcolor(colores, alpha.f = 0.4), lwd = 3, lty = c(1,1,1))
sm.density.compare(data.new$wsg, data.new$fg, xlab="Wood density (g/cm3)", col = adjustcolor(colores, alpha.f = 0.4), lwd = 3, lty = c(1,1,1))
sm.density.compare(data.new$hmax, data.new$fg, xlab="log10(maximum height)", col = adjustcolor(colores, alpha.f = 0.4), lwd = 3, lty = c(1,1,1))
