# checked on april 15 - 2021
#############################################################################################
################################### species empirical #######################################
### this code graphs the empirical trends of each species in each F.G

rm(list = ls())

#bci census data
cen.data1 = readRDS(paste0("census.data", "/cen.data", 1, ".rds"))
cen.data2 = readRDS(paste0("census.data", "/cen.data", 2, ".rds"))
cen.data3 = readRDS(paste0("census.data", "/cen.data", 3, ".rds"))
cen.data4 = readRDS(paste0("census.data", "/cen.data", 4, ".rds"))
cen.data5 = readRDS(paste0("census.data", "/cen.data", 5, ".rds"))
cen.data6 = readRDS(paste0("census.data", "/cen.data", 6, ".rds"))
cen.data7 = readRDS(paste0("census.data", "/cen.data", 7, ".rds"))


#### Table with number of individuals per species per census

c1 = table(cen.data1$code)
c2 = table(cen.data2$code)
c3 = table(cen.data3$code)
c4 = table(cen.data4$code)
c5 = table(cen.data5$code)
c6 = table(cen.data6$code)
c7 = table(cen.data7$code)
list = list(c1,c2,c3,c4,c5,c6,c7)

new.table = data.frame(cbind(as.character(unique(cen.data1$code))))
new.table$c1 = rep(0, dim(new.table)[1])
new.table$c2 = rep(0, dim(new.table)[1])
new.table$c3 = rep(0, dim(new.table)[1])
new.table$c4 = rep(0, dim(new.table)[1])
new.table$c5 = rep(0, dim(new.table)[1])
new.table$c6 = rep(0, dim(new.table)[1])
new.table$c7 = rep(0, dim(new.table)[1])
new.table$fg = rep(0, dim(new.table)[1])

names = c("code","c1","c2","c3","c4","c5","c6","c7", "fg")
colnames(new.table) = names

for (i in 1:dim(new.table)[1]){
  
  use = as.character(new.table[i,1])
  
  for (j in 2:8){
    
    new.table[i,j] = as.numeric(list[[j-1]][use])
  }
}

load('bci.clust.Rdata') #clust is the file

funk = function(x){ #compare the cluster species to the data to assign FG
  
  sp=x #recorre los nombres de las especies
  positions=match(toString(sp),names(clust))  #compare with clust vector
  fg=clust[positions]  #find the FG number
  
  return(fg)
  
}

#asign the functional group to the data, applying the function funk
f.g=as.numeric(unlist(lapply(new.table$code, funk))) 
new.table$fg = f.g

### by functional guild
f1 = new.table[new.table$fg == 1,]
rownames(f1) = 1:dim(f1)[1]
f2 = new.table[new.table$fg == 2,]
rownames(f2) = 1:dim(f2)[1]
f3 = new.table[new.table$fg == 3,]
rownames(f3) = 1:dim(f3)[1]


par(mfrow = c(1,3))

plot(1:7, log(f1[1,2:8]), type = "l",ylim = c(0,11), xlab = "Census", ylab = "log(Individuals per species (FG 1))")
for (i in 2:nrow(f1)){
  
  lines(1:7, log(f1[i,2:8]), col = sample(rainbow(i)))
  
}

plot(1:7, log(f2[1,2:8]), type = "l",ylim = c(0,11), xlab = "Census", ylab = "log(Individuals per species (FG 2))")
for (i in 2:nrow(f2)){
  
  lines(1:7, log(f2[i,2:8]), col = sample(rainbow(i)))
  
}

plot(1:7, log(f3[1,2:8]), type = "l",ylim = c(0,11), xlab = "Census", ylab = "log(Individuals per species (FG 3))")
for (i in 2:nrow(f3)){
  
  lines(1:7, log(f3[i,2:8]), col = sample(rainbow(i)))
  
}

