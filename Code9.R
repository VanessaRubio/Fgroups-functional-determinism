################
########## using different sizes and spatial scale to test for functional group proportions, supplementary figures
######### and count od dead and new individuals per census

##### SIZES
#bci census data created in code1.R

cen.data1 = readRDS(paste0("census.data", "/cen.data", 1, ".rds"))
cen.data2 = readRDS(paste0("census.data", "/cen.data", 2, ".rds"))
cen.data3 = readRDS(paste0("census.data", "/cen.data", 3, ".rds"))
cen.data4 = readRDS(paste0("census.data", "/cen.data", 4, ".rds"))
cen.data5 = readRDS(paste0("census.data", "/cen.data", 5, ".rds"))
cen.data6 = readRDS(paste0("census.data", "/cen.data", 6, ".rds"))
cen.data7 = readRDS(paste0("census.data", "/cen.data", 7, ".rds"))
census.total = list(cen.data1,cen.data2,cen.data3,cen.data4,cen.data5,cen.data6,cen.data7)

###create different lists for different sizes: 10-49mm; 50-99mm; >=100mm
####

##trees with size 10-49mm

cen.emp = list() #the empirical data
cen.emp[[1]] = cen.data1[cen.data1$dbh<=49,] #184649/227566 = 0.8114086 
cen.emp[[2]] = cen.data2[cen.data2$dbh<=49,] #189679/234436 = 0.8090865
cen.emp[[3]] = cen.data3[cen.data3$dbh<=49,] #186325/232874 = 0.8001108
cen.emp[[4]] = cen.data4[cen.data4$dbh<=49,] #169748/216527 = 0.7839577
cen.emp[[5]] = cen.data5[cen.data5$dbh<=49,] #158496/204348 = 0.7756181
cen.emp[[6]] = cen.data6[cen.data6$dbh<=49,] #153010/198383 = 0.7712858
cen.emp[[7]] = cen.data7[cen.data7$dbh<=49,] #151043/196732 = 0.7677602

### Counting

g.all.emp=list()
for (i in 1:length(cen.emp)){
  g1 = 0
  g1 = table(cen.emp[[i]]$fg)
  g.all.emp[[i]] = g1
  print(i)
}

###

n.fg = 3
fun = function(x){x/sum(x)} #to calculate the percentages

g = g.all.emp #censuses
g = lapply(g,fun) #calculates the percentages of each FG

cn1 = list() ### cn1[[1]]=census1 [[1]]=fg1 (total reps)


for (i in 1:n.fg){
  
  use1 = c()
  
  for (j in 1:length(g)){
    
    use1 = c(use1,g[[j]][i])
  }
  cn1[[i]] = use1
}


### plot of empirical data for functional guilds proportions
# 
plot(main="Trees with 10<=DBH<=49mm",cn1[[1]],type="b",yaxt="n",xlab="Census",ylab="Proportion",ylim=c(0,0.72),pch = 16,cex = 1, cex.axis = 1, cex.lab = 1, col = adjustcolor("deepskyblue4",alpha.f = 0.5))
for (h in 2:n.fg){
  p = c(16,15,17)
  colores = c("darkgreen", "tan1")
  lines(cn1[[h]],lwd=1,pch=p[h],type="b", cex = 1, col = adjustcolor(colores[h-1],alpha.f = 0.5))
}
axis(2, at=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7), labels=c("0.1","0.2","0.3","0.4","0.5","0.6","0.7"),tick=T, cex.axis = 1)


##trees with size 49-99mm
rm(cen.emp)
cen.emp = list() #the empirical data
cen.emp[[1]] = cen.data1[cen.data1$dbh>49 & cen.data1$dbh<=99,] #24472/227566 = 0.107538 
cen.emp[[2]] = cen.data2[cen.data2$dbh>49 & cen.data2$dbh<=99,] #26457/234436 = 0.1128538
cen.emp[[3]] = cen.data3[cen.data3$dbh>49 & cen.data3$dbh<=99,] #27723/232874 = 0.1190472
cen.emp[[4]] = cen.data4[cen.data4$dbh>49 & cen.data4$dbh<=99,] #27646/216527 = 0.1276792
cen.emp[[5]] = cen.data5[cen.data5$dbh>49 & cen.data5$dbh<=99,] #26902/204348 = 0.131648
cen.emp[[6]] = cen.data6[cen.data6$dbh>49 & cen.data6$dbh<=99,] #26760/198383 = 0.1348906
cen.emp[[7]] = cen.data7[cen.data7$dbh>49 & cen.data7$dbh<=99,] #27162/196732 = 0.138066

### Counting

g.all.emp=list()
for (i in 1:length(cen.emp)){
  g1 = 0
  g1 = table(cen.emp[[i]]$fg)
  g.all.emp[[i]] = g1
  print(i)
}

###

n.fg = 3
#g.all is a list of each census, [[1]]is the simulation; each one has the total number of FGs as column heads and the total number of individuals

fun = function(x){x/sum(x)} #to calculate the percentages

g = g.all.emp #censuses
g = lapply(g,fun) #calculates the percentages of each FG

cn1 = list() ### cn1[[1]]=census1 [[1]]=fg1 (total reps)


for (i in 1:n.fg){
  
  use1 = c()
  
  for (j in 1:length(g)){
    
    use1 = c(use1,g[[j]][i])
  }
  cn1[[i]] = use1
}


### plot of empirical data for functional guilds proportions
# 
plot(main="Trees with 49mm<DBH<=99mm",cn1[[1]],type="b",yaxt="n",xlab="Census",ylab="Proportion",ylim=c(0,0.78),pch = 16,cex = 1, cex.axis = 1, cex.lab = 1, col = adjustcolor("deepskyblue4",alpha.f = 0.5))
for (h in 2:n.fg){
  p = c(16,15,17)
  colores = c("darkgreen", "tan1")
  lines(cn1[[h]],lwd=1,pch=p[h],type="b", cex = 1, col = adjustcolor(colores[h-1],alpha.f = 0.5))
}
axis(2, at=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7), labels=c("0.1","0.2","0.3","0.4","0.5","0.6","0.7"),tick=T, cex.axis = 1)


##trees with size >100mm
rm(cen.emp)
cen.emp = list() #the empirical data
cen.emp[[1]] = cen.data1[cen.data1$dbh>=100,] #18445/227566 = 0.08105341 
cen.emp[[2]] = cen.data2[cen.data2$dbh>=100,] #18404/234436 = 0.0785033
cen.emp[[3]] = cen.data3[cen.data3$dbh>=100,] #18926/232874 = 0.08127142
cen.emp[[4]] = cen.data4[cen.data4$dbh>=100,] #19133/216527 = 0.08836311
cen.emp[[5]] = cen.data5[cen.data5$dbh>=100,] #18970/204348 = 0.09283184
cen.emp[[6]] = cen.data6[cen.data6$dbh>=100,] #18623/198383 = 0.09387397
cen.emp[[7]] = cen.data7[cen.data7$dbh>=100,] #18527/196732 = 0.0941738

### Counting

g.all.emp=list()
for (i in 1:length(cen.emp)){
  g1 = 0
  g1 = table(cen.emp[[i]]$fg)
  g.all.emp[[i]] = g1
  print(i)
}

###

n.fg = 3
#g.all is a list of each census, [[1]]is the simulation; each one has the total number of FGs as column heads and the total number of individuals

fun = function(x){x/sum(x)} #to calculate the percentages

g = g.all.emp #censuses
g = lapply(g,fun) #calculates the percentages of each FG

cn1 = list() ### cn1[[1]]=census1 [[1]]=fg1 (total reps)


for (i in 1:n.fg){
  
  use1 = c()
  
  for (j in 1:length(g)){
    
    use1 = c(use1,g[[j]][i])
  }
  cn1[[i]] = use1
}


### plot of empirical data for functional guilds proportions
# 
plot(main="Trees with DBH>=100mm",cn1[[1]],type="b",yaxt="n",xlab="Census",ylab="Proportion",ylim=c(0,0.72),pch = 16,cex = 1, cex.axis = 1, cex.lab = 1, col = adjustcolor("deepskyblue4",alpha.f = 0.5))
for (h in 2:n.fg){
  p = c(16,15,17)
  colores = c("darkgreen", "tan1")
  lines(cn1[[h]],lwd=1,pch=p[h],type="b", cex = 1, col = adjustcolor(colores[h-1],alpha.f = 0.5))
}
axis(2, at=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7), labels=c("0.1","0.2","0.3","0.4","0.5","0.6","0.7"),tick=T, cex.axis = 1)


#########################################################################################
#########################################################################################
#### counts of dead and new individuals!!

# number of trees that died, tags that are in census i but not in i+1

shared.12 = (cen.data1$id %in% cen.data2$id) #this is the num of trees shared
dead.12 = length(shared.12[shared.12==FALSE])
not.shared.21 = (cen.data2$id %in% cen.data1$id)
new.12 = length(not.shared.21[not.shared.21==FALSE])

shared.23 = (cen.data2$id %in% cen.data3$id) #this is the num of trees shared
dead.23 = length(shared.23[shared.23==FALSE])
not.shared.23 = (cen.data3$id %in% cen.data2$id)
new.23 = length(not.shared.23[not.shared.23==FALSE])

shared.34 = (cen.data3$id %in% cen.data4$id) #this is the num of trees shared
dead.34 = length(shared.34[shared.34==FALSE])
not.shared.34 = (cen.data4$id %in% cen.data3$id)
new.34 = length(not.shared.34[not.shared.34==FALSE])

shared.45 = (cen.data4$id %in% cen.data5$id) #this is the num of trees shared
dead.45 = length(shared.45[shared.45==FALSE])
not.shared.45 = (cen.data5$id %in% cen.data4$id)
new.45 = length(not.shared.45[not.shared.45==FALSE])

shared.56 = (cen.data5$id %in% cen.data6$id) #this is the num of trees shared
dead.56 = length(shared.56[shared.56==FALSE])
not.shared.56 = (cen.data6$id %in% cen.data5$id)
new.56 = length(not.shared.56[not.shared.56==FALSE])

shared.67 = (cen.data6$id %in% cen.data7$id) #this is the num of trees shared
dead.67 = length(shared.67[shared.67==FALSE])
not.shared.67 = (cen.data7$id %in% cen.data6$id)
new.67 = length(not.shared.67[not.shared.67==FALSE])


counts= c(dead.12,new.12,dead.23,new.23,dead.34,new.34,dead.45,new.45,dead.56,new.56,dead.67,new.67)

output.path <- paste0("counts", ".rds")
saveRDS(counts, output.path)
