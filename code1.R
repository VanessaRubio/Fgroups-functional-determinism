# checked on april 15 - 2021
###################################################################################################
################################### graphs empirical GUILD PROPORTION #############################
#this code calculates and graphs the empirical functional guild proportions of the 7 census of bci 
#at the end adds the simulated lines of the functional guild proportions of the 7 census of bci
#the simulated lines (medians) come from: april.code3.R

rm(list = ls())

#load BCI census data
census1=read.table("MasterBCI_C1.txt",h=T)
census1$fg=rep(0,nrow(census1))

census2=read.table("MasterBCI_C2.txt",h=T)
census2$fg=rep(0,nrow(census2))

census3=read.table("MasterBCI_C3.txt",h=T)
census3$fg=rep(0,nrow(census3))

census4=read.table("MasterBCI_C4.txt",h=T)
census4$fg=rep(0,nrow(census4))

census5=read.table("MasterBCI_C5.txt",h=T)
census5$fg=rep(0,nrow(census5))

census6=read.table("MasterBCI_C6.txt",h=T)
census6$fg=rep(0,nrow(census6))

census7=read.table("MasterBCI_C7.txt",h=T)
census7$fg=rep(0,nrow(census7))

###################################################################################################
#load the traits and performs the PCA

traits=read.table("traits.txt",h=T) 
rownames(traits) = traits$code
traits$code = NULL

shapiro.test(traits$hmax) #maximum height
traits$hmax=log10(traits$hmax)

shapiro.test(traits$la) #leaf area
traits$la=log10(traits$la)

shapiro.test(traits$lma3) #leaf mass per area
traits$lma3=log10(traits$lma3)

shapiro.test(traits$wsg) #wood density
#traits$wsg=log(traits$wsg)

pca=prcomp(na.omit(traits),retx=TRUE,scale.=T)
sd = pca$sdev
loadings = data.frame(pca$rotation)
scores = pca$x 
var = sd^2
var.percent = var/sum(var) * 100

col = dim(pca$x)[2] 
barplot(var.percent, xlab="PC", ylab="Percent Variance", names.arg=1:length(var.percent), las=1, ylim=c(0,max(var.percent)), col="gray")
abline(h=1/col*100, col="red")

###################################################################################################
#find the best cluster partitioning based on euclidean distances

library(vegan)
library(NbClust)

nbclust_out <- NbClust(
  data = pca$x[,1:2],
  distance = "euclidean",
  min.nc = 2, # minimum number of clusters
  max.nc = 12, # maximum number of clusters
  method = "ward.D",
  index = "all"
)

table = t(nbclust_out$Best.nc)
write.table(table, "nbclust.csv", sep = ",", col.names = NA)
clust = nbclust_out$Best.partition

#######################################################################################
#######################################################################################
#### For the  species: Empirical graphics

#PCA by guild

funk = function(x){ #compare the cluster species to the data to assign FG
  
  sp=x #recorre los nombres de las especies
  positions=match(toString(sp),names(clust))  #compare with clust vector
  fg=clust[positions]  #find the FG number
  
  return(fg)
  
}

#Figure 2
data = data.frame("wsg" = traits2$wsg,"hmax" = traits2$hmax,"lma" = traits2$lma3, "la" = traits2$la3)
guilds = as.numeric(unlist(lapply(rownames(data), funk))) 
data$guild = guilds
ordered = data[order(data$guild) , ]
n.variables = 4

pca=prcomp(na.omit(ordered[,1:n.variables]),retx=TRUE,scale.=T)
biplot(pca)
sd = pca$sdev
loadings = data.frame(pca$rotation)
scores = pca$x 
var = sd^2
explained.variance = var/sum(var) * 100

pch.group <- c(rep(16, times= as.numeric(table(clust)[1])), rep(15, times=as.numeric(table(clust)[2])), rep(17, times=as.numeric(table(clust)[3]))) #circulo, cuadrado, triangulo
colores = c(rep("deepskyblue4",as.numeric(table(clust)[1])),rep("darkgreen",as.numeric(table(clust)[2])),rep("tan1", as.numeric(table(clust)[3])))
lambda <- pca$sdev * sqrt(nrow(pca$x)) 

plot (t(t(pca$x)/lambda),pch=pch.group, xlab="PC1 (41.12%)", ylab="PC2 (30.94%)", yaxt="n", xaxt="n",ylim = c(-0.2,0.2),xlim = c(-0.21,0.21),col = adjustcolor(colores,alpha.f=0.4)) 
axis(side=1, at=c(-0.1,0,0.1), labels=c(-0.1,0,0.1), mgp=c(3,1,0), cex.axis=0.9)
axis(side=2, at=c(-0.1,0,0.1), labels=c(-0.1,0,0.1), mgp=c(3,1,0), cex.axis=0.9)
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
l.x <- pca$rotation[,1]/5
l.y <- pca$rotation[,2]/5

# Label position
l.pos <- l.y # Create a vector of y axis coordinates
lo <- which(l.y < 0) # Get the variables on the bottom half of the plot
hi <- which(l.y > 0) # Get variables on the top half
# Replace values in the vector
l.pos <- replace(l.pos, lo, "1")
l.pos <- replace(l.pos, hi, "3")
arrows(x0=0, x1=l.x, y0=0, y1=l.y, col="black", length=0.1, lwd=2)
row.names(pca$rotation) = c("wsg", "hmax", "lma", "la") 

l.pos[3] = 1
text(l.x, l.y, labels=row.names(pca$rotation), col="black", pos=l.pos, cex = 0.8)

legend("topleft", legend=c("FG 1", "FG 2", "FG 3"), col=  adjustcolor(c("deepskyblue4","darkgreen","tan1"),alpha.f = 0.4),pch=c(16, 15, 17), pt.cex=1, cex=0.75)

################################################################################################################################################################################
################################################################################################################################################################################
################################################################################################################################################################################
################ assign the functional group to the data, applying the function funk

funk = function(x){ #compare the cluster species to the data to assign FG
  
  sp=x
  positions=match(toString(sp),names(clust))  
  fg=clust[positions] 
  
  return(fg)
  
}

f.g1=as.numeric(unlist(lapply(census1$code, funk))) 
f.g2=as.numeric(unlist(lapply(census2$code, funk)))
f.g3=as.numeric(unlist(lapply(census3$code, funk)))
f.g4=as.numeric(unlist(lapply(census4$code, funk)))
f.g5=as.numeric(unlist(lapply(census5$code, funk)))
f.g6=as.numeric(unlist(lapply(census6$code, funk)))
f.g7=as.numeric(unlist(lapply(census7$code, funk)))

#create the tables, for each census
cen.dat1 = data.frame(cbind(f.g1,census1$quadrat,census1$StemID, as.data.frame(census1$code),1))
#re-name the column 5
cen1=cen.dat1[!is.na(cen.dat1$f.g1),] #just use the species that have a functional group
cen.data1=data.frame(cbind(cen1$f.g1,cen1$census1.quadrat, cen1$census1.StemID, as.data.frame(cen1$census1.code),1))

cen.dat2 = data.frame(cbind(f.g2,census2$quadrat,census2$StemID, as.data.frame(census2$code),1))
#re-name the column 5
cen2=cen.dat2[!is.na(cen.dat2$f.g2),] #just use the species that have a functional group
cen.data2=data.frame(cbind(cen2$f.g2,cen2$census2.quadrat, cen2$census2.StemID, as.data.frame(cen2$census2.code),1))

cen.dat3 = data.frame(cbind(f.g3,census3$quadrat,census3$StemID, as.data.frame(census3$code),1))
#re-name the column 5
cen3=cen.dat3[!is.na(cen.dat3$f.g3),] #just use the species that have a functional group
cen.data3=data.frame(cbind(cen3$f.g3,cen3$census3.quadrat, cen3$census3.StemID, as.data.frame(cen3$census3.code),1))

cen.dat4 = data.frame(cbind(f.g4,census4$quadrat,census4$StemID, as.data.frame(census4$code),1))
#re-name the column 5
cen4=cen.dat4[!is.na(cen.dat4$f.g4),] #just use the species that have a functional group
cen.data4=data.frame(cbind(cen4$f.g4,cen4$census4.quadrat, cen4$census4.StemID, as.data.frame(cen4$census4.code),1))

cen.dat5 = data.frame(cbind(f.g5,census5$quadrat,census5$StemID, as.data.frame(census5$code),1))
#re-name the column 5
cen5=cen.dat5[!is.na(cen.dat5$f.g5),] #just use the species that have a functional group
cen.data5=data.frame(cbind(cen5$f.g5,cen5$census5.quadrat, cen5$census5.StemID, as.data.frame(cen5$census5.code),1))

cen.dat6 = data.frame(cbind(f.g6,census6$quadrat,census6$StemID, as.data.frame(census6$code),1))
#re-name the column 5
cen6=cen.dat6[!is.na(cen.dat6$f.g6),] #just use the species that have a functional group
cen.data6=data.frame(cbind(cen6$f.g6,cen6$census6.quadrat, cen6$census6.StemID, as.data.frame(cen6$census6.code),1))

cen.dat7 = data.frame(cbind(f.g7,census7$quadrat,census7$StemID, as.data.frame(census7$code),1))
#re-name the column 5
cen7=cen.dat7[!is.na(cen.dat7$f.g7),] #just use the species that have a functional group
cen.data7=data.frame(cbind(cen7$f.g7,cen7$census7.quadrat,cen7$census7.StemID, as.data.frame(cen7$census7.code),1))

names = c("fg","quadrat","id","code","X1")
colnames(cen.data1) = names
colnames(cen.data2) = names
colnames(cen.data3) = names
colnames(cen.data4) = names
colnames(cen.data5) = names
colnames(cen.data6) = names
colnames(cen.data7) = names

#save each cleaned census data 
census.total = list(cen.data1,cen.data2,cen.data3,cen.data4,cen.data5,cen.data6,cen.data7)

dir.create("census.data")

for (i in 1:7){
  output.path <- paste0("census.data", "/cen.data", i, ".rds")
  saveRDS(census.total[[i]], output.path)
}

cen.emp = list() #the empirical data
cen.emp[[1]] = cen.data1
cen.emp[[2]] = cen.data2
cen.emp[[3]] = cen.data3
cen.emp[[4]] = cen.data4
cen.emp[[5]] = cen.data5
cen.emp[[6]] = cen.data6
cen.emp[[7]] = cen.data7

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
plot(cn1[[1]],type="b",yaxt="n",xlab="Census",ylab="Proportion",ylim=c(0,0.72),pch = 16,cex = 1, cex.axis = 1, cex.lab = 1)
for (h in 2:n.fg){
  p = c(16,15,17)
  lines(cn1[[h]],lwd=1,pch=p[h],type="b", cex = 1)
}
axis(2, at=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7), labels=c("0.1","0.2","0.3","0.4","0.5","0.6","0.7"),tick=T, cex.axis = 1)


##add the simulated values for functional guilds proportions. 
##Find them in code3.R
lines(medians[[1]],lty = 3)
lines(medians[[2]],lty = 3)
lines(medians[[3]],lty = 3)
