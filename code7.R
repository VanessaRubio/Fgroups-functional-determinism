# checked on april 16 - 2021
####################################################################################################################################
################################### torus translation ##############################################################################
### this code performs the torus translation for the functional guilds

rm(list = ls())
library("vegan")
library("dplyr")
library("NbClust")
######## PCA
setwd("~/Downloads/torus")
bci.soil = read.table("~/Downloads/torus/bci.block20.data.txt",h=T) #goes from 0 to 24 and then 100 to 124, 200 to 224....,4900 to 4924, or 1:1250 from bottom of each column.

######## this arranges the data in a nicer table with index (x_y values in plot, x, y, PC1, PC2, PC3, and all nutrient values)
index<-paste((bci.soil$x-10),"_",(bci.soil$y-10),sep="")
envt.index<-cbind(index, bci.soil[,1:2], pca.soil$x[,1:3],bci.soil[,3:15])

#### repeat this code for all the different census

census=read.table("~/Downloads/MasterBCI_C1.txt",h=T)
envt.index$quad = sort(unique(census$quadrat))

######## assigns the soil PCA and the raw soil variables to the census data
funk = function(x){ 
  
  quad=x
  positions=match(quad,envt.index$quad)  #compare with soil data
  variables=envt.index[positions,]  #find the variables
  
  return(variables)
  
}

all.variables = do.call("rbind", lapply(census$quadrat, funk)) 
data = cbind(census,all.variables) #binds census data with the variables data

########  Add the raw trait data to the census data
traits = read.table("~/Downloads/BCI_sp.txt",h=T) #trait data
traits$hmax = log(traits$hmax)
traits$la = log(traits$la)

funk2 = function(x){ #
  
  sp=x #recorre los nombres de las especies
  positions=match(sp,traits$code)  #compare with clust vector
  all.traits=traits[positions,]  #find the FG number
  
  return(all.traits)
  
}

all.variables.traits = do.call("rbind", lapply(census$code, funk2))
data = cbind(data,all.variables.traits)

######## Add the functional guilds to the data
######## cluster with the functional guilds
load('~/Downloads/bci.clust.Rdata') #cluster of species by guild

funk3 = function(x){ 

  sp=x
  positions=match(toString(sp),names(clust))
  fg=clust[positions] 

  return(fg)

}

data$fg = as.numeric(unlist(lapply(data$code,funk3)))


#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 

#loads the topographic types for each quadrat
harms = read.table("~/Downloads/harms.txt",h=T)

harms$quad = sort(unique(census$quadrat))
funk4 = function(x){
  
  quad=x 
  positions=match(quad,harms$quad) 
  h=harms[positions,"HabitatNumber"]
  
  return(h)
  
}

data$harms = as.numeric(unlist(lapply(data$quad, funk4))) 
data = na.omit(data)
length(unique(data$code))


#filename=paste("data.c1.torus",".Rdata",sep="")
#save(data,file=filename)


#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 

load('data.c1.torus.Rdata') #data
data[,2]=NULL #removes code duplicated
harms = read.table("~/Downloads/harms.txt",h=T) #harms et al. soil classes 7

################################################## WITH FUNCTIONAL GUILDS ##################################################
################################################## ################################################## ################################################## 
#### cluster
library(dplyr)
obs <- data %>% group_by(quadrat) %>% summarise(fg1 = sum(fg==1), fg2 = sum(fg==2), fg3=sum(fg==3))  # this is the observed empirical abundance of guilds in each of the 1250 quadrats
obs = as.data.frame(obs)
rownames(obs) = obs$quadrat
obs$quadrat = NULL
allabund3 = as.data.frame(t(obs))
total = colSums(allabund3)
allabund3 = rbind(allabund3,total)
rownames(allabund3) = c("fg1","fg2","fg3","total")
species.list3 = case.names(allabund3)[-4]
allabund3 = as.matrix(allabund3)
colnames(allabund3) = 1:1250

length(unique(data$code))
table(data$fg)
c(sum(allabund3[1,]), sum(allabund3[2,]), sum(allabund3[3,]))

################################################## ################################################## #######################
################################################## ################################################## ################################################## 
### habitat matrices:
print(date())
# harms et al, habitats 1:7
habmat=matrix(harms$HabitatNumber,nrow=25)

## code taken and modified from: Harms et al 2001, Ecology; and the forestgeo package and function tt_test

torusallspp = function(habmat, allabund, species.list, plotdim=c(1000,500), gridsize=20) #habmat2,allabund,species.list,plotdim=c(1000,500), gridsize=20
{
  plotdimqx=plotdim[1]/gridsize  # Calculates no. of x-axis quadrats of plot. #50
  plotdimqy=plotdim[2]/gridsize  # Calculates no. of y-axis quadrats of plot.habitat #25
  
  num.habs=length(unique(as.vector(habmat)))  # Determines tot. no. of  types. #7
  sppno=length(species.list)  # Determines tot. no. of spp. for which to conduct habitat analysis. #4guilds
  GrLsEq=matrix(0,sppno,num.habs*6)  # Creates empty matrix for output. Feb 24 2021
  rownames(GrLsEq)=species.list  # Names rows of output matrix.
  
  for(i in 1:num.habs)  # Creates names for columns of output matrix.
  {
    if(i==1)
      cols=c(paste("N.Hab.",i,sep=""), paste("Gr.Hab.",i,sep=""), paste("Ls.Hab.",i,sep=""), paste("Eq.Hab.",i,sep=""),paste("Rep.Agg.Neut.", i, sep = ""),paste("Obs.Quantile.", i, sep = ""))
    if(i>1)
      cols=c(cols, paste("N.Hab.",i,sep=""), paste("Gr.Hab.",i,sep=""), paste("Ls.Hab.",i,sep=""), paste("Eq.Hab.",i,sep=""),paste("Rep.Agg.Neut.", i, sep = ""),paste("Obs.Quantile.", i, sep = ""))  
  }
  colnames(GrLsEq)=cols  # Names columns of output matrix. N.hab = total in real hab?, Gr.Hab = found more than real habitat, Ls.Hab = found less, Eq.Hab = found equal
  
  
  for(j in 1:sppno)  # Opens "for loop" through all spp.
  {
    cat("Currently on species = ", species.list[j], "\n")  
    # cat() prints current sp. mnem. to screen.
    
    # CALCULATIONS FOR OBSERVED RELATIVE DENSITIES ON THE TRUE HABITAT MAP
    
    spmat=matrix(allabund[species.list[j],],plotdimqy,plotdimqx,byrow=F)  # Fills a matrix, with no. rows = plotdimqy and no. columns = plotdimqx, with the indiv. counts per quadrat of ONE species. Quadrat 1 on the top left, quadrta 25 on the bottom left: [1,2]=quadrat 26; [25,2]=quadrat 50...
    
    tot= allabund[sppno+1,] # Creates empty vector for tot. no. stems per quadrat.
    
    totmat=matrix(tot,plotdimqy,plotdimqx,byrow=F)  # Converts "tot" to a matrix, with no. rows = plotdimqy and no. columns = plotdimqx. 
    
    spstcnthab=numeric()  # Creates empty vector for stem counts per sp. per habitat.
    totstcnthab=numeric()  # Creates empty vector for tot. stem counts per habitat.
    
    for(k in 1:num.habs)  
    {
      totstcnthab[k]=sum(totmat[habmat==k])  # Determines tot. no. stems per habitat of the true map.
      spstcnthab[k]=sum(spmat[habmat==k])  # Determines tot. no. stems for focal sp. per habitat of the true map. 	
    }
    spprophab=spstcnthab/totstcnthab  # Calculates observed relative stem density of focal sp. per habitat of the true map.
    
    
    # CALCULATIONS FOR RELATIVE DENSITIES ON THE TORUS-BASED HABITAT MAPS
    habmat.template <- habmat
    
    for (m in 1:4) {
      # apply rotations and mirrors
      # if j==1 do nothing

      if (m == 2) habmat=apply(habmat.template, 2, rev)
      if (m == 3) habmat=t(apply(habmat.template, 1, rev))
      if (m == 4) habmat=t(apply(apply(habmat.template, 2, rev), 1, rev))


    for(x in 0:(plotdimqx-1))  # Opens "for loop" through all 20-m translations along x-axis.
    { 
      for (y in 0:(plotdimqy-1))  # Opens "for loop" through all 20-m translations along y-axis.
      { 
        newhab=matrix(0,plotdimqy,plotdimqx)  # Creates empty matrix of quadrats' habitat designations.
        
        # The following "if" statements create the x,y torus-translation of the habitat map.
        
        if(y==0 & x==0)          
          newhab=habmat
        
        if(y==0 & x>0)
          newhab=habmat[c(1:plotdimqy),c((plotdimqx-x+1):plotdimqx,1:(plotdimqx-x))] #moves the last row above
        
        if(x==0 & y>0)
          newhab=habmat[c((plotdimqy-y+1):plotdimqy,1:(plotdimqy-y)),c(1:plotdimqx)]
        
        if(x>0 & y>0)
          newhab=habmat[c((plotdimqy-y+1):plotdimqy,1:(plotdimqy-y)),c((plotdimqx-x+1):plotdimqx,1:(plotdimqx-x))] 
      
        
        Torspstcnthab=numeric()  # Creates empty vector for stem counts per sp. per habitat in torus-based maps.
        Tortotstcnthab=numeric()  # Creates empty vector for tot. stem counts per habitat in torus-based maps.
        for(i in 1:num.habs)
        {
          Tortotstcnthab[i]=sum(totmat[newhab==i])  # Determines tot. no. stems per habitat of the focal torus-based map.
          Torspstcnthab[i]=sum(spmat[newhab==i])  # Determines tot. no. stems for focal sp. per habitat of the focal torus-based map.
        }
        
        Torspprophab=Torspstcnthab/Tortotstcnthab  # Calculates relative stem density of focal sp. per habitat of the focal torus-based map.  
        
        for(i in 1:num.habs)
        {
          
          GrLsEq[j,(6*i)-5]=spstcnthab[i]
          
          if(spprophab[i]>Torspprophab[i])  # If rel. dens. of focal sp. in focal habitat of true map is greater than rel. dens. of focal sp. in focal habitat of torus-based map, then add one to "greater than" count. 		
            #GrLsEq[j,(4*i)-2]=GrLsEq[j,(4*i)-2]+1  
            GrLsEq[j,(6*i)-4]=GrLsEq[j,(6*i)-4]+1  
          
          if(spprophab[i]<Torspprophab[i])  # If rel. dens. of focal sp. in focal habitat of true map is less than rel. dens. of focal sp. in focal habitat of torus-based map, then add one to "less than" count. 
            #GrLsEq[j,(4*i)-1]=GrLsEq[j,(4*i)-1]+1 
            GrLsEq[j,(6*i)-3]=GrLsEq[j,(6*i)-3]+1
          
          if(spprophab[i]==Torspprophab[i])  # If rel. dens. of focal sp. in focal habitat of true map is equal to rel. dens. of focal sp. in focal habitat of torus-based map, then add one to "equal to" count.
            #GrLsEq[j,4*i]=GrLsEq[j,4*i]+1 
            GrLsEq[j,(6*i)-2]=GrLsEq[j,(6*i)-2]+1 
          
          
          #### From the tt_test package: forestgeo
          
          # if rel.dens. of sp in true map is greater than rel. dens. in torus map
          # less than 2.5% of the time, then repelled
          if (GrLsEq[j, (6 * i) - 4] / (4 * (plotdimqx * plotdimqy)) <= 0.025) {
            GrLsEq[j, (6 * i) - 1] <- -1
          }
          
          # if rel.dens. of sp in true map is greater than rel. dens. in torus map
          # more than 97.5% of the time, then aggregated
          if (GrLsEq[j, (6 * i) - 4] / (4 * (plotdimqx * plotdimqy)) >= 0.975) {
            GrLsEq[j, (6 * i) - 1] <- 1
          }
          
          # otherwise it's neutral (not different from random dist)
          if ((GrLsEq[j, (6 * i) - 4] / (4 * (plotdimqx * plotdimqy)) < 0.975) &
              (GrLsEq[j, (6 * i) - 4] / (4 * (plotdimqx * plotdimqy)) > 0.025)) {
            GrLsEq[j, (6 * i) - 1] <- 0
          }
          
          # quantile in the TT distribtution of relative densities of the true
          # relative density
          GrLsEq[j, (6 * i)] <- GrLsEq[j, (6 * i) - 4] / (4 * (plotdimqx * plotdimqy))
          
          
          
          
        }
      
      }  # Closes "for loop" through all 20-m translations along y-axis.
    }    # Closes "for loop" through all 20-m translations along x-axis.
  } #closes for loos through mirrors and rotations
  
  }  # Closes "for loop" through all spp.                   
  return(GrLsEq)
} 

# harms: cluster
harms = torusallspp(habmat,allabund3,species.list3,plotdim=c(1000,500), gridsize=20) 
View(harms)

########################################################################################################################################################
#######################################################################################################################################################
################################################################## graphs  #############################################################################
## 

cen=read.table("~/Downloads/MasterBCI_C5.txt",h=T)
harms = read.table("~/Downloads/harms.txt",h=T) #harms et al. soil classes 7
load('~/Downloads/bci.clust.Rdata') #clust is the file

funk3 = function(x){
  
  sp=x 
  positions=match(toString(sp),names(clust)) 
  fg=clust[positions]
  
  return(fg)
  
}

cen$fg = as.numeric(unlist(lapply(cen$code,funk3)))

## f.g3
image(xax,yax,t(bci.map))
guild3 = cen[cen$fg == 3,]
selection <- !is.na(guild3$px) & !is.na(guild3$py)
guild3_noNA <- guild3[selection, ]

x <- guild3_noNA$px
y <- guild3_noNA$py
dens2d <- MASS::kde2d(x, y)
d <- densCols(x, y, colramp = colorRampPalette(rev(rainbow(10, end = 4/6))))                                                                                         
point.cols <- scales::alpha(d, .3)                                                                                                                                          
plot(x, y,  pch = 20, col = point.cols)

## f.g1
par(mfrow = c(1,2))
image(xax,yax,t(bci.map))
guild1 = cen[cen$fg == 1,]
selection <- !is.na(guild1$px) & !is.na(guild1$py)
guild1_noNA <- guild1[selection, ]

x <- guild1_noNA$px
y <- guild1_noNA$py
dens2d <- MASS::kde2d(x, y)
d <- densCols(x, y, colramp = colorRampPalette(rev(rainbow(10, end = 4/6))))                                                                                         
point.cols <- scales::alpha(d, .3)                                                                                                                                          
plot(x, y,  pch = 20, col = point.cols)

## f.g3 
par(mfrow = c(1,2))
image(xax,yax,t(bci.map))
guild2 = cen[cen$fg == 2,]
selection <- !is.na(guild2$px) & !is.na(guild2$py)
guild2_noNA <- guild2[selection, ]

x <- guild2_noNA$px
y <- guild2_noNA$py
dens2d <- MASS::kde2d(x, y)
d <- densCols(x, y, colramp = colorRampPalette(rev(rainbow(10, end = 4/6))))                                                                                         
point.cols <- scales::alpha(d, .3)                                                                                                                                          
plot(x, y,  pch = 20, col = point.cols)
