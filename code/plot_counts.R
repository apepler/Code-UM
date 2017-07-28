## This is code that takes the 11 ensemble members of a POAMA run
## Started on a particular day
## And outputs some interested results for further analysis

## I care about ECLs, so main result is a daily timeseries of # of members that
## have an ECL on that day

## Also outputs a figure for each of the months in the file
## That shows the number of cyclones centred in that month
## Option to set a higher intensity threshold

library(maps)
color.palette <- function(steps, n.steps.between=NULL, ...){
  
  if(is.null(n.steps.between)) n.steps.between <- rep(0, (length(steps)-1))
  if(length(n.steps.between) != length(steps)-1) stop("Must have one less n.steps.between value than steps")
  
  fill.steps <- cumsum(rep(1, length(steps))+c(0,n.steps.between))
  RGB <- matrix(NA, nrow=3, ncol=fill.steps[length(fill.steps)])
  RGB[,fill.steps] <- col2rgb(steps)
  
  for(i in which(n.steps.between>0)){
    col.start=RGB[,fill.steps[i]]
    col.end=RGB[,fill.steps[i+1]]
    for(j in seq(3)){
      vals <- seq(col.start[j], col.end[j], length.out=n.steps.between[i]+2)[2:(2+n.steps.between[i]-1)]  
      RGB[j,(fill.steps[i]+1):(fill.steps[i+1]-1)] <- vals
    }
  }
  
  new.steps <- rgb(RGB[1,], RGB[2,], RGB[3,], maxColorValue = 255)
  pal <- colorRampPalette(new.steps, ...)
  return(pal)
}

col_anom <- color.palette(c("darkred","red","white","blue","darkblue"),c(10,20,20,10))
col_val <- color.palette(c("white","blue","darkblue","black"),c(20,10,5))

ColorBar <- function(brks,cols,vert=T,subsampleg=1)
{
  par(mar = c(3, 1, 3, 3), mgp = c(1, 1, 0), las = 1, cex = 1)
  image(1, c(1:length(cols)), t(c(1:length(cols))), axes = FALSE, col = cols, 
        xlab = '', ylab = '')
  box()
  axis(4, at = seq(1.5, length(brks) - 1.5, subsampleg), tick = TRUE, 
       labels = brks[seq(2, length(brks)-1, subsampleg)])
}

plot_counts<-function(year1,year2,dir="gcyc_out",cv=NA,dur=NA,slp=NA,month1=1,month2=12,fout=NA)
{
years=seq(year1,year2,1)

lat=seq(-89.5,89.5,1)
lon=seq(0.5,359.5,1)  ### Can always combine into bigger cells later
if(month2>=month1) systems<-array(0,c(length(lon),length(lat),length(years))) else systems<-array(0,c(length(lon),length(lat),length(years-1)))

for(y in 1:length(years))
{
print(years[y])
fname=paste(dir,"/tracks_",years[y],".dat",sep="")
read.table(fname, sep="",skip=1)->fixes
colnames(fixes)=c("ID","Fix","Date","Time","Open","Lon","Lat","MSLP","CV","Meh")
fixes$Year=floor(fixes$Date/10000)
fixes$CV=abs(fixes$CV)
fixes=fixes[fixes$Year==unique(fixes$Year)[2],]
fixes$Month=floor(fixes$Date/100)%%100
fixes$Lat2=floor(fixes$Lat)
fixes$Lon2=floor(fixes$Lon)%%360
### Make table of events to combine with DJF for exclusion
 if(!is.na(dur))
  {
  x<-rle(fixes$ID)
  events<-cbind(x$values,x$lengths,matrix(data=0,nrow=length(x$values),ncol=1))
  events=events[events[,2]>=dur,]
  include<-match(fixes[,1],events[,1])
  J<-which(is.na(include)==0)
  fixes=fixes[J,]
  }
if(!is.na(cv)) fixes=fixes[fixes$CV>=cv,]
if(!is.na(slp)) fixes=fixes[fixes$MSLP>=slp,]

if(month2>=month1)
{
I=which(fixes$Month>=month1 & fixes$Month<=month2)
systems[,,y]=table(factor(fixes$Lon2,levels=0:359),factor(fixes$Lat2,levels=-90:89))
} else {
if(y<length(years))
{
I=which(fixes$Month>=month1)
systems[,,y]=table(factor(fixes$Lon2,levels=0:359),factor(fixes$Lat2,levels=-90:89))
}
if(y>1)
{
I=which(fixes$Month<=month2)
systems[,,y-1]=systems[,,y-1]+table(factor(fixes$Lon2,levels=0:359),factor(fixes$Lat2,levels=-90:89))
}
}

} # End year loop

print("Calculating means")
if(month2<month1) years=seq(year1,year2-1,1)

### Plot the average & the linear trend

### Mean frequency

lat2=seq(-87.5,-2.5,5)
lon2=seq(2.5,357.5,5)
meanfreq<-array(0,c(length(lon2),length(lat2)))

for(i in 1:length(lon2))
  for(j in 1:length(lat2))
  {
    I=which(lon>=lon2[i]-2.5 & lon<lon2[i]+2.5)
    J=which(lat>=lat2[j]-2.5 & lat<lat2[j]+2.5)
    meanfreq[i,j]=mean(apply(systems[I,J,],3,sum))
  }

### Linear trend

print("Calculating trends")
lat3=seq(-85,-5,10)
lon3=seq(5,355,10)
cyctrend<-array(NaN,c(length(lon3),length(lat3),2))

for(i in 1:length(lon3))
  for(j in 1:length(lat3))
  {
    I=which(lon>=lon3[i]-5 & lon<lon3[i]+5)
    J=which(lat>=lat3[j]-5 & lat<lat3[j]+5)
    tmp=apply(systems[I,J,],3,sum)
    
    if(mean(tmp)>=2)
    {
      a=lm(tmp~years)
      b=summary(a)$coefficients
      cyctrend[i,j,1]=100*a$coefficients[2]/mean(tmp)
      cyctrend[i,j,2]=b[2,4]          
    }
  }

### Plot

print("Plotting")
breaks1=c(0,1,5,10,20,40,60,80,100,1000)
col1=col_val(9)
breaks2=c(-100,seq(-3,3,0.5),100)
col2=col_anom(14)

if(is.na(fout)) fout=paste("Systems_mean_trend_",year1,"_",year2,".pdf")

pdf(file=fout,width=10,height=7)
layout(cbind(c(1,2),c(3,4)),width=c(1,0.1))
par(mar=c(3,3,4,1))

image(lon2,lat2,meanfreq,breaks=breaks1,col=col1,xlab="",ylab="",
          main=paste("Mean frequency",year1,"-",year2))
map('world2',add=T)

image(lon3,lat3,cyctrend[,,1],breaks=breaks2,col=col2,xlab="",ylab="",
          main=paste("Linear trend",year1,"-",year2,"(%/year)"))
map('world2',add=T)
sigmask=which(cyctrend[,,2]<0.05,arr.ind=T)
points(lon3[sigmask[,1]],lat3[sigmask[,2]],col="black",pch=19,cex=0.6)

ColorBar(breaks1,col1,subsampleg=1)
ColorBar(breaks2,col2,subsampleg=1)
dev.off()
}

plot_counts(1950,2016,dir="/short/eg3/asp561/cts.dir/gcyc_out/NCEP1/proj100_rad5cv0.15",
           fout="NCEP1_UM_cyclones_meantrend_1950-2016_rad5cv15.pdf")
plot_counts(1950,2016,dir="/short/eg3/asp561/cts.dir/gcyc_out/NCEP1/proj100_rad5cv0.15",dur=2,
           fout="NCEP1_UM_cyclones_meantrend_1950-2016_rad5cv15_D2.pdf")
plot_counts(1950,2016,dir="/short/eg3/asp561/cts.dir/gcyc_out/NCEP1/proj100_rad5cv0.15",dur=2,cv=0.25,
           fout="NCEP1_UM_cyclones_meantrend_1950-2016_rad5cv25_D2.pdf")


