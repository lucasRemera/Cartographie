library(ggplot2)
load("rhone.RData",verbose = T)
load("isere.RData",verbose = T)
load("loire.RData",verbose = T)
load("ain.RData",verbose = T)
ggrhone=fortify(rhone)
ggloire=fortify(loire)
ggisere=fortify(isere)
ggain=fortify(a2) #departement in polygon format

# AR=1/cos(mean(ggisere$lat)*pi/180)
# g2=ggplot()+ geom_polygon(data=ggrhone, aes(long, lat, group = group),colour = alpha("black", 1/2), size = 0.7, fill = 'grey', alpha = .3)+
#   geom_polygon(data=ggisere, aes(long, lat, group = group), colour = alpha("black", 1/2), size = 0.7, fill = 'grey', alpha = .3)+
#   geom_polygon(data=ggain, aes(long, lat, group = group), colour = alpha("black", 1/2), size = 0.7, fill = 'grey', alpha = .3)+
#   geom_polygon(data=ggloire, aes(long, lat, group = group), colour = alpha("black", 1/2), size = 0.7, fill = 'grey', alpha = .3)+
#   coord_fixed(ratio=AR) #the card of Rhone-Alpes region

#get card of Rhone-Alpes region
plotRA=function(region=list(ggrhone,ggisere,ggloire,ggain),ar=TRUE,transparency=0.3){
 gg=ggplot()
 for(r in region){
   gg=gg+geom_polygon(data=r, aes(long, lat, group = group),colour = alpha("black", 1/2), size = 0.7, fill = 'grey', alpha = transparency)
 }
 if(ar){
   AR=1/cos(mean(region[[1]]$lat)*pi/180)
   gg=gg+coord_fixed(ratio=AR)
 }
 return(gg)
}

# ril=rbind(ggrhone,ggisere,ggloire,ggain)
# prec=200
# mx=seq(min(ril[,1])-0.05,max(ril[,1]),length.out = prec)
# my=seq(min(ril[,2])-0.05,max(ril[,2]),length.out = prec)
# mm=expand.grid(mx,my)
# mIn=expand.grid(mx,my)
# library(sp)
# inRegion=which(  point.in.polygon(mIn[,1],mIn[,2],ggrhone[,1],ggrhone[,2])>0 |
#                    point.in.polygon(mIn[,1],mIn[,2],ggisere[,1],ggisere[,2])>0|
#                    point.in.polygon(mIn[,1],mIn[,2],ggain[,1],ggain[,2])>0|
#                    point.in.polygon(mIn[,1],mIn[,2],ggloire[,1],ggloire[,2])>0)
# 
# 
# mIn=mIn[inRegion,]
# colnames(mIn)=c("x","y")
# #mIn 

discreteRegion=function(region=list(ggrhone,ggisere,ggloire,ggain),precision=200){
  ril=do.call(rbind,region)
  mx=seq(min(ril[,1])-0.05,max(ril[,1]),length.out = precision)
  my=seq(min(ril[,2])-0.05,max(ril[,2]),length.out = precision)
  mm=expand.grid(mx,my)
  mIn=expand.grid(mx,my)
  isInRegion=rep(FALSE,nrow(mIn))
  for(r in region){
    isInRegion=isInRegion|point.in.polygon(mIn[,1],mIn[,2],r[,1],r[,2])>0
  }
  mIn=mIn[which(isInRegion),]
  colnames(mIn)=c("x","y")
  return(mIn)
}
