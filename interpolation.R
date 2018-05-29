kPlusProchesVoisins=function(x0,y0,xref,yref,f1ref,f2ref=NULL, k=3,seuil=Inf,nmin=0,toreturn=c("list","vector")){
  d=sqrt((x0-xref)**2+(y0-yref)**2)
  i=order(d)[1:k]
  if(min(d)>seuil){
    p=NA
    c=0
    t=0
  } 
  else{
    i=i[which(d[i]<seuil)]
    c=sum(f1ref[i])
    t=sum(f2ref[i])
    if(t+c>=nmin) p=c/(t+c)
    else p=NA
  }
  if(toreturn[1]=="list") return(list(c=c,t=t,p=p))
  else return(c(c=c,t=t,p=p))
}


PID=function(x0,y0,xref,yref,f1ref,f2ref=NULL,seuil=Inf,p=1,nmin=0){
  d=sqrt((x0-xref)**2+(y0-yref)**2)
  # if(min(d)==0){
  #   i=which(d==0)
  #   return(list(c=f1ref[i],t=f2ref[i],p=f1ref[i]/(f2ref[i]+f1ref[i])))
  # } 
  if(min(d)>seuil){
    p=NA
    c=0
    t=0
  }
  else{
    c=sum(f1ref[which(d<seuil)]/(d[which(d<seuil)]**p))
    t=sum(f2ref[which(d<seuil)]/(d[which(d<seuil)]**p))
    if(t+c >= nmin) p=c/(t+c)
    else p=NA
  }
  return(list(c=c,t=t,p=p))
}

#data a data.fram xith $X, $Y, $cas, $temoins
epicentre=function(data,xy,vk=c(1,3,5,9,15),alpha=.05){
  ptm=sum(data$cas)/(sum(data$temoins)+sum(data$cas))
  nepi=rep(0,nrow(xy))
  for(kk in vk){
    kppv.tm=t(sapply(1:nrow(xy),function(i) kPlusProchesVoisins(xy[i,1],xy[i,2],data$X,data$Y,data$cas,data$temoins,toreturn = "vector",k = kk)))
    pv.tm=1-pbinom(kppv.tm[,1],kppv.tm[,1]+kppv.tm[,2],ptm)
    nepi=nepi+as.numeric(pv.tm<alpha)
  }
  return(nepi)
}

# toutesMalf=as.data.frame(tbl)
# ptm=sum(toutesMalf$cas)/(sum(toutesMalf$temoins+toutesMalf$cas))
# kppv.tm=t(sapply(1:nrow(mIn),function(i) kPlusProchesVoisins(mIn[i,1],mIn[i,2],toutesMalf$X,toutesMalf$Y,toutesMalf$cas,toutesMalf$temoins,toreturn = "vector")))
# ggplot()+geom_raster(aes(x=mIn$x,y=mIn$y,fill=kppv.tm[,3]))+scale_fill_gradient2(low="green",high="red",midpoint = ptm)
# pv.tm=1-pbinom(kppv.tm[,1],kppv.tm[,1]+kppv.tm[,2],ptm)
# ggplot()+geom_raster(aes(x=mIn$x,y=mIn$y,fill=pv.tm<.05))
# ##pid=apply(mIn,1,function(i) PID(i[1],i[2],corine$x,corine$y,corine$cas,corine$temoins,seuil=0.5,p=1,nmin=1))
# 
# npv=epicentre(toutesMalf,mIn,alpha=.025)
# plotRA()+geom_raster(aes(x=mIn$x,y=mIn$y,fill=(npv)))+scale_fill_gradient2(low="green",mid="yellow",high="red",midpoint = 2)+labs(fill="#clusters",x="longitude",y="latitude")+ggtitle("Epicentre geographique")
