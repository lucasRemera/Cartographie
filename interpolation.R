kPlusProchesVoisins=function(x0,y0,xref,yref,f1ref,f2ref=NULL, k=3,seuil=Inf,nmin=0){
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
  return(list(c=c,t=t,p=p))
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

##kppv=apply(mIn,1,function(i) kPlusProchesVoisins(i[1],i[2],corine$x,corine$y,corine$cas,corine$temoins))
##pid=apply(mIn,1,function(i) PID(i[1],i[2],corine$x,corine$y,corine$cas,corine$temoins,seuil=0.5,p=1,nmin=1))