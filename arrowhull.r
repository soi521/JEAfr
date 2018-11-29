
#n.b. Legend only comes up with colours if you have showhull=TRUE

  propinhull=0.5	#Put =1.0 to show a convex hull covering all points, =0.5 to show hull covering half the points (if showhull=TRUE)
  arrowdata=c()
  for (plotregion in plotregions) {
   uu1=which(areanamesu==plotregion)
   nnn=length(arstxCF[uu1,])
   stxl=arstxCF[uu1,];styl=arstyCF[uu1,];enxl=arendxF[uu1,];enyl=arendyF[uu1,];clrl=colcpal2[uu1];Region=as.character(areanamesu[uu1])
   stxb=rep(arstx[uu1],times=nnn);styb=rep(arsty[uu1],times=nnn);enxb=rep(arendx[uu1],times=nnn);enyb=rep(arendy[uu1],times=nnn);arrhs=rep(arrheadsize[uu1],times=nnn);newxb=rep(newx[uu1],times=nnn);newyb=rep(newy[uu1],times=nnn)
   arrowdata=rbind(arrowdata,data.frame(Region,clrl,stxl,styl,enxl,enyl,stxb,styb,enxb,enyb,arrhs,newxb,newyb))	#"b" for Big arrows data (same each row within each Region) and "l" for Little arrows data
  }
  arrowdata_noNA=remNAs(arrowdata)
  blanks=rep(NA,times=nrow(arrowdata_noNA))
  if (plotprobs) {
   minx=0;maxx=1;miny=0;maxy=1
  } else {
   if (littlearrows) {
    minx=min(c(arrowdata_noNA$stxl,arrowdata_noNA$enxl),na.rm=TRUE)*0.9
    maxx=max(c(arrowdata_noNA$stxl,arrowdata_noNA$enxl),na.rm=TRUE)*1.1
    miny=min(c(arrowdata_noNA$styl,arrowdata_noNA$enyl),na.rm=TRUE)*0.9
    maxy=max(c(arrowdata_noNA$styl,arrowdata_noNA$enyl),na.rm=TRUE)*1.1
   } else {
    minx=min(c(arrowdata_noNA$stxb,arrowdata_noNA$enxb),na.rm=TRUE)*0.9
    maxx=max(c(arrowdata_noNA$stxb,arrowdata_noNA$enxb),na.rm=TRUE)*1.1
    miny=min(c(arrowdata_noNA$styb,arrowdata_noNA$enyb),na.rm=TRUE)*0.9
    maxy=max(c(arrowdata_noNA$styb,arrowdata_noNA$enyb),na.rm=TRUE)*1.1
   }
  }
  pp=ggplot(data=arrowdata_noNA,aes(x=stxl,y=styl,colour=clrl,fill=clrl))+xlab(xtxt)+ylab(ytxt)+xlim(c(minx,maxx))+ylim(c(miny,maxy*1.05))+ggtitle(arrowstxt)	#Set up plot
  pp=pp+geom_point(aes(x=stxl,y=styl,colour=clrl))+geom_point(aes(x=enxl,y=enyl,colour=clrl))	#Apply start and end points (although they get taken off by the geom_segment command below)
  if (showhull && newhull) {
   pp=pp+stat_bag(prop=propinhull,aes(x=stxl,y=styl,colour=clrl,fill=arrowdata_noNA$clrl),alpha=0.2,lty=2,lwd=1.5)#Plus the same with (prop=0.5,alpha=0.5) and then (prop=0.1,alpha=0.8) to have nested hulls
   pp=pp+stat_bag(prop=propinhull,aes(x=enxl,y=enyl,colour=clrl,fill=arrowdata_noNA$clrl),alpha=0.5,lty=1,lwd=1.5)
  }
  pp=pp+
#    geom_segment(data=arrowdata_noNA,mapping=aes(x=stxb,y=styb,xend=enxb,yend=enyb),arrow=arrow(length=unit(arrowdata_noNA$arrhs,"cm"),type="closed",angle=20),size=1.5,alpha=1.0,col="black",fill=arrowdata_noNA$clrl,linejoin="mitre")+
    geom_segment(data=arrowdata_noNA,mapping=aes(x=stxb,y=styb,xend=enxb,yend=enyb),arrow=arrow(length=unit(arrowdata_noNA$arrhs,"cm"),type="closed",angle=20),size=1.5,alpha=1.0,col=arrowdata_noNA$clrl,linejoin="mitre")+	#angle=40 is not the angle of the arrow but the steepness of the drawn arrowhead
    geom_text(data=arrowdata_noNA,aes(x=newxb,y=newyb,col=clrl),label=arrowdata_noNA$Region,size=4,col=arrowdata_noNA$clrl)+
    scale_colour_manual(name="Region",values=blanks,breaks=as.character(arrowdata_noNA$clrl),limits=as.character(arrowdata_noNA$clrl),labels=as.character(arrowdata_noNA$Region))+
    scale_fill_manual(name="Region",values=as.character(arrowdata_noNA$clrl),breaks=as.character(arrowdata_noNA$clrl),limits=as.character(arrowdata_noNA$clrl),labels=as.character(arrowdata_noNA$Region))+
#     geom_point(data=arrowdata_noNA,mapping=aes(x=stxb,y=styb),size=2.5,shape=21,fill="black",stroke=2,col=arrowdata_noNA$clrl)+
    geom_point(data=arrowdata_noNA,mapping=aes(x=stxb,y=styb),size=1.5,shape=21,fill=arrowdata_noNA$clrl,stroke=2,col=arrowdata_noNA$clrl)
  if (arrloop==1 && littlearrows) {
   pp=pp+geom_segment(data=arrowdata_noNA,mapping=aes(x=stxl,y=styl,xend=enxl,yend=enyl),color=arrowdata_noNA$clrl,arrow=arrow(length=unit(0.2,"cm")),size=0.3,alpha=0.5,show.legend=FALSE)
  }
  if (arrloop==2 && ggplotworks && littlearrows) {	#Not sure whether this will still work because I should really merge the df3inset data into arrowdata ...
   stx1=arrowstartx*multiplierx;sty1=arrowstarty*multipliery;enxCF=xvaluesCF*multiplierx;enyCF=yvaluesCF*multipliery;enxF=xvaluesF*multiplierx;enyF=yvaluesF*multipliery
   df3inset=data.frame(stx1,sty1,enxCF,enyCF,enxF,enyF)
   df3inset_noNA=remNAs(df3inset)
   if (!is.null(df3inset_noNA)) {
    pp=pp+geom_segment(data=df3inset_noNA,mapping=aes(x=stx1,y=sty1,xend=enxCF,yend=enyCF),color="blue3",arrow=arrow(length=unit(0.2,"cm")),size=0.1,show.legend=FALSE)+
     geom_segment(data=df3inset_noNA,mapping=aes(x=stx1,y=sty1,xend=enxF,yend=enyF),color="peru",arrow=arrow(length=unit(0.2,"cm")),size=0.1,show.legend=FALSE)
   }
  }
  if (plotprobs) {
   pp=pp+geom_hline(yintercept=0,linetype="dashed",color="grey4",size=1)
   pp=pp+geom_hline(yintercept=1,linetype="dashed",color="grey4",size=1)
   pp=pp+geom_vline(xintercept=0,linetype="dashed",color="grey4",size=1)
   pp=pp+geom_vline(xintercept=1,linetype="dashed",color="grey4",size=1)
  } else {
   if (choice8x==10 || choice8x==44 || choice8x==46 || choice8x==47) {
#    abline(v=0,lty=2,lwd=2)	#Precip or SPI on x
    pp=pp+geom_vline(xintercept=0,linetype="dashed",color="grey4",size=1)
   }
   if (choice8y==10 || choice8y==44 || choice8y==46 || choice8y==47) {
#    abline(h=0,lty=2,lwd=2)}	#Precip on y
    pp=pp+geom_hline(yintercept=0,linetype="dashed",color="grey4",size=1)
   }
   if (choice8x==52) {
#    abline(v=30,lty=2,lwd=2)	#ET on x
    pp=pp+geom_vline(xintercept=30,linetype="dashed",color="grey4",size=1)
   }
   if (choice8x==54) {
#    abline(v=-30,lty=2,lwd=2)	#-ET on x
    pp=pp+geom_vline(xintercept=-30,linetype="dashed",color="grey4",size=1)
   }
   if (choice8y==52) {
#    abline(h=30,lty=2,lwd=2)	#ET on y
    pp=pp+geom_hline(yintercept=30,linetype="dashed",color="grey4",size=1)
   }
   if (choice8y==54) {
#    abline(h=-30,lty=2,lwd=2)	#-ET on y
    pp=pp+geom_hline(yintercept=-30,linetype="dashed",color="grey4",size=1)
   }
  }
