##FUNCIÓ PER FER PCAs (DOS COMPONENTS)
#x = matriu de dades
#colors =  colors que hagis definit (normalment en el targets)
#dataDesc = títol que vols posar al gràfic
#labels =  noms de les mostres (normalment serà el shortname del targets)
#formapunts = forma que vols que tinguin els punts al gràfic
#myCex = tamany dels punts

plotPCA <- function ( X, labels=NULL, colors=NULL, dataDesc="", formapunts=NULL, myCex=NULL,...)
{
  pcX<-prcomp(t(X))
  loads<- round(pcX$sdev^2/sum(pcX$sdev^2)*100,1)
  xlab<-c(paste("PC1",loads[1],"%"))
  ylab<-c(paste("PC2",loads[2],"%"))
  if (is.null(colors)) colors=colores
  plot(pcX$x[,1:2],xlab=xlab,ylab=ylab,
       xlim=c(min(pcX$x[,1])-5,max(pcX$x[,1])+5),pch=formapunts, col=colors)
  text(pcX$x[,1],pcX$x[,2],labels,pos=3,cex=myCex, col=colors)
  title(paste(var, dataDesc, sep=" "), cex=0.2)
}

#Exemple
#plotPCA(exprs(rawData), colors=colores, dataDesc="Principal Component analysis",
#        var="",labels=sampleNames,myCex=0.6,formapunts = forma2pca)

#legend("bottomright", c("ISC.IP","ISC.CL","SHAM.IP","SHAM.CL"), 
#       col=c("darkblue","lightblue","darkgreen","lightgreen"), pch=c(15,16,17,18),
#       cex=0.6)