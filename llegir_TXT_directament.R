
#Llegim les dades tots els fitxers csv, donant per fet que segueixen un mateix format, i les guardem en una matriu on a cada columna hi aniran els Cts d'una mostra diferent
## Tenim counts per mostra en fitxers separats, que cal llegir i ajuntar en una mateixa taula
## Llegir tots les mostres en serie:

for (i in 1:length(list.files(path = dataDir))){
  print(list.files(path = dataDir)[i])
  data <- read.table(file=file.path(dataDir, list.files(path = dataDir)[i]), skip = 2, 
                     header=TRUE, sep=",", dec=".")
  #data <- data[order(data$Well),]
  if (i==1){
    Cts <- as.matrix(data$Ct)
    rownames(Cts) <- data$Well
    colnames(Cts) <- substr(list.files(path = dataDir)[i], 1, 5)
    print(tail(Cts))
  }
  else{
    Cts <- cbind(Cts, as.matrix(data$Ct))
    colnames(Cts)[i] <- substr(list.files(path = dataDir)[i], 1, 5)
    print(tail(Cts))
  }
}
dim(Cts)
