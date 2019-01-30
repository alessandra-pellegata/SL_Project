# Pulizia dataset

Met <- read.csv("meteorite-landings.csv")

#Rimozione osservazioni con dati mancanti
data <- Met[complete.cases(Met),]

#Rimozione osservazioni precedenti al 1900 e 
#successive al 2012 (poiché incomplete)
data2 = data[which(data$year >= 1900 & data$year <= 2012 ),]

#Cambiamento tipo variabile
data2$year <- as.numeric(data2$year)

#Rimozione osservazioni con coordinate geografiche mancanti
#e/o massa nulla.
data3<-data2[-which(data2$reclat==0),]
data4<-data3[-which(data3$mass==0),]

#Creazione variabile "colors" per distinzione tipo di meteorite
#(fell o found)
data4$colors<-ifelse(data4$fall=="Fell","darkred","forestgreen")

#Alcuni meteoriti, divisi in frammenti, risultano essere situati nelle stesse coordinate
#geografiche, o molto vicini. Si decide di rimuovere le osservazioni di questo tipo tenendo
#solo quella con massa maggiore, e tenendo un conteggio del numero di frammenti.

#Analisi Asuka (1505 - 1509)
indAsuka <- which(substr(data4$name, 0, 5)=="Asuka")
Asuka <- data4[indAsuka,]
ind_big <- which.max(Asuka$mass)
Asuka_big <- Asuka[ind_big,]

#Analisi Meteorite Hills (657 - 1130)
data4[which(data4$name=="Meteorite Hills 001005"),]
indMeteoriteHills <- which(substr(data4$name, 0, 15)=="Meteorite Hills")
MeteoriteHills <- data4[indMeteoriteHills,]
ind_big <- which.max(MeteoriteHills$mass)
MeteoriteHills_big <- MeteoriteHills[ind_big,]

#Analisi Elephant Moraine (539)
data4[which(data4$name=="Elephant Moraine 82608"),]
indElephant <- which(substr(data4$name, 0, 16)=="Elephant Moraine")
Elephant <- data4[indElephant,]
ind_big <- which.max(Elephant$mass)
Elephant_big <- Elephant[ind_big,]

#Analisi Allan Hills (637)
data4[which(data4$name=="Allan Hills 83100"),]
indAlan <- which(substr(data4$name, 0, 11)=="Allan Hills")
Alan <- data4[indAlan,]
ind_big <- which.max(Alan$mass)
Alan_big <- Alan[ind_big,]

#Queen Alexandra Range (3040)
data4[which(data4$name=="Queen Alexandra Range 02105"),]
indQueen <- which(substr(data4$name, 0, 21)=="Queen Alexandra Range")
Queen <- data4[indQueen,]
ind_big <- which.max(Queen$mass)
Queen_big <- Queen[ind_big,]

#Yamato (4760)
data4[which(data4$name=="Yamato 790388"),]
indYamato <- which(substr(data4$name, 0, 6)=="Yamato")
Yamato <- data4[indYamato,]
ind_big <- which.max(Yamato$mass)
Yamato_big <- Yamato[ind_big,]
summary(Yamato)

#Aggiungiamo una variabile che conta il numero di frammenti
Alan_big$numframmenti <- length(indAlan)
Asuka_big$numframmenti <- length(indAsuka)
Elephant_big$numframmenti <- length(indElephant)
MeteoriteHills_big$numframmenti <- length(indMeteoriteHills)
Queen_big$numframmenti <- length(indQueen)
Yamato_big$numframmenti <- length(indYamato)

#Creazione nuovo dataset
data5 <- data4[-c(indAlan,indAsuka,indElephant,indMeteoriteHills,indQueen,indYamato),]
data5$numframmenti <- NA
data5 <- rbind(data5, Alan_big, Asuka_big, Elephant_big, MeteoriteHills_big,
               Queen_big, Yamato_big)

#La variabile recclass è una variabile di tipo Factor con un numero elevato di livelli.
#Raggruppiamo i meteoriti con composizione simile creando delle macroclassi.

OrdinarioH<-c("H", "H3", "H3.2", "H3.3", "H3.4", "H3.5", "H3.6", "H3.7", "H3.8", "H3.9", "H3-4", "H3-5", "H3-6", "H4", "H4-5", "H4-6", "H5", "H5-6", "H6", "H7", "H / L", 
              "H3/2", "H3/3", "H3/4", "H3/5", "H3/6", "H3/7", "H3/8", "H3/9", "H4/5", "H4/6", "H5/6", "H4-an","H(5?)","H/L5","H3.4/3.5","H~4","H5-melt breccia",
              "H-melt rock","H6-melt breccia","H~5","H~6","H(L)3-an","H(L)3","H-imp melt","H/L3","H/L3.5","H-an","H3.9-6","H?","H3-an","H5-an","H3.8-5",
              "H3.9-5","H3.9/4","H3.8/4","H3.6-6","H5 ","H4(?)","H4 ","H3.8-an","H3.8/3.9","H3.8-6","H3.8-4","H3.7/3.8","H3.7-5","H3.4-5","H3.2-3.7","H3.10","H3.0-3.4","H3 ","H~4/5",
              "H/L6", "H/L4-5", "H/L4","H/L~4","H-metal","H6 ")
OrdinarioL<-c("T","L", "L3", "L3.0", "L3.1", "L3.2", "L3.3", "L3.4", "L3.5", "L3.6", "L3.7", "L3.8", "L3.9", "L3-4", "L3-5", "L3-6", "L4", "L4-5", "L4-6", "L5", "L5-6", "L6", "L6-7", "L7", "L / LL3", "L / LL3-5", "L / LL3-6", "L / LL4", "L / LL5", "L / LL5-6", "L / LL6",
              "L-imp melt","L-melt breccia","L-melt rock","L-metal","L(?)3",
              "L(LL)3","L(LL)3.5-3.7","L(LL)5","L(LL)6","L/LL(?)3","L/LL~4","L/LL~5","L/LL~6","L/LL3","L/LL3-5","L/LL3-6",
              "L/LL3.10","L/LL3.4","L/LL3.6/3.7","L/LL4","L/LL4-6","L/LL4/5","L/LL5","L/LL5-6","L/LL5/6","L/LL6",
              "L/LL6-an","L~3","L~4","L~5","L~6","L3.0-3.7","L3.0-3.9","L3.2-3.5",
              "L3.5-3.9","L3.5-5","L3.6-4","L3.7-6","L3.7/3.8","L3.8-6","L3.8-an","L3.9-5","L3.9-6","L3.9/4","L3/4","L4/5","L4 ",
              "L5-7","L5 ","L5/6","L6-melt breccia","L6 ","L6/7","L3.5-3.8","L3.3-3.5")
OrdinarioLL<-c("LL", "LL3", "LL3.0", "LL3.1", "LL3.2", "LL3.3", "LL3.4", "LL3.5", "LL3.6", "LL3.7", "LL3.8", "LL3.9", "LL3-6", "LL4", "LL4-5", "LL4-6", "LL5", "LL5-6", "LL5-7", "LL6", "LL7",
               "LL(L)3","LL~3","LL~5","LL~6","LL3-5","LL3.00","LL3.1-3.5","LL3.7-6","LL3.8-6","LL3.9/4","LL3/4","LL4/5","LL5/6","LL6 ")

CarboniosoM<-c("CM1", "CM1-2", "CM2","CM-an")
CarboniosoR<-c("CR", "CR2","CR-an","CR1","CR2-an")
CarboniosoCKOV<-c("CK", "CK3", "CK4", "CK4-5", "CK5", "CK5-6", "CK6","CK3-an","CK4-an","CK4/5","CO3","CO3.0","CO3.2","CO3.3","CO3.5","CO3.6","CO3.8",
                  "CV2", "CV3", "CV3.0", "CV3.2", "CV3.3","CV3-an")
CarboniosoCH<-c("CH","CH/CBb","CH3")
Carbonioso<-c("C","C2-ung","C3-ung","C4-ung","C5/6-ung","CBa","CBb","CI1",CarboniosoCH,CarboniosoCKOV,CarboniosoM,CarboniosoR)

Entasite<-c("E","E3","E3-an","E4","EH", "EH3", "EH4", "EH4-5", "EH5", "EH6","EH-imp melt","EH3/4-an",
            "EH4/5","EH7-an","EL", "EL3", "EL4-5", "EL5", "EL6","EL4","EL4/5","EL6/7","EL7")

AltreCondriti<-c( "K","K3","Chondrite-fusion crust","Chondrite-ung","OC",
             "R", "R3.8", "R3-4", "R3-5", "R3-6", "R4","R3","R3.5-6","R3.6","R3.8-5","R3.8-6","R5")

AcondritiVarie<-c("Eucrite","Eucrite-an","Eucrite-br","Eucrite-cm","Eucrite-Mg rich","Eucrite-mmict","Eucrite-pmict","Eucrite-unbr","Acapulcoite","Acapulcoite/Lodranite","Aubrite","Aubrite-an","Ureilite","Ureilite-an","Ureilite-pmict",
             "Diogenite","Diogenite-an","Diogenite-olivine","Diogenite-pm","Achondrite-ung","Angrite","Brachinite","Howardite","Winonaite","Lodranite","Enst achon-ung")
Lunar<-c("Lunar","Lunar (anorth)","Lunar (bas. breccia)","Lunar (bas/anor)","Lunar (basalt)","Lunar (feldsp. breccia)","Lunar (norite)")
Martian<-c("Martian (nakhlite)","Martian (shergottite)")


Sideroliti<-c("Mesosiderite","Mesosiderite-A","Mesosiderite-A1","Mesosiderite-A2","Mesosiderite-A3","Mesosiderite-A4","Mesosiderite-an","Mesosiderite-B1","Mesosiderite-B4",
              "Mesosiderite-C","Mesosiderite-C2","Mesosiderite?","Pallasite","Pallasite, PES","Pallasite, PMG","Pallasite, PMG-an","Pallasite, ungrouped")

data5$class<-NA
for (i in 1:nrow(data5)){
  if (is.element(data5$recclass[i],OrdinarioH))(data5$class[i]<-"OrdinarieH")
  else if (is.element(data5$recclass[i],OrdinarioL))(data5$class[i]<-"OrdinarieL")
  else if (is.element(data5$recclass[i],OrdinarioLL))(data5$class[i]<-"OrdinarieLL")
  else if (is.element(data5$recclass[i],Martian))(data5$class[i]<-"Martian")
  else if (is.element(data5$recclass[i],Sideroliti))(data5$class[i]<-"Sideroliti")
  else if (is.element(data5$recclass[i],Lunar))(data5$class[i]<-"Lunar")
  else if (substr(data5$recclass[i],0,4)=="Iron")(data5$class[i]<-"Sideriti")
  else if (is.element(data5$recclass[i],Entasite))(data5$class[i]<-"Entasiti")
  else if (is.element(data5$recclass[i],Carbonioso))(data5$class[i]<-"Carboniose")
  else if (is.element(data5$recclass[i],AltreCondriti))(data5$class[i]<-"AltreCondriti")
  else if (is.element(data5$recclass[i],AcondritiVarie))(data5$class[i]<-"AcondritiVarie")
  else (data5$class[i]<-as.character(data5$recclass[i]))
}
data5$class<-as.factor(data5$class)

#Assegnazione di un colore diverso ad ogni tipo di meteorite

pal <- colorNumeric(c("red", "green", "blue","pink"), 1:12)
data5$colorsMat<-NA
for (i in 1:nrow(data5)){
  if (data5$class[i]=="AcondritiVarie"){data5$colorsMat[i]<-pal(1)}
  else if(data5$class[i]=="Carboniose"){data5$colorsMat[i]<-pal(2)}
  else if(data5$class[i]=="AltreCondriti"){data5$colorsMat[i]<-pal(3)}
  else if(data5$class[i]=="Entasiti"){data5$colorsMat[i]<-pal(4)}
  else if(data5$class[i]=="Sideriti"){data5$colorsMat[i]<-pal(5)}
  else if(data5$class[i]=="Lunar"){data5$colorsMat[i]<-pal(6)}
  else if(data5$class[i]=="Martian"){data5$colorsMat[i]<-pal(7)}
  else if(data5$class[i]=="OrdinarieH"){data5$colorsMat[i]<-pal(8)}
  else if(data5$class[i]=="OrdinarieL"){data5$colorsMat[i]<-pal(9)}
  else if(data5$class[i]=="OrdinarieLL"){data5$colorsMat[i]<-pal(10)}
  else if(data5$class[i]=="Sideroliti"){data5$colorsMat[i]<-pal(11)}
  else if(data5$class[i]=="Stone-uncl"){data5$colorsMat[i]<-pal(12)}
}

#Divisione della variabile massa in classi

data5$numMass<-NA
summary(data5$mass)
for ( i in 1:nrow(data5)){
  if (data5$mass[i] <= 1000){
    data5$numMass[i]=1
  }
  else if  (data5$mass[i] > 1000 & data5$mass[i]<=5000){
    data5$numMass[i]=2
  }
  else if  (data5$mass[i] > 5000 & data5$mass[i]<=10000){
    data5$numMass[i]=3
  }
  else if  (data5$mass[i] > 10000 & data5$mass[i]<=20000){
    data5$numMass[i]=4
  }
  else if  (data5$mass[i] > 20000 & data5$mass[i]<=100000){
    data5$numMass[i]=5
  }
  else if  (data5$mass[i] > 100000){
    data5$numMass[i]=6
  }
}

data5$macroclass <- NA
Condriti <- c("OrdinarieH", "OrdinarieL", "OrdinarieLL", "Carboniose", "Entasiti", "AltreCondriti")
Acondriti <- c("AcondritiVarie", "Lunar", "Martian")

for (i in 1:nrow(data5)){
  if (is.element(data5$class[i],Condriti))(data5$macroclass[i]<-"Condriti")
  else if (is.element(data5$class[i],Acondriti))(data5$macroclass[i]<-"Acondriti")
  else (data5$macroclass[i]<-as.character(data5$class[i]))
}
data5$macroclass <- as.factor(data5$macroclass)
levels(data5$macroclass)


#Esportazione dataset
write.csv(data5, "meteorite_definitivo.csv")
