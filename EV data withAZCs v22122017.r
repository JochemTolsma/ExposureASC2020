#rm (list = ls( ))
# Load required R packages 
require(XML)
require(foreign)
require(sp)
require(maptools)
require(rgdal)
require(splancs)
require(spatstat)
require(RColorBrewer)
require(spdep)
require(raster)
require(reshape)
require(stringr)
require(psych)
require(lme4)
require(sjPlot)
require(plyr)
require(Hmisc)
require(rgeos)
require(anesrake)
require(weights)

#CONTENT
#1. Data AZCS - pc4 en gc
#2. Data other characteristics - pc4 and gc
#3. EV data
#4. merge EV data with other data 
#5. EV data - make  indv level vars


#FOR READ in DATA
#setwd("C:/Users/U497130/AppData/Local/surfdrive_data/artikel azc/data/")
#thuis
#setwd("C:/Users/Joran/SURFdrive/artikel azc/data/")
setwd("U:/Surfdrive backup/artikel azc/data/")

#COMBINE REGULAR AND CRISIS CENTRE AZC DATA
#regulier en nood
regnood <- read.csv2(file="Adressenlijst opvanglocaties jan 2015_16082016.csv")
head(regnood)
#Landal:https://www.landaleigenaren.nl/nl-nl/nieuws/landal-levert-bijdrage-aan-vluchtelingenopvang-14659

#crisis tussen 18/09 en 22/12
crisis <- read.csv2(file="Adressen opvanglocatie CNO capaciteit_16082016.csv")
head(crisis)

#samenvoegen azcs
rbind.all.columns <- function(x, y) {
 
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
 
    x[, c(as.character(y.diff))] <- NA
 
    y[, c(as.character(x.diff))] <- NA
 
    return(rbind(x, y))
}

azc <- rbind.all.columns(regnood, crisis)
head(azc)

#KOPPEL GCODE AAN AZC. 
pc4gc_2013 <- read.csv2(file="pc4gc_2013.csv", row.names="X")
head(pc4gc_2013)
azc<- merge(azc, pc4gc_2013, all.x=T, by.x="PC4", by.y="pc4", sort=F)
head(azc)

#make azc vars
azc$s1cap_pc4t1 <- ifelse(azc$Soort==1, azc$Cap_01012015, 0)
azc$s1cap_pc4t2 <- ifelse(azc$Soort==1, azc$Cap_15112015, 0)

azc$s2cap_pc4t1 <- ifelse(azc$Soort==2, azc$Cap_01012015, 0)
azc$s2cap_pc4t2 <- ifelse(azc$Soort==2, azc$Cap_15112015, 0)

azc$s3cap_pc4t1 <- 0
azc$s3cap_pc4t2 <- ifelse(azc$Soort==3, azc$Cap_Gem, 0)

head(azc)

#Handle pc4 with 2 or more azc (beslisregels:Stappen AZC data 15032017.doc)
dups <- rbind(azc[duplicated(azc$PC4),],azc[duplicated(azc$PC4, fromLast=TRUE),])
dups <- dups[order(dups$PC4),]
dups[,c(1,2,13:18)]
azc_sel <- azc[,c(1,12,13:18)]
azc_sel <- azc_sel[rownames(azc_sel)!="153",]
azc_sel <- azc_sel[rownames(azc_sel)!="29",]
azc_sel$s3cap_pc4t2[rownames(azc_sel)=="30"] <- azc_sel$s3cap_pc4t2[rownames(azc_sel)=="31"]
azc_sel <- azc_sel[rownames(azc_sel)!="31",]
azc_sel$s3cap_pc4t2[rownames(azc_sel)=="171"] <- azc_sel$s3cap_pc4t2[rownames(azc_sel)=="171"]+azc_sel$s3cap_pc4t2[rownames(azc_sel)=="172"]
azc_sel <- azc_sel[rownames(azc_sel)!="172",]
azc_sel$s3cap_pc4t2[rownames(azc_sel)=="40"] <- azc_sel$s3cap_pc4t2[rownames(azc_sel)=="39"]
azc_sel <- azc_sel[rownames(azc_sel)!="39",]
azc_sel$s1cap_pc4t2[rownames(azc_sel)=="47"] <- azc_sel$s1cap_pc4t2[rownames(azc_sel)=="47"] + azc_sel$s3cap_pc4t2[rownames(azc_sel)=="48"] 
azc_sel <- azc_sel[rownames(azc_sel)!="48",]
azc_sel$s2cap_pc4t2[rownames(azc_sel)=="75"] <- azc_sel$s2cap_pc4t2[rownames(azc_sel)=="76"] 
azc_sel <- azc_sel[rownames(azc_sel)!="76",]
azc_sel <- azc_sel[rownames(azc_sel)!="80",]
azc_sel$s1cap_pc4t1[rownames(azc_sel)=="87"] <- azc_sel$s1cap_pc4t1[rownames(azc_sel)=="87"] + azc_sel$s1cap_pc4t1[rownames(azc_sel)=="88"]
azc_sel$s1cap_pc4t2[rownames(azc_sel)=="87"] <- azc_sel$s1cap_pc4t2[rownames(azc_sel)=="87"] + azc_sel$s1cap_pc4t2[rownames(azc_sel)=="88"]
azc_sel <- azc_sel[rownames(azc_sel)!="88",]
azc_sel$s1cap_pc4t2[rownames(azc_sel)=="92"] <- azc_sel$s1cap_pc4t2[rownames(azc_sel)=="92"] + azc_sel$s3cap_pc4t2[rownames(azc_sel)=="93"]
azc_sel <- azc_sel[rownames(azc_sel)!="93",]


#AGGREGEER NAAR GC. 
azcgc <- aggregate(azc_sel[,c("s1cap_pc4t1", "s1cap_pc4t2", "s2cap_pc4t1","s2cap_pc4t2","s3cap_pc4t1","s3cap_pc4t2")], by=list(azc_sel$gc), FUN=sum)
names(azcgc) <- c("gc","s1cap_gct1", "s1cap_gct2", "s2cap_gct1","s2cap_gct2","s3cap_gct1","s3cap_gct2")
head(azcgc)


#MATCH AZC DATA aan PC4 FILE

#Load PC4 shape
shapepc4 <- readOGR("U:/PhD Documents/PC4Basic/shape", layer="Nlp4_r14_basic")
#thuis
#shapepc4 <- readOGR("C:/Users/Joran/Documents/PhD Nijmegen/PC4Basic/shape", layer="Nlp4_r14_basic")
shapepc4 <- shapepc4[, c("PC4NR")]

pc4 <- merge(shapepc4, pc4gc_2013, all.x=T, by.x="PC4NR", by.y="pc4", sort=F)
pc4 <- merge(pc4, azc_sel, by.x=c("PC4NR", "gc"), by.y=c("PC4","gc"), sort=F) 
pc4 <- merge(pc4, azcgc, by.x=c("gc"), by.y=c("gc"), sort=F) 

pc4@data[,c(3:14)][is.na(pc4@data[,c(3:14)])] <- 0


#AGGREGEER NAAR AANGR
pc4_nbq<-poly2nb(pc4, row.names = c(1:nrow(pc4)))

pc4$s3cap_aangrt2  <- pc4$s3cap_aangrt1 <- pc4$s2cap_aangrt2 <- pc4$s2cap_aangrt1 <- pc4$s1cap_aangrt2 <- pc4$s1cap_aangrt1 <- rep(NA, nrow(pc4))

funs1t1 <- function(x) { sum(pc4$s1cap_pc4t1[x]) }
funs1t2 <- function(x) { sum(pc4$s1cap_pc4t2[x]) }
funs2t1 <- function(x) { sum(pc4$s2cap_pc4t1[x]) }
funs2t2 <- function(x) { sum(pc4$s2cap_pc4t2[x]) }
funs3t1 <- function(x) { sum(pc4$s3cap_pc4t1[x]) }
funs3t2 <- function(x) { sum(pc4$s3cap_pc4t2[x]) }

pc4$s1cap_aangrt1 <- sapply(pc4_nbq, funs1t1) 
pc4$s1cap_aangrt2 <- sapply(pc4_nbq, funs1t2) 
pc4$s2cap_aangrt1 <- sapply(pc4_nbq, funs2t1) 
pc4$s2cap_aangrt2 <- sapply(pc4_nbq, funs2t2) 
pc4$s3cap_aangrt1 <- sapply(pc4_nbq, funs3t1) 
pc4$s3cap_aangrt2 <- sapply(pc4_nbq, funs3t2) 

#plus eigen buurt er ook bij
pc4$s1cap_aangrt1 <- pc4$s1cap_aangrt1 + pc4$s1cap_pc4t1
pc4$s1cap_aangrt2 <- pc4$s1cap_aangrt2 + pc4$s1cap_pc4t2
pc4$s2cap_aangrt1 <- pc4$s2cap_aangrt1 + pc4$s2cap_pc4t1
pc4$s2cap_aangrt2 <- pc4$s2cap_aangrt2 + pc4$s2cap_pc4t2
pc4$s3cap_aangrt1 <- pc4$s3cap_aangrt1 + pc4$s3cap_pc4t1
pc4$s3cap_aangrt2 <- pc4$s3cap_aangrt2 + pc4$s3cap_pc4t2


#ADD OTHER NBHOOD CHARACTERISTICS

#LOAD %NONWEST, WOZ, INW
#alternatief via spTransform
rd2wgs84 <- function(X, Y)
{
#http://www.dekoepel.nl/pdf/Transformatieformules.pdf
#basispunten definieren
X0 <- 155000.00 
Y0 <- 463000.00 
j0 <- 52.15517440
l0 <- 5.38720621
#coefficienten definieren
K01<- 3235.65389 
K20<- -32.58297
K02<- -0.24750 
K21<- -0.84978 
K03<- -0.06550 
K22<- -0.01709 
K10<- -0.00738 
K40<- 0.00530 
K23<- -0.00039 
K41<- 0.00033 
K11<- -0.00012 

L10<- 5260.52916
L11<- 105.94684
L12<- 2.45656
L30<- -0.81885
L13<- 0.05594
L31<- -0.05607
L01<- 0.01199
L32<- -0.00256
L14<- 0.00128
L02<- 0.00022
L20<- -0.00022
L50<- 0.00026

dX <- (X - X0)*10^-5
dY <- (Y - Y0)*10^-5 
	{
	j <- j0 + 
	(
	K01*dX^0*dY^1 +
	K02*dX^0*dY^2 +
	K03*dX^0*dY^3 +
	K10*dX^1*dY^0 +
	K20*dX^2*dY^0 +
	K21*dX^2*dY^1 +
	K22*dX^2*dY^2 +
	K23*dX^1*dY^3 +
	K40*dX^2*dY^0 +
	K41*dX^2*dY^1 
	)/3600
	}

	{
	l <- l0 + 
	(
	L10*dX^1*dY^0 +
	L11*dX^1*dY^1 +
	L12*dX^1*dY^2 +
	L30*dX^3*dY^0 +
	L13*dX^1*dY^3 +
	L31*dX^3*dY^1 +
	L01*dX^0*dY^1 +
	L32*dX^3*dY^2 +
	L14*dX^1*dY^4 +
	L02*dX^0*dY^2 +
	L20*dX^2*dY^0 +
	L50*dX^5*dY^0 
	)/3600
	}
wgs84<-cbind(j,l)
return(wgs84)
}

#load cbs100
setwd("U:/PhD Documents/CBS vierkant 2014/")
#Thuis
#setwd("C:/Users/Joran/Documents/PhD Nijmegen/CBS vierkant 2014/") 

cbs100<- read.spss(file="CBSvierkant100m201410.sav",use.value.labels=FALSE, to.data.frame=T)
names(cbs100)
length(cbs100[,1])

cbs100$E <- as.numeric(substr(cbs100$c28992r100, 2, 5))
cbs100$N <- as.numeric(substr(cbs100$c28992r100, 7, length(cbs100$c28992r100)))
cbs100$east<-(cbs100$E*100)+50
cbs100$north<-(cbs100$N*100)+50
cbs100<- cbind(cbs100, rd2wgs84(cbs100$east, cbs100$north))
cbs100$ncel<-1
cbs100$celid <- 1:length(cbs100$ncel)
head(cbs100)

#RECODE CBS100 OP BASIS VAN DISTRIBUTIE IN BUURTEN.
#2014
nedb <- readOGR("U:/PhD Documents/CBS shapefiles/shape 2014", layer="buurt_2014")
#thuis
#nedb <- readOGR("C:/Users/Joran/Documents/PhD Nijmegen/CBS shapefiles/shape 2014", layer="buurt_2014")

nedb <- nedb[nedb$WATER=="NEE",]
names(nedb)

#alleen land delen.
nedb2 <- nedb[nedb$WATER=="NEE",]
nedb2$P_N_W_AL  <- ifelse(nedb2$P_N_W_AL==-99999997,0,nedb2$P_N_W_AL)
nedb2$P_WEST_AL  <- ifelse(nedb2$P_WEST_AL==-99999997,0,nedb2$P_WEST_AL)
nedb2$P_AUT  <- 100 - nedb2$P_N_W_AL  -  nedb2$P_WEST_AL 

#volgens cbs handleiding
#5: geen autochtonen
#4: 90 procent of meer autochtonen
#3: 75 tot 90 procent autochtonen
#2: 60 tot 75 procent autochtonen
#1: 40 tot 60 procent autochtonen
#7: minder dan 40 procent autochtonen

#5: geen westerse allochtonen
#3: 45 procent of meer westerse allochtonen
#2: 25 tot 45 procent westerse allochtonen
#1: 15 tot 25 procent westerse allochtonen
#4: 8 tot 15 procent westerse allochtonen
#7: minder dan 8 procent westerse allochtonen

#5: geen niet-westerse allochtonen
#4: 67 procent of meer niet-westerse allochtonen
#3: 45 tot 67 procent niet-westerse allochtonen
#2: 25 tot 45 procent niet-westerse allochtonen
#1: 10 tot 25 procent niet-westerse allochtonen
#7: minder dan 10 procent niet-westerse allochtonen

#2014
nedb2$P_AUT2[nedb2$P_AUT==0] <- 0
nedb2$P_AUT2[nedb2$P_AUT>=90] <- 1
nedb2$P_AUT2[nedb2$P_AUT>=75 & nedb2$P_AUT< 90] <- 2
nedb2$P_AUT2[nedb2$P_AUT>=60 & nedb2$P_AUT< 75] <- 3
nedb2$P_AUT2[nedb2$P_AUT>=40 & nedb2$P_AUT< 65] <- 4
nedb2$P_AUT2[nedb2$P_AUT>0 & nedb2$P_AUT< 40] <- 5

des <- describeBy(nedb2$P_AUT,nedb2$P_AUT2) 	
pautrecode <- matrix(as.numeric(unclass(unlist(des))), nrow=dim(des), byrow=T)
pautrecode <- data.frame(pautrecode)
colnames(pautrecode) <- names(des[[1]])
pautrecode$mean
# [1]  0.00000 97.37508 82.06138 69.92389 50.40706 23.88861

nedb2$P_WEST_AL2[nedb2$P_WEST_AL==0] <- 0
nedb2$P_WEST_AL2[nedb2$P_WEST_AL>=45] <- 1
nedb2$P_WEST_AL2[nedb2$P_WEST_AL>=25 & nedb2$P_WEST_AL< 45] <- 2
nedb2$P_WEST_AL2[nedb2$P_WEST_AL>=15 & nedb2$P_WEST_AL< 25] <- 3
nedb2$P_WEST_AL2[nedb2$P_WEST_AL>=8 & nedb2$P_WEST_AL< 15] <- 4
nedb2$P_WEST_AL2[nedb2$P_WEST_AL>0 & nedb2$P_WEST_AL< 8] <- 5

des <- describeBy(nedb2$P_WEST_AL,nedb2$P_WEST_AL2) 	
pwalrecode <- matrix(as.numeric(unclass(unlist(des))), nrow=dim(des), byrow=T)
pwalrecode <- data.frame(pwalrecode)
colnames(pwalrecode) <- names(des[[1]])
pwalrecode$mean
# [1]  0.000000 51.931746 31.865649 16.231917 10.027004  5.405292

nedb2$P_N_W_AL2[nedb2$P_N_W_AL==0] <- 0
nedb2$P_N_W_AL2[nedb2$P_N_W_AL>=67] <- 1
nedb2$P_N_W_AL2[nedb2$P_N_W_AL>=45 & nedb2$P_N_W_AL< 67] <- 2
nedb2$P_N_W_AL2[nedb2$P_N_W_AL>=25 & nedb2$P_N_W_AL< 45] <- 3
nedb2$P_N_W_AL2[nedb2$P_N_W_AL>=10 & nedb2$P_N_W_AL< 25] <- 4
nedb2$P_N_W_AL2[nedb2$P_N_W_AL>0 & nedb2$P_N_W_AL< 10] <- 5

des <- describeBy(nedb2$P_N_W_AL,nedb2$P_N_W_AL2) 	
pnwalrecode <- matrix(as.numeric(unclass(unlist(des))), nrow=dim(des), byrow=T)
pnwalrecode <- data.frame(pnwalrecode)
colnames(pnwalrecode) <- names(des[[1]])
pnwalrecode$mean
#[1]  0.000000 75.998914 53.030019 30.792600 15.197071  4.109998

#make numeric first
cbs100$p_auto2014 <- as.numeric(cbs100$p_auto2014)
cbs100$p_wal2014 <- as.numeric(cbs100$p_wal2014)
cbs100$p_nwal2014 <- as.numeric(cbs100$p_nwal2014)

#recategorize auto
cbs100$pauto2014[cbs100$p_auto2014==5] <- 0
cbs100$pauto2014[cbs100$p_auto2014==4] <- pautrecode$mean[1]
cbs100$pauto2014[cbs100$p_auto2014==3] <- pautrecode$mean[2]
cbs100$pauto2014[cbs100$p_auto2014==2] <- pautrecode$mean[3]
cbs100$pauto2014[cbs100$p_auto2014==1] <- pautrecode$mean[4]
cbs100$pauto2014[cbs100$p_auto2014==7] <- pautrecode$mean[5]
cbs100$nauto2014 <- cbs100$inw2014 * (cbs100$pauto2014/100)
cbs100$nauto2014[cbs100$inw2014==0] <- 0
cbs100$nauto2014b <- cbs100$nauto2014
cbs100$nauto2014b[is.na(cbs100$nauto2014b)] <- 2
cbs100$nauto2014mis <- ifelse(is.na(cbs100$nauto2014), 1, 0)
cbs100$nauto2014[is.na(cbs100$nauto2014)] <- 0
sum(cbs100$nauto2014, na.rm=T)

#recategorize wal
cbs100$pwal2014[cbs100$p_wal2014==5] <- pwalrecode$mean[1]
cbs100$pwal2014[cbs100$p_wal2014==3] <- pwalrecode$mean[2]
cbs100$pwal2014[cbs100$p_wal2014==2] <- pwalrecode$mean[3]
cbs100$pwal2014[cbs100$p_wal2014==1] <- pwalrecode$mean[4]
cbs100$pwal2014[cbs100$p_wal2014==4] <- pwalrecode$mean[5]
cbs100$pwal2014[cbs100$p_wal2014==7] <- pwalrecode$mean[6]
cbs100$nwal2014 <- cbs100$inw2014 * (cbs100$pwal2014/100)
cbs100$nwal2014[cbs100$inw2014==0] <- 0
cbs100$nwal2014b <- cbs100$nwal2014
cbs100$nwal2014b[is.na(cbs100$nwal2014b)] <- 2
cbs100$nwal2014mis <- ifelse(is.na(cbs100$nwal2014), 1, 0)
cbs100$nwal2014[is.na(cbs100$nwal2014)] <- 0
sum(cbs100$nwal2014, na.rm=T)

#recategorize nwal
cbs100$pnwal2014[cbs100$p_nwal2014==5] <- pnwalrecode$mean[1]
cbs100$pnwal2014[cbs100$p_nwal2014==4] <- pnwalrecode$mean[2]
cbs100$pnwal2014[cbs100$p_nwal2014==3] <- pnwalrecode$mean[3]
cbs100$pnwal2014[cbs100$p_nwal2014==2] <- pnwalrecode$mean[4]
cbs100$pnwal2014[cbs100$p_nwal2014==1] <- pnwalrecode$mean[5]
cbs100$pnwal2014[cbs100$p_nwal2014==7] <- pnwalrecode$mean[6]
cbs100$nnwal2014 <- cbs100$inw2014 * (cbs100$pnwal2014/100)
cbs100$nnwal2014[cbs100$inw2014==0] <- 0
cbs100$nnwal2014b <- cbs100$nnwal2014
cbs100$nnwal2014b[is.na(cbs100$nnwal2014b)] <- 2
cbs100$nnwal2014mis <- ifelse(is.na(cbs100$nnwal2014), 1, 0)
cbs100$nnwal2014[is.na(cbs100$nnwal2014)] <- 0
sum(cbs100$nnwal2014, na.rm=T)

#bevolking
ntotaal <- sum(cbs100$nauto2014, na.rm=T) + sum(cbs100$nwal2014, na.rm=T) + sum(cbs100$nnwal2014, na.rm=T)
ntotaalb <- sum(cbs100$nauto2014b, na.rm=T) + sum(cbs100$nwal2014b, na.rm=T) + sum(cbs100$nnwal2014b, na.rm=T)
sum(cbs100$nwal2014, na.rm=T) / ntotaal

###########################

#oppervlakte
oppv <- as.numeric(gArea(shapepc4, byid=T)/1000^2) #nu in vierkante km
#134km^2 voor postcode 8252 klopt
oppv <- as.data.frame(cbind(oppv, shapepc4$PC4NR))
oppv <- rename(oppv, c("V2"="PC4NR"))

pc4 <- merge(pc4, oppv, by="PC4NR")

#coordinaatstelsel omzetten pc4
shapepc4 <- spTransform(shapepc4,CRS("+proj=longlat +ellps=WGS84")) 
proj4string(shapepc4)

#gridcells voor elke PC4 selecteren
coordinates(cbs100) = c("l","j")
llCRS<-CRS("+proj=longlat +ellps=WGS84")
loc_g<-SpatialPoints(cbs100,proj4string=llCRS)
proj4string(loc_g)<-proj4string(shapepc4)
locg<-over(shapepc4, loc_g, returnList=T)

#2014
#select necessary data
popcounts <- cbs100[c("nauto2014", "nwal2014", "nnwal2014", "inw2014")]
popcounts$inw2014[popcounts$inw2014<0] <- NA

#calculate proportions for every pc4.
prop <- function(x) {
ifelse(cbind(length(x)!=0, length(x)!=0, length(x)!=0), colSums(as.data.frame(popcounts@data[x,1:3]))/sum(colSums(as.data.frame(popcounts@data[x,1:3]))),c(NA,NA,NA)) }
prop2 <- lapply(locg, prop)
PC4prop <-data.frame(matrix(unlist(prop2), nrow=length(locg),ncol=3, byrow=T))
colnames(PC4prop) <- c("pauto2014", "pwal2014",  "pnwal2014")

pc4prop <- cbind(PC4prop, shapepc4$PC4NR)
pc4prop <- rename(pc4prop, c("shapepc4$PC4NR"="PC4NR"))
head(pc4prop)

pc4 <- merge(pc4, pc4prop, by="PC4NR")

#calculate aantinw
inw <- function(x) {
ifelse(length(x)!=0, sum(as.data.frame(popcounts@data[x,4]),na.rm = TRUE),c(NA)) }
inw2 <- lapply(locg, inw)
PC4inw <-data.frame(matrix(unlist(inw2), nrow=length(locg),ncol=1, byrow=T))
colnames(PC4inw) <- c("inw2014")

pc4inw <- cbind(PC4inw, shapepc4$PC4NR)
pc4inw <- rename(pc4inw, c("shapepc4$PC4NR"="PC4NR"))
head(pc4inw)

pc4 <- merge(pc4, pc4inw, by="PC4NR")

#calculate weightedwoz
wozcounts <- cbs100[c("wozwon2012", "won2012")]
wozcounts$wozwon2012[wozcounts$wozwon2012<0] <- NA
wozcounts$won2012[wozcounts$won2012<0] <- NA

woz <- function(x) {
ifelse(length(x)!=0, sum(as.data.frame(wozcounts@data[x,1])*as.data.frame(wozcounts@data[x,2]),na.rm=T)/sum(as.data.frame(wozcounts@data[x,2]),na.rm=T),c(NA)) }
woz2 <- lapply(locg, woz)
PC4woz<-data.frame(matrix(unlist(woz2), nrow=length(locg),ncol=1, byrow=T))
colnames(PC4woz) <- c("woz2012")

pc4woz <- cbind(PC4woz, shapepc4$PC4NR)
pc4woz <- rename(pc4woz, c("shapepc4$PC4NR"="PC4NR"))
head(pc4woz)

pc4 <- merge(pc4, pc4woz, by="PC4NR") 

#add coordinates centroids pc4
pc4$l <- coordinates(shapepc4)[,1]
pc4$j <- coordinates(shapepc4)[,2]

head(pc4)

pc4$nnwal <- pc4$pnwal2014*pc4$inw2014


#AANGRENZENDE context data
names(pc4@data)[21:28] <- paste(names(pc4@data[,c(21:28)]),"pc4", sep = "_")

pc4$woz2012_aangr <- pc4$inw2014_aangr <- pc4$pnwal2014_aangr <- rep(NA, nrow(pc4))

funinw <- function(x) { sum(pc4$inw2014_pc4[x], na.rm=T)} 
funpnw <- function(x) { sum(pc4$pnwal2014_pc4[x] *  pc4$inw2014_pc4[x] , na.rm=T)/ sum(pc4$inw2014_pc4[x], na.rm=T) } 
funwoz <- function(x) { sum(pc4$woz2012_pc4[x], na.rm=T)/ length(pc4$woz2012_pc4[x]) } 
funlen <- function(x) { length(pc4$woz2012_pc4[x]) } 

pc4$inw2014_aangr <- sapply(pc4_nbq, funinw) 
pc4$pnwal2014_aangr <- sapply(pc4_nbq, funpnw) 
pc4$woz2012_aangr <- sapply(pc4_nbq, funwoz)
pc4$wozlength <- sapply(pc4_nbq, funlen)

#plus eigen buurt erbij
pc4$inw2014_aangr <- pc4$inw2014_aangr + pc4$inw2014_pc4
pc4$pnwal2014_aangr <- (pc4$pnwal2014_aangr*pc4$inw2014_aangr + pc4$pnwal2014_pc4*pc4$inw2014_pc4)/(pc4$inw2014_aangr+pc4$inw2014_pc4)
pc4$woz2012_aangr <- ((pc4$woz2012_aangr*pc4$wozlength) + pc4$woz2012_pc4)/(pc4$wozlength+1)

pc4 <- pc4[,c(1:ncol(pc4)-1)]

#GEMEENTES context data
nedg <- readOGR("U:/PhD Documents/CBS shapefiles/shape 2014", layer="gem_2014")
#thuis
#nedg <- readOGR("C:/Users/Joran/Documents/PhD Nijmegen/CBS shapefiles/shape 2014", layer="gem_2014")

nedg <- nedg[nedg$WATER=="NEE",]
gemeente<-as.character(nedg@data$GM_CODE)
gemeente <- substr(gemeente, 3, length(gemeente))
nedg$gcode <- as.numeric(gemeente)

#oppervlakte
oppv <- as.numeric(gArea(nedg, byid=T)/1000^2) #nu in vierkante km
#134km^2 voor postcode 8252 klopt
oppv <- as.data.frame(cbind(oppv, nedg$gcode))
oppv <- rename(oppv, c("V2"="gcode"))

#Transform into spatial data
nedg <- spTransform(nedg,CRS("+proj=longlat +ellps=WGS84"))
#gridcells voor elke gemeente selecteren
llCRS<-CRS("+proj=longlat +ellps=WGS84")
loc_g<-SpatialPoints(cbs100,proj4string=llCRS)
proj4string(loc_g)<-proj4string(nedg)
locg<-over(nedg, loc_g, returnList=T)

#select necessary data
popcounts <- cbs100[c("nauto2014", "nwal2014", "nnwal2014", "inw2014")]
popcounts$inw2014[popcounts$inw2014<0] <- NA

#calculate proportions for every gem.
prop <- function(x) {
ifelse(cbind(length(x)!=0, length(x)!=0, length(x)!=0), colSums(as.data.frame(popcounts@data[x,1:3]))/sum(colSums(as.data.frame(popcounts@data[x,1:3]))),c(NA,NA,NA)) }
prop2 <- lapply(locg, prop)
gemprop <-data.frame(matrix(unlist(prop2), nrow=length(locg),ncol=3, byrow=T))
colnames(gemprop) <- c("pauto2014", "pwal2014",  "pnwal2014")

gemprop <- cbind(gemprop, nedg$gcode)
gem <- rename(gemprop, c("nedg$gcode"="gcode"))
head(gem)

#calculate aantinw
inw <- function(x) {
ifelse(length(x)!=0, sum(as.data.frame(popcounts@data[x,4]),na.rm = TRUE),c(NA)) }
inw2 <- lapply(locg, inw)
geminw <-data.frame(matrix(unlist(inw2), nrow=length(locg),ncol=1, byrow=T))
colnames(geminw) <- c("inw2014")

geminw <- cbind(geminw, nedg$gcode)
geminw <- rename(geminw, c("nedg$gcode"="gcode"))
head(geminw)

gem <- merge(gem, geminw, by="gcode")

#calculate weightedwoz
wozcounts <- cbs100[c("wozwon2012", "won2012")]
wozcounts$wozwon2012[wozcounts$wozwon2012<0] <- NA
wozcounts$won2012[wozcounts$won2012<0] <- NA

woz <- function(x) {
ifelse(length(x)!=0, sum(as.data.frame(wozcounts@data[x,1])*as.data.frame(wozcounts@data[x,2]),na.rm=T)/sum(as.data.frame(wozcounts@data[x,2]),na.rm=T),c(NA)) }
woz2 <- lapply(locg, woz)
gemwoz<-data.frame(matrix(unlist(woz2), nrow=length(locg),ncol=1, byrow=T))
colnames(gemwoz) <- c("woz2012")

gemwoz <- cbind(gemwoz, nedg$gcode)
gemwoz <- rename(gemwoz, c("nedg$gcode"="gcode"))
head(gemwoz)

gem <- merge(gem, gemwoz, by="gcode") 

#add oppv
gem <- merge(gem, oppv, by="gocde")

#add centroid coordinates for gem
head(coordinates(nedg))
gem$l <- coordinates(nedg)[,1]
gem$j <- coordinates(nedg)[,2]

#_gc to datanames 
colnames(gem)[2:8] <- paste(colnames(gem[2:8]),"gc", sep = "_")

#READ IN 1VOP DATA
#setwd("C:/Users/U497130/AppData/Local/surfdrive_data/artikel azc/data/")
#thuis
#setwd("C:/Users/Joran/SURFdrive/artikel azc/data/")
setwd("U:/Surfdrive backup/Artikel azc/data")

#Data Beide Waves
#data W2 1vandaag
ev_t2 <- read.spss(file="AZC TOLSMA 3_17.11.15_08.40.15.AM.sav",use.value.labels=FALSE, to.data.frame=T)

#pc4 var aanmaken
names(ev_t2)
ev_t2 <- rename(ev_t2, c(Postcode_Opinie="pc4"))
ev_t2$pc4 <- str_trim(ev_t2$pc4)
ev_t2$pc4 <- as.numeric(ev_t2$pc4) #666 zonder geldige pc4

#select necessary vars
ev_t2 <- ev_t2[,c(1,29:35,45:47,58:60,71:74,95:99,109,173:176,183:197,199)]
names(ev_t2) 

# #Recode vars met _t2 erbij except id and pc4
# colnames(ev_t2) <- paste(colnames(ev_t2),"t2", sep = "_")
# ev_t2 <- rename(ev_t2, c(PanelistIdQuestion_t2="PanelistIdQuestion"))
# ev_t2 <- rename(ev_t2, c(pc4_t2="pc4"))

#data W1 1vandaag
ev_t1 <- read.spss(file="top1provincialestaten.sav",use.value.labels=FALSE, to.data.frame=T)

#pc4 var aanmaken
names(ev_t1)
ev_t1 <- rename(ev_t1, c(Postcode_Opinie="pc4"))
ev_t1$pc4 <- str_trim(ev_t1$pc4)
ev_t1$pc4[11317] <- substr(ev_t1$pc4[11317], 1, 4)
ev_t1$pc4 <- as.numeric(ev_t1$pc4) #473 zonder geldige pc4

#select necessary vars
ev_t1 <- ev_t1[,c(1,15,19:37)]
names(ev_t1) 
 
# #Recode vars met _t1 erbij except id and pc4
# colnames(ev_t1) <- paste(colnames(ev_t1),"t1", sep = "_")
# ev_t1 <- rename(ev_t1, c(PanelistIdQuestion_t1="PanelistIdQuestion"))
# ev_t1 <- rename(ev_t1, c(pc4_t1="pc4"))
 
#merge waves - COLUMN-WISE 
# evpanel <- merge(ev_t2, ev_t1, by.x="PanelistIdQuestion", by.y="PanelistIdQuestion", sort=F) #20844 resps
# evpanel$pc4 <- ifelse(is.na(evpanel$pc4_t1), evpanel$pc4_t2, evpanel$pc4_t2) #same pc4s missings beide waves

#merge waves - ROW-WISE 
#ID var for wave
ev_t1$tijd <- 1
ev_t2$tijd <- 2
head(ev_t1$PanelistIdQuestion)
head(ev_t2$PanelistIdQuestion)
#Rowbind and order two dataframes
evpanel <- rbind.fill(ev_t1, ev_t2)
evpanel <- evpanel[order(evpanel[,"PanelistIdQuestion"], evpanel[,"tijd"]),]
head(evpanel)
names(evpanel)

#merge 1vandaag aan gc codes
head(pc4gc_2013)
evpanel <- merge(evpanel, pc4gc_2013, all.x=T, by="pc4", sort=F)

#GCCODES 2013 OMZETTEN NAAR GCODES 2014, ZOVER ALS MOGELIJK
unique(evpanel$gc[!(evpanel$gc %in% nedg$gcode)]) # 499   82   55   51 1672  653 

#gcodes 2013 shape laden
nedg2 <- readOGR("U:/PhD Documents/CBS shapefiles/shape 2013", layer="gem_2013_v1")
nedg2 <- nedg2[nedg2$WATER=="NEE",]
gemeente<-as.character(nedg2@data$GM_CODE)
gemeente <- substr(gemeente, 3, length(gemeente))
nedg2$gcode <- as.numeric(gemeente)

nedg2$GM_NAAM[nedg2$gcode==499] # boskoop wordt alhpen aan de rijn
nedg2$GM_NAAM[nedg2$gcode==1672] #rijnwoude wordt alhpen aan de rijn
nedg2$GM_NAAM[nedg2$gcode==82] #lemsterland wordt de friese meren
nedg2$GM_NAAM[nedg2$gcode==55] #Boarnsterhim wordt de friese meren
nedg2$GM_NAAM[nedg2$gcode==51] #SkarsterlÃ¢n wordt de friese meren
nedg2$GM_NAAM[nedg2$gcode==653] #GaasterlÃ¢n-Sleat wordt de friese meren

nedg$gcode[nedg$GM_NAAM=="Alphen aan den Rijn"] #484
nedg$gcode[nedg$GM_NAAM=="De Friese Meren"] #1921

evpanel$gc[evpanel$gc==499] <- 484
evpanel$gc[evpanel$gc==1672] <- 484
evpanel$gc[evpanel$gc==82] <- 1921
evpanel$gc[evpanel$gc==51] <- 1921
evpanel$gc[evpanel$gc==55] <- 1921
evpanel$gc[evpanel$gc==653] <- 1921


#ONE PERSON IN NEW AREA, SHOULD HAVE A GCODE
pc4$PC4NR[is.na(pc4$gc)] # (1114) 1706 (5057)
evpanel$pc4[evpanel$pc4==1114 | evpanel$pc4==1706 | evpanel$pc4==5057] #one respondent in 1706
#1706 is heerhugowaard
names(nedg)
nedg[nedg$GM_NAAM=="Heerhugowaard",] # gc = 398
evpanel$gc[!is.na(evpanel$pc4) & evpanel$pc4==1706] <- 398

#pc4s that are not in pc4 shape
evpanel$pc4[!(evpanel$pc4 %in% pc4$PC4NR)]

#[1] 1000 1001 1020 1050 1070 1090 1100 1130 1200 1224 1234 1250 1270 1300 1500 1533 1550
#[19] 1612 1672 1800 1917 1922 1940 1986 2000 2030 2100 2130 2300 2310 2399 2400 2482 2500 2501
#[37] 2569 2588 2600 2695 2700 2800 2900 3000 3127 3160 3200 3283 3300 3350 3377 3391 3407 3440
#[55] 3450 3500 3519 3535 3600 3620 3647 3700 3745 3760 3800 3810 3854 3900 3964 3977 4200 4330
#[73] 4490 4500 4517 4530 4600 4750 4777 4800 4831 4857 4900 4919 4940 5000 5110 5120 5183 5200
#[91] 5210 5300 5350 5360 5500 5520 5565 5572 5596 5600 5650 5700 5743 5800 5900 5945 5967 5983
#[109] 6000 6040 6100 6130 6200 6203 6293 6400 6434 6500 6501 6552 6614 6800 6872 6906 6970 7000
#[127] 7153 7175 7200 7219 7240 7248 7270 7300 7320 7360 7400 7500 7550 7600 7650 7698 7743 7770
#[145] 7800 7900 7976 8000 8100 8180 8200 8234 8352 8440 8470 8500 8520 8543 8692 8705 8795 8847
#[163] 8860 8900 9400 9480 9500 9504 9506 9540 9600 9700 9704 9800 9818 9930

#other pc4 that should have gc codes
nedg[nedg$GM_NAAM=="Blaricum",] # gc = 376
evpanel$gc[!is.na(evpanel$pc4) & evpanel$pc4==1262] <- 376

#pc4s on NA die niet bestaan, on the basis of pc4 shape, postbussen etc
evpanel$pc4[!(evpanel$pc4 %in% pc4$PC4NR)] <- NA


#MATCH EV DATA TO AZCS & OTHER DATA
pc4_sel <- pc4[,c(1,3:25,28:31)]
evpanel <- merge(evpanel, pc4_sel, all.x=T, by.x="pc4", by.y="PC4NR", sort=F)

gem_sel <- gem[,c(1:6)]
evpanel <- merge(evpanel, gem_sel, all.x=T, by.x="gc", by.y="gcode", sort=F)


#AZC SIZE AS number of seekers per 1000inhab OF PC4/PC4aangr/GC
azcproppc4 <- evpanel[,c(56:61)]/(evpanel[,c(77)]/1000)
summary(evpanel[is.na(evpanel$inw2014_pc4),c(1,2,56:61)]) 
#azcproppc4[is.na(azcproppc4) & !is.na(evpanel$pc4)] <- 0 #allemaal 0, dus NA kan op 0
colnames(azcproppc4) <- paste(colnames(azcproppc4),"rel", sep = "")
evpanel <- cbind(evpanel, azcproppc4)

azcpropaangr <- evpanel[,c(68:73)]/(evpanel[,c(81)]/1000)
summary(evpanel[is.na(evpanel$inw2014_pc4),c(1,2,68:73)]) 
#azcpropaangr[is.na(azcpropaangr)] <- 0 #allemaal 0, dus NA kan op 0
colnames(azcpropaangr) <- paste(colnames(azcpropaangr),"rel", sep = "")
evpanel <- cbind(evpanel, azcpropaangr)

azcpropgc <- evpanel[,c(62:67)]/(evpanel[,c(86)]/1000)
summary(evpanel[is.na(evpanel$inw2014_gc),c(1,2,62:67)]) 
#azcpropgc[is.na(azcpropgc)] <- 0 #allemaal 0, dus NA kan op 0
colnames(azcpropgc) <- paste(colnames(azcpropgc),"rel", sep = "")
evpanel <- cbind(evpanel, azcpropgc)
evpanel[!is.na(evpanel$gc) & is.na(evpanel$s1cap_gct1rel),c(100:105)] <- 0
names(evpanel)

#AZC SIZE AS number of seekers per nb non west OF PC4
azcnwpc4 <- evpanel[,c(56:61)]/(evpanel[,c(79)])
summary(evpanel[is.na(evpanel$inw2014_pc4),c(1,2,56:61)]) 
#azcproppc4[is.na(azcproppc4) & !is.na(evpanel$pc4)] <- 0 #allemaal 0, dus NA kan op 0
colnames(azcnwpc4) <- paste(colnames(azcnwpc4),"nw", sep = "")
evpanel <- cbind(evpanel, azcnwpc4)

#######################################
#CReate individual VARIABLES

#Select only native Dutch respondents
table(evpanel$Herkomst_familie_Opinie, useNA="always")
evpanel <- evpanel[evpanel$Herkomst_familie_Opinie==1 & !is.na(evpanel$Herkomst_familie_Opinie),] # minus (849) respondents 
table(evpanel$Herkomst_familie_Opinie, useNA="always")


#DEPENDENT VARIABLES

#Voting intentions - W1
table(evpanel$stemvraag, useNA="always")
evpanel$vote <- evpanel$stemvraag
#t1: "1"="vvd", "2"="pvda", "3"="pvv", "4"="cda", "5"="sp", "6"="d66", "7"="gl", "8"="cu", "9"="sgp", "10"="dier", "11"="50plus", "12"="blanco", "13"="dontknow", "14"="novote","15"=not allowed","16"="no answer", "17"="other"
#t2: "1"="vvd", "2"="pvda", "3"="pvv", "4"="cda", "5"="sp", "6"="d66", "7"="gl", "8"="cu", "9"="sgp", "10"="dier", "11"="50plus", "12"="vnl","13"="blanco", "14"="dontknow", "15"="novote", "16"=not allowed","17"="no answer", "18"="other"))
evpanel$vote[evpanel$tijd==1 & evpanel$vote==17] <- 18
evpanel$vote[evpanel$tijd==1 & evpanel$vote==16] <- 17
evpanel$vote[evpanel$tijd==1 & evpanel$vote==15] <- 16
evpanel$vote[evpanel$tijd==1 & evpanel$vote==14] <- 15
evpanel$vote[evpanel$tijd==1 & evpanel$vote==13] <- 14
evpanel$vote[evpanel$tijd==1 & evpanel$vote==12] <- 13

#evpanel$vote <- ifelse(evpanel$vote==13,NA,evpanel$vote)
#evpanel$vote <- ifelse(evpanel$vote==15,NA,evpanel$vote)
#evpanel$vote <- ifelse(evpanel$vote==16,NA,evpanel$vote)
#evpanel$vote <- ifelse(evpanel$vote==17,NA,evpanel$vote)
table(evpanel$vote, useNA="always")

#PVV vote
# 12=Blanco (0.6%), 13=Weet ik nog niet (11,4%), 14=Wel gerechtigd, ga niet (1,1%), 15=niet stemgerectigd (0.004%), 16=geen opgave (0.5%),17=andere partij
evpanel$PVV<-ifelse(evpanel$vote==3,1,0)
table(evpanel$PVV, useNA="always")

#Vote GL 
evpanel$GL<-ifelse(evpanel$vote==7,1,0)
table(evpanel$GL, useNA="always")


#threat
#for threatDV as DV dont know with 
evpanel$threat <- NA
evpanel$threat[evpanel$RB_1==1] <- 4
evpanel$threat[evpanel$RB_1==2] <- 3
evpanel$threat[evpanel$RB_1==3] <- 2
evpanel$threat[evpanel$RB_1==4] <- 1
evpanel$threat[evpanel$RB_1==5] <- 0
evpanel$threat[evpanel$RB_1==6] <- 2 #dont know cat.
table(evpanel$threat, useNA="always")


evpanel$threat[evpanel$komst_minderheden==1] <- 4
evpanel$threat[evpanel$komst_minderheden==2] <- 3
evpanel$threat[evpanel$komst_minderheden==3] <- 2
evpanel$threat[evpanel$komst_minderheden==4] <- 1
evpanel$threat[evpanel$komst_minderheden==5] <- 0
evpanel$threat[evpanel$komst_minderheden==6] <- 2 #dont know cat.
table(evpanel$threat, useNA="always")


#contact
#Contact (Hoe vaak heeft u in uw buurt persoonlijk contact met iemand van niet-westerse herkomst/oost europees)
#1=bijna elke dag, 7=nooit, 8=nvt
evpanel$contactnw <- NA
evpanel$contactnw[evpanel$RB_3a==1] <- 6
evpanel$contactnw[evpanel$RB_3a==2] <- 5
evpanel$contactnw[evpanel$RB_3a==3] <- 4
evpanel$contactnw[evpanel$RB_3a==4] <- 3
evpanel$contactnw[evpanel$RB_3a==5] <- 2
evpanel$contactnw[evpanel$RB_3a==6] <- 1
evpanel$contactnw[evpanel$RB_3a==7] <- 0 #NOOIT
evpanel$contactnw[evpanel$RB_3a==8] <- 0 #NVT
table(evpanel$contactnw, useNA="always")

table(evpanel$Niet_westers)
evpanel$contactnw[evpanel$Niet_westers==1] <- 6
evpanel$contactnw[evpanel$Niet_westers==2] <- 5
evpanel$contactnw[evpanel$Niet_westers==3] <- 4
evpanel$contactnw[evpanel$Niet_westers==4] <- 3
evpanel$contactnw[evpanel$Niet_westers==5] <- 2
evpanel$contactnw[evpanel$Niet_westers==6] <- 1
evpanel$contactnw[evpanel$Niet_westers==7] <- 0
evpanel$contactnw[evpanel$Niet_westers==8] <- 0
table(evpanel$contactnw, useNA="always")


#azc vars

#perceptie azc in de buurt?
#Worden er in uw buurt vluchtelingen opgevangen of zijn daar plannen voor?
#1.Ja, in mijn buurt staat een azc
#2.Ja, in mijn buurt is er een tijdelijke opvang voor vluchtelingen
#3.Nee, maar daar zijn wel plannen voor
#4.Nee, in mijn buurt worden geen vluchtelingen opgevangen
#5.Weet niet / geen mening
# table(evpanel$Opvang_buurt, useNA="always")
# evpanel$percpc4azc <- ifelse(evpanel$Opvang_buurt==1 |  evpanel$Opvang_buurt==2, 1, 0)
# table(evpanel$percpc4azc, useNA="always")
# #dummy version
# evpanel$percpc4azc_dum <- as.factor(evpanel$Opvang_buurt)
# levels(evpanel$percpc4azc_dum ) <- c("ja, azc","ja, tijdelijk","wel plannen", "nee", "weet niet")

#Covariates

#Gender
table(evpanel$Geslacht_Opinie, useNA="always")
evpanel$gender <- ifelse(evpanel$Geslacht_Opinie==1, 1,0) # 1= male
table(evpanel$gender, useNA="always")

#Age
#table(evpanel$Geboortedatum_Opinie)
evpanel$gebdat <- as.POSIXlt(evpanel$Geboortedatum_Opinie, origin="1582/10/14") 
evpanel$ages <- as.character(evpanel$gebdat)
evpanel$age <- substr(evpanel$ages, 1, 4)
evpanel$age <- as.numeric(evpanel$age)
evpanel$age <- 2015 - evpanel$age
table(evpanel$age, useNA="always")
#Select on Age higher than 18
evpanel <- evpanel[evpanel$age>=18,]

# Education (years of education that constitute the shortest route to obtain a university degreeAfter this coding, all values are subtracted from the maximum years of schooling necessary to obtain a university grade))
#(1) 5 lagere school not finished (4yrs) /lagere school (6yrs) 
#(2) 7 lbo, vmbo-kb/bbl (6,5yrs) /mavo, vmbo-tl (8yrs) 
#(3) 11 havo (10yrs) / vwo/gymnasium (12yrs)
#(4) 9,5 mbo-kort (kmbo) (8,5yrs) /mbo-tussen/lang (mbo) (10,5yrs) 
#(5) 14 hbo (14yrs) 
#(6) 16,5 universiteit (bachelormaster, doctoraal)(16,5)

table(evpanel$Opleiding_Opinie, useNA="always")
evpanel$educ <- NA
evpanel$educ[evpanel$Opleiding_Opinie==1] <- 5 
evpanel$educ[evpanel$Opleiding_Opinie==2] <- 7 
evpanel$educ[evpanel$Opleiding_Opinie==3] <- 11 
evpanel$educ[evpanel$Opleiding_Opinie==4] <- 9.5 
evpanel$educ[evpanel$Opleiding_Opinie==5] <- 14 
evpanel$educ[evpanel$Opleiding_Opinie==6] <- 16.5 
table(evpanel$educ, useNA="always")

#Civil status
#1=gehuwd/gereg partner, 2=gescheiden,3=weduwe,4=ongehuwd
table(evpanel$Burgerlijke_staat_Opinie, useNA="always")
evpanel$civilstatus <- evpanel$Burgerlijke_staat_Opinie
table(evpanel$civilstatus, useNA="always")

#Household comp
#1=inw bij ouders,2=single no kids, 3=single kids, 4=student/community,5=married no kids, 6=married kids, 7=other
table(evpanel$Situatie_huishouden_Opinie, useNA="always")
evpanel$household <- NA
evpanel$household[evpanel$Situatie_huishouden_Opinie==2] <- 1
evpanel$household[evpanel$Situatie_huishouden_Opinie==3] <- 2
evpanel$household[evpanel$Situatie_huishouden_Opinie==5] <- 3
evpanel$household[evpanel$Situatie_huishouden_Opinie==6] <- 4
evpanel$household[evpanel$Situatie_huishouden_Opinie==1] <- 5
evpanel$household[evpanel$Situatie_huishouden_Opinie==4] <- 5
evpanel$household[evpanel$Situatie_huishouden_Opinie==7] <- 5
table(evpanel$household, useNA="always")
#1=single no kids, 2=single kids, 3=married no kids, 4=married kids, 5=other

#Daily activity
#1=betaald werk, 2=zelfstandig,3=werkzoekend,4=arbeidsong,5=student,6=huisvrouw,7=pensoen,8=other
table(evpanel$Dagelijkse_activiteit_Opinie, useNA="always")
evpanel$dailyact <- NA
evpanel$dailyact[evpanel$Dagelijkse_activiteit_Opinie==1] <- 1
evpanel$dailyact[evpanel$Dagelijkse_activiteit_Opinie==2] <- 1
evpanel$dailyact[evpanel$Dagelijkse_activiteit_Opinie==3] <- 2
evpanel$dailyact[evpanel$Dagelijkse_activiteit_Opinie==4] <- 3
evpanel$dailyact[evpanel$Dagelijkse_activiteit_Opinie==5] <- 4
evpanel$dailyact[evpanel$Dagelijkse_activiteit_Opinie==6] <- 5
evpanel$dailyact[evpanel$Dagelijkse_activiteit_Opinie==7] <- 6
evpanel$dailyact[evpanel$Dagelijkse_activiteit_Opinie==8] <- 7
#1=betaald werk/zelfstandig,2=werkzoekend,3=arbeidsong,4=student,5=huisvrouw,6=pensoen,7=other
table(evpanel$dailyact, useNA="always")


#only keep relevant vars
evpanel_sel <- evpanel[,c(1,2,18,23,56:113,115:117,120:124)]

names(evpanel)

#SELECTION ON PANEL resps
evpanel_sel <- evpanel_sel[evpanel_sel$PanelistIdQuestion %in% evpanel_sel$PanelistIdQuestion[duplicated(evpanel_sel$PanelistIdQuestion)],]


#ADD WEIGHTS TO Data

#make caseidn
evpanel_sel$caseidn <- 1:length(evpanel_sel[,1])

#MAKE TARGET VALUES
#Geslacht
gender_w <- c(0.503,0.497) # vrouw - man
sum(gender_w)

#Age: 3cats
age_w <- c(0.238,0.344,0.418) #18-35,35-55,55+
names(age_w) <- c("age1", "age2", "age3")
sum(age_w)

#Opl: 3cats
educ_w <- c(0.323,0.393,0.284) #basis/vmbo/mbo1,mbo2-4/HV/VWO,HBO/WO 
names(educ_w) <- c("opl1", "opl2", "opl3")
sum(educ_w)

targets <- list(gender_w,age_w, educ_w) 
names(targets) <- c("gender_w","age_w", "educ_w") 

#check variables in data
table(evpanel_sel$gender, useNA="always") #man=1
evpanel_sel$gender_w <- as.logical(evpanel_sel$gender)
table(evpanel_sel$gender_w, useNA="always")

#age: 3cats
table(evpanel_sel$age, useNA="always") #18-35,35-55,55+
evpanel_sel$age_w <- NA
evpanel_sel$age_w[evpanel_sel$age<=35] <- 1
evpanel_sel$age_w[evpanel_sel$age>35 & evpanel_sel$age<=55] <- 2
evpanel_sel$age_w[evpanel_sel$age>55] <- 3 
evpanel_sel$age_w <- as.factor(evpanel_sel$age_w)
levels(evpanel_sel$age_w) <- c("age1", "age2", "age3")
table(evpanel_sel$age_w, useNA="always")

#Opl: 3cats
table(evpanel_sel$educ, useNA="always") 
evpanel_sel$educ_w <- NA
evpanel_sel$educ_w[evpanel_sel$educ<=8] <- 1
evpanel_sel$educ_w[evpanel_sel$educ> 8 & evpanel_sel$educ<=12] <- 2
evpanel_sel$educ_w[evpanel_sel$educ>12] <- 3
evpanel_sel$educ_w <-  as.factor(evpanel_sel$educ_w)
levels(evpanel_sel$educ_w) <- c("opl1", "opl2", "opl3")
table(evpanel_sel$educ_w, useNA="always") 

#make weights (for 3cats cap is 6cap)
weighted <- anesrake(inputter=targets, dataframe=evpanel_sel, choosemethod="max",caseid=evpanel_sel$caseidn,cap = 5)
test <- weightassess(targets, evpanel_sel, weighted$weightvec)

oplp <- test$educ_w[c(1:3),1]*39976
oplw <- test$educ_w[c(1:3),4]
test2 <- matrix(c(oplp, oplw), byrow=T, nrow=2)
dimnames(test2) <- list(weightype=c("p","w"),opl=c("1","2","3"))
chisq.test(test2)

agep <- test$age_w[c(1:3),1]*39976
agew <- test$age_w[c(1:3),4]
test2 <- matrix(c(agep, agew), byrow=T, nrow=2)
dimnames(test2) <- list(weightype=c("p","w"),age=c("1","2","3"))
chisq.test(test2)

geslp <- test$gender_w[c(1:2),1]*39976
geslw <- test$gender_w[c(1:2),4]
test2 <- matrix(c(geslp, geslw), byrow=T, nrow=2)
dimnames(test2) <- list(weightype=c("p","w"),age=c("v","m"))
chisq.test(test2)

evpanel_sel <- cbind(evpanel_sel, weighted$weightvec)
evpanel_sel <- rename(evpanel_sel, c("weighted$weightvec"="weightvec"))


#scale weights to cluster (https://www.statmodel.com/download/Scaling3.pdf)

#PC4
nresppc4 <- aggregate(evpanel_sel$weightvec, by=list(evpanel_sel$pc4), FUN=length)
weightpc4 <- aggregate(evpanel_sel$weightvec, by=list(evpanel_sel$pc4), FUN=sum)
evpanel_sel$weightvec_pc4 <- evpanel_sel$weightvec*((nresppc4$x[match(evpanel_sel$pc4,nresppc4$Group.1)])/(weightpc4$x[match(evpanel_sel$pc4,weightpc4$Group.1)]))

#PC4
nrespgc <- aggregate(evpanel_sel$weightvec, by=list(evpanel_sel$gc), FUN=length)
weightgc <- aggregate(evpanel_sel$weightvec, by=list(evpanel_sel$gc), FUN=sum)
evpanel_sel$weightvec_gc <- evpanel_sel$weightvec*((nrespgc$x[match(evpanel_sel$gc,nrespgc$Group.1)])/(weightgc$x[match(evpanel_sel$gc,weightgc$Group.1)]))


evpanel_sel <- evpanel_sel[,c(1:70, 75:77)]

#data wegscrhrijven
#write.table(evpanel_sel, file="C:/Users/U497130/AppData/Local/surfdrive_data/Artikel azc/data/evazc v15032017.csv")
#thuis
#write.table(evpanel_sel, file="C:/Users/Joran/SURFdrive/Artikel azc/data/evazc v22122017.csv")

#data to stata
#write.dta(evpanel_sel, "C:/Users/u497130/AppData/Local/Surfdrive_data/Artikel azc/data/evazc v22122017.dta")
#thuis
write.dta(evpanel_sel, file="U:/Surfdrive backup/Artikel azc/data/evazc v22122017II.dta")


