getwd()
setwd("~/Dropbox/data/imputed-gdp")


library(foreign)
library(WDI)
library(countrycode)
library(reshape)
library(plyr)
library(corrplot)


RGDP <- read.csv("~/Dropbox/data/expgdpv5.0/expgdp_v6.0.csv", sep="\t") # Gleditsch data, v. 6
COWNMC <- read.csv("~/Dropbox/data/MID/NMC_v4_0.csv") # I started with MID only when I started playing with COW data, hence the MID directory
WBD <- WDI(country="all", indicator=c("NY.GDP.PCAP.CD","EG.USE.PCAP.KG.OE"), start=1945, end=2014)
CY <- read.csv("~/Dropbox/data/EUGene-output/country-years-1946-2008.csv") # Blank country-year frame, via EUGene.

WBD <- rename(WBD, c(NY.GDP.PCAP.CD="wbgdppc"))
WBD <- rename(WBD, c(EG.USE.PCAP.KG.OE="wbpec"))

WBD$ccode <- countrycode(WBD$iso2c, "iso2c", "cown")
WBD$ccode[WBD$iso2c == "RS"] <- 345

WBD$country <- WBD$iso2c <- NULL
WBD <- subset(WBD, !is.na(ccode))

WBD$logwbgdppc <- log(WBD$wbgdppc)
WBD$logwbpec <- log(WBD$wbpec)

## Clean COWNMC
###############

COWNMCs <- with(COWNMC, data.frame(ccode,year,irst, pec, cinc))

COWNMCs <- rename(COWNMCs, c(pec="cowpec"))
COWNMCs$logcowpec <- log(COWNMCs$cowpec+1)



RGDP$ccode <- countrycode(RGDP$stateid, "cowc", "cown")

RGDP$ccode[RGDP$stateid == "SER"] <- 345
RGDP$ccode[RGDP$stateid == "RUM"] <- 360
RGDP$ccode[RGDP$stateid == "FJI"] <- 950
RGDP$ccode[RGDP$stateid == "KBI"] <- 946

RGDP <- subset(RGDP, !(year == 2006 & stateid == "YUG"))
RGDP <- subset(RGDP, !is.na(ccode))

RGDPs <- with(RGDP, data.frame(ccode, year, rgdppc))
RGDPs$logrgdppc <- log(RGDPs$rgdppc)

## Country-Year Dataframe
#########################

Alivein2008 <- subset(CY, year == 2008)

EmptyPDF <- data.frame(
        ccode =unique(Alivein2008$ccode),
	 dates = c("2009-2015"))

EmptyPDF <- cbind(EmptyPDF, colsplit(EmptyPDF$date, "-", c("start", "end")))
EmptyPDF <- adply(EmptyPDF, 1, summarise, year = seq(start, end))[c("ccode", "year")]

# South Sudan *should* be the only one missing here since it enters only in 2011.
SouthSudan <- data.frame(ccode=626,year=seq(2011,2015))

EmptyPDF <- rbind(EmptyPDF,SouthSudan)
CYn <- rbind(CY,EmptyPDF)


## Merge.
#########

Data <- merge(CYn, RGDPs, by=c("ccode","year"), all.x = TRUE)
Data <- merge(Data, WBD, by=c("ccode","year"), all.x = TRUE)
Data <- merge(Data, COWNMCs, by=c("ccode","year"), all.x = TRUE)


checkfordup <- with(Data, data.frame(ccode, year))
checkfordup[duplicated(checkfordup),]

Data <- Data[order(Data$ccode, Data$year),]

write.table(Data,file="imputed-gdp.csv",sep=",",row.names=F,na="")

# insheet using /home/steve/Dropbox/data/imputed-gdp/imputed-gdp.csv, clear
# regress logrgdppc logwbpec i.ccode i.year
# predict fv1
# regress logrgdppc logcowpec i.ccode i.year
# predict fv2
# regress logwbgdppc logwbpec i.ccode i.year
# predict fv3
# regress logwbgdppc logcowpec i.ccode i.year
# predict fv4
# export delimited using "C:\users\steve\My Documents\Dropbox\data\imputed-gdp\imputed-gdp.csv", replace

Data <- read.csv("imputed-gdp.csv")

Data$explogrgdppc <- ifelse(is.na(Data$logrgdppc), Data$fv1, Data$logrgdppc)
Data$explogrgdppc <- ifelse(is.na(Data$explogrgdppc), Data$fv2, Data$explogrgdppc)

Data$explogwbgdppc <- ifelse(is.na(Data$logwbgdppc), Data$fv3, Data$logwbgdppc)
Data$explogwbgdppc <- ifelse(is.na(Data$explogwbgdppc), Data$fv4, Data$explogwbgdppc)

CorrDF <- with(Data, data.frame(logrgdppc, logwbgdppc, explogrgdppc, explogwbgdppc, fv1, fv2, fv3, fv4))
Corr <- cor(CorrDF, use="complete.obs")

colnames(Corr) <- c("Log Real GDP per Capita (Gleditsch)", "Log GDP Per Capita (WB)", "Expected Log Real GDP per Capita", "Expected Log GDP per Capita", "Fitted Value 1", "Fitted Value 2", "Fitted Value 3", "Fitted Value 4")
rownames(Corr) <- c("Log Real GDP per Capita (Gleditsch)", "Log GDP Per Capita (WB)", "Expected Log Real GDP per Capita", "Expected Log GDP per Capita", "Fitted Value 1", "Fitted Value 2", "Fitted Value 3", "Fitted Value 4")

png(file="corr-gdp.png", width=675, height=525, pointsize=12)
pdf(file="corr-gdp.pdf", width=9, height=7, pointsize=12)
 
corrplot(Corr, method = "number",  tl.col="black", tl.cex=.8, tl.srt=45, type="lower")
graphics.off()


write.table(Data,file="imputed-gdp.csv",sep=",",row.names=F,na="")








