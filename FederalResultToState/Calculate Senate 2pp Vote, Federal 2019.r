require(dplyr)
require(tidyr)
require(tibble)
require(stringr)

## Note that you will need to download the Senate vote data on your own as those files are very big
## In particular you will need to download the ZIP files from the "Formal Preferences" folder
## Extract the CSVs and place them into your working directory (edit code below to direct R accordingly)

BaseDir <- "ENTER WORKING DIRECTORY HERE"
States <- c("New South Wales","Victoria","Queensland","Western Australia","South Australia","Tasmania","Northern Territory","Australian Capital Territory")
StateAbs <- c("NSW","VIC","QLD","WA","SA","TAS","NT","ACT")

BallotFiles <- list.files(paste0(BaseDir))
Party1 <- c("Labor.Country.Labor","Labor","Australian.Labor.Party","A.L.P.","Australian.Labor.Party..Northern.Territory..Branch")
Party2 <- c("Liberal...Nationals","Liberal.The.Nationals","LIBERAL.THE.NATIONALS","Liberal.National.Party.of.Queensland","Liberal","Country.Liberals..NT.")
PartyAb1 <- "ALP"
PartyAb2 <- "LNC"
PrefBallotData <- data.frame(StateAb=character(),
                             DivisionID=integer(),
                             DivisionNm=character(),
                             PollingPlaceID=integer(),
                             PollingPlaceNm=character(),
                             Party1=character(),
                             Party2=character(),
                             RankParty1=integer(),
                             RankParty2=integer(),
                             Preference=character())

## Extracts highest voter ranking for each party listed (smallest number)
## This includes below-the-line votes
## So e.g. if a voter ranked candidate 2 of Party1 second, this is recorded as ranking Party1 second
for(File in BallotFiles){
  AllBallots <- read.csv(paste0(BaseDir,"Research/Federal-2019/BallotData/",File))
  
  Parties <- substr(colnames(AllBallots),3,nchar(colnames(AllBallots)))[-(1:6)]
  Group1 <- strsplit(colnames(AllBallots)[c(rep(FALSE,6),Parties %in% Party1)],".",fixed=TRUE)[[1]][1]
  Group2 <- strsplit(colnames(AllBallots)[c(rep(FALSE,6),Parties %in% Party2)],".",fixed=TRUE)[[1]][1]
  
  StatePrefData <- data.frame(StateAb=strsplit(strsplit(File,".",fixed=TRUE)[[1]][1],split="-",fixed=TRUE)[[1]][5],
                              DivisionID=NA,
                              DivisionNm=AllBallots$Division,
                              PollingPlaceID=NA,
                              PollingPlaceNm=AllBallots$Vote.Collection.Point.Name,
                              Party1=PartyAb1,
                              Party2=PartyAb2,
                              RankParty1=do.call(pmin,args=c(AllBallots[,c(rep(FALSE,6),substr(colnames(AllBallots)[-(1:6)],1,nchar(Group1)+1) == paste0(Group1,"."))],list(na.rm=TRUE))),
                              RankParty2=do.call(pmin,args=c(AllBallots[,c(rep(FALSE,6),substr(colnames(AllBallots)[-(1:6)],1,nchar(Group2)+1) == paste0(Group2,"."))],list(na.rm=TRUE))),
                              Preference=NA)
  
  PrefBallotData <- rbind(PrefBallotData,StatePrefData)
  
  cat(paste("Preference analysis complete for",States[StateAbs==strsplit(strsplit(File,".",fixed=TRUE)[[1]][1],"-",fixed=TRUE)[[1]][5]],"\n"))
  rm(list=c("AllBallots","StatePrefData","Parties","Group1","Group2"))
  invisible(gc())
}

PrefBallotData$DivisionID <- DivSenData2019$DivisionID[match(PrefBallotData$DivisionNm,DivSenData2019$DivisionNm)]
PrefBallotData$Preference[replace_na(PrefBallotData$RankParty1,999) < replace_na(PrefBallotData$RankParty2,999)] <- PrefBallotData$Party1[replace_na(PrefBallotData$RankParty1,999) < replace_na(PrefBallotData$RankParty2,999)]
PrefBallotData$Preference[replace_na(PrefBallotData$RankParty1,999) > replace_na(PrefBallotData$RankParty2,999)] <- PrefBallotData$Party2[replace_na(PrefBallotData$RankParty1,999) > replace_na(PrefBallotData$RankParty2,999)]
PrefBallotData$Preference[replace_na(PrefBallotData$RankParty1,999) == replace_na(PrefBallotData$RankParty2,999)] <- "EXH"

## Aggregates 2pp vote count by state
State2ppCount <- table(PrefBallotData$StateAb,PrefBallotData$Preference)
State2ppLNC <- State2ppCount[,3]/(State2ppCount[,1]+State2ppCount[,3])

write.csv(State2ppLNC,"Federal-2019-Senate-2pp-State.csv",row.names=FALSE)
