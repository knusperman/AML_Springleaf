library(maps)
library(maptools)
library(gridExtra)
states <- as.data.frame(read.csv2("USstatePopulation.csv", strip.white = TRUE))
colnames(states)<-c("State","Pop")
states$State <- tolower(gsub("\\.","",states$State))
states$Pop <- as.numeric(gsub("\\.","",states$Pop))
states <- states[order(states$State),]

train <- read_csv("springleaf_train.csv",n_max = 10000)
target <- train[,ncol(train)]
train_char = train[, sapply(train, is.character)]

mapUSA <- map('state', fill=TRUE, plot=FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))

mapStates = function(df, feat, relative="F"){
  dat = data.frame(table(df[,feat]))
  names(dat) = c("state.abb", "value")
  dat$states <- tolower(state.name[match(dat$state.abb,  state.abb)])
  dat[dat$state.abb=="PR",]$states = "puerto rico"
  dat[dat$state.abb=="DC",]$states = "district of columbia"
  dat <- dat[!is.na(dat$states),]
  dat <- dat[order(dat$states),]

  if(relative=="P"){
    
    
    dat <- cbind(dat, states$Pop)
    colnames(dat) <- c("states.abb","value","states","pop")
    dat$value = dat$value/dat$pop
  }
  if(relative=="T"){
    dat = data.frame(table(df[,feat]))
    tab = table(train[, colnames(train) %in% c(feat, "target")])
    dat$ratio = NA
    for (i in 1:dim(tab)[1]) {
      dat[i,"ratio"]= tab[i,2]/tab[i,1]
    }
    dat$Freq = NULL
    names(dat) = c("state.abb", "value")
    dat$states <- tolower(state.name[match(dat$state.abb,  state.abb)])
    dat[dat$state.abb=="PR",]$states = "puerto rico"
    dat[dat$state.abb=="DC",]$states = "district of columbia"
    dat <- dat[!is.na(dat$states),]
    dat <- dat[order(dat$states),]
  }
  
  idx <- match(unique(nms),  dat$states)
  dat2 <- data.frame(value = dat$value[idx], state = unique(nms))
  row.names(dat2) <- unique(nms) 
  USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
  spplot(USAsp['value'], main=paste(feat, "value count"), col.regions=rev(heat.colors(21)))
}
#grid.arrange(mapStates(train_char, "VAR_0274"), mapStates(train_char, "VAR_0237"),ncol=2)

generatePDF = function(varname){
  pdf(paste(varname,".pdf",sep = ""))
  mapStates(train_char, varname)
  dev.off()
}
