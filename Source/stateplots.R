library(maps)
library(maptools)
library(gridExtra)
library(readr)
states <- as.data.frame(read.csv2("data/USstatePopulation.csv", strip.white = TRUE))
colnames(states)<-c("State","Pop")
states$State <- tolower(gsub("\\.","",states$State))
states$Pop <- as.numeric(gsub("\\.","",states$Pop))
states <- states[order(states$State),]

train <- read_csv("/Users/markusheuchert/AML/springleaf_train.csv")
target <- train[,ncol(train)]
train_char = train[, sapply(train, is.character)]

mapUSA <- map('state', fill=TRUE, plot=FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))

mapStates = function(df, feat, relative="F"){
  dat = data.frame(table(df[,feat]))
  names(dat) = c("state.abb", "value")
  dat$states <- tolower(state.name[match(dat$state.abb,  state.abb)])
  if(sum(dat$state.abb=="PR") >0){dat[dat$state.abb=="PR",]$states = "puerto rico"}
  if(sum(dat$state.abb=="DC") >0){dat[dat$state.abb=="DC",]$states = "district of columbia"}
  dat <- dat[!is.na(dat$states),]
  dat <- dat[order(dat$states),]

  if(relative=="P"){
    dat <- cbind(dat, states[which(states$State %in% dat$states),]$Pop)
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
    if(sum(dat$state.abb=="PR") >0){dat[dat$state.abb=="PR",]$states = "puerto rico"}
    if(sum(dat$state.abb=="DC") >0){dat[dat$state.abb=="DC",]$states = "district of columbia"}
    dat <- dat[!is.na(dat$states),]
    dat <- dat[order(dat$states),]
  }
  
  idx <- match(unique(nms),  dat$states)
  dat2 <- data.frame(value = dat$value[idx], state = unique(nms))
  row.names(dat2) <- unique(nms) 
  USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
  if(relative=="P"){
    title = "w.r.t. to state population"
  }
  if(relative=="T"){
    title = "w.r.t. to target"
  }
  if(relative=="F"){
    title = "value count"
  }
  spplot(USAsp['value'], main=paste(feat, title), col.regions=rev(heat.colors(21)),par.settings=list(fontsize=list(text=20)))
}
#grid.arrange(mapStates(train_char, "VAR_0274"), mapStates(train_char, "VAR_0237"),ncol=2)

###

pdf("fig/VAR0274_P.pdf")
mapStates(train_char, "VAR_0274","P")
dev.off()

pdf("fig/VAR0274_T.pdf")
mapStates(train_char, "VAR_0274","T")
dev.off()

pdf("fig/VAR0274_F.pdf")
mapStates(train_char, "VAR_0274")
dev.off()

#####

pdf("fig/VAR0237_P.pdf")
mapStates(train_char, "VAR_0237","P")
dev.off()

pdf("fig/VAR0237_T.pdf")
mapStates(train_char, "VAR_0237","T")
dev.off()

pdf("fig/VAR0237_F.pdf")
mapStates(train_char, "VAR_0237")
dev.off()


