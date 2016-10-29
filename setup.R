#temp path for markus.. upload data to git when fast wifi available
train <- read.csv("/Volumes/Nifty/springleaf_train.csv",n_max = 20000)
#test
y = train$target
# remove the id and target
train = subset(train, select=-c(ID, target))
# get the rowcount

