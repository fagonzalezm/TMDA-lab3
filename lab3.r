require(maxent)

IMDB <- read.table("./IMDB", header=TRUE, sep=",")
head(IMDB)
attach(IMDB)