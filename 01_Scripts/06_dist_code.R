#DOESN'T WORK ON MY COMPUTER (TO HAVY)

#https://egallic.fr/en/closest-distance-between-countries/
#https://gist.githubusercontent.com/mtriff/185e15be85b44547ed110e412a1771bf/raw/1bb4d287f79ca07f63d4c56110099c26e7c6ee7d/getCountryDist.r

install.packages("maps")
install.packages("geosphere")
install.packages("sp")


library(maps)
library(geosphere)
library(dplyr)
library(sp)
world.map <- map("world", fill = TRUE)

indicePays <- seq(1,length(world.map$names))

# https://stat.ethz.ch/pipermail/r-help/2010-April/237031.html
splitNA <- function(x){
  idx <- 1 + cumsum(is.na(x))
  not.na <- !is.na(x)
  split(x[not.na], idx[not.na])
}

# Coordinates of every country
lesCoordsX <- splitNA(world.map$x)
lesCoordsY <- splitNA(world.map$y)

lesDistancesUnPays <- function(unIndicePays){
  # Borders coordinates for current country
  coordsPays <- data.frame(long = lesCoordsX[[unIndicePays]], lat = lesCoordsY[[unIndicePays]])
  
  # Indexes of countries except the current one
  # and the one for which the computation has already been done
  lesIndicesAutresPays <- indicePays[indicePays > unIndicePays]
  
  
  distancePoint <- function(unPoint){
    unPoint.m <- matrix(unPoint, ncol = 2)
    
    
    # We need to compute distances between unPoint and every border points of every other countries
    # it is given by lesIndicesAutresPays
    
    distancePointPays <- function(unIndicePays2){
      coordsPays2 <- matrix(cbind(long = lesCoordsX[[unIndicePays2]], lat = lesCoordsY[[unIndicePays2]]), ncol = 2)
      lesDistPointPays2 <- spDists(x=coordsPays2, y=matrix(unPoint, ncol=2), longlat=TRUE)
      return(min(lesDistPointPays2)) # shortest distance between unPoint and country which index is unIndicePays2
    }
    lesDistPointPays2 <- lapply(lesIndicesAutresPays, distancePointPays)
    res <- unlist(lesDistPointPays2)
    return(res)
  }
  
  distancesPays <- apply(coordsPays, 1, distancePoint)
  # Shortest distances between unPoint and every other country
  if(!is.matrix(distancesPays)){
    # For the last country on the list
    plusCourtesDistances <- min(distancesPays)
  }else{
    plusCourtesDistances <- apply(distancesPays, 1, min)
  }
  
  resul <- cbind(pays1 = rep(unIndicePays, length(plusCourtesDistances)),pays2 = lesIndicesAutresPays, dist = plusCourtesDistances)
  return(resul)
}

# We don't need distances for the last country (they have all been computed)
lesDist <- lapply(indicePays[-length(indicePays)], lesDistancesUnPays)

# Convert to data frame
distMat <- do.call(rbind, lesDist)
lesDist <- as.data.frame(distMat)

# We need to recover distances for each couple
lesDist$ID <- paste(sprintf("%04d", lesDist$pays1), sprintf("%04d", lesDist$pays2), sep = "")

lesDist2 <- data.frame(cbind(pays1 = rep(indicePays, each = length(indicePays)),
                             pays2 = rep(indicePays, length(indicePays))))
lesDist2  <-  lesDist2[-which(lesDist2$pays1 == lesDist2$pays2),]
lesDist2$ID <- paste(sprintf("%04d", lesDist2$pays1), sprintf("%04d", lesDist2$pays2), sep = "")
lesDist2$ID2 <- paste(sprintf("%04d", lesDist2$pays2), sprintf("%04d", lesDist2$pays1), sep = "")
lesDist2$match <- match(lesDist2$ID, lesDist$ID)
lesDist2[is.na(lesDist2$match),"match"] <- match(lesDist2$ID2[is.na(lesDist2$match)], lesDist$ID)
lesDist2$dist <- lesDist[lesDist2$match, "dist"]
lesDist2 <- lesDist2[,c("pays1", "pays2", "dist")]

lesDist <- lesDist2
rm(lesDist2)

# Let's add countries names
lesDist$pays1 <- world.map$names[lesDist$pays1]
lesDist$pays2 <- world.map$names[lesDist$pays2]

# Now, only get the countries from the dataset
allCountries <- sort(unique(regmatches(world.map$names, regexpr('^([A-Za-z]*[^:])*', world.map$names, ignore.case = TRUE))))
newLesDist <- data.frame(pays1=character(),pays2=character(),dist=double())
for (country1 in allCountries) {
  print(paste("Processing country: ",country1))
  for (country2 in allCountries) {
    if (country1 == country2) {
      next
    }
    countryDists1 <- lesDist[which( grepl(paste('^',country1,sep=''), lesDist$pays1) & grepl(paste('^',country2,sep=''), lesDist$pays2)), ]
    countryDists2 <- lesDist[which( grepl(paste('^',country2,sep=''), lesDist$pays1) & grepl(paste('^',country1,sep=''), lesDist$pays2)), ]
    minDist1 <- min(countryDists1$dist)
    minDist2 <- min(countryDists2$dist)
    trueMinDist <- NA
    if (minDist1) {
      trueMinDist <- minDist1
    } else {
      trueMinDist <- minDist2
    }
    result <- cbind(pays1 = country1, pays2 = country2, dist = trueMinDist)
    newLesDist <- rbind(newLesDist, result)
  }
}

write.csv(newLesDist, file="countries_distances.csv")