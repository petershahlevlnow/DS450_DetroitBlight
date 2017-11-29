# The following code is based on the following report: 
# https://rstudio-pubs-static.s3.amazonaws.com/194529_5b7aff21a29541fb94b8f4176e42abf2.html
# by Paulo Cardso

gpsParse <- function(loc.txt, p="\\(.*\\)"){
  r <- regexpr(p, loc.txt)
  out <- rep(NA, length(r))
  out[r != -1] <- regmatches(loc.txt, r)
  out <- gsub("[()]", "", out)
  lat <- unlist(lapply(out, function(x) as.numeric(strsplit(x, split=",")[[1]][1])))
  long <- unlist(lapply(out, function(x) as.numeric(strsplit(x, split=",")[[1]][2])))
  list(lat=lat, long=long)
}
