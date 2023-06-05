library(gdata)
begin()
write.fwf(iris, file = 'fwf.txt', formatInfo = TRUE, colnames = FALSE)
