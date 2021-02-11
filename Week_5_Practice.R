#Week 5 - Chapter 7 - Sections 1 through 5

install.packages("car")
install.packages("QuantPsyc")

library(Rcmdr)

album1 <- read.delim("Album Sales 1.dat", header = TRUE)

#Run a regression
albumSales.1 <- lm(album1$sales ~ album1$adverts)
summary(albumSales.1)
