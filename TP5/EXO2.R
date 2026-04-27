library(readxl)

data <- read_excel("C:/Users/asusz/Downloads/données3.xlsx")

par(mfrow=c(3,3))

for (i in 1:7) {
  x <- data[[i]]
  
  plot(x, type="l", main=paste("Series X", i))
  
  acf(x, main=paste("ACF X", i))
  
  pacf(x, main=paste("PACF X", i))
}