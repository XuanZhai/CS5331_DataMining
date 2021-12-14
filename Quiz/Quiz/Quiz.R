install.packages("xlsx")
library(xlsx)




mlb <- xlsx::read.xlsx2("https://michael.hahsler.net/SMU/DS_Workshop_Intro_R/examples/MLB_cleaned.xlsx", sheetIndex = 1)