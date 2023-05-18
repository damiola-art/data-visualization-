library(tidyverse)
openDt <- read.csv("2022 OpenSSF Security Survey - Full Survey Reference.csv")
pdf("visualization.pdf")
newDt <- openDt[, c(19,180,187)]
newDt
View(newDt)
newDt <- newDt[-c(1,2),]
newDt <- newDt %>% rename('Industry'=q9, 'Added Intelligence'=X.131, 'Unique Software Component ID'=X.138)
View(newDt)
newDt$Industry[newDt$Industry == '1'] <- 'Agriculture'
newDt$Industry[newDt$Industry == '2'] <- 'Automotive'
newDt$Industry[newDt$Industry == '3'] <- 'Business Services'
newDt$Industry[newDt$Industry == '4'] <- 'Consumer Packaged Goods'
newDt$Industry[newDt$Industry == '5'] <- 'Construction/Engineering'
newDt$Industry[newDt$Industry == '6'] <- 'Secondary Education'
newDt$Industry[newDt$Industry == '7'] <- 'University Education'
newDt$Industry[newDt$Industry == '8'] <- 'Financial Services'
newDt$Industry[newDt$Industry == '9'] <- 'National Government'
newDt$Industry[newDt$Industry == '10'] <- 'Local Government'
newDt$Industry[newDt$Industry == '11'] <- 'Health Care'
newDt$Industry[newDt$Industry == '12'] <- 'Hospitality'
newDt$Industry[newDt$Industry == '13'] <- 'Information Technology'
newDt$Industry[newDt$Industry == '14'] <- 'Life Sciences'
newDt$Industry[newDt$Industry == '15'] <- 'Manufacturing'
newDt$Industry[newDt$Industry == '16'] <- 'Media'
newDt$Industry[newDt$Industry == '17'] <- 'Oil & Gas'
newDt$Industry[newDt$Industry == '18'] <- 'Retail'
newDt$Industry[newDt$Industry == '19'] <- 'Telecommunications'
newDt$Industry[newDt$Industry == '20'] <- 'Transportation'
newDt$Industry[newDt$Industry == '21'] <- 'Utilities'
newDt$Industry[newDt$Industry == '0'] <- 'Others'

newDt$`Added Intelligence`[newDt$`Added Intelligence` == 1] <- 'Added Intelligence'
newDt$`Unique Software Component ID`[newDt$`Unique Software Component ID` == 8] <- 'Unique Software Component ID'

View(newDt)



dtTable1 <- table(newDt$Industry, newDt$`Added Intelligence`)
#view(dtTable1)
dtTable2 <- table(newDt$Industry, newDt$`Unique Software Component ID`)
#view(dtTable2)
dtTable <- cbind(dtTable1, dtTable2)
#removes 0 value columns
dtTable <- dtTable[-c(1), c(-1,-3,-5)]

View(dtTable)
dtTable <- as.data.frame(dtTable)
#barplot(t(dtTable))
dtTable$Total <- (dtTable$`Added Intelligence` + dtTable$`Unique Software Component ID`)
dtTable
dtTable$`Added Intelligence` <- (dtTable$`Added Intelligence`/dtTable$Total)*100
dtTable$`Unique Software Component ID` <- (dtTable$`Unique Software Component ID`/dtTable$Total)*100
View(dtTable)
df <- dtTable[-3]
df <- t(df)
par(mar=c(12, 4.6, 5, 4.1))
barplot(df,
        cex.axis = 1,
        cex.names = 0.9,
        las=2,
        col = c("red", "blue"),
        legend = TRUE,
        legend.text=c("Added Intelligence", "Unique Software Component ID"),
        las=2,
        main = "Barplot of the Rate of important Activities",
        args.legend = list(bty = "n", x = "top", ncol = 2, inset = c(0, -0.13)),
        ylab = "Percentage of Important Activities",
        cex.lab=1
)
mtext("Industries", side=1, line=11)