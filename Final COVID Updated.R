install.packages("ggplot2")
library(ggplot2)
ncol(kffmarch21)
## subset races
white <- subset(kffmarch21,V4  == "White")
black <- subset(kffmarch21,V4  == "Black or African-American")
asian <- subset(kffmarch21,V4  == "Asian")
other <- subset(kffmarch21,V4  == "Other or mixed race")

## compare races to vaccination status
table(white$V32)
table(black$V32)
table(asian$V32)
table(other$V32)

## compare vaccination rates by race
kffmarch21$fwhite2dose <- ifelse(kffmarch21$V4 == "White" &
                                   kffmarch21$V32 == "Yes, got both doses of a two-dose vaccine", 1, 0)
kffmarch21$fwhitefull1dose <- ifelse(kffmarch21$V4 == "White" &
                                   kffmarch21$V32 == "Yes, got one-dose vaccine", 1, 0)
kffmarch21$black2dose <- ifelse(kffmarch21$V4 == "Black or African-American" &
                                   kffmarch21$V32 == "Yes, got both doses of a two-dose vaccine", 1, 0)
kffmarch21$blackfull1dose <- ifelse(kffmarch21$V4 == "Black or African-American" &
                                  kffmarch21$V32 == "Yes, got one-dose vaccine", 1, 0)

whitefullvac <- sum(kffmarch21$fwhite2dose + kffmarch21$fwhitefull1dose)
blackfullvac <- sum(kffmarch21$black2dose + kffmarch21$blackfull1dose)
283/1038
138/532
mean(whitefullvac)
mean(blackfullvac)
values <- c(25.93985,27.26397)
differenceinrace <- mean(whitefullvac - blackfullvac)

barplot(values,
        main = "Black vs. White Respondents Fully Vaccinated",
        ylim = c(0,50),
        names.arg = c("Black","White"),
        ylab = "Percentage Fully Vaccinated",
        col = "darkred")

## compare political affiliation to vaccination rate
kffmarch21$Republican <- ifelse(kffmarch21$V116 == "Republican", 1, 0)
kffmarch21$Democrat <- ifelse(kffmarch21$V116 == "Democrat", 1, 0)

kffmarch21$fullvac <- ifelse(kffmarch21$V32 == "Yes, got both doses of a two-dose vaccine", 1, 0)
kffmarch21$onedosefull <- ifelse(kffmarch21$V32 == "Yes, got one-dose vaccine", 1, 0)
fullyvaccinated <- sum(kffmarch21$fullvac + kffmarch21$onedosefull)

sum(kffmarch21$Republican)
sum(kffmarch21$Democrat)
342/479
731/479

values2 <- c(71.40,152.61)
barplot(values2,
        main = "Republican vs. Democrat Respondents Fully Vaccinated",
        ylim = c(0,200),
        names.arg = c("Republican","Democrat"),
        ylab = "Percentage Fully Vaccinated",
        col = "green")
