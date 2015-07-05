## Should I
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Load packages
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
library(shinydashboard)
# library(dplyr)
library(xlsx)
# require(ggplot2)
#do the data reading bits here
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*       Wage data
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
wage.file <- "data/absEarnings.xls"
wage.data <- read.xlsx(wage.file, 2,
                       startRow = 8, endRow = 362,
                       header=FALSE,
                       colClasses = c("character",
                                      rep("numeric", 6)),
                       stringsAsFactors = FALSE)
names(wage.data) <- c("occupation", "wage_male", "wage_female",
                      "wage_person", "average_age_male",
                      "average_age_female", "average_age_person")
#split anzsco code
wage.data$anzsco_code <- substr(wage.data$occupation, 1,4)
wage.data$anzsco_desc <- substr(wage.data$occupation, 6,
                                nchar(wage.data$occupation))
#replace 0s with NAs
wage.data[(wage.data$wage_male == 0) &
            !is.na(wage.data$wage_male), c("wage_male")] <- NA
wage.data[(wage.data$wage_female == 0) &
            !is.na(wage.data$wage_female), c("wage_female")] <- NA
wage.data[(wage.data$wage_person == 0) &
            !is.na(wage.data$wage_person), c("wage_person")] <- NA
#get rid of unwanted columns
wage.data.sub <- wage.data[,c(1,4,7,8,9)]
#get rid of incomplete rows
wage.data.sub <- wage.data.sub[complete.cases(wage.data.sub), ]
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*       Public transport data
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# Month pass (28 days)	119.8	129.36
# Single ride - 2 hrs, zone 1	3.48	3.76
transit <-data.frame(monthly = c(119.8, 129.36),
                     singleride = c(3.48, 3.76))
row.names(transit) <- c("Adelaide", "Melbourne")
transit$state <- rownames(transit)
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*       Occupations in demand
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
demand.file <- "data/demand.csv"
demand.data <- read.csv(demand.file,
                      # skip=1,
                      # header=FALSE,
                      colClasses = c("character"),
                      stringsAsFactors = FALSE)
demand.data$ANZSCO4 <- substr(demand.data$ANZSCO, 1, 4)
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*       Dwellings mean price thing
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
houseprice.file <- "data/dwellings.xlsx"
houseprice.data <- read.xlsx(houseprice.file, 2,
                       # startRow = 8, endRow = 362,
                       # header=FALSE,
                       colClasses = c("character",
                                      rep("numeric", 9)),
                       stringsAsFactors = FALSE)
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*       Dwellings mean price thing
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
pop.file <- "data/pop.xlsx"
pop.data <- read.xlsx(pop.file, 1,
                             colClasses = c("character",
                                            rep("numeric", 8)),
                             stringsAsFactors = FALSE)
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*       Functions
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
getWage <- function(xoccupaton) {
  w <- wage.data.sub[wage.data.sub$anzsco_code == xoccupaton, c("wage_person")]
  return(w)
}
# usage: getWage("1321") returns [1] 2067.7
is.demand <- function(xoccupation, xstate) {
  data <- demand.data[demand.data$State == xstate, ]
  outdat <- data[data$ANZSCO4 == xoccupation,]
  outdat$Occupation <- paste(outdat$Occupation, outdat$ANZSCO)
  #outdat[,c(1:3)]
  if (xoccupation %in% data$ANZSCO4)
    {return(paste("Following related occupations in demand:<br>",
                  paste(outdat$Occupation, sep = "", collapse=", ")))} else
  {return("Not in demand")}
}
#usage:
# > is.demand("1321", "SA")
# [1] "is in demand"
# > is.demand("1321", "VIC")
# [1] "is not in demand"
#get houseprice
getHousingPrice <- function(xstate) {
  b <- houseprice.data[c(8), c(xstate) ] #mean house price
  return(format(round(b*1000,0), format=d, big.mark=","))
}
#usage
# > getHousingPrice("SA")
# [1] 416700
# > getHousingPrice("VIC")
# [1] 569100
#get houseindex
getHousingIndex <- function(xstate, xoccupation) {
  w <- getWage(xoccupation) #average weekly wage
  b <- houseprice.data[c(8), c(xstate) ] #mean house price
  s <- 0.25 #savings rate
  n <- (b*1000*.2)/(w*s) #housing index AKA dingo index
  return(round(n,1))
}

#usage
# > getHousingIndex("SA", "1111")
# [1] 120.5642
# > getHousingIndex("VIC", "1111")
# [1] 164.6582
#get population figures
getPop <- function(xstate, xage) {
  male <- pop.data[pop.data$State == xstate, c(xage + 1)]
  female <- pop.data[pop.data$State == xstate, c(xage*2 + 1)]
#   tot <- pop.data[pop.data$State == xstate, c(8)] +
#     pop.data[pop.data$State == xstate, c(9)]
#   prop.male <- 100 * male / pop.data[pop.data$State == xstate, c(8)]
#   prop.female <- 100 * female / pop.data[pop.data$State == xstate, c(9)]
  return(paste0("Total population in my age group: ",
               format(male + female, format=d, big.mark=","),
               " (", round(male * 100/ (male + female),2),
               "% males and ",
               round(female * 100 / (male + female), 2),
               "% females).<br>"
#                "The males of this group account for ",
#                round(prop.male, 1),
#                "% of total males. And the females of this group account for ",
#                round(prop.female, 1),
#                "% of total females"
               ))
}
#usage
#getPop("Melbourne", 1)
getTransit <- function(xstate, xtype) {
  return(transit[transit$state == xstate, c(xtype)])
}
#drawing a comparison graph (ggplot2 bar graph)
drawGraph <- function(a, b) {
  dat <- data.frame(
    state = factor(c("VIC","SA"), levels=c("VIC","SA")),
    val = c(a, b)
  )
  gplot <- ggplot(data=dat, aes(x=state, y=val, fill=state)) +
    geom_bar(colour="black", stat="identity") +
    guides(fill=FALSE) + coord_flip() +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          title = element_text(colour="black", size = 16),
          axis.title.x = element_text(hjust=1,colour="deepskyblue4", size = 10),
          axis.title.y = element_text(vjust=90, colour="deepskyblue4", size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.position = "none",
          axis.text.y  = element_text(hjust=1,colour="firebrick4", size = 14),
          #         axis.text.y  = element_blank(),
          axis.ticks  = element_blank()) +
    ggtitle("") +
    xlab("") + ylab("")
  return(gplot)
}
#usage
# drawGraph(4,5)
drawGraph <- function(a, b, xtitle) {
  dat <- data.frame(
    state = factor(c("VIC","SA"), levels=c("VIC","SA")),
    val = c(a, b)
  )
  gplot <- ggplot(data=dat, aes(x=state, y=val, fill=state)) +
    geom_bar(colour="black", stat="identity") +
    guides(fill=FALSE) + coord_flip() +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          title = element_text(colour="black", size = 16),
          axis.title.x = element_text(hjust=1,colour="deepskyblue4", size = 10),
          axis.title.y = element_text(vjust=90, colour="deepskyblue4", size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.position = "none",
          axis.text.y  = element_text(hjust=1,colour="firebrick4", size = 14),
          #         axis.text.y  = element_blank(),
          axis.ticks  = element_blank()) +
    ggtitle(xtitle) +
    xlab("") + ylab("")
  return(gplot)
}
#usage
#drawGraph(12,14)
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*       Shiny server
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function(input, output, session) {
output$outWage <- renderText(paste0("<b>$",
                                   getWage(input$occupation), "</b><br>"))
output$outDemand1 <- renderText(paste(is.demand(input$occupation, input$selectTo)))
output$outDemand2 <- renderText(paste(is.demand(input$occupation, "SA")))

output$outTransit2 <- renderText(paste0("Cost of monthly pass (28 days) is <b>$",
                                       getTransit("Adelaide", "monthly"),
                                       "</b><br> The cost of single trip is <b>$",
                                       getTransit("Adelaide", "singleride"), "</b>"))
output$outTransit1 <- renderText(paste0("Cost of monthly pass (28 days) is <b>$",
                                       getTransit("Melbourne", "monthly"),
                                       "</b><br> The cost of single trip is <b>$",
                                       getTransit("Melbourne", "singleride"),"</b>"))
output$outHousing1 <- renderText(paste("At a 25% savings rate, it will take me <b>",
                                      getHousingIndex(input$selectTo, input$occupation),
                                      "weeks</b> to save a 20% deposit."))
output$outHousing2 <- renderText(paste("At a 25% savings rate, it will take me <b>",
                                       getHousingIndex("SA", input$occupation),
                                       "weeks</b> to save a 20% deposit."))
output$outHousingPrice2 <- renderText(paste0("Mean state residential housing price :<b>$",
                                       getHousingPrice("SA"), "</b>"))
output$outHousingPrice1 <- renderText(paste0("Mean state residential housing price :<b>$",
                                            getHousingPrice("VIC"),"</b>"))
output$outPop2 <- renderText(getPop("Melbourne",
                                    as.numeric(input$age)))
output$outPop1 <- renderText(getPop("Adelaide",
                                    as.numeric(input$age)))
# output$graphTransit <- renderPlot(drawGraph(12,16.7, "Cost of montly pass"),
#                                   height = 170)
}
