library(shiny)
library(RJSONIO)
library(ggplot2)
library(grid)
library(gridExtra)
library(png)
library(jpeg)
library(RCurl)
library(hexbin)
library(plyr)
players <- fromJSON("http://stats.nba.com/stats/commonallplayers?IsOnlyCurrentSeason=1&LeagueID=00&Season=2015-16")
playersDF <- data.frame(matrix(unlist(players$resultSets[[1]][[3]]), byrow = TRUE, ncol = 12))
colnames(playersDF) <- players$resultSets[[1]][[2]]
teams <- levels(as.factor(playersDF$TEAM_NAME))[2:31]

court <- rasterGrob(readJPEG("court.jpg"),
                    width=unit(1,"npc"), height=unit(1,"npc"))
season <- "2015-16"

shinyServer(function(input, output) {
    output$teamSelect <- renderUI({
        selectInput(inputId = "inTeam", label = "Choose a team",
                    choices = teams, selected = "76ers") 
    })
    teamData <- reactive({
        subset(playersDF[playersDF$GAMES_PLAYED_FLAG %in% "Y",], TEAM_NAME == input$inTeam)
    })
    teamPlayers <- reactive({
        as.character(teamData()$DISPLAY_LAST_COMMA_FIRST)
    })
    output$playerSelect <- renderUI({
        selectInput(inputId = "player", label = "Choose a player",
                    choices = teamPlayers(), selected = teamPlayers()[1] )
    })
    
    playerShotData <-  eventReactive(input$go,{
        # if (input$goButton > 0){ 
        playerID <- teamData()$PERSON_ID[teamData()$DISPLAY_LAST_COMMA_FIRST == input$player ]
        shotChartURL <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=",season ,"&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=",playerID,"&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=",season ,"&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0", sep = "")
        playerData <- fromJSON(shotChartURL)
        playerShotData <- data.frame(matrix(unlist(playerData$resultSets[[1]][[3]]), ncol=21, byrow = TRUE))
        colnames(playerShotData) <- playerData$resultSets[[1]][[2]]
        playerShotData$LOC_X <- as.numeric(as.character(playerShotData$LOC_X))
        playerShotData$LOC_Y <- as.numeric(as.character(playerShotData$LOC_Y))
        playerShotData$SHOT_DISTANCE <- as.numeric(as.character(playerShotData$SHOT_DISTANCE))
        playerShotData
        # }
    })
    playerShotHalf <- reactive({
        playerShotData()[which(!playerShotData()$SHOT_ZONE_BASIC=='Backcourt'), ]
    })
    output$shotChart <- renderPlot({
        playerShotHalf <- playerShotData()[which(!playerShotData()$SHOT_ZONE_BASIC=='Backcourt'), ]
        playerShotSimple <- ddply(playerShotHalf, .(SHOT_ZONE_BASIC, SHOT_ZONE_AREA), summarize, 
                                  SHOTS_ATTEMPTED = length(SHOT_MADE_FLAG),
                                  SHOTS_MADE = sum(as.numeric(as.character(SHOT_MADE_FLAG))),
                                  MLOC_X = mean(LOC_X),
                                  MLOC_Y = mean(LOC_Y))
        # calculate shot zone accuracy and add zone accuracy labels
        playerShotSimple$SHOT_ACCURACY <- (playerShotSimple$SHOTS_MADE / playerShotSimple$SHOTS_ATTEMPTED)
        playerShotSimple$SHOT_ACCURACY_LAB <- paste(as.character(round(100 * playerShotSimple$SHOT_ACCURACY, 1)), "%", sep="")
        plotTheme <- theme(line = element_blank(),
                           axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           axis.text.x = element_blank(),
                           axis.text.y = element_blank(),
                           legend.title = element_blank(),
                           legend.text=element_text(size = 12),
                           plot.title = element_text(size = 12, lineheight = 1.2, face = "bold"))
        ggplot(playerShotSimple) + 
            annotation_custom(court, -250, 250, -52, 418) +
            geom_point(aes(x = LOC_X, y = LOC_Y, colour = SHOT_ZONE_AREA),data = playerShotHalf, size = 1) +
            geom_text(aes(x=MLOC_X, y=MLOC_Y, label = SHOT_ACCURACY_LAB), color = "black", vjust = 1.4, size = 5) +
            guides(alpha = FALSE, size = FALSE) + xlim(250, -250) + ylim(-50, 420) + coord_fixed() +
            ggtitle("Shot Accuracy") + plotTheme
        # }
    }, width = 600, height = 400)
    
    clickData <- reactive({
        printData <- subset(playerShotHalf(), select = c(LOC_X, LOC_Y, EVENT_TYPE, ACTION_TYPE) )
        nearPoints(printData, input$plot_click, maxpoints = 10, threshold = 10)
    })
    output$click_info <- renderPrint({
        # str(input$plot_click)
        printData2 <- subset(clickData(),  select = c(EVENT_TYPE, ACTION_TYPE))
        printData2
    })
})
