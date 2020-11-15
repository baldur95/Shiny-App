library(shiny)
library(tidyverse)
library(elo)
library(shinydashboard)

###setting dashboard skin###
skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "black"



df <- read.csv("data.csv")
df %>% colnames()

                                ####### CLEANING DATA #######

#Separate red and blue corner and clean names 
r_df <- df %>% 
  select_at(vars(starts_with("R_"),contains("Winner"))) %>%
  mutate(key = row_number()) %>% 
  rename_all(.funs = function(x) { str_replace(x, "R_", "")}) 

b_df <- df %>% 
  select_at(vars(starts_with("B_"),contains("Winner"))) %>% 
  mutate(key = row_number()) %>% 
  rename_all(.funs = function(x) { str_replace(x, "B_", "")})
shared_features <- df %>% select(date, location, title_bout, weight_class, no_of_rounds) %>% mutate(key = row_number())

#Combines data to a tidy format for each fighter and the fight event stats 
tidy_df <- rbind(
  r_df %>% 
    mutate(Winner = case_when(
      Winner == "Red" ~ 1,
      Winner == "Blue" ~ 0,
      TRUE ~ .5)),
  b_df %>% 
    mutate(Winner = case_when(
      Winner == "Blue" ~ 1,
      Winner == "Red" ~ 0,
      TRUE ~ .5)))  %>% 
  left_join(shared_features, by = "key") %>% 
  rename("match_id" = "key", 
         "winner" = "Winner")

#Joins the tidy format together into a head to head match format 
elo_df1 <- tidy_df %>% 
  select(match_id, date, fighter, winner, weight_class) %>% 
  left_join(tidy_df %>% 
              select(match_id, fighter),
            by = "match_id") %>% 
  select(match_id, date, fighter = fighter.x, opponent = fighter.y, winner, weight_class) %>% 
  filter(fighter != opponent) %>% #Applies filtering to remove duplicated fighters 
  arrange(fighter, date) %>% 
  group_by(match_id) %>% 
  slice(1) %>% 
  ungroup()

elo_df<-as.data.frame(elo_df1)
colnames(elo_df)<-c("match_id","date","fighter","opponent","winner","weight_class")
elo_df$fighter
                        ################# CREATING DATA WITH ELO RATINGS ############

# Creating data frame with elo rating for a given k
# we don't have elo rating in our raw data
create_elo_data <- function(k){
  temp_df <- elo.run(winner ~ fighter + opponent, k = k, 
                     data = elo_df %>% arrange(fighter, date)) %>% 
    as_tibble() %>% 
    cbind(elo_df %>% arrange(fighter, date) %>% select(match_id)) %>% 
    select(team.A, team.B, elo.A, elo.B, match_id)
  
  rbind(temp_df %>% 
          select_at(vars(contains(".A"), contains("match_id"))) %>% 
          rename_all(.funs = function(x) str_replace(x, ".A", "")),
        temp_df %>% 
          select_at(vars(contains(".B"), contains("match_id"))) %>% 
          rename_all(.funs = function(x) str_replace(x, ".B", ""))) %>% 
    rename("fighter" = "team") %>% 
    left_join(elo_df %>% 
                select(fighter, date, weight_class, match_id),
              by = c("fighter", "match_id")) %>% 
    mutate(date = as.Date(date))
}

create_elo_data(20) %>% colnames()

title1<-tags$a("UFC")


                        ################## UI SIDE ####################

ui <- dashboardPage(
  dashboardHeader(title = "UFC Rating Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Information",
               tabName = "info_tab",
               icon = icon("info")),
      menuItem("Weight Class",
               tabName = "weight_class_tab",
               icon = icon("dumbbell")),
      menuItem("Head to Head",
               tabName = "head_tab",
               icon = icon("fist-raised")),
      menuItem("Built by",
               tabName = "built_tab",
               icon = icon("hard-hat"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "info_tab",
              fluidRow(infoBoxOutput("count_", width = 3), infoBoxOutput("wclass_", width = 3),infoBoxOutput("mind_", width = 3),infoBoxOutput("maxd_", width = 3)),
              fluidRow(infoBoxOutput("text2",width=12),infoBoxOutput("text3",width=12),infoBoxOutput("text4",width=12),infoBoxOutput("text5",width=12),infoBoxOutput("text7", width = 3))
              )
              
              
      ,
      tabItem(tabName = "weight_class_tab",
              box(uiOutput("weight_class_selector_1"),title = "Select Weight Class",height = 140),
              box(sliderInput(inputId = "v_k_1",
                              min = 1, 
                              max = 100,
                              label="",
                              value = 20),title = "Select K for ELO",height = 140),
              box(plotOutput("elo_timeseries"),title = "ELO Ratings of fighters over the years",height = 480),
              box(plotOutput("elo_dist"),title = "ELO Rating Histogram",height = 480),
              box(tableOutput("top_5_table"),title="Top 5 Fighters"),
              fluidRow(title="",box(imageOutput("image1",height = "auto"),width=1))
          
              
      ),
      tabItem(tabName = "head_tab",
              box(uiOutput("weight_class_selector_2"),title = "Select Weight Class",height = 140),
              box(sliderInput("v_k_2",
                              label = "K for ELO",
                              min = 1,
                              max = 100,
                              value = 20),title = "Select K for ELO",height = 140),
              fluidRow(box(uiOutput("fighter_selector"),height = 120),box(uiOutput("opponent_selector"),height = 120)),
              fluidRow(box(valueBoxOutput("fighter_card",width = 10),height = 120),box(valueBoxOutput("opponent_card",width = 10),height = 120))
              ),
      tabItem(tabName = "built_tab",
              fluidRow(title="",box(imageOutput("image2"),width = 6,height = 260)),
              fluidRow(infoBoxOutput("text6",width=12))
              )
              
              
      )
    )
)
  



                                             ############ SERVER SIDE ###########

server <- function(input, output) {

##Info Tab Data###
  st1<- "This App helps user to compare ELO ratings of different UFC Fighters."
  st2<-"   ELO Rating system is a method for calculating the relative skill levels of players in zero-sum games."
  st3<-"A player's Elo rating is represented by a number which may change depending on the outcome of rated games played. After every game, the winning player takes points from the losing one. The difference between the ratings of the winner and loser determines the total number of points gained or lost after a game. If the high-rated player wins, then only a few rating points will be taken from the low-rated player. However, if the lower-rated player scores an upset win, many rating points will be transferred. The lower-rated player will also gain a few points from the higher rated player in the event of a draw. This means that this rating system is self-correcting."
  
  st4<-"The K factor/value represents the rate of speed a player's Elo rating will converge against its true value. If a player plays with a consistent strength, their Elo will be pretty stable. If they are still learning, it is probably increasing. A higher K factor makes the Elo value increase faster as long as the player has better than expected results, and decrease faster with worse than expected results. On the other hand, a too high K factor will make a stable player's Elo fluctuate too much, since the result of each game/tournament will influence the Elo calculation to a higher degree."
  st5<-"To put it simply, the K-Factor is a cap on how many Elo points a player can win or lose from a single match. K-Factor is a critical part of maintaining an accurate rating."

  st6<-"There are two different tabs in this app, 1st: Weigh Class and 2nd: Head to Head"
  
  st7<-"Weight Class Tab:"
  st8<-  "- Option to select different weight categories and K value to calculate ELO rating"
  st9<- "- Allows user to view top 5 fighters in a particular weight class."
  st10<- "- Evolution of ELO ratings of these top 5 fighters compared to rest of the fighters in their weight class over the years"
  st11<-"- Histogram of ELO ratings of fighters"
  
  st12<-"Head to Head Tab:"
  st13<-"- Option to select different weight categories and K value to calculate ELO rating"
  st14<-"- Option to select a fighter and his opponent to predict win probabilities of each fighter"
  st15<-"Note: there is no option to select same fighter as his own opponent :p"
  
###Output definitions###
  
  output$text2<-renderInfoBox({
    infoBox(
      title = "ELO Rating System",
     value = HTML(paste(st2," ",st3," ",st4," ",st5," ",sep = '\n'))
      
    )
    
    
  })
  
  output$text3<-renderInfoBox({
    infoBox(
      title = "Tab Info",
      value = HTML(paste(st6,sep = '\n'))
      
    )
    
    
  })
  
  output$text4<-renderInfoBox({
    infoBox(
      title = "Weight Class Tab",
      value = HTML(paste(st12," ",st13," ",st14," ",st15,sep = '\n')),
      fill=TRUE,
      color = "yellow",
      icon = icon("dumbbell")
    )
    
    
  })
  
  output$text5<-renderInfoBox({
    infoBox(
      title = "Weight Class Tab",
      value = HTML(paste(st7," ",st8," ",st9," ",st10," ",st11,sep = '\n')),
      fill = TRUE,
      color = "red",
      icon = icon("fist-raised")
    )
    
    
  })
  
  
##my motivation text##
  
  st16<- "I have always been interested in combat sports mainly UFC and I wanted to develop an app related to it." 
  st17<- "The important question and main topic of discussion or argument has always been who is the best fighter. We can't simply answer these questions by just looking at the total number of wins or the ratio of total number of wins per losses."
  st18<- "The quality of opponents whom the fighter has defeated matters the most, in simple words: the resume of a fighter. Another important factor is the consistency of the fighter."
  
  st19<- "To answer these questions and to predict among 2 fighters who has more probability to win, I created this app. The app revolves around the ELO Rating, as ELO rating system is a fair way to match players up. Each fight has its own system and takes into consideration different metrics." 
  st20<- "I will be working on this app and adding new features, stay tuned. :D " 
  
  output$text6<-renderInfoBox({
    infoBox(
      title = "My Motivation and Peronal Thoughts",
      value = HTML(paste(st16," ",st17," ",st18," ",st19," ",st20,sep = '\n')),
      fill = TRUE,
      color = "blue",
      icon = icon("comment")
    )
    
    
  })
  
####
  
  output$text7<-renderInfoBox({
    infoBox(
      title = "Data Source: Kaggle",
      href = "https://www.kaggle.com/rajeevw/ufcdata/data#",
      subtitle = "Click here to visit the source",
      fill = TRUE,
      color = "blue",
      icon = icon("database")
    )
    
    
  })
  
    output$count_ <- renderInfoBox({
    infoBox(title = "Fighters", 
            value = length(unique(elo_df$fighter)),
            subtitle = "number of fighters in dataset",
            fill = TRUE,
            color = "red",
            icon = icon("fist-raised")) 
    
  })
  
  output$wclass_ <- renderInfoBox({
    infoBox(title = "Weight Class", 
            value = length(unique(elo_df$weight_class)),
            subtitle = "number of weight classes in dataset",
            fill = TRUE,
            color = "yellow",
            icon = icon("dumbbell")) 
    
  })
  
  dfyear<-format(as.Date(elo_df$date, format="%Y-%m-%d"),"%Y")
  
  output$mind_ <- renderInfoBox({
    infoBox(title = "Data From", 
            value = min(dfyear),
            fill = TRUE,
            color = "blue",
            icon = icon("calendar")) 
    
  })
  
  output$maxd_ <- renderInfoBox({
    infoBox(title = "Data To", 
            value = max(dfyear),
            fill = TRUE,
            color = "blue",
            icon = icon("calendar")) 
    
  })
  
  output$weight_class_selector_1 <- renderUI({
    
    weight_class_1_df <- create_elo_data(input$v_k_1)
    
    
    selectInput(inputId = "v_weight_class_1",
                label = "",
                choices = weight_class_1_df %>% 
                  select(weight_class) %>% 
                  distinct() %>% 
                  arrange(weight_class))
  })
  output$weight_class_selector_2 <- renderUI({
    
    weight_class_2_df <- create_elo_data(input$v_k_2)
    
    
    selectInput(inputId = "v_weight_class_2",
                label = "",
                choices = weight_class_2_df %>% 
                  select(weight_class) %>% 
                  distinct() %>% 
                  arrange(weight_class))
  })
 
  
  output$image1<- renderImage({
    filename<-normalizePath(file.path(paste('www/',"UFCP",'.png',sep="")))
    list(
      src=filename,
      width=100,
      height=50
    )
  },deleteFile = FALSE)
  
  output$image2<- renderImage({
    filename<-normalizePath(file.path(paste('www/',"pdp",'.jpg',sep="")))
    list(
      src=filename,
      width=400,
      height=200
    )
  },deleteFile = FALSE)
  
  output$fighter_selector <- renderUI({
    
    fighter_selector_df <- create_elo_data(input$v_k_2) %>% 
      filter(weight_class == input$v_weight_class_2) %>% 
      select(fighter) %>% 
      distinct() %>% 
      arrange(fighter)

    
    selectInput(inputId = "v_fighter",
                label = "Fighter",
                choices = fighter_selector_df)
  })
  output$opponent_selector <- renderUI({
    opponent_selector_df <- create_elo_data(input$v_k_2) %>% 
      filter(weight_class == input$v_weight_class_2) %>% 
      filter(fighter != input$v_fighter) %>% 
      select(fighter) %>% 
      distinct() %>% 
      arrange(fighter)
    
    selectInput(inputId = "v_opponent",
                label = "Opponent",
                choices = opponent_selector_df)
    
    
  })
  output$top_5_table <- renderTable({
    
    table_df <- create_elo_data(input$v_k_1)
    
    table_df %>% 
      filter(weight_class == input$v_weight_class_1) %>% 
      group_by(fighter) %>% 
      arrange(desc(elo)) %>% 
      slice(1) %>% 
      ungroup() %>% 
      top_n(elo, n = 5) %>% 
      arrange(desc(elo)) %>% 
      select(fighter, elo) %>% 
      mutate(rank = row_number())
    
    
  })
  
  output$elo_timeseries <- renderPlot({
    elo_timeseries_df <- create_elo_data(input$v_k_1) %>% 
      filter(weight_class == input$v_weight_class_1)
    
    top_5_fighters <- elo_timeseries_df %>% 
      group_by(fighter) %>% 
      arrange(desc(elo)) %>% 
      slice(1) %>% 
      ungroup() %>% 
      top_n(elo, n = 5) %>% 
      select(fighter)
    
    ggplot(data = elo_timeseries_df, aes(x = date, y = elo)) + 
      geom_point() + 
      geom_point(data = elo_timeseries_df %>% filter(fighter %in% top_5_fighters$fighter),
                 aes(x = date, y = elo, color = fighter))  + labs(y="ELO Rating",x="Year")
    
    
  })
  output$elo_dist <- renderPlot({
    elo_dist <- create_elo_data(input$v_k_1) %>% 
      filter(weight_class == input$v_weight_class_1)
    
    ggplot(data = elo_dist, aes(x = elo)) + geom_histogram(fill="purple") + labs(y="Number of Fighters",x="ELO Rating")
  })
  
  output$fighter_card <- renderValueBox({
    elo <- elo.run(winner ~ fighter + opponent,
                   k = input$v_k_2,
                   data = elo_df)
    
    fighter_prob <- round(100*predict(elo, data.frame(fighter = input$v_fighter, opponent = input$v_opponent)),0)
    
    valueBox(
      value = paste(fighter_prob, "%", sep = ""),
      subtitle = paste(input$v_fighter, " Probability", sep = ""),
      color = "blue",
      icon = icon("hand-rock")
    )
    
  })
  
  output$opponent_card <- renderValueBox({
    elo <- elo.run(winner ~ fighter + opponent,
                   k = input$v_k_2,
                   data = elo_df)
    
    opponent_prob <- round(100*predict(elo, data.frame(fighter = input$v_opponent, opponent = input$v_fighter)),0)
    
    valueBox(
      value = paste(opponent_prob, "%", sep = ""),
      subtitle = paste(input$v_opponent, " Probability", sep = ""),
      color = "red",
      icon = icon("hand-rock")
    )
    
  })
  
  
}

         ############## SHINY APP  ########
shinyApp(ui = ui, server = server)

