# ---------- Loading Packages ----------
  
library(tidyverse)
library(plotly)
library(shiny)
library(radarchart)
library(dendextend)
library(shinydashboard)
library(d3heatmap)

# ---------- Loading Pokemon Dataset ----------

poke <- read.csv("Pokemon.csv")

# ---------- Image Data ----------

pic.id <- c()

for(ii in 1:nrow(poke)) {
  
  if (grepl("Mega ", poke$Name[ii]) == TRUE & grepl(" X", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-mega-x.png", sep = "")
  
  else if (grepl("Mega ", poke$Name[ii]) == TRUE & grepl(" Y", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-mega-y.png", sep = "")
  
  else if (grepl("Primal ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-primal.png", sep = "")
  
  else if (grepl("Attack ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-attack.png", sep = "")
  
  else if (grepl("Defense ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-defense.png", sep = "")
  
  else if (grepl("Normal ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-normal.png", sep = "")
  
  else if (grepl("Speed ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-speed.png", sep = "")
  
  else if (grepl("Plant ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-plant.png", sep = "")
  
  else if (grepl("Sandy ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-sandy.png", sep = "")
  
  else if (grepl("Trash ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-trash.png", sep = "")
  
  else if (grepl("Fan ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-fan.png", sep = "")
  
  else if (grepl("Frost ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-frost.png", sep = "")
  
  else if (grepl("Heat ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-heat.png", sep = "")
  
  else if (grepl("Mow ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-mow.png", sep = "")
  
  else if (grepl("Wash ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-wash.png", sep = "")
  
  else if (grepl("Altered ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-altered.png", sep = "")
  
  else if (grepl("Origin ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-origin.png", sep = "")
  
  else if (grepl("Land ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-land.png", sep = "")
  
  else if (grepl("Sky ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-sky.png", sep = "")
  
  else if (grepl("Standard ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-standard.png", sep = "")
  
  else if (grepl("Zen ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-standard.png", sep = "")
  
  else if (grepl("Incarnate ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-incarnate.png", sep = "")
  
  else if (grepl("Therian ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-therian.png", sep = "")
  
  else if (grepl("Black ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-black.png", sep = "")
  
  else if (grepl("White ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-white.png", sep = "")
  
  else if (grepl("Ordinary ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-ordinary.png", sep = "")
  
  else if (grepl("Resolute ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-resolute.png", sep = "")
  
  else if (grepl("Pirouette ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-pirouette.png", sep = "")
  
  else if (grepl("Aria ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "", sep = "")
  
  else if (grepl("Blade ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-blade.png", sep = "")
  
  else if (grepl("Shield ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-shield.png", sep = "")
  
  else if (grepl("Confined", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "", sep = "")
  
  else if (grepl("Unbound", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-unbound.png", sep = "")
  
  else if (grepl("Mega ", poke$Name[ii]) == TRUE) pic.id[ii] <- paste(poke$X.[ii], "-mega.png", sep = "")
  
  else pic.id[ii] <- paste(poke$X.[ii], ".png", sep = "")
  
}

poke <- read.csv("Pokemon.csv")
poke$Name = as.character(poke$Name)
newnames <- poke$Name
for(ii in 1:nrow(poke)) {
  if(grepl("Mega ", poke$Name[ii]) == TRUE) {newnames[ii] <- sub(".*Mega ", "Mega ", poke$Name[ii])}
}
newnames
poke$Name <- newnames

namelist = as.list(poke$Name)
names(namelist) = poke$Name

# ---------- Colors data ----------

colors = data.frame(type = unique(poke$Type.1), color = c("darkolivegreen2", "darkorange1", "dodgerblue1", "darkolivegreen3", "cornsilk3", "darkmagenta", "gold1", "burlywood2", "lightpink1", "brown", "deeppink", "goldenrod4", "darkviolet", "darkslategray1", "darkslateblue", "chocolate4", "azure3", "mediumpurple1"))


# ---------- Stat Choices ----------

stat_choices = c("Total Points" = "Total", "Health Points" = "HP", "Attack Points" = "Attack", "Defense Points" = "Defense", "Special Attack" = "Sp..Atk", "Special Defense" = "Sp..Def", "Speed" = "Speed")


# ---------- Defining UI ----------

ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Pokedex 2.0"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Info", tabName ="info"),
                        menuItem("Pokemon Stats", tabName = "pokestats"),
                        menuItem("Compare Types", tabName = "types",
                                 menuSubItem("Radar Plot", tabName = "radar"),
                                 menuSubItem("Density Plot", tabName = "density"),
                                 menuSubItem("Super Effectiveness", tabName = "super")),
                        menuItem("Choose the Best Pokemon", tabName = "pokes",
                                 menuSubItem("Individual Stats", tabName = "pokemon"),
                                 menuSubItem("Overview", tabName = "overview")),
                        menuItem("Legendaries",
                                 menuSubItem("Legendaries by Statistic", tabName = "legendstat"),
                                 menuSubItem("Grouping Legendaries", tabName = "legendgroup")),
                        menuItem("Generational History",
                                 menuSubItem("How Types Have Changed", tabName = "gentypechange"),
                                 menuSubItem("Do Generations Favor Types", tabName = "gentypefav")),
                        menuItem("More",
                                 menuSubItem("Make Your Own Comparisons", tabName = "random"),
                                 menuSubItem("Credits", tabName = "credits"))
                      ) # closes sidebar menu
                    ), # closes dash sidebar
                    dashboardBody(
                      
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      ),
                      
                      tabItems(
                        tabItem(tabName = "info",
                                fluidRow(box(h1("Welcome to the Pokedex 2.0!"),
                                             p("This is an analytical tool to compare statistics of different Pokemon types, generations, and more! Using the Pokemon Dataset from Kaggle, we have compiled many tools to help analyze which Pokemon are right for you. 
                                               First, check out the 'Pokemon Stats' Tab to see statistics on individual Pokemon from all types and generations. 
                                               Then, try out the 'Compare Types' Tab to see if a certain type of Pokemon fits your playstyle, or is more advantageous overall. 
                                               Once you've found your favorite type, you can use the 'Choosing the Best Pokemon' Tab to see which Pokemon in that type are the most powerful.
                                               You can also use this tab to compare individual Pokemon's statistics. 
                                               The most powerful Pokemon of all are the Legendary Pokemon of course. 
                                               You can use the 'Legendaries' Tab to compare different Legendary Pokemon and decide which one to track down. 
                                               The 'Generational History' Tab shows how the pokemon have changed through all six generations and which types have become more favorable over time. 
                                               Under the 'More' Tab you can do your own analysis on the statistics of Pokemon and see what interesting findings you can make, as well as learn more about the creators of this app and the dataset. 
                                               Enjoy!"),
                                             width = 12, imageOutput("background"), align = "center")
                                         )
                        ),
                        
                        tabItem(tabName = "pokestats",
                                fluidRow(
                                  box(width = 12,
                                      h1("Lookup Individual Pokemon Statistics"),
                                      p("This page works just like a traditional Pokedex. Just type in the name of the Pokemon you want to get statistics for, and you can view all of that Pokemon's info!"),
                                      inputPanel(
                                        
                                        selectizeInput("pokeselectize", label = "Search Pokemon:", choices = namelist,
                                                       options = namelist, multiple = FALSE)
                                      )
                                  )),
                                fluidRow(box(width = 6,
                                             tableOutput("pokestattable"), color = "fuchsia"),
                                         box(width = 6, align = "center", imageOutput("pokepic"), color = "purple")
                                         )
                        ),
                        
                        tabItem(tabName = "radar",
                                box(h1("Radar Plot"),
                                             p("Here, you can produce a radar plot comparing all Pokemon types and their average statistics. Select what types you want to compare and see if you can find the best type that matches your play style, or would be advantageous in an upcoming battle! Click the 'Include Legendary' button to include legendary Pokemon in your averages. Be sure to check out the Density Plot to get a more detailed view of the distribution of Pokemon within your favorite types!"), width = 12),
                                             box(inputPanel(
                                               checkboxGroupInput("typeradar", label = "Types:",
                                                                  choices = c("Grass", "Fire", "Water", "Bug", "Normal", "Poison", "Electric", "Ground", "Fairy", "Fighting", "Psychic", "Rock", "Ghost", "Ice", "Dragon", "Dark", "Steel", "Flying"), inline = TRUE, selected = c("Grass", "Fire", "Water")),
                                               
                                               
                                               checkboxInput("legend", label = "Include Legendary", FALSE)
                                             ), width = 4),
                                box(fluidRow(chartJSRadarOutput("radarplot", height = 250)), width = 8)),
                        
                        tabItem(tabName = "density",
                                fluidRow(box(width = 12,
                                             h1("Density Plots by Type"),
                                             p("Here, you can produce density plots to see how the Pokemon within each type are distributed by their statistics. Does your favorite type have lots of high powered Pokemon, or just a few? Select multiple types to overlay their density plots and compare!"))),
                                             fluidRow(box(inputPanel(
                                               
                                               checkboxGroupInput("typedensity", label = "Types:",
                                                                  choices = c("Grass", "Fire", "Water", "Bug", "Normal", "Poison", "Electric", "Ground", "Fairy", "Fighting", "Psychic", "Rock", "Ghost", "Ice", "Dragon", "Dark", "Steel", "Flying"), selected = c("Grass", "Fire", "Water"), inline = TRUE),
                                               
                                               selectInput("statdensity", label = "Statistic:", choices = stat_choices, selected = 
                                                             "Grass")
                                               
                                               
                                             ), width = 4),
                                             box(plotOutput("density"), width = 8))
                        ),
                        
                        tabItem(tabName = "pokemon",
                                fluidRow(box(width = 12,
                                             h1("All Pokemon Stats by Type"),
                                             p("Here, you can compare all regular Pokemon in your chosen type. Select your favorite type and statistic of comparison and refer to the resulting bar plot showing all regular Pokemon of that type and their statistic!"),
                                             inputPanel(
                                               selectInput("typeallpoke", label = "Type:",
                                                           choices = c("Grass", "Fire", "Water", "Bug", "Normal", "Poison", "Electric", "Ground", "Fairy", "Fighting", "Psychic", "Rock", "Ghost", "Ice", "Dragon", "Dark", "Steel", "Flying"), selected = "Grass"),
                                               
                                               selectInput("statallpoke", label = "Statistic:",
                                                           choices = stat_choices)
                                               
                                             )
                                )
                                ),
                                fluidRow(plotOutput("allpoke", height = 1000))
                        ),
                        
                        tabItem(tabName = "overview",
                                box(h1("Overview of Individual Pokemon"),
                                    p("Similar to the type radar plot, this plot helps you compare individual Pokemon. Search for different Pokemon to overlay their radar plots and see how their statistics stack up against one another!"), width = 12),
                                box(inputPanel(
                                  selectizeInput("pokes", label = "Pokemon to Compare:",
                                                     choices = namelist, options = namelist, multiple = TRUE, selected = c("Bulbasaur", "Charmander", "Squirtle"))
                               
                                ), width = 4),
                                box(fluidRow(chartJSRadarOutput("overview")), width = 8)),
                        
                        tabItem(tabName = "super",
                                box(width = 12,
                                    h1("Super Effectiveness"),
                                    p("When preparing for more difficult battles it is important to consider what types of Pokemon your opponent is using and which types will be super effective against those types. Here, you can see a plot depicting how effective or ineffective types' Attack on the rows are against which other types' Defense on the columns. If you have a tough battle coming up, consider using types that will be super effective against your opponent and reconsider using Pokemon that will be at a disadvantage!")),
                                box(d3heatmapOutput("superheat"), width = 12, align = "center")
                          
                        ),
                        
                        tabItem(tabName = "legendstat",
                                box(width = 12,
                                             h1("Bar Plots of Legendary Pokemon"),
                                             p("Here, you can compare Legendary Pokemon by their statistics. Select which types of Legendaries you want to compare and your statistic of interest and see which legendaries are more powerful!")),
                                             box(width = 4, inputPanel(
                                               checkboxGroupInput("mytype", label = "Types:",
                                                                  choices = c("Grass", "Fire", "Water", "Normal", "Electric", "Ground", "Fairy", "Fighting", "Psychic", "Rock", "Ghost", "Ice", "Dragon", "Dark", "Steel", "Flying"),
                                                                  selected = c("Grass", "Fire", "Water"),
                                                                  inline = TRUE),
                                               
                                               selectInput("mystat", "Statistic:", 
                                                           choices = c("Total Points" = "Total",
                                                                       "Health Points" = "HP",
                                                                       "Attack Points" = "Attack", 
                                                                       "Defense Points" = "Defense",
                                                                       "Special Attack" = "`Sp. Atk`", 
                                                                       "Special Defence" = "`Sp. Def`", 
                                                                       "Speed" = "Speed"),
                                                           selected = "Total Points")
                                             )),
                                
                                box(plotlyOutput("legendary_stats_plot"), width = 8)
                        ),
                        
                        tabItem(tabName = "legendgroup",
                                box(width = 12,
                                             h1("Grouping Legendary Pokemon by Statistics"),
                                             p("Legendary Pokemon can be placed in groups based on their statistics. Are your favorite Legendaries really better than other ones, or are their stats very similar? We can group them by many statistics. Select which types of Legendaries you would like to group and what statistics on which to group them. The more distance there is plotted for two Pokemon, the more statistically different they are!")),
                                fluidRow(box(width = 4, inputPanel(
                                               checkboxGroupInput("mytypegroup", label = "Types:",
                                                                  choices = c("Grass", "Fire", "Water", "Normal", "Electric", "Ground", "Fairy", "Fighting", "Psychic", "Rock", "Ghost", "Ice", "Dragon", "Dark", "Steel", "Flying"),
                                                                  selected = c("Fire", "Water", "Grass"),
                                                                  inline = TRUE),
                                               
                                               checkboxGroupInput("mystatgroup", h3("Interested Statistic(s):"),
                                                                  choices = c("Total", "HP", "Attack", "Defense",
                                                                              "Special Attack" = "Sp. Atk", 
                                                                              "Special Defense" = "Sp. Def", "Speed"),
                                                                  selected = c("HP", "Attack", "Defense", "Sp. Atk", "Sp. Def", "Speed"))
                                             )
                                ),
                                box(plotOutput("dend"), width = 8))
                        ),
                        
                        tabItem(tabName = "gentypechange",
                                box(width = 12,
                                             h1("Regression of Type Statistics Through Generations"),
                                             p("Here, you can examine how the statistics of certain types have changed through the generations. Select which type and statistic to examine and see a scatterplot with regression line of these statistics. Is your favorite type getting more powerful with each generation?")),
                                             fluidRow(box(width = 4, inputPanel(
                                               
                                               radioButtons("which_typegenc", label = "Type:",
                                                            choices = c("Grass", "Fire", "Water", "Bug", "Normal", "Poison", "Electric", "Ground", "Fairy", "Fighting", "Psychic", "Rock", "Ghost", "Ice", "Dragon", "Dark", "Steel", "Flying"), selected = "Grass", inline = TRUE),
                                               
                                               selectInput("statgenc", label = "Statistic:", choices = stat_choices, selected = 
                                                             "Grass")
                                               
                                               
                                             )),
                                box(plotlyOutput("reg"), width = 8)
                        )),
                        
                        tabItem(tabName = "gentypefav",
                                box(width = 12,
                                             h1("Comparing Types Over Generations"),
                                             p("Some types may be more powerful than others in different generations of Pokemon. If you're playing an older game, there might be differences in the average statistics of each type. Here, we can compare types through generations! Select which types you'd like to compare and see how they stack up in different generations.")),
                                             fluidRow(box(width = 4, inputPanel(
                                               checkboxGroupInput("typegenf", label = "Type:",
                                                                  choices = c("Grass", "Fire", "Water", "Bug", "Normal", 
                                                                              "Poison", "Electric", "Ground", "Fairy", 
                                                                              "Fighting", "Psychic", "Rock", "Ghost", "Ice", 
                                                                              "Dragon", "Dark", "Steel", "Flying"), 
                                                                  selected = c("Grass", "Fire", "Water"),
                                                                  inline = TRUE),
                                               
                                               selectInput("statgenf", label = "Statistic:", choices = stat_choices)
                                             )),
                                
                                box(width = 8, plotlyOutput("lines"))
                        )),
                        
                        tabItem(tabName = "random",
                                fluidRow(box(width = 12,
                                             h1("Compare Your Own Statistics"),
                                             p("Here, you can compare many statistics and draw some interesting conclusions of your own. Select two statistics to examine among types and generations of your choice, and generate a scatterplot of your data! Click the 'Add Regression Line' button to include a smooth regression line in your plot. What interesting things about Pokemon statistics can you find?"),
                                             inputPanel(
                                               checkboxGroupInput("typerand", label = "Type:",
                                                                  choices = c("Grass", "Fire", "Water", "Bug", "Normal", 
                                                                              "Poison", "Electric", "Ground", "Fairy", 
                                                                              "Fighting", "Psychic", "Rock", "Ghost", "Ice", 
                                                                              "Dragon", "Dark", "Steel", "Flying"), 
                                                                  selected = c("Grass", "Fire", "Water"),
                                                                  inline = TRUE),
                                               checkboxGroupInput("generationrand", label = "Generation:",
                                                                  choices = 1:6,
                                                                  selected = 1:6),
                                               
                                               selectInput("stat1rand", label = "X-Statistic:", choices = stat_choices,
                                                           selected = "Attack"),
                                               
                                               selectInput("stat2rand", label = "Y-Statistic:", choices = stat_choices,
                                                           selected = "Defense"),
                                               
                                               checkboxInput("regrand", label = "Add Regression Line")
                                             ))
                                ),
                                fluidRow(plotlyOutput("rand", height = 600))
                        ),
                        
                        tabItem(tabName = "credits",
                                p("App creators:"), 
                                br(), 
                                p("William Shancey"),
                                p("Andre Aquije Flores"), 
                                p("Korrawat Jianthanakanon"), 
                                p("Nicholas Sclafani"), 
                                br(),
                                br(), 
                                p("Dataset: 'Pokemon with stats' from Kaggle"), 
                                p("https://www.kaggle.com/abcsds/pokemon/data"),
                                br(),
                                br(),
                                p("App created using R and shiny dashboard"),
                                br(),
                                p("Packages:"),
                                p("tidyverse"),
                                p("shinydashboard"),
                                p("plotly"),
                                p("shiny"),
                                p("radarchart"),
                                p("dendextend")
                                  )
                        
                        
                        # closes tab item
                      ) # closes tab items
                    ) # closes dash body
) # closes dash page

# ---------- Defining Server ----------

server <- function(input, output) {
  output$pokestattable <- renderTable({
    
    poke.mat <- as.matrix(poke[which(poke$Name == input$pokeselectize), -1])
    colnames(poke.mat) <- c("Name", "Primary Type", "Secondary Type", "Total Points", "Health Points", "Attack Points", "Defense Points", "Special Attack", "Special Defense", "Speed", "Generation", "Legendary")
    rownames(poke.mat) <- ""
    poke.mat <- t(poke.mat)
    
    poke.tab <- as.table(poke.mat)
    poke.tab
    
  }, rownames = FALSE, colnames = FALSE)
  
  output$radarplot <- renderChartJSRadar({
    
    if(input$legend == FALSE) {
      poke <- poke[which(poke$Legendary == "False"),]
    }
    
    scores <- list(
      "Grass" = c(mean(poke$HP[which(poke$Type.1 == "Grass" | poke$Type.2 == "Grass")]),
                  mean(poke$Attack[which(poke$Type.1 == "Grass" | poke$Type.2 == "Grass")]),
                  mean(poke$Sp..Atk[which(poke$Type.1 == "Grass" | poke$Type.2 == "Grass")]),
                  mean(poke$Speed[which(poke$Type.1 == "Grass" | poke$Type.2 == "Grass")]),
                  mean(poke$Defense[which(poke$Type.1 == "Grass" | poke$Type.2 == "Grass")]),
                  mean(poke$Sp..Def[which(poke$Type.1 == "Grass" | poke$Type.2 == "Grass")])),
      
      "Fire" = c(mean(poke$HP[which(poke$Type.1 == "Fire" | poke$Type.2 == "Fire")]),
                 mean(poke$Attack[which(poke$Type.1 == "Fire" | poke$Type.2 == "Fire")]),
                 mean(poke$Sp..Atk[which(poke$Type.1 == "Fire" | poke$Type.2 == "Fire")]),
                 mean(poke$Speed[which(poke$Type.1 == "Fire" | poke$Type.2 == "Fire")]),
                 mean(poke$Defense[which(poke$Type.1 == "Fire" | poke$Type.2 == "Fire")]),
                 mean(poke$Sp..Def[which(poke$Type.1 == "Fire" | poke$Type.2 == "Fire")])),
      
      "Water" = c(mean(poke$HP[which(poke$Type.1 == "Water" | poke$Type.2 == "Water")]),
                  mean(poke$Attack[which(poke$Type.1 == "Water" | poke$Type.2 == "Water")]),
                  mean(poke$Sp..Atk[which(poke$Type.1 == "Water" | poke$Type.2 == "Water")]),
                  mean(poke$Speed[which(poke$Type.1 == "Water" | poke$Type.2 == "Water")]),
                  mean(poke$Defense[which(poke$Type.1 == "Water" | poke$Type.2 == "Water")]),
                  mean(poke$Sp..Def[which(poke$Type.1 == "Water" | poke$Type.2 == "Water")])),
      
      "Bug" = c(mean(poke$HP[which(poke$Type.1 == "Bug" | poke$Type.2 == "Bug")]),
                mean(poke$Attack[which(poke$Type.1 == "Bug" | poke$Type.2 == "Bug")]),
                mean(poke$Sp..Atk[which(poke$Type.1 == "Bug" | poke$Type.2 == "Bug")]),
                mean(poke$Speed[which(poke$Type.1 == "Bug" | poke$Type.2 == "Bug")]),
                mean(poke$Defense[which(poke$Type.1 == "Bug" | poke$Type.2 == "Bug")]),
                mean(poke$Sp..Def[which(poke$Type.1 == "Bug" | poke$Type.2 == "Bug")])),
      
      "Normal" = c(mean(poke$HP[which(poke$Type.1 == "Normal" | poke$Type.2 == "Normal")]),
                   mean(poke$Attack[which(poke$Type.1 == "Normal" | poke$Type.2 == "Normal")]),
                   mean(poke$Sp..Atk[which(poke$Type.1 == "Normal" | poke$Type.2 == "Normal")]),
                   mean(poke$Speed[which(poke$Type.1 == "Normal" | poke$Type.2 == "Normal")]),
                   mean(poke$Defense[which(poke$Type.1 == "Normal" | poke$Type.2 == "Normal")]),
                   mean(poke$Sp..Def[which(poke$Type.1 == "Normal" | poke$Type.2 == "Normal")])),
      
      "Poison" = c(mean(poke$HP[which(poke$Type.1 == "Poison" | poke$Type.2 == "Poison")]),
                   mean(poke$Attack[which(poke$Type.1 == "Poison" | poke$Type.2 == "Poison")]),
                   mean(poke$Sp..Atk[which(poke$Type.1 == "Poison" | poke$Type.2 == "Poison")]),
                   mean(poke$Speed[which(poke$Type.1 == "Poison" | poke$Type.2 == "Poison")]),
                   mean(poke$Defense[which(poke$Type.1 == "Poison" | poke$Type.2 == "Poison")]),
                   mean(poke$Sp..Def[which(poke$Type.1 == "Poison" | poke$Type.2 == "Poison")])),
      
      "Electric" = c(mean(poke$HP[which(poke$Type.1 == "Electric" | poke$Type.2 == "Electric")]),
                     mean(poke$Attack[which(poke$Type.1 == "Electric" | poke$Type.2 == "Electric")]),
                     mean(poke$Sp..Atk[which(poke$Type.1 == "Electric" | poke$Type.2 == "Electric")]),
                     mean(poke$Speed[which(poke$Type.1 == "Electric" | poke$Type.2 == "Electric")]),
                     mean(poke$Defense[which(poke$Type.1 == "Electric" | poke$Type.2 == "Electric")]),
                     mean(poke$Sp..Def[which(poke$Type.1 == "Electric" | poke$Type.2 == "Electric")])),
      
      "Ground" = c(mean(poke$HP[which(poke$Type.1 == "Ground" | poke$Type.2 == "Ground")]),
                   mean(poke$Attack[which(poke$Type.1 == "Ground" | poke$Type.2 == "Ground")]),
                   mean(poke$Sp..Atk[which(poke$Type.1 == "Ground" | poke$Type.2 == "Ground")]),
                   mean(poke$Speed[which(poke$Type.1 == "Ground" | poke$Type.2 == "Ground")]),
                   mean(poke$Defense[which(poke$Type.1 == "Ground" | poke$Type.2 == "Ground")]),
                   mean(poke$Sp..Def[which(poke$Type.1 == "Ground" | poke$Type.2 == "Ground")])),
      
      "Fairy" = c(mean(poke$HP[which(poke$Type.1 == "Fairy" | poke$Type.2 == "Fairy")]),
                  mean(poke$Attack[which(poke$Type.1 == "Fairy" | poke$Type.2 == "Fairy")]),
                  mean(poke$Sp..Atk[which(poke$Type.1 == "Fairy" | poke$Type.2 == "Fairy")]),
                  mean(poke$Speed[which(poke$Type.1 == "Fairy" | poke$Type.2 == "Fairy")]),
                  mean(poke$Defense[which(poke$Type.1 == "Fairy" | poke$Type.2 == "Fairy")]),
                  mean(poke$Sp..Def[which(poke$Type.1 == "Fairy" | poke$Type.2 == "Fairy")])),
      
      "Fighting" = c(mean(poke$HP[which(poke$Type.1 == "Fighting" | poke$Type.2 == "Fighting")]),
                     mean(poke$Attack[which(poke$Type.1 == "Fighting" | poke$Type.2 == "Fighting")]),
                     mean(poke$Sp..Atk[which(poke$Type.1 == "Fighting" | poke$Type.2 == "Fighting")]),
                     mean(poke$Sp..Atk[which(poke$Type.1 == "Fighting" | poke$Type.2 == "Fighting")]),
                     mean(poke$Defense[which(poke$Type.1 == "Fighting" | poke$Type.2 == "Fighting")]),
                     mean(poke$Sp..Def[which(poke$Type.1 == "Fighting" | poke$Type.2 == "Fighting")])),
      
      "Psychic" = c(mean(poke$HP[which(poke$Type.1 == "Psychic" | poke$Type.2 == "Psychic")]),
                    mean(poke$Attack[which(poke$Type.1 == "Psychic" | poke$Type.2 == "Psychic")]),
                    mean(poke$Sp..Atk[which(poke$Type.1 == "Psychic" | poke$Type.2 == "Psychic")]),
                    mean(poke$Speed[which(poke$Type.1 == "Psychic" | poke$Type.2 == "Psychic")]),
                    mean(poke$Defense[which(poke$Type.1 == "Psychic" | poke$Type.2 == "Psychic")]),
                    mean(poke$Sp..Def[which(poke$Type.1 == "Psychic" | poke$Type.2 == "Psychic")])),
      
      "Rock" = c(mean(poke$HP[which(poke$Type.1 == "Rock" | poke$Type.2 == "Rock")]),
                 mean(poke$Attack[which(poke$Type.1 == "Rock" | poke$Type.2 == "Rock")]),
                 mean(poke$Sp..Atk[which(poke$Type.1 == "Rock" | poke$Type.2 == "Rock")]),
                 mean(poke$Speed[which(poke$Type.1 == "Rock" | poke$Type.2 == "Rock")]),
                 mean(poke$Defense[which(poke$Type.1 == "Rock" | poke$Type.2 == "Rock")]),
                 mean(poke$Sp..Def[which(poke$Type.1 == "Rock" | poke$Type.2 == "Rock")])),
      
      "Ghost" = c(mean(poke$HP[which(poke$Type.1 == "Ghost" | poke$Type.2 == "Ghost")]),
                  mean(poke$Attack[which(poke$Type.1 == "Ghost" | poke$Type.2 == "Ghost")]),
                  mean(poke$Sp..Atk[which(poke$Type.1 == "Ghost" | poke$Type.2 == "Ghost")]),
                  mean(poke$Speed[which(poke$Type.1 == "Ghost" | poke$Type.2 == "Ghost")]),
                  mean(poke$Defense[which(poke$Type.1 == "Ghost" | poke$Type.2 == "Ghost")]),
                  mean(poke$Sp..Def[which(poke$Type.1 == "Ghost" | poke$Type.2 == "Ghost")])),
      
      "Ice" = c(mean(poke$HP[which(poke$Type.1 == "Ice" | poke$Type.2 == "Ice")]),
                mean(poke$Attack[which(poke$Type.1 == "Ice" | poke$Type.2 == "Ice")]),
                mean(poke$Sp..Atk[which(poke$Type.1 == "Ice" | poke$Type.2 == "Ice")]),
                mean(poke$Speed[which(poke$Type.1 == "Ice" | poke$Type.2 == "Ice")]),
                mean(poke$Defense[which(poke$Type.1 == "Ice" | poke$Type.2 == "Ice")]),
                mean(poke$Sp..Def[which(poke$Type.1 == "Ice" | poke$Type.2 == "Ice")])),
      
      "Dragon" = c(mean(poke$HP[which(poke$Type.1 == "Dragon" | poke$Type.2 == "Dragon")]),
                   mean(poke$Attack[which(poke$Type.1 == "Dragon" | poke$Type.2 == "Dragon")]),
                   mean(poke$Sp..Atk[which(poke$Type.1 == "Dragon" | poke$Type.2 == "Dragon")]),
                   mean(poke$Speed[which(poke$Type.1 == "Dragon" | poke$Type.2 == "Dragon")]),
                   mean(poke$Defense[which(poke$Type.1 == "Dragon" | poke$Type.2 == "Dragon")]),
                   mean(poke$Sp..Def[which(poke$Type.1 == "Dragon" | poke$Type.2 == "Dragon")])),
      
      "Dark" = c(mean(poke$HP[which(poke$Type.1 == "Dark" | poke$Type.2 == "Dark")]),
                 mean(poke$Attack[which(poke$Type.1 == "Dark" | poke$Type.2 == "Dark")]),
                 mean(poke$Sp..Atk[which(poke$Type.1 == "Dark" | poke$Type.2 == "Dark")]),
                 mean(poke$Speed[which(poke$Type.1 == "Dark" | poke$Type.2 == "Dark")]),
                 mean(poke$Defense[which(poke$Type.1 == "Dark" | poke$Type.2 == "Dark")]),
                 mean(poke$Sp..Def[which(poke$Type.1 == "Dark" | poke$Type.2 == "v")])),
      
      "Steel" = c(mean(poke$HP[which(poke$Type.1 == "Steel" | poke$Type.2 == "Steel")]),
                  mean(poke$Attack[which(poke$Type.1 == "Steel" | poke$Type.2 == "Steel")]),
                  mean(poke$Sp..Atk[which(poke$Type.1 == "Steel" | poke$Type.2 == "Steel")]),
                  mean(poke$Speed[which(poke$Type.1 == "Steel" | poke$Type.2 == "Steel")]),
                  mean(poke$Defense[which(poke$Type.1 == "Steel" | poke$Type.2 == "Steel")]),
                  mean(poke$Sp..Def[which(poke$Type.1 == "Steel" | poke$Type.2 == "Steel")])),
      
      "Flying" = c(mean(poke$HP[which(poke$Type.1 == "Flying" | poke$Type.2 == "Flying")]),
                   mean(poke$Attack[which(poke$Type.1 == "Flying" | poke$Type.2 == "Flying")]),
                   mean(poke$Sp..Atk[which(poke$Type.1 == "Flying" | poke$Type.2 == "Flying")]),
                   mean(poke$Speed[which(poke$Type.1 == "Flying" | poke$Type.2 == "Flying")]),
                   mean(poke$Defense[which(poke$Type.1 == "Flying" | poke$Type.2 == "Flying")]),
                   mean(poke$Sp..Def[which(poke$Type.1 == "Flying" | poke$Type.2 == "Flying")]))
    )
    
    coluhs = apply(colors[2], 1, col2rgb)
    colnames(coluhs) = c("Grass", "Fire", "Water", "Bug", "Normal", "Poison", "Electric", "Ground", "Fairy", "Fighting", "Psychic", "Rock", "Ghost", "Ice", "Dragon", "Dark", "Steel", "Flying")
    
    plot1 <- chartJSRadar(labs = c("Health Points", "Attack Points", "Special Attack", "Speed", "Defense Points", "Special Defense"), scores = scores[input$typeradar], colMatrix = as.matrix(coluhs[,input$typeradar]), maxScale = 120)
    
    
    plot1
    
  })
  
  output$density <- renderPlot({
    
    poke1 = poke[, -4]
    poke2 = poke[, -3]
    colnames(poke2)[3] <- "Type.1"
    poke2 <- poke2[-which(poke2$Type.1 == ""), ]
    
    big.poke <- rbind(poke1, poke2)
    
    
    p <- ggplot(poke[which(big.poke$Type.1 == input$typedensity),], aes_string(x = input$statdensity)) +
      geom_density(aes(fill = Type.1, color = Type.1), alpha = .5) + 
      scale_fill_manual(values = c("Grass" = "darkolivegreen2", "Fire" = "darkorange1", "Water" = "dodgerblue1", "Bug" = "darkolivegreen3", "Normal" = "cornsilk3", "Poison" = "darkmagenta", "Electric" = "gold1", "Ground" = "burlywood2", "Fairy" = "lightpink1", "Fighting" = "brown", "Psychic" = "deeppink", "Rock" = "goldenrod4", "Ghost" = "darkviolet", "Ice" = "darkslategray1", "Dragon" = "darkslateblue", "Dark" = "chocolate4", "Steel" = "azure3", "Flying" = "mediumpurple1")) +
      scale_color_manual(values = c("Grass" = "darkolivegreen2", "Fire" = "darkorange1", "Water" = "dodgerblue1", "Bug" = "darkolivegreen3", "Normal" = "cornsilk3", "Poison" = "darkmagenta", "Electric" = "gold1", "Ground" = "burlywood2", "Fairy" = "lightpink1", "Fighting" = "brown", "Psychic" = "deeppink", "Rock" = "goldenrod4", "Ghost" = "darkviolet", "Ice" = "darkslategray1", "Dragon" = "darkslateblue", "Dark" = "chocolate4", "Steel" = "azure3", "Flying" = "mediumpurple1")) +
      labs(x = names(stat_choices[stat_choices==input$statdensity]), 
           y = "Density",
           title = "Density of Pokemon Statistic per Type",
           color = "Type",
           fill = "Type")
    
    p
    
  })
  
  output$allpoke <- renderPlot({
    
    poke <- poke[which(poke$Legendary == "False"), ]
    
    plot1 <- ggplot(poke[which(poke$Type.1 == input$typeallpoke | poke$Type.2 == input$typeallpoke),]) + 
      geom_bar(aes_string(x = paste0("reorder(Name,",input$statallpoke, ")"), y = input$statallpoke), stat = "identity", position = "dodge", fill = colors[which(colors$type == input$typeallpoke), 2], color = "black") + 
      theme(axis.text.x = element_text(hjust = 1, vjust = 1)) +
      coord_flip() +
      labs(title = paste(names(stat_choices[stat_choices==input$statallpoke]) , "of", input$type, "Pokemon"),
           y = names(stat_choices[stat_choices==input$statallpoke]),
           x = "Pokemon")
    
    
    plot1
    
  })
  
  output$legendary_stats_plot <- renderPlotly({
    
    pokemon <- read_csv("Pokemon.csv")
    legendaries <- subset(pokemon, Legendary == "True")
    pokemon_colors <- data.frame(type = unique(pokemon$`Type 1`), color = c("darkolivegreen2", "darkorange1", "dodgerblue1", "darkolivegreen3", "cornsilk3", "darkmagenta", "gold1", "burlywood2", "lightpink1", "brown", "deeppink", "goldenrod4", "darkviolet", "darkslategray1", "darkslateblue", "chocolate4", "azure3", "mediumpurple1"))
    
    
    to_examine <- subset(legendaries, `Type 1` %in% input$mytype | `Type 2` %in% input$mytype)
    to_examine <- mutate(to_examine, Type = ifelse(`Type 1` %in% input$mytype, `Type 1`, `Type 2`))
    
    myplot <- ggplot(to_examine, aes_string(x = paste("reorder(Name, -", input$mystat, ")"), y = input$mystat, fill = "Type")) +
      geom_bar(stat = "identity") + #, fill = "steelblue3") +
      labs(title = "Legendary Pokemon Statistics",
           x = "Name",
           y = names(stat_choices[stat_choices==input$mystat])) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_fill_manual(values = c("Grass" = "darkolivegreen2", 
                                   "Fire" = "darkorange1", 
                                   "Water" = "dodgerblue1", 
                                   "Bug" = "darkolivegreen3", 
                                   "Normal" = "cornsilk3", 
                                   "Poison" = "darkmagenta", 
                                   "Electric" = "gold1", 
                                   "Ground" = "burlywood2", 
                                   "Fairy" = "lightpink1", 
                                   "Fighting" = "brown", 
                                   "Psychic" = "deeppink", 
                                   "Rock" = "goldenrod4", 
                                   "Ghost" = "darkviolet", 
                                   "Ice" = "darkslategray1", 
                                   "Dragon" = "darkslateblue", 
                                   "Dark" = "chocolate4", 
                                   "Steel" = "azure3", 
                                   "Flying" = "mediumpurple1"))
    
    ggplotly(myplot)
  })
  
  output$dend <- renderPlot({
    pokemon <- read_csv("Pokemon.csv")
    legendaries <- subset(pokemon, Legendary == "True")
    pokemon_colors <- data.frame(type = unique(pokemon$`Type 1`), color = c("darkolivegreen2", "darkorange1", "dodgerblue1", "darkolivegreen3", "cornsilk3", "darkmagenta", "gold1", "burlywood2", "lightpink1", "brown", "deeppink", "goldenrod4", "darkviolet", "darkslategray1", "darkslateblue", "chocolate4", "azure3", "mediumpurple1"))
    
    poke_colors <- c("Grass" = "darkolivegreen2", 
                     "Fire" = "darkorange1", 
                     "Water" = "dodgerblue1", 
                     "Bug" = "darkolivegreen3", 
                     "Normal" = "cornsilk3", 
                     "Poison" = "darkmagenta", 
                     "Electric" = "gold1", 
                     "Ground" = "burlywood2", 
                     "Fairy" = "lightpink1", 
                     "Fighting" = "brown", 
                     "Psychic" = "deeppink", 
                     "Rock" = "goldenrod4", 
                     "Ghost" = "darkviolet", 
                     "Ice" = "darkslategray1", 
                     "Dragon" = "darkslateblue", 
                     "Dark" = "chocolate4", 
                     "Steel" = "azure3", 
                     "Flying" = "mediumpurple1")
    
    to_examine <- subset(legendaries, `Type 1` %in% input$mytypegroup | `Type 2` %in% input$mytypegroup)
    to_examine <- mutate(to_examine, Type = ifelse(`Type 1` %in% input$mytype, `Type 1`, `Type 2`))
    to_examine <- mutate(to_examine, color = poke_colors[Type])
    
    mysubset <- to_examine[c("Name", "Type", input$mystatgroup)] %>%
      remove_rownames() %>%
      column_to_rownames(var = "Name")
    
    mydist <- dist(mysubset) %>% round(1)
    mydend <- hclust(mydist, method = "single") %>% as.dendrogram
    
    mydend <- mydend %>% 
      set("labels_cex", .75) %>%
      set("labels_col", poke_colors[mysubset$Type], order_value = TRUE) %>%
      set("branches_col", "#fdf6e3")
    myplot <- ggplot(mydend, theme = NULL, horiz = TRUE) +
      labs(title = "Comparing Legendary Pokemon Similarities",
           subtitle = "Colored By Type",
           x = "Legendary Pokemon Names",
           y = "Distance") +
      theme(axis.text.y = element_blank(), 
            panel.background = element_rect(fill = "#586e75",
                                            colour = "#586e75",
                                            size = 0.5, linetype = "solid"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    myplot
  })
  
  output$reg <- renderPlotly({
    
    p <- ggplot(poke[which(poke$Type.1 == input$which_typegenc | poke$Type.2 == input$which_typegenc),],
                aes_string(x = "Generation", y = input$statgenc)) +
      geom_point(color = colors[which(colors$type == input$which_typegenc), 2]) + 
      geom_smooth(span = 2, color = "gray", se = FALSE)+
      labs(x = "Generation", 
           y = names(stat_choices[stat_choices==input$statgenc]),
           title = paste(names(stat_choices[stat_choices==input$statgenc]), "of", input$which_typegenc,"Pokemon across Generations")) +
      scale_x_continuous(breaks = 1:6)
    
    ggplotly(p)
    
  })
  
  output$lines <- renderPlotly({
    
    poke1 = poke[, -4]
    poke2 = poke[, -3]
    colnames(poke2)[3] <- "Type.1"
    poke2 <- poke2[-which(poke2$Type.1 == ""), ]
    
    big.poke <- rbind(poke1, poke2)
    
    poke_type_gen <- big.poke %>% group_by(Type.1, Generation) %>%
      summarise(Total = mean(Total),
                HP = mean(HP),
                Attack = mean(Attack),
                Defense = mean(Defense),
                Sp..Atk = mean(Sp..Atk),
                Sp..Def = mean(Sp..Def),
                Speed = mean(Speed))
    
    type_df <- poke_type_gen[which(poke_type_gen$Type.1 %in% input$typegenf),]
    
    p2 <- ggplot(type_df, aes_string(x = "Generation", y = input$statgenf, color = "Type.1")) + 
      #geom_bar(stat = "identity", pos = "dodge") + 
      geom_point() + geom_line() +
      scale_color_manual(values = c("Grass" = "darkolivegreen2", "Fire" = "darkorange1", "Water" = "dodgerblue1", "Bug" = "darkolivegreen3", "Normal" = "cornsilk3", "Poison" = "darkmagenta", "Electric" = "gold1", "Ground" = "burlywood2", "Fairy" = "lightpink1", "Fighting" = "brown", "Psychic" = "deeppink", "Rock" = "goldenrod4", "Ghost" = "darkviolet", "Ice" = "darkslategray1", "Dragon" = "darkslateblue", "Dark" = "chocolate4", "Steel" = "azure3", "Flying" = "mediumpurple1")) +
      labs(title = paste("Generational Changes of Average", names(stat_choices[stat_choices==input$statgenf]) ,"for Pokemon Types"),
           color = "Type",
           y = paste("Average", names(stat_choices[stat_choices==input$statgenf]))) +
      scale_x_continuous(breaks = 1:6)
    ggplotly(p2)
    
  })
  
  output$rand <- renderPlotly({
    
    types_df <- poke[which(poke$Type.1 %in% input$typerand),]
    types_df <- types_df[which(types_df$Generation %in% input$generationrand),]
    
    print(input$statrand)
    p2 <- ggplot(types_df, aes_string(x = input$stat1rand, y = input$stat2rand)) + 
      geom_point(aes_string(color = "Type.1")) + 
      scale_color_manual(values = c("Grass" = "darkolivegreen2", "Fire" = "darkorange1", "Water" = "dodgerblue1", "Bug" = "darkolivegreen3", "Normal" = "cornsilk3", "Poison" = "darkmagenta", "Electric" = "gold1", "Ground" = "burlywood2", "Fairy" = "lightpink1", "Fighting" = "brown", "Psychic" = "deeppink", "Rock" = "goldenrod4", "Ghost" = "darkviolet", "Ice" = "darkslategray1", "Dragon" = "darkslateblue", "Dark" = "chocolate4", "Steel" = "azure3", "Flying" = "mediumpurple1")) + 
      labs(title = "Pokemon Trait Comparisons",
           color = "Type",
           x = names(stat_choices[stat_choices==input$stat1rand]),
           y = names(stat_choices[stat_choices==input$stat2rand]))
    
    if(input$regrand == TRUE) { p2 = p2 + geom_smooth(color = "black", span = 2, se = FALSE) }
    
    
    ggplotly(p2)
    
  })
  
 output$pokepic <- renderImage({
   list(
     src = pic.id[which(poke$Name == input$pokeselectize)],
     width = 300,
     height = 300,
     alt = "Image Not Available :("
   )
 }, deleteFile = FALSE)
 
 output$background <- renderImage({
   list(
     src = "pokemonbackground.jpg",
     width = 600,
     height = 300,
     alt = "Image Not Available :("
       )
   }, deleteFile = FALSE)
 
 output$overview <- renderChartJSRadar({
   
   pokelist <- list()
   
   for(ii in 1:nrow(poke)) {
     pokelist[[ii]] <- c(poke$HP[ii], poke$Attack[ii], poke$Defense[ii], poke$Sp..Atk[ii], poke$Sp..Def[ii], poke$Speed[ii])
   }
   
   names(pokelist) <- poke$Name
   
   coluhs = apply(colors[2], 1, col2rgb)
   colnames(coluhs) = c("Grass", "Fire", "Water", "Bug", "Normal", "Poison", "Electric", "Ground", "Fairy", "Fighting", "Psychic", "Rock", "Ghost", "Ice", "Dragon", "Dark", "Steel", "Flying")
   
   plot1 <- chartJSRadar(labs = c("Health Points", "Attack Points", "Special Attack", "Speed", "Defense Points", "Special Defense"), scores = pokelist[input$pokes])
   
   plot1
   
 })
 
 output$superimage <- renderImage({
   list(
     src = "supereffect.png",
     width = 800,
     height = 400,
     alt = "Image Not Available :("
   )
 }, deleteFile = FALSE)
 
 output$superheat <- renderD3heatmap({
   poke_type <- poke %>% group_by(Type.1) %>%
     summarise(Total = mean(Total),
               Attack = mean(Attack),
               Defense = mean(Defense))
   
   rownames(poke_type) <- poke_type$Type.1
   eff_mat <- matrix(rep(0, 18*18), nrow = 18)
   rownames(eff_mat) <- poke_type$Type.1
   colnames(eff_mat) <- poke_type$Type.1
   for (t in poke_type$Type.1) {
     for (s in poke_type$Type.1) {
       eff_mat[t,s] <- as.numeric(poke_type[t,"Attack"] / poke_type[s, "Defense"])
     }
   }
   d3heatmap(eff_mat, Rowv = FALSE, Colv = FALSE, 
             colors = colorRamp(colors = c("Blue", "White", "Red")),
             xaxis_font_size = 12,
             yaxis_font_size = 12)
 })
 
 
  
}


# ---------- Calling App ----------

shinyApp(ui, server)


