library("dplyr")
library("ggplot2")
library("reshape2")
library("plotly")
library("shiny")
library("ggmap")
ui <- fluidPage(
  titlePanel("Motor Collision Data in Seattle"),
  sidebarLayout(
    sidebarPanel(
      style = ("position:fixed;width:inherit;"),
      p("Select specific district for the map"),
      selectInput("dist", "District:", choices = c("B", "C", "D", "E", "F", "G", "J", "K", "L",
                                                   "K", "L", "M", "N", "O", "Q", "R", "S", "U", "W")),
      p("Select a specific hour for the first graph"),
      sliderInput("hour", "Hour:", min = 1, max = 24, value = 1, step = 1),
      p("Select a specific district for the second graph"),
      selectInput("district", "District:", choices = c("B", "C", "D", "E", "F", "G", "J", "K", "L", "M", "N", "O", "Q", "R", "S", "U", "W"))
    ),
    mainPanel(
      h5("by: Naved Krishnamurthy, Michael Sui, Andrew Sang"),
      h1("Introduction"),
      tags$img(src='precinctmap.png'),
      p("In this project, we explore the statistics behind accidents in the City of Seattle since June 17th, 2014. 
        Our objective was to determine if we could find any potential trends in accident and DUI data. Analysis is 
        performed on data from Seattle Police Departments 911 Incident Response data set, and is focused primarily 
        on accidents and Driving Under the Influence (DUI) incidences. We asked several questions about these public 
        safety concerns, and found the relationships between them and factors such as time of day and location. 
        We’ve also taken a deeper look into the specific type of incidence, and its severity."),
      h2("Abstract Information"),
      p("Accidents appear to be most common at 7 PM and seem to occur most frequently in the R2 beat. 
        However, DUIs tend to occur at 10 PM in the N3 beat, whereas DUIs that result in accidents most often occur 
        at 3 AM at the W1 beat."),
      h2("Map Visualization"),
      plotOutput("map2"),
      br(),
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("map")),
                  tabPanel("Table", tableOutput("table"))
      ),
      p("With this visualization, we wanted to see where accidents(motor collisions) occured in Seattle. Most 
        importantly we wanted to see where in Seattle were the most prone to being in a car accident. 
        This knowledge would allow us to see which parts of Seattle are per say, the most dangerous. This 
        visualization utilizes a drop down menu to select which zone of Seattle that the user wishes to view. 
        Zones are defined by the Seattle Police Department with a total amount of 19 zones that are equally divided 
        up by the different precincts of SPD. Additionally there is also a table which shows an example of the data set that we used and what information we utilized to make the visualization. In addition to that visualization there is also another one that just shows the full overview of where the accidents occur.
        From our visualization we could see that most accidents were concentrated in downtown Seattle while highways 
        and freeways seem to frquently also have alot of accidents. This is pretty understandable as downtown Seattle 
        is easily the most populated area while highways and freeways were always places that have more traffic flow 
        anyways. This shows that as long as there is a heavier traffic flow, there will be more accidents."),
      h2("DUI occurences for each Zone in Seattle"),
      plotOutput("bar"),
      p("This plot analyzes the specific zone in Seattle. From this graph, we can see that the most popular instances of DUI happen during the hours of 1am to 3am.
        There is no particular correlation between how many DUI's occur depending on the district, but we do learn that 
        drunk driving happens the most during the hours past midnight. This makes sense since a lot of people do drink at night
        and sometimes immediately drive home without waiting for themselves to sober up. This allows us to answer the question that
        we need to be really careful when driving from 1am to 3am in Seattle as drunk driving collisions are very likely to occur
        at this time. This confirms our question that drunk driving depends on the time of the day."),
      h2("Amount of Hit and Runs Depending on Hour of the Day"),
      plotOutput("line"),
      p("We wanted to see the amount of hit and runs everywhere in Seattle to see if hit and runs occur at a specific time. This would help us analyze
        if maybe its more dangerous to be outside at a certain time compared to others. This graph analyzes when hit and runs occur, as you
        can choose which district in Seattle you want to see these hit and run occurences. We realized that from the data,
        the most hit and runs occur at around 7pm regardless of the district. At 7pm, the amount of occurences of a hit and run for each district
        was always the highest. This is interesting data that tells us to cautious at certain times when driving in Seattle.
        This confirms to us that hit and runs do depends on time, but not so much the district of Seattle."),
      h2("Conclusions"),
      p("It appears like the greatest number of accidents occur in the R2 beat, the site of Rainier Ave which is well 
        known to be Seattle’s most dangerous street. It was also interesting to us that the greatest number of accidents 
        occured during 7PM, a time you wouldn’t think there’d be a lot of traffic at. We conjecture this might be due to 
        changing road conditions (day -> night) or perhaps folks getting the sun in their eyes. What was expected though 
        was that the number of accidents and incidences of DUIs correlated with population density and traffic. 
        It made sense for there to b e more accidents where there’s more driving. One issue with using police data is that 
        we cannot know what is the real number of incidences of the stats. This might be due to under-reporting, 
        elective non-reporting, or simply the police not hearing about it. Therefore, it is a fair assessment to claim 
        that the number of incidences of drunk driving is higher than this analysis indicates.")
      
    )
  )
)
data <- read.csv("data/Police_Incidents_After_2014.csv")
filtered <- filter(data, Event.Clearance.Code == "430")
test <- filtered
map.Seattle <- qmap("seattle", zoom = 11, source = "stamen", maptype ="toner", darken = c(.3, "#BBBBBB"))
data <- mutate(data, Event.Clearance.AMPM = substr(Event.Clearance.Date, 21, 22))
data2 <- mutate(data, Event.Clearance.Hour = as.integer(substr(Event.Clearance.Date, 12, 13)))
data2 <- mutate(data2, Event.Clearance.Final = if_else(Event.Clearance.AMPM == "PM", Event.Clearance.Hour + 12, as.numeric(Event.Clearance.Hour)))

server <- function(input, output) {
  selectValues2 <- reactive({
    sector <- filter(filtered, District.Sector == input$dist)
    sector
  })
  output$map2 <- renderPlot({
    map.Seattle +
      geom_point(data = filtered, aes(x=Longitude, y=Latitude, color = District.Sector),alpha = 0.03, size= 1.1) +
      theme(legend.title = element_blank()) + 
      theme(legend.position = 'none') +
      ggtitle("Seattle Car Accidents", subtitle = "2015-2018")
  })
  output$map <- renderPlot({
    sector_select <- selectValues2()
      map.Seattle +
      geom_point(data = sector_select, aes(x=Longitude, y=Latitude), color = "red", alpha = 0.03, size= 1.1)+
      theme(legend.title = element_blank()) + 
      theme(legend.position = 'none') + 
      ggtitle("Seattle Car Accidents(By Zone)", subtitle = "2015-2018")
  })
  output$table <- renderTable({
    sector_select <- selectValues2()
    sector_select <- head(sector_select, 6)
    sector_select %>%
      select(Event.Clearance.Date, Hundred.Block.Location, Incident.Location, Initial.Type.Description)
  })
  sliderValues <- reactive({
    if(input$hour > 12) {
      data3 <- filter(data2, Event.Clearance.AMPM == "PM", Event.Clearance.Hour ==  input$hour - 12)
    }
    else {
      data3 <- filter(data2, Event.Clearance.AMPM == "AM", Event.Clearance.Hour ==  input$hour)
    }
    data3
  })
  selectValues <- reactive({
    data5 <- filter(data2, District.Sector == input$district, Initial.Type.Description == "MOTOR VEHICLE COLLISION, HIT AND RUN")
    data5
  })
  output$bar<-renderPlot({
    data4 <- filter(sliderValues(), Event.Clearance.Code == "450", Initial.Type.Subgroup == "MOTOR VEHICLE COLLISION INVESTIGATION")
    p <- ggplot(data4, aes(x = Zone.Beat, fill = Initial.Type.Description)) +
      geom_bar(stat = 'count') +
      scale_y_continuous(breaks=c(1,3,7,10))
    p + labs(x = "Zone ID", y = "Freqeuncy") + 
      scale_fill_discrete(name = "Type of Investigation",
                          breaks = c("MOTOR VEHICLE COLLISION WITH INJURIES - PRIORITY 1", 
                                     "MOTOR VEHICLE COLLISION, BLOCKING",
                                     "MOTOR VEHICLE COLLISION, HIT AND RUN",
                                     "MOTOR VEHICLE COLLISION, REPORT ONLY - NON INJURY/NON BLOCKING",
                                     "MOTOR VEHICLE COLLISION, UNKNOWN INJURIES"),
                          labels = c("Priority 1", "Blocking", "Hit and Run", "Non-Injury", "Unknown Injuries"))
  })
  output$line <- renderPlot({
    data6 <- selectValues()
    ggplot(data6, aes(x = Event.Clearance.Final)) +
      geom_line(stat = 'count') +
      scale_x_continuous(breaks  = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)) +
      labs(x = "Hour of the Day", y = "Occurences")
  })
}
shinyApp(ui, server)