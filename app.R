#IMPORTING REQUIRED LIBRARIES

library(shiny)
library(ggplot2)
library(dplyr)
library(DBI)
library(RODBC)
library(RMySQL)
library(shinydashboard)
library(shinyjs)
library(leaflet)
library(ggmap)

con <- dbConnect(MySQL(), user = "your_username", password = "your_password", dbname = "your_dbname", host = "your_hostname")

# Load data from SQL tables
asia <- dbGetQuery(con,"SELECT * FROM asia")
europe <- dbGetQuery(con,"SELECT * FROM europe")
northamerica <- dbGetQuery(con,"SELECT * FROM northamerica")
southamerica <- dbGetQuery(con,"SELECT * FROM southamerica")
oceania <- dbGetQuery(con,"SELECT * FROM oceania")
africa <- dbGetQuery(con,"SELECT * FROM africa")

# Combine data from all tables
data <- rbind(asia, europe, northamerica, southamerica, oceania, africa)

# Geocode location data
geocode_result <- geocode(data$`Country/Territory`)

# Add latitude and longitude columns to data
data$lat <- geocode_result$lat
data$lng <- geocode_result$lng

#DEFINING THE USER INTERFACE

ui <- dashboardPage(
  dashboardHeader(title = "Population Analyzer"),
  dashboardSidebar(
  collapsed = TRUE, 
  div(htmlOutput("welcome"), style = "padding: 20px"),
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("View Tables", tabName = "view_table", icon = icon("search")),
    menuItem("Update Tables", tabName = "update_table", icon = icon("exchange-alt")),
    menuItem("Insert Entries", tabName = "insert_value", icon = icon("edit")),
    menuItem("Delete Tables", tabName = "del_table", icon = icon("trash-alt")),
    menuItem("Dashboard", tabName = "db", icon = icon("chart-line")),
    menuItem("Map", tabName = "map", icon = icon("map"))
  )
),
  dashboardBody(
  tags$img(src = "https://static.spacecrafted.com/b849f747e16744a5be3537c67cbb5770/i/fbe1464dda09476c92916c8067791ea4/1/GCuCv726gZycFxatRCb7iU/population.png",height = 700,width = 1400,
             style='position: absolute; opacity: 0.5;'),
  tabItems(
    tabItem (tabName = "db", uiOutput("tab6UI")),
    tabItem(tabName = "view_table", uiOutput("tab2UI")),
    tabItem(tabName = "del_table", uiOutput("tab3UI")),
    tabItem(tabName = "update_table", uiOutput("tab4UI")),
    tabItem(tabName = "insert_value", uiOutput("tab5UI")),
    tabItem(tabName = "home", uiOutput("tab1UI")),
    tabItem(tabName = "map", uiOutput("tab7UI"))
  )
)
)


#SERVER FUNCTION

server <- function(input, output) 
{

#ESTABLISHING CONNECTION WITH MYSQL

con <- dbConnect(MySQL(), user = "your_username", password = "your_password", dbname = "your_dbname", host = "your_hostname")

output$tab7UI <- renderUI ({
fluidPage(
  leafletOutput("map")
)
})

output$map <- renderLeaflet({
    leaflet(data) %>%
      addTiles() %>%
      addMarkers(lat = ~lat, lng = ~lng, popup = ~`Country/Territory`)
})



#DASHBOARD PAGE

output$tab6UI <- renderUI({
fluidPage(
fluidRow(
titlePanel("World Population Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("continent", "Continent", choices = c("Asia", "NorthAmerica", "SouthAmerica", "Oceania", "Europe", "Africa")),
      selectInput("charttype", "Chart Type", choices = c("Bar Chart", "Scatter Plot", "Line Chart"))
    ),
    mainPanel(
      plotOutput("chart")
    )
)
),
fluidRow(
column(width=6,
 titlePanel("Population by Age Groups"),
  sidebarLayout(
   sidebarPanel(
      selectInput("continent2", "Continent", choices = c("Asia", "NorthAmerica", "SouthAmerica", "Oceania", "Europe", "Africa")),
),
    mainPanel(
      tabsetPanel(
        tabPanel("Total Population", plotOutput("total_population")),
        tabPanel("Male Population", plotOutput("male_population")),
        tabPanel("Female Population", plotOutput("female_population")),
        tabPanel("Children Population", plotOutput("children_population"))
      )
    )
)
),
column(width=6,
titlePanel("Sex Ratio across Continents"),
  sidebarLayout(
    sidebarPanel(
      selectInput("continent3", 
                  label = "Select a Continent", 
                  choices = c("Asia", "NorthAmerica", "SouthAmerica","Oceania", "Europe", "Africa"))
    ),
    mainPanel(
      plotOutput("pie"),
      textOutput("d")
    )
  )


)
)
  )#fluidpage
}) #renderUI

continent.data <- reactive({
    if (input$continent == "All") {
      data <- dbGetQuery(con, "SELECT * FROM asia UNION ALL SELECT * FROM northamerica UNION ALL SELECT * FROM southamerica UNION ALL SELECT * FROM oceania UNION ALL SELECT * FROM europe UNION ALL SELECT * FROM africa")
    } else {
      data <- dbGetQuery(con, paste("SELECT * FROM", input$continent))
    }
    return(data)
  })
  
output$chart <- renderPlot({
    if (input$charttype == "Bar Chart") {
      ggplot(continent.data(), aes(x = reorder(`Country/Territory`, -TotalPopulation), y = TotalPopulation)) +
        geom_bar(stat = "identity", fill = "blue") +
        xlab("Country/Territory") +
        ylab("Total Population") +
        ggtitle("Total Population by Country/Territory")
    } else if (input$charttype == "Scatter Plot") {
      ggplot(continent.data(), aes(x = Density, y = TotalPopulation)) +
        geom_point(color = "blue") +
        xlab("Density") +
        ylab("Total Population") +
        ggtitle("Total Population vs Density")
    } else if (input$charttype == "Line Chart") {
      ggplot(continent.data(), aes(x = Rank, y = TotalPopulation)) +
        geom_line(color = "blue") +
        xlab("Rank") +
        ylab("Total Population") +
        ggtitle("Total Population by Rank")
    }
  })

  # Total population plot
  output$total_population <- renderPlot({
    # Query the MySQL table for the selected continent
    query <- paste("SELECT `Country/Territory`, TotalPopulation FROM ", input$continent2)
    data <- dbGetQuery(con, query)
    
    # Plot the total population by country
    ggplot(data, aes(x = reorder(`Country/Territory`, TotalPopulation), y = TotalPopulation)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      ggtitle(paste("Total Population by Country in", input$continent2)) +
      xlab("Country/Territory") +
      ylab("Total Population") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Male population plot
  output$male_population <- renderPlot({
    # Query the MySQL table for the selected continent
    query <- paste("SELECT `CCA3`, MalePopulation FROM ", input$continent2)
    data <- dbGetQuery(con, query)
    
    # Plot the male population by country
    ggplot(data, aes(x = reorder(`CCA3`, MalePopulation), y = MalePopulation)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      ggtitle(paste("Male Population by Country in", input$continent2)) +
      xlab("Country") +
      ylab("Male Population") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Female population plot
  output$female_population <- renderPlot({
    # Query the MySQL table for the selected continent
    query <- paste("SELECT `CCA3`, FemalePopulation FROM ", input$continent2)
    data <- dbGetQuery(con, query)
    
    # Plot the female population by country
    ggplot(data, aes(x = reorder(`CCA3`, FemalePopulation), y = FemalePopulation)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      ggtitle(paste("Female Population by Country in", input$continent2)) +
      xlab("Country") +
      ylab("Female Population") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })


  # Child population plot
  output$children_population <- renderPlot({
    # Query the MySQL table for the selected continent
    query <- paste("SELECT `CCA3`, ChildrenPopulation FROM ", input$continent2)
    data <- dbGetQuery(con, query)
    
    # Plot the female population by country
    ggplot(data, aes(x = reorder(`CCA3`, ChildrenPopulation), y = ChildrenPopulation)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      ggtitle(paste("Children Population by Country in", input$continent2)) +
      xlab("Country") +
      ylab("Children Population") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })



output$d <- renderText ({
continentname<- input$continent3
  
  query <- sprintf("SELECT MalePopulation, FemalePopulation FROM %s", continentname)

 data <- dbGetQuery(con, query)
  
  data$total <- data$MalePopulation + data$FemalePopulation
  
  data$MalePercentage <- data$MalePopulation / data$total * 100
  data$FemalePercentage <- data$FemalePopulation / data$total * 100
  
  output$pie <- renderPlot({
    ggplot(data, aes(x = "", y = FemalePercentage, fill = "Female")) + 
      geom_bar(stat = "identity", width = 1) +
      geom_bar(aes(y = MalePercentage, fill = "Male"), 
               stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c("blue", "pink")) +
      labs(fill = "Gender", x = NULL, y = NULL,
           title = paste("Population by Gender in", continentname))
  })

})

#VIEW PAGE

output$tab2UI <- renderUI({
fluidPage(
box(width = NULL, 
    sidebarLayout(
      sidebarPanel(
        box(width = 12,
            collapsible = TRUE,
            div(style = "height: 5px; background-color: white;"),
            title = "DataBase Info:", "The dataset contains world population across different continents",
        p("")),
        selectInput("database", "Tables in Database", choices = dbListTables(con),selected = "asia"),
        textOutput("result"),
        tags$head(tags$style("#tab_intro{font-size: 15px;font-style: italic;}"))
        ),
      mainPanel(
        h4(strong("Table Preview")),
        dataTableOutput("bb")
      )
    )
  )
)
})

output$result <- renderText ({
selected_option <- input$database
drv = dbDriver("MySQL") 
con = dbConnect(drv,host="your_hostname",dbname="your_dbname",user="your_username",pass="your_password") 
stmt =  sprintf("select * from %s",selected_option)
output$bb <- renderDataTable(dbGetQuery(con, statement = stmt  ))
})

#DELETE PAGE

output$tab3UI <- renderUI({
fluidPage(
useShinyjs(),
fluidRow(
box(title="Delete",width = NULL, background = "black",
        selectInput("cont", "Continents in Database", choices = dbListTables(con),selected = "asia"),
		tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#oldcount').on('input', function() {
     					     var regex = /[^a-zA-Z]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
     	textInput(inputId="cou", label="Country", value='', width = NULL, placeholder = NULL),
			textOutput("delresult"),
            	actionButton("deletecountry","Remove Country"),
	div(id = "error_msg",style = "display:none;")
   )
)
)
})

output$delresult <- renderText ({
selected_continent <- input$cont
selected_country <- input$cou

observeEvent(input$deletecountry,
{ 
	if(nchar(input$cou) == 0)
	{
		shinyjs::show(id = "error_msg",anim = TRUE)
		shinyjs::html("error_msg","Country to be deleted can't be empty")
		return()
	}
 drv = dbDriver("MySQL") 
 conn = dbConnect(drv,host="your_hostname",dbname="your_dbname",user="your_username",pass="your_password")
 stmnt = sprintf("delete from %s where `Country/Territory` = '%s'",selected_continent,selected_country)
 dbSendQuery(conn,statement = stmnt)
 shiny::showNotification("Successfully deleted country")

}
)
})

#UPDATE PAGE

output$tab4UI <- renderUI({
  fluidPage(      
  useShinyjs(),
    fluidRow(
           column(width = 4,
     				 box(title = "Rename Country", width = NULL,
      			 selectInput("contname", "Continents in Database", choices = dbListTables(con),selected = "asia"),
     				 wellPanel(
						tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#oldcount').on('input', function() {
     					     var regex = /[^a-zA-Z]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
       				      textInput("oldcount","Enter the current name of the country"),
						tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#newcount').on('input', function() {
     					     var regex = /[^a-zA-Z]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
      				      textInput("newcount","Enter the new name of the country"),
    				            actionButton("renamecount","Rename Country"),
	                              div(id = "error_msg",style = "display:none;")

					     )
          			      )
      		),
                 column(width = 4,
    	    box(
            title = "Rename Column",width=NULL,
            selectInput("continentname", "Continents in Database", choices = dbListTables(con),selected = "asia"),
            textInput(inputId="countryname", label="Country", value='', width = NULL, placeholder = NULL),
			tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#rankcol').on('input', function() {
     					     var regex = /[^0-9]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
            textInput(inputId="rankcol", label="Rank", value='', width = NULL, placeholder = NULL),
		tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#areacol').on('input', function() {
     					     var regex = /[^0-9.]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),

		textInput(inputId="areacol", label="Area", value='', width = NULL, placeholder = NULL),
		tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#densitycol').on('input', function() {
     					     var regex = /[^0-9.]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),

		textInput(inputId="densitycol", label="Density", value='', width = NULL, placeholder = NULL),
		tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#growthratecol').on('input', function() {
     					     var regex = /[^0-9.]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),

            textInput(inputId="growthratecol", label="Growth Rate", value='', width = NULL, placeholder = NULL)
		)
	    ),
		column(width=4,
		box(
            	title = "Population Counts",width=NULL,
			tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#worldpopulationpercentagecol').on('input', function() {
     					     var regex = /[^0-9.]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),

                  textInput(inputId="worldpopulationpercentagecol", label="World Population Percentage", value='', width = NULL, placeholder = NULL),
			tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#malepopcol').on('input', function() {
     					     var regex = /[^0-9.]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
			textInput(inputId="malepopcol", label="Male Population", value='', width = NULL, placeholder = NULL),
			tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#femalepopcol').on('input', function() {
     					     var regex = /[^0-9.]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),

  			textInput(inputId="femalepopcol", label="Female Population", value='', width = NULL, placeholder = NULL),
			tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#childrenpopcol').on('input', function() {
     					     var regex = /[^0-9.]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
			textInput(inputId="childrenpopcol", label="Children Population", value='', width = NULL, placeholder = NULL),
			tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#workpopcol').on('input', function() {
     					     var regex = /[^0-9.]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
			textInput(inputId="workpopcol", label = "Working Poulation", value='', width = NULL, placeholder = NULL),
			tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#totalpopcol').on('input', function() {
     					     var regex = /[^0-9.]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
			textInput(inputId="totalpopcol", label="Total Population", value='', width = NULL, placeholder = NULL),
			actionButton("updatecols","Update Column Values")
		    )#box
		   )#col
         )#row
)#page
})


observeEvent(input$renamecount,
{ 
 oldcountname <- input$oldcount
 newcountname <- input$newcount
 conti <- input$contname
 
 if(nchar(input$oldcount) == 0)
	{
		shinyjs::show(id = "error_msg",anim = TRUE)
		shinyjs::html("error_msg","Old Country name can't be empty")
		return()
	}

 if(nchar(input$newcount) == 0)
	{
		shinyjs::show(id = "error_msg",anim = TRUE)
		shinyjs::html("error_msg","New Country name can't be empty")
		return()
	}
 
 
 drv = dbDriver("MySQL") 
 connec = dbConnect(drv,host="your_hostname",dbname="your_dbname",user="your_username",pass="your_password")
 stamnts = sprintf("UPDATE %s SET `Country/Territory` = '%s' WHERE `Country/Territory` = '%s'",conti,newcountname,oldcountname)
 dbSendQuery(connec,statement = stamnts)
 shiny::showNotification("Successfully updated Country name")

}
)

observeEvent(input$updatecols,
{ 
 renamecont <- input$continentname
 renamecount <- input$countryname
 renamerank <- as.numeric(input$rankcol)
 renamearea <- as.numeric(input$areacol)
 renamedensity <- as.numeric(input$densitycol)
 renamegrowth <- as.numeric(input$growthratecol)
 renameworldpop <- as.numeric(input$worldpopulationpercentagecol)
 renamemalepop <- as.numeric(input$malepopcol)
 renamefemalepop <- as.numeric(input$femalepopcol)
 renamework<- as.numeric(input$workpopcol)
 renamechpop <- as.numeric(input$childrenpopcol)
 renametotalpop <- as.numeric(input$totalpopcol)

 if(nchar(input$continentname) == 0)
	{
		shinyjs::show(id = "error_msg",anim = TRUE)
		shinyjs::html("error_msg","Continent name can't be empty")
		return()
	}
  if(nchar(input$countryname) == 0)
	{
		shinyjs::show(id = "error_msg",anim = TRUE)
		shinyjs::html("error_msg","Country name can't be empty")
		return()
	}
 if(input$rankcol == 0)
	{
		shinyjs::show(id = "error_msg",anim = TRUE)
		shinyjs::html("error_msg","Rank can't be empty")
		return()
	}
if(input$areacol == 0)
	{
		shinyjs::show(id = "error_msg",anim = TRUE)
		shinyjs::html("error_msg","Area can't be empty")
		return()
	}
if(input$densitycol == 0)
	{
		shinyjs::show(id = "error_msg",anim = TRUE)
		shinyjs::html("error_msg","Density can't be empty")
		return()
	}
if(input$growthratecol == 0)
	{
		shinyjs::show(id = "error_msg",anim = TRUE)
		shinyjs::html("error_msg","Growth Rate can't be empty")
		return()
	}
if(input$worldpopulationpercentagecol == 0)
	{
		shinyjs::show(id = "error_msg",anim = TRUE)
		shinyjs::html("error_msg","World Population Percentage can't be empty")
		return()
	}
if(input$malepopcol == 0)
	{
		shinyjs::show(id = "error_msg",anim = TRUE)
		shinyjs::html("error_msg","Male Population can't be empty")
		return()
	}
if(input$femalepopcol == 0)
	{
		shinyjs::show(id = "error_msg",anim = TRUE)
		shinyjs::html("error_msg","Female Population can't be empty")
		return()
	}
if(input$childrenpopcol == 0)
	{
		shinyjs::show(id = "error_msg",anim = TRUE)
		shinyjs::html("error_msg","Children Population can't be empty")
		return()
	} 
 if(input$totalpopcol == 0)
	{
		shinyjs::show(id = "error_msg",anim = TRUE)
		shinyjs::html("error_msg","Total Population can't be empty")
		return()
	}

 drvr = dbDriver("MySQL") 
 connection = dbConnect(drvr,host="your_hostname",dbname="your_dbname",user="your_username",pass="your_password")
 stamnts1 = sprintf("UPDATE %s SET `Rank` = %f, Area = %f, Density = %f, `Growth Rate` = %f, `World Population Percentage` = %f, MalePopulation = %f, FemalePopulation = %f, WorkingClassPopulation = %f, ChildrenPopulation = %f, TotalPopulation = %f  WHERE `Country/Territory` = '%s' ",renamecont,renamerank,renamearea, renamedensity, renamegrowth, renameworldpop, renamemalepop, renamefemalepop, renamework, renamechpop, renametotalpop,renamecount)
 dbSendQuery(connection,statement = stamnts1)
 shiny::showNotification("Successfully updated column values")
}
)

#INSERT PAGE

output$tab5UI <- renderUI ({
fluidPage(
fluidRow(
column(width = 4,
    		box(
            title = "Nominal Details",width=NULL,
		tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#rankcol').on('input', function() {
     					     var regex = /[^0-9]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
            textInput(inputId="rankcol", label="Rank", value='', width = NULL, placeholder = NULL),
		tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#cca3col').on('input', function() {
     					     var regex = /[^a-zA-Z]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
            textInput(inputId="cca3col", label="Country Code", value='', width = NULL, placeholder = NULL),
		tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#continentcol').on('input', function() {
     					     var regex = /[^a-zA-Z]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
		textInput(inputId="continentcol", label="Continent", value='', width = NULL, placeholder = NULL),
		tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#countrycol').on('input', function() {
     					     var regex = /[^a-zA-Z]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
  		textInput(inputId="countrycol", label="Country/Territory", value='', width = NULL, placeholder = NULL)
		)
),
column(width=4,
            box(
            title = "Global Percentages",width=NULL,
		tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#areacol').on('input', function() {
     					     var regex = /[^0-9.]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
		textInput(inputId="areacol", label="Area", value='', width = NULL, placeholder = NULL),
		tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#densitycol').on('input', function() {
     					     var regex = /[^0-9.]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
		textInput(inputId="densitycol", label="Density", value='', width = NULL, placeholder = NULL),
		tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#growthratecol').on('input', function() {
     					     var regex = /[^0-9.]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
            textInput(inputId="growthratecol", label="Growth Rate", value='', width = NULL, placeholder = NULL),
		tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#worldpopulationpercentagecol').on('input', function() {
     					     var regex = /[^0-9.]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
            textInput(inputId="worldpopulationpercentagecol", label="World Population Percentage", value='', width = NULL, placeholder = NULL)
		)
),
column(width=4,
		box(
            title = "Population Counts",width=NULL,
		tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#malepopcol').on('input', function() {
     					     var regex = /[^0-9]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
		textInput(inputId="malepopcol", label="Male Population", value='', width = NULL, placeholder = NULL),
		tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#femalepopcol').on('input', function() {
     					     var regex = /[^0-9]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
  		textInput(inputId="femalepopcol", label="Female Population", value='', width = NULL, placeholder = NULL),
		tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#workingclasspopcol').on('input', function() {
     					     var regex = /[^0-9]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
		textInput(inputId="workingclasspopcol", label="Working Class Population", value='', width = NULL, placeholder = NULL),
		tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#childrenpopcol').on('input', function() {
     					     var regex = /[^0-9]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
		textInput(inputId="childrenpopcol", label="Children Population", value='', width = NULL, placeholder = NULL)
		)
      ) #col
), #row

fluidRow
(
  column(
	width=12,
	box(
		title = "Submit",width = NULL,background="black",
		tags$head(
   					 tags$script("
					      $(document).ready(function() {
    					    $('#totalpopcol').on('input', function() {
     					     var regex = /[^0-9]/g;
   					       $(this).val($(this).val().replace(regex, ''));
        					});
     						 });
   						 ")
						  ),
		textInput(inputId="totalpopcol", label="Total Population", value='', width = NULL, placeholder = NULL),
		actionButton("insertvals","Add new record")
	   )
	)
 )
) #page
})

observeEvent(input$insertvals,
{ 
 rankinput <- as.numeric(input$rankcol)
 ccainput <- input$cca3col
 continentinput <- input$continentcol
 countryinput <- input$countrycol
 areainput <- as.numeric(input$areacol)
 densityinput <- as.numeric(input$densitycol)
 growthrateinput <- as.numeric(input$growthratecol)
 wppinput <- as.numeric(input$worldpopulationpercentagecol)
 maleinput <- as.numeric(input$malepopcol)
 femaleinput <- as.numeric(input$femalepopcol)
 workingclassinput <- as.numeric(input$workingclasspopcol)
 childinput <- as.numeric(input$childrenpopcol)
 totalinput <- as.numeric(input$totalpopcol)


 drv = dbDriver("MySQL") 
 conne = dbConnect(drv,host="your_hostname",dbname="your_dbname",user="your_username",pass="your_password")
 stamnt = sprintf("INSERT INTO %s VALUES (%f,'%s','%s','%s',%f,%f,%f,%f,%f,%f,%f,%f,%f)",continentinput,rankinput,ccainput,continentinput,countryinput,areainput,densityinput,growthrateinput,wppinput,maleinput,workingclassinput,femaleinput,childinput,totalinput)
 dbSendQuery(conne,statement = stamnt)
 shiny::showNotification("Successfully inserted values")

}
)

#HOME PAGE

output$tab1UI <- renderUI ({


tags$head(
      tags$style(
        HTML(
          ".box-body {
            text-align: center;
          }"
        )
      )
    )
    box(
      title = "Data Visualization Laboratory - Population Analyzer",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      height = 425,
      div(
          HTML("<b>Introduction:</b>")),
     div(
          "The application is developed by Ashmitha S and Arthi Saradha K N as an effort to analyze the population trends across various continents of the globe. It makes use of the lates6t global population data to provide an interface for the end users to perform CRUD operations on the dataset as and when the fluctuations occur to reflect upon the current information. In addition to this, the last tab in this project is dedicated to present a dashboard which represents the statistical data, visually for gaining quick insights.",
           class = "box-body"
      ),
     div(
          HTML("<b>Tabs in the Project:</b>")),
     div(style = "white-space: pre;", HTML("\tTab 1: View Data Tables\n\tTab 2: Update Existing Entries\n\tTab 3: Insert New Entries\n\tTab 4: Delete Existing Entries\n\tTab 5: Interactive Dashboard\n")),
     div(
          HTML("<b>\nTools used:</b>")),
     div(style = "white-space: pre;", HTML("\tFront End: R Shiny\n\tBack End: Java Script\n\tData Base: MySQL Workbench"))
    )
})

}

#RUNNING THE APP

shinyApp(ui = ui, server=server)



















