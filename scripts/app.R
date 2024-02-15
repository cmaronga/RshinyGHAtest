library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(rvest)


# Specify the application port
# Having maximum control over the deployment process of the application

options(shiny.host = "0.0.0.0") # host on which the app will run
options(shiny.port = 8181)      # port for which the app will run

# Webscraping date of publication into CRAN -------------------------------

#Specifying the url for desired website to be scraped
url <- 'https://cran.r-project.org/web/packages/available_packages_by_date.html'

# Reading the HTML code from the website
webpage <- read_html(url)


# Date published ----------
date_published <- html_nodes(webpage,'td:nth-child(1)')

# Converting the ranking data to text
date_published_data <- html_text(date_published)

# Name of package ----------
pkg_name <- html_nodes(webpage, "a")


# pkg name data to text
pkg_name_data <- html_text(pkg_name)


# Title of package ----------
pkg_title <- html_nodes(webpage, "td~ td+ td")


# pkg name data to text
pkg_title_data <- html_text(pkg_title)


# Create a data frame -----------------------------------------------------

cran_pkg_date <- data.frame(
  date_published = date_published_data,
  Name = pkg_name_data,
  Title = pkg_title_data
) %>% mutate(
  date_published = lubridate::ymd(date_published)
)

# + Available packages

available_pks <- available.packages(#repos = "http://cran.us.r-project.org", 
  repos = "https://cran.r-project.org/")[, c("Version",
                                             "Depends",
                                             "Repository", 
                                             "NeedsCompilation",
                                             "License")] %>% 
  as.data.frame()%>% tibble::rownames_to_column(var = "pkg_name")


# Merge with data with dates ----------------------------------------------

CRAN_pkgs <- available_pks %>% 
  left_join(cran_pkg_date, 
            by = c("pkg_name" = "Name")) %>% 
  mutate(
    year_published = lubridate::year(date_published)
  )



# Define header
header_contents <- dashboardHeader(
  title = "Exploring CRAN packages",
  titleWidth = 400
)

# define sidebar
sideBar_contents <- dashboardSidebar(
  sidebarMenu(
    menuItem(h5("CRAN packages list"),
             tabName = "cran_pkgs")
  )
)

# define body
body_contents <- dashboardBody(
  tabItems(
    tabItem(tabName = "cran_pkgs",
            fluidRow(
              box(width = 10, solidHeader = T, status = "primary",
                  dataTableOutput("all_pkgs"),
                  title = paste0("List of total number of packages available in CRAN as of ", format(Sys.time(), '%d %B, %Y'))
              ),
              
              box(width = 2,
                  title = "Control Displays", solidHeader = T, status = "primary",
                  
                  radioButtons("display", "Choose display type:",
                               choices = c("Table", "Line Graph", "Bar plot"),
                               selected = "Table"),
                  tags$hr(),
                  
                  selectInput("year", "Select year (Table ONLY):",
                              choices = c("All time", sort(unique(CRAN_pkgs$year_published), decreasing = TRUE)),
                              selected = "All time"),
                  
                  textOutput("pkg_statement"),
                  
                  tags$hr(),
                  radioButtons("compilation", "Need compilation(Table ONLY)",
                               choices = c("Yes", "No"))
              )
              
            )
    )
  )
)


# combine into app

ui <- dashboardPage(header = header_contents,
                    sidebar = sideBar_contents,
                    body = body_contents)

# define server
server <- function(input, output, session){
  
  # create function to download tables
  # call the function like 'download_DT()' at end of the table
  
  download_DT <- function(x) {
    DT::datatable(
      x, rownames = F,
      extensions = 'Buttons',
      options = list(
        dom = 'Blfrtip',
        buttons = c('csv', 'excel', 'pdf'),
        lengthMenu = list(c(15, 25, 50, -1),
                          c(15, 25, 50, "All"))
      )
    )
  }
  

  # reactive dataset for the tables
  cran_pks <- reactive({
    if (input$year == "All time"){
      CRAN_pkgs %>% 
        select(pkg_name, Version, date_published, Title, Repository, NeedsCompilation) %>% 
        arrange(desc(date_published))
    } else {
      CRAN_pkgs %>% 
        filter(year_published == as.numeric(input$year)) %>% 
        select(pkg_name, Version, date_published, Title, Repository, NeedsCompilation) %>% 
        arrange(desc(date_published))
      
      
    }
  })
  
  output$all_pkgs <- renderDataTable(
    cran_pks() %>% download_DT(),
    rownames = F
  )
  
  # package statement
  output$pkg_statement <- renderText(
    
    if (input$year != "All time"){
      paste0("A total of ", nrow(cran_pks()), " packages were published in the year ", input$year) 
    } else {
      paste0("All time published packages ", nrow(cran_pks()))
    }
  )
  
  
  
}

shinyApp(ui = ui, server = server)