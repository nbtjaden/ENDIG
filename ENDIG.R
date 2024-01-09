####################################
## load packages
library(shiny)
library(ggplot2)
library(sf)
library(bslib)  # shiny themes (optional)

####################################
## load data
# meta data
load(file.path("./data/processed/disease_IDs.rds"))
load(file.path("./data/processed/disease_IDs_ECannex.rds"))
disease_IDs_additional <- subset(disease_IDs, !disease_IDs %in% disease_IDs_ECannex)

# spatial data background map
load(file.path("./data/processed/europe.rds"))

# spatial data disease surveillance systems
data_spatial <- readRDS(file=file.path("./data/shiny/data_spatial.rds"))

# non-spatial data disease surveillance systems
data_heatmap <- readRDS(file.path("./data/shiny/data_heatmap.rds"))


####################################
## Color definitions
# these are based on the colors the European Environmental Agency
# uses for their maps

col.nodata <- "#ffffff"
col.nonEU <- "#e3e4e4"
col.bg <- "#dcf0fa"
col.bg.line <- "#0081c6"
col.borders <- "#878889"
col.frame <- "#d8d8d8"

col.none <- "#f6a800"
col.grade1 <- "#168130"
col.grade2 <- "#6fb22c"
col.unknown <- "#bed492"

####################################
## prepare map background

bgmap <- ggplot() +
  geom_sf(data = europe, fill = col.nonEU, color = NA) +
  xlim(c(2200000, 6500000)) +
  ylim(c(1380000, 5500000)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill=col.bg),
        panel.grid = element_line(color = col.bg.line),
        panel.border = element_rect(color = col.frame, fill=NA),
        text = element_text(size = 20)
        
  )

####################################
## User interface

ui <- fluidPage(
  theme=bs_theme(bootswatch="flatly"), # optional theme, requires bslib
  titlePanel("ENDIG: European Notifiable Diseases Interactive Geovisualization"),
  sidebarLayout(
    # Sidebar/left panel
    sidebarPanel(
      # Select disease list
      radioButtons(
        inputId = "annex_list_chosen",
        label = "Disease list",
        choiceNames = c("EC communicable diseases list", "Additional diseases"),
        choiceValues = c(TRUE, FALSE)
      ),
      # Select disease
      selectInput(inputId = "disease", label = "Choose a disease/pathogen:", choices=disease_IDs_ECannex),
      # choose surveillance system characteristic to display
      radioButtons(
        inputId = "systype",
        label = "Surveillance system characteristics:",
        selected = NULL,
        inline = FALSE,
        width = NULL,
        choiceNames = c("compulsory vs. voluntary", "comprehensive vs. sentinel", "active vs. passive", "aggregated vs. case-based"),
        choiceValues = c("compu", "compre", "act", "agg")
        #choiceValues = c("compulsory", "comprehensive", "active", "aggregated")
      )
    ),
    # Main Panel
    mainPanel(
      tabsetPanel(
        tabPanel("Spatial view",
                 div(style = "margin: auto; width: 80%",
                     plotOutput("EUmap",  width = "100%"),
                     sliderInput(inputId = "year",
                                 label = "choose year",
                                 min=2015, max=2021, value=2021,
                                 sep="",
                                 width="80%"),
                 ),
        ), 
        tabPanel("Temporal view", plotOutput("heatmap",  width = "100%")), 
        tabPanel("Info",
                 h3("Research paper & source code"),
                 div(
                   p("For further information, please refer to the research paper published in AGILE-GISS: "),
                   div(tags$ul(
                     tags$li("Tjaden, N. B. and Blanford, J. I.: ENDIG: Interactive Geovisualization of Surveillance Systems for Notifiable Diseases in Europe, AGILE GIScience Ser., 4, 46,", tags$a(href="https://doi.org/10.5194/agile-giss-4-46-2023", "https://doi.org/10.5194/agile-giss-4-46-2023"), ", 2023." 
                     )
                   ),
                   p("Source code for data processing and the ENDIG application is available from: ",
                     tags$a(href='https://github.com/nbtjaden/ENDIG', 'https://github.com/nbtjaden/ENDIG')),
                   ),
                 ),
                 h3("Disease surveillance system classification info"),
                 div(tags$ul(
                   tags$li(tags$b("Compulsory vs. voluntary:"), "Describes whether reporting this disease is compulsory a given country or done on a voluntary basis."), 
                   tags$li(tags$b("Active vs. passive:"), "In an active system the national surveillance agency is responsible for collecting the data from healthcare providers, and in a passive one the healthcare providers are responsible for reporting to the surveillance agency."), 
                   tags$li(tags$b("Comprehensive vs. sentinel-based:"), "Describes whether all (comprehensive) or only a representative sample (sentinel) of healthcare providers supply data."), 
                   tags$li(tags$b("Case-based vs. aggregated:"), "Describes whether full (anonymized) case data or total number of cases are reported.") 
                 )),
                 h3("Data sources & source code"),
                 div(
                   p("All administrational boundaries are shown according to and using data from ",
                     tags$a(href='https://www.naturalearthdata.com/', 'Natural Earth')
                   ),
                   p("Data for disease surveillance systems is ©European Centre for Disease Prevention and Control (ECDC) 2015-2023. See",
                     tags$a(href='https://www.ecdc.europa.eu/en/copyright', 'ECDC copyright information'),
                     " for details. The individual tables can be found at the following links:",
                     div(tags$ul(
                       tags$li(tags$a(href="https://www.ecdc.europa.eu/en/publications-data/surveillance-systems-overview-2015", "Surveillance systems overview for 2015")),
                       tags$li(tags$a(href="https://www.ecdc.europa.eu/en/publications-data/surveillance-systems-overview-2016", "Surveillance systems overview for 2016")),
                       tags$li(tags$a(href="https://www.ecdc.europa.eu/en/publications-data/surveillance-systems-overview-2017", "Surveillance systems overview for 2017")),
                       tags$li(tags$a(href="https://www.ecdc.europa.eu/en/publications-data/surveillance-systems-overview-2018", "Surveillance systems overview for 2018")),
                       tags$li(tags$a(href="https://www.ecdc.europa.eu/en/publications-data/surveillance-systems-overview-2019", "Surveillance systems overview for 2019")),
                       tags$li(tags$a(href="https://www.ecdc.europa.eu/en/publications-data/surveillance-systems-overview-2020", "Surveillance systems overview for 2020")),
                       tags$li(tags$a(href="https://www.ecdc.europa.eu/en/publications-data/surveillance-systems-overview-2021", "Surveillance systems overview for 2021")),
                     )
                     )
                   )
                 )
        )
      )
    )
  )
)


####################################
## Server
server <- function(input, output, session){
  # switch disease list based on radio buttons
  observe({
    x <- input$annex_list_chosen
    if(x){
      updateSelectInput(session, "disease",
                        choices = disease_IDs_ECannex
      )
    }else{
      updateSelectInput(session, "disease",
                        choices = disease_IDs_additional
      )
    }
  }
  )

  # Plot the EU map
  output$EUmap <-renderPlot(
    bgmap+
      geom_sf(data_spatial[[as.character(input$year)]][[input$disease]], mapping=aes(fill = !! sym(input$systype))) +
      scale_fill_manual(values=c(col.none, col.grade1, col.grade2, col.unknown, col.nodata), drop=FALSE, name ="Surveillance System") +
      geom_sf(data = europe, fill = NA, color = col.bg.line)
  )
  
  # Plot the heatmap
  output$heatmap <- renderPlot(
    ggplot(data_heatmap[[input$disease]], aes(x=Year, y=Country, fill= !! sym(input$systype))) +
      geom_tile(color=col.borders)+
      scale_fill_manual(values=c(col.none, col.grade1, col.grade2, col.unknown, col.nodata), drop=FALSE, name ="Surveillance System") +
      scale_y_discrete(limits=rev) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            text = element_text(size = 20),
            aspect.ratio=2
      ),
    height=600
  )
}

####################################
## run the app
shinyApp(ui = ui, server = server)