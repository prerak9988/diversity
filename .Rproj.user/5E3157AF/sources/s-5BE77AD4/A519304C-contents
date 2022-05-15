library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(markdown)
library(shinythemes)
library(ggplot2)
library(shinyWidgets)
library(DT)
library(dplyr, warn.conflicts = FALSE)
library(shinyjs)
library(readr)
library(dtplyr)
library(bslib)
library(shinyBS)
library(openxlsx)
library(writexl)
library(readxl)
library(ggiraph)
library(ggplot2)
library(rpivotTable)
library(collapsibleTree)
library(maps)
library(leaflet)
library(timevis)




#Code to filter Poland data - Use chunks, vectorization and filter
#gen = read_csv("occurence.csv", chunksize=1000000)
#df = union_all((x.query("country == 'Poland'") for x in gen), ignore_index=True)


# Setting Paths
# Change boolean while running on docker

local = TRUE
docker = FALSE

#for running locally
if (local == TRUE)
  
{
  multi = "/Users/prsethia/Documents/Rshiny/diversity/app/multimedia.rds"
  occur = "/Users/prsethia/Documents/Rshiny/diversity/app/Occur.csv"
  
}

# for running on docker
if (docker == TRUE)
  
{
  #when running on docker
  multi = "/diversity/app/multimedia.rds"
  occur = "/diversity/app/Occur.csv"
  
}



# Data Transformations
#--------------------

occ <- read.csv(occur)

occ$occurrenceID <-
  paste0("<a href='", occ$occurrenceID, "'>", occ$occurrenceID, "</a>")
occ$references <-
  paste0("<a href='", occ$references, "'>", occ$references, "</a>")

#mult <- readRDS("multimedia.rds")

occ$Year <- as.POSIXct(occ$eventDate, format = "%Y")
occ$Year <- format(occ$Year, format="%Y")

data <- occ[, c("vernacularName","eventDate" , "modified")]

colnames(data) <- c('content', 'start', 'end')




###########
# LOAD UI #
###########


ui <- tagList(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = "shinyjs.refreshApp = function() { location.reload(); }", functions = c("refreshApp")),
  
  
  #Theme Selector
  shinythemes::themeSelector(),
  div(id = "loading"),
  tags$head(
    tags$link(href = "loadingsign.css", rel = "stylesheet", type = "text/css"),
    tags$script(src = "loader.js")
  ),
  
  
  
  # to add favicon
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.png")),
  
  tags$footer(
    "Designed with ❤️ by Prerak Sethia (Email: pr.seth9988@gmail.com)",
    style =
      "position: fixed;
               bottom: 0;
               width: 100%;
               height: 30px;
               padding-top: 10px;
               padding-bottom: 10px;
               text-align: center;
               color: white;
               background-color: #2c3e50;
               z-index: 900;"
  ),
  
  
  
  
  navbarPage(
    theme = shinytheme("flatly"),
    title = div(
      tags$img(
        height = 33,
        width = 45,
        src = paste0("front.jpg"),
        style = "left: 0px; margin-right: 0px; padding-bottom: 5px; top: 10px;"
      ),
      "Global Diversity"
      
    ),
    
    
    tabPanel(
      "Data",
      icon = icon("table"),
      fluidPage(
        setBackgroundColor("ghostwhite"),
        tags$h4("Species Details from Poland"),
        br(),
        
        fluidRow(column(
          2,
          pickerInput(
            "Ver",
            "Vernacular Name:",
            choices = unique(as.character(occ$vernacularName)),
            selected = TRUE,
            multiple = T,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `virtualScroll` = TRUE,
              `selected-text-format` = "count > 3",
              `count-selected-text` = "Multi-selected",
              size = 10
            )
          )
          
          
        ),
        
        column(
          2,
          pickerInput(
            "Sci",
            "Scientific Name:",
            choices = unique(as.character(occ$scientificName)),
            selected = TRUE,
            multiple = T,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `virtualScroll` = TRUE,
              `selected-text-format` = "count > 3",
              `count-selected-text` = "Multi-selected",
              size = 10
            )
          )
        )),
        
        
        fluidRow(column(
          2,
          offset = 10,
          downloadButton('dl', 'Download-filtered-data', class =
                           "butt"),
          tags$head(
            tags$style(".butt{background-color:#2c3e50;} .butt{color: #17A589;}")
          )
        )),
        
        br(),
        
        fluidRow(
          br(),
          column(12, DT::dataTableOutput("table1")),
          
          br(),
          br(),
          
          tags$h6("Move pages")
        )
      )
    ),
    
    tabPanel("Map",
             icon = icon("thumbtack"),
             fluidPage(setBackgroundColor("ghostwhite"),
                       
                       fluidRow(
                         
                         tags$head(tags$style(
                           HTML(
                             "
                                                                               .item {
                                                                     background: #17A589 !important;
                                                                      color: white !important;
                                                                                            }
                                                                    .selectize-dropdown-content .active {
                                                                     background: #2c3e50 !important;
                                                                     color: white !important;
                                                                                      }
                                                                                       "
                           )
                         )),
                         
                         column(
                           3,
                           selectizeInput(
                             "Vern",
                             "Vernacular Name:",
                             choices = TRUE,
                             
                             multiple = FALSE,
                             selected = TRUE,
                             options = list(
                               
                               placeholder = 'Select Scientific Name',
                               plugins = list("drag_drop", "remove_button")
                               
                             )
                           )
                         ),
                         
                         column(
                           3,
                           selectizeInput(
                             "Scin",
                             "Scientific Name:",
                             choices = TRUE,
                             
                             multiple = FALSE,
                             selected = TRUE,
                             options = list(
                              
                               placeholder = 'Select Scientific Name',
                               plugins = list("drag_drop", "remove_button")
                               
                             )
                           )
                         )
                         
                       ),
                       
                       box(
                       tags$h5('Map'),
                       leafletOutput("mymap", width="100%", height = "800px") ),
                       
                      
                       
                      box(
                          tags$h5('Timeline'),
                          timevisOutput("timeline"))
                          





             )),

                       
               
    
    tabPanel("Metrics",
             icon = icon("tasks"),
             fluidPage(setBackgroundColor("ghostwhite"),
                       tags$h4("Number of Observations"),
                       
                       tags$h5("Multiple metrics can be compared and sorted at different levels and aggregations for a maximum of 5 Species"),
                       
                       
                       fluidRow(
                         
                         tags$head(tags$style(
                                     HTML(
                                       "
                                                                               .item {
                                                                     background: #17A589 !important;
                                                                      color: white !important;
                                                                                            }
                                                                    .selectize-dropdown-content .active {
                                                                     background: #2c3e50 !important;
                                                                     color: white !important;
                                                                                      }
                                                                                       "
                                     )
                                   )),
                         

                         column(
                                     3,
                                     selectizeInput(
                                       "PID",
                                       "Scientific Name:",
                                       choices = TRUE,
                                       multiple = TRUE,
                                       selected = TRUE,
                                       options = list(
                                         maxItems = 5,
                                         placeholder = 'Select Scientific Name',
                                         plugins = list("drag_drop", "remove_button")
                                         
                                       )
                                     )
                                   )
                       ),
                       
                       fluidRow(
                       
                       tags$head(
                         tags$style(
                           type = 'text/css',
                           '#pivotREBU{ overflow-x: scroll; }',

                           '.pvtTable {font-family: tahoma; background: #2c3e50;}.pvtRowLabel { background: #2c3e50 none repeat scroll 0 0;}

                           .pvtRows, .pvtCols { background: #2c3e50 none repeat scroll 0 0; font: 15px tahoma; } '
                         )
                       ),
                       
                       box(
                       rpivotTableOutput('pivotREBU', width = "100%", height = "700px")),
                       
                       box(
                       ggiraphOutput("plot3"))
                       
                       )
                       
                       
                       
             )
             
    ),
    
    tabPanel("Species Tree - Taxonomy",
             icon = icon("tree"),
             fluidPage(setBackgroundColor("ghostwhite"),
                       
                       sidebarPanel(
                         
                         selectInput(
                           "hierarchy", "Tree hierarchy",
                           choices = c(
                             "taxonRank","kingdom", "family","lifeStage", "sex", "vernacularName","locality" 
                           ),
                           selected = c( "taxonRank","kingdom", "family", "lifeStage"),
                           multiple = TRUE
                         ),
                         
                         selectInput(
                           "fill", "Node color",
                           choices = c("individualCount"),
                           selected = "individualCount"
                           
                         ),
                         
                         tags$p("The node you most recently clicked:"),
                         verbatimTextOutput("str")
                          
                       ),
                       
                       # Tree diagram with the selected root node
                       mainPanel(
                         
                         tags$head(tags$style(
                           HTML(
                             "
                                                                               .item {
                                                                     background: #17A589 !important;
                                                                      color: white !important;
                                                                                            }
                                                                    .selectize-dropdown-content .active {
                                                                     background: #2c3e50 !important;
                                                                     color: white !important;
                                                                                      }
                                                                                       "
                           )
                         )),
                         
                         column(
                           3,
                           selectizeInput(
                             "Sci2",
                             "Scientific Name:",
                             choices = TRUE,
                            
                             multiple = TRUE,
                             selected = TRUE,
                             options = list(
                               maxItems = 5,
                               placeholder = 'Select Scientific Name',
                               plugins = list("drag_drop", "remove_button")
                               
                             )
                           )
                         ),
                         
                         
                         tags$h4("Visualize a collapsible hierarchical tree of Species"),
                         
                         collapsibleTreeOutput("plot", height = "500px"))
                        
             )
             
    )
    
    
    
  )

)
    
    
    
    ###############
    # LOAD SERVER #
    ###############
    
    
    
    server <- function(input, output, session) {
      
      
      #taxonomy Server code
      #-------------------
      
      
      updateSelectizeInput(
        session = session,
        inputId = 'Sci2',
        choices = unique(occ$scientificName),
        selected = c('Oriolus oriolus'),
        server=TRUE
        
      )
      
      observe({
        
        occ <-
          occ[  (occ$scientificName %in% input$Sci2)
              ,]
        
        output$plot <- renderCollapsibleTree({
          collapsibleTreeSummary(
            occ,
            hierarchy = input$hierarchy,
            inputId = "node",
            root = input$fill,
            attribute = input$fill
          )
        })
        
        output$str <- renderPrint(str(input$node))
        
        
        
      })
      
     
      
      
      
      # Data Server Code
      #-----------------
      
      
      observe({
        updatePickerInput(
          session = session,
          inputId = 'Ver',
          choices = unique(occ$vernacularName),
          selected = unique(occ$vernacularName)
          
        )
        
        updatePickerInput(
          session = session,
          inputId = 'Sci',
          choices = unique(occ$scientificName),
          selected = unique(occ$scientificName)
          
        )
        
      })
      
      
      observe({
        occ <-
          occ[(occ$vernacularName %in% input$Ver) &
                (occ$scientificName %in% input$Sci)
              ,]
        
        occ <- distinct(occ)
        
        output$table1 = DT::renderDT(
          datatable(
            occ,
            escape = FALSE,
            options = list(
              stateSave = TRUE,
              pageLength = 20,
              paging = TRUE,
              lengthMenu = list(
                c(50, 100, 200, 500, 1000),
                c('50', '100', '200', '500', '1000')
                
                
                
              ),
              autoWidth = TRUE,
              scrollX = TRUE,
              scrollY = TRUE,
              searching = TRUE,
              keys = TRUE,
              searchHighlight = TRUE,
              dom = 'Bflirtp',
              buttons = list(list(extend = 'colvis'), 'csv'),
              
              columnDefs = list(
                list(targets = '_all', className = 'dt-center'),
                list(targets = c(1:37), width = '200px')
              ),
              
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background': '#17A589','color': 'black'});",
                "}"
              )
            ),
            
            extensions = c('Buttons',
                           'ColReorder',
                           'KeyTable',
                           'FixedColumns'),
            
            callback = JS(
              '$("button.buttons-csv").css("background","#17A589");
                         $("button.buttons-colvis").css("background","#17A589");
                         return table;'
            ),
            
            filter = list(
              position = 'top',
              clear = TRUE,
              plain = TRUE
            )
            
          ),
          server = TRUE
        )
        
        
        #Download  files
        output$dl <- downloadHandler(
          # Create the download file name
          filename = function() {
            "Species_Data.xlsx"
          },
          content = function(file) {
            write_xlsx(occ, path = file)
          }
        )
        
      })
      
      
      
      # Metrics Server code
      #--------------------
      
      
      updateSelectizeInput(
        session = session,
        inputId = 'PID',
        choices = unique(occ$scientificName),
        selected = c('Oriolus oriolus', 'Anguis fragilis'),
        server=TRUE
        
      )
      
      observe({
        
        occ <-
          occ[(occ$scientificName %in% input$PID)
              ,]
        
        output$pivotREBU <- renderRpivotTable({
          
          rpivotTable(
            data = occ ,
            rows = "Year",
            cols = "family",
            vals = "catalogNumber",
            aggregatorName = "Count",
       
            aggregators = list(Count = htmlwidgets::JS('$.pivotUtilities.aggregators["Count"]'),
                               `Count Unique Values` = htmlwidgets::JS('$.pivotUtilities.aggregators["Count Unique Values"]')),
            
            
             rendererName =  "Col Heatmap",
            
            renderers = list(`Col Heatmap` = htmlwidgets::JS('$.pivotUtilities.renderers["Col Heatmap"]'),
                               Heatmap = htmlwidgets::JS('$.pivotUtilities.renderers["Heatmap"]'),
                               `Table` = htmlwidgets::JS('$.pivotUtilities.renderers["Table"]'),
                                `Table Barchart` = htmlwidgets::JS('$.pivotUtilities.renderers["Table Barchart"]'))
            
             
          )
        })
        
        
        final_table3 <- reactive({

          a <- subset(occ, occ$scientificName %in% input$PID) %>% group_by(lifeStage) %>%
            summarise(unique_SCI = n_distinct(scientificName)) %>%
            mutate(percentage = unique_SCI/sum(unique_SCI)) %>%
            mutate(percentage_label = paste0(round(100 * percentage, 1), "%"),hover_text = paste0(lifeStage, ": ", unique_SCI," Count of Scientific name", "\n ","(",percentage_label,")"))
          a <- droplevels(a)

        })



        output$plot3 <- renderggiraph({
          donut_plot3 <- ggplot(final_table3(), aes(y = unique_SCI, fill = lifeStage ))+
            geom_bar_interactive(
              aes(x = 3, tooltip = hover_text),
              width = 0.1,
              stat = "identity",
              show.legend = TRUE
            ) +
            annotate(
              geom = "text",
              x = 2.8,
              y = 0,
              label = 12,
              size = 0
            ) + scale_fill_brewer(palette = 4)+
            coord_polar(theta="y")+ theme_void() +  ggtitle("Life Stage") + theme(plot.title = element_text(hjust = 0.5))

          ggiraph(ggobj = donut_plot3)
        })
        
        
        
      })
      
      
      
      # Map server
      #-----------
      
      updateSelectizeInput(
        session = session,
        inputId = 'Vern',
        choices = unique(occ$vernacularName),
        selected = c('Common Sedge'),
        server=TRUE
        
      )
      
      updateSelectizeInput(
        session = session,
        inputId = 'Scin',
        choices = unique(occ$scientificName),
        selected = c( 'Anguis fragilis'),
        server=TRUE
        
      )
      
      observe({
        
        occ <-
          occ[(occ$vernacularName %in% input$Vern) |
                (occ$scientificName %in% input$Scin)
              ,]
        
        occ2 <-
          occ %>% mutate(
            popup_info = paste(
              "Family:",
              family,
              "<br/>",
              "Kingdom:",
              kingdom,
              "<br/>",
              "Taxon Rank:",
              taxonRank,
              "<br/>",
              "Sex:",
              sex,
              "<br/>",
              "Life Stage:",
              lifeStage,
              "<br/>",
              "Vernacular Name:",
              vernacularName,
              "<br/>",
              "Scientific Name:",
              scientificName
            )
          )
        
        output$mymap <- renderLeaflet({
          
          leaflet(occ2 %>%
                    dplyr::filter(country == "Poland"
                    )) %>%
            addTiles() %>%
            addMarkers(lat=~latitudeDecimal, lng=~longitudeDecimal, popup = ~popup_info)
          
        })
        
        data <-
          data[(data$content %in% input$Vern) 
              ,]
        
        
        output$timeline <- renderTimevis({
          timevis(data,
                  showZoom = TRUE,
                  zoomFactor = 0.5,
                  )
        })
        
        
      })
      
      
    
      
      
      
  
      
      
    }
    
    # Run the application
    shinyApp(ui = ui, server = server)
    