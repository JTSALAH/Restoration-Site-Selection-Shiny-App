# Restoration Site Choice with AHP - Shiny Application

require(shiny)
require(sf)
require(ahpsurvey)
require(dplyr)
require(here)
require(leaflet)

ui <- fluidPage(
  titlePanel("Restoration Site Choice with AHP"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("contig_cohesion", "Contiguity vs Cohesion",     min = -9, max = 9, value = 1),
      sliderInput("contig_np", "Contiguity vs Number of Patches",  min = -9, max = 9, value = 4),
      sliderInput("contig_elev", "Contiguity vs Elevation",        min = -9, max = 9, value = 8),
      sliderInput("contig_slope", "Contiguity vs Slope",           min = -9, max = 9, value = 4),
      sliderInput("cohesion_np", "Cohesion vs Number of Patches",  min = -9, max = 9, value = 3),
      sliderInput("cohesion_elev", "Cohesion vs Elevation",        min = -9, max = 9, value = 7),
      sliderInput("cohesion_slope", "Cohesion vs Slope",           min = -9, max = 9, value = 5),
      sliderInput("np_elev", "Number of Patches vs Elevation",     min = -9, max = 9, value = 5),
      sliderInput("np_slope", "Number of Patches vs Slope",        min = -9, max = 9, value = 2),
      sliderInput("elev_slope", "Elevation vs Slope",              min = -9, max = 9, value = -2)
    ),
    mainPanel(
      uiOutput("desc"),
      uiOutput("guide"),
      uiOutput("header"),
      textOutput("selectedParcel"),
      textOutput("cr"),
      leafletOutput("map")
    )
  )
)

server <- function(input, output) {
  # Read shapefile
  parcels <- reactive({
    st_read(here('data', 'Parcels', 'MTTOBY_PARCELS.shp'))
  })
  
  # Create a reactive expression for the optimal parcel ID based on AHP analysis
  optimal_parcel <- reactive({
    importance <- data.frame(
      contig_cohesion = input$contig_cohesion,
      contig_np       = input$contig_np,
      contig_elev     = input$contig_elev,
      contig_slope    = input$contig_slope,
      cohesion_np     = input$cohesion_np,
      cohesion_elev   = input$cohesion_elev,
      cohesion_slope  = input$cohesion_slope,
      np_elev         = input$np_elev,
      np_slope        = input$np_slope,
      elev_slope      = input$elev_slope
    )
    
    ahp_mat <- ahp.mat(importance, atts = c("contig", "cohesion", "np", "elev", "slope"), negconvert = FALSE)
    imean <- ahp.indpref(ahp_mat, c("contig", "cohesion", "np", "elev", "slope"), method = "arithmetic")
    cr <- ahp.cr(ahp_mat, c("contig", "cohesion", "np", "elev", "slope"))
    
    all_parcels <- read.csv(here('Normalized_Parcel_Metrics.csv'))
    orig_atts <- c("contig_mn", "cohesion", "np", "avg_elev", "avg_slope")
    names(imean) <- orig_atts
    all_parcels$score <- rowSums(sapply(names(imean), function(criterion) {
      all_parcels[[criterion]] * imean[[criterion]]
    }))
    optimal_parcel <- all_parcels[order(-all_parcels$score), ]
    return(list(id = optimal_parcel[1, "X"], cr = cr))
  })
  
  output$selectedParcel <- renderText({
    parcel_info <- optimal_parcel()
      paste("The selected parcel ID is:", parcel_info$id)
  })
  
  output$cr <- renderText({
    parcel_info <- optimal_parcel()
    if (parcel_info$cr < 0.1) {
      paste("CR =", parcel_info$cr, "which indicates consistent preferences!")
    } else {
      paste("CR =", parcel_info$cr, "which indicates your preferences are inconsistent, please adjust your weights.")
    }
  })
  
  parcel_colors <- reactive({
      parcel_info <- optimal_parcel()
      # Create a vector with 'orange' as the default color for all parcels
      colors <- rep("white", nrow(parcels()))
      # Find the index of the optimal parcel and set it to 'green'
      colors[parcel_info$id] <- "green"
      return(colors)
  })
  
  output$map <- renderLeaflet({
    leaflet(parcels()) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addProviderTiles(providers$Stadia.StamenTerrainLabels, options = providerTileOptions(opacity = 0.7)) %>% 
      addPolygons(
        fillColor = parcel_colors(),
        color = "black",
        weight = 1,
        fillOpacity = 0.5
      )
  })
  
  observeEvent(input$contig_cohesion, {
    leafletProxy("map", data = parcels()) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = parcel_colors(),
        color = "black",
        weight = 1,
        fillOpacity = 0.5
      )
  })
  
  output$desc <- renderUI({
    HTML("<h2>Analysis Description</h2>
        <p>The Analytical Hierarchy Process is a model selection tool designed to
        make the best choices based on user preferences. AHP is a structured technique 
        for helping you make complex decisions by breaking down the decision into a 
        hierarchy of more easily comprehensible parts. In this project we can 
        use landscape characteristics such as contiguity, cohesion, number of 
        patches, elevation, and slope to determine which parcel is most in 
        need of a restoration at Mt. Toby State Park in Western Massachusetts.</p>")
  })
  
  output$guide <- renderUI({
    HTML("<h2>Application Tutorial</h2>
        <p>In this application, the sliders allow you to adjust the importance of one variable relative to another to determine which parcel is most in need of restoration. For example, the slider 'Contiguity vs Cohesion' lets you decide how much more important 'Contiguity' is compared to 'Cohesion'.</p>
        <ul>
          <li>Positive values indicate a preference for the variable listed first. For instance, a value of 4 on 'Contiguity vs Cohesion' means 'Contiguity' is four times as important as 'Cohesion'.</li>
          <li>Negative values indicate a preference for the variable listed second. Thus, a value of -4 would mean 'Cohesion' is four times as important as 'Contiguity'.</li>
        </ul>
        <table style='width: 60%; margin: auto; border-collapse: collapse; border: 1px solid black;'>
          <tr>
            <th style='text-align: center; border: 1px solid black;'>Importance</th>
            <th style='text-align: center; border: 1px solid black;'>Definition</th>
          </tr>
          <tr>
            <td style='text-align: center; border: 1px solid black;'>1</td>
            <td style='text-align: center; border: 1px solid black;'>Equal importance</td>
          </tr>
          <tr>
            <td style='text-align: center; border: 1px solid black;'>2</td>
            <td style='text-align: center; border: 1px solid black;'>Equal to moderate importance</td>
          </tr>
          <tr>
            <td style='text-align: center; border: 1px solid black;'>3</td>
            <td style='text-align: center; border: 1px solid black;'>Moderate importance</td>
          </tr>
          <tr>
            <td style='text-align: center; border: 1px solid black;'>4</td>
            <td style='text-align: center; border: 1px solid black;'>Moderate to strong importance</td>
          </tr>
          <tr>
            <td style='text-align: center; border: 1px solid black;'>5</td>
            <td style='text-align: center; border: 1px solid black;'>Strong importance</td>
          </tr>
          <tr>
            <td style='text-align: center; border: 1px solid black;'>6</td>
            <td style='text-align: center; border: 1px solid black;'>Strong to very strong importance</td>
          </tr>
          <tr>
            <td style='text-align: center; border: 1px solid black;'>7</td>
            <td style='text-align: center; border: 1px solid black;'>Very strong importance</td>
          </tr>
          <tr>
            <td style='text-align: center; border: 1px solid black;'>8</td>
            <td style='text-align: center; border: 1px solid black;'>Very to extremely strong importance</td>
          </tr>
          <tr>
            <td style='text-align: center; border: 1px solid black;'>9</td>
            <td style='text-align: center; border: 1px solid black;'>Extremely importance</td>
          </tr>
        </table>
        <p>The application uses your preferences to calculate an overall score for each parcel, helping you identify the most suitable parcel for restoration efforts. 
         Ensure the consistency of your preferences as inconsistent judgments can lead to unreliable results. The Consistency Ratio (CR) provided helps monitor the consistency of your pairwise comparisons. A CR less than 0.1 generally indicates a consistent matrix.</p>")
  })
  
  
  output$header <- renderUI({
    HTML("<h2>Analytical Hierarchy Process - Parcel Selection</h2>")})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
