# ============================================================
# APP SHINY – INCENDIOS FORESTALES EN ESPAÑA (1996–2016)
# ============================================================

library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)
library(sf)
library(scales)

# ---------------------------
# CARGA Y PREPROCESADO
# ---------------------------

fires_raw <- read.csv("incendios.csv", sep = ";")

fires <- fires_raw %>%
  mutate(
    anio = as.integer(anio),
    mes_num = month(as.Date(deteccion)),   # mes numérico 1–12
    perdidassuperficiales = as.numeric(perdidassuperficiales),
    causa_binaria = case_when(
      idcausa >= 400 & idcausa <= 499 ~ "intencionado",
      TRUE ~ "no_intencionado"
    )
  )

# Etiquetas para el selector de meses
mes_labels <- setNames(
  1:12,
  paste0(1:12, " - ", month.name)
)

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  titlePanel("Incendios forestales en España (1996–2016)"),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput(
        "anio",
        "Rango de años:",
        min = min(fires$anio, na.rm = TRUE),
        max = max(fires$anio, na.rm = TRUE),
        value = c(1996, 2016),
        sep = ""
      ),
      
      selectInput(
        "comunidad",
        "Comunidad autónoma:",
        choices = c("Todas", sort(unique(fires$comunidad))),
        selected = "Todas"
      ),
      
      selectInput(
        "mes",
        "Mes del año:",
        choices = c("Todos" = 0, mes_labels),
        selected = 0
      )
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel(
          "Evolución temporal",
          plotlyOutput("evolucion_plot", height = "500px")
        ),
        
        tabPanel(
          "Patrones espaciales",
          leafletOutput("mapa_gif", height = "550px")
        ),
        
        tabPanel(
          "Meteorología",
          plotlyOutput("meteo_plot", height = "500px")
        ),
        
        tabPanel(
          "Detección por equipo",
          selectInput(
            "tipo_incendio",
            "Tipo de incendio:",
            choices = c("Todos", sort(unique(fires$claseincendio))),
            selected = "Todos"
          ),
          plotlyOutput("deteccion_plot", height = "500px")
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ---------------------------
  # FILTRO GLOBAL
  # ---------------------------
  
  fires_filtered <- reactive({
    
    df <- fires %>%
      filter(anio >= input$anio[1],
             anio <= input$anio[2])
    
    if (input$comunidad != "Todas") {
      df <- df %>% filter(comunidad == input$comunidad)
    }
    
    if (input$mes != 0) {
      df <- df %>% filter(mes_num == as.integer(input$mes))
    }
    
    df
  })
  
  # ---------------------------
  # 1. EVOLUCIÓN TEMPORAL
  # ---------------------------
  
  output$evolucion_plot <- renderPlotly({
    
    annual <- fires_filtered() %>%
      group_by(anio) %>%
      summarise(
        superficie_total = sum(perdidassuperficiales, na.rm = TRUE),
        prop_gif = mean(claseincendio == "gif", na.rm = TRUE),
        .groups = "drop"
      )
    
    plot_ly(
      annual,
      x = ~anio,
      y = ~superficie_total,
      type = "scatter",
      mode = "lines",
      line = list(color = "firebrick"),
      customdata = ~prop_gif,
      hovertemplate = paste(
        "<b>Año:</b> %{x}<br>",
        "<b>Superficie quemada:</b> %{y:,.0f} Ha<br>",
        "<b>Proporción GIF:</b> %{customdata:.2f}<extra></extra>"
      )
    ) %>%
      layout(
        xaxis = list(title = "Año"),
        yaxis = list(title = "Superficie quemada (Ha)")
      )
  })
  
  # ---------------------------
  # 2. MAPA GRANDES INCENDIOS
  # ---------------------------
  
  output$mapa_gif <- renderLeaflet({
    
    gif_sf <- fires_filtered() %>%
      filter(
        claseincendio == "gif",
        !is.na(latitud),
        !is.na(longitud)
      ) %>%
      st_as_sf(coords = c("longitud", "latitud"), crs = 4326)
    
    leaflet(gif_sf) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        radius = ~sqrt(perdidassuperficiales) / 5,
        fillColor = ~colorNumeric("YlOrRd", perdidassuperficiales)(perdidassuperficiales),
        fillOpacity = 0.6,
        stroke = TRUE,
        color = "black",
        weight = 0.3
      )
  })
  
  # ---------------------------
  # 3. METEOROLOGÍA
  # ---------------------------
  
  output$meteo_plot <- renderPlotly({
    
    meteo <- fires_filtered() %>%
      filter(claseincendio == "gif") %>%
      select(tempmaxima, humrelativa, perdidassuperficiales) %>%
      drop_na()
    
    plot_ly(
      meteo,
      x = ~tempmaxima,
      y = ~humrelativa,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = ~sqrt(perdidassuperficiales) * 0.5 + 2,
        color = ~perdidassuperficiales,
        colorscale = "Reds",
        showscale = TRUE
      )
    ) %>%
      layout(
        xaxis = list(title = "Temperatura máxima (ºC)"),
        yaxis = list(title = "Humedad relativa (%)")
      )
  })
  
  # ---------------------------
  # 4. DETECCIÓN POR EQUIPO
  # ---------------------------
  
  output$deteccion_plot <- renderPlotly({
    
    df <- fires_filtered()
    
    if (input$tipo_incendio != "Todos") {
      df <- df %>% filter(claseincendio == input$tipo_incendio)
    }
    
    detect_trend <- df %>%
      filter(!is.na(iddetectadopor)) %>%
      group_by(anio, iddetectadopor) %>%
      summarise(n_incendios = n(), .groups = "drop")
    
    plot_ly(
      detect_trend,
      x = ~anio,
      y = ~n_incendios,
      color = ~iddetectadopor,
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(
        xaxis = list(title = "Año"),
        yaxis = list(title = "Número de incendios"),
        legend = list(title = list(text = "Equipo detector"))
      )
  })
}

# ============================================================
# RUN APP
# ============================================================

shinyApp(ui = ui, server = server)
