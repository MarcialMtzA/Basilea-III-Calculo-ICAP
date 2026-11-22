setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("scripts/funciones.R")
source("scripts/objetos.R")

#######################################
# UI
#######################################
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Proyecto para seminario de finanzas, calculo de capital neto y de activos ponderados a riesgo de credito por Marcial Mtz A :)",
      collapsible = TRUE,
      
      radioButtons(
        inputId = "tab_activa",
        label   = "Seleccione el calculo que desea realizar :",
        choices = c(
          "Capital Neto"      = "tab_capital",
          "Riesgo de Crédito" = "tab_riesgo"
        ),
        selected = "tab_capital"
      ),
      hr(),
      
      uiOutput("sidebar_inputs")
    ),
    
    uiOutput("main_panel")
  )
)

#######################################
# SERVER
#######################################
server <- function(input, output, session) {
  
  ########################################################
  # df_1: archivo para Capital Neto
  ########################################################
  df_1 <- reactive({
    req(input$archivo_excel_df1)
    read_excel(
      path  = input$archivo_excel_df1$datapath,
      sheet = 1
    )
  })
  
  ########################################################
  # df_2: archivo para Riesgo de Crédito
  ########################################################
  df_2 <- reactive({
    req(input$archivo_excel_df2)
    read_excel(
      path  = input$archivo_excel_df2$datapath,
      sheet = 1
    )
  })
  
  ########################################################
  # SIDEBAR 
  ########################################################
  output$sidebar_inputs <- renderUI({
    if (input$tab_activa == "tab_capital") {
      tagList(
        numericInput("ASRT", "Ingrese los activos sujetos a riesgo totales", value = NA_real_, min = 0),
        numericInput("ASRC_metodo_interno", "Ingrese los activos sujetos a riesgo de crédito (método interno)", value = NA_real_, min = 0),
        numericInput("ASRC_metodo_estandar", "Ingrese los activos sujetos a riesgo de crédito (método estándar)", value = NA_real_, min = 0),
        
        hr(),
        
        fileInput("archivo_excel_df1", "Ingresa un archivo excel, debe contener una columna concepto y otra importe", accept = c(".xlsx", ".xls")),
        uiOutput("info_archivo_df1"),
        
        actionButton("btn_calcular_capital", "Calcular Capital Neto", class = "btn-warning w-100")
      )
    } else {
      tagList(
        fileInput("archivo_excel_df2", "Ingresa un archivo Excel con información para Riesgo de Crédito", accept = c(".xlsx", ".xls")),
        uiOutput("info_archivo_df2"),
        
        actionButton("btn_calcular_riesgo", "Calcular Riesgo de Crédito", class = "btn-warning w-100")
      )
    }
  })
  
  ########################################################
  # Info d archivos
  ########################################################
  output$info_archivo_df1 <- renderUI({
    req(input$archivo_excel_df1)
    tags$div(
      tags$small(
        paste0(
          "Archivo: ", input$archivo_excel_df1$name,
          " | Filas: ", nrow(df_1()),
          " | Columnas: ", ncol(df_1())
        )
      )
    )
  })
  
  output$info_archivo_df2 <- renderUI({
    req(input$archivo_excel_df2)
    tags$div(
      tags$small(
        paste0(
          "Archivo: ", input$archivo_excel_df2$name,
          " | Filas: ", nrow(df_2()),
          " | Columnas: ", ncol(df_2())
        )
      )
    )
  })
  
  ########################################################
  # Capital Neto
  ########################################################
  calculo_capital_neto <- eventReactive(input$btn_calcular_capital, {
    req(df_1())
    req(!is.na(input$ASRT))
    req(!is.na(input$ASRC_metodo_interno))
    req(!is.na(input$ASRC_metodo_estandar))
    
    Capital_Neto(
      df_1(),
      ASRT = input$ASRT,
      ASRC_metodo_interno = input$ASRC_metodo_interno,
      ASRC_metodo_estandar = input$ASRC_metodo_estandar
    )
  })
  
  output$plot_gf1 <- renderPlot({
    req(calculo_capital_neto())
    calculo_capital_neto()$gf1
  })
  
  output$plot_gf2 <- renderPlot({
    req(calculo_capital_neto())
    calculo_capital_neto()$gf2
  })
  
  output$tabla_x <- renderUI({
    req(calculo_capital_neto())
    tagList(
      HTML(as.character(calculo_capital_neto()$tabla_capital_neto)),
      tags$br(),
      tags$p(
        tags$strong("Se te ha generado un archivo llamado excel_capital_neto")
      ))
    #HTML(as.character(calculo_capital_neto()$tabla_capital_neto)) 
  })
  
  ########################################################
  # Riesgo de Crédito
  ########################################################
  calculo_riesgo_de_credito <- eventReactive(input$btn_calcular_riesgo, {
    req(df_2())
    Riesgo_d_Credito(df_2())
  })
  
  output$tabla_creditos_dispuestos <- renderUI({
    req(calculo_riesgo_de_credito())
    tagList(
      HTML(as.character(calculo_riesgo_de_credito()$tabla_creditos_dispuestos)),
      tags$br(),
      tags$p(
        tags$strong("Se te ha generado un archivo llamado excel_creditos_dispuesto")
      ))
    #HTML(as.character(calculo_riesgo_de_credito()$tabla_creditos_dispuestos))
  })
  
  output$tabla_lineas_d_credito <- renderUI({
    req(calculo_riesgo_de_credito())
    tagList(
      HTML(as.character(calculo_riesgo_de_credito()$tabla_lineas_d_credito)),
      tags$br(),
      tags$p(
        tags$strong("Se te ha generado un archivo llamado excel_lineas_d_credito")
      ))
    #HTML(as.character(calculo_riesgo_de_credito()$tabla_lineas_d_credito))
  })
  
  ########################################################
  # PANEL PRINCIPAL 
  ########################################################
  output$main_panel <- renderUI({
    if (input$tab_activa == "tab_capital") {
      tagList(
        fluidRow(
          column(6, card(card_header("Capital Basico y complementario"), plotOutput("plot_gf1", height = "350px"))),
          column(6, card(card_header("Capital de nivel 1, nivel 2 y nivel 3"), plotOutput("plot_gf2", height = "350px")))
        ),
        br(),
        fluidRow(
          column(12, card(card_header("Calculos del capital"), uiOutput("tabla_x")))
        )
      )
    } else {
      tagList(
        fluidRow(
          column(12, card(card_header("Créditos dispuestos"), uiOutput("tabla_creditos_dispuestos")))
        ),
        br(),
        fluidRow(
          column(12, card(card_header("Líneas de crédito (no dispuestas)"), uiOutput("tabla_lineas_d_credito")))
        )
      )
    }
  })
}

shinyApp(ui, server)