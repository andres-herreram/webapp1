# =============================================================================
# App Shiny: Cálculo de Valor en Riesgo (VaR) para Cartera de Préstamos
# Autor: Gemini IA para Cooperativa Horizonte
# Fecha: 2025-08-13
# Versión: 15.0 (Final - Incluye Pestaña de Análisis Integral y Estrategias)
# =============================================================================


# 1. VERIFICACIÓN E INSTALACIÓN AUTOMÁTICA DE LIBRERÍAS
# -----------------------------------------------------------------------------
required_packages <- c("shiny", "bslib", "readxl", "dplyr", "ggplot2", "plotly", "scales", "shinyjs", "DT", "shinycssloaders")

not_installed <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(not_installed) > 0) {
  install.packages(not_installed)
}

lapply(required_packages, library, character.only = TRUE)
# -----------------------------------------------------------------------------


# --- Función para crear tarjetas de KPI ---
kpi_card <- function(value, subtitle, icon_name, bg_color, text_color = "white") {
  div(class = "col-md-4 mb-3",
      div(
        class = "card h-100",
        style = paste0("background-color: ", bg_color, "; color: ", text_color, "; border-radius: .375rem;"),
        div(class = "card-body",
            style = "display: flex; justify-content: space-between; align-items: center; padding: 1.25rem;",
            div(
              style = "text-align: left;",
              h2(style = "font-weight: bold; margin: 0;", value),
              p(style = "margin: 0; opacity: 0.9;", subtitle)
            ),
            div(
              style = "font-size: 3em; opacity: 0.5;",
              icon(icon_name)
            )
        )
      )
  )
}


# -----------------------------------------------------------------------------
# INTERFAZ DE USUARIO (UI)
# -----------------------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(), 
  theme = bs_theme(version = 5, bootswatch = "litera"),
  tags$head(
    tags$style(HTML("
      .compact-input .form-group { margin-bottom: 5px !important; }
      .compact-input .form-control { height: 30px; }
      .pd-lgd-header { margin-bottom: 10px; }
      .card-body-text { font-size: 0.9rem; line-height: 1.6; }
      .tab-content { padding-top: 20px; }
    "))
  ),
  titlePanel(
    windowTitle = "VaR - Cooperativa Horizonte",
    title = div(
      img(src = "https://i.imgur.com/yH98725.png", height = "50px", style = "margin-right: 20px;"),
      "Análisis de Valor en Riesgo (VaR) para Cartera de Préstamos"
    )
  ),
  
  navbarPage(
    "Cooperativa Horizonte",
    
    # --- PESTAÑA 1: Carga y Parámetros ---
    tabPanel("1. Carga y Parámetros",
             icon = icon("upload"),
             sidebarLayout(
               sidebarPanel(
                 h4("1. Cargar Cartera de Créditos"),
                 fileInput("file1", "Subir archivo Excel (.xlsx/.xls)",
                           accept = c(".xlsx", ".xls")),
                 hr(),
                 h4("2. Parámetros de Riesgo (PD & LGD)"),
                 p(class="text-muted", "Ajuste los % según la experiencia."),
                 
                 div(class = "well", style = "background-color: #f8f9fa;",
                     div(class = "pd-lgd-header",
                         fluidRow(
                           column(5, strong("Estado")),
                           column(3, strong("PD%")),
                           column(4, strong("LGD%"))
                         )
                     ),
                     div(class = "compact-input",
                         fluidRow(
                           column(5, p("Al día", style="margin-top:5px;")),
                           column(3, numericInput("pd_aldia", NULL, value = 0.5)),
                           column(4, numericInput("lgd_aldia", NULL, value = 40))
                         ),
                         fluidRow(
                           column(5, p("30 días", style="margin-top:5px;")),
                           column(3, numericInput("pd_30", NULL, value = 5)),
                           column(4, numericInput("lgd_30", NULL, value = 45))
                         ),
                         fluidRow(
                           column(5, p("60 días", style="margin-top:5px;")),
                           column(3, numericInput("pd_60", NULL, value = 15)),
                           column(4, numericInput("lgd_60", NULL, value = 50))
                         ),
                         fluidRow(
                           column(5, p("90 días", style="margin-top:5px;")),
                           column(3, numericInput("pd_90", NULL, value = 40)),
                           column(4, numericInput("lgd_90", NULL, value = 60))
                         ),
                         fluidRow(
                           column(5, p("Cobro Judicial", style="margin-top:5px;")),
                           column(3, numericInput("pd_judicial", NULL, value = 90)),
                           column(4, numericInput("lgd_judicial", NULL, value = 80))
                         )
                     )
                 )
               ),
               mainPanel(
                 div(class = "card",
                     div(class = "card-header", h5("Resumen General de la Cartera")),
                     div(class = "card-body", uiOutput("kpi_summary_ui"))
                 ),
                 
                 div(class = "card", style = "margin-top: 20px;",
                     div(class = "card-header", h5("Detalle de la Cartera Cargada")),
                     div(class = "card-body", DTOutput("contents"))
                 ),
                 
                 div(class = "card", style = "margin-top: 20px;",
                     div(class = "card-header", h5("Composición de la Cartera por Estado")),
                     div(class = "card-body", plotlyOutput("portfolio_composition_plot"))
                 )
               )
             )
    ),
    
    # --- PESTAÑA 2: Simulación VaR Monte Carlo ---
    tabPanel("2. Simulación VaR Monte Carlo",
             icon = icon("gears"),
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Configurar Simulación"),
                 p(class="text-muted", "Ajuste los parámetros y ejecute."),
                 hr(),
                 sliderInput("confidence_level", "Nivel de Confianza del VaR:",
                             min = 90, max = 99.9, value = 99, step = 0.1, post = "%"),
                 numericInput("simulations", "Número de Simulaciones:",
                              value = 10000, min = 1000, max = 100000, step = 1000),
                 hr(),
                 div(style = "text-align: center;",
                     actionButton("run_monte_carlo", "Ejecutar Simulación", icon = icon("play-circle"), class = "btn-success btn-lg")
                 )
               ),
               mainPanel(
                 width = 9,
                 uiOutput("monte_carlo_results_ui") %>% withSpinner(type = 6, color = "#28a745")
               )
             )
    ),
    
    # --- PESTAÑA 3: Análisis Integral y Estrategias ---
    tabPanel("3. Análisis y Estrategias",
             icon = icon("lightbulb"),
             fluidRow(
               column(12,
                      div(class = "card",
                          div(class = "card-header", h5("Análisis Comparativo del Valor en Riesgo (VaR)")),
                          div(class = "card-body", 
                              p("Esta tabla resume las pérdidas potenciales de la cartera bajo dos niveles de confianza estándar. El 95% representa un escenario 'malo' pero probable, mientras que el 99% representa un escenario 'severo' y menos frecuente."),
                              tableOutput("summary_table")
                          )
                      ),
                      br(),
                      div(class = "card",
                          div(class = "card-header", h5("Estrategias Recomendadas para la Mitigación de Riesgo")),
                          div(class = "card-body card-body-text",
                              h6(strong("1. Diversificación de la Cartera")),
                              p("La concentración excesiva del riesgo en un solo sector económico (ej. turismo, agricultura) o en una zona geográfica específica es peligrosa. Una estrategia clave es establecer límites de exposición por sector y región, y buscar activamente la colocación de créditos en nuevos mercados o industrias. Esto asegura que una crisis en un área no afecte desproporcionadamente a toda la cooperativa."),
                              hr(),
                              h6(strong("2. Fortalecimiento de las Políticas de Crédito y Cobranza")),
                              p("Reducir el riesgo empieza con una buena originación del crédito. Se recomienda revisar y fortalecer los modelos de 'scoring' para evaluar la capacidad de pago de los solicitantes de forma más precisa. Adicionalmente, implementar un proceso de cobranza proactivo y escalonado (ej. recordatorios automáticos, llamadas personalizadas) puede reducir significativamente la probabilidad de que un atraso se convierta en un impago, disminuyendo la Pérdida Esperada (EL)."),
                              hr(),
                              h6(strong("3. Constitución de Provisiones y Reservas de Capital")),
                              p("El análisis VaR no solo identifica el riesgo, sino que cuantifica la cantidad de capital que se necesita para soportarlo. La cooperativa debe utilizar los resultados del VaR (especialmente el del 99%) como una guía para establecer provisiones voluntarias o reservas de capital. Este 'colchón' financiero asegura la solvencia y estabilidad de la entidad ante un mes inesperadamente malo, protegiendo así el capital de todos los socios.")
                          )
                      )
               )
             )
    )
  )
)


# -----------------------------------------------------------------------------
# LÓGICA DEL SERVIDOR (SERVER)
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  datos_cargados <- reactive({
    req(input$file1)
    tryCatch({
      df <- read_excel(input$file1$datapath)
      expected_cols <- c("Saldo actual (CRC)", "Estado de morosidad")
      if (!all(expected_cols %in% names(df))) {
        stop("El archivo no contiene las columnas requeridas: 'Saldo actual (CRC)' y 'Estado de morosidad'.")
      }
      df %>% rename(ead = `Saldo actual (CRC)`, estado = `Estado de morosidad`)
    }, error = function(e) {
      showNotification(paste("Error al leer el archivo:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$kpi_summary_ui <- renderUI({
    df <- datos_cargados()
    req(df)
    
    total_ead <- sum(df$ead, na.rm = TRUE)
    num_creditos <- nrow(df)
    ead_al_dia <- sum(df$ead[df$estado == "Al día"], na.rm = TRUE)
    pct_al_dia <- if(total_ead > 0) ead_al_dia / total_ead else 0
    
    fluidRow(
      kpi_card(
        value = number(total_ead, prefix = "₡", big.mark = ",", accuracy = 1),
        subtitle = "Monto Total de Cartera",
        icon_name = "wallet",
        bg_color = "#4e73df"
      ),
      kpi_card(
        value = number(num_creditos, big.mark = ","),
        subtitle = "Número Total de Créditos",
        icon_name = "file-contract",
        bg_color = "#36b9cc"
      ),
      kpi_card(
        value = percent(pct_al_dia, accuracy = 0.1),
        subtitle = "% Cartera 'Al día'",
        icon_name = "chart-pie",
        bg_color = "#1cc88a"
      )
    )
  })
  
  output$contents <- renderDT({
    df_original <- datos_cargados()
    req(df_original)
    
    df_display <- df_original %>%
      rename(EAD = ead, Estado_orig = estado) %>%
      mutate(
        Estado = case_when(
          Estado_orig == "Al día" ~ paste0('<span style="display:none;">1</span><i class="fa fa-check-circle" style="color:#28a745;"></i> ', Estado_orig),
          Estado_orig == "30 días" ~ paste0('<span style="display:none;">2</span><i class="fa fa-exclamation-triangle" style="color:#ffc107;"></i> ', Estado_orig),
          Estado_orig == "60 días" ~ paste0('<span style="display:none;">3</span><i class="fa fa-exclamation-triangle" style="color:#fd7e14;"></i> ', Estado_orig),
          Estado_orig == "90 días" ~ paste0('<span style="display:none;">4</span><i class="fa fa-times-circle" style="color:#dc3545;"></i> ', Estado_orig),
          Estado_orig == "Cobro judicial" ~ paste0('<span style="display:none;">5</span><i class="fa fa-gavel" style="color:#6f42c1;"></i> ', Estado_orig),
          TRUE ~ Estado_orig
        )
      ) %>%
      select(-Estado_orig)
    
    cols_to_format_currency <- c()
    if ("Monto del préstamo (CRC)" %in% names(df_display)) cols_to_format_currency <- c(cols_to_format_currency, "Monto del préstamo (CRC)")
    if ("Cuota" %in% names(df_display)) cols_to_format_currency <- c(cols_to_format_currency, "Cuota")
    if ("EAD" %in% names(df_display)) cols_to_format_currency <- c(cols_to_format_currency, "EAD")
    
    col_names <- names(df_display)
    
    datatable(df_display,
              class = 'compact stripe hover', rownames = FALSE, escape = FALSE,
              options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE,
                             columnDefs = list(
                               list(className = 'dt-right', targets = which(col_names %in% c("Monto del préstamo (CRC)", "Tasa de interés (%)", "Cuota", "EAD")) - 1),
                               list(className = 'dt-center', targets = which(col_names %in% c("Plazo", "Estado")) - 1)
                             ))) %>%
      formatCurrency(columns = cols_to_format_currency, currency = "₡", digits = 2)
  })
  
  output$portfolio_composition_plot <- renderPlotly({
    df <- datos_cargados()
    req(df)
    
    color_palette <- c("Al día" = "#28a745", "30 días" = "#ffc107", "60 días" = "#fd7e14", "90 días" = "#dc3545", "Cobro judicial" = "#6f42c1")
    format_labels <- function(x) {
      case_when(x >= 1e9 ~ paste0("₡", round(x/1e9,1),"B"), x >= 1e6 ~ paste0("₡", round(x/1e6,1),"M"), x >= 1e3 ~ paste0("₡", round(x/1e3,1),"K"), TRUE~paste0("₡",x))
    }
    
    plot_data <- df %>% group_by(estado) %>% summarise(total_ead = sum(ead, na.rm=TRUE), .groups='drop') %>% mutate(total_portfolio=sum(total_ead), percentage=total_ead/total_portfolio)
    
    p <- ggplot(plot_data, aes(x=reorder(estado, -total_ead), y=total_ead, fill=estado, text=paste0("<b>Estado:</b> ", estado, "<br><b>Saldo Total:</b> ", number(total_ead, prefix="₡", big.mark=","), "<br><b>Porcentaje de Cartera:</b> ", percent(percentage, accuracy=0.1)))) +
      geom_col(alpha = 0.9) +
      geom_text(aes(label = format_labels(total_ead)), vjust = -0.5, size = 3.5, fontface = "bold", color = "#343a40") +
      scale_fill_manual(values = color_palette) +
      scale_y_continuous(labels = format_labels, expand = expansion(mult = c(0, .15))) +
      labs(x=NULL, y=NULL) +
      theme_minimal() +
      theme(legend.position="none", panel.grid.major.x=element_blank(), panel.grid.minor=element_blank(), axis.text.x=element_text(face="bold"))
    
    ggplotly(p, tooltip = "text") %>% config(displaylogo = FALSE, modeBarButtonsToRemove = c('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'autoScale2d'))
  })
  
  sim_results <- reactiveVal(NULL)
  
  observeEvent(input$run_monte_carlo, {
    req(datos_cargados())
    sim_results(NULL) 
    
    df <- datos_cargados()
    
    pd_map <- c("Al día" = input$pd_aldia / 100, "30 días" = input$pd_30 / 100, "60 días" = input$pd_60 / 100, "90 días" = input$pd_90 / 100, "Cobro judicial" = input$pd_judicial / 100)
    lgd_map <- c("Al día" = input$lgd_aldia / 100, "30 días" = input$lgd_30 / 100, "60 días" = input$lgd_60 / 100, "90 días" = input$lgd_90 / 100, "Cobro judicial" = input$lgd_judicial / 100)
    
    portfolio <- df %>% mutate(pd = pd_map[estado], lgd = lgd_map[estado], el_loan = ead * pd * lgd)
    total_el <- sum(portfolio$el_loan, na.rm = TRUE)
    
    portfolio <- portfolio %>% mutate(variance_loan = (ead^2) * (lgd^2) * pd * (1 - pd))
    portfolio_stdev <- sqrt(sum(portfolio$variance_loan, na.rm = TRUE))
    
    # Cálculos para VaR Paramétrico a 95% y 99%
    var_parametric_95 <- total_el + qnorm(0.95) * portfolio_stdev
    var_parametric_99 <- total_el + qnorm(0.99) * portfolio_stdev
    
    n_loans <- nrow(portfolio)
    n_sims <- input$simulations
    random_matrix <- matrix(runif(n_loans * n_sims), nrow = n_sims, ncol = n_loans)
    default_matrix <- t(ifelse(random_matrix < portfolio$pd, 1, 0))
    loss_if_default <- portfolio$ead * portfolio$lgd
    simulated_losses <- colSums(default_matrix * loss_if_default, na.rm = TRUE)
    
    # Cálculos para VaR Monte Carlo a 95% y 99%
    var_monte_carlo_95 <- quantile(simulated_losses, probs = 0.95)
    var_monte_carlo_99 <- quantile(simulated_losses, probs = 0.99)
    
    results_list <- list(
      total_ead = sum(portfolio$ead, na.rm = TRUE), total_el = total_el, 
      var_parametric_95 = var_parametric_95, var_parametric_99 = var_parametric_99, 
      var_monte_carlo_95 = var_monte_carlo_95, var_monte_carlo_99 = var_monte_carlo_99,
      simulated_losses = simulated_losses
    )
    
    sim_results(results_list)
  })
  
  output$monte_carlo_results_ui <- renderUI({
    res <- sim_results()
    
    if (is.null(res)) {
      return(div(class = "alert alert-info", style="margin-top:20px; text-align:center;", 
                 h4("Listo para el Análisis"),
                 p("Ajuste los parámetros y haga clic en 'Ejecutar Simulación' para ver los resultados.")
      ))
    }
    
    confidence <- isolate(input$confidence_level)
    # Seleccionar el VaR correcto según el input del usuario para la visualización principal
    var_mc_display <- if(confidence == 99) res$var_monte_carlo_99 else quantile(res$simulated_losses, probs = confidence/100)
    var_p_display <- if(confidence == 99) res$var_parametric_99 else res$total_el + qnorm(confidence/100) * sqrt(sum((res$total_ead*res$lgd)^2*res$pd*(1-res$pd),na.rm = TRUE))
    
    tagList(
      div(class = "card",
          div(class = "card-header", h5("Resultados Clave de la Simulación")),
          div(class = "card-body",
              fluidRow(
                kpi_card(number(res$total_el, prefix="₡", big.mark=",", accuracy=1), "Pérdida Esperada (EL)", "calculator", "#f6c23e"),
                kpi_card(number(var_mc_display, prefix="₡", big.mark=",", accuracy=1), paste0("VaR Monte Carlo (", confidence, "%)"), "shield-alt", "#e74a3b"),
                kpi_card(number(var_p_display, prefix="₡", big.mark=",", accuracy=1), "VaR Paramétrico (Comp.)", "chart-line", "#6c757d")
              )
          )
      ),
      br(),
      fluidRow(
        column(8,
               div(class="card h-100",
                   tabsetPanel(
                     id = "plot_tabs",
                     tabPanel("Histograma de Frecuencia", plotlyOutput("monte_carlo_plot")),
                     tabPanel("Distribución Acumulada (CDF)", plotlyOutput("monte_carlo_cdf_plot"))
                   )
               )
        ),
        column(4,
               div(class="card h-100",
                   div(class="card-header", h5("Interpretación de Resultados")),
                   div(class="card-body card-body-text",
                       p(strong("Pérdida Esperada (EL):"), " Es la pérdida promedio que la cooperativa puede esperar en el próximo mes. Para esta cartera es de ", strong(number(res$total_el, prefix="₡", big.mark=",", accuracy=1)), "."),
                       hr(),
                       p(strong("VaR Monte Carlo:"), " Representa la pérdida máxima potencial en un escenario adverso. El VaR de Monte Carlo es:", strong(number(var_mc_display, prefix="₡", big.mark=",", accuracy=1)), "."),
                       p("Esto significa que tenemos un ", strong(paste0(confidence, "%")), " de confianza en que las pérdidas del próximo mes ", strong("no superarán"), " esa cifra."),
                       hr(),
                       p(strong("VaR Paramétrico:"), " Este método usa una fórmula matemática que asume que las pérdidas siguen una distribución Normal. Sirve como un buen punto de referencia, aunque generalmente es menos preciso que Monte Carlo para capturar escenarios extremos.")
                   )
               )
        )
      )
    )
  })
  
  output$monte_carlo_plot <- renderPlotly({
    res <- sim_results()
    req(res)
    var_mc_display <- quantile(res$simulated_losses, probs = isolate(input$confidence_level)/100)
    
    p <- ggplot(data.frame(losses = res$simulated_losses), aes(x = losses)) + 
      geom_histogram(aes(y = ..density..), bins=50, fill="#0d6efd", alpha=0.7) + 
      geom_density(col="red", size=1) + 
      geom_vline(xintercept=var_mc_display, color="red", linetype="dashed", size=1.2) +
      annotate("text", x=var_mc_display, y=0, 
               label=paste("VaR:", number(var_mc_display, prefix="₡", big.mark=",", accuracy=1)), 
               hjust = -0.1, vjust = -1, color = "#343a40", fontface = "bold") + 
      labs(title=NULL, x="Pérdida Total Simulada (CRC)", y="Densidad") + 
      scale_x_continuous(labels=number_format(prefix="₡", big.mark=",")) + theme_minimal()
    ggplotly(p)
  })
  
  output$monte_carlo_cdf_plot <- renderPlotly({
    res <- sim_results()
    req(res)
    confidence_dec <- isolate(input$confidence_level) / 100
    var_mc_display <- quantile(res$simulated_losses, probs = confidence_dec)
    
    p <- ggplot(data.frame(losses = res$simulated_losses), aes(x = losses)) +
      stat_ecdf(geom = "step", color = "#0d6efd", size = 1.2) +
      geom_segment(aes(x = -Inf, xend = var_mc_display, y = confidence_dec, yend = confidence_dec), color = "red", linetype = "dashed") +
      geom_segment(aes(x = var_mc_display, xend = var_mc_display, y = -Inf, yend = confidence_dec), color = "red", linetype = "dashed") +
      annotate("text", x = var_mc_display, y = confidence_dec,
               label = paste0(isolate(input$confidence_level), "% VaR:\n", number(var_mc_display, prefix="₡", big.mark=",", accuracy=1)),
               hjust = 1.1, vjust = -0.5, color = "#343a40", fontface = "bold") +
      labs(title=NULL, x="Pérdida Total Simulada (CRC)", y="Probabilidad Acumulada") +
      scale_x_continuous(labels = number_format(prefix = "₡", big.mark = ",")) +
      scale_y_continuous(labels = percent_format()) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # --- NUEVA TABLA para la pestaña de Análisis Integral ---
  output$summary_table <- renderTable({
    res <- sim_results()
    req(res) # Requiere que la simulación se haya corrido al menos una vez
    
    summary_df <- data.frame(
      Métrica = c("Pérdida Esperada (EL)", "VaR Monte Carlo", "VaR Paramétrico"),
      `Nivel de Confianza 95%` = c(
        number(res$total_el, prefix="₡", big.mark=",", accuracy=1),
        number(res$var_monte_carlo_95, prefix="₡", big.mark=",", accuracy=1),
        number(res$var_parametric_95, prefix="₡", big.mark=",", accuracy=1)
      ),
      `Nivel de Confianza 99%` = c(
        number(res$total_el, prefix="₡", big.mark=",", accuracy=1),
        number(res$var_monte_carlo_99, prefix="₡", big.mark=",", accuracy=1),
        number(res$var_parametric_99, prefix="₡", big.mark=",", accuracy=1)
      )
    )
    # El check.names=FALSE es para que los nombres de columna con % y espacios se muestren bien
    summary_df
  }, check.names = FALSE)
  
}


# -----------------------------------------------------------------------------
# EJECUTAR LA APLICACIÓN
# -----------------------------------------------------------------------------
shinyApp(ui = ui, server = server)