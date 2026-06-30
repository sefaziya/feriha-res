# RES Monitor — Shiny dashboard

status_badge <- function(status) {
  colors <- c(
    complete = "#198754",
    running = "#0d6efd",
    pending = "#6c757d",
    queued = "#adb5bd"
  )
  labels <- c(
    complete = "Tamamlandi",
    running = "Calisiyor",
    pending = "Bekliyor",
    queued = "Sirada"
  )
  bg <- colors[[status]] %||% "#6c757d"
  lb <- labels[[status]] %||% status
  sprintf(
    '<span style="background:%s;color:#fff;padding:2px 8px;border-radius:4px;font-size:12px;">%s</span>',
    bg, lb
  )
}

fields_to_df <- function(fields) {
  if (length(fields) == 0) {
    return(data.frame(
      Alan = character(),
      Durum = character(),
      Sonraki = character(),
      Ilerleme = numeric(),
      N = numeric(),
      R2 = numeric(),
      Stress = numeric(),
      Sure = character(),
      Guncelleme = character(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    Alan = vapply(fields, function(x) x$field, character(1)),
    Durum = vapply(fields, function(x) status_badge(x$status), character(1)),
    Sonraki = vapply(fields, function(x) {
      step <- x$next_step
      if (is.null(step) || identical(step, "—")) return("—")
      PIPELINE_STEP_LABELS[[step]] %||% step
    }, character(1)),
    Ilerleme = vapply(fields, function(x) x$progress_pct, numeric(1)),
    N = vapply(fields, function(x) as.character(x$N), character(1)),
    `R²` = vapply(fields, function(x) {
      if (is.na(x$PERMANOVA_R2)) "—" else sprintf("%.4f", x$PERMANOVA_R2)
    }, character(1)),
    Stress = vapply(fields, function(x) {
      if (is.na(x$Stress)) "—" else sprintf("%.4f", x$Stress)
    }, character(1)),
    Sure = vapply(fields, function(x) x$Runtime, character(1)),
    Guncelleme = vapply(fields, function(x) x$last_update, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

run_monitor_app <- function(proj_root, monitor_cfg = NULL, config = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Install with renv::install('shiny').")
  }

  options(res.project_root = proj_root)
  source(file.path(proj_root, "03_Modules", "io.R"), local = FALSE)
  source(file.path(proj_root, "03_Modules", "discovery.R"), local = FALSE)
  source(file.path(proj_root, "03_Modules", "checkpoint.R"), local = FALSE)
  source(file.path(proj_root, "07_Monitor", "status.R"), local = FALSE)

  if (is.null(config)) config <- load_config()
  if (is.null(monitor_cfg)) monitor_cfg <- load_monitor_config()

  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        body { font-family: system-ui, sans-serif; }
        .metric-card { background:#f8f9fa; border:1px solid #dee2e6; border-radius:8px; padding:12px 16px; margin-bottom:8px; }
        .metric-title { font-size:12px; color:#6c757d; text-transform:uppercase; }
        .metric-value { font-size:20px; font-weight:600; }
        .on { color:#198754; } .off { color:#dc3545; }
        #log_box { font-family: ui-monospace, monospace; font-size:12px; white-space:pre-wrap; background:#1e1e1e; color:#d4d4d4; padding:12px; border-radius:6px; max-height:420px; overflow-y:auto; }
        .progress-wrap { background:#e9ecef; border-radius:4px; height:8px; }
        .progress-bar { background:#0d6efd; height:8px; border-radius:4px; }
      "))
    ),
    shiny::titlePanel("RES — Research Execution System Monitor"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("run_date", "Calistirma tarihi", choices = character()),
        shiny::sliderInput("refresh_sec", "Yenileme (sn)", min = 3, max = 60, value = monitor_cfg$refresh_seconds %||% 5, step = 1),
        shiny::actionButton("refresh_now", "Simdi yenile", class = "btn-primary"),
        shiny::hr(),
        shiny::h4("Kontrol"),
        shiny::actionButton("start_engine", "Engine baslat (tmux)", class = "btn-success"),
        shiny::helpText("Mevcut checkpoint'ten devam eder. RES oturumu varsa baslatmaz."),
        shiny::hr(),
        shiny::h5("Baglanti"),
        shiny::verbatimTextOutput("access_hint")
      ),
      shiny::mainPanel(
        shiny::fluidRow(
          shiny::column(3, shiny::div(class = "metric-card", shiny::div(class = "metric-title", "Engine"), shiny::uiOutput("engine_status"))),
          shiny::column(3, shiny::div(class = "metric-card", shiny::div(class = "metric-title", "tmux RES"), shiny::uiOutput("tmux_status"))),
          shiny::column(3, shiny::div(class = "metric-card", shiny::div(class = "metric-title", "Ilerleme"), shiny::uiOutput("overall_progress"))),
          shiny::column(3, shiny::div(class = "metric-card", shiny::div(class = "metric-title", "Veri dosyalari"), shiny::uiOutput("data_count")))
        ),
        shiny::fluidRow(
          shiny::column(4, shiny::div(class = "metric-card", shiny::div(class = "metric-title", "Aktif alan"), shiny::textOutput("current_field", inline = TRUE))),
          shiny::column(4, shiny::div(class = "metric-card", shiny::div(class = "metric-title", "Aktif adim"), shiny::textOutput("current_step", inline = TRUE))),
          shiny::column(4, shiny::div(class = "metric-card", shiny::div(class = "metric-title", "Process"), shiny::textOutput("process_info", inline = TRUE)))
        ),
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Alanlar",
            shiny::br(),
            shiny::uiOutput("fields_table"),
            shiny::helpText(shiny::textOutput("last_update", inline = TRUE))
          ),
          shiny::tabPanel(
            "Log",
            shiny::br(),
            shiny::div(id = "log_box", shiny::textOutput("log_tail"))
          ),
          shiny::tabPanel(
            "Sistem",
            shiny::br(),
            shiny::verbatimTextOutput("system_info")
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    auth_ok <- shiny::reactiveVal(!isTRUE(monitor_cfg$auth$enabled))

  if (isTRUE(monitor_cfg$auth$enabled)) {
    shiny::showModal(shiny::modalDialog(
      title = "RES Monitor — Giris",
      shiny::textInput("auth_user", "Kullanici adi", placeholder = monitor_cfg$auth$username %||% "res"),
      shiny::passwordInput("auth_pw", "Sifre"),
      footer = shiny::tagList(
        shiny::actionButton("auth_btn", "Giris", class = "btn-primary"),
        shiny::tags$script(shiny::HTML("
          $(document).on('keydown', '#auth_pw, #auth_user', function(e) {
            if (e.key === 'Enter') { e.preventDefault(); $('#auth_btn').click(); }
          });
        "))
      ),
      easyClose = FALSE
    ))
    shiny::observeEvent(input$auth_btn, {
      user_in <- trimws(input$auth_user %||% "")
      pass_in <- input$auth_pw %||% ""
      user_exp <- trimws(monitor_cfg$auth$username %||% "")
      pass_exp <- as.character(monitor_cfg$auth$password %||% "")
      user_ok <- identical(tolower(user_in), tolower(user_exp))
      pass_ok <- identical(pass_in, pass_exp)
      if (user_ok && pass_ok) {
        auth_ok(TRUE)
        shiny::removeModal()
      } else {
        shiny::showNotification("Hatali kullanici adi veya sifre", type = "error")
      }
    }, ignoreInit = TRUE)
  }

    shiny::observe({
      shiny::req(auth_ok())
      dates <- list_run_dates(config)
      if (length(dates) == 0) dates <- format(Sys.Date(), "%Y%m%d")
      shiny::updateSelectInput(session, "run_date", choices = dates, selected = dates[[1]])
    })

    status_data <- shiny::reactive({
      shiny::req(auth_ok(), input$run_date)
      collect_dashboard_status(config, input$run_date, monitor_cfg)
    })

    shiny::observe({
      shiny::req(auth_ok())
      shiny::invalidateLater(max(3, input$refresh_sec) * 1000)
      status_data()
    })

    shiny::observeEvent(input$refresh_now, { status_data() })

    output$engine_status <- shiny::renderUI({
      s <- status_data()
      cls <- if (s$engine_running) "on" else "off"
      lbl <- if (s$engine_running) "Calisiyor" else "Durdu"
      shiny::tagList(
        shiny::tags$div(class = paste("metric-value", cls), lbl),
        shiny::tags$div(style = "font-size:11px;color:#6c757d;", paste("Kaynak:", s$activity_source))
      )
    })

    output$tmux_status <- shiny::renderUI({
      s <- status_data()
      cls <- if (s$tmux_res) "on" else "off"
      lbl <- if (s$tmux_res) "Aktif" else "Yok"
      shiny::tags$div(class = paste("metric-value", cls), lbl)
    })

    output$overall_progress <- shiny::renderUI({
      s <- status_data()
      pct <- if (s$fields_total > 0) round(100 * s$fields_complete / s$fields_total) else 0
      shiny::tagList(
        shiny::div(class = "metric-value", sprintf("%d / %d", s$fields_complete, s$fields_total)),
        shiny::div(class = "progress-wrap", shiny::div(class = "progress-bar", style = sprintf("width:%d%%", pct)))
      )
    })

    output$data_count <- shiny::renderUI({
      shiny::div(class = "metric-value", status_data()$data_files)
    })

    output$current_field <- shiny::renderText({ status_data()$current_field })
    output$current_step <- shiny::renderText({
      step <- status_data()$current_step
      PIPELINE_STEP_LABELS[[step]] %||% step
    })

    output$process_info <- shiny::renderText({
      s <- status_data()
      if (!s$engine_running) return("—")
      sprintf("PID %s | CPU %s%% | RAM %s%% | %s", s$engine_pid, s$engine_cpu, s$engine_mem, s$engine_elapsed)
    })

    output$last_update <- shiny::renderText({
      s <- status_data()
      extra <- if (nzchar(s$live_updated) && !identical(s$live_updated, "—")) {
        paste("| live:", s$live_updated)
      } else ""
      paste("Son guncelleme:", s$timestamp, extra)
    })

    output$fields_table <- shiny::renderUI({
      fields <- status_data()$fields
      df <- fields_to_df(fields)
      if (nrow(df) == 0) {
        return(shiny::p("Henuz alan verisi yok. Engine baslatildiginda manifest olusur."))
      }
      html <- "<table class='table table-sm table-striped'><thead><tr>"
      for (nm in names(df)) html <- paste0(html, "<th>", nm, "</th>")
      html <- paste0(html, "</tr></thead><tbody>")
      for (i in seq_len(nrow(df))) {
        row_style <- if (isTRUE(fields[[i]]$is_active)) " style='background:#e7f1ff;'" else ""
        html <- paste0(html, "<tr", row_style, ">")
        for (j in seq_len(ncol(df))) {
          val <- df[i, j]
          if (names(df)[j] == "Ilerleme") {
            val <- sprintf(
              '<div class="progress-wrap"><div class="progress-bar" style="width:%s%%"></div></div> %s%%',
              val, val
            )
          }
          html <- paste0(html, "<td>", val, "</td>")
        }
        html <- paste0(html, "</tr>")
      }
      html <- paste0(html, "</tbody></table>")
      shiny::HTML(html)
    })

    output$log_tail <- shiny::renderText({
      lines <- tail_live_log(config, monitor_cfg, 120)
      if (length(lines) == 1) lines else paste(lines, collapse = "\n")
    })

    output$system_info <- shiny::renderText({
      s <- status_data()
      paste(
        sprintf("Run ID: %s", s$run_id),
        sprintf("Run date: %s", s$run_date),
        sprintf("Engine PID: %s", s$engine_pid),
        sprintf("Durum kaynagi: %s", s$activity_source),
        sprintf("tmux oturumlari: %s", paste(s$tmux_sessions, collapse = ", ")),
        sprintf("Proje: %s", proj_root),
        sep = "\n"
      )
    })

    output$access_hint <- shiny::renderText({
      host <- monitor_cfg$host %||% "127.0.0.1"
      port <- monitor_cfg$port %||% 8788
      auth_on <- isTRUE(monitor_cfg$auth$enabled)
      if (host == "0.0.0.0") {
        sprintf(
          "Mobil/uzak: http://SUNUCU_IP:%d\nKullanici: %s\nAuth: %s",
          port,
          monitor_cfg$auth$username %||% "res",
          if (auth_on) "acik" else "kapali (acmayin!)"
        )
      } else {
        sprintf(
          "http://127.0.0.1:%d\nSSH tuneli:\nssh -L %d:127.0.0.1:%d root@SUNUCU",
          port, port, port
        )
      }
    })

    shiny::observeEvent(input$start_engine, {
      res <- start_engine_tmux(monitor_cfg)
      shiny::showNotification(res$message, type = if (res$ok) "message" else "warning")
    })
  }

  shiny::shinyApp(ui, server)
}
