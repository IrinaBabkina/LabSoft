library(markdown)
library(shiny)
library(DT)
library(dplyr)
library(rhandsontable)
library(tidyr)

good <- data.frame(Status = c("��� ��. ����� ������� :)"), stringsAsFactors = F)

#UI----
ui <- navbarPage(
  "LabSoft",
  
  tabPanel(
    "Table",
    column(4,
    # Input: Select a file ----
    fileInput("file", "�������� ����")),
    column(4,
    # Input column in Table ----
    selectInput(inputId = "select",label =  "�������� ������� ��� �����������",choices =  "", multiple = TRUE)), 
    # Output in Table ----
    dataTableOutput("contents")), 
  
  tabPanel(
    "Balance",
    #input balance 
    column(
      2,
      #input balance column
      selectInput("category", "�������� �������", c(1, 2)),
      #input balance level
      selectInput("choice", "�������� �������", c(1, 2))
    ),
    #balance output what count
    column(4,
           offset = 1,
           textOutput("selected_var"),
           textOutput("count_row"),
           br(),
           #balance output what need----
           h3("� ����� ���� �� �������..."),
           rHandsontableOutput("need_buy")
    ),
    dataTableOutput("balance_table")
  ), 
  
  tabPanel(
    "Modified",
    
    titlePanel("��������������"),
    sidebarLayout(
      sidebarPanel(
        width = 2,
        #input usetype modified table----
        wellPanel(
          h4("�����"),
          radioButtons("useType", "��������������", c("TRUE", "FALSE"))
        ),
        br(),
        #input save modified----
        wellPanel(
          h4("��������� ���������"),
          actionButton("save", "���������")
        ),
        br(),
        #input add column modified----
        wellPanel(
          h4("�������� �������"),
          div(
            class = "row",
            div(
              class = "col-sm-8",
              uiOutput("ui_newcolname"),
              actionButton("addcolumn", "��������")
            ),
            div(class = "col-sm-3")
          )
        )
      ),
      
      mainPanel(
        width = 10,
        #output modified table ----
        rHandsontableOutput("hot"),
        #input add row modified----
        actionButton("addrow", "�������� ������")
        
      )
    )
    
  )
)


server <- function(input, output, session) {
  # global base_df----
  values <- reactiveValues(base_df = NULL)
  # variants for selectInput in Table menu----
  outVar = reactive({
    mydata = values$base_df
    names(mydata)
  })
  
  #variants for selectInput in Balance menu----
  problem = reactive({
    base_df = values$base_df
    
    problemList <- list(
    "���������" = setNames(as.list(levels(base_df$���������)), levels(base_df$���������)),
    "�������" = setNames(as.list(levels(base_df$�������)), levels(base_df$�������)),
    "������������" = setNames(as.list(levels(base_df$������������)), levels(base_df$������������))
  )
    problemList})
  
  
  # interactive selectInput in Table menu----
  observe({
    updateSelectInput(session, "select",
                      choices = outVar()
    )})
  
  # interactive selectInput category in Balance menu----
  observe({
    updateSelectInput(session, "category",
                      choices = names(problem())
    )})
  # interactive selectInput choice depend on category in Balance menu----
  observe({
    updateSelectInput(session, "choice", choices = problem()[[input$category]])
  })
  
  #input file from computer----
  observeEvent(input$file, {
    values$base_df <- read.csv(input$file$datapath)
  })
  
  ## Save modification----
  observeEvent(input$save, {
    base_df<-values$base_df
    showNotification("��������� ���������", type ="message" )
    finalDF <- isolate(values[["base_df"]])
    write.csv(x = finalDF, file = paste0("Base_versions/Base_Littorina_", format(Sys.Date()),".csv"), row.names = F)
  })
  
  #interactive effect of modification----
  observe({

    if (!is.null(input$hot)) {
      base_df <- hot_to_r(input$hot)
    } else {
      base_df <- values[["base_df"]]
    }
    values[["base_df"]] <- base_df
  })
  
  ## Add row modified
  observeEvent(input$addrow, {
    base_df <- values$base_df
    new_r <- data.frame(matrix(ncol = ncol(base_df), nrow = 1))
    colnames(new_r) <- names(base_df)
    values$base_df <- setNames(rbind(base_df, new_r), c(names(base_df)))
  })
  
  #output modified table
  output$hot <- renderRHandsontable({
    base_df <- values[["base_df"]]
    if (!is.null(base_df)) {
      rhandsontable(base_df, useTypes = as.logical(input$useType), stretchH = "all", height = 610)
    }
  })
  
  #output selected balance group
  output$selected_var <- renderText({
    paste("�� �������", input$category, input$choice)
  })
  
  #output count balance group
  output$count_row <- renderText({
    base_df<-values$base_df
    need <- input$choice
    from <- input$category
    sel_col <- base_df %>% select(from)
    count <- sum(sel_col == need)
    paste("��� ���� �������", count, "������� � ����")
  })
  
#output need buy Balance table
  output$need_buy <-
    renderRHandsontable({
      base_df<-values$base_df
      res <- base_df %>%
        separate(col = ��������, c("����_N", "����_�����"), convert = T) %>%
        separate(col = �������, c("���_N", "���_�����"), convert = T) %>%
        mutate_at(vars(contains("_N")), ~ replace(., is.na(.), 1))
      immetca <- res %>%
        filter(��������� == "�������") %>%
        group_by(������������, ���) %>%
        mutate(Count = ���_N * ����_N) %>%
        summarise(Position = sum(Count, na.rm = T))
      
      immetca$Min <- c(NA, 100, 5, 50, rep(100, 7), rep(300, 3), 20, 2000)
      immetca$status <- ifelse(immetca$Position < immetca$Min, "NEED!", "OK")
      immetca_need <- immetca %>% filter(status == "NEED!")
      if (nrow(immetca_need) == 0) immetca <- good else immetca <- immetca_need
      rhandsontable(immetca)
    })
  
  # output balance table
  output$balance_table <- renderDataTable(
    {
      base_df<-values$base_df
      need <- input$choice
      from <- input$category
      sel_col <- base_df %>% select(from)
      base_df[sel_col == need, ]
    },
    filter = "top",
    options = list(
      pageLength = 5,
      lengthMenu = list(c(5, 10, -1), c("5", "10", "All")),
      scrollX = TRUE, scrollY = "350px"
    )
  )
  
  #output table in Table ----
  output$contents <- renderDataTable(
    {
      base_df<-values$base_df
      columns <- names(base_df)
      if (!is.null(input$select)) {
        columns <- input$select
      }
      base_df[, columns, drop = FALSE]
    },
    filter = "top",
    options = list(
      pageLength = 5,
      lengthMenu = list(c(5, 10, -1), c("5", "10", "All")),
      scrollX = TRUE, scrollY = "420px"
    )
  )
}

runApp(shinyApp(ui = ui, server = server), launch.browser = T)
