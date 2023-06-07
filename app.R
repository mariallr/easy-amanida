##################################AMANIDA SHINY APP######################################
# rm(list=ls())
#LOAD LIBRARIES
library(shiny)
library(shinyWidgets)
library(shinyFeedback) #Alerts
library(amanida)
library(bslib)
library(shinyjs)
library(shinycssloaders)
library(kableExtra)
library(stringr)
library(shinydashboardPlus)
library(data.table)
library(DT)
library(BiocManager)
library(metaboliteIDmapping)
#----------------------
no_accent <- function (x) gsub("à","a",x)
#Load amanida custom functions:
source("plots_amanida.R")

#Set working directory. Has to be commented for the app to work. When needed descomment:
# setwd("S:\\6_Projectes\\2021_72AMANIDA\\5_Productes\\amanida")

#Deploy APP. When needed descomment:
#Run one time this before deploying:
# options(repos = BiocManager::repositories())
# library(rsconnect)
# rsconnect::deployApp('S:\\6_Projectes\\2021_72AMANIDA\\5_Productes\\easy-amanida',account="ubidi")

#Load Dictionary with language translations between English, Spanish and Catalan:
Translate<-fread("translate.csv") %>%
  as.data.frame() %>%
  column_to_rownames("var")

#Add to navbarPage the option of puting an input, for the selector of language in the bar above:
navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}


#Initializate the dataframe df containing the name of the language and the image that will be shown altogether in the bar above:
df <- data.frame(
  val = c("Català","Castellano","English")
)

df$img = c(
  sprintf("<img src='catala.png' width=20px><div class='jhr'>%s</div></img>", df$val[1]),
  sprintf("<img src='castellano.png' width=20px><div class='jhr'>%s</div></img>", df$val[2]),
  sprintf("<img src='english.png' width=20px><div class='jhr'>%s</div></img>", df$val[3])
)

#################################################################################################
## User Interface (UI)
#################################################################################################
ui <- fluidPage(
  #To be able to use the box function from the ShinyDashboardPlus package:
  useShinydashboard(),
  #Change the colors, font style, etc. to minty theme:
  theme = bslib::bs_theme(
    version=4,
    bootswatch = "minty"
  ),
  #Line of HTML code for customizing app aesthetics:
  tags$head(tags$style(
    ".direct-chat-contacts {
    width: 25%;
    position: absolute;
    right: 0px;
    }
    "
    ),
    tags$style(
      ".jhr{
    display: inline;
    vertical-align: middle;
    padding-left: 5px;
}
    "
    ),
    tags$style(
      ".btn-default{
      background-color:#f3969a;
      }"
    ),
    tags$style(
      ".btn-default:hover{
      background-color:#f3969a;
      }"
    ),
    tags$style(
      ".form-control{
      background-color:#f8f9fa;
      }"
    ),
    tags$style(
      "#reset1 {
    background-color: #78c2ad;
}"
    ),
    tags$style(
      "#reset2 {
    background-color: #78c2ad;
}"
    ),
    tags$style(
      ".input-group{
      margin-bottom:-10px;
      }"
    )
  ),
  navbarPageWithInputs(
    windowTitle = "Easy-Amanida APP",
    title=div(img(src="urv-bandera-color.png", width="12%"),
              img(src="IISPV_LogotipSenseFons.png", width="8%"),
              img(src="logoIDIBELL.png",width="10%")
    ),
    inputs=pickerInput(inputId = "lan",
                       label = NULL,
                       choices=df$val,
                       selected="English",
                       choicesOpt = list(content = df$img),
                       width='140px')
  ),
  sidebarLayout(
    sidebarPanel(
      ##### LEFT SIDE MENU #####  
      tags$span(style="font-size:24px;font-weight:bold;font-family:Montserrat;color:#78c2ad;","Easy-Amanida"),
      wellPanel(id="data",
                style="margin-top:15px;",
                useShinyFeedback(),
                tags$span(style="font-size:25px;font-weight:500;font-family:Montserrat;",textOutput("text_load_data")),
                br(),
                fileInput("file",textOutput("text_file"),placeholder=""),
                uiOutput("ui_exdata"),
                wellPanel(style="margin-top:20px;padding: 0.5rem;margin-bottom:20px;",htmlOutput("upload_message")),
                hr(),
                uiOutput("ui_data"),
                hr(),
                uiOutput("ui_param_calc")
      )
    ),
    #---------------------------------
    
    ########## MAIN PANEL ###################
    mainPanel(
      navbarPage(title = "", id="menu",
                 tabPanel(textOutput("title_home"),
                          br(),
                          fluidRow(
                            column(9,
                                   offset=3,
                                   imageOutput("home",width="50%")
                            )
                          )
                 ),
                 #Menu 1. Dataset preview
                 tabPanel(value="Data preview",
                          textOutput("title_data_preview"),
                          uiOutput("data_preview")
                 ),
                 # Menu 2. Quantitative Analysis
                 tabPanel(textOutput("title_quant_ana"),
                          uiOutput("quant_ana")
                 ),
                 # Menu 3. Qualitative Analysis
                 tabPanel(textOutput("title_qual"),
                          uiOutput("ui_qual")
                 ),
                 tabPanel(textOutput("title_report"),
                          uiOutput("ui_report")
                          ),
                 # Menu 4. CREDITS
                 tabPanel(textOutput("title_help"),
                          tabsetPanel(
                            tabPanel(textOutput("title_how_to"),
                                     wellPanel(
                                       uiOutput("ui_how_to")
                                     )
                            ),
                            tabPanel(textOutput("title_about"),
                              wellPanel(uiOutput("ui_about")
                              )
                          )
                        )
                          
                 )
      )
      
    )
  )
)

#################################################################################################
## Server
#################################################################################################

server <- function(input, output, session){
  
  output$home <- renderImage({
    list(src = "WWW/easyamanida_nom.png")
    },deleteFile = FALSE)
    
  
  #---------------Translations----------------
  output$text_load_data <- renderText({
    Translate["load_data",input$lan]
  })
  
  output$text_file <- renderText({
    Translate["upload_data",input$lan]
  })
  
  output$ui_exdata <- renderUI({
    actionButton("exdata",Translate["example",input$lan],icon=icon("arrow-circle-up"))
  })
  
  output$title_home <- renderText({
    Translate["home",input$lan]
  })
  
  output$title_data_preview <- renderText({
    Translate["data_preview",input$lan]
  })
  
  output$title_quant_ana <- renderText({
    Translate["quant_ana",input$lan]
  })
  
  output$title_qual <- renderText({
    Translate["qual_ana",input$lan]
  })
  
  output$title_report <- renderText({
    Translate["report",input$lan]
  })
  
  
  output$title_help <- renderText({
    Translate["help",input$lan]
  })
  
  output$title_about <- renderText({
    Translate["about",input$lan]
  })
  
  output$title_how_to <- renderText({
    Translate["how_to",input$lan]
  })
  
  output$ui_how_to <- renderUI({
    includeHTML(str_glue("how_to_{no_accent(input$lan)}.html"))
  })
  
  output$ui_about <- renderUI({
    includeHTML(str_glue("about_{no_accent(input$lan)}.html"))
  })
  
  #---------------READ DATA-------------------------
  rdata<-reactiveValues(file=NULL,
                        df=NULL,
                        df_go=NULL,
                        text="",
                        result=NULL)
  
  observeEvent(input$lan,{
    rdata$df <- NULL
    if(is.null(input$df)){
      rdata$text <- str_glue('{Translate["no_data",input$lan]}...')
    }
  })
  
  observeEvent(input$file,{
    rdata$df <- NULL
    rdata$df_go <- NULL
    rdata$text <- Translate["no_data",input$lan]
    rdata$result <- NULL
    rdata$file <- input$file$datapath
    dat <- try(data.table::fread(rdata$file),silent=TRUE)
    if(!inherits(dat,"try-error")){
      dat <- dat %>% 
        as.data.frame()
      
    if(ncol(dat)>=5){
          rdata$df <- dat
          rdata$text <- paste0(Translate["data_uploaded",input$lan],": <i>",input$file$name,"</i>")
    }else{
      showToast(
        "error",
        Translate["less_5var",input$lan]
      )
    }
    }else{
      showToast(
        "error", 
        Translate["cant_read",input$lan]
      )
    }
  })
  
  observeEvent(input$exdata,{
    rdata$df <- NULL
    rdata$file <- getsampleDB()
    rdata$df <- data.table::fread(rdata$file) %>% 
      as.data.frame()
    rdata$df_go <- NULL
    rdata$text <- str_glue('{Translate["data_uploaded",input$lan]}: <i> {Translate["example",input$lan]}</i>')
    rdata$result <- NULL
    reset("file")
  })  
    
  observeEvent(rdata$df,{
    if(!is.null(rdata$df)){
      showToast(
        "success",
        Translate["success_message",input$lan]
      )
    }
  })
  
  observeEvent(rdata$df,{
    if(!is.null(rdata$df)){
      updateTabsetPanel(session,
                        inputId = "menu",
                        selected="Data preview")
    }
  })
  
  
  output$upload_message <- renderText({
    rdata$text
  })
  
  #-----------------------
  
  #-----------------REACTIVE UI THAT SHOWS UP WHEN UPLOADING ANY DATA------------
  
  output$ui_data <- renderUI({
    if(!is.null(rdata$df)){
      modes <- list(0,1,2)
      names(modes) <- c(" ",Translate["quan",input$lan],Translate["qual",input$lan])
      tagList(
        selectInput("mode",Translate["select_mode",input$lan],choices=modes),
        uiOutput("ui_data2")
      )
      }else{
      HTML(str_glue('<i>{Translate["upload_any",input$lan]}...</i>'))
    }
  })
  
  output$ui_data2 <- renderUI({
    if(!is.null(input$mode) & input$mode!="0"){
      if(input$mode=="1"){
        tagList(
          awesomeCheckbox(
            inputId = "show_id",
            label = Translate["show_id",input$lan], 
            value = FALSE
          ),
          HTML(str_glue('<i>{Translate["select_needed",input$lan]}:</i>')),
          wellPanel(
            style="background:white;padding:5px;margin-top:5px;",
            fluidRow(
              column(6,
                     str_glue('{Translate["compound_name",input$lan]}:')
                     ),
              column(6,
                     selectizeInput("compound_name", label=NULL, choices=c("",names(rdata$df)))
                     )
            )
          ),
          wellPanel(
            style="background:white;padding:5px;margin-top:5px;",
            fluidRow(
              column(6,
                     str_glue('{Translate["p_value",input$lan]}:')
              ),
              column(6,
                     selectizeInput("p_value", label=NULL, choices=c("",names(rdata$df)))
              )
            )
          ),
          wellPanel(
            style="background:white;padding:5px;margin-top:5px;",
            fluidRow(
              column(6,
                     Translate["fold_change",input$lan]
              ),
              column(6,
                     selectizeInput("fold_change", label=NULL, choices=c("",names(rdata$df)))
              )
            )
          ),
          wellPanel(
            style="background:white;padding:5px;margin-top:5px;",
            fluidRow(
              column(6,
                     "N total:"
              ),
              column(6,
                     selectizeInput("n_total", label=NULL, choices=c("",names(rdata$df)))
              )
            )
          ),
          wellPanel(
            style="background:white;padding:5px;margin-top:5px;",
            fluidRow(
              column(6,
                     str_glue('{Translate["references",input$lan]}:')
              ),
              column(6,
                     selectizeInput("references", label=NULL, choices=c("",names(rdata$df)))
              )
            )
          )
        )
      }else{
        tagList(
          HTML(str_glue('<i>{Translate["select_needed",input$lan]}:</i>')),
          wellPanel(
            style="background:white;padding:5px;margin-top:5px;",
            fluidRow(
              column(6,
                     str_glue('{Translate["compound_name",input$lan]}:')
              ),
              column(6,
                     selectizeInput("compound_name", label=NULL, choices=c("",names(rdata$df)))
              )
            )
          ),
          wellPanel(
            style="background:white;padding:5px;margin-top:5px;",
            fluidRow(
              column(6,
                     str_glue('{Translate["behaviour",input$lan]}:')
              ),
              column(6,
                     selectizeInput("behaviour", label=NULL, choices=c("",names(rdata$df)))
              )
            )
          ),
          wellPanel(
            style="background:white;padding:5px;margin-top:5px;",
            fluidRow(
              column(6,
                     str_glue('{Translate["references",input$lan]}:')
              ),
              column(6,
                     selectizeInput("references", label=NULL, choices=c("",names(rdata$df)))
              )
            )
          )
        )
        
      }
      
    }else{
      HTML(str_glue('<i>{Translate["select_proceed",input$lan]}...</i>'))
    }
  })
  
  #Warning si es tria show_id:
  observeEvent(input$show_id,{
    if(!is.null(input$show_id)){
      if(input$show_id){
        showToast(type="warning",
                  Translate["warning_show_id",input$lan]
                  )
      }
    }
    
  })
  #Update each choice selection:
  #p-value:
  observeEvent(c(input$compound_name,input$p_value,input$fold_change,input$n_total,input$references,input$behaviour),{
    if(input$mode=="1"){
      
      choices_sel <- c(input$compound_name,input$p_value,input$fold_change,input$n_total,input$references)
      choices1 <- names(rdata$df)[!names(rdata$df)%in%choices_sel[choices_sel!=input$compound_name]]
      updateSelectizeInput(session,"compound_name",choices=c("",choices1),selected=input$compound_name)
      
      choices2 <- names(rdata$df)[!names(rdata$df)%in%choices_sel[choices_sel!=input$p_value]]
      updateSelectizeInput(session,"p_value",choices=c("",choices2),selected=input$p_value)
      
      choices3 <- names(rdata$df)[!names(rdata$df)%in%choices_sel[choices_sel!=input$fold_change]]
      updateSelectizeInput(session,"fold_change",choices=c("",choices3),selected=input$fold_change)
      
      choices4 <- names(rdata$df)[!names(rdata$df)%in%choices_sel[choices_sel!=input$n_total]]
      updateSelectizeInput(session,"n_total",choices=c("",choices4),selected=input$n_total)
      
      choices5 <- names(rdata$df)[!names(rdata$df)%in%choices_sel[choices_sel!=input$references]]
      updateSelectizeInput(session,"references",choices=c("",choices5),selected=input$references)
      
    }else{
      
      choices_sel <- c(input$compound_name,input$behaviour,input$references)
      
      choices1 <- names(rdata$df)[!names(rdata$df)%in%choices_sel[choices_sel!=input$compound_name]]
      updateSelectizeInput(session,"compound_name",choices=c("",choices1),selected=input$compound_name)
      
      choices2 <- names(rdata$df)[!names(rdata$df)%in%choices_sel[choices_sel!=input$behaviour]]
      updateSelectizeInput(session,"behaviour",choices=c("",choices2),selected=input$behaviour)
      
      choices3 <- names(rdata$df)[!names(rdata$df)%in%choices_sel[choices_sel!=input$references]]
      updateSelectizeInput(session,"references",choices=c("",choices3),selected=input$references)
      
    }
  })
  
  columns <- reactiveVal(NULL)
  
  observeEvent(c(input$mode,input$compound_name,input$p_value,input$fold_change,input$n_total,input$references,input$behaviour),{
    if(!is.null(input$mode)){
      if(input$mode=="1"){
        
        cols <- c(input$compound_name,input$p_value,input$fold_change,input$n_total,input$references)
        
        columns(cols[cols!=""])
        
      }else{
        
        cols <- c(input$compound_name,input$behaviour,input$references)
        
        columns(cols[cols!=""])
        
      }
    }
  })
  
  output$prova <- renderPrint({
    columns()
  })
  
  output$ui_param_calc <- renderUI({
    if(!is.null(input$mode) & !is.null(rdata$df)){
      ncol <- c(5,3)[as.numeric(input$mode)]
      
      if((input$mode=="1" & length(columns())==5) | (input$mode=="2" & length(columns())==3)){
        actionBttn("go",Translate["calculate",input$lan],style="unite",icon=icon("sync"),color="success",block=TRUE)
      }else{
        HTML(str_glue('<i>{Translate["you_have",input$lan]} {ncol} {Translate["columns_needed",input$lan]}</i>'))
      }
    }
  })
  
  ##------MENU 1: DATA PREVIEW-------
  
  output$data_preview <- renderUI({
    if(length(columns())>0){
      htmlOutput("go_data_preview") %>% 
        withSpinner()
    }else{
      imageOutput("ini_data_preview")
    }
  })
  
  output$go_data_preview <- renderText({
    dat <- rdata$df[,columns()] %>% 
      as.data.frame()
    
    names(dat) <- columns()
    
    if(init()){
      kable(dat,escape=F)%>%
        kable_styling(bootstrap_options = c("striped", "hover")) %>%
        scroll_box(width = "100%", height = "760px")
    }else{
      #Paint rows with missing values (empty or missing) that amanida will not compute and send message:
      w_miss <- which(rowSums(dat=="" | is.na(dat))>0)
      
      if(length(w_miss)>0){
        showToast(
          "warning", 
          str_glue('{Translate["dataset_contains",input$lan]} {length(w_miss)} {Translate["missing_message",input$lan]}')
        )
      } 
      
      
      kable(dat,escape=F)%>%
        kable_styling(bootstrap_options = c("striped", "hover")) %>%
        row_spec(w_miss, bold = T, color = "white", background = "#ffce67")%>%
        scroll_box(width = "100%", height = "760px")
      
      
    }
  })
  
  output$ini_data_preview <- renderImage({
    # Return a list containing the filename and alt text
    list(src = str_glue("WWW/ini_page_{no_accent(input$lan)}.png"),
         width="100%",
         height="100%")
  },deleteFile = FALSE)
  
  #-------
  
  #-------------GENERAL CALCULATIONS----------------
  observeEvent(input$go,{
    
    withProgress(message=paste0(Translate["calc_message",input$lan],":"),
                 detail=Translate["patience",input$lan],
                 value=0,
                 {
                   incProgress(1/15)
                   mode <- c("quan","qual")[as.numeric(input$mode)]
                   rdata$df_go <- amanida_read(rdata$file, mode = mode, columns(), separator=";")
                   incProgress(5/15)
                   if(input$mode=="1"){
                     rdata$result <- compute_amanida(rdata$df_go, comp.inf = input$show_id)
                   }else{
                     rdata$result <- amanida_vote(rdata$df_go)
                   }
                   incProgress(15/15)
                 }
                 )
    
    
  })
  
  #Go to data preview when clicking calculate!
  observeEvent(input$go,{
    # tabsel <- str_glue("{input$mode} Analysis")
    updateTabsetPanel(session,
                      inputId = "menu",
                      selected="Data preview")

  })
  #-------------------
  
  #-----------MAIN PANEL-------------
  init <- reactiveVal(TRUE)
  observeEvent(input$go,{
    if(is.null(rdata$df_go) | length(columns())==0) init(TRUE)
    else init(FALSE)
  })
  #When we change the parameters, init has to be TRUE again
  observeEvent(c(columns()),{
    init(TRUE)
  })
  
  #When we change the mode, init has to be TRUE again
  
  observeEvent(input$mode,{
    init(TRUE)
  })
  
  #When we change the language, init has to be TRUE again
  
  observeEvent(input$lan,{
    init(TRUE)
  })
  
  
  
  ##------MENU 2: QUANTITATIVE ANALYSIS---------
  
  output$quant_ana <- renderUI({
    if(!is.null(rdata$df)){
      if(!init() & input$mode=="1"){
        tabsetPanel(
          tabPanel(Translate["volcano_plot",input$lan],
                   box(
                     width=12,
                     title="",
                     uiOutput("ui_plot_volcano"),
                     sidebar = boxSidebar(
                       width=25,
                       id = "boxsidebar_volcano",
                       startOpen = TRUE,
                       sliderInput("cutoff_pval",label=Translate["cutoff_pvalue",input$lan],min=0.01, max=0.1, value=0.05, step=0.01),
                       sliderInput("cutoff_fold",label=Translate["cutoff_fold_change",input$lan],min=2,max=10,value=4,step=0.1),
                       fluidRow(
                         column(4),
                         column(4,
                                actionButton("reset1", Translate["reset",input$lan])
                                ),
                         column(4)
                       ),
                       br(),
                       prettySwitch(
                         inputId = "show_names",
                         label = Translate["show_label",input$lan],
                         status="primary",
                         fill=TRUE,
                         value=TRUE
                       ),
                       br(),
                       fluidRow(
                         column(3),
                         column(
                           8,
                           downloadButton("down_volcano")
                         ),
                         column(3)
                       )
                     )
                   )
                   ),
          tabPanel(Translate["table_results",input$lan],
                   br(),
                   dataTableOutput("table_volcano")
                   ),
          tabPanel(Translate["table_results_id",input$lan],
                   br(),
                   dataTableOutput("table_volcano_id")
          )
        )
      }else if(!init() & input$mode=="2"){
        imageOutput("ini_quant_qual")
      }else{
        imageOutput("ini_quant_calc")
      }
    }else{
      imageOutput("ini_quant")
    }
  })
  
  observeEvent(input$reset1,{
    updateSliderInput(session,"cutoff_pval",value=0.05)
    updateSliderInput(session,"cutoff_fold",value=4)
  })
  
  output$down_volcano <- downloadHandler(
      filename="volcano_plot.png",
      content = function(file) {
        shiny::withProgress(
          message = str_glue('{Translate["downloading",input$lan]}...'),
          value = 0,
          {
            shiny::incProgress(1/10)
            Sys.sleep(1)
            shiny::incProgress(5/10)
            if(input$show_names){
              plot <- volcano_plot(rdata$result,cutoff=c(input$cutoff_pval,input$cutoff_fold)) 
              ggsave(file,plot,dpi="retina")
            }else{
              plot <- volcano_plot(rdata$result,cutoff=c(input$cutoff_pval,input$cutoff_fold),names=FALSE) 
              ggsave(file,plot,dpi="retina")
            }
          }
        )
      }
  )
  
  output$ui_plot_volcano <- renderUI({
    if(input$show_names){
      plotlyOutput("plotly_volcano",height="600px") %>%
        withSpinner()
    }else{
      plotOutput("plot_volcano",height="600px") %>% 
        withSpinner()
    }
  })
  
  output$plot_volcano <- renderPlot({
      volcano_plot(rdata$result,cutoff=c(input$cutoff_pval,input$cutoff_fold),names=FALSE)
  })
  
  output$plotly_volcano <- renderPlotly({
    volcano_plotly(rdata$result,cutoff=c(input$cutoff_pval,input$cutoff_fold)) %>% 
      layout(xaxis = list(title = "log2(Fold-change)"),
             yaxis = list(title = "-log10(<i>p</i>-value)"),
             #Don't overlap legend with axis title:
             legend = list(y = -0.2)
             )
  })
  
  
  output$ini_quant <- renderImage({
    # Return a list containing the filename and alt text
    list(src = str_glue("WWW/ini_page_{no_accent(input$lan)}.png"),
         width="100%",
         height="100%")
  },deleteFile = FALSE)
  
  output$ini_quant_calc <- renderImage({
    # Return a list containing the filename and alt text
    list(src = str_glue("WWW/ini_page_calc_{no_accent(input$lan)}.png"),
         width="100%",
         height="100%")
  },deleteFile = FALSE)
  
  output$ini_quant_qual <- renderImage({
    # Return a list containing the filename and alt text
    list(src = str_glue("WWW/ini_page_qual_{no_accent(input$lan)}.png"),
         width="100%",
         height="100%")
  },deleteFile = FALSE)
  
  
  output$table_volcano <- renderDataTable(server = FALSE, {
    
    
    taula <- rdata$result@stat %>% 
      dplyr::select(id:N_total) %>% 
      mutate_if(is.numeric,signif,2) %>% 
      arrange(pval) 
      
    
    language <- c("Catalan","Spanish","English")[input$lan==c("Català","Castellano","English")]
    
    datatable(
      taula,
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(
        dom = 'Bftipr',
        buttons = list(
          list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = Translate["download", input$lan]
          )
        ),
        language = list(
          url = str_glue(
            '//cdn.datatables.net/plug-ins/1.10.11/i18n/{language}.json'
          )
        )
      )
    )
  })
  
  output$table_volcano_id <- renderDataTable(server = FALSE, {
    
    taula <- rdata$result@stat %>% 
      dplyr::select(id,PubChem_CID:Drugbank)
    
    language <- c("Catalan","Spanish","English")[input$lan==c("Català","Castellano","English")]
    
    datatable(
      taula,
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(
        dom = 'Bftipr',
        buttons = list(list(
          extend = 'collection',
          buttons = c('csv', 'excel', 'pdf'),
          text = Translate["download", input$lan]
        )),
        language = list(
          url = str_glue(
            '//cdn.datatables.net/plug-ins/1.10.11/i18n/{language}.json'
          )
        ),
        scrollX = TRUE
      )
    )
  })
  
  
  ##-------------------
  
  ##------MENU 3: QUALITATIVE ANALYSIS---------
  
  #Search maximum number of votes for count option:
  max_counts <- reactiveVal(10)
  observeEvent(rdata$result,{
    max_counts(max(abs(rdata$result@vote$votes)))
  })
  
  output$ui_qual <- renderUI({
    if(!is.null(rdata$df)){
      if(!init()){
        tabsetPanel(
          tabPanel(Translate["vote_plot",input$lan],
                   box(
                     width=12,
                     title="",
                     plotOutput("vote",height="auto") %>% 
                       withSpinner(),
                     sidebar = boxSidebar(
                       width=25,
                       id = "boxsidebar_vote",
                       startOpen = TRUE,
                       sliderInput("counts",label=Translate["counts_cutoff",input$lan],min=0, max=max_counts(), value=1, step=1),
                       br(),
                       fluidRow(
                         column(3),
                         column(
                           8,
                           downloadButton("down_vote")
                         ),
                         column(3)
                       )
                     )
                   )
          ),
          tabPanel(Translate["explore_plot",input$lan],
                   uiOutput("ui_explore_plot")
          )
        )
      }else{
        imageOutput("ini_qual_calc1")
      }
    }else{
      imageOutput("ini_qual1")
    }
  })
  
  #Compute height of vote (depends on counts input):
  
  height_vote <- reactiveVal(600)
  
  observeEvent(c(rdata$result,input$counts),{
    if(!is.null(input$counts)){
        sdat <- rdata$result@vote %>% 
          filter(abs(votes)>=input$counts)
        
        height_vote(max(600,nrow(sdat)*8))
    }
  })
  
  output$down_vote <- downloadHandler(
    filename="vote_plot.png",
    content = function(file) {
      shiny::withProgress(
        message = str_glue('{Translate["downloading",input$lan]}...'),
        value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(5/10)
          plot <- vote_plot(rdata$result,counts=input$counts)
          #conversion of px to inches (px aren't admitted). Also, the height in the image saved requires to be bigger than the one in the app (+1000px):
          ggsave(file,plot,dpi="retina",height=(height_vote()+1000)*0.0104166667)
        }
      )
    }
  )
  
  output$vote <- renderPlot({
    if(!is.null(input$counts)){
      vote_plot(rdata$result, counts=input$counts)
    }
  },height=function(){
    height_vote()
  })
  
  output$ini_qual1 <- renderImage({
    # Return a list containing the filename and alt text
    list(src = str_glue("WWW/ini_page_{no_accent(input$lan)}.png"),
         width="100%",
         height="100%")
  },deleteFile = FALSE)
  
  output$ini_qual_calc1 <- renderImage({
    # Return a list containing the filename and alt text
    list(src = str_glue("WWW/ini_page_calc_{no_accent(input$lan)}.png"),
         width="100%",
         height="100%")
  },deleteFile = FALSE)
  
  ##-------------------
  
  output$ui_explore_plot <- renderUI({
    if(!is.null(rdata$df)){
      if(!init()){
        box(
          width=12,
          title="",
          plotOutput("explore",height="auto") %>% 
            withSpinner(),
          sidebar = boxSidebar(
            width=25,
            id = "boxsidebar_explore",
            startOpen = TRUE,
            # selectInput("type_explore","Type of explore plot", choices=c("all","sub","mix"),selected="sub"),
            sliderInput("counts_explore",label="Counts cut-off",min=0, max=max_counts(), value=1, step=1),
            # prettySwitch(
            #   inputId = "show_all2",
            #   label = "Show All",
            #   status="primary",
            #   fill=TRUE,
            #   value=FALSE
            # ),            
            br(),
            fluidRow(
              column(3),
              column(
                8,
                downloadButton("down_explore")
              ),
              column(3)
            )
          )
        )
      }else{
        imageOutput("ini_qual_calc2")
      }
    }
  })
  
  #Compute height of vote (depends on counts input):
  
  height_explore <- reactiveVal(600)
  
  observeEvent(c(rdata$result,input$counts_explore),{
    
    if(!is.null(input$counts_explore)){
      
      dat <- explore_plot(rdata$df_go, type="all", counts=input$counts_explore, return_data = TRUE)
      
      height_explore(max(600,nrow(dat)*11))
    }
  })
  
  output$down_explore <- downloadHandler(
    filename="explore_plot.png",
    content = function(file) {
        shiny::withProgress(
          message = str_glue('{Translate["downloading",input$lan]}...'),
          value = 0,
          {
            shiny::incProgress(1/10)
            Sys.sleep(1)
            shiny::incProgress(5/10)
            plot <- explore_plot(rdata$df_go, type="all", counts=input$counts_explore)
            #conversion of px to inches (px aren't admitted). Also, the height in the image saved requires to be bigger than the one in the app (+1000px):
            ggsave(file,plot,dpi="retina",height=(height_explore()+1000)*0.0104166667)
          }
        )
    }
  )
  
  # output$explore <- renderPlot({
  #   if(input$show_all2){
  #     explore_plot(rdata$df_go, type = "all", counts = input$counts_explore)
  #   }else{
  #     explore_plot(rdata$df_go, type = "all", counts = input$counts_explore)
  #   }
  # },height=function(){
  #    if(!input$show_all2){
  #      600
  #    }else{
  #      nrow(rdata$df_go)*10
  #    }
  # }
  # )
  
  output$explore <- renderPlot({
    explore_plot(rdata$df_go, type = "all", counts = input$counts_explore)
  },height=function(){
      height_explore()
  }
  )
  
  output$ini_qual_calc2 <- renderImage({
    # Return a list containing the filename and alt text
    list(src = str_glue("WWW/ini_page_calc_{no_accent(input$lan)}.png"),
         width="100%",
         height="100%")
  },deleteFile = FALSE)
  
  
  #-----------MENU 4: REPORT---------------
  output$ui_report <- renderUI({
    if(!is.null(rdata$df)){
      if(!init()){
        tagList(
          str_glue('{Translate["select_mode",input$lan]}'),
          wellPanel(
            uiOutput("ui_mode_rep")
            ),
          br(),
          str_glue('{Translate["select_param",input$lan]}'),
          wellPanel(
            awesomeCheckbox(
              inputId = "show_id_rep",
              label = Translate["show_id",input$lan], 
              value = FALSE
            ),
            uiOutput("ui_params_rep"),
            sliderInput("counts_rep",label=Translate["counts_cutoff",input$lan],min=0, max=max_counts(), value=1, step=1)
            # prettySwitch("show_code",
            #              label="Show code?",
            #              status="primary",
            #              fill=TRUE,
            #              value=FALSE)
          ),
          br(),
          str_glue('{Translate["click_down",input$lan]}...'),
          wellPanel(
            downloadButton("report", label=Translate["down_report",input$lan])
          )
        )
      }else{
        imageOutput("ini_report_calc")
      }
    }else{
      imageOutput("ini_report")
    }
  })
  
  output$ui_mode_rep <- renderUI({
    if(!is.null(input$mode)){
      modes <- list(1,2,3)
      names(modes) <- c(Translate["quan_qual",input$lan],Translate["quan",input$lan],Translate["qual",input$lan])
      if(input$mode=="1"){
        selectInput("mode_rep",label=NULL,choices=modes[c(1,2)])
      }else{
        selectInput("mode_rep",label=NULL,choices=modes[3])
      }
    }
  })
  
  output$ui_params_rep <- renderUI({
    if(!is.null(input$mode)){
        if(input$mode!="2"){
          tagList(
            sliderInput("cutoff_pval_rep",label=Translate["cutoff_pvalue",input$lan],min=0.01, max=0.1, value=0.05, step=0.01),
            sliderInput("cutoff_fold_rep",label=Translate["cutoff_fold_change",input$lan],min=2,max=10,value=4,step=0.1),
            fluidRow(
              column(4),
              column(2,offset=1,
                     actionButton("reset2", Translate["reset",input$lan])
              ),
              column(4)
            ),
            br()
          )
        }
    }
  })
  
  observeEvent(input$reset2,{
    updateSliderInput(session,"cutoff_pval_rep",value=0.05)
    updateSliderInput(session,"cutoff_fold_rep",value=4)
  })
  
  output$report <- downloadHandler(
    filename = function(){
      mode <- c("quanqual","quan","qual")[as.numeric(input$mode_rep)]
      str_glue("amanida_report_{mode}.html")
    },
    content = function(file) {
      shiny::withProgress(
        message = str_glue('{Translate["downloading_report",input$lan]}...'),
        value=0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(5/10)
          # Set up parameters to pass to Rmd document
          
          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          if(input$mode_rep=="1"){
            params <- list(file_name=rdata$file,
                           separator=";",
                           analysis_type="quan-qual",
                           column_id=columns(),
                           comp_inf = input$show_id_rep,
                           pvalue_cutoff=input$cutoff_pval_rep,
                           fc_cutoff=input$cutoff_fold_rep,
                           votecount_lim=input$counts_rep
                           # show_code=input$show_code
                           )
            
            
            rmarkdown::render("amanida_report_quanqual_v4.Rmd", output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
          }else if(input$mode_rep=="2"){
            params <- list(file_name=rdata$file,
                           separator=";",
                           analysis_type="quan",
                           column_id=columns(),
                           comp_inf = input$show_id_rep,
                           pvalue_cutoff=input$cutoff_pval_rep,
                           fc_cutoff=input$cutoff_fold_rep,
                           votecount_lim=input$counts_rep
                           # show_code=input$show_code
                           )
            
            rmarkdown::render("amanida_report_quan_v4.Rmd", output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
          }else{
            
              params <- list(file_name=rdata$file,
                             separator=";",
                             analysis_type="qual",
                             column_id=columns(),
                             comp_inf = input$show_id_rep,
                             votecount_lim=input$counts_rep
                             # show_code=input$show_code
                             )
            
            rmarkdown::render("amanida_report_qual_v3.Rmd", output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
          }
          
        }
      )
    }
  )
  
  output$ini_report <- renderImage({
    # Return a list containing the filename and alt text
    list(src = str_glue("WWW/ini_page_{no_accent(input$lan)}.png"),
         width="100%",
         height="100%")
  },deleteFile = FALSE)
  
  output$ini_report_calc <- renderImage({
    # Return a list containing the filename and alt text
    list(src = str_glue("WWW/ini_page_calc_{no_accent(input$lan)}.png"),
         width="100%",
         height="100%")
  },deleteFile = FALSE)
  
  
}


## runApp

shinyApp(ui, server)
