library(shinythemes)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(formattable)
library(tidyverse)
#library(colmaps)
library(sf)
library(rworldmap)
library(agricolae) 
library(maps)
library(readr)
library(dplyr)
library(RColorBrewer)
library(classInt)
library(ggiraph)
library(rjson)
library(RSocrata)



url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
datos <- read_csv(url)
paises <- datos[datos$location!="World" & datos$location!="International",]
datos2 <- paises %>% group_by(location) %>% summarise(media1=mean(new_cases),
                                                      media2=mean(new_deaths))

world <- datos[datos$location=="World",]

# # base de datos colombia

# df <- read.socrata(
#   "https://www.datos.gov.co/resource/gt2j-8ykr.json",
#   app_token = "oTK8XvuyGq2JatvmWNo4f64bP",
#   email     = "sgarcesc@unal.edu.co",
#   password  = "Sara4948923"
# )

# url2 <- "https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv?accessType=DOWNLOAD"
# datacolombia <- read_csv(url2, n_max = 1)
# 
# 
# # ARREGLAR UN POCO LA BASE DE DATOS
# 
# colnames(datacolombia) <- c("ID", "Date", "Code DIVIPOLA", "City", "Department", "Atention", "Age",
#                           "Sex", "Type", "State", "CountryOrigin", "FIS", "Death date", 
#                           "Diagnosis date", "Recovery date", "Web reporting date")
# 
# 
# 
# datacolombia$City <- toupper(datacolombia$City)
# datacolombia$Sex <- toupper(datacolombia$Sex)
# datacolombia$CountryOrigin <- toupper(datacolombia$CountryOrigin)
# datacolombia$Department <- toupper(datacolombia$Department)
# str_sub(datacolombia$Atention, 1, 1) <- str_to_upper(str_sub(datacolombia$Atention, 1, 1))
# str_sub(datacolombia$Atention, 2) <- str_to_lower(str_sub(datacolombia$Atention, 2))
# str_sub(datacolombia$Type, 1, 1) <- str_to_upper(str_sub(datacolombia$Type, 1, 1))
# str_sub(datacolombia$Type, 2) <- str_to_lower(str_sub(datacolombia$Type, 2))
# str_sub(datacolombia$State, 1, 1) <- str_to_upper(str_sub(datacolombia$State, 1, 1))
# str_sub(datacolombia$State, 2) <- str_to_lower(str_sub(datacolombia$State, 2))
# 
# datacolombia <- datacolombia %>% 
# mutate(Atention = ifelse(Atention == "Fallecido", "Passed Away", 
#                         ifelse(Atention == "Recuperado", "Recovered",
#                         ifelse(Atention == "Casa", "Home",
#                         ifelse(Atention == "Hospital uci", "Hospital ICU", 
#                         ifelse(Atention == "Hospital", "Hospital", "N/A"))))))
# 
# datacolombia <- datacolombia %>% 
#   mutate(Type = ifelse(Type == "En estudio", "In Study", 
#                            ifelse(Type == "Importado", "Imported",
#                                   ifelse(Type == "Relacionado", "Related", "N/A"))))
# 
# datacolombia <- datacolombia %>% 
#   mutate(State = ifelse(State == "Fallecido", "Passed away", 
#                        ifelse(State == "Grave", "Serious",
#                               ifelse(State == "Leve", "Mild", 
#                                      ifelse(State == "Moderado", "Moderate", 
#                                             ifelse(State == "Asintomático", "Asymptomatic", "N/A"))))))
# 
# datoscolombia2 <- datacolombia %>% group_by(City) %>% summarise(n())
# datoscolombia3 <- datacolombia %>% group_by(Department) %>% summarise(n())
# 
# dep <- departamentos@data
# colnames(dep) <- c("id", "Department")
# dep$Department <- toupper(dep$Department)
# 
# dep <- dep %>% 
#   mutate(Department = replace(Department, Department == "ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA",
#                                 "SAN ANDRÉS")) %>%
#   mutate(Department = replace(Department, Department == "BOGOTÁ, D. C.",
#                                 "BOGOTÁ D.C."))




  ui = tagList(
      navbarPage(
          theme = shinytheme("flatly"),  # <--- To use a theme, uncomment this
          "The COVID-19 pandemic",
          tabPanel("COVID-19 around the world",
                   sidebarPanel(
                       selectInput(inputId = "country",
                                   label = "Country of interest",
                                   choices = c("Top 5", datos2$location),
                                   selected = "Top 5"),
                       dateRangeInput('daterange',
                                      label = 'Date range',
                                      start = Sys.Date() - 10, end = Sys.Date()
                       ),
                       radioButtons(inputId = "graph", 
                                    label = "Outcome", 
                                    choices = c("Number of confirmed cases/deaths each day",
                                                "Cumulative number of confirmed cases/deaths"),
                                    selected = "Number of confirmed cases/deaths each day"),
                       hr(),
                       helpText("Data from Our World in Data")
                   ),
                   mainPanel(
                       tabsetPanel(
                           tabPanel("Confirmed Cases",
                                    plotlyOutput("plot1"),
                                    hr(),
                                    formattableOutput("summary")
                           ),
                           tabPanel("Deaths", 
                                    plotlyOutput("plot2"),
                                    hr(),
                                    formattableOutput("summary2")
                                    )
                       )
                   )
          ),
          tabPanel("Global Statistics", 
                   sidebarPanel(
                           dateRangeInput('daterangeworld',
                                    label = 'Date range',
                                    start = Sys.Date() - 10, end = Sys.Date()),
                           radioButtons(inputId = "graph2", 
                                        label = "Outcome", 
                                        choices = c("Number of confirmed cases/deaths each day",
                                                    "Cumulative number of confirmed cases/deaths"),
                                        selected = "Number of confirmed cases/deaths each day"), 
                           hr(),
                           helpText("Data from Our World in Data")
                   ),
                   mainPanel(
                           tabsetPanel(
                                   tabPanel("Confirmed Cases",
                                            plotlyOutput("plot3"),
                                            hr(),
                                            formattableOutput("summary3")
                                   ),
                                   tabPanel("Deaths", 
                                            plotlyOutput("plot4"),
                                            hr(),
                                            formattableOutput("summary4")
                                   )
                           )
                   )
          ),
          tabPanel("Heat Map",
                   tabsetPanel(
                   tabPanel("Confirmed cases",
                      plotOutput("heatmapworld1")
                   ),
                   tabPanel("Deaths",
                      plotOutput("heatmapworld2")
                   )
                   )
          ),
          #tabPanel("COVID-19 in Colombia",
                           # tabsetPanel(
                           #         tabPanel("Cities",
                           #                  sidebarPanel(
                           #                    selectInput(inputId = "city",
                           #                                label = "City of interest",
                           #                                choices = datoscolombia2$City,
                           #                                selected = "Anapoima"),
                           #                    selectInput(inputId = "table",
                           #                                label = "Variable",
                           #                                choices = c("Atention",
                           #                                            "Age",
                           #                                            "Country of Origin",
                           #                                            "Sex",
                           #                                            "State",
                           #                                            "Type"),
                           #                                selected = "Atention"),
                           #                    
                           #                    hr(),
                           #                    helpText("Data from Datos Abiertos Colombia")
                           #                  ),
                           #                  mainPanel(
                           #                    formattableOutput("table")
                           #                  )
                           #         ),
                           #         tabPanel("Departments",
                           #                  sidebarPanel(
                           #                    selectInput(inputId = "department",
                           #                                label = "Department of interest",
                           #                                choices = datoscolombia3$Department,
                           #                                selected = "Amazonas"),
                           #                    selectInput(inputId = "table2",
                           #                                label = "Variable",
                           #                                choices = c("Atention",
                           #                                            "Age",
                           #                                            "Country of Origin",
                           #                                            "Sex",
                           #                                            "State",
                           #                                            "Type"),
                           #                                selected = "Atention"),
                           #                    
                           #                    hr(),
                           #                    helpText("Data from Datos Abiertos Colombia")
                           #                  ),
                           #                  mainPanel(
                           #                    formattableOutput("table2")
                           #                  )
                           #         ),
                           #         tabPanel("Country", 
                           #                  sidebarPanel(
                           #                    selectInput(inputId = "bar",
                           #                                label = "Variable",
                           #                                choices = c("Atention",
                           #                                            "Age",
                           #                                            "City",
                           #                                            "Country of Origin",
                           #                                            "Department",
                           #                                            "Sex",
                           #                                            "State",
                           #                                            "Type"),
                           #                                selected = "Atention"),
                           #                    
                           #                    hr(),
                           #                    helpText("Data from Datos Abiertos Colombia")
                           #                  ),
                           #                  mainPanel(
                           #                    plotlyOutput("plot5")
                           #                  )    
                           #         ),
                           #         tabPanel("Heat Map",
                           #                  plotOutput("heatmap")
                           #         )
                           # )
                   
                   #),
          tabPanel("Data",
                   DT::dataTableOutput("mytableworld")
                   # tabsetPanel(
                   #   tabPanel("World",
                   #            DT::dataTableOutput("mytableworld")
                   #   ),
                   #   tabPanel("Colombia",
                   #            DT::dataTableOutput("mytablecolombia")
                   #   )
                   # )
          ),
          tabPanel("About",
                   mainPanel(
                     titlePanel("The COVID-19 pandemic"),
                     h4("by R-Ladies Medellín"),
                     br(),
                     p("This app was created to give people around the world the opportunity to 
                       interactively access data related to the COVID-19 pandemic. The data used 
                       to build this app is updated once daily."),
                     p("In this app, different types of graphs, tables, heat maps, 
                       and other tools are included to help you know and understand the behavior of the COVID-19 
                       pandemic around the world. Also, you can find a section dedicated to the current 
                       state of the pandemic in Colombia."),
                     br(),
                     h4("Code"),
                     p("You can find the code used to build this Shiny app here: ",
                       a("GitHub", href = "https://github.com/SaraGarcesCespedes/CORONAVIRUS-APP")),
                     br(),
                     h4("Source"),
                     p("You can find the datasets used to build this Shiny app here: "),
                     p(strong("COVID-19 cases around the world: "),
                       a("Our World in Data", href = "https://ourworldindata.org/coronavirus-source-data")),
                     #p(strong("COVID-19 cases in Colombia: "),
                      # a("Datos Abiertos Colombia", href = "https://www.datos.gov.co/Salud-y-Protecci-n-Social/Casos-positivos-de-COVID-19-en-Colombia/gt2j-8ykr/data")),
                     br(),
                     # h4("Authors"),
                     # p("Sara Garcés Céspedes"),
                     # p("Master of Statistics Student, Universidad Nacional de Colombia"),
                     # p("sgarcesc@unal.edu.co")
                     )
                     
                   )
                   
          ) 
      
      )
  
  server = function(input, output, session) {
          

# Para trabajar con datos del país seleccionado ---------------------------
          result <- reactive({
            if(input$country == "Top 5"){
              data <- paises %>% group_by(location) %>% summarise(tail(new_cases, 1),
                                                                  tail(new_deaths, 1),
                                                                  tail(total_cases, 1),
                                                                  tail(total_deaths, 1))
              colnames(data) <- c("country", "tailnewcases", "tailnewdeaths", "tailtotalcases",
                                  "tailtotaldeaths")
              data1 <- arrange(data, -tailnewcases)
              data2 <- arrange(data, -tailnewdeaths)
              data3 <- arrange(data, -tailtotalcases)
              data4 <- arrange(data, -tailtotaldeaths)
              datanew <- cbind(data1[1:5, 1], data2[1:5, 1], data3[1:5, 1], data4[1:5, 1])
              colnames(datanew) <- c("newcases", "newdeaths", "totalcases", "totaldeaths")
            }else{
                  datanew <- paises[paises$location==input$country,]
                  datanew <- datanew %>% filter(date >= input$daterange[1] & date <= input$daterange[2])
            }
                  datanew
          })
          
          

# Para graficar casos confirmados del país seleccionado (ACUMULADO O NO ACUMULADO) --------
          output$plot1 <- renderPlotly({
              if(input$country == "Top 5"){
                if(input$graph == "Number of confirmed cases/deaths each day"){
                  data <- result()
                  data <- data[, 3]
                  datatop10 <- filter(paises, location %in% data)  %>% 
                                filter(date >= input$daterange[1] & date <= input$daterange[2])
                  ggplot(data=datatop10,
                         aes(x=date, y=new_cases, colour=location)) +
                         geom_line() + theme_bw() + geom_point() +
                         theme(legend.position="top", 
                               legend.title = element_text(size = 8),
                               legend.text = element_text(size = 6)) +
                         labs(x="Date", y="Number of new confirmed cases",
                              color="Country")
                }else{
                  data <- result()
                  data <- data[, 3]
                  datatop10 <- filter(paises, location %in% data)  %>% 
                                filter(date >= input$daterange[1] & date <= input$daterange[2])
                  ggplot(data=datatop10,
                         aes(x=date, y=total_cases, colour=location)) +
                    geom_line() + theme_bw() + geom_point() +
                    theme(legend.position="top", 
                          legend.title = element_text(size = 8),
                          legend.text = element_text(size = 6)) + 
                    labs(x="Date", y="Cumulative number of confirmed cases",
                         color="Country")
                }
              }else{
                 if(input$graph == "Number of confirmed cases/deaths each day"){
                 plot_ly(data = result(), x = ~date, y = ~new_cases, 
                         type = "scatter", mode = 'lines+markers', color = I('aquamarine3')) %>%
                          
                          layout(                        
                                  xaxis = list(           
                                          title = "Date"),       
                                  yaxis = list(           
                                          title = "Number of new confirmed cases")     
                          )
                 }else{
                         plot_ly(data = result(), x = ~date, y = ~total_cases, 
                                 type = "scatter", mode = 'lines+markers', color = I('aquamarine3')) %>%
                                 
                                 layout(                        
                                         xaxis = list(           
                                                 title = "Date"),       
                                         yaxis = list(           
                                                 title = "Cumulative number of confirmed cases")     
                                 )
                         
                 }
          }
          })
          
          

# Para graficar muertes del país seleccionado (ACUMULADO O NO ACUMULADO) --------
          output$plot2 <- renderPlotly({
            if(input$country == "Top 5"){
              if(input$graph == "Number of confirmed cases/deaths each day"){
                data <- result()
                data <- data[, 4]
                datatop10 <- filter(paises, location %in% data)  %>% 
                              filter(date >= input$daterange[1] & date <= input$daterange[2])
                ggplot(data=datatop10,
                       aes(x=date, y=new_deaths, colour=location)) +
                  geom_line() + geom_point() +
                  theme_bw() + theme(legend.position="top", 
                                     legend.title = element_text(size = 8),
                                     legend.text = element_text(size = 6)) +
                  labs(x="Date", y="Number of deaths",
                       color="Country")
              }else{
                data <- result()
                data <- data[, 4]
                datatop10 <- filter(paises, location %in% data)  %>% 
                              filter(date >= input$daterange[1] & date <= input$daterange[2])
                ggplot(data=datatop10,
                       aes(x=date, y=total_deaths, colour=location)) +
                  geom_line() + theme_bw() + geom_point() +
                  theme(legend.position="top",
                        legend.title = element_text(size = 8),
                        legend.text = element_text(size = 6)) + 
                  labs(x="Date", y="Cumulative number of deaths",
                       color="Country")
              }
            }else{
                  if(input$graph == "Number of confirmed cases/deaths each day"){
                  plot_ly(data = result(), x = ~date, y = ~new_deaths, 
                          type = "scatter", mode = 'lines+markers', color = I('aquamarine3')) %>%
                          
                          layout(                        
                                  xaxis = list(           
                                          title = "Date"),       
                                  yaxis = list(           
                                          title = "Number of deaths")     
                          )
                  }else{
                          plot_ly(data = result(), x = ~date, y = ~total_deaths, 
                                  type = "scatter", mode = 'lines+markers', color = I('aquamarine3')) %>%
                                  
                                  layout(                        
                                          xaxis = list(           
                                                  title = "Date"),       
                                          yaxis = list(           
                                                  title = "Cumulative number of deaths")     
                                  )
                          
                  }
            }
                  
          })
          

# Para graficar casos confirmados del mundo (ACUMULADO O NO ACUMULADO) --------
          resultworld <- reactive({
            world <- world %>% filter(date >= input$daterangeworld[1] & date <= input$daterangeworld[2])
            world
          })
          
          output$plot3 <- renderPlotly({
                  if(input$graph2 == "Number of confirmed cases/deaths each day"){
                          plot_ly(data = resultworld(), x = ~date, y = ~new_cases, 
                                  type = "scatter", mode = 'lines+markers', color = I('aquamarine3')) %>%
                                  
                                  layout(                        
                                          xaxis = list(           
                                                  title = "Date"),       
                                          yaxis = list(           
                                                  title = "Number of new confirmed cases")     
                                  )
                  }else{
                          plot_ly(data = resultworld(), x = ~date, y = ~total_cases, 
                                  type = "scatter", mode = 'lines+markers', color = I('aquamarine3')) %>%
                                  
                                  layout(                        
                                          xaxis = list(           
                                                  title = "Date"),       
                                          yaxis = list(           
                                                  title = "Cumulative number of confirmed cases")     
                                  )
                          
                  }
          })
          

# Para graficar muertes del mundo (ACUMULADO O NO ACUMULADO) --------------
          output$plot4 <- renderPlotly({
                  if(input$graph2 == "Number of confirmed cases/deaths each day"){
                          plot_ly(data = resultworld(), x = ~date, y = ~new_deaths, 
                                  type = "scatter", mode = 'lines+markers', color = I('aquamarine3')) %>%
                                  
                                  layout(                        
                                          xaxis = list(           
                                                  title = "Date"),       
                                          yaxis = list(           
                                                  title = "Number of deaths")     
                                  )
                  }else{
                          plot_ly(data = resultworld(), x = ~date, y = ~total_deaths, 
                                  type = "scatter", mode = 'lines+markers', color = I('aquamarine3')) %>%
                                  
                                  layout(                        
                                          xaxis = list(           
                                                  title = "Date"),       
                                          yaxis = list(           
                                                  title = "Cumulative number of deaths")     
                                  )
                          
                  }
                  
          })
          
    

# Tabla con resumen de casos confirmados por paises (NO ACUMULADO) -------------------
          Valuescases <- reactive({
            if(input$country != "Top 5"){
            dataset <- paises[paises$location==input$country,] %>% 
              filter(date >= input$daterange[1] & date <= input$daterange[2]) %>% 
              summarise(Mean = round(mean(new_cases),4),
                        SD = round(sd(new_cases),4),
                        Min = min(new_cases),
                        Q1 = quantile(new_cases, 0.25),
                        Median = median(new_cases),
                        Q3 = quantile(new_cases, 0.75),
                        Max = max(new_cases))
            dataset
            }else if(input$country == "Top 5" & input$graph == "Number of confirmed cases/deaths each day"){
              data <- result()
              data1 <- data[, 1]
              datatop10 <- filter(paises, location %in% data1) %>% 
                filter(date >= input$daterange[1] & date <= input$daterange[2])
              dataset <- datatop10 %>% group_by(location) %>% summarise(tail(new_cases, 1))
              colnames(dataset) <- c("Country", "Number of confirmed cases")
              dataset
            }else if(input$country == "Top 5" & input$graph != "Number of confirmed cases/deaths each day"){
              data <- result()
              data1 <- data[, 3]
              datatop10 <- filter(paises, location %in% data1) %>% 
                filter(date >= input$daterange[1] & date <= input$daterange[2])
              dataset <- datatop10 %>% group_by(location) %>% summarise(tail(total_cases, 1))
              colnames(dataset) <- c("Country", "Cumulative number of confirmed cases")
              dataset
            }
          })
          
# Tabla con resumen de casos confirmados top 5 (ACUMULADO) -------------------            

          
# Para mostrar tabla de casos confirmados por paises ---------------------------------
              output$summary <- renderFormattable({
                if(input$country != "Top 5"){
                  data <- Valuescases()
                  formattable(data, 
                              align =c("c","c","c","c","c","c","c"))
                }else if(input$graph == "Number of confirmed cases/deaths each day" & input$country == "Top 5"){
                  Green = "#71CA97" 
                  Green2 = "#DeF7E9" 
                  data <- Valuescases()
                  formattable(data, 
                              align =c("l","c"), list("Number of confirmed cases"= color_tile(Green2, Green))) 
                }else if(input$country == "Top 5" & input$graph != "Number of confirmed cases/deaths each day"){
                  Green = "#71CA97" 
                  Green2 = "#DeF7E9" 
                  data <- Valuescases()
                  formattable(data, 
                              align =c("l","c"), list("Cumulative number of confirmed cases"= color_tile(Green2, Green))) 
                }
              })
              

# Tabla con resumen de muertes por paises (NO ACUMULADO) -----------------------------
              Valuesdeaths <- reactive({
                if(input$country != "Top 5"){
                dataset <- paises[paises$location==input$country,] %>% 
                            filter(date >= input$daterange[1] & date <= input$daterange[2]) %>%
                            summarise(Mean = round(mean(new_deaths),4),
                                        SD = round(sd(new_deaths),4),
                                        Min = min(new_deaths),
                                        Q1 = quantile(new_deaths, 0.25),
                                        Median = median(new_deaths),
                                        Q3 = quantile(new_deaths, 0.75),
                                        Max = max(new_deaths))
                dataset
                }else if(input$country == "Top 5" & input$graph == "Number of confirmed cases/deaths each day"){
                  data <- result()
                  data1 <- data[, 2]
                  datatop10 <- filter(paises, location %in% data1) %>% 
                    filter(date >= input$daterange[1] & date <= input$daterange[2])
                  dataset <- datatop10 %>% group_by(location) %>% summarise(tail(new_deaths, 1))
                  colnames(dataset) <- c("Country", "Number of deaths")
                  dataset
                }else if(input$country == "Top 5" & input$graph != "Number of confirmed cases/deaths each day"){
                  data <- result()
                  data1 <- data[, 4]
                  datatop10 <- filter(paises, location %in% data1) %>% 
                    filter(date >= input$daterange[1] & date <= input$daterange[2])
                  dataset <- datatop10 %>% group_by(location) %>% summarise(tail(total_deaths, 1))
                  colnames(dataset) <- c("Country", "Cumulative number of deaths")
                  dataset
                }
              })
              
          

# Para mostrar tabla de muertes por paises -------------------------------------------
              output$summary2 <- renderFormattable({
                if(input$country != "Top 5"){
                  data <- Valuesdeaths()
                  formattable(data, 
                              align =c("c","c","c","c","c","c","c"))
                }else if(input$graph == "Number of confirmed cases/deaths each day" & input$country == "Top 5"){
                  Green = "#71CA97" 
                  Green2 = "#DeF7E9"
                  data <- Valuesdeaths()
                  formattable(data, 
                              align =c("l","c"), list("Number of deaths"= color_tile(Green2, Green)))   
                }else if(input$country == "Top 5" & input$graph != "Number of confirmed cases/deaths each day"){
                  Green = "#71CA97" 
                  Green2 = "#DeF7E9"
                  data <- Valuesdeaths()
                  formattable(data, 
                              align =c("l","c"), list("Cumulative number of deaths"= color_tile(Green2, Green))) 
                }
              })

              
# Tabla con resumen de casos confirmados a nivel global (NO ACUMULADO) -------------------
              Valuescasesworld <- reactive({
                dataset <- datos[datos$location=="World",] %>% 
                            filter(date >= input$daterangeworld[1] & date <= input$daterangeworld[2]) %>%
                            summarise(Mean = round(mean(new_cases),4),
                            SD = round(sd(new_cases),4),
                            Min = min(new_cases),
                            Q1 = quantile(new_cases, 0.25),
                            Median = median(new_cases),
                            Q3 = quantile(new_cases, 0.75),
                            Max = max(new_cases))
                dataset
              })
              
              
# Para mostrar tabla de casos confirmados a nivel global ---------------------------------
              output$summary3 <- renderFormattable({
                  data <- Valuescasesworld()
                  formattable(data, 
                              align =c("c","c","c","c","c","c","c"))
              })
              
              
# Tabla con resumen de muertes a nivel global (NO ACUMULADO) -----------------------------
              Valuesdeathsworld <- reactive({
                dataset <- datos[datos$location=="World",] %>% 
                  filter(date >= input$daterangeworld[1] & date <= input$daterangeworld[2]) %>%
                  summarise(Mean = round(mean(new_deaths),4),
                            SD = round(sd(new_deaths),4),
                            Min = min(new_deaths),
                            Q1 = quantile(new_deaths, 0.25),
                            Median = median(new_deaths),
                            Q3 = quantile(new_deaths, 0.75),
                            Max = max(new_deaths))
                dataset
              })
              
              
              
# Para mostrar tabla de muertes a nivel global -------------------------------------------
              output$summary4 <- renderFormattable({
                  data <- Valuesdeathsworld()
                  formattable(data, 
                              align = c("c","c","c","c","c","c","c"))
              })
              

# tablas por ciudades Colombia ----------------------------------------------------
              output$table <- renderFormattable({
                datanew2 <- datacolombia[datacolombia$City==input$city,]
                if(input$table == "Atention"){
                  data <- datanew2 %>% group_by(Atention) %>% 
                    summarise(Frequency = n()) 
                  Green = "#71CA97" 
                  Green2 = "#DeF7E9"
                  formattable(data, 
                              align =c("l","c"), list("Frequency"= color_tile(Green2, Green)))
                }else if(input$table == "Age"){
                  data <- datanew2 %>% 
                    #Creo una variable explícita ad hoc con los grupos. 
                    # No es elegante, pero sirve para validar el paso intermedio.
                    mutate(Age = case_when(between(Age, 0, 9) ~ "0-9",
                                           between(Age, 10, 19) ~ "10-19", 
                                           between(Age, 20, 29) ~ "20-29",
                                           between(Age, 30, 39) ~ "30-39",
                                           between(Age, 40, 49) ~ "40-49",
                                           between(Age, 50, 59) ~ "50-59",
                                           between(Age, 60, 69) ~ "60-69",
                                           between(Age, 70, 79) ~ "70-79",
                                           between(Age, 80, 89) ~ "80-89",
                                           between(Age, 90, 99) ~ "90-99")) %>% 
                    #Agrupo por las categorías de intervalo
                    group_by(Age) %>%
                    #Sumo Resultado dentro de cada grupo
                    summarise(Frequency = n())
                    Green = "#71CA97" 
                    Green2 = "#DeF7E9"
                    formattable(data, 
                              align =c("l","c"),  list("Frequency"= color_tile(Green2, Green)))
                }else if(input$table == "Country of Origin"){
                  data <- datanew2 %>% group_by(CountryOrigin) %>% 
                    summarise(Frequency = n()) 
                  colnames(data) <- c("Country of Origin", "Frequency")
                  Green = "#71CA97" 
                  Green2 = "#DeF7E9"
                  formattable(data, 
                              align =c("l","c"), list("Frequency"= color_tile(Green2, Green)))
                }else if(input$table == "Sex"){
                  data <- datanew2 %>% group_by(Sex) %>% 
                    summarise(Frequency = n()) 
                  Green = "#71CA97" 
                  Green2 = "#DeF7E9"
                  formattable(data, 
                              align =c("l","c"),list("Frequency"= color_tile(Green2, Green)))
                }else if(input$table == "State"){
                  data <- datanew2 %>% group_by(State) %>% 
                    summarise(Frequency = n()) 
                  Green = "#71CA97" 
                  Green2 = "#DeF7E9"
                  formattable(data, 
                              align =c("l","c"),list("Frequency"= color_tile(Green2, Green)))
                }else{
                  data <- datanew2 %>% group_by(Type) %>% 
                    summarise(Frequency = n()) 
                  Green = "#71CA97" 
                  Green2 = "#DeF7E9"
                  formattable(data, 
                              align =c("l","c"), list("Frequency"= color_tile(Green2, Green)))
                }
              })
          
# tablas por departmantos Colombia ----------------------------------------------------
          output$table2 <- renderFormattable({
            datanew2 <- datacolombia[datacolombia$Department==input$department,]
            if(input$table2 == "Atention"){
              data <- datanew2 %>% group_by(Atention) %>% 
                summarise(Frequency = n()) 
              Green = "#71CA97" 
              Green2 = "#DeF7E9"
              formattable(data, 
                          align =c("l","c"), list("Frequency"= color_tile(Green2, Green)))
            }else if(input$table2 == "Age"){
              data <- datanew2 %>% 
                #Creo una variable explícita ad hoc con los grupos. 
                # No es elegante, pero sirve para validar el paso intermedio.
                mutate(Age = case_when(between(Age, 0, 9) ~ "0-9",
                                       between(Age, 10, 19) ~ "10-19", 
                                       between(Age, 20, 29) ~ "20-29",
                                       between(Age, 30, 39) ~ "30-39",
                                       between(Age, 40, 49) ~ "40-49",
                                       between(Age, 50, 59) ~ "50-59",
                                       between(Age, 60, 69) ~ "60-69",
                                       between(Age, 70, 79) ~ "70-79",
                                       between(Age, 80, 89) ~ "80-89",
                                       between(Age, 90, 99) ~ "90-99")) %>% 
                #Agrupo por las categorías de intervalo
                group_by(Age) %>%
                #Sumo Resultado dentro de cada grupo
                summarise(Frequency = n())
              Green = "#71CA97" 
              Green2 = "#DeF7E9"
              formattable(data, 
                          align =c("l","c"),  list("Frequency"= color_tile(Green2, Green)))
            }else if(input$table2 == "Country of Origin"){
              data <- datanew2 %>% group_by(CountryOrigin) %>% 
                summarise(Frequency = n()) 
              colnames(data) <- c("Country of Origin", "Frequency")
              Green = "#71CA97" 
              Green2 = "#DeF7E9"
              formattable(data, 
                          align =c("l","c"), list("Frequency"= color_tile(Green2, Green)))
            }else if(input$table2 == "Sex"){
              data <- datanew2 %>% group_by(Sex) %>% 
                summarise(Frequency = n()) 
              Green = "#71CA97" 
              Green2 = "#DeF7E9"
              formattable(data, 
                          align =c("l","c"),list("Frequency"= color_tile(Green2, Green)))
            }else if(input$table2 == "State"){
              data <- datanew2 %>% group_by(State) %>% 
                summarise(Frequency = n()) 
              Green = "#71CA97" 
              Green2 = "#DeF7E9"
              formattable(data, 
                          align =c("l","c"),list("Frequency"= color_tile(Green2, Green)))
            }else{
              data <- datanew2 %>% group_by(Type) %>% 
                summarise(Frequency = n()) 
              Green = "#71CA97" 
              Green2 = "#DeF7E9"
              formattable(data, 
                          align =c("l","c"), list("Frequency"= color_tile(Green2, Green)))
            }
          })   

# Histogramas Colombia ----------------------------------------------------
              output$plot5 <- renderPlotly({
                if(input$bar == "City"){
                  data <- datacolombia %>% group_by(City) %>% 
                    summarise(Frecuencia = n()) 
                  newdata <- arrange(data, -Frecuencia) 
                  ggplot(data= newdata[1:10,], aes(x = reorder(City, -Frecuencia), y = Frecuencia)) +
                    geom_bar(stat = "identity", fill = "aquamarine3") +  
                    labs(x="City", y="Frequency") + theme_bw() +
                    geom_text(size = 3.5, aes(label = Frecuencia), vjust = -0.5) +
                    theme(axis.text.x = element_text(angle = 45))
                }else if (input$bar == "Department"){
                  data <- datacolombia %>% group_by(Department) %>% 
                    summarise(Frecuencia = n()) 
                  newdata <- arrange(data, -Frecuencia) 
                  ggplot(data=newdata[1:10,], aes(x = reorder(Department, -Frecuencia), y = Frecuencia)) +
                    geom_bar(stat = "identity", fill = "aquamarine3") +  
                    labs(x="Department", y="Frequency") + theme_bw() +
                    geom_text(size = 3.5, aes(label = Frecuencia), vjust = -0.5) +
                    theme(axis.text.x = element_text(angle = 45))
                }else if(input$bar =="Atention"){
                  datacolombia %>% group_by(Atention) %>% 
                    summarise(Frecuencia = n()) %>% 
                  ggplot(aes(x = reorder(Atention, -Frecuencia), y = Frecuencia)) +
                    geom_bar(stat = "identity", fill = "aquamarine3") +  
                    labs(x="Atention", y="Frequency") +  theme_bw() +
                    geom_text(size = 3.5, aes(label = Frecuencia), vjust = -0.5) 
                }else if(input$bar =="Age"){
                  tablaedad <- table.freq(hist(datacolombia$Age, breaks=11, right=FALSE, plot=FALSE))
                  tablaedad
                  
                  # Gráfica de barras con el número de infectados pertenecientes a cada intervalo de edad
                  ggplot(data = tablaedad, aes(x = factor(Upper), y = Frequency)) +
                    geom_bar(stat = "identity", fill = "aquamarine3") +  
                    labs(x = "Age", y ="Frecuency") +  theme_bw() +
                    geom_text(size = 3.5, aes(label = Frequency), vjust = -0.5) +
                    scale_x_discrete(labels = c("0-9", "10-19",
                                                "20-29", "30-39",
                                                "40-49", "50-59", 
                                                "60-69", "70-79", 
                                                "80-89", "90-99",
                                                "100-109"))
                }else if(input$bar =="Country of Origin"){
                  data <- datacolombia %>% group_by(CountryOrigin) %>% 
                              summarise(Frecuencia = n()) 
                    newdata <- arrange(data, -Frecuencia) 
                  ggplot(data= newdata[1:10,], aes(x = reorder(CountryOrigin, -Frecuencia), y = Frecuencia)) +
                    geom_bar(stat = "identity", fill = "aquamarine3") +  
                    labs(x="Country of Origin", y="Frequency") + theme_bw() +
                    geom_text(size = 3.5, aes(label = Frecuencia), vjust = -0.5) +
                    theme(axis.text.x = element_text(angle = 45))
                }else if(input$bar =="Sex"){
                  datacolombia %>% group_by(Sex) %>% 
                    summarise(Frecuencia = n()) %>%
                  ggplot(aes(x = reorder(Sex, -Frecuencia), y = Frecuencia)) +
                    geom_bar(stat = "identity", fill = "aquamarine3") +  
                    labs(x="Sex", y="Frequency") + theme_bw() + 
                    geom_text(size = 3.5, aes(label = Frecuencia), vjust = -0.5) 
                }else if(input$bar =="State"){
                  datacolombia %>% group_by(State) %>% 
                    summarise(Frecuencia = n()) %>%
                    ggplot(aes(x = reorder(State, -Frecuencia), y = Frecuencia)) +
                    geom_bar(stat = "identity", fill = "aquamarine3") +  
                    labs(x="State", y="Frequency") + theme_bw() + 
                    geom_text(size = 3.5, aes(label = Frecuencia), vjust = -0.5)
                }else{
                  datacolombia %>% group_by(Type) %>% 
                    summarise(Frecuencia = n()) %>%
                  ggplot(aes(x = reorder(Type, -Frecuencia), y = Frecuencia)) +
                    geom_bar(stat = "identity", fill = "aquamarine3") +  
                    labs(x="Type", y="Frequency") + theme_bw() +
                    geom_text(size = 3.5, aes(label = Frecuencia), vjust = -0.5) 
                }
              })

# MAPA DE CALOR COLOMBIA --------------------------------------------------
              datosmapa <- reactive({
                datacolombia <- datacolombia %>% 
                  mutate(Department = replace(Department, Department == "BARRANQUILLA D.E.",
                                                             "ATLÁNTICO")) %>% 
                  mutate(Department = replace(Department, Department == "CARTAGENA D.T. Y C",
                                                             "BOLÍVAR")) %>% 
                  mutate(Department = replace(Department, Department == "SANTA MARTA D.T. Y C.",
                                                             "MAGDALENA")) %>% 
                  mutate(Department = replace(Department, Department == "BUENAVENTURA D.E.",
                                                             "VALLE DEL CAUCA")) %>%
                  filter(Atention != "Recovered")
                
                
                datadepartamentos <- datacolombia  %>% group_by(Department) %>% summarise(Frecuencia = n())
                colnames(datadepartamentos) <- c("Department", "Frequency")
                
                datamap <- dep %>% left_join(datadepartamentos, by="Department")
                
                datamap <- datamap %>% mutate(Frequency = replace(Frequency, which(is.na(Frequency)), 0))
                
                datamap
              })
              
              output$heatmap <- renderPlot({
                gg <- colmap(map = departamentos, data = datosmapa(), var = "Frequency")
                gg +   scale_fill_continuous(low = "#e0f3db", high = "#43a2ca") + theme_void() +   
                  guides(fill=guide_legend(title="Confirmed Cases of COVID-19"))
              })


# MAPA DE CALOR MUNDO -----------------------------------------------------

              mapworld <- reactive({
                paises <- paises %>% 
                  mutate(location = replace(location, location == "United States",
                                            "USA")) %>% 
                  mutate(location = replace(location, location == "United Kingdom",
                                            "UK")) %>% 
                  mutate(location = replace(location, location == "Congo",
                                            "Republic of Congo")) %>% 
                  mutate(location = replace(location, location == "Democratic Republic of Congo",
                                            "Democratic Republic of the Congo"))
                
                data <- paises %>% group_by(location) %>% summarise(tail(total_cases, 1),
                                                                    tail(total_deaths, 1))
                colnames(data) <- c("region", "Confirmed Cases of COVID-19", "Deaths by COVID-19")
                
                world_data <- map_data('world')
                world_data <- fortify(world_data)
                #head(world_data)
                
                #world_data %>% group_by(region) %>% summarise(frec = n())
                
                datosnuevo <- data %>% left_join(world_data, by="region")
                datosnuevo
              })
              
              output$heatmapworld1 <- renderPlot({
                ggplot() + 
                  geom_polygon_interactive(data = mapworld(), color = 'gray70', size = 0.1,
                                           aes(x = long, y = lat, fill = `Confirmed Cases of COVID-19`, group = group)) +
                  scale_fill_gradientn(colours = brewer.pal(5, "GnBu"), na.value = 'grey') + 
                  scale_y_continuous(limits = c(-60, 90), breaks = c()) + 
                  scale_x_continuous(breaks = c()) + 
                  theme_void()
              })
              
              
              output$heatmapworld2 <- renderPlot({
                ggplot() + 
                  geom_polygon_interactive(data = mapworld(), color = 'gray70', size = 0.1,
                                           aes(x = long, y = lat, fill = `Deaths by COVID-19`, group = group)) +
                  scale_fill_gradientn(colours = brewer.pal(5, "GnBu"), na.value = 'grey') + 
                  scale_y_continuous(limits = c(-60, 90), breaks = c()) + 
                  scale_x_continuous(breaks = c()) + 
                  theme_void()
              })
              
              output$mytableworld = DT::renderDataTable({
                datos
              })
              
              output$mytablecolombia = DT::renderDataTable({
                datacolombia
              })
              
              


}# END SERVER
# END SHINY
  
shinyApp(ui = ui, server = server)


