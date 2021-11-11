library(shinythemes)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(formattable)
library(tidyverse)
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
lista <- c("World", "International", "High income", "Lower income", "Upper middle income", "Lower middle income")
paises <- datos %>% filter(!location %in% lista)
mundo <- datos %>% filter(location == "World")



# UI ----------------------------------------------------------------------
ui = tagList(
    navbarPage(
        theme = shinytheme("flatly"),  # <--- To use a theme, uncomment this
        "The COVID-19 pandemic",
        tabPanel("COVID-19 around the world",
                 sidebarPanel(
                     selectInput(inputId = "country",
                                 label = "Country of interest",
                                 choices = c("Top 5", unique(paises$location)),
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
        tabPanel("Data",
                 DT::dataTableOutput("mytableworld")
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
                   )
                   
                 )
                 
        ) 
    
    )

server = function(input, output, session) {
        

# Para trabajar con datos del país seleccionado ---------------------------
        result <- reactive({
          if(input$country == "Top 5"){
            lista <- c("Africa", "Asia", "Europe", "European Union",
                       "North America", "Oceania", "South America")
            data <- paises %>% filter(!location %in% lista)
            data <- data %>% group_by(location) %>% summarise(tail(new_cases, 1),
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
                       geom_line() + theme_bw() + 
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
                  geom_line() + theme_bw() + 
                  theme(legend.position="top", 
                        legend.title = element_text(size = 8),
                        legend.text = element_text(size = 6)) + 
                  labs(x="Date", y="Cumulative number of confirmed cases",
                       color="Country")
              }
            }else{
               if(input$graph == "Number of confirmed cases/deaths each day"){
               plot_ly(data = result(), x = ~date, y = ~new_cases, 
                       type = "scatter", mode = 'lines', color = I('aquamarine3')) %>%
                        
                        layout(                        
                                xaxis = list(           
                                        title = "Date"),       
                                yaxis = list(           
                                        title = "Number of new confirmed cases")     
                        )
               }else{
                       plot_ly(data = result(), x = ~date, y = ~total_cases, 
                               type = "scatter", mode = 'lines', color = I('aquamarine3')) %>%
                               
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
                geom_line() + 
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
                geom_line() + theme_bw() + 
                theme(legend.position="top",
                      legend.title = element_text(size = 8),
                      legend.text = element_text(size = 6)) + 
                labs(x="Date", y="Cumulative number of deaths",
                     color="Country")
            }
          }else{
                if(input$graph == "Number of confirmed cases/deaths each day"){
                plot_ly(data = result(), x = ~date, y = ~new_deaths, 
                        type = "scatter", mode = 'lines', color = I('aquamarine3')) %>%
                        
                        layout(                        
                                xaxis = list(           
                                        title = "Date"),       
                                yaxis = list(           
                                        title = "Number of deaths")     
                        )
                }else{
                        plot_ly(data = result(), x = ~date, y = ~total_deaths, 
                                type = "scatter", mode = 'lines', color = I('aquamarine3')) %>%
                                
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
          world <- mundo %>% filter(date >= input$daterangeworld[1] & date <= input$daterangeworld[2])
          world
        })
        
        output$plot3 <- renderPlotly({
                if(input$graph2 == "Number of confirmed cases/deaths each day"){
                        plot_ly(data = resultworld(), x = ~date, y = ~new_cases, 
                                type = "scatter", mode = 'lines', color = I('aquamarine3')) %>%
                                
                                layout(                        
                                        xaxis = list(           
                                                title = "Date"),       
                                        yaxis = list(           
                                                title = "Number of new confirmed cases")     
                                )
                }else{
                        plot_ly(data = resultworld(), x = ~date, y = ~total_cases, 
                                type = "scatter", mode = 'lines', color = I('aquamarine3')) %>%
                                
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
                                type = "scatter", mode = 'lines', color = I('aquamarine3')) %>%
                                
                                layout(                        
                                        xaxis = list(           
                                                title = "Date"),       
                                        yaxis = list(           
                                                title = "Number of deaths")     
                                )
                }else{
                        plot_ly(data = resultworld(), x = ~date, y = ~total_deaths, 
                                type = "scatter", mode = 'lines', color = I('aquamarine3')) %>%
                                
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
  
            
            


}# END SERVER
# END SHINY
  
shinyApp(ui = ui, server = server)


