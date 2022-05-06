####### BIBLIOTECAS #######
library("shiny")
library("shinythemes")
library("dplyr")
library("lubridate")
library("plotly")
library("data.table")

options(scipen = 999)

texto_intro <- HTML(paste0("<font color=\"#696969\"><font size=\"3\"> Aplicativo desenvolvido por <b>Cassiano Ricardo Dalberto</b>",  
                           " para a aula de Sistemas de Amortização, da disciplina de Matemática Financeira Aplicada",
                           " à Contabilidade (CCN6004) - UFSC, 06/05/2022</font></font>"))


############## UI ##############

ui <- fluidPage(theme = shinytheme("cerulean"),
                
                
                titlePanel(fluidRow(
                    column(12, "Sistemas de Amortização"),
                    column(12,  texto_intro))),
                
                
                navbarPage('',
                           
                           tabPanel('Gerar tabela e gráfico',
                                    
                                    # Painel lateral         
                                    sidebarLayout(
                                        sidebarPanel(
                                            
                                            radioButtons("tipo",
                                                         "Escolha o Sistema",
                                                         choices = c('SAC','SPC'),
                                                         selected = 'SAC'),
                                            
                                            numericInput("principal",
                                                         "Valor do Principal:",
                                                         10000),
                                            
                                            numericInput("taxa",
                                                         "Taxa de Juros:",
                                                         1.005),
                                            
                                            numericInput("termo",
                                                         "Nº de Períodos",
                                                         12),
                                            
                                            br(), 
                                            actionButton("do", "Aplicar!"),
                                            br(), 
                                            
                                            width = 2
                                            
                                        ),
                                        
                                        # Painel central
                                        mainPanel(
                                            
                                            fluidRow(
                                                column(5,DT::dataTableOutput("tabela")),
                                                column(5, plotlyOutput("pizza"),
                                                       htmlOutput("texto"), align="center")
                                            )
                                            
                                            
                                            
                                            ,
                                            width = 10
                                        )
                                    )
                                    
                           ),    # end tab 1
                           
                           
                           tabPanel("Recursos adicionais e código",
                                    fluidPage(
                                        br(),
                                        includeMarkdown("recursos.Rmd")
                                    ) # 
                                    
                           ),
                           
                           tabPanel("Exercício",
                                    fluidPage(
                                        br(),
                                        includeMarkdown("exercicio.Rmd")
                                    ) # 
                                    
                           )
                           
                ) # end navbarpage
)

############## SERVER ##############

server <- function(input, output) {
    
    
    spc_function = function(valor_principal, termo, taxa) {
        
        fpv = (1 - ((1 + taxa) ^ -(termo)))/taxa # Fator de valor presente
        pmt = valor_principal / fpv # prestação
        
        # initializar variáveis
        interest = principal = payment = balance = vector("numeric", termo)
        
        # calcular tabela
        outstanding_principal = valor_principal
        for (i in 1:termo) {
            
            intr = outstanding_principal * taxa
            if(outstanding_principal < payment[i]){
                prnp = outstanding_principal
                outstanding_principal = 0
                final = TRUE
                
            }else{
                
                prnp = pmt - intr
                outstanding_principal = outstanding_principal - prnp
                final = FALSE
            }
            
            interest[i]  = round(intr,2)
            principal[i] = round(prnp,2)
            payment[i] = round((prnp + intr),2)
            balance[i] = round(outstanding_principal,2)
        }
        
        data.frame('Período' = 1:termo, 'Juros' = interest, 'Amortização' = principal, 'Prestação' = payment, 'Saldo Devedor' = balance,
                   check.names = F) 
    }
    
    sac_function = function(valor_principal, termo, taxa) {
        
        amort_const = (valor_principal / termo) # amortização
        
        # inicializar variáveis
        interest = principal = payment = balance = vector("numeric", termo)
        
        # calcular tabela
        outstanding_principal = valor_principal
        for (i in 1:termo) {
            
            intr = amort_const * (termo - i + 1)*taxa
            
            if(outstanding_principal < payment[i]){
                prnp = outstanding_principal
                outstanding_principal = 0
                final = TRUE
                
            }else{
                
                prnp = amort_const
                outstanding_principal = outstanding_principal - prnp
                final = FALSE
            }
            
            interest[i]  = round(intr,2)
            principal[i] = round(prnp,2)
            payment[i] = round((intr + amort_const),2)
            balance[i] = round(outstanding_principal,2)
        }
        
        data.frame('Período' = 1:termo, 'Juros' = interest, 'Amortização' = principal, 'Prestação' = payment, 'Saldo Devedor' = balance, 
                   check.names = F) 
    }
    
    # Botão de ação
    tabela_data <- eventReactive(input$do, {  
        
        # Calcular tabela segundo tipo de sistema
        if (input$tipo == "SPC"){
            
            tab_amort <- spc_function(valor_principal = input$principal, termo = input$termo, taxa = input$taxa/100)
            
            return(tab_amort)
            
        }else{
            
            tab_amort <- sac_function(valor_principal = input$principal, termo = input$termo, taxa = input$taxa/100)
            
            return(tab_amort)
        }
    })
    
    pizza_data <- eventReactive(input$do, {  
        
        # Gráfico segundo tipo de sistema
        if (input$tipo == "SPC"){
            
            tab_amort <- spc_function(valor_principal = input$principal, termo = input$termo, taxa = input$taxa/100)
            name = c("Principal", "Juros")
            start_bal = input$principal
            final_bal = tab_amort$Balance[input$termo]
            principal = sum(tab_amort$Amortização)
            interest = sum(tab_amort$Juros)
            values = c(principal, interest )
            dat = data.frame(cbind(name, values))
            dat$values <- as.numeric(as.character(dat$values))
            total <- sum(dat$values)
            dat$percents <- 100*(dat$values / total)
            dat = dat %>% dplyr::arrange(desc(percents)) 
            
            return(dat)
            
        }else{
            
            tab_amort <- sac_function(valor_principal = input$principal, termo = input$termo, taxa = input$taxa/100)
            name = c("Principal", "Juros")
            start_bal = input$principal
            final_bal = tab_amort$Balance[nrow(tab_amort)]
            principal = sum(tab_amort$Amortização)
            interest = sum(tab_amort$Juros)
            values = c(principal, interest )
            dat = data.frame(cbind(name, values))
            dat$values <- as.numeric(as.character(dat$values))
            total <- sum(dat$values)
            dat$percents <- 100*(dat$values / total)
            dat = dat %>% dplyr::arrange(desc(percents)) 
            
            return(dat)
        }
    })
    
    # Gerar tabela após o botão ser clicado
    output$tabela <- DT::renderDataTable(
        DT::datatable(
            tabela_data(),
            rownames = F,
            extensions = 'Buttons',
            options = list(
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'),
                pageLength = 24,
                searching = F,
                ordering = F,
                digits = 2,
                dom = 'B',
                buttons = list(
                    list(extend = 'copy',
                         text = "Copiar"), 
                    'csv', 'excel', 'pdf')
            )
            
        ) %>%
            DT::formatRound(columns=c('Juros', 'Amortização', 'Prestação', 'Saldo Devedor'), digits=2)
        
        
        
    )
    
    output$pizza <- renderPlotly({
        
        data <- tibble(pizza_data())
        fig <- plot_ly(data, labels = ~name, values = ~values, marker = list(colors = c('#7259C4', '#1EC672')), type = 'pie') 
        fig <- fig %>% layout(title = paste0(input$tipo, ' - Detalhes', sep=""),
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
            plotly::config(locale = 'pt-BR')
        
        
        
    })
    
    output$texto <- renderText({ 
        dado_texto <- tibble(pizza_data())
        HTML(paste0("<b><font color=\"#696969\"><font size=\"4\">Valor Total: ", sum(dado_texto$values), "</font></font></b>"))
        
    })
}

###### RODAR ######
shinyApp(ui = ui, server = server)