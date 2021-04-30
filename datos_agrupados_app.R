library(shiny)

# Definir la parte UI del archivo
ui <- fluidPage(

  # Se selecciona el título de la interfaz
    titlePanel("Hacks para datos agrupados"),

  # Generacion de un conjunto de pestañas 
    tabsetPanel(
        
      # Primera pestaña
        tabPanel('El magi número',
                 
          # Se genera interfaz que contiene entrada y salida
            sidebarLayout(
                
              # Parte de la entrada de interfaz 1
                sidebarPanel(
                    
                           # Se especifica el tipo de entrada, su codigo interno y como aparece
                             numericInput(inputId = 'na',
                                          label = 'Tamaño de la muestra',
                                          value = 0),
                         
                             textInput(inputId = 'opcion',
                                       label = 'Escoja un cálculo',
                                       value = 'Cuartil'),
                            
                             numericInput(inputId = 'numa',
                                          label = 'Número auxiliar (num)',
                                          value = 0)
                
                            ),
              
              # Parte de la salida de la interfaz 1
                mainPanel(textOutput('caption1'))
            
            
            )

            
            ),
        
      # Segunda pestaña 
        tabPanel('Fórmulas varias',
            sidebarLayout(
              
              # Parte de la entrada de la interfaz 2
                sidebarPanel(width = 2/3, # Para modificar el ancho de la interfaz
                    fluidRow(
                        column(2,
                               
                               numericInput(inputId = 'Icb', 
                                            label = 'Intervalo de clase', 
                                            value = 0),
                               
                               numericInput(inputId = 'Lib',
                                            label = 'Limite inferior', 
                                            value = 0)
                               
                        ),
                        
                        column(3,
                               
                               numericInput(inputId = 'fib',
                                            label = 'freq absoluta', 
                                            value = 0),
                               
                               numericInput(inputId = 'Fantb',
                                            label = 'Freq acumuluda anterior', 
                                            value = 0)
                               
                        ),
                        
                        column(4,
                               
                               numericInput(inputId = 'numb',
                                            label = 'num (Q, Dec, Perc, Med)', 
                                            value = 0),
                               
                               numericInput(inputId = 'nb',
                                            label = 'Tamaño de la muestra', 
                                            value = 0)
                               
                        )
                    ),
                    
                    textInput(inputId = 'opciones',
                              label = 'Escoge un cálculo',
                              value = 'Percentil')
                ),
            
          # Parte de salida de la interfaz 2
            mainPanel(textOutput("caption2"))    
                
            )
            
                
            ),
        
      # Tercera pestaña
        tabPanel('Ecuación de la moda',
           sidebarLayout(
             
             # Parte de la entrada de la interfaz 3
               sidebarPanel(width = 0.45,
                   fluidPage(
                       column(3,
                              
                              numericInput(inputId = 'Lic',
                                           label = 'Límite inferior',
                                           value = 0),
                              
                              numericInput(inputId = 'Icc',
                                           label = 'Intervalo de clase',
                                           value = 0),
                              
                              numericInput(inputId = 'fic',
                                           label = 'Freq absoluta',
                                           value = 0) 
                              
                       ),
                       
                      column(4,
                             
                             numericInput(inputId = 'fantc',
                                          label = 'Freq absoluta acumulada anterior',
                                          value = 0),
                             
                             numericInput(inputId = 'fdep',
                                          label = 'Freq absoluta acumulada posterior',
                                          value = 0)
                             
                      )
                   
                   
                   
                       
                   )
                   
               ),
             
             # Parte de salida de la interfaz 3
               mainPanel(textOutput('caption3'))
               
           ) 
        )
            

        
    )
    
)


# Se define la parter server que genera los procedimientos

  server <- function(input, output) {
  
  # Se específica el procedimiento de la pestaña 2
    output$caption2 <- renderPrint({
      
      # Codigo de funcion de pestaña 2
        per_dec <- function(n, fi, Li, Ic, Fant, num, calculo = 'Percentil'){
            
          # Condiciones inciales para entregar un buen resultado
            if(calculo != 'Mediana' & calculo != 'Cuartil' & calculo != 'Decil' & calculo != 'Percentil') stop("calculo debe ser 'Mediana', o 'Cuartil', o 'Decil', o 'Percentil'")
            if(calculo == 'Mediana'){
                if(num != 1) stop("El valor de 'num' admitido para cálculo = 'Mediana' es 1")
            }  
            if(calculo == 'Decil'){
                if(num <= 0 | num >= 10) stop("los valores de 'num' admitidos para calculo = 'Decil' es entre 0 y 10")
            }
            if(calculo == 'Percentil'){
                if(num <= 0 | num >= 100) stop("los valores de 'num' admitidos para calculo = 'Percentil' es entre 0 y 100")
            }
            if(calculo == 'Cuartil'){
                if(num != 1 & num != 2 & num != 3) stop("Para calculo = 'Cuartil' los 'num' admitidos son 1 (Q1), 2 (Q2) y 3 (Q3)")
            }
            
            c <- n * num
            switch(calculo,
                   'Decil' = co <- c/10,
                   'Percentil'=  co <- c/100,
                   'Cuartil' = co <- c/4,
                   'Mediana' = co <- c/2
            )
            tot <- Li + ((co-Fant)/(fi)) * Ic
            round(tot, 2)
        }
      
      # Ejecucion interna del comando
        res <- per_dec(n = input$nb, fi = input$fib, Li = input$Lib, Ic = input$Icb, 
                       Fant = input$Fantb, num = input$numb, calculo = input$opciones)
        
        if(input$opciones == 'Mediana'){
            paste('El valor de la', input$opciones, 'es de:', res)
        } else(paste('El valor del', input$opciones, input$numb, 'es de:', res))
        
    })
    
  # Se específica el procedimiento de la pestaña 1
    output$caption1 <- renderPrint({
        
        magic_number <- function(n, calculo = 'Cuartil', num){
            if(calculo != 'Mediana' & calculo != 'Cuartil' & calculo != 'Decil' & calculo != 'Percentil') stop("calculo debe ser 'Mediana', o 'Cuartil', o 'Decil', o 'Percentil'")
            if(calculo == 'Mediana'){
                if(num != 1) stop("El valor de 'num' admitido para calculo = 'Mediana' es 1")
            }  
            if(calculo == 'Decil'){
                if(num <= 0 | num >= 10) stop("los valores de 'num' admitidos para calculo = 'Decil' es entre 0 y 10")
            }
            if(calculo == 'Percentil'){
                if(num <= 0 | num >= 100) stop("los valores de 'num' admitidos para calculo = 'Percentil' es entre 0 y 100")
            }
            if(calculo == 'Cuartil'){
                if(num != 1 & num != 2 & num != 3) stop("Para calculo = 'Cuartil' los 'num' admitidos son 1 (Q1), 2 (Q2) y 3 (Q3)")
            }
          
            c <- n * num
            switch(calculo,
                   'Decil' = co <- c/10,
                   'Percentil'=  co <- c/100,
                   'Cuartil' = co <- c/4,
                   'Mediana' = co <- c/2)
          
            co
        }
        
        opci <- input$opcion
        resula <- magic_number(n = input$na, calculo = opci, num = input$numa)
        paste('Tu magi numero es:', resula)
        
    })
  
  # Se específica el procedimiento de la pestaña 3
    output$caption3 <-  renderPrint({
        
        Moda <- function(Li, Ic, fi, fant, fdep){
            del1 <- fi - fant
            del2 <- fdep - fant
            y <- Li+((del1)/(del1+del2))*Ic
            round(y,2)
        }
        
        mo <- Moda(Li = input$Lic, Ic = input$Icc, fi = input$fic, fant = input$fantc, fdep = input$fdep)
        paste('El resultado de la moda es', mo)
        
    })
    
}

# Se ejecuta la aplicacion
  shinyApp(ui = ui, server = server)
