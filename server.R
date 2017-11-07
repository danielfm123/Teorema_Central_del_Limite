library(shiny)
library(ggplot2)
library(reshape2)

shinyServer(function(input, output,session) {
  
    parametros = reactive({
      c(input$par1,input$par2)
    })
  
    get_funcion = function(tipo){
      #       d = derivada
      #       p = acumulada
      #       q = alfa
      #       r = aleatorio
      funcion = eval(parse(text = paste(tipo,input$distribucion,sep="")))
      function(x,par){
        switch (input$distribucion,
          norm = funcion(x,par[1],par[2]),
          exp = funcion(x,par[1]),
          t = funcion(x,par[1]),
          cauchy = funcion(x,par[1],par[2]),
          chisq = funcion(x,par[1],par[2]),
          binom = funcion(x,par[1],par[2])
        )
      }
    }
    
    output$parametros = renderUI({
      switch (input$distribucion,
              norm = list(numericInput("par1","esperanza",0),
                          numericInput("par2","desvest",1,min = 0)),
              exp =  list(numericInput("par1","tasa",1,min = 0)),
              t =    list(numericInput("par1","grados de livertad",10,min = 1,step = 1)),
              cauchy = list(numericInput("par1","location",0),
                            numericInput("par2","escala",1,min = 0)),
              chisq =  list(numericInput("par1","gragdos de libertad",10,min = 0,step = 1),
                            numericInput("par2","ncp",1,min = 0)),
              binom = list(numericInput("par1","size",1),
                           numericInput("par2","tasa",0.5))
      )
    })
    
    observe({
      input$distribucion
      minimo = tryCatch(max(-100,get_funcion("q")(.001,parametros())),error = function(e) {0})
      maximo = tryCatch(max(-100,get_funcion("q")(.999,parametros())),error = function(e) {1})
      updateSliderInput(session,"rango",value = c(minimo,maximo))
    })
    
    output$grafico_dist = renderPlot({
      if(input$pestana == "distribucion"){
        valores_x = seq(from = input$rango[1], to = input$rango[2], length.out = 201)
        valores = data.frame(x = valores_x,
                             derivada = get_funcion("d")(valores_x,parametros()),
                             acumulada = get_funcion("p")(valores_x,parametros())
        )
        h <- ggplot(valores, aes(x=x))
        h = h + geom_area(aes(y = acumulada), fill = "grey")
        h = h + geom_line(aes(y = derivada), color = "blue", size = 2)
        return(h)
      }
    })
  
    muestra = reactive({
      input$distribucion
      input$reejecutar
      matrix(get_funcion("r")(input$tamano_muestra*input$experimentos,parametros()),ncol = input$experimentos)
    })
    
    promedios = reactive({
      data.frame(promedio = apply(muestra(),2,mean))
    })
    
    desvest = reactive({
      data.frame(promedio = apply(muestra(),2,sd))
    })
  
    output$tabla_indicadores = renderTable({
      tabla = data.frame(promedio = mean(promedios()$promedio),
                         desvest = sd(promedios()$promedio),
                         prom_desvest = mean(desvest()$promedio),
                         minimo = min(promedios()$promedio),
                         quantil_01 = quantile(promedios()$promedio,.01),
                         quantil_05 = quantile(promedios()$promedio,.05),
                         quantil_25 = quantile(promedios()$promedio,.25),
                         mediana = quantile(promedios()$promedio,.5),
                         quantil_75 = quantile(promedios()$promedio,.75),
                         quantil_95 = quantile(promedios()$promedio,.95),
                         quantil_99 = quantile(promedios()$promedio,.99),
                         maximo = max(promedios()$promedio)
      )
      tabla = melt(tabla,value.name = "Valor",variable.name="Indicador")
      return(tabla)
    })
    
    output$grafico_histograma = renderPlot({
      h = ggplot(promedios(),aes(x = promedio))
      h = h + geom_histogram(aes(y = ..density..), fill = "grey")
      h = h + geom_density( alpha = .3, fill = "blue", size = 0)
      return(h)
    })
})
