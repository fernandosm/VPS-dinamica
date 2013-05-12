# file name: server.R
#
# Purpose: 
#    Executa o modelo SIS.
# 
#  Record of revisions:
#      Date        Programmer          Description of change
#    13/05/05     M.S. Fernando     Documentação e criação do script
# 
#  inputs:
#    b:        taxa de contatos potencialmente infectantes
#    g:        taxa de retorno de indivíduos infectados ao compartimento de suscetíveis
#    S0:       número (ou densidade) inicial de indivíduos suscetíveis (no tempo t=0)
#    I0:       número (ou densidade) inicial de indivíduos infectados (no tempo t=0)
#    tempoSim: tempo de simulação
#    grafico:  lógico, cria ou não gráfico
#
#  outputs:
#    S(t): número (ou densidade) de indivíduos suscetíveis no instante t
#    I(t): número (ou densidade) de indivíduos infectados no instante t
#
#  Other argumnts & variables:
#    SISmodel: função que carrega modelo
#    parmsODE: armazena os parâmetros
#    times:    vetor com os tempos que retornarão com o resultado da simulação
#    yini:     vetor que armazena os valores iniciais

library(shiny)
library(deSolve)
library(ggplot2)
library(reshape)

sisModel <- function(b,g,So,Io,tempoSim){
  
  #### modelo Suscetível-Infectado-Suscetível ####
  SISmodel <- function(Time, State, Pars) {
    with(as.list(c(State, Pars)), {
      dS <- -b*S*I + g*I;
      dI <- b*S*I - g*I;
      return(list(c(dS, dI)))
    })
  }
  
  #### Parâmetros do Modelo e da Simução Numérica ####
  parmsODE <- c(b=b,g=g)
  times <- seq(0, tempoSim, by = tempoSim/100)
  
  #### Condição Inicial ####
  yini <- c(S = So, I=Io)
  
  #### Simulação Numérica ####
  outSISmodel<- as.data.frame(ode(yini, times, SISmodel, parmsODE,method="ode45"))
  names(outSISmodel) <- c('Tempo','Suscetíveis','Infectados')
  
  #### Transformação dos dados para plotagem ####
  outSIS <- melt(as.data.frame(outSISmodel),id="Tempo")
  
  #### Plotanto os resultados ####
  graf <- ggplot(outSIS,aes(x=Tempo,y=value,colour=variable,group=variable)) + geom_line(size=1.1) +
    ggtitle("Dinâmica SIS") +
    xlab("Tempo") + ylab("Número de Animais") + labs(colour = "População") +
    theme(text=element_text(size=20));
  return(graf)
}

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  output$simPlot <- renderPlot({
    graf <- sisModel(input$b,input$g,input$So,input$Io,input$tempoSim)
    plot(graf)
  })
})