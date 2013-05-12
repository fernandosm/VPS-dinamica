# file name: server.R
#
# Purpose: 
#    Executa simulação numérica do modelo SLI.
# 
#  Record of revisions:
#      Date        Programmer          Description of change
#    13/05/11     M.S. Fernando     Documentação e criação do script
# 
#  inputs:
#
#   a:         taxa de natalidade média (ano-1)
#   b:         taxa de contatos potencialmente infecciosos (km2 ano-1)
#   m:         taxa de mortalidade natural média (ano-1)
#   K:         capacidade de suporte do meio (animais por km2)
#   alfa:      taxa de mortalidade pela raiva (letalidade) (ano-1)
#   sigma:     inverso do período médio de latência (ano-1)
#
#  outputs:
#    S(t): densidade de indivíduos suscetíveis no instante t
#    L(t): densidade de indivíduos infectados que ainda não são infecciosos no instante t
#    I(t): densidade de indivíduos infectados no instante t
#
#  Other argumnts & variables:
#    SLImodel: função que carrega modelo
#    parmsODE: armazena os parâmetros
#    times:    vetor com os tempos que retornarão com o resultado da simulação
#    yini:     vetor que armazena os valores iniciais

#### Bibliotecas Necessárias ####
library(deSolve)
library(ggplot2)
library(reshape)

sliModel <- function(a,b,m,K,alfa,sigma,So,Lo,Io,tempoSim){
  
  #### modelo Suscetível-Infectado-Suscetível ####
  SLImodel <- function(Time, State, Pars) {
    with(as.list(c(State, Pars)), {
      
      dS <- (a - m)*S - ((a-m)/K)*S*(S+L+I) -b*S*I
      dL <- b*S*I - (sigma + m + ((a-m)/K)*(S+L+I))*L
      dI <- sigma*L - (alfa + m + ((a-m)/K)*(S+L+I))*I
      
      return(list(c(dS, dL, dI)))
    })
  }
  
  #### Parâmetros do Modelo e da Simução Numérica ####
  parmsODE <- c(a=a,b=b,m=m,K=K,alfa=alfa,sigma=sigma)
  times <- seq(0, tempoSim, by = tempoSim/100)
  
  #### Condição Inicial ####
  yini <- c(S=So, L=Lo, I=Io)
  
  #### Simulação Numérica ####
  outSLImodel<- as.data.frame(ode(yini, times, SLImodel, parmsODE,method="ode45"))
  names(outSLImodel) <- c('Tempo','Suscetíveis','Latentes','Infectados')
  
  #### Transformação dos dados para plotagem ####
  outSLI <- melt(as.data.frame(outSLImodel),id="Tempo")
  
  #### Plotanto os resultados ####
  graf <- ggplot(outSLI,aes(x=Tempo,y=value,colour=variable,group=variable)) + geom_line(size=1.1) +
    ggtitle(paste("Dinâmica SLI\na=",a," b=",b," m=",m,"K=",K,"alfa=",alfa,sep=" ")) +
    xlab("Tempo") + ylab("Densidade de Animais") + labs(colour = "População") +
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
    graf <- sliModel(input$a,input$b,input$m,input$K,input$alfa,input$sigma,
                     input$So,input$Lo,input$Io,input$tempoSim)
    plot(graf)
  })
})