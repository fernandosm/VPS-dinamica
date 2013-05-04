# file name: sisModel.R
#
# Purpose: 
#    Executa simulação numérica do modelo SIS.
# 
#  Record of revisions:
#      Date        Programmer          Description of change
#    13/04/02     M.S. Fernando     Documentação e criação do script
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
#
#

sisModel <- function(b=9e-3,g=5e-1,So=100,Io=1,tempoSim=100,grafico=TRUE){
  
  #### Bibliotecas Necessárias ####
  library(deSolve)
  library(ggplot2)
  library(reshape)
  
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
  
  if (grafico==T){
    
    #### Transformação dos dados para plotagem ####
    outSIS <- melt(as.data.frame(outSISmodel),id="Tempo")
    
    #### Plotanto os resultados ####
    graf <- ggplot(outSIS,aes(x=Tempo,y=value,colour=variable,group=variable)) + geom_line(size=1.1) +
      ggtitle(paste("Dinâmica SIS\nb=",b," g=",g,sep=" " )) +
      xlab("Tempo") + ylab("Número de Animais") + labs(colour = "População") +
      theme(text=element_text(size=20));
    plot(graf)
  }
  return(outSISmodel)
}

