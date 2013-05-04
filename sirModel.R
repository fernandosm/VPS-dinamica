# file name: sirModel.R
#
# Purpose: 
#    Executa simulação numérica do modelo SIR.
# 
#  Record of revisions:
#      Date        Programmer          Description of change
#    13/04/02     M.S. Fernando     Documentação e criação do script
# 
#  inputs:
#    b:        taxa de contatos potencialmente infectantes
#    g:        taxa de recuperação = inverso do período infeccioso
#    m:        taxa de natalidade = taxa de mortalidade
#    vo:       taxa de vacinação constante
#                (essa é uma taxa relacionada à proporção de cobertura vacinal,
#                cujo valor numérico é de difícil interpretação)
#    t1 e t2:  intervalo de vacinação entre t1 e t2
#    S0:       número (ou densidade) inicial de indivíduos suscetíveis (no tempo t=0)
#    I0:       número (ou densidade) inicial de indivíduos infectados (no tempo t=0)
#    tempoSim: tempo de simulação
#    grafico:  lógico, cria ou não o gráfico
#
#  outputs:
#    S(t): número de indivíduos suscetíveis no instante t
#    I(t): número de indivíduos infectados no instante t
#    R(t): número de indivíduos recuperados no instante t
#
#  Other argumnts & variables:
#    SIRmodel: função que carrega modelo
#    parmsODE: armazena os parâmetros
#    times:    vetor com os tempos que retornarão com o resultado da simulação
#    yini:     vetor que armazena os valores iniciais
#
#
#  Referência
# Amaku M, Coutinho FAB, Azevedo RS, Burattini MN, Lopez LF, Massad E. Vaccination against rubella:
#  analysis of the temporal evolution of the age-dependent force of infection and the effects of
#  different contact patterns. Physical Review E 2003; 67 (051907):1-11.

sirModel <- function(b=5e-3,g=1e-2,vo=0,m=15e-2,t1=20,t2=50,
                     So=100,Io=1,Ro=0,tempoSim=100,grafico=TRUE){
  
  #### Bibliotecas Necessárias ####
  library(deSolve)
  library(ggplot2)
  library(reshape)
  
  #### modelo Suscetível-Infectado-Suscetível ####
  SIRmodel <- function(Time, State, Pars) {
    with(as.list(c(State, Pars)), {
      v <- vo*(sign(Time-t1) + 1)/2*(sign(t2-Time) + 1)/2
      dS <- m*(I+R) - b*S*I -v*S
      dI <- b*S*I - m*I - g*I 
      dR <- v*S + g*I - m*R
      return(list(c(dS, dI, dR)))
    })
  }
  
  #### Parâmetros do Modelo e da Simução Numérica ####
  parmsODE <- c(b=b,g=g,m=m,vo=vo,t1=t1,t2=t2)
  times <- seq(0, tempoSim, by = tempoSim/100)
  
  #### Condição Inicial ####
  yini <- c(S=So, I=Io, R=Ro)
  
  #### Simulação Numérica ####
  outSIRmodel<- as.data.frame(ode(yini, times, SIRmodel, parmsODE,method="ode45"))
  names(outSIRmodel) <- c('Tempo','Suscetíveis','Infectados','Recuperados')
  
  if (grafico==T){
    
    #### Transformação dos dados para plotagem ####
    outSIR <- melt(as.data.frame(outSIRmodel),id="Tempo")
    
    #### Plotanto os resultados ####
    if (vo==0){
      graf <- ggplot(outSIR,aes(x=Tempo,y=value,colour=variable,group=variable)) + geom_line(size=1.1) +
        ggtitle(paste("Dinâmica SIR\nb=",b," g=",g," m=",m,sep=" ")) +
        xlab("Tempo") + ylab("Número de Animais") + labs(colour = "População") +
        theme(text=element_text(size=20));
    }
    else{
      graf <- ggplot(outSIR,aes(x=Tempo,y=value,colour=variable,group=variable)) + geom_line(size=1.1) +
        ggtitle(paste("Dinâmica SIR\nb=",b," g=",g," m=",m,"vo=",vo,sep=" ")) +
        xlab("Tempo") + ylab("Número de Animais") + labs(colour = "População") +
        geom_vline(xintercept = c(t1,t2),linetype = "longdash") +
        theme(text=element_text(size=20));
    }
    plot(graf)
  }
  return(outSIRmodel)
}