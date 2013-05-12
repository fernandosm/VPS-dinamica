# file name: ui.R
#
# Purpose: 
#    Executa interface do usuário para o modelo SIS.
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
#
#

library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Dinâmica Populacional - SIS"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    numericInput("b", "Taxa de contatos potencialmente infectantes:", 9e-3,step=1e-3),
    numericInput("g", "Taxa de retorno de indivíduos infectados ao compartimento de suscetíveis:", 5e-1,step=1e-1),
    numericInput("tempoSim", "Tempo de simulação:",100),
    sliderInput("So", 
                "Número inicial de indivíduos suscetíveis:", 
                min = 0, 
                max = 100, 
                value = 100),
    sliderInput("Io", 
                "Número inicial de indivíduos infectados:", 
                min = 0, 
                max = 100, 
                value = 1)
    
  ),  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("simPlot")
  )
))