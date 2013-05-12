# file name: ui.R
#
# Purpose: 
#    Executa interface do usuário para o modelo SLI.
# 
#  Record of revisions:
#      Date        Programmer          Description of change
#    13/05/11     M.S. Fernando     Documentação e criação do script

library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Dinâmica Populacional - SLI"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    numericInput("a", "Taxa de natalidade média:",1),
    numericInput("b", "Taxa de contatos potencialmente infecciosos (km2 ano-1):", 77),
    numericInput("m","Taxa de mortalidade natural média (ano-1)",5e-1,step=0.1),
    numericInput("K","Capacidade de suporte do meio (animais por km2)", 2,step=0.5),
    numericInput("alfa","Taxa de mortalidade pela raiva (letalidade) (ano-1)", 73),
    numericInput("sigma", "Inverso do período médio de latência (ano-1):",13),
    numericInput("So","Densidade inicial de indivíduos suscetíveis:",2.5,step=0.1),
    numericInput("Lo","Densidade inicial de indivíduos infectados que 
                 ainda não são infecciosos no instante:",0.1,step=0.1),
    numericInput("Io", "Densidade inicial de indivíduos infectados:",1,step=0.1),
    numericInput("tempoSim", "Tempo de simulação:",0.5,step=0.1)
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("simPlot")
  )
))