# file name: ui.R
#
# Purpose: 
#    Executa interface do usuário para o modelo SIR.
# 
#  Record of revisions:
#      Date        Programmer          Description of change
#    13/05/05     M.S. Fernando     Documentação e criação do script

library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Dinâmica Populacional - SIR"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    numericInput("b", "Taxa de contatos potencialmente infectantes:", 5e-3,step=1e-3),
    numericInput("g", "Taxa de recuperação (=inverso do período infeccioso):",1e-2,step=1e-2),
    numericInput("m","Taxa de natalidade (=taxa de mortalidade)",15e-2,step=1e-3),
    numericInput("vo","Taxa relacionada à proporção de cobertura vacinal", 0,step=1e-2),
    numericInput("t1", "Início da campanha de vacinação",25),
    numericInput("t2","Fim da campanha de vacianção",50),
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
                value = 1),
    sliderInput("Ro", 
                "Número inicial de indivíduos recuperados:", 
                min = 0, 
                max = 100, 
                value = 0)
    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("simPlot")
  )
))