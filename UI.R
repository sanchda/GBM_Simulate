library(shiny)

# Define UI, based loosely on one of Shiny's templates
shinyUI(fluidPage(

  ### Title ###
  titlePanel("GBM Population Simulation"),

  ### TOP PANEL ###
  tabsetPanel(
  tabPanel("Simulation",

    ### SIDE PANEL ###
    fluidRow(
    column(3,
    numericInput("n", "Population Size", 50, 1, 10^4),
	
	### Age
	  uiOutput("stackoutput"),
    radioButtons("ageDist1", h3("Age Distribution:"), choices=c(
        "Normal",
        "Uniform",
        "Log-normal"="lognormal",
        "Exponential"
      ), selected="Normal"
    ),
    
    conditionalPanel(
      condition="input.ageDist1=='Normal'",
      numericInput("ageNormalMean1", "Mean", value=37.2, min=0, max=100),
      numericInput("ageNormalSD1", "Standard Deviation", value=10, min=0, max=60)
    ),
    
    conditionalPanel(
      condition="input.ageDist1=='Uniform'",
      numericInput("ageUniformMin", "Value", value=37.2, min=0, max=120),
	  numericInput("ageUniformMax", "Value", value=37.2, min=0, max=120)
    ),
    
    conditionalPanel(
      condition="input.ageDist1=='lognormal'",
      numericInput("ageLognormalMean1", "log-Mean", value=2, min=0, max=5),
      numericInput("ageLognormalSD1", "log-Standard Deviation", value=0.25, min=0, max=2)
    ),
    
    conditionalPanel(
      condition="input.ageDist1=='Exponential'",
      numericInput("ageExponentialRate1", "Rate", value=1, min=0, max=100)
    ),
	
	### KPS
    radioButtons("kpsDist1", h3("KPS Distribution:"), choices=c(
        "Normal",
        "Uniform",
        "Log-normal",
        "Exponential"
      ), selected="Normal"
    ),
    
    conditionalPanel(
      condition="input.kpsDist1=='Normal'",
      numericInput("kpsNormalMean1", "Mean", value=70, min=0, max=100),
      numericInput("kpsNormalSD1", "Standard Deviation", value=20, min=0, max=100)
    ),
    
    conditionalPanel(
      condition="input.kpsDist1=='Uniform'",
      numericInput("kpsUniformMin", "Value", value=70, min=0, max=120),
	    numericInput("kpsUniformMax", "Value", value=70, min=0, max=120)

    ),
    
    conditionalPanel(
      condition="input.kpsDist1=='lognormal'",
      numericInput("kpsLognormalMean1", "log-Mean", value=2, min=0, max=5),
      numericInput("kpsLognormalSD1", "log-Standard Deviation", value=0.25, min=0, max=2)
    ),
    
    conditionalPanel(
      condition="input.kpsDist1=='Exponential'",
      numericInput("kpsExponentialRate1", "Rate", value=1, min=0, max=100)
    ),
	
	### EOR
    radioButtons("eorDist1", h3("EOR Distribution:"), choices=c(
        "Normal",
        "Uniform",
        "Log-normal",
        "Exponential"
      ), selected="Normal"
    ),
    
    conditionalPanel(
      condition="input.eorDist1=='Normal'",
      numericInput("eorNormalMean1", "Mean", value=0.5, min=0, max=100,step=0.1),
      numericInput("eorNormalSD1", "Standard Deviation", value=0.1, min=0, max=0.9,step=0.01)
    ),
    
    conditionalPanel(
      condition="input.eorDist1=='Uniform'",
      numericInput("eorUniformMin", "Value", value=0.5, min=0, max=100,step=0.1),
	    numericInput("eorUniformMax", "Value", value=0.5, min=0, max=100,step=0.1)
    ),
    
    conditionalPanel(
      condition="input.eorDist1=='lognormal'",
      numericInput("eorLognormalMean1", "log-Mean", value=2, min=0, max=5),
      numericInput("eorLognormalSD1", "log-Standard Deviation", value=0.25, min=0, max=2)
    ),
    
    conditionalPanel(
      condition="input.eorDist1=='Exponential'",
      numericInput("eorExponentialRate1", "Rate", value=1, min=0, max=100)
    ),
	
	### XRT
    numericInput("xrtPerc1", h3("XRT Ratio:"), 0.5, 0, 1),
	
	### TMZ
    numericInput("tmzPerc1", h3("TMZ Ratio:"), 0.5, 0, 1)

    
    ),
    
    ### MAIN PANEL
    column(9,
           plotOutput("popPlot"),
           tableOutput("popTable")
    ))
  ))
))