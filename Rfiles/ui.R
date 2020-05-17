# Define UI for bacondecomp
ui <- fluidPage(
    tags$head(
        tags$style(
            HTML('body{font-size:11px;background-color:#FFFFFF} #RegSum1{font-size:11px}#RegSum2{font-size:9px}label{font-weight:normal}')
        )
    ),
    # Sidebar for settings
    column(3,
           # Settings
           strong("Settings"),br(),
           # Inputs
           numericInput(inputId= "seed",label="Set seed", 1909, min = 0, max = NA),
           strong("Treatment effects"),
           sliderInput(inputId = "group2treatmenteffect",label = "Treatment effect for group 2 (baseline):",min = 0,max = 3,value = 1.4,step=0.1),
           sliderInput(inputId = "group3treatmenteffect",label = "Treatment effect for group 3 (baseline):",min = 0,max = 3,value = 1.6,step=0.1),br(),
           strong("Time-varying treatment effects"),
           sliderInput(inputId = "group2timeeffect",label = "Change in treatment effect over time for group 2:",min = -0.03,max = 0.03,value = 0.01,step=0.01),
           sliderInput(inputId = "group3timeeffect",label = "Change in treatment effect over time for group 3:",min = -0.03,max = 0.03,value = 0,step=0.01),br(),
           strong("Treatment timing"),
           sliderInput(inputId = "group2treatment",label = "When does group 2 get treated?",min = 2,max = 28,value = 10),
           sliderInput(inputId = "group3treatment",label = "When does group 3 get treated?",min = 2,max = 28,value = 20),br(),
           strong("Group size"),
           sliderInput(inputId = "group2size",label = "Size group 2?",min = 2,max = 100,value = 30),
           sliderInput(inputId = "group3size",label = "Size group 3?",min = 2,max = 100,value = 30),
           
    ),
    # Main panel with results
    column(6,
           h3("Illustration of 'Goodman-Bacon (2019): DD  with Variation in Treatment Timing' "),
           br(),"by Hans H. Sievertsen", tags$a(href="https://github.com/hhsievertsen/bacondecomp", "(source code)"),"(h.h.sievertsen@bristol.ac.uk)",br(),
           # chart
           plotOutput(outputId = "distPlot"),
           tableOutput(outputId = "RegSum1"),br(),
           "Notes: The DGP  for the overall ATT (first row) refers to the population weighted ATT across the two groups. The estimate for the overall ATT (first row) is the 2-way fixed effects estimate.)",
    ),
    # Side bar with info
    fluidRow(column(3,
                    # Explanation
                  strong("! Change settings in the panel on the left !"),br(), 
                  br(),
                    strong("Setup"),br(),
                    "- 3 groups. Group 1 has a fixed size of 30.",br(),
                    "- Group 1 is never treated.",br(),
                    "- Groups 2 & 3 get treated at some point (see left panel).",br(),
                    "- Treatment effects can vary across groups and over time (see left panel).",br(),
                    "- The two-way fixed effects DD  is estimated with felm() from the lfe package, by estimating equation (2) from Goodman-Bacon (2019):",br(),
              
                    withMathJax(
                        helpText('$$y_{it}=\\alpha_{i\\cdot}+\\alpha_{\\cdot t}+\\beta^{DD}D_{it}+e_{it}$$')),br(),
                    "- the Bacon Decomposition is done using the bacomdecomp package",
                    br(),
                    br("Sources"),
                    "* ",  tags$a(href="https://cdn.vanderbilt.edu/vu-my/wp-content/uploads/sites/2318/2019/07/29170757/ddtiming_7_29_2019.pdf", "Goodman-Bacon (2019) Working Paper"),
                    br(),
                    "* ",tags$a(href="https://cran.r-project.org/web/packages/bacondecomp/readme/README.html", "bacondecomp for R"),
               
                   br(),br(),
                   strong("Updates:"),br(),
                   "- May 12, 2020: first version by Hans H. Sievertsen",br(),
                   "- May 13, 2020: incorporated changes by Matthieu Stigler (https://matthieustigler.github.io/)",br(),
                   "- May 14, 2020: corrected my mistake in calculating ATT and updated table.",br(),
                   "- May 17, 2020: added event study chart and DGP values to table.",br(),br(),
                   "Corrections and suggestions are very welcome (by e-mail: h.h.sievertsen@bristol.ac.uk or on github: https://github.com/hhsievertsen/bacondecomp)",br(),br(),br()
                  ,
                  strong("Regression output:"),
                  verbatimTextOutput(outputId = "RegSum2"),
    )
   ))
