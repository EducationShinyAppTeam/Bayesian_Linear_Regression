# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(psych)
library(ggplot2)
library(dplyr)
library(rstanarm)
library(Metrics)
library(ggpubr)
# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "black",
    ### Create the app header ----
    dashboardHeader(
      title = "Bayesian Linear Regr.",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Bayesian_Linear_Regression")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Example", tabName = "example", icon = icon("book-reader")),
        menuItem("OLS Regression", tabName = "ols", icon = icon("wpexplorer")),
        menuItem("Bayesian Regression", tabName = "bayesian", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Bayesian Linear Regression"), # This should be the full name.
          p("This app introduces the concept of Bayesian linear regression. 
            We will compare the Simple linear regression (using ordinary least 
            squares (OLS) technique to find the best-fitting line for a set of data points) 
            with Bayesian linear regression, and see some results applied to simple 
            datasets."),
          h2("Instructions"),
          p("In order to use this app more effectively, it is recommended to 
            explore in the following order."),
          tags$ol(
            tags$li("Review prerequistes using the Prerequistes tab."),
            tags$li("Enter the Explore page and compare the results of 
                    Bayesian method and traditional method.")
          ),
          ##### Go prere
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goPre",
              label = "Prerequisites!",
              size = "large",
              icon = icon("book"),
              style = "default"
            )
          ),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Jing Fu.",
            br(),
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 7/5/2022 by JF.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, please review the
            following:"),
          tags$ul(
            tags$li("Ordinary least squares (OLS) is a method for fitting the 
                    model parameters by minimizing the residual sum of squares (RSS). 
                    RSS is the total of the squared differences between the actual 
                    values (y) and the predicted model outputs."),
            tags$li("Bayesian linear regression is an approach to linear regression 
                    in which the statistical analysis is undertaken within the 
                    context of Bayesian inference. The aim of Bayesian Linear 
                    Regression is not to find the single “best” value of the model 
                    parameters, but rather to determine the posterior distribution 
                    for the model parameters.")
          ),
          br(),
          br(),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          )
        ),
        #### Set up an Example Page ----
        tabItem(
          tabName = "example",
          h2("Example"),
          withMathJax(
            p("We have 700 students' SAT Verbal and SAT Quantitative 
              self-report scores. We want to study whether there is a 
              linear relationship between SAT Verbal scores and SAT 
              Quantitative scores. The expression is: 
              $$SATQ = \\beta_0 + \\beta_1SATV +\\epsilon$$")
          ),
          p("The following graph shows OLS and BLR model. With OLS, we get a single 
            estimate with 95% confidence interval for the model parameters, the 
            intercept and slope here. With BLR, we get posterior distributions of 
            parameters, intercept, slope and sigma here. And we explore 100 
            possible models drawn from the model parameter posteriors."),
          br(),
          fluidPage(
            ##### OLS----
            ###### input----
            tags$strong("Ordinary Least Squares"),
            br(),
            br(),
            column(
              width = 4,
              offset = 0,
              wellPanel(
                checkboxInput(
                  inputId = "olsband",
                  label = "Show 95% confidence band", 
                  value = FALSE
                )
              )
            ),
            ###### output----
            column(
              width = 8,
              offset = 0,
              plotOutput("olsfit"),
              checkboxInput(
                inputId = "olsparaC",
                label = "Results table", 
                value = FALSE
              ),
              conditionalPanel(
                condition = "input.olsparaC==1",
                tableOutput("olspara")
              )
            ),
            br(),
            ##### BLR----
            tags$strong("Bayesian Linear Regression"),
            br(),
            br(),
            column(
              width =4,
              offset = 0,
              wellPanel(
                ###### input----
                selectInput(
                  inputId = "priortype",
                  label = "Prior distributions",
                  choices = c(
                    "Default priors" = "defaultp",
                    "Informative priors" = "informp"
                  ),
                  selected = "defaultp"
                ),
                conditionalPanel(
                  condition = "input.priortype == 'defaultp'",
                  uiOutput("defaultpriors"),
                ),
                conditionalPanel(
                  condition = "input.priortype == 'informp'",
                  tags$strong("Interept"),
                  sliderInput(
                    inputId = "beta0mu",
                    label = "Mean for intercept",
                    min = 200,
                    max = 800,
                    value = 500
                  ),
                  sliderInput(
                    inputId = "beta0sigma",
                    label = "SD for intercept",
                    min = 10,
                    max = 200,
                    value = 50
                  ),
                  tags$strong("Slope"),
                  sliderInput(
                    inputId = "beta1mu",
                    label = "Mean for slope",
                    min = -4,
                    max = 4,
                    value = 0.5,
                    step = 0.1
                  ),
                  sliderInput(
                    inputId = "beta1sigma",
                    label = "SD for slope",
                    min = 0.01,
                    max = 1,
                    value = 0.03,
                    step = 0.01
                  ),
                  tags$strong("Sigma"),
                  sliderInput(
                    inputId = "sigmarate",
                    label = "Rate for sigma",
                    min = 0.01,
                    max = 0.1,
                    step = 0.01,
                    value = 0.02
                  )
                )
              )
            ),
            ###### output----
            column(
              width = 8,
              offset = 0,
              plotOutput("blrfit"),
              checkboxInput(
                inputId = "blrparaC",
                label = "Results table", 
                value = FALSE
              ),
              conditionalPanel(
                condition = "input.blrparaC==1",
                tableOutput("blrpara")
              ),
              checkboxInput(
                inputId = "blrparaplotC",
                label = "Show prior and posterior distributions of parameters", 
                value = FALSE
              ),
              conditionalPanel(
                condition = "input.blrparaplotC==1",
                plotOutput("blrparaplot")
              )
            )
          )
        ),
        #### Set up an OLS Prediction Page ----
        tabItem(
          tabName = "ols",
          h2("Explore Ordinary Least Squares regression prediction results"),
          withMathJax(
            p("We have 700 students' SAT Verbal and SAT Quantitative self-report scores. 
              We want to study whether there is a linear relationship between SAT 
              Verbal scores and SAT Quantitative scores. The expression is: 
              $$SATQ = \\beta_0 + \\beta_1SATV +\\epsilon$$")),
          p("Explore the prediction results of the OLS method. Use the slider 
              to select the proportion of data in the training set and select 
              the SAT verbal scores you want to use to predict SAT quantitative 
              scores. We will show the results predicted by the OLS method with 
              the 95% confidence interval.In the two plots, you will find that 
              once the training set is selected, the prediction curve for average 
              verbal will be fixed (black one), but as the verbal you select 
              changes, the prediction curve for it will also change (blue one)."),
          
          fluidPage(
            #####input----
            column(
              width =4,
              offset = 0,
              wellPanel(
                sliderInput(
                  inputId = "trainingsizepreols",
                  label = "Proportion of full dataset",
                  min = 0.05,
                  max = 0.95,
                  value = 0.7,
                  step = 0.05
                ),
                sliderInput(
                  inputId = "xpointols",
                  label = "SAT Verbal for prediction",
                  min = 200,
                  max = 800,
                  value = 700,
                  step = 5
                ),
                checkboxInput(
                  inputId = "olsbandpre",
                  label = "Show 95% confidence band", 
                  value = FALSE
                ),
                bsButton(
                  inputId = "newsamplepreols",
                  label = "New Sample",
                  size = "large",
                  icon = icon("retweet"),
                  style = "default"
                ),
                br(),
                br(),
                uiOutput("maesols")
              )
            ),
            ##### output----
            column(
              width = 8,
              offset = 0,
              plotOutput("olsfitpre"),
              checkboxInput(
                inputId = "olsparapreC",
                label = "Results table", 
                value = FALSE
              ),
              conditionalPanel(
                condition = "input.olsparapreC==1",
                tableOutput("olsparapre")
              ),
              plotOutput("olsestimate"),
              checkboxInput("resultsols",
                            "Results table", FALSE),
              conditionalPanel(
                condition = "input.resultsols==1",
                tableOutput("tableols")
              ),
              uiOutput("test")
            )
          )
        ),
        #### Set up a Bayesian Regr. Prediction Page ----
        tabItem(
          tabName = "bayesian",
          h2("Explore Bayesian linear regression prediction results"),
          withMathJax(
            p("We have 700 students' SAT Verbal and SAT Quantitative self-report scores. 
              We want to study whether there is a linear relationship between SAT 
              Verbal scores and SAT Quantitative scores. The expression is: 
              $$SATQ = \\beta_0 + \\beta_1SATV +\\epsilon$$")),
          p("Explore the prediction results of the BLR model. 
              Use the slider to select the proportion of data in the training set, 
              and select the prior distribution for Bayesian regression. Then 
              select the SAT verbal scores you want to use to predict SAT quantitative 
              scores. We will show the results predicted by the BLR method and 
              the 95% credible interval.In the two plots, you will find that 
              once the training set is selected, the prediction curve for 
              average verbal will be fixed (black one), but as the verbal you 
              select changes, the prediction curve for it will also change (blue one)."),  
          fluidPage(
            #####input----
            column(
              width =4,
              offset = 0,
              wellPanel(
                sliderInput(
                  inputId = "trainingsizepreblr",
                  label = "Proportion of full dataset",
                  min = 0.05,
                  max = 0.95,
                  value = 0.7,
                  step = 0.05
                ),
                sliderInput(
                  inputId = "xpointblr",
                  label = "SAT Verbal for prediction",
                  min = 200,
                  max = 800,
                  value = 700,
                  step = 5
                ),
                selectInput(
                  inputId = "priortypepreblr",
                  label = "Prior distributions",
                  choices = c(
                    "Default priors" = "defaultp",
                    "Informative priors" = "informp"
                  ),
                  selected = "defaultp"
                ),
                conditionalPanel(
                  condition = "input.priortypepreblr == 'informp'",
                  tags$strong("Intercept"),
                  sliderInput(
                    inputId = "beta0mupreblr",
                    label = "Mean for intercept",
                    min = 200,
                    max = 800,
                    value = 500
                  ),
                  sliderInput(
                    inputId = "beta0sigmapreblr",
                    label = "SD for intercept",
                    min = 10,
                    max = 200,
                    value = 50
                  ),
                  tags$strong("Slope"),
                  sliderInput(
                    inputId = "beta1mupreblr",
                    label = "Mean for slope",
                    min = -4,
                    max = 4,
                    value = 0.5,
                    step = 0.1
                  ),
                  sliderInput(
                    inputId = "beta1sigmapreblr",
                    label = "SD for slope",
                    min = 0.01,
                    max = 1,
                    value = 0.03,
                    step = 0.01
                  ),
                  tags$strong("Sigma"),
                  sliderInput(
                    inputId = "sigmaratepreblr",
                    label = "Mean for sigma",
                    min = 0.01,
                    max = 0.1,
                    step = 0.01,
                    value = 0.02
                  )
                ),
                conditionalPanel(
                  condition = "input.priortypepreblr == 'defaultp'",
                  uiOutput("blrpriorOutputpre")
                ),
                br(),
                bsButton(
                  inputId = "newsamplepreblr",
                  label = "New Sample",
                  size = "large",
                  icon = icon("retweet"),
                  style = "default"
                ),
                br(),
                br(),
                uiOutput("maesblr")
              )
            ),
            ##### output----
            column(
              width = 8,
              offset = 0,
              plotOutput("blrfitpre"),
              checkboxInput(
                inputId = "blrparaCpre",
                label = "Results table", 
                value = FALSE
              ),
              conditionalPanel(
                condition = "input.blrparaCpre==1",
                tableOutput("blrparapre")
              ),
              plotOutput("bayesestimate"),
              checkboxInput("resultsblr",
                            "Results table", FALSE),
              conditionalPanel(
                condition = "input.resultsblr==1",
                tableOutput("tableblr")
              )
            )
          )
        ),
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v 0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. J. (2022). boastUtils: BOAST utlities.
            (v 0.1.12.3). [R package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Change, W., and Borges Ribeiro, B. (2021). shinydashboard: Create 
            dashboards with 'shiny'. (v 0.7.2) [R package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y.,
            Allen, J., McPherson, J., Dipert, A., and Borges, B. (2021). shiny:
            Web application framework for R. (v 1.7.1). [R package]. Available
            from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Goodrich, B., Gabry, J., Ali, I., and Brilleman, S. (2022). rstanarm: Bayesian 
            applied regression modeling via Stan. (v 2.21.3) [R package]. Available
            from https://mc-stan.org/rstanarm/"
          ),
          p(
            class = "hangingindent",
            "Hamner, B., Michael, F., and LeDell, E. (2022). Metrics: Evaluation Metrics for Machine Learning 
            (v 2.2.5) [R package]. Available from https://CRAN.R-project.org/package=Metrics"
          ),
          p(
            class = "hangingindent",
            "Kassambara, A. (2020). ggpubr: 'ggplot2' Based Publication Ready Plots.
            (v 0.4.0) [R package]. Available from https://CRAN.R-project.org/package=ggpubr"
          ),
          p(
            class = "hangingindent",
            "Revelle, W. (2022). psych: Procedures for Psychological, Psychometric, 
            and Personality Research. Northwestern University, Evanston, Illinois. 
            (v 2.2.5) [R package]. Available from https://CRAN.R-project.org/package=psych"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
            Springer-Verlag:New York. (v 3.3.6) [R package]. Available from
            https://ggplot2.tidyverse.org"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry, L.,and Müller, K. (2022). dplyr: A 
            Grammar of Data Manipulation. (v 1.0.9).[R package]. Available from 
            https://dplyr.tidyverse.org"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "This App will help you to understand the differences between Bayesian
        Linear Regression and Frequentist Linear Regression"
      )
    }
  )
  ## prerequisites button
  observeEvent(
    eventExpr = input$goPre,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites"
      )
    }
  )
  ## example button
  observeEvent(
    eventExpr = input$go,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "example"
      )
    }
  )
  ## Full data
  satFull<-sat.act[,c(5,6)]
  ## Example Page----
  ### OLS----
  ## training set
  trainols<-reactive(na.omit(satFull))
  olsmodel<-reactive(lm(SATQ~SATV,data = trainols()))
  
  output$olsfit<-renderPlot({
    band<-
      if(input$olsband == 1){
        stat_smooth(
          data=trainols(),
          mapping = aes(x=SATV,y=SATQ),
          method = "lm",
          formula = y~x,
          level = 0.95,
          color = psuPalette[1]
        )
      }else{
        stat_smooth(
          data = trainols(),
          mapping = aes(x=SATV,y=SATQ),
          method = "lm",
          formula = y~x,
          se = FALSE,
          color = psuPalette[1]
        )
      }
    p<-ggplot()+
      geom_point(
        data=trainols(),
        mapping = aes(x=SATV,y=SATQ),
        colour="black",
        alpha = 0.5
      )+
      band+
      labs(
        title = "Ordinary Least Squares",
        x = "SAT Verbal", 
        y = "SAT Quantitative",
        alt = "The plot shows the linear relationship between sat verbal scores and sat quantitative scores"
      )+
      theme_bw()+
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16)
      )
    return(p)
  })
  
  output$olspara<-renderTable({
    coeff<-coefficients(olsmodel())
    c2<-paste("95% Confidence interval"," lower bound",sep = "<br>")
    c3<-paste("95% Confidence interval"," upper bound",sep = "<br>")
    ctable<-matrix(c(coeff[1],confint(olsmodel())[1],confint(olsmodel())[3],coeff[2],confint(olsmodel())[2],confint(olsmodel())[4]),nrow=2,ncol=3,byrow = TRUE,
                   dimnames = list(c("Intercept","SAT Verbal"),c("Estimate",c2,c3)))
  },rownames = TRUE,bordered = TRUE,sanitize.text.function=identity
  )
  
  ### Bayesian----
  ## training set
  trainblr<-reactive(na.omit(satFull))
  blrmodel<-reactive({
    if(input$priortype=='defaultp'){
      blrmodel<-stan_glm(SATQ~SATV,data = trainblr(),iter=2000,chain=1)
    }
    if(input$priortype=='informp'){
      blrmodel<-
        stan_glm(SATQ~SATV,data = trainblr(),prior = normal(location = input$beta1mu,scale = input$beta1sigma),
                 prior_intercept = normal(location = input$beta0mu, scale = input$beta0sigma),
                 prior_aux = exponential(rate = input$sigmarate),
                 iter=1500,
                 chain=1)
    }
    return(blrmodel)
  })
  
  ## posterior values
  postparas<-reactive(as.data.frame(blrmodel()))
  
  output$defaultpriors<-renderUI({
    validate(
      need(
        input$priortype == 'defaultp',
        "")
    )
    pr<-prior_summary(blrmodel())
    withMathJax(
      paste0("Intercept~N(\u03BC=",
             round(pr$prior_intercept$location,2),", \u03C3=",
             round(pr$prior_intercept$adjusted_scale,2),")"),
      br(),
      paste0("Slope~N(\u03BC=",round(pr$prior$location,2),", \u03C3=",
             round(pr$prior$adjusted_scale,2),")"),
      br(),
      paste0("Sigma~Exp(\u03BB=",
             round(1/pr$prior_aux$adjusted_scale,2),")")
    )
  })
  
  output$blrfit<-renderPlot({
    newdata<-sample_n(postparas(),100)
    ggplot()+
      geom_point(
        data=trainblr(),
        mapping = aes(x=SATV,y=SATQ),
        colour="black",
        alpha = 0.5
      )+
      geom_segment(
        data = newdata,
        mapping = aes(
          x= 200, 
          xend = 800,
          y = newdata[,1] + newdata[,2]*200, 
          yend = newdata[,1] + newdata[,2]*800
        ),
        color = psuPalette[1],
        alpha = 0.1
      )+
      labs(
        title = "Bayesian Linear Regression",
        subtitle = "* 100 regression lines sampled from posterior distributions",
        x = "SAT Verbal", 
        y = "SAT Quantitative",
        alt = "The plot shows the linear relationship between sat verbal scores and sat quantitative scores"
      )+
      theme_bw()+
      theme(
        plot.caption = element_text(size = 18),
        plot.subtitle = element_text(lineheight = 0.9,size = 16),
        text = element_text(size = 18),
        axis.title = element_text(size = 16),
      )
  })
  
  output$blrpara<-renderTable({
    c2<-paste("95% Credible interval"," lower bound",sep = "<br>")
    c3<-paste("95% Credible interval"," upper bound",sep = "<br>")
    ctable<-matrix(
      c(mean(postparas()[,1]),
        quantile(postparas()[,1],probs=.025),
        quantile(postparas()[,1],probs=.975),
        mean(postparas()[,2]),
        quantile(postparas()[,2],probs=.025),
        quantile(postparas()[,2],probs=.975),
        mean(postparas()[,3]),
        quantile(postparas()[,3],probs=.025),
        quantile(postparas()[,3],probs=.975)),
      nrow=3,ncol=3,byrow = TRUE,dimnames = list(c("Intercept","SAT Verbal","Sigma"),
                                                 c("Mean",c2,c3)))
  },rownames = TRUE,bordered = TRUE,sanitize.text.function=identity)
  
  output$blrparaplot<-renderPlot({
    ## informative
    if(input$priortype=='informp'){
      # intercept
      intx<-seq(100,800,length=1000)
      inty<-dnorm(intx,mean = input$beta0mu,sd=input$beta0sigma)
      intdata<-data.frame(x=intx,y=inty)
      combined1<-
        ggplot()+
        geom_line(
          data=intdata,
          mapping = aes(x=x,y=y,colour = "Prior")
        )+
        geom_density(
          data = postparas(),
          mapping = aes(x = postparas()[,1],colour = "Posterior")
        )
      combined1all<-
        combined1+
        geom_point(
          mapping=aes(x=c(quantile(postparas()[,1],probs=.025),quantile(postparas()[,1],probs=.975)),y=c(0,0)),
          alpha=0
        )+
        geom_errorbarh(
          aes(xmin=quantile(postparas()[,1],probs=.025),xmax=quantile(postparas()[,1],probs=.975),y=0,colour="Credible region"),
          height=0.05*layer_scales(combined1)$y$get_limits()[2],
          size=1
        )+
        labs(
          x = "Intercept", 
          y = "Density",
          alt = "The plot combined the prior distribution and the posterior plot"
        )+
        scale_x_continuous(expand = expansion(mult = 0)) +
        scale_y_continuous(expand = expansion(mult = .05))+
        scale_color_manual(
          name = NULL,
          values = c(
            "Credible region" = psuPalette[1],
            "Prior" = psuPalette[3],
            "Posterior" = psuPalette[2]
          )
        )+
        theme_bw()+
        theme(
          plot.caption = element_text(size = 18),
          text = element_text(size = 18),
          axis.title = element_text(size = 16),
          legend.position = "bottom"
        )
      # slope
      slopex<-seq(-4,4,length=1000)
      slopey<-dnorm(slopex,mean = input$beta1mu,sd=input$beta1sigma)
      slopedata<-data.frame(x=slopex,y=slopey)
      combined2<-
        ggplot()+
        geom_line(
          data=slopedata,
          mapping = aes(x=x,y=y,colour = "Prior")
        )+
        geom_density(
          data = postparas(),
          mapping = aes(x = postparas()[,2], colour = "Posterior")
        )
      combined2all<-
        combined2+
        geom_point(
          mapping=aes(x=c(quantile(postparas()[,2],probs=.025),quantile(postparas()[,2],probs=.975)),y=c(0,0)),
          alpha=0
        )+
        geom_errorbarh(
          aes(xmin=quantile(postparas()[,2],probs=.025),xmax=quantile(postparas()[,2],probs=.975),y=0,colour = "Credible region"),
          height=0.05*layer_scales(combined2)$y$get_limits()[2],
          size=1
        )+
        labs(
          x = "Slope", 
          y = "Density",
          alt = "The plot combined the prior distribution and the posterior plot"
        )+
        scale_x_continuous(expand = expansion(mult = 0)) +
        scale_y_continuous(expand = expansion(mult = .05))+
        scale_color_manual(
          name = NULL,
          values = c(
            "Credible region" = psuPalette[1],
            "Prior" = psuPalette[3],
            "Posterior" = psuPalette[2]
          )
        )+  
        theme_bw()+
        theme(
          plot.caption = element_text(size = 18),
          text = element_text(size = 18),
          axis.title = element_text(size = 16),
          legend.position = "bottom"
        )
      # sigma
      sigmax<-seq(0,800,length=1000)
      sigmay<-dexp(sigmax,rate=input$sigmarate)
      sigmadata<-data.frame(x=sigmax,y=sigmay)
      combined3<-
        ggplot()+
        geom_line(
          data=sigmadata,
          mapping = aes(x=x,y=y,colour = "Prior")
        )+
        geom_density(
          data = postparas(),
          mapping = aes(x = postparas()[,3],colour="Posterior")
        )
      combined3all<-
        combined3+
        geom_point(
          mapping=aes(x=c(quantile(postparas()[,3],probs=.025),quantile(postparas()[,3],probs=.975)),y=c(0,0)),
          alpha=0
        )+
        geom_errorbarh(
          aes(xmin=quantile(postparas()[,3],probs=.025),xmax=quantile(postparas()[,3],probs=.975),y=0,colour = "Credible region"),
          height=0.05*layer_scales(combined3)$y$get_limits()[2],
          size=1
        )+
        labs(
          x = "Sigma", 
          y = "Density",
          alt = "The plot combined the prior distribution and the posterior plot"
        )+
        scale_x_continuous(expand = expansion(mult = 0)) +
        scale_y_continuous(expand = expansion(mult = .05))+
        scale_color_manual(
          name = NULL,
          values = c(
            "Credible region" = psuPalette[1],
            "Prior" = psuPalette[3],
            "Posterior" = psuPalette[2]
          )
        )+
        theme_bw()+
        theme(
          plot.caption = element_text(size = 18),
          text = element_text(size = 18),
          axis.title = element_text(size = 16),
          legend.position = "bottom"
        )
      all<-
        ggarrange(combined1all,combined2all,combined3all,ncol=2,nrow=2,common.legend = TRUE,legend = "bottom")
      allt<-annotate_figure(
        p = all,
        top = text_grob(
          "Prior and Posterior",
          color = "black", 
          size = 22
        )
      )
      return(allt)
    }
    ## weakly informative
    if(input$priortype=='defaultp'){
      # default prior
      pr<-prior_summary(blrmodel())
      # intercept
      intx<-seq(100,800,length=1000)
      inty<-dnorm(intx,mean = pr$prior_intercept$location,sd=pr$prior_intercept$adjusted_scale)
      intdata<-data.frame(x=intx,y=inty)
      combined1<-
        ggplot()+
        geom_line(
          data=intdata,
          mapping = aes(x=x,y=y, colour = "Prior")
        )+
        geom_density(
          data = postparas(),
          mapping = aes(x = postparas()[,1], colour = "Posterior")
        )
      combined1all<-
        combined1+
        geom_point(
          mapping=aes(x=c(quantile(postparas()[,1],probs=.025),quantile(postparas()[,1],probs=.975)),y=c(0,0)),
          alpha=0
        )+
        geom_errorbarh(
          aes(xmin=quantile(postparas()[,1],probs=.025),xmax=quantile(postparas()[,1],probs=.975),y=0,colour = "Credible region"),
          height=0.05*layer_scales(combined1)$y$get_limits()[2],
          size=1
        )+
        labs(
          x = "Intercept", 
          y = "Density",
          alt = "The plot combined the prior distribution and the posterior plot"
        )+
        scale_x_continuous(expand = expansion(mult = 0)) +
        scale_y_continuous(expand = expansion(mult = .05))+
        scale_color_manual(
          name = NULL,
          values = c(
            "Credible region" = psuPalette[1],
            "Prior" = psuPalette[3],
            "Posterior" = psuPalette[2]
          )
        )+
        theme_bw()+
        theme(
          plot.caption = element_text(size = 18),
          text = element_text(size = 18),
          axis.title = element_text(size = 16),
          legend.position = "bottom"
        )
      # slope
      slopex<-seq(-4,4,length=1000)
      slopey<-dnorm(slopex,mean = pr$prior$location,sd=pr$prior$adjusted_scale)
      slopedata<-data.frame(x=slopex,y=slopey)
      combined2<-
        ggplot()+
        geom_line(
          data=slopedata,
          mapping = aes(x=x,y=y,colour = "Prior")
        )+
        geom_density(
          data = postparas(),
          mapping = aes(x = postparas()[,2], colour = "Posterior")
        )
      combined2all<-
        combined2+
        geom_point(
          mapping=aes(x=c(quantile(postparas()[,2],probs=.025),quantile(postparas()[,2],probs=.975)),y=c(0,0)),
          alpha=0
        )+
        geom_errorbarh(
          aes(xmin=quantile(postparas()[,2],probs=.025),xmax=quantile(postparas()[,2],probs=.975),y=0,colour = "Credible region"),
          height=0.05*layer_scales(combined2)$y$get_limits()[2],
          size=1
        )+
        labs(
          x = "Slope", 
          y = "Density",
          alt = "The plot combined the prior distribution and the posterior plot"
        )+
        scale_x_continuous(expand = expansion(mult = 0)) +
        scale_y_continuous(expand = expansion(mult = .05))+
        scale_color_manual(
          name = NULL,
          values = c(
            "Credible region" = psuPalette[1],
            "Prior" = psuPalette[3],
            "Posterior" = psuPalette[2]
          )
        )+
        theme_bw()+
        theme(
          plot.caption = element_text(size = 18),
          text = element_text(size = 18),
          axis.title = element_text(size = 16),
          legend.position = "bottom"
        )
      # sigma
      sigmax<-seq(0,800,length=1000)
      sigmay<-dexp(sigmax,rate=1/pr$prior_aux$adjusted_scale)
      sigmadata<-data.frame(x=sigmax,y=sigmay)
      combined3<-
        ggplot()+
        geom_line(
          data=sigmadata,
          mapping = aes(x=x,y=y,colour = "Prior")
        )+
        geom_density(
          data = postparas(),
          mapping = aes(x = postparas()[,3],colour="Posterior")
        )
      combined3all<-
        combined3+
        geom_point(
          mapping=aes(x=c(quantile(postparas()[,3],probs=.025),quantile(postparas()[,3],probs=.975)),y=c(0,0)),
          alpha=0
        )+
        geom_errorbarh(
          aes(xmin=quantile(postparas()[,3],probs=.025),xmax=quantile(postparas()[,3],probs=.975),y=0,colour = "Credible region"),
          height=0.05*layer_scales(combined3)$y$get_limits()[2],
          size=1
        )+
        labs(
          x = "Sigma", 
          y = "Density",
          alt = "The plot combined the prior distribution and the posterior plot"
        )+
        scale_x_continuous(expand = expansion(mult = 0)) +
        scale_y_continuous(expand = expansion(mult = .05))+
        scale_color_manual(
          name = NULL,
          values = c(
            "Credible region" = psuPalette[1],
            "Prior" = psuPalette[3],
            "Posterior" = psuPalette[2]
          )
        )+
        theme_bw()+
        theme(
          plot.caption = element_text(size = 18),
          text = element_text(size = 18),
          axis.title = element_text(size = 16),
          legend.position = "bottom"
        )
      all<-
        ggarrange(combined1all,combined2all,combined3all,ncol=2,nrow=2,common.legend = TRUE,legend = "bottom")
      allt<-annotate_figure(
        p = all,
        top = text_grob(
          "Prior and Posterior",
          color = "black", 
          size = 22
        )
      )
      return(allt)
    }
  })
  
  ### Reactive sliderinput----
  observeEvent(
    eventExpr = input$trainingsizepreols,
    handlerExpr = {
      updateSliderInput(
        session = session,
        inputId = "trainingsizepreblr",
        value = input$trainingsizepreols
      )
    }
  )
  
  observeEvent(
    eventExpr = input$trainingsizepreblr,
    handlerExpr = {
      updateSliderInput(
        session = session,
        inputId = "trainingsizepreols",
        value = input$trainingsizepreblr
      )
    }
  )
  ### Click times----
  ### the same sample selected each time
  clicktimes<-reactive(input$newsamplepreols+input$newsamplepreblr)
  
  ## OLS Prediction----
  seeds3<-reactiveValues(seedpre=0)
  observeEvent(
    eventExpr = {
      input$trainingsizepreols
      ## the same sample selected each time
      clicktimes()
    },
    handlerExpr = {
      seeds3$seedpre<-seeds3$seedpre+1
    }
  )
  sampleRpreols<-reactive(repeatable(sample,seed = seeds3$seedpre))
  ##training data
  split2preols<-reactive(sampleRpreols()(1:nrow(satFull),size=nrow(satFull)*input$trainingsizepreols,replace = FALSE))
  trainpreols<-reactive(na.omit(satFull[split2preols(),]))
  testpreols<-reactive(na.omit(satFull[-split2preols(),]))
  ## ols model
  olsmodelpre<-reactive(lm(SATQ~SATV,data = trainpreols()))
  ## compare models:Mean Absolute Error
  output$maesols<-renderUI({
    newdatamae<-data.frame(SATV = testpreols()[,1])
    predictedmae<-predict(olsmodelpre(),newdata = newdatamae)
    mae1<-mae(actual = testpreols()[,2],predicted = predictedmae)
    withMathJax(
      p(tags$strong("Mean Absolute Error"),"for OLS = ", round(mae1,2))
    )
  })
  output$olsfitpre<-renderPlot({
    band<-
      if(input$olsbandpre == 1){
        stat_smooth(
          data=trainpreols(),
          mapping = aes(x=SATV,y=SATQ),
          method = "lm",
          formula = y~x,
          level = 0.95,
          color = psuPalette[1]
        )
      }else{
        stat_smooth(
          data = trainpreols(),
          mapping = aes(x=SATV,y=SATQ),
          method = "lm",
          formula = y~x,
          se = FALSE,
          color = psuPalette[1]
        )
      }
    p<-ggplot()+
      geom_point(
        data=trainpreols(),
        mapping = aes(x=SATV,y=SATQ),
        colour="black",
        alpha = 0.5
      )+
      band+
      labs(
        title = "Ordinary Least Squares",
        x = "SAT Verbal", 
        y = "SAT Quantitative",
        alt = "The plot shows the linear relationship between sat verbal scores and sat quantitative scores"
      )+
      theme_bw()+
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16)
      )
    return(p)
  })
  
  output$olsparapre<-renderTable({
    coeff<-coefficients(olsmodelpre())
    c2<-paste("95% Confidence interval"," lower bound",sep = "<br>")
    c3<-paste("95% Confidence interval"," upper bound",sep = "<br>")
    ctable<-matrix(
      c(coeff[1],confint(olsmodelpre())[1],confint(olsmodelpre())[3],
        coeff[2],confint(olsmodelpre())[2],confint(olsmodelpre())[4]),
      nrow=2,ncol=3,byrow = TRUE,
      dimnames = list(c("Intercept","SAT Verbal"),c("Estimate",c2,c3)))
  },rownames = TRUE,bordered = TRUE,sanitize.text.function=identity
  )
  
  output$olsestimate<-renderPlot({
    x<-input$xpointols
    newdata<-data.frame(SATV=x)
    df<-df.residual(olsmodelpre())
    pointes<-predict(olsmodelpre(),newdata = newdata)
    c<-(trainpreols()$SATV-mean(trainpreols()$SATV))^2
    mse<-mean(summary(olsmodelpre())$residuals^2)
    ci<-
      c(pointes+qt(0.025,df=df)*sqrt(mse*(1/nrow(trainpreols())+(x-mean(trainpreols()$SATV))^2/sum(c))),
        pointes+qt(0.975,df=df)*sqrt(mse*(1/nrow(trainpreols())+(x-mean(trainpreols()$SATV))^2/sum(c))))
    # all estimate 80%-90%
    estimate<-NULL
    estimatelist<-c()
    range<-0.005
    rangechange<-(0.995-0.005)/1000
    rangelist<-c()
    while(range<=0.995){
      estimate<-pointes+qt(range,df=df)*sqrt(mse*(1/nrow(trainpreols())+(x-mean(trainpreols()$SATV))^2/sum(c)))
      estimatelist<-c(estimatelist,estimate)
      rangelist<-c(rangelist,range)
      range<-range+rangechange}
    estimatelist2<-data.frame(x=estimatelist)
    ##predition-avarage point
    ave<-mean(trainpreols()$SATV)
    newdata2<-data.frame(SATV=ave)
    pointesave<-predict(olsmodelpre(),newdata = newdata2)
    # all estimate 80%-90%
    estimateave<-NULL
    estimatelistave<-c()
    rangeave<-0.005
    rangechangeave<-(0.995-0.005)/1000
    rangelistave<-c()
    while(rangeave<=0.995){
      estimateave<-pointesave+qt(rangeave,df=df)*sqrt(mse*(1/nrow(trainpreols())+(ave-mean(trainpreols()$SATV))^2/sum(c)))
      estimatelistave<-c(estimatelistave,estimateave)
      rangelistave<-c(rangelistave,rangeave)
      rangeave<-rangeave+rangechangeave}
    estimatelistave2<-data.frame(x=estimatelistave)
    endpoints<-data.frame(x=c(ci[1],ci[2]),y=c(0,0))
    p1<-ggplot()+
      geom_density(
        trim =TRUE,
        data=estimatelistave2,
        mapping = aes(x=x,colour = "Average verbal"),
        size = 1
      )+
      geom_density(
        trim =TRUE,
        data=estimatelist2,
        mapping = aes(x=x,colour = "Verbal selected"),
        size = 1
      )+
      ## 95%CI
      geom_segment(
        aes(x=ci[1],y=0,xend=ci[2],yend=0,colour = "Confidence interval"),
        size = 1
      )
    p<-
      p1+
      geom_point(
        data=endpoints,
        mapping=aes(x=x,y=y),
        alpha=0
      )+
      geom_errorbarh(
        aes(xmin=ci[1],xmax=ci[2],y=0,colour = "Confidence interval"),
        height=0.05*layer_scales(p1)$y$get_limits()[2],
        size=1
      )+
      labs(
        title =paste0("OLS Prediction for Mean Verbal vs. Verbal = ",x),
        subtitle = paste0("* Predicted values of SAT Quantitative when SAT Verbal = ",round(mean(trainpreols()$SATV),1)," vs. ",x),
        x = "Predicted SAT Quantitative Scores", 
        y = "Density",
        alt = "A plot for the point estimate"
      )+
      scale_color_manual(
        name = NULL,
        values = c(
          "Average verbal" = "black",
          "Verbal selected" = "blue",
          "Confidence interval" = psuPalette[1]
        )
      )+
      theme_bw()+
      theme(
        plot.caption = element_text(size = 18),
        plot.subtitle = element_text(lineheight = 0.9,size = 16),
        text = element_text(size = 18),
        axis.title = element_text(size = 16),
        legend.position = "bottom"
      )
    return(p)
  })
  ## table
  output$tableols<-renderTable({
    x<-input$xpointols
    newdata<-data.frame(SATV=x)
    df<-df.residual(olsmodelpre())
    pointes<-predict(olsmodelpre(),newdata = newdata)
    c<-(trainpreols()$SATV-mean(trainpreols()$SATV))^2
    mse<-mean(summary(olsmodelpre())$residuals^2)
    ci<-
      c(pointes+qt(0.025,df=df)*sqrt(mse*(1/nrow(trainpreols())+(x-mean(trainpreols()$SATV))^2/sum(c))),
        pointes+qt(0.975,df=df)*sqrt(mse*(1/nrow(trainpreols())+(x-mean(trainpreols()$SATV))^2/sum(c))))
    c1<-paste("95% Confidence interval"," lower bound",sep = "<br>")
    c2<-paste("95% Confidence interval"," upper bound",sep = "<br>")
    ctable<-matrix(c(round(ci[1],3),round(ci[2],3)),nrow=1)
    colnames(ctable)<-c(c1,c2)
    ctable
  },bordered = TRUE,sanitize.text.function=identity)
  
  ## BLR Prediction----
  seeds4<-reactiveValues(seedpre=0)
  observeEvent(
    eventExpr = {
      input$trainingsizepreblr
      ## same sample
      clicktimes()
    },
    handlerExpr = {
      seeds4$seedpre<-seeds4$seedpre+1
    }
  )
  sampleRpreblr<-reactive(repeatable(sample,seed = seeds4$seedpre))
  ##training data
  split2preblr<-reactive(sampleRpreblr()(1:nrow(satFull),size=nrow(satFull)*input$trainingsizepreblr,replace = FALSE))
  trainpreblr<-reactive(na.omit(satFull[split2preblr(),]))
  testpreblr<-reactive(na.omit(satFull[-split2preblr(),]))
  ## bayesian model
  blrmodelpre<-reactive({
    if(input$priortypepreblr=='defaultp'){
      blrmodel<-stan_glm(SATQ~SATV,data = trainpreblr(),iter=1500,chain=1)
    }
    if(input$priortypepreblr=='informp'){
      blrmodel<-
        stan_glm(SATQ~SATV,data = trainpreblr(),
                 prior = normal(location = input$beta1mupreblr,scale = input$beta1sigmapreblr),
                 prior_intercept = normal(location = input$beta0mupreblr, scale = input$beta0sigmapreblr),
                 prior_aux = exponential(rate = input$sigmaratepreblr),
                 iter=1500,
                 chain=1)
    }
    return(blrmodel)
  })
  ## compare models:Mean Absolute Error
  output$maesblr<-renderUI({
    # blr
    predictions<-posterior_predict(blrmodelpre(),newdata = testpreblr())
    means<-colMeans(predictions)
    mae2<-mean(abs(means-testpreblr()[,2]))
    withMathJax(
      p(tags$strong("Mean Absolute Error"),"for BLR = ", round(mae2,2))
    )
  })
  ## prediction--average point
  avepoint<-reactive(data.frame(SATV=mean(trainpreblr()$SATV)))
  avepredblr<-reactive(as.data.frame(posterior_linpred(blrmodelpre(),newdata = avepoint())))
  ## prediction--new point
  newpoint<-reactive(data.frame(SATV=input$xpointblr))
  newpredblr<-reactive(as.data.frame(posterior_linpred(blrmodelpre(),newdata = newpoint())))
  ## posterior
  postparaspre<-reactive(as.data.frame(blrmodelpre()))
  output$blrfitpre<-renderPlot({
    newdata<-sample_n(postparaspre(),100)
    ggplot()+
      geom_point(
        data=trainpreblr(),
        mapping = aes(x=SATV,y=SATQ),
        colour="black",
        alpha = 0.5
      )+
      geom_segment(
        data = newdata,
        mapping = aes(
          x= min(trainpreblr()$SATV), 
          xend = max(trainpreblr()$SATV),
          y = newdata[,1] + newdata[,2]*min(trainpreblr()$SATV), 
          yend = newdata[,1] + newdata[,2]*max(trainpreblr()$SATV)
        ),
        color = psuPalette[1],
        alpha = 0.1
      )+
      labs(
        title = "Bayesian Linear Regression",
        subtitle = "* 100 regression lines sampled from posterior distributions",
        x = "SAT Verbal", 
        y = "SAT Quantitative",
        alt = "The plot shows the linear relationship between sat verbal scores and sat quantitative scores"
      )+
      theme_bw()+
      theme(
        plot.caption = element_text(size = 18),
        plot.subtitle = element_text(lineheight = 0.9,size = 16),
        text = element_text(size = 18),
        axis.title = element_text(size = 16),
      )
  })
  
  output$blrparapre<-renderTable({
    c2<-paste("95% Credible interval"," lower bound",sep = "<br>")
    c3<-paste("95% Credible interval"," upper bound",sep = "<br>")
    ctable<-matrix(
      c(mean(postparaspre()[,1]),
        quantile(postparaspre()[,1],probs=.025),
        quantile(postparaspre()[,1],probs=.975),
        mean(postparaspre()[,2]),
        quantile(postparaspre()[,2],probs=.025),
        quantile(postparaspre()[,2],probs=.975),
        mean(postparaspre()[,3]),
        quantile(postparaspre()[,3],probs=.025),
        quantile(postparaspre()[,3],probs=.975)),
      nrow=3,ncol=3,byrow = TRUE,dimnames = list(c("Intercept","SAT Verbal","Sigma"),
                                                 c("Mean",c2,c3)))
  },rownames = TRUE,bordered = TRUE,sanitize.text.function=identity)
  
  
  output$bayesestimate<-renderPlot({
    ## 95% credible
    valueci<-quantile(newpredblr()[,1], probs = c(.025,.975)) 
    p1<-
      ggplot()+
      geom_density(
        trim = TRUE,
        data=avepredblr(),
        mapping = aes(x=avepredblr()[,1],color="Average verbal"),
        size = 1
      )+
      geom_density(
        trim =TRUE,
        data=newpredblr(),
        mapping = aes(x=newpredblr()[,1],color="Verbal selected"),
        size = 1
      )+
      ## 95%CI
      geom_segment(
        aes(x=valueci[1],y=0,xend=valueci[2],yend=0,colour = "Credible interval"),
        size = 1
      )
    p2<-
      p1+
      geom_point(
        mapping=aes(x=c(valueci[1],valueci[2]),y=c(0,0)),
        alpha=0
      )+
      geom_errorbarh(
        aes(xmin=valueci[1],xmax=valueci[2],y=0,colour = "Credible interval"),
        height=0.05*layer_scales(p1)$y$get_limits()[2],
        size=1
      )+
      labs(
        title = paste0("Bayesian Prediction for Mean Verbal vs. Verbal = ",input$xpointblr),
        subtitle = paste0("* Predicted values of SAT Quantitative when SAT Verbal = ",round(mean(trainpreblr()$SATV),1)," vs. ",input$xpointblr),
        x = "Predicted SAT Quantitative Scores", 
        y = "Density",
        alt = "A plot for the point estimate"
      )+
      scale_color_manual(
        name = NULL,
        values = c(
          "Average verbal" = "black",
          "Verbal selected" = "blue",
          "Credible interval" = psuPalette[1]
        )
      )+
      theme_bw()+
      theme(
        plot.caption = element_text(size = 18),
        plot.subtitle = element_text(lineheight = 0.9, size = 16),
        text = element_text(size = 18),
        axis.title = element_text(size = 16),
        legend.position = "bottom"
      )
    return(p2)
  })
  
  output$blrpriorOutputpre<-renderUI({
    validate(
      need(
        input$priortypepreblr == 'defaultp',
        "")
    )
    prp<-prior_summary(blrmodelpre())
    withMathJax(
      paste0("Intercept~N(\u03BC=",
             round(prp$prior_intercept$location,2),", \u03C3=",
             round(prp$prior_intercept$adjusted_scale,2),")"),
      br(),
      paste0("Slope~N(\u03BC=",round(prp$prior$location,2),", \u03C3=",
             round(prp$prior$adjusted_scale,2),")"),
      br(),
      paste0("Sigma~Exp(\u03BB=",
             round(1/prp$prior_aux$adjusted_scale,2),")")
    )
  })
  
  output$tableblr<-renderTable({
    # ci
    ci<-valueci<-quantile(newpredblr()[,1], probs = c(.025,.975)) 
    c1<-paste("95% Credible region"," lower bound",sep = "<br>")
    c2<-paste("95% Credible region"," upper bound",sep = "<br>")
    ctable<-matrix(c(round(ci[1],3),round(ci[2],3)),nrow=1)
    colnames(ctable)<-c(c1,c2)
    ctable
  },bordered = TRUE,sanitize.text.function=identity)
  
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
