#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),  # Set up shinyjs
    
    # Application title
    titlePanel("Should I implement the complete izolation in my country?"),
    p("Dear president of a country,"),
    p(HTML("This simulator intends to show you the impact of your political decisions.\
      In particular it estimates the Coronavirus death toll and makes some estimates of your chances of getting reellected.\
      <b>These are very rough estimates</b>, but they give you an idea that despite the curve\
      being flat at the beginning, it goes crazy after a couple of weeks.")),
    p(HTML("If your are not the leader of your country, kindly forward it to your president, prime minister, or supreme leader. &#128540;"),style="color: #aa0000"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("population",
                        "Population of your country in mln:",
                        min = 1,
                        max = 1500,
                        value = 350),
            sliderInput("izolation",
                        "The day at which you plan to implement the total lockdown (counting from patient 100)",
                        min = 1,
                        max = 50,
                        value = 40),
            h4(a(id = "toggleAdvanced", "Show/hide advanced info", style="cursor: pointer;"),style="text-align: center;"),
            shinyjs::hidden(
                div(id = "advanced",
                    p("Dear president, here are some extra model assumptions, consult it with your epidemiologist!"),
                    sliderInput("capacity",
                                "Ventilators per thousand citizens",
                                min = 0,
                                max = 1,
                                value = 0.5),
                    sliderInput("rate_daily",
                                "Daily increase with no policy (%)",
                                min = 0,
                                max = 100,
                                value = 33),
                    HTML("<p style='font-size: 10pt'>How quickly does the number of new cases increase every day without any policies.</br><br/></p>"),
                    sliderInput("rate_policy",
                                "Daily decrease with a policy (%)",
                                min = 0,
                                max = 100,
                                value = 20),
                    HTML("<p style='font-size: 10pt'>How quickly does the number of new cases decrease every day with your policy.<br/><br/></p>"),
                    sliderInput("days",
                                "Number of day of simulation",
                                min = 0,
                                max = 100,
                                value = 50)
                )
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            htmlOutput("deathsText"),
            plotOutput("casesPlot"),
            plotOutput("cumsumPlot"),
            tags$head(tags$style(type="text/css", ".container-fluid {  max-width: 900px; }")),
        )
    ),
    hr(),
    h3("Further readings"),
    HTML("This is definitely not an exhaustive analysis of the situation and should be taken as educational material.\
         It's just an illustration that one day delay in implementation of policies can cost you thousands of lives.\
         DO NOT TAKE ACTUAL POLICICAL DECISIONS BASED ON THAT. Consult your epidemiologists.<br/><br/>\
         This simulator was motivated by some of the online readings on coronavirus and epidemiology:\
         <ul><li><a href='https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf'>Impact of non-pharmaceutical interventions (NPIs) to reduce COVID-19 mortality and healthcare demand</a>
         <li><a href='https://medium.com/@tomaspueyo/coronavirus-act-today-or-people-will-die-f4d3d9cd99ca'>Coronavirus: Why You Must Act Now</a>
         <li><a href='https://medium.com/@ariadnelabs/social-distancing-this-is-not-a-snow-day-ac21d7fa78b4'>Social Distancing: This is Not a Snow Day</a></ul>
         If you want to improve my elementary model and simulations, push to <a href='http://github.com/kidzik/corona'>this repository</a>.\
         You can learn more about the author <a href='http://kidzinski.com/'>here</a>."),
    
    HTML("<br><br>")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    cex = 1.3
    shinyjs::onclick("toggleAdvanced",
                     shinyjs::toggle(id = "advanced", anim = TRUE))  

    model <- eventReactive(c(input$izolation, input$population, input$days, input$rate_policy, input$rate_daily, input$capacity), {
        # generate bins based on input$bins from ui.R
        capacity = input$capacity * input$population * 10**3
        days = input$days
        cases = 100
        deaths = 1
        for (i in 1:(days-1)){
            if (i >= input$izolation)
                rate = (100 - input$rate_policy)/100
            else
                rate = (100 + input$rate_daily)/100
            
            susceptible = (input$population * 10**6 - sum(cases))/(input$population * 10**6)
            
            current_cases = cases[i] * rate * susceptible
            cases = c(cases, current_cases)
            current_deaths = min(current_cases, capacity) * 0.03 + max(0, current_cases - capacity) * 0.2
            deaths = c(deaths, current_deaths)
        }
        list(cases=cases, deaths=deaths, capacity=capacity)
    })
    
    output$casesPlot <- renderPlot({
        mod_list = model()
        
        scale = 1/1000000
        
        # draw the histogram with the specified number of bins
        plot(1:input$days,
             mod_list$cases*scale,
             type='l',
             col='blue',
             ylim = c(0,max(mod_list$cases,mod_list$capacity)*scale),
             ylab="New cases daily in millions",
             xlab="Number of days from patient #100",
             lwd=3,
             cex.lab=cex, cex.axis=cex, cex.main=cex, cex.sub=cex
        )
        legend("topleft", inset=.05,
               c("new cases","new deaths","hospitals capacity"),col=c("blue","red","black"),lwd=c(2,2,1))
        lines(1:input$days,
              mod_list$deaths*scale,
              col='red',
              lwd=3)
        title("Daily new cases")
        abline(h = mod_list$capacity*scale)
    })

    output$cumsumPlot <- renderPlot({
        mod_list = model()
        
        scale = 1/1000000
        
        # draw the histogram with the specified number of bins
        plot(1:input$days,
             cumsum(mod_list$cases)*scale,
             type='l',
             col='blue',
             ylim = c(0,max(sum(mod_list$cases),mod_list$capacity)*scale),
             ylab="Cumulative number of cases in millions",
             xlab="Number of days from patient #100",
             lwd=3,
             cex.lab=cex, cex.axis=cex, cex.main=cex, cex.sub=cex
        )
        legend("topleft", inset=.05,
               c("total cases","total deaths"),col=c("blue","red"),lwd=c(2,2))
        lines(1:input$days,
              cumsum(mod_list$deaths)*scale,
              col='red',
              lwd=3)
        title("Cumulative infections and death toll")
        abline(h = mod_list$capacity*scale)
    })
    
    output$deathsText <- renderText({
        mod_list = model()
        infected = ceiling(sum(mod_list$cases))
        deaths = ceiling(sum(mod_list$deaths))
        pop_prec = round((infected / (input$population*1000000))*100,4)
        pop_prec_deaths = round((deaths / (input$population*1000000))*100,4)
        
        if (pop_prec > 0.1){
            result = "You can do much better than that. Try harder or you won't be reelected!"
        }
        if (pop_prec <= 0.1){
            result = "Not bad! Maybe you can get reelected!"
        }
        
        paste0("<div style='text-align: center'><h3>Personalized report of your policy</h3>",
               "<p>As a result of your policies, coronavirus killed</p><p style='font-size: 20pt'>",
               format(deaths, big.mark=",", nsmall=0),
               "</p><p>people. It also infected</p><p style='font-size: 20pt'>",
               format(infected, big.mark=",", nsmall=0),
               "</p><p>people which is about</p><p style='font-size: 20pt'>",
               pop_prec,"%",
               "</p><p>of your population.</p>",
               "<p><b>",result,"</b></p></div>")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
