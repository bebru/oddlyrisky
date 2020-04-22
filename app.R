
source(here::here("global.R"))

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Oddly risky"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("prob_V1",
                        "Probability of x1:",
                        min = 0,
                        max = 1,
                        value = 0.8),
            sliderInput("coef_b0",
                        "Coefficient log(b0):",
                        min = 0,
                        max = 5,
                        value = 0.5,
                        step = 0.1),
            sliderInput("coef_b1",
                        "Coefficient log(b1):",
                        min = 0,
                        max = 5,
                        value = 2,
                        step = 0.1)
        ),

        # Show a plot
        # for multiple plots:
        # https://stackoverflow.com/questions/34384907/how-can-put-multiple-plots-side-by-side-in-shiny-r
        mainPanel(
           plotOutput("patchPlot")
        )
    )
)

# Define server logic
server <- function(input, output) {

    # number of observatiosn
    n <- 1e4
    
    output$patchPlot <- renderPlot({
        
        # betas
        b0 <- log(input$coef_b0)
        b <- log(input$coef_b1)
        
        # generate data
        ds <- sim_ds(n = n, x = input$prob_V1, b0 = b0, b = b)
        mod <- glm( y~., data=ds, family="binomial")
        
        # prevalence
        tbl_prev <- tally_discrete(ds)
        plt_prev <- plot_prev(tbl_prev)
        
        # conditional prevalence
        tbl_prev_cond <- tally_discrete_cond(ds)
        plt_prev_cond <- plot_prev_cond(tbl_prev_cond)
        
        # OR
        mod_or <- calc_or(mod = mod)
        plt_or <- plot_or(mod_or)
        # AME
        mod_ame <- margins(mod)
        plt_ame <- plot_ame(mod_ame)
        
        # patch plots together
        plt_prev / plt_prev_cond / plt_or / plt_ame
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
