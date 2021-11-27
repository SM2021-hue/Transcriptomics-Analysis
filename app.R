#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
##library(DT)
library(data.table)
library(dplyr)
library(ggplot2)
library(shiny)

ui <- shinyUI(fluidPage(
    fluidRow(
        column(
            6, 
            h1("Gene Expressiom  boxplots"),
            h2("Case(P.A) Vs. Control"),
            DT::dataTableOutput("table_results2")
        ),
        column(1),
        column(
            5,
            h1("Pick gene"),
            textInput(inputId = "gene", 
                      label = "Type geneID in the box", 
                      value = "ABCA13"),
            plotOutput("selected_gene", height = "450px")
        )
    )
))



results2 <- read.csv("results2.csv", row.names = "X") %>%
    select(log2FoldChange, padj)
pheno <- read.csv("Pheno.csv")
case <- pheno %>%
    filter(Diagnosis == "Pathologic Aging") %>%
    pull(UID)
control <- pheno %>%
    filter(Diagnosis == "Control") %>%
    pull(UID)

expression <- fread("expression.csv")

server <- shinyServer(function(input, output) {
    output$table_results2 <- DT::renderDataTable({
        results2
    },
    rownames = TRUE,
    filter = list(position = 'top', clear = FALSE),
    options = list(
        pageLength = 10,
        processing=FALSE
    ))
    
    output$selected_gene <- renderPlot({
        gene <- input$gene
        gene_expr <- expression[V1 == gene] %>%
            .[, V1 := NULL]
        # remove "X" at the beginning of column name, which is add to column
        # name as number is not allowed to start
        gene_expr <- data.table(UID = sub(".", "", names(gene_expr)), 
                                expr = as.matrix(gene_expr)[1,]) %>%
            .[UID %in% case, diagnosis := "Pathologic Aging"] %>%
            .[UID %in% control, diagnosis := "Control"]
        
        ggplot(gene_expr, aes(diagnosis, expr)) +
            geom_jitter(width = 0.1, height = 0, color = "grey") +
            geom_boxplot(fill = NA) +
            labs(x = "Diagnosis", 
                 y = "Expression") +
            theme_bw()
        
    })
})


# Run the application 
shinyApp(ui = ui, server = server)
