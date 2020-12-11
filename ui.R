library(shiny)
library(DT)
library(readr)
library(readxl)
library(tidyverse)
library(enrichR)
library(clusterProfiler)
library(enrichplot)
library(colourpicker)


ui <- pageWithSidebar(
  headerPanel('BubblePath(beta)'),
  
  
  
  sidebarPanel(
    tags$h3("Input file"),
    selectInput('groups', 'Example list', c("MTB_CRC","MTB_ESCA","G1","G2","G3","G4")),
    
    selectInput('FileSource', 'FileSource', c("Example","Upload")),
    fileInput(inputId = "file", label = "File Input:", 
              accept = c("text/csv", ".csv")),
    
    
    tags$h3("Database"),
    selectInput('genelibrary', 'GeneSet library', c("KEGG_2019_Human",
                                                    "WikiPathways_2019_Human",
                                                    "BioPlanet_2019",
                                                    "Reactome_2016")),
    
    
    tags$h3("Filtering"),
  
    numericInput('pvalue', 'pvalue', 0.05, min = 0, max = 1),
    
    selectInput("pathwaykeyword", "Pathway Filter", c("signaling pathway",
                                                            "signaling",
                                                            "all")),
    textInput("keyword_manual", label="Pathway Keyword", value=""),
    

    tags$h3("Palette"),
    colourInput("col1", label = "COLOUR1:",value="#6BC860",allowTransparent = TRUE),
    colourInput("col2", label = "COLOUR2:",value="#6FB1FC",allowTransparent = TRUE),
    colourInput("col3", label = "COLOUR3:",value="#f64f59",allowTransparent = TRUE),
    colourInput("col4", label = "COLOUR4:",value="#FF9300",allowTransparent = TRUE),
    colourInput("col5", label = "COLOUR5:",value="#6B2FDC",allowTransparent = TRUE)
    
    
    
  ),
  
  mainPanel(
    tags$h2("Enrichment Comparison"),
    plotOutput("emaplot",height = "700px"),

    
    tags$h2("Shared Pathways"),
    DTOutput("shared.pathway"),
    plotOutput("dotplot_overlap",height = "500px"),
    
    
    tags$h2("All Pathways"),
    DTOutput("pathway_all"),
    plotOutput("kegg",height = "600px")
  )
  
)
