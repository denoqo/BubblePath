function(input, output) {
  

  

  gene.list <- reactive({
    if((input$FileSource=="Example")){
      read_excel(str_glue("data/",input$groups,".xlsx"), sheet = "gene")
    } else if(input$FileSource=="Upload") {
      read_csv(input$file$datapath)
    }
  })
    
    

  
#  gene.list <- reactive({
#    read_excel(str_glue("data/",input$groups,".xlsx"), sheet = "gene")
#  })
  

#  gene.list <- reactive({
#    read_csv(input$file$datapath)
#  })
  
  
  ListType <-reactive({
    colnames(gene.list())
  })
  pk<-reactive({
    if((input$pathwaykeyword=="all")){
      input$keyword_manual
    } else {
      input$pathwaykeyword
    }
  })
    

  sub_pathway_enrichR <- reactive({

    enrichr.list<-list()
    for (i in 1:length(ListType())){
      tag<-ListType()[i]
      genelist<-gene.list()[[tag]]%>%na.omit()%>%as.vector()
      dbs<-input$genelibrary
      enrichR <- enrichr(genelist,dbs)[[dbs]]%>%
        as_tibble()%>%mutate(P.value=P.value%>%round(3))%>%
        dplyr::rename(`P-value`=P.value,
                                    `Adjusted P-value`=Adjusted.P.value)
      pathway <- enrichR%>%
        mutate(Genes=gsub("[;]","/",Genes))  %>% #change gene list
        mutate(Cluster=tag)%>%mutate(across(Cluster, factor)) #add cluster name
      assign(tag,pathway)
      enrichr.list[[tag]]<-pathway
    }
    bind_rows(enrichr.list)
  })
  
  
  sub_pathway0 <- reactive({
    data = sub_pathway_enrichR()%>%
      filter(str_detect(Term,regex(pk(),ignore_case = F))) %>% 
      filter(`P-value`<input$pvalue)
  })
  
  
  # output
  output$pathway_all<-({
    renderDT(
      pathway_all<-sub_pathway0()%>%
        select(Cluster,Term,`P-value`,Genes)%>%
        arrange(Cluster,`P-value`)%>%rename(Group=Cluster,
                                            All.Pathways=Term)
      )
  })
  
  
  # output
  output$shared.pathway<-
    renderDT({
      pathway_overlap<-sub_pathway0()%>%dplyr::group_by(Term)%>%filter(n()>1)%>%
        select(Cluster,Term,`P-value`,Genes)%>%arrange(Term)%>%rename(Group=Cluster,
                                                                      Shared.Pathways=Term)
    })
  
  # kegg dotpot
  #fix the order for ggplot
  sub_pathway <- reactive({
    sub_pathway0()%>%mutate(`KEGG PATHWAY`=reorder(Term, -1*log10(`P-value`)))
  })
    
  output$kegg <- renderPlot({
      ggplot(sub_pathway(),aes(-1*log10(`P-value`),`KEGG PATHWAY`),font.size = 6)+
        geom_point(aes(size=10,color=-1*log10(`P-value`)))+
        theme(text = element_text(size=10))+
        theme_light(base_size = 13)+
        scale_color_gradient(low="blue",high ="red")+
        scale_y_discrete(labels = function(x) str_wrap(x, width = 40))+
        facet_wrap(facets = "Cluster",scales = "free")+
        theme(axis.title.x = element_blank())
    })
  
  
  
  ck <- reactive({
    library(clusterProfiler)
    library(enrichplot)
    ck<-readRDS("data/ck.rds")
    ck2.2<-sub_pathway()%>%
      mutate(Cluster=Cluster,
             ID=Term,Description=Term,
             GeneRatio=1,BgRatio=1,
             pvalue=`P-value`,qvalue=1,
             geneID=Genes,Count=1)%>%
      mutate(across(Cluster, factor))
    ck2.2$Cluster<-factor(ck2.2$Cluster,levels = ListType())
    ck2.2$Cluster%>%unique
    ck@compareClusterResult <- ck2.2
    ck
  })
  
  output$emaplot <- renderPlot({
    
    #change color
    #devtools::install_github('Mikata-Project/ggthemr')
    library(ggthemr)
    fivecolor <- define_palette(
      swatch = c("black", input$col1,input$col2,input$col3,input$col4,input$col5),
      gradient = c(lower = input$col1, upper = input$col5),
      text = c("grey", "grey"));ggthemr(fivecolor)
    four <- define_palette(
      swatch = c("black", "#6BC860","#0433FF","#f64f59","#FF9300","#6B2FDC"),
      gradient = c(lower = "#1D976C", upper = "#6B2FDC"),
      text = c("grey", "grey"));#ggthemr(four)
    three <- define_palette(
      swatch = c("black", "#ed6782","#74d686","#5e9fff"),
      gradient = c(lower = "#eb59b6", upper = "#5996f7"),
      text = c("grey", "grey"));#ggthemr(three)
    #ggthemr_reset()
    
    emapplot(ck(),showCategory=20,
             layout = "kk",
             color = "pvalue",
             line_scale=0.3,
             font.size=0.1);
  })
  
  output$dotplot_overlap <- renderPlot({
    dotplot(ck(),title="KEGG Enrichment Comparison",
            color = "pvalue",
            showCategory=100)
    })

    
}      
