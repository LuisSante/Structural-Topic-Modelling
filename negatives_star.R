library(stm)        
library(igraph)     
library(stmCorrViz)

# Leer los datos
data <- read.csv("C:/Users/Usuario/Documents/Excelsius/word_count/out/model_yape_target_star.csv")
data$reviews <- as.character(data$reviews) 
data$reviews[is.na(data$reviews)] <- ""

summary(data)

unique_value <- unique(data$label)
unique_value
length(unique_value)

data_negative <- subset(data, label == "Negativo")
head(data_negative)
length(data_negative$label)

summary(data_negative)

data_negative_sorted <- data_negative[order(data_negative$appVersion), ]
row.names(data_negative_sorted) <- 1:nrow(data_negative_sorted)
head(data_negative_sorted)

unique_negative_value
length(unique_negative_value)

sort_values_un <- sort(unique_negative_value)
head(sort_values_un)
sort_values_un[2]

for (version in sort_values_un[2:length(sort_values_un)]) {
  tryCatch({
    print(paste0("La version es ", version))
    dir.create(paste0("C:/Users/Usuario/Documents/RStudioCode/star_model/", version))
    setwd(paste0("C:/Users/Usuario/Documents/RStudioCode/star_model/", version))
    new_data <- subset(data_negative_sorted, appVersion == version)
    
    processed <- textProcessor(new_data$reviews, metadata=new_data)
    out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
    docs <- out$documents
    vocab <- out$vocab
    meta <- out$meta
    
    if (length(docs) <= 5){
      setwd("C:/Users/Usuario/Documents/RStudioCode/star_model/")
      next
    }
    
    K <- max(2, min(5, floor(length(docs)/50)))
    print(paste("Documents:", length(docs), "Topics:", K))
    
    meta$at <- as.numeric(as.Date(meta$at))
    meta$label <- as.factor(meta$label)
    
    stmFit <- stm(docs, vocab, K=K, prevalence=~s(at, df=5), 
                  max.em.its=75, data=meta, init.type="Spectral", 
                  seed=8458159, verbose=FALSE)
    
    if(!is.null(stmFit)) {
      # Plot generation with proper device handling
      pdf_file <- pdf("stm-plot-prevfit-summary.pdf", width=10, height=8.5)
      plot(stmFit, type="summary", xlim=c(0, .4))
      dev.off()
      
      topics_to_plot <- 1:K
      pdf("stm-plot-prevfit-labels.pdf", width=10, height=8.5)
      plot(stmFit, type="labels", topics=topics_to_plot)
      dev.off()
      
      if(K >= 2) {
        pdf("stm-plot-prevfit-perspectives.pdf", width=14, height=12.5)
        plot(stmFit, type="perspectives", topics=c(1,2))
        dev.off()
      }
      
      # Skip searchK for small K values
      if(K >= 3) {
        kResult <- searchK(docs, vocab, K=2:K, prevalence=~s(at), data=meta)
        pdf("stm-plot-searchk.pdf", width=10, height=8.5)
        plot(kResult)
        dev.off()
      }
      
      pdf("stm-plot-topic-quality.pdf", width=10, height=8.5)
      topicQuality(model=stmFit, documents=docs)
      dev.off()
      
      # File handling with explicit closes
      sink_file <- file("stm-list-label-topics-selected.txt", "w")
      sink(sink_file)
      print(labelTopics(stmFit, topics_to_plot))
      sink()
      close(sink_file)
      
      sink_file2 <- file("stm-list-sagelabel.txt", "w")
      sink(sink_file2)
      print(sageLabels(stmFit))
      sink()
      close(sink_file2)
      
      prep <- estimateEffect(1:K ~ s(at), stmFit, meta=meta, uncertainty="Global")
      pdf("stm-plot-estimate-effect.pdf", width=10, height=8.5)
      plot(prep, covariate="at", topics=topics_to_plot, model=stmFit, method="continuous",
           xlab="Time (at)", main="Effect of Time on Topics")
      dev.off()
      
      pdf("stm-plot-topic-correlations.pdf", width=10, height=8.5)
      plot(mod.out.corr)
      dev.off()
      
      # Cleanup
      graphics.off()
      closeAllConnections()
    }
    
  }, error = function(e) {
    print(paste("Error in version", version, ":", e$message))
    graphics.off()
    closeAllConnections()
    setwd("C:/Users/Usuario/Documents/RStudioCode/star_model/")
  }, finally = {
    graphics.off()
    closeAllConnections()
  })
  
  # Additional cleanup after each iteration
  graphics.off()
  closeAllConnections()
}