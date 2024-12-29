library(stm)        
library(igraph)     
library(stmCorrViz)

# Leer los datos
data <- read.csv("C:/Users/Usuario/Documents/Excelsius/word_count/out/model_yape_target_star.csv")
data$reviews <- as.character(data$reviews) 
data$reviews[is.na(data$reviews)] <- ""

summary(data)

unique_value <- unique(data$label)
length(unique_value)

data_negative <- subset(data, label == "Negativo")
head(data_negative)
length(data_negative$label)

summary(data_negative)

data_negative_sorted <- data_negative[order(data_negative$appVersion), ]
data_negative_sorted <- data_negative_sorted[data_negative_sorted$appVersion != "", ]
row.names(data_negative_sorted) <- 1:nrow(data_negative_sorted)
head(data_negative_sorted)

unique_version <- unique(data_negative_sorted$appVersion)
length(unique_version)

most_representative <- c()
for (version in unique_version[1:length(unique_version)]) {
    new_data <- subset(data_negative_sorted, appVersion == version)
    
    if (length(new_data$appVersion) > 200){
      most_representative <- append(most_representative, length(new_data$appVersion))
      print(paste0("Version ", version, " Tamaño ", length(new_data$reviews)))
    }
      
}

most_representative <- sort(most_representative)
lenght_subsets <- unique(tail(most_representative, 4))

version_representative <- c()
for (size in lenght_subsets) {
  for (version in unique_version) {
    new_data <- subset(data_negative_sorted, appVersion == version)
    
    if (length(new_data$appVersion) == size) {
      version_representative <- append(version_representative, version)
    }
  }
}


for (version in version_representative) {
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
    
    # Graficar palabras eliminadas según el umbral
    pdf("stm-plot-removed.pdf", width=10, height=8.5)
    plotRemoved(processed$documents, lower.thresh=seq(1, 200, by=100))
    dev.off()
    
    if (length(docs) <= 5){
      setwd("C:/Users/Usuario/Documents/RStudioCode/star_model/")
      next
    }
    
    K <- max(2, min(10, floor(length(docs)/50)))
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
      
      pdf("stm-plot-prevfit-labels.pdf", width=10, height=8.5)
      plot(stmFit, type="labels", topics=c(5, 8, 10))
      dev.off()
      
      pdf("stm-plot-prevfit-perspectives.pdf", width=14, height=12.5)
      plot(stmFit, type="perspectives", topics=c(7,10))
      dev.off()
    
      # Skip searchK for small K values
      kResult <- searchK(docs, vocab, K=c(5, 8, 10), prevalence=~s(at), data=meta)
      pdf("stm-plot-searchk.pdf", width=10, height=8.5)
      plot(kResult)
      dev.off()
      
      pdf("stm-plot-topic-quality.pdf", width=10, height=8.5)
      topicQuality(model=stmFit, documents=docs)
      dev.off()
      
      # Etiquetar temas
      labelTopicsSel <- labelTopics(stmFit, c(3, 7, 10))
      sink("stm-list-label-topics-selected.txt", append=FALSE, split=TRUE)
      print(labelTopicsSel)
      sink()
      
      # Palabras clave con Sage Labels
      sink("stm-list-sagelabel.txt", append=FALSE, split=TRUE)
      print(sageLabels(stmFit))
      sink()
      
      # Obtener ejemplos de documentos para cada tema
      thoughts7 <- findThoughts(stmFit, texts=meta$reviews, n=3, topics=7)$docs[[1]]
      pdf("stm-plot-thoughts7.pdf", width=10, height=8.5)
      plotQuote(thoughts7, width=40, main="Topic 7")
      dev.off()
      
      meta$at <- as.numeric(as.Date(meta$at))
      
      # Estimar efectos de covariables, incluyendo comparación entre etiquetas
      prep <- estimateEffect(1:10 ~ s(at), stmFit, meta=meta, uncertainty="Global")
      pdf("stm-plot-estimate-effect.pdf", width=10, height=8.5)
      plot(prep, covariate="at", topics=c(3, 7, 10), model=stmFit, method="continuous",
           xlab="Time (at)", main="Effect of Time on Topics")
      dev.off()
      
      # Correlación entre temas
      mod.out.corr <- topicCorr(stmFit)
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
