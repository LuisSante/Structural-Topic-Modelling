library(stm)        
library(igraph)     
library(stmCorrViz)

# Leer los datos
data <- read.csv("C:/Users/Usuario/Documents/Excelsius/word_count/out/model_yape_target.csv")
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

unique_negative_value <- unique(data_negative$appVersion)
unique_negative_value
length(unique_negative_value)

sort_values_un <- sort(unique_negative_value)
head(sort_values_un)
sort_values_un[2]

for (version in sort_values_un[2:length(sort_values_un)]) {
  tryCatch({
    print(paste0("La version es ", version))
    dir.create(paste0("C:/Users/Usuario/Documents/RStudioCode/our_model/", version))
    setwd(paste0("C:/Users/Usuario/Documents/RStudioCode/our_model/", version))
    new_data <- subset(data_negative_sorted, appVersion == version)
    
    processed <- textProcessor(new_data$reviews, metadata=new_data)
    out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
    docs <- out$documents
    vocab <- out$vocab
    meta <- out$meta
    
    # More stringent minimum document check
    if (length(docs) <= 10){
      setwd("C:/Users/Usuario/Documents/RStudioCode/our_model/")
      next
    }
    
    # More conservative K calculation
    K <- max(2, min(5, floor(length(docs)/50)))
    print(paste("Documents:", length(docs), "Topics:", K))
    
    meta$at <- as.numeric(as.Date(meta$at))
    meta$label <- as.factor(meta$label)
    
    stmFit <- stm(docs, vocab, K=K, prevalence=~s(at, df=5), 
                  max.em.its=75, data=meta, init.type="Spectral", 
                  seed=8458159, verbose=FALSE)
    
    # Only proceed with plots if model converged
    if(!is.null(stmFit)) {
      pdf("stm-plot-prevfit-summary.pdf", width=10, height=8.5)
      plot(stmFit, type="summary", xlim=c(0, .4))
      dev.off()
      
      topics_to_plot <- 1:K
      pdf("stm-plot-prevfit-labels.pdf", width=10, height=8.5)
      plot(stmFit, type="labels", topics=topics_to_plot)
      dev.off()
      
      labelTopicsSel <- labelTopics(stmFit, topics_to_plot)
      sink("stm-list-label-topics-selected.txt")
      print(labelTopicsSel)
      sink()
    }
    
  }, error = function(e) {
    print(paste("Error in version", version, ":", e$message))
    setwd("C:/Users/Usuario/Documents/RStudioCode/our_model/")
  })
}