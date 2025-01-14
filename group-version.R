# Cargar las librerías necesarias
library(stm)        # Para modelado estructurado de temas (structural topic modeling)
library(igraph)     # Para análisis y visualización de grafos
library(stmCorrViz) # Para visualizar correlaciones entre temas de stm

# Leer los datos
data <- read.csv("https://raw.githubusercontent.com/LuisSante/sentiment-analysis/refs/heads/main/out/model_yape_target.csv")
data$reviews <- as.character(data$reviews) 
data$reviews[is.na(data$reviews)] <- ""

# Ordenar los datos filtrados por la columna 'appVersion'
data <- data[order(data$appVersion), ]
# Eliminar filas donde 'appVersion' esté vacía
data <- data[data$appVersion != "", ]

# Definir las versiones únicas principales
data$mainVersion <- as.numeric(sub("\\..*", "", data$appVersion))
mainVersions <- unique(data$mainVersion)
base_path <- "C:/Users/Usuario/Documents/RStudioCode/group_version/"

# Definir las etiquetas únicas que se analizarán con el orden que deseamos
unique_label <- c("Negativo", "Positivo", "Neutral")

contador <- 0
# Iterar sobre cada versión principal
for (mainVer in mainVersions) {
  # Crear la carpeta principal para la versión
  version_path <- paste0(base_path, "/", mainVer)
  if (!dir.exists(version_path)) {
    dir.create(version_path)
  }
  
  # Crear subset por la versión principal actual
  subDataVersion <- subset(data, mainVersion == mainVer)
  
  # Iterar sobre cada sentimiento dentro de la versión
  for (sentimiento in unique_label) {
    # Crear la carpeta para el sentimiento dentro de la versión
    sentimiento_path <- paste0(version_path, "/", sentimiento)
    if (!dir.exists(sentimiento_path)) {
      dir.create(sentimiento_path)
    }
    
    # Crear subset por sentimiento dentro de la versión actual
    subDataSentimiento <- subset(subDataVersion, label == sentimiento)
    
    # Mostrar un resumen del subconjunto de datos filtrado
    summary(subDataSentimiento)
    
    # Continuar con el procesamiento (igual que antes)
    row.names(subDataSentimiento) <- 1:nrow(subDataSentimiento)
    
    # Cambiar el directorio a la carpeta actual de sentimiento
    setwd(sentimiento_path)
    
    # Preprocesamiento del texto y entrenamiento STM (mantener la misma lógica que antes)
    tryCatch({
      # Preprocesar el texto de los datos filtrados
      processed <- textProcessor(subDataSentimiento$reviews, metadata=subDataSentimiento)
      
      # El resultado de textProcessor incluye:
      # - processed$documents: Lista compacta de documentos tokenizados.
      # - processed$vocab: Vocabulario único derivado del texto.
      # - processed$meta: Metadata asociada a los documentos.
      out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
      docs <- out$documents
      vocab <- out$vocab
      meta <- out$meta
      
      if (length(docs) <= 5) {
        setwd(base_path)
        next
      }
      
      # Determinar el número de temas (K) basado en el tamaño de los documentos
      K <- max(2, min(10, floor(length(docs) / 50)))
      print(paste("Documents:", length(docs), "Topics:", K))
      
      # Convertir la columna 'at' a formato numérico
      meta$at <- as.numeric(as.Date(meta$at))
      meta$label <- as.factor(meta$label)
      
      # Entrenar el modelo STM
      stmFit <- stm(docs, vocab, K=K, prevalence=~s(at, df=5), 
                    max.em.its=75, data=meta, init.type="Spectral", 
                    seed=8458159, verbose=FALSE)
      
      if(!is.null(stmFit)) {
        # Graficar resúmenes de temas
        pdf_file <- pdf("stm-plot-prevfit-summary.pdf", width=10, height=8.5)
        plot(stmFit, type="summary", xlim=c(0, .4))
        dev.off()
        
        # Graficar etiquetas de temas
        pdf("stm-plot-prevfit-labels.pdf", width=10, height=8.5)
        plot(stmFit, type="labels", topics=c(5, 8, 10))
        dev.off()
        
        # Graficar perspectivas entre temas
        pdf("stm-plot-prevfit-perspectives.pdf", width=14, height=12.5)
        plot(stmFit, type="perspectives", topics=c(7,10))
        dev.off()
        
        # Realizar búsqueda del mejor número de temas (searchK)
        kResult <- searchK(docs, vocab, K=c(4, 6 , 8 , 10), prevalence=~s(at), data=meta)
        pdf("stm-plot-searchk.pdf", width=10, height=8.5)
        plot(kResult)
        dev.off()
        
        # Evaluar calidad de los temas
        pdf("stm-plot-topic-quality.pdf", width=10, height=8.5)
        topicQuality(model=stmFit, documents=docs)
        dev.off()
        
        # Etiquetar temas
        labelTopicsSel <- labelTopics(stmFit, c(3, 7, 10))
        sink("stm-list-label-topics-selected.txt", append=FALSE, split=TRUE)
        print(labelTopicsSel)
        sink()
        
        # Etiquetar temas con Sage Labels
        sink("stm-list-sagelabel.txt", append=FALSE, split=TRUE)
        print(sageLabels(stmFit))
        sink()
        
        # Obtener ejemplos de documentos relacionados con temas
        thoughts7 <- findThoughts(stmFit, texts=meta$reviews, n=3, topics=7)$docs[[1]]
        pdf("stm-plot-thoughts7.pdf", width=10, height=8.5)
        plotQuote(thoughts7, width=40, main="Topic 7")
        dev.off()
        
        # Estimar efectos de las covariables
        prep <- estimateEffect(1:10 ~ s(at), stmFit, meta=meta, uncertainty="Global")
        pdf("stm-plot-estimate-effect.pdf", width=10, height=8.5)
        plot(prep, covariate="at", topics=c(3, 7, 10), model=stmFit, method="continuous",
             xlab="Time (at)", main="Effect of Time on Topics")
        dev.off()
        
        # Graficar correlación entre temas
        mod.out.corr <- topicCorr(stmFit)
        pdf("stm-plot-topic-correlations.pdf", width=10, height=8.5)
        plot(mod.out.corr)
        dev.off()
        
        # Cleanup
        graphics.off()
        closeAllConnections()
      }
  
      
    }, error = function(e) {
      print(paste("Error in version", mainVer, "sentimiento", sentimiento, ":", e$message))
    }, finally = {
      graphics.off()
      closeAllConnections()
    })
  }
}
