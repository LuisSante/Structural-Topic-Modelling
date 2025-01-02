# Cargar las librerías necesarias
library(stm)        # Para modelado estructurado de temas (structural topic modeling)
library(igraph)     # Para análisis y visualización de grafos
library(stmCorrViz) # Para visualizar correlaciones entre temas de stm

# Leer los datos
data <- read.csv("C:/Users/Usuario/Documents/Excelsius/word_count/out/model_yape_target.csv")
data$reviews <- as.character(data$reviews) 
data$reviews[is.na(data$reviews)] <- ""

# Mostrar un resumen de las variables en el dataset
summary(data)

# Definir las etiquetas únicas que se analizarán con el orden que deseamos
unique_label <- c("Negativo", "Positivo", "Neutral")

# Ruta base para guardar los resultados
base_path <- "C:/Users/Usuario/Documents/RStudioCode/our_model/"

# Iterar sobre cada etiqueta en unique_label
for (sentimiento in unique_label) {
  
  # Crear la carpeta principal para el sentimiento
  sentimiento_path <- paste0(base_path, sentimiento)
  if (!dir.exists(sentimiento_path)) {
    dir.create(sentimiento_path)
  }
  
  # Filtrar el dataset por la etiqueta actual
  subData <- subset(data, label == sentimiento)
  
  # Mostrar un resumen del subconjunto de datos filtrado
  summary(subData)
  
  # Ordenar los datos filtrados por la columna 'appVersion'
  subDataSorted <- subData[order(subData$appVersion), ]
  
  # Eliminar filas donde 'appVersion' esté vacía
  subDataSorted <- subDataSorted[subDataSorted$appVersion != "", ]
  
  # Asignar nuevos índices a las filas del subconjunto ordenado
  row.names(subDataSorted) <- 1:nrow(subDataSorted)
  
  # Mostrar las primeras filas del subconjunto ordenado
  head(subDataSorted)
  
  # Extraer el número principal de la versión (primer dígito antes de ".")
  subDataSorted$mainVersion  <- as.numeric(sub("\\..*", "", subDataSorted$appVersion))
  
  # Obtener las versiones únicas principales
  mainVersions <- unique(subDataSorted$mainVersion)
  
  # Crear subsets para cada versión principal
  for (mainVer in mainVersions) {
    tryCatch({
      # Crear la carpeta para la versión dentro de la carpeta del sentimiento
      version_path <- paste0(sentimiento_path, "/", mainVer)
      if (!dir.exists(version_path)) {
        dir.create(version_path)
      }
      
      # Cambiar el directorio de trabajo a la carpeta creada para la versión
      setwd(version_path)
      
      # Crear un subconjunto de datos para la versión principal actual
      subDataByVersion <- subset(subDataSorted, mainVersion == mainVer)
      
      # Preprocesar el texto de los datos filtrados
      processed <- textProcessor(subDataByVersion$reviews, metadata=subDataByVersion)
      
      # El resultado de textProcessor incluye:
      # - processed$documents: Lista compacta de documentos tokenizados.
      # - processed$vocab: Vocabulario único derivado del texto.
      # - processed$meta: Metadata asociada a los documentos.
      out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
      docs <- out$documents
      vocab <- out$vocab
      meta <- out$meta
      
      # Graficar palabras eliminadas según el umbral
      pdf("stm-plot-removed.pdf", width=10, height=8.5)
      plotRemoved(processed$documents, lower.thresh=seq(1, 200, by=100))
      dev.off()
      
      # Si hay menos de 5 documentos, pasar a la siguiente iteración
      if (length(docs) <= 5){
        setwd(base_path)
        next
      }
      
      # Determinar el número de temas (K) basado en el tamaño de los documentos
      K <- max(2, min(10, floor(length(docs)/50)))
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
      
      
    }, error = function(e){
      print(paste("Error in version", version, ":", e$message))
      graphics.off()
      closeAllConnections()
      setwd("C:/Users/Usuario/Documents/RStudioCode/our_model/")
    }, finally = {
      graphics.off()
      closeAllConnections()
    })
  }
}
