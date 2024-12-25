library(stm)        
library(igraph)     
library(stmCorrViz)

# Leer los datos
data <- read.csv("C:/Users/Usuario/Documents/Excelsius/word_count/out/model_yape_target.csv")
data$reviews <- as.character(data$reviews) 
data$reviews[is.na(data$reviews)] <- ""

# Convertir las etiquetas en factores
data$label <- as.factor(data$label)

# Procesamiento de texto
processed <- textProcessor(data$reviews, metadata=data)

# Preparar documentos para el modelo STM
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# Graficar palabras eliminadas según el umbral
pdf("stm-plot-removed.pdf", width=10, height=8.5)
plotRemoved(processed$documents, lower.thresh=seq(1, 200, by=100))
dev.off()

meta$at <- as.Date(meta$at)  # Convertir 'at' a formato de fecha
#meta$days_since_start <- as.numeric(meta$at - min(meta$at, na.rm=TRUE))  # Diferencia en días
meta$label <- as.factor(meta$label) 
meta$label
length(meta$label)

# Ajustar el modelo STM con 20 temas, incluyendo "label" como prevalencia
stmFit <- stm(docs, vocab, K=20, prevalence=~label + s(at), 
              max.em.its=75, data=meta, init.type="Spectral", seed=8458159)

# Visualización básica del modelo
pdf("stm-plot-prevfit-summary.pdf", width=10, height=8.5)
plot(stmFit, type="summary", xlim=c(0, .4))
dev.off()

# Visualización de etiquetas para temas seleccionados
pdf("stm-plot-prevfit-labels.pdf", width=10, height=8.5)
plot(stmFit, type="labels", topics=c(3, 7, 20))
dev.off()

# Visualización de perspectivas entre dos temas
pdf("stm-plot-prevfit-perspectives.pdf", width=14, height=12.5)
plot(stmFit, type="perspectives", topics=c(7, 10))
dev.off()

# Identificar el número óptimo de temas (K)
kResult <- searchK(docs, vocab, K=c(7, 10, 15, 20), prevalence=~label + s(at), data=meta)
pdf("stm-plot-searchk.pdf", width=10, height=8.5)
plot(kResult)
dev.off()

# Analizar la calidad de los temas
pdf("stm-plot-topic-quality.pdf", width=10, height=8.5)
topicQuality(model=stmFit, documents=docs)
dev.off()

# Etiquetar temas
labelTopicsSel <- labelTopics(stmFit, c(3, 7, 20))
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
prep <- estimateEffect(1:20 ~ label + s(at), stmFit, meta=meta, uncertainty="Global")
pdf("stm-plot-estimate-effect.pdf", width=10, height=8.5)
plot(prep, covariate="label", topics=c(3, 7, 20), model=stmFit, method="difference",
     cov.value1="Positivo", cov.value2="Neutral", xlab="Neutral ... Positivo",
     main="Effect of Sentiment on Topics", xlim=c(-.15, .15))
dev.off()

# Correlación entre temas
mod.out.corr <- topicCorr(stmFit)
pdf("stm-plot-topic-correlations.pdf", width=10, height=8.5)
plot(mod.out.corr)
dev.off()

##########################################################

summary(processed$docs.removed)
max(processed$docs.removed) <= nrow(data)

anyDuplicated(data$reviews)
anyDuplicated(processed$docs.removed)

remaining_indices <- setdiff(seq_len(nrow(data)), processed$docs.removed)
length(remaining_indices)

filtered_data <- filtered_data[seq_len(length(docs)), ]
identical(filtered_data$content_join, processed$meta$content_join[remaining_indices])

##########################################################

# Visualización interactiva
stmCorrViz(
  stmFit, 
  "stm-interactive-correlation.html", 
  documents_raw = filtered_data$content_join, 
  documents_matrix = docs
)
