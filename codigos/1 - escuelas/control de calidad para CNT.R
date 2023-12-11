#chequeo de calidad de los reportes

# Escuelas ----
## REVISION DE FALTANTES ----
df <- data.frame(rbd = rbds)
archivos <- list.files("D:\\Downloads\\WorkDocsDownloads\\escuelas_nov_2023")
pdfs <- data.frame(nombre_pdf = archivos)
pdfs <- pdfs %>% filter(!grepl(".tex", archivos, fixed = TRUE))
pdfs <- pdfs %>% mutate(rbd = gsub(".pdf", "", nombre_pdf)) %>% mutate(rbd = as.numeric(rbd))
head(pdfs)
nrow(pdfs)

df <- left_join(df, pdfs, by = "rbd")
pendientes <- df %>% filter(is.na(nombre_pdf))
pendientes <- unique(pendientes$rbd)
pendientes

# Sostenedores ----

#REVISION DE FALTANTES
df <- data.frame(rbd = ruts_sost )
archivos <- list.files("D:\\Downloads\\WorkDocsDownloads\\sostenedores_nov_2023")
pdfs <- data.frame(nombre_pdf = archivos)
head(pdfs)
pdfs <- pdfs%>% filter(!grepl(".tex", nombre_pdf, fixed = TRUE))
pdfs <- pdfs%>% filter(!grepl(".xlsx", nombre_pdf, fixed = TRUE))
colnames(pdfs)
pdfs <- pdfs %>% filter(nombre_pdf != "NA.pdf") %>% mutate(rbd = gsub(".pdf", "", nombre_pdf)) %>% mutate(rbd = as.numeric(rbd))
#(pdfs)
head(pdfs)
nrow(pdfs)


df <- left_join(df, pdfs, by = "rbd")
errores <- df %>% filter(is.na(nombre_pdf))
errores <- unique(errores$rbd)
errores <- errores[!is.na(errores)]
errores
length(errores)

#### EXCEL 

#REVISION DE FALTANTES
df <- data.frame(rbd = ruts_sost )
nrow(df)
archivos <- list.files("D:\\Downloads\\WorkDocsDownloads\\sostenedores_nov_2023")
pdfs <- data.frame(nombre_pdf = archivos)
head(pdfs)
colnames(pdfs)
pdfs <- pdfs %>% filter(nombre_pdf != "NA.xlsx") %>%
  mutate(rbd = gsub(".xlsx", "", nombre_pdf)) %>%
  mutate(rbd = as.numeric(rbd))
#(pdfs)
head(pdfs)
nrow(pdfs)


df <- left_join(df, pdfs, by = "rbd")
errores <- df %>% filter(is.na(nombre_pdf))
errores <- unique(errores$rbd)
errores <- errores[!is.na(errores)]
errores
length(errores)








