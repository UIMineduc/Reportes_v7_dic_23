#chequeo de calidad de los reportes

#REVISION DE FALTANTES
df <- data.frame(rbd = ruts_sost )
archivos <- list.files("F:\\MINEDUC\\CNT\\Reportes\\2023\\3° Reporte Trayectorias (Septiembre)\\Sostenedores\\Sostenedores (PDF)")
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
#chequeo de calidad de los reportes

#REVISION DE FALTANTES
df <- data.frame(rbd = ruts_sost )
nrow(df)
archivos <- list.files("F:\\MINEDUC\\CNT\\Reportes\\2023\\3° Reporte Trayectorias (Septiembre)\\Sostenedores\\Sostenedores (Excel)")
pdfs <- data.frame(nombre_pdf = archivos)
head(pdfs)
colnames(pdfs)
pdfs <- pdfs %>% filter(nombre_pdf != "NA.xlsx") %>% mutate(rbd = gsub(".xlsx", "", nombre_pdf)) %>% mutate(rbd = as.numeric(rbd))
#(pdfs)
head(pdfs)
nrow(pdfs)


df <- left_join(df, pdfs, by = "rbd")
errores <- df %>% filter(is.na(nombre_pdf))
errores <- unique(errores$rbd)
errores <- errores[!is.na(errores)]
errores
length(errores)








