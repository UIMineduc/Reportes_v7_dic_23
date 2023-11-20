"Este codigo hace el traspaso de las bases de datos a formato .Rds"

pacman::p_load(tidyverse,data.table,haven)
"Debemos fijar el directorio de donde estarán las bases"

bases <- "D:\\OneDrive - Ministerio de Educación\\acceso_bbdd_reportes\\Finales\\"
salida <- "D:\\OneDrive - Ministerio de Educación\\2023 - UID\\101 Reportes\\Reportes_v7_dic_23\\Inputs\\"

files <- list.files(bases)
files
files <- files[grep("\\.csv$", files)]

for (archivo in files){
  nombre <- paste0(sub("\\.csv$", "", archivo),".rds")
  df <- fread(paste0(bases,"/",archivo),header=TRUE,encoding = "UTF-8",col.names = tolower)
  saveRDS(df,file=paste0(salida,"/",nombre))
}