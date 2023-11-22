
#title: "Política de Reactivación Educativa Integral"
#author: "Pablo Espinosa - Centro de Estudios MINEDUC. Mayo 2023"
#Editor: "Alonso Arraño - CEM. Agosto 2023"

#**************************************************************************************************************************/
# 0. Ajustes iniciales  ----------------------------------------------------------------
#**************************************************************************************************************************/


#Sys.setlocale("LC_ALL","Spanish")
Sys.setlocale("LC_ALL","en_US.UTF-8")
library(pacman)

p_load(tidyverse, rio, dplyr, ggplot2, stringr, tidyr, kableExtra, 
       texreg, Publish, broom, lubridate, labelled, pivottabler, knitr, data.table, plotly, 
       tictoc, htmlwidgets, webshot, tinytex,here,pander)


options(scipen=999) # Desactiva la notaciÃ³n cientÃfica
options(max.print = 99999999) # Max print de la consola
options(width = 1000)
opts_chunk$set(  fig.align='center',
                 external=TRUE,
                 echo=TRUE,
                 warning=FALSE,
                 fig.pos='H')
# 
#install.packages("orca")
#library("orca")

# install.packages('reticulate')
# reticulate::install_miniconda()
reticulate::conda_install('r-reticulate', 'python-kaleido')
reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
reticulate::use_miniconda('r-reticulate')


#**************************************************************************************************************************/
# 1. Lectura de BBDD  ----------------------------------------------------------------
#**************************************************************************************************************************/

#Sys.setlocale("LC_ALL", "ES_ES.UTF-8")
#Sys.setlocale("LC_ALL","en_US.UTF-8")

#Carga de BD de datos
#{
#Se lee datos de estudiantes desvinculados y de asistencia
bd <- fread("Inputs/Desvinculacion_agosto_2023.csv", encoding="UTF-8")
colnames(bd) <- tolower(colnames(bd))

bd_asis <- fread("Inputs/202309_inasistencia_grave_agosto.csv", encoding="UTF-8")
colnames(bd_asis) <- tolower(colnames(bd_asis))


#Desvinculados intranual
bd_desvinc_intran <- fread("Inputs/retirados_sin_mat_agosto_parv_val_v2.csv", encoding="UTF-8")
colnames(bd_desvinc_intran) <- tolower(colnames(bd_desvinc_intran))
bd_desvinc_intran <- bd_desvinc_intran %>% mutate(rbd_ret = rbd)
bd_desvinc_intran <- bd_desvinc_intran %>% mutate(run_alu2 = ifelse(dgv_alu != "", paste0(run_alu, "-", dgv_alu), run_alu))
bd_desvinc_intran <- bd_desvinc_intran %>% mutate(gen_alu = case_when(gen_alu == 1 ~ "M", gen_alu == 2 ~ "F", gen_alu == 0 ~ ""))
head(bd_desvinc_intran$run_alu2)

#Se lee nombre de BD que tiene los nombres de los grados.
nom_grad <- fread("Inputs/Nombre grados.csv")
colnames(nom_grad) <- tolower(colnames(nom_grad))
# length(unique(nom_grad$nom_grado))
unicos <- unique(nom_grad$nom_grado)
unicos

orden_grados <- c("Sala Cuna Menor", "Sala Cuna Mayor","Sala Cuna Heterogéneo",  "Nivel Medio Menor", "Nivel Medio Mayor", "Nivel Medio Heterogéneo", "Pre-kinder", "Kinder", "Nivel Transición Heterogéneo",
"Heterogéneo", "Atención Temprana", "Laboral 1", "Laboral 2", "Laboral 3", "Laboral 4", "1° básico", "2° básico", "3° básico", "4° básico", "5° básico", "6° básico", "7° básico", "8° básico", "1° medio", "2° medio", "3° medio HC", "3° medio TP","3° medio Artístico", 
"4° medio TP", "4° medio HC", "4° medio Artístico", "1° a 4° básico Adult.", "5° y 6° básico Adult.", "7° y 8° básico Adult.", "1° y 2° medio Adult.",  "3° medio TP Adult.", "4° medio TP Adult.", 
"3° y 4° medio HC Adult.")

nom_grad$nom_grado <- factor(nom_grad$nom_grado, levels = orden_grados, labels = orden_grados)


#Sys.setlocale("LC_ALL","Spanish")
#}

#Doble desivnculados (de 2021 a 2023)
doble_desvinc <- fread("Inputs/Desvinculados_2021_2023.csv")


#descarga de textos para tablas de variables.
textos_variables_desvinculados <- openxlsx::read.xlsx("Inputs/Tabla variables.xlsx", sheet = "Desvinculados")
textos_variables_retirados <- openxlsx::read.xlsx("Inputs/Tabla variables.xlsx", sheet = "Retirados")
textos_variables_asistencia <- openxlsx::read.xlsx("Inputs/Tabla variables.xlsx", sheet = "Asistencia")
textos_variables_doble_desvinculados <- openxlsx::read.xlsx("Inputs/Tabla variables.xlsx", sheet = "Doble_desvinculados")

#Se selecciona rut y columnas de deserción para hacer la comparación
bd_desvinc_intran_r <- bd_desvinc_intran %>%  select("run_alu", "validacion_estudios",  "desertor_intra")#,  "desertor_intra_validacion")   #select(2, 67, 68, 70, 71)   
colnames(bd_desvinc_intran)

#Se crea otro data frame con la columnas de datos de los estudiantes retirados.
bd_desvinc_intran_r2 <- bd_desvinc_intran %>% select("run_alu", "rbd", "nom_alu", "app_alu", "apm_alu", "gen_alu", "edad_alu", "nom_com_alu", "cod_grado", "cod_ense", "fec_ret_rbd", "run_alu2") 
colnames(bd_desvinc_intran_r2) <- paste0(colnames(bd_desvinc_intran_r2), "_ret")
colnames(bd_desvinc_intran_r2)


#Unión bd_desvinc e intraanual
bd <- full_join(bd, bd_desvinc_intran_r, by = "run_alu")

table(bd$desert_glob_total_aju_agosto_2023, bd$desertor_intra, useNA = "ifany")

#Se define categoría para tablas de deserción
bd <- bd %>% mutate(categoria_desert = if_else(desert_glob_total_aju_agosto_2023 == 1 & desertor_intra == 1, "Retirado 2023", "otro"))
bd <- bd %>% mutate(categoria_desert = if_else(desert_glob_total_aju_agosto_2023 == 0, "No desertor", categoria_desert))
bd <- bd %>% mutate(categoria_desert = if_else(desert_glob_total_aju_agosto_2023 == 1 & is.na(desertor_intra), "Desercion incidencia", categoria_desert))
bd <- bd %>% mutate(categoria_desert = ifelse(desertor_intra == 1, "Retirado 2023", categoria_desert))


bd <- bd %>% mutate(valid_estud = ifelse(validacion_estudios == 1 | valida_estudios == 1, "Sí", "No"))
bd <- bd %>% mutate(valid_estud = ifelse(is.na(valid_estud), "No", valid_estud))


bd <- bd %>% mutate(cert_val_22 = ifelse(cert_val_22 == 1, "Sí", "No"))
bd <- bd %>% mutate(cert_val_22 = ifelse(is.na(cert_val_22), "No", cert_val_22))




pp <- bd %>% filter(cod_depe2_2022r == 3)

#**************************************************************************************************************************/
# 2. Gestión de BBDD  ----------------------------------------------------------------
#**************************************************************************************************************************/


# Se pasa variable cur_deser_g con labels entendibles. (11 = 3° medio HC, 12 = 3° medio TP, 13 = 4° medio HC, 14 = 4° medio TP)
# bd$cur_deser_g <- factor(bd$cur_deser_g, levels = c(1:14), labels = c("1° básico", "2° básico", "3° básico", "4° básico", "5° básico", "6° básico", "7° básico", "8° básico", 
#                                                                       "1° medio", "2° medio", "3° medio HC", "3° medio TP", "4° medio HC", "4° medio TP"))
# 
# table(bd$cur_deser_g, useNA = "always")
# # Se pasa variable curso2022 labels entendibles. (11 = 3° medio HC, 12 = 3° medio TP, 13 = 4° medio HC, 14 = 4° medio TP)
# bd$curso2022r <- factor(bd$curso2022r, levels = c(1:14), labels = c("1° básico", "2° básico", "3° básico", "4° básico", "5° básico", "6° básico", "7° básico", "8° básico", 
#                                                                       "1° medio", "2° medio", "3° medio HC", "3° medio TP", "4° medio HC", "4° medio TP"))

#Recodificamos variable región
bd$cod_reg_rbd2022r2 <- factor(bd$cod_reg_rbd_2022r, levels = c(-15:0), labels = c("Región de Arica y Parinacota","Región de Tarapacá", "Región de Antofagasta", "Región de Atacama", "Región de Coquimbo", "Región de Valparaíso", "Región Metropolitana", "Región de O'Higgins", "Región del Maule", 
                                                                     "Región del Ñuble", "Región del Biobío", "Región de La Araucanía", "Región de Los Ríos", "Región de Los Lagos", "Región de Aysén", "Región de Magallanes"))

bd <- bd %>% mutate(run_alu2 = ifelse(dgv_alu2022 != "", paste0(run_alu, "-", dgv_alu2022), run_alu))

bd <- bd %>% mutate(gen_alu_2022r = case_when(gen_alu_2022r == 1 ~ "M", gen_alu_2022r == 2 ~ "F", gen_alu_2022r == 0 ~ ""))
#table(bd$cod_reg_rbd2022r2, bd$cod_reg_rbd2022r)

bd <- bd %>% mutate(sit_fin_r_2022r = case_when(is.na(sit_fin_r_2022r) | sit_fin_r_2022r == "" ~ "Sin Registro", sit_fin_r_2022r == "P" ~ "Promovido", sit_fin_r_2022r == "R" ~ "Reprobado", sit_fin_r_2022r == "Y" ~ "Retirado"))



#Se agrega la columna nombre_grado del estudiante en el año 2022
bd <- left_join(bd, nom_grad, by = c("cod_ense_2022r" = "cod_ense", "cod_grado_2022r" = "cod_grado"))
bd_asis <- left_join(bd_asis, nom_grad, by = c("cod_ense" = "cod_ense", "cod_grado" = "cod_grado"))


#Ajustes en renombre de etiquetas de la base de dobles desvinculados
doble_desvinc <- left_join(doble_desvinc, nom_grad, by = c("cod_ense" = "cod_ense", "cod_grado" = "cod_grado"))
doble_desvinc <- doble_desvinc %>% mutate(gen_alu = case_when(gen_alu == 1 ~ "M", gen_alu == 2 ~ "F", gen_alu == 0 ~ ""))
doble_desvinc <- doble_desvinc %>% mutate(sit_fin_r = case_when(is.na(sit_fin_r) | sit_fin_r == "" | sit_fin_r == "Z"  ~ "Sin Registro", sit_fin_r == "P" ~ "Promovido", sit_fin_r == "R" ~ "Reprobado", sit_fin_r == "Y" ~ "Retirado"))
doble_desvinc <- doble_desvinc %>% mutate(run_alu2 = ifelse(dgv_alu != "", paste0(run_alu, "-", dgv_alu), run_alu))

doble_desvinc <- doble_desvinc %>% mutate(cert_val_21 = if_else(cert_val_21 == 1, "Sí", "No"))
doble_desvinc <- doble_desvinc %>% mutate(cert_val_22 = if_else(cert_val_22 == 1, "Sí", "No"))
doble_desvinc <- doble_desvinc %>% mutate(insc_val_23 = if_else(insc_val_23 == 1, "Sí", "No"))


#Se cambia el nombre de la columna
bd <- bd %>% rename(nom_grado_2022r = nom_grado)

bd <- bd %>% mutate(prom_gral2022 = ifelse(sit_fin_r_2022r == "Retirado" & prom_gral2022 == 0, NA, prom_gral2022))
bd <- bd %>% mutate(asistencia2022 = ifelse(sit_fin_r_2022r == "Retirado" & asistencia2022 == 0, NA, asistencia2022))



bd_asis <- bd_asis %>% mutate(run_alu2 = ifelse(dgv_alu != "", paste0(run_alu, "-", dgv_alu), run_alu))

# bd_asis$cod_curso <- factor(bd_asis$cod_curso, levels = c("1° básico", "2° básico", "3° básico", "4° básico", "5° básico", "6° básico", "7° básico", "8° básico", 
#                                                           "1° Medio", "2° Medio", "3° Medio HC", "3° Medio TP", "4° Medio HC", "4° Medio TP"), labels = c("1° básico", "2° básico", "3° básico", "4° básico", "5° básico", "6° básico", "7° básico", "8° básico", 
#                                                                     "1° medio", "2° medio", "3° medio HC", "3° medio TP", "4° medio HC", "4° medio TP"))
bd_asis$cod_curso <- bd_asis$nom_grado

colnames(bd_asis)

bd_asis <- bd_asis %>% mutate(tipo_asis_3 = case_when(porcentage_asistencia_acumulada >= 90 ~ 'Asistencia esperada (90% - 100% asistencia)', porcentage_asistencia_acumulada < 90 & porcentage_asistencia_acumulada >= 85 ~ 'Inasistencia reiterada (85% - 89% asistencia)', porcentage_asistencia_acumulada < 85 & porcentage_asistencia_acumulada >= 50 ~ 'Inasistencia grave (50% - 84% asistencia)', porcentage_asistencia_acumulada < 50  ~ 'Inasistencia crítica (0% - 49% asistencia)'))
bd_asis <- bd_asis %>% mutate(tipo_asis_2 = case_when(asistencia_categorias_acumulada == 4 ~ 'Asist. esperada (90%-100% asist.)', asistencia_categorias_acumulada == 3 ~ 'Inasist. reiterada (85%-89% asist.)', asistencia_categorias_acumulada == 2  ~ 'Inasist. grave (50%-84% asist.)', asistencia_categorias_acumulada == 1  ~ 'Inasist. crítica (0%-49% asist.)', is.na(asistencia_categorias_acumulada) ~ 'Sin información'))


bd_asis <- bd_asis %>% mutate(asistencia_2022 = ifelse(asistencia_2022 == "Sin información", NA, asistencia_2022))
bd_asis <- bd_asis %>% mutate(asistencia_2022 = as.numeric(asistencia_2022))

table(bd_asis$asistencia_categorias_acumulada, useNA = "a")

#Corrección de errores
bd_asis <- bd_asis %>% mutate(nom_rbd = ifelse(rbd == 1684, "ESCUELA LIBERTADOR BERNARDO OHIGGINS", nom_rbd))
bd_asis$nom_rbd <- gsub('_',' ',bd_asis$nom_rbd)
bd_asis$nom_rbd <- gsub('^',' ', bd_asis$nom_rbd)
bd_asis$nom_rbd <- gsub('&','Y',bd_asis$nom_rbd)
bd$nom_rbd2022r <- gsub('_',' ',bd$nom_rbd2022r)
bd$nom_rbd2022r <- gsub('^',' ', bd$nom_rbd2022r)
bd$nom_rbd2022r <- gsub('&','Y',bd$nom_rbd2022r)
bd_asis$nombre_sost_rbd2022 <- gsub('_',' ',bd_asis$nombre_sost_rbd2022)
bd_asis$nombre_sost_rbd2022 <- gsub('^',' ', bd_asis$nombre_sost_rbd2022)
bd_asis$nombre_sost_rbd2022 <- gsub('&','Y',bd_asis$nombre_sost_rbd2022)

#Se corrige nombre que tira error en Latex
bd <- bd %>% mutate(nom_alu2022 = ifelse(run_alu == "100565271","DJOULISSA",nom_alu2022))
bd_asis <- bd_asis %>% mutate(nom_alu = ifelse(run_alu == "100565271","DJOULISSA", nom_alu))

#**************************************************************************************************************************/
# 3. Loop de Render para cada RBD  ----------------------------------------------------------------
#**************************************************************************************************************************/

#length(unique(bd$rbd2023))
#**************************************************************************************************************************/
## 3.1 Ajuste fuente gráficos  ----------------------------------------------------------------
#**************************************************************************************************************************/
#Se definen los colores
azul <- "1A4672"
celeste <- "74C7D0"   
rojo <-  "D35765"
amarillo <- "F4A416" 
verde <- "61B798"
blanco <- "FAFAFA"
gris <- "#D8D8D8"

f1 <- list(
  family = "verdana",
  size = 14,
  color = azul)
f2 <- list(
  family = "verdana",
  size = 17,
  color = azul)

m <- list(
  l = 40,
  pad = 5
)

f3 <- list(
  family = "verdana",
  size = 12,
  color = '#007096')

f4 <- list(
  family = "verdana")

m2 <- list(
  l = 50,
  r = 50,
  b = 50,
  t = 50,
  pad = 3
)


  

#**************************************************************************************************************************/

## 3.2 Loop  ----------------------------------------------------------------
#**************************************************************************************************************************/

#Se sacan los pp
#bd <- bd %>% filter(cod_depe2_2022r != 3)
bd_asis <- bd_asis %>% filter(cod_depe == 4)

#Se arma bd para retirados
bd2 <- bd %>% filter(categoria_desert == "Retirado 2023")
bd2 <- left_join(bd2, bd_desvinc_intran_r2, by = c("run_alu" = "run_alu_ret"))
bd2 <- left_join(bd2, nom_grad, by = c("cod_ense_ret" = "cod_ense", "cod_grado_ret" = "cod_grado"))
head(bd2$fec_ret_rbd_ret)
bd2$fec_ret_rbd_ret <- format(as.Date(as.character(bd2$fec_ret_rbd_ret), "%Y%m%d"),"%d/%m/%Y")
head(bd2$fec_ret_rbd_ret)


#FILTRO para educación de adultos
bd_asis <- bd_asis %>% filter(grepl("Adult.", nom_grado, fixed=TRUE))
bd <- bd %>% filter(grepl("Adult.", nom_grado_2022r, fixed=TRUE))
bd2 <- bd2 %>% filter(grepl("Adult.", nom_grado, fixed=TRUE))
doble_desvinc <- doble_desvinc %>% filter(grepl("Adult.", nom_grado, fixed=TRUE))



## Datos para gráficos
data_plot2_n_todos <- bd %>% group_by(rbd_2022r, nom_grado_2022r) %>% summarise(n_total = n()) %>% ungroup()
data_plot3_n_todos <- bd_asis %>% group_by(rbd, cod_curso) %>% summarise(n_total = n()) %>% ungroup()
data_plot2_n_2_todos <- bd_asis %>% group_by(rbd, nom_grado) %>% summarise(n_total = n()) %>% ungroup()

rbds <- unique(bd_asis$rbd)
length(rbds)



###############

### 3.2 Loop informe común  ----------------------------------------------------------------
#**************************************************************************************************************************/

rbds1 <- rbds#[1:2533]  #[2534:5067] #[5068:7601] #[7602:10140]
# ee <- 6765#rbds[1]#

i = 0
nn = length(rbds1)
nn

#REVISION DE FALTANTES
df <- data.frame(rbd = rbds1)
archivos <- list.files("Outputs/Escuelas 0 (PP y EPJA)")
pdfs <- data.frame(nombre_pdf = archivos)
pdfs <- pdfs %>% filter(!grepl(".tex", archivos, fixed = TRUE))
pdfs <- pdfs %>% mutate(rbd = gsub("-EPJA.pdf", "", nombre_pdf)) %>% mutate(rbd = as.numeric(rbd))
head(pdfs)
nrow(pdfs)

df <- left_join(df, pdfs, by = "rbd")
pendientes <- df %>% filter(is.na(nombre_pdf))
pendientes <- unique(pendientes$rbd)
pendientes

i = 0
nn = length(pendientes)
nn


      
      {
        
          # ee = rbds0[10]
        for(ee in rbds1){ #rbds1[1]){     #(ee in quinto1$`0`){   #for(ee in unique(bd_asis$rbd)){ #9810 EEFRANI #3055 #3573 no tiene desvinc  #c(9647, 9648, 9653, 9654, 9655)
          try({   #En caso de que código tenga error el código sigue ejectuándose
                tic()
                #**************************************************************************************************************************/
                ## 3.1 PRE RENDER  ----------------------------------------------------------------
                #**************************************************************************************************************************/
                i = i+1
                
                pass = paste0("r", ee)
                ########## SE COMIENZA CON DESVINCULADOS
                desvinc <- bd[bd$rbd_2022r == ee,]
                
                nrow(desvinc)
              # })}
                
               
                    
                data_plot2 <- data.frame(nom_grado_2022r = unique(desvinc$nom_grado_2022r))
                
            
                n_mat2022 <- nrow(desvinc)
                
                    # Dejamos solo estudiantes desertores en mayo y por si acaso se filtra fallecidos, aunque ya vienen como deserción == 0
                desvinc <- desvinc %>% filter(categoria_desert == "Desercion incidencia")    #(desert_glob_total_aju_marzo_2023 == 1 & fallece_tot == 0)
                desvinc2 <- bd2[bd2$rbd_ret == ee,]
                desvinc2 <- desvinc2 %>% filter(categoria_desert == "Retirado 2023")
                
                desvinc3 <- doble_desvinc[doble_desvinc$rbd == ee,]
                
                n_desvinc <- nrow(desvinc)
                n_desvinc2 <- nrow(desvinc2)
                n_desvinc3 <- nrow(desvinc3)
                
                desvinc <- desvinc %>% select("run_alu2", "nom_alu2022", "app_alu2022", "apm_alu2022", "gen_alu_2022r", "edad_alu_2022r", "nom_com_alu2022", "nom_grado_2022r", "prom_gral2022", "asistencia2022", "sit_fin_r_2022r", 
                                                "rbd_2022r", "nom_rbd_2022r", "cod_depe2_2022r", "cod_reg_rbd2022r2", "nom_com_rbd_2022r", "nom_deprov_rbd2022", "nombre_sost_rbd2022", "rut_sost_rbd2022", "email_sost_rbd2022",  "tel_sost_rbd", "cert_val_22", "valid_estud")
                desvinc <- desvinc %>% arrange(nom_grado_2022r)
                
                desvinc2 <- desvinc2 %>% select("run_alu2_ret", "nom_alu_ret", "app_alu_ret", "apm_alu_ret", "gen_alu_ret", "edad_alu_ret", "nom_com_alu_ret", "nom_grado", "fec_ret_rbd_ret", "valid_estud")
                desvinc2 <- desvinc2 %>% arrange(nom_grado)   #(nom_grado_2022r)
                
                #desvinc3 <- desvinc3 %>% select("run_alu2", "nom_alu", "app_alu", "apm_alu", "gen_alu", "edad_alu", "nom_com_alu", "nom_grado", "sit_fin_r", "rbd", "nom_rbd", "cod_depe2", "cod_reg_rbd", "nom_com_rbd", "cert_val_22") # cert vald 21? muchos vacios
                                                #"nom_deprov_rbd2022", "nombre_sost_rbd2022", "rut_sost_rbd2022", "email_sost_rbd2022",  "tel_sost_rbd", "valid_estud")
                
                desvinc3 <- desvinc3 %>% select("run_alu2", "nom_alu", "app_alu", "apm_alu", "gen_alu", "edad_alu", "nom_com_alu", "nom_grado", "sit_fin_r", "rbd", "nom_rbd", "cod_depe2", "cod_reg_rbd", "nom_com_rbd", "cert_val_21", "cert_val_22", "insc_val_23") # cert vald 21? muchos vacios
                desvinc3 <- desvinc3 %>% arrange(nom_grado)
                
                ########## SE EXTRAEN LOS CASOS DE INASISTENCIA Y PARAMETROS 
                asis_crit <- bd_asis[bd_asis$rbd == ee,]
                data_plot3 <- data.frame(cod_curso = unique(asis_crit$cod_curso))
                data_plot2_2 <- data.frame(nom_grado = unique(asis_crit$nom_grado))
                nom_rbd <- asis_crit$nom_rbd[1]
                print("------")
                print(ee)
                print(nom_rbd)
                nom_com_rbd <- asis_crit$nom_com_rbd[1]
                print(nom_com_rbd)
                n_mat2023 = nrow(asis_crit)
                
                # #Datos gráfico de barra de rangos de asistencia
                # data_plot5 <- data.frame(tipo_asis_2 = c("Inasist. crítica (0%-49% asist.)", "Inasist. grave (50%-84% asist.)", "Inasist. reiterada (85%-89% asist.)", "Asist. esperada (90%-100% asist.)", "Sin información"), color = c(rojo, amarillo, celeste, azul, gris))  #azul celeste amarillo rosado rojo gris
                # data_plot5$tipo_asis_2 <- factor(data_plot5$tipo_asis_2, levels = c("Inasist. crítica (0%-49% asist.)", "Inasist. grave (50%-84% asist.)","Inasist. reiterada (85%-89% asist.)", "Asist. esperada (90%-100% asist.)", "Sin información"), labels =  c("Inasist. crítica (0%-49% asist.)", "Inasist. grave (50%-84% asist.)", "Inasist. reiterada (85%-89% asist.)", "Asist. esperada (90%-100% asist.)", "Sin información"))
                # data_plot5$tipo_asis_2
                # if(nrow(asis_crit) > 0){
                #   data_plot5_t <- prop.table(table(asis_crit$tipo_asis_2))
                #   data_plot5_t <- data.frame("tipo_asis_2"=names(data_plot5_t), "prop" = as.numeric(data_plot5_t))
                #   data_plot5_t_n <- table(asis_crit$tipo_asis_2)
                #   data_plot5_t_n <- data.frame("tipo_asis_2"=names(data_plot5_t_n), "n" = as.numeric(data_plot5_t_n))
                # } else{
                #   data_plot5_t <- data.frame(tipo_asis_2= c("Inasist. crítica (0%-49% asist.)", "Inasist. grave (50%-84% asist.)", "Inasist. reiterada (85%-89% asist.)", "Asist. esperada (90%-100% asist.)", "Sin información"), prop = c(0,0,0,0,0))
                #   data_plot5_t_n <- data.frame(tipo_asis_2= c("Inasist. crítica (0%-49% asist.)", "Inasist. grave (50%-84% asist.)", "Inasist. reiterada (85%-89% asist.)", "Asist. esperada (90%-100% asist.)", "Sin información"), n = c(0,0,0,0,0))
                # }
                # #data_plot5_t
                # data_plot5 <- left_join(data_plot5, data_plot5_t, by = "tipo_asis_2")
                # data_plot5 <- left_join(data_plot5, data_plot5_t_n, by = "tipo_asis_2")
                # data_plot5 <- data_plot5 %>% mutate(prop = ifelse(is.na(prop), 0 , prop))
                # data_plot5 <- data_plot5 %>% mutate(n = ifelse(is.na(n), 0 , n))
                # ####------
                
              
                
                 asis_crit <- asis_crit %>% filter(inasistencia_grave_acumulada == TRUE)
                 n_inasis2023 = nrow(asis_crit)
                # #asis_crit <- asis_crit %>% select("run_alu2", "nom_alu", "app_alu", "apm_alu", "cod_curso", "rbd", "porcentage_asistencia_marzo","porcentage_asistencia_abril","porcentage_asistencia_mayo","porcentage_asistencia_junio","porcentage_asistencia_julio","porcentage_asistencia_agosto","porcentage_asistencia_septiembre","porcentage_asistencia_octubre","porcentage_asistencia_noviembre","porcentage_asistencia_acumulada", "inasistencia_grave_acumulada", "asistencia_categorias_acumulada", "tipo_asis_2")
                # asis_crit <- asis_crit %>% select("run_alu2", "nom_alu", "app_alu", "apm_alu", "cod_curso", "rbd", "porcentage_asistencia_marzo", "porcentage_asistencia_abril", "porcentage_asistencia_mayo", "porcentage_asistencia_acumulada", "inasistencia_grave_acumulada", "asistencia_categorias_acumulada", "tipo_asis_2", "asistencia_2022", "sit_fin_22")
                # 
                # 
                # asis_crit <- asis_crit %>% arrange(cod_curso, porcentage_asistencia_acumulada)
                # 
                # 
                # 
                # 
                # asis_crit <- asis_crit %>% mutate(porcentage_asistencia_acumulada = ifelse(!is.na(porcentage_asistencia_acumulada), paste0(round(porcentage_asistencia_acumulada*100,0),"%"), "-"))
                # asis_crit <- asis_crit %>% mutate(porcentage_asistencia_marzo = ifelse(!is.na(porcentage_asistencia_marzo), paste0(round(porcentage_asistencia_marzo*100,0),"%"), "-"))
                # asis_crit <- asis_crit %>% mutate(porcentage_asistencia_abril = ifelse(!is.na(porcentage_asistencia_abril), paste0(round(porcentage_asistencia_abril*100,0),"%"), "-"))
                # asis_crit <- asis_crit %>% mutate(porcentage_asistencia_mayo = ifelse(!is.na(porcentage_asistencia_mayo), paste0(round(porcentage_asistencia_mayo*100,0),"%"), "-"))
                # # asis_crit <- asis_crit %>% mutate(porcentage_asistencia_junio = ifelse(!is.na(porcentage_asistencia_junio), paste0(round(porcentage_asistencia_junio*100,0),"%"), "-"))
                # # asis_crit <- asis_crit %>% mutate(porcentage_asistencia_julio = ifelse(!is.na(porcentage_asistencia_julio), paste0(round(porcentage_asistencia_julio*100,0),"%"), "-"))
                # # asis_crit <- asis_crit %>% mutate(porcentage_asistencia_agosto = ifelse(!is.na(porcentage_asistencia_agosto), paste0(round(porcentage_asistencia_agosto*100,0),"%"), "-"))
                # # asis_crit <- asis_crit %>% mutate(porcentage_asistencia_septiembre = ifelse(!is.na(porcentage_asistencia_septiembre), paste0(round(porcentage_asistencia_septiembre*100,0),"%"), "-"))
                # # asis_crit <- asis_crit %>% mutate(porcentage_asistencia_octubre = ifelse(!is.na(porcentage_asistencia_octubre), paste0(round(porcentage_asistencia_octubre*100,0),"%"), "-"))
                # # asis_crit <- asis_crit %>% mutate(porcentage_asistencia_noviembre = ifelse(!is.na(porcentage_asistencia_noviembre), paste0(round(porcentage_asistencia_noviembre*100,0),"%"), "-"))
                # asis_crit <- asis_crit %>% mutate(asistencia_2022 = ifelse(!is.na(asistencia_2022), paste0(round(asistencia_2022*100,0),"%"), "-"))
                # #print(desvinc)
                # #print(asis_crit)
                
            
                
                ########## GRAFICO SIT_FIN
                data_plot1 <- data.frame(sit_fin_r_2022r = c("Promovido", "Reprobado", "Retirado", "Sin Registro"), color = c(azul, celeste, rojo, amarillo))
                data_plot1$sit_fin_r_2022r <- factor(data_plot1$sit_fin_r_2022r, levels =  c("Promovido", "Reprobado", "Retirado", "Sin Registro"), labels =  c("Promovido", "Reprobado", "Retirado", "Sin Registro"))
                
                if(nrow(desvinc) > 0){
                  data_plot1_t <- prop.table(table(desvinc$sit_fin_r_2022r))
                  data_plot1_t <- data.frame("sit_fin_r_2022r"=names(data_plot1_t), "prop" = as.numeric(data_plot1_t))
                  data_plot1_t_n <- table(desvinc$sit_fin_r_2022r)
                  data_plot1_t_n <- data.frame("sit_fin_r_2022r"=names(data_plot1_t_n), "n" = as.numeric(data_plot1_t_n))
                } else{
                  data_plot1_t <- data.frame(sit_fin_r_2022r= c("Promovido", "Reprobado", "Retirado", "Sin Registro"), prop = c(0,0,0,0))
                  data_plot1_t_n <- data.frame(sit_fin_r_2022r= c("Promovido", "Reprobado", "Retirado", "Sin Registro"), n = c(0,0,0,0))
                }
              
                
                data_plot1 <- left_join(data_plot1, data_plot1_t, by = "sit_fin_r_2022r")
                data_plot1 <- left_join(data_plot1, data_plot1_t_n, by = "sit_fin_r_2022r")
                data_plot1 <- data_plot1 %>% mutate(prop = ifelse(is.na(prop), 0 , prop))
                data_plot1 <- data_plot1 %>% mutate(n = if_else(is.na(n), 0 , n))
              
                ########## GRAFICO TORTA SIN FIN
              
                
                p <- plot_ly(data_plot1, labels = ~sit_fin_r_2022r, values = ~round(prop,3), text = paste0(round(data_plot1$prop*100,1), "%"," <i>(", data_plot1$n,")</i>"), type = 'pie', hole = 0, textposition = 'outside', 
                             textinfo = 'label+text', marker = list(colors = ~color)) %>% 
                  layout(legend = list(font = list(family = "verdana", size = 18, color = azul), orientation = "h", x=0.1 , y=1.6), font= list(family = "verdana", size = 17, color = azul), xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
                         paper_bgcolor='transparent', autosize = T
                  )
                
          
                # url_sin_fin_g = paste0("/Img temp/",ee,"_sit_fin_g.png")
                # orca(p, url_sin_fin_g)
                
                url_sin_fin_g = paste0(getwd(),"/Img temp/",ee,"_sit_fin_g.png")
                save_image(p,url_sin_fin_g)
                
                ########## GRAFICO BARRA CURSO DESVINC
                
                data_plot2_n <- data_plot2_n_todos %>% filter(rbd_2022r == ee)
                
                
                data_plot2 <- data_plot2 %>% arrange(nom_grado_2022r)
                data_plot2_t <- desvinc %>% group_by(nom_grado_2022r) %>% summarise(n = n())
                data_plot2  <- left_join(data_plot2, data_plot2_t, by="nom_grado_2022r")
                data_plot2  <- left_join(data_plot2, data_plot2_n, by="nom_grado_2022r")
                data_plot2  <- data_plot2 %>% mutate(n = ifelse(is.na(n), 0, n)) %>% mutate(porc_desvinc = ifelse(n_total != 0, 100*round(n/n_total, 3), 0))
                
                #print(data_plot2)
                  
                k <- plot_ly(x=~data_plot2$nom_grado_2022r, y = ~data_plot2$n , type = "bar", marker = list(color = celeste), text = paste0("<b style='color:#007096'>", data_plot2$n), textfont = list(family = "verdana", size = 35, color = '#FFFFFF'), textposition = 'outside') %>%
                  layout(font = list(family = "verdana", color = azul, size = 30), title = "",  #azul antiguo '#007096'
                         xaxis=list(title="<b>Grado 2022</b>", font = list(family = "verdana", size = 40), showlegend = FALSE,  categoryarray = fct_rev(data_plot2$nom_grado_2022r), categoryorder = "array", tickangle = -45),
                         yaxis=list(title="<b>N° estudiantes 2022 no matriculados en 2023</b>", font = list(family = "verdana", size = 24), range = list(0, max(data_plot2$n) + 5), showlegend = FALSE, showgrid = F, autotick = FALSE, dtick = 5),
                         plot_bgcolor='transparent', paper_bgcolor='transparent', autosize = F, width = 2000, height = 1200, margin = list(l=-0.1, t = -0.1, r=-0.1))
                
                # url_cur_desv_g = paste0("/Img temp/", ee,"_cur_desv_g.png")
                # orca(k, url_cur_desv_g)
              
                url_cur_desv_g = paste0(getwd(),"/Img temp/",ee,"_cur_desv_g.png")
                save_image(k,url_cur_desv_g)
                
            
                
                ########## GRAFICO BARRA CURSO RETIRADOS
                
                data_plot2_n_2 <- data_plot2_n_2_todos %>% filter(rbd == ee)
                
                
                data_plot2_2 <- data_plot2_2 %>% arrange(nom_grado)
                data_plot2_t_2 <- desvinc2 %>% group_by(nom_grado) %>% summarise(n = n())
                data_plot2_2  <- left_join(data_plot2_2, data_plot2_t_2, by="nom_grado")
                data_plot2_2  <- left_join(data_plot2_2, data_plot2_n_2, by="nom_grado")
                data_plot2_2  <- data_plot2_2 %>% mutate(n = ifelse(is.na(n), 0, n)) %>% mutate(porc_desvinc = ifelse(n_total != 0, 100*round(n/n_total, 3), 0))
                
                #print(data_plot2)
                
                k2 <- plot_ly(x=~data_plot2_2$nom_grado, y = ~data_plot2_2$n , type = "bar", marker = list(color = celeste), text = paste0("<b style='color:#007096'>", data_plot2_2$n), textfont = list(family = "verdana", size = 35, color = '#FFFFFF'), textposition = 'outside') %>%
                  layout(font = list(family = "verdana", color = azul, size = 30), title = "", #azul antiguo '#007096'
                         xaxis=list(title="<b>Grado 2023</b>", font = list(family = "verdana", size = 40), showlegend = FALSE,  categoryarray = fct_rev(data_plot2_2$nom_grado), categoryorder = "array", tickangle = -45),
                         yaxis=list(title="<b>N° estudiantes 2023 retirados sin matríc. vigente</b>", font = list(family = "verdana", size = 24), range = list(0, max(data_plot2_2$n) + 3), showlegend = FALSE, showgrid = F, autotick = FALSE, dtick = 5),
                         plot_bgcolor='transparent', paper_bgcolor='transparent', autosize = F, width = 2000, height = 1200, margin = list(l=-0.1, t = -0.1, r=-0.1))
                
                
                # url_cur_desv_g_2 = paste0("/Img temp/", ee,"_cur_desv_g_2.png")
                # orca(k2, url_cur_desv_g_2)
                
                url_cur_desv_g_2 = paste0(getwd(),"/Img temp/",ee,"_cur_desv_g_2.png")
                save_image(k2,url_cur_desv_g_2)
                
                ########## GRAFICO BARRA INASISTENCIA GRAVE
                
                
                # 
                # data_plot3_t <- asis_crit %>% group_by(cod_curso) %>% summarise(n = n())
                # data_plot3_n <- data_plot3_n_todos %>% filter(rbd == ee)
                # 
                # data_plot3  <- data_plot3 %>% arrange(desc(cod_curso))
                # data_plot3  <- left_join(data_plot3, data_plot3_t, by="cod_curso")
                # data_plot3  <- left_join(data_plot3, data_plot3_n, by="cod_curso")
                # data_plot3  <- data_plot3 %>% mutate(n = ifelse(is.na(n), 0, n)) %>% mutate(porc_asis = ifelse(n_total != 0, 100*round(n/n_total, 3), 0))
                # 
                # 
                # j <- plot_ly() %>% add_trace(x = ~data_plot3$porc_asis, y = ~data_plot3$cod_curso, marker = list(color = celeste), type = "bar", orientation = "v", text = paste0("<b style='color:#007096'>", data_plot3$porc_asis, "%","</b>", "<i style='color:#007096';> (", data_plot3$n," de ", data_plot3$n_total,")</i>"), textposition = "outside", textfont = list(family = "verdana", size = 30, color = '#FFFFFF'))
                #   j <- j %>% layout(font = list(family = "verdana", color = azul, size = 30), title = "",  #azul antiguo '#007096'
                #                     xaxis = list(title="<b>% Estudiantes 2023 con asistencia bajo 85%</b> <i> (N° Estudiantes respecto a matrícula 2023)</i>", font = list(family = "verdana", size = 30), range = list(0, max(data_plot3$porc_asis) + 19), showlegend = FALSE, ticksuffix = "%", showgrid = FALSE, side = "top"),
                #                     yaxis = list(title="<b>Grado 2023</b>", font = list(family = "verdana", size = 30), showlegend = FALSE,  categoryarray = fct_rev(data_plot3$cod_curso), categoryorder = "array"),  
                #                     plot_bgcolor='transparent', paper_bgcolor='transparent', autosize = F, width = 2000, height = 1200, margin = list(l=-0.1, t = -0.1, r=-0.1)
                #   )
                # 
             
                # 
                # url_asis_crit_g = paste0("/Img temp/", ee,"_asis_crit_g.png")
                # orca(j, url_asis_crit_g)
                
                
                # 
                # ########## GRAFICO TORTA INASISTENCIA GRAVE
                # 
                # o <- plot_ly(data_plot5, labels = ~tipo_asis_2, values = ~round(prop,3), type = 'pie', hole = 0.5, textposition = 'outside', text = paste0(round(data_plot5$prop,3)*100,"%<br><i>(", data_plot5$n,")</i>"),
                #              textinfo = 'text', sort = FALSE, marker = list(colors = ~color)) %>% 
                #   layout(legend = list(font = list(family = "verdana", size = 14, color = azul), x=1.3 , y=0.5), font=list(family = "verdana", size = 16, color = azul), xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                #          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
                #          paper_bgcolor='transparent', autosize = TRUE, margin = list(l = -5, r = -5)
                #   )
                # 
                # url_asis_crit_tort_g = paste0("/Img temp/", ee,"_asis_crit_tort_g.png")
                # orca(gituo, url_asis_crit_tort_g)
                
                
                #**************************************************************************************************************************/
                ## 3.2 RENDER  ----------------------------------------------------------------
                #**************************************************************************************************************************/
                #Sys.setlocale("LC_ALL","Spanish")
                Sys.setlocale("LC_ALL", "ES_ES.UTF-8")
                rmarkdown::render("RMKD Informe Escuela (PP y EPJA).Rmd", 
                                  params = list("rbd" = ee, "nom_rbd" = nom_rbd, "nom_com_rbd" = nom_com_rbd, "desvinc" = desvinc, "desvinc2" = desvinc2, "desvinc3" = desvinc3, "asis_crit" = asis_crit, "url_sin_fin_g" = url_sin_fin_g, "url_cur_desv_g" = url_cur_desv_g, "url_cur_desv_g_2" = url_cur_desv_g_2,
                                                "n_mat2022" = n_mat2022, "n_desvinc" = n_desvinc, "n_desvinc2" = n_desvinc2, "n_desvinc3" = n_desvinc3, "n_mat2023" = n_mat2023, "n_inasis2023" = n_inasis2023, "textos_variables_desvinculados" = textos_variables_desvinculados, "textos_variables_retirados" = textos_variables_retirados, "textos_variables_asistencia" = textos_variables_asistencia, "textos_variables_doble_desvinculados" = textos_variables_doble_desvinculados, "pass" = pass),
                                  output_file = paste0("Outputs/Escuelas 0 (PP y EPJA)/", ee, "-EPJA.pdf"))
                print(paste0("Listo ", i ," de ", nn, " establecimientos. (RBD: ", ee, ")"))
                toc()
           }) #cierre Try
          }
  
                
      }

