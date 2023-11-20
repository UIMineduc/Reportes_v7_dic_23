#**************************************************************************************************************************/
# 0. Ajustes iniciales  ----------------------------------------------------------------
#**************************************************************************************************************************/


Sys.setlocale("LC_ALL","en_US.UTF-8")
library(pacman)

p_load(tidyverse, rio, dplyr, ggplot2, stringr, tidyr, kableExtra, 
       texreg, Publish, broom, ggpubr, lubridate, labelled, Hmisc, pivottabler, knitr, data.table, plotly, 
       tictoc, htmlwidgets, webshot, tinytex, openxlsx)

options(scipen=999) # Desactiva la notaciÃ³n cientÃfica
options(max.print = 99999999) # Max print de la consola
options(width = 1000)
opts_chunk$set(  fig.align='center',
                 external=TRUE,
                 echo=TRUE,
                 warning=FALSE,
                 fig.pos='H')


options(encoding = "UTF-8")
install.packages('reticulate')
reticulate::install_miniconda()
reticulate::conda_install('r-reticulate', 'python-kaleido')
reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
reticulate::use_miniconda('r-reticulate')


#**************************************************************************************************************************/
# 1.0 (omitir) Lectura de BBDD  ----------------------------------------------------------------
#**************************************************************************************************************************/
#SE COMENTA TODO  LO QUE TOMA TIEMPO DE PROCESAR RESPECTO A AL BASE DE DATOS, YA QUE ESTÁ LISTO. EN CASO DE VOLVER A PROCESAR SE PUEDE DESCOMENTAR
#
#Se lee datos de estudiantes desvinculados
bd <- fread("Inputs/Desvinculacion_agosto_2023_3.csv", encoding="UTF-8")
colnames(bd) <- tolower(colnames(bd))
colnames(bd)
head(bd)

bd_asis <- fread("Inputs/202309_inasistencia_grave_agosto_2.csv", encoding="UTF-8")                  #fread("Inputs/20230624_inasistencia_grave_marzo_abril_mayo_2023.csv", encoding="UTF-8")
colnames(bd_asis) <- tolower(colnames(bd_asis))
colnames(bd_asis)

#Doble desivnculados (de 2021 a 2023)
doble_desvinc <- fread("Inputs/Desvinculados_2021_2023.csv")

#descarga de textos para tablas de variables.
textos_variables_desvinculados <- openxlsx::read.xlsx("Inputs/Tabla variables.xlsx", sheet = "Desvinculados")
textos_variables_retirados <- openxlsx::read.xlsx("Inputs/Tabla variables.xlsx", sheet = "Retirados")
textos_variables_asistencia <- openxlsx::read.xlsx("Inputs/Tabla variables.xlsx", sheet = "Asistencia")
textos_variables_doble_desvinculados <- openxlsx::read.xlsx("Inputs/Tabla variables.xlsx", sheet = "Doble_desvinculados")

#Desvinculados intranual
bd_desvinc_intran <- fread("Inputs/retirados_sin_mat_agosto_parv_val_v2.csv", encoding="UTF-8")
colnames(bd_desvinc_intran) <- tolower(colnames(bd_desvinc_intran))
bd_desvinc_intran <- bd_desvinc_intran %>% mutate(rbd_ret = rbd)
bd_desvinc_intran <- bd_desvinc_intran %>% mutate(run_alu2 = ifelse(dgv_alu != "", paste0(run_alu, "-", dgv_alu), run_alu))
bd_desvinc_intran <- bd_desvinc_intran %>% mutate(gen_alu = case_when(gen_alu == 1 ~ "M", gen_alu == 2 ~ "F", gen_alu == 0 ~ "", TRUE ~ ""))
head(bd_desvinc_intran$run_alu2)
nrow(bd_desvinc_intran)

#Se corrige nombre que tira error en latex
bd_desvinc_intran <- bd_desvinc_intran %>% mutate(nom_alu = ifelse(run_alu == "100565271", "DJOULISSA", nom_alu))

#Se lee nombre de BD que tiene los nombres de los grados.
nom_grad <- fread("Inputs/Nombre grados.csv", encoding="UTF-8")
colnames(nom_grad) <- tolower(colnames(nom_grad))
# length(unique(nom_grad$nom_grado))
unicos <- unique(nom_grad$nom_grado)
unicos

orden_grados <- c("Sala Cuna Menor", "Sala Cuna Mayor","Sala Cuna Heterogéneo",  "Nivel Medio Menor", "Nivel Medio Mayor", "Nivel Medio Heterogéneo", "Pre-kinder", "Kinder", "Nivel Transición Heterogéneo",
                  "Heterogéneo", "Atención Temprana", "Laboral 1", "Laboral 2", "Laboral 3", "Laboral 4", "1° básico", "2° básico", "3° básico", "4° básico", "5° básico", "6° básico", "7° básico", "8° básico", "1° medio", "2° medio", "3° medio HC", "3° medio TP","3° medio Artístico", 
                  "4° medio TP", "4° medio HC", "4° medio Artístico", "1° a 4° básico Adult.", "5° y 6° básico Adult.", "7° y 8° básico Adult.", "1° y 2° medio Adult.",  "3° medio TP Adult.", "4° medio TP Adult.", 
                  "3° y 4° medio HC Adult.")

nom_grad$nom_grado <- factor(nom_grad$nom_grado, levels = orden_grados, labels = orden_grados)



#Se agrega nombre grado a los retirados
bd_desvinc_intran <- left_join(bd_desvinc_intran, nom_grad, by = c("cod_ense" = "cod_ense", "cod_grado" = "cod_grado"))

#Se selecciona rut y columnas de deserción para hacer la comparación
bd_desvinc_intran_r <- bd_desvinc_intran %>% select("run_alu", "validacion_estudios",  "desertor_intra") #,  "desertor_intra_validacion")    select(2, 67, 68, 70, 71)   
colnames(bd_desvinc_intran_r)

#Se crea otro data frame con la columnas de datos de los estudiantes retirados.
bd_desvinc_intran_r2 <- bd_desvinc_intran %>% select("run_alu", "nom_alu", "app_alu", "apm_alu", "gen_alu", "edad_alu", "nom_com_alu", "cod_grado", "cod_ense", "nom_grado", "fec_ret_rbd", "run_alu2",
                                                     "rbd", "nom_rbd", "cod_depe2", "cod_reg_rbd", "cod_com_rbd", "nom_com_rbd", "cod_deprov_rbd", "nom_grado") 


#Se le pega rut de sostenedor a la bd de retirados, ya que no lo tiene
bd_rbd_rutsost <- bd_asis %>% group_by(rut_sost_rbd2022, rbd) %>% summarise(n = n()) %>% ungroup() %>% select(rut_sost_rbd2022, rbd)

colnames(bd_desvinc_intran_r2)
bd_desvinc_intran_r2  <- left_join(bd_desvinc_intran_r2, bd_rbd_rutsost, by = "rbd")
colnames(bd_desvinc_intran_r2)

colnames(bd_desvinc_intran_r2) <- paste0(colnames(bd_desvinc_intran_r2), "_ret")
colnames(bd_desvinc_intran_r2)



#Unión bd_desvinc e intraanual
bd <- full_join(bd, bd_desvinc_intran_r, by = "run_alu")

table(bd$desert_glob_total_aju_agosto_2023, bd$desertor_intra, useNA = "ifany")

colnames(bd)

#Se define categoría para tablas de deserción
bd <- bd %>% mutate(categoria_desert = if_else(desert_glob_total_aju_agosto_2023 == 1 & desertor_intra == 1, "Retirado 2023", "otro"))
bd <- bd %>% mutate(categoria_desert = if_else(desert_glob_total_aju_agosto_2023 == 0, "No desertor", categoria_desert))
bd <- bd %>% mutate(categoria_desert = ifelse(desertor_intra == 1 , "Retirado 2023", categoria_desert))
bd <- bd %>% mutate(categoria_desert = if_else(desert_glob_total_aju_agosto_2023 == 1 & is.na(desertor_intra), "Desercion incidencia", categoria_desert))



bd <- bd %>% mutate(valid_estud = ifelse(validacion_estudios == 1 | valida_estudios == 1, "Sí", "No"))
bd <- bd %>% mutate(valid_estud = ifelse(is.na(valid_estud), "No", valid_estud))

bd <- bd %>% mutate(cert_val_22 = ifelse(cert_val_22 == 1, "Sí", "No"))
bd <- bd %>% mutate(cert_val_22 = ifelse(is.na(cert_val_22), "No", cert_val_22))

# bd <- bd %>% mutate(valid_estud = ifelse(validacion_menores == 1 | validacion_mayores == 1 | valida_estudios == 1 | valida_estudios_adulto == 1, "Sí", "No"))
# bd <- bd %>% mutate(valid_estud = ifelse(is.na(valid_estud), "No", valid_estud))


#**************************************************************************************************************************/
# 2. Gestión de BBDD  ----------------------------------------------------------------
#**************************************************************************************************************************/


#Recodificamos variable región
bd$cod_reg_rbd2022r2 <- factor(bd$cod_reg_rbd_2022r, levels = c(-15:0), labels = c("Región de Arica y Parinacota","Región de Tarapacá", "Región de Antofagasta", "Región de Atacama", "Región de Coquimbo", "Región de Valparaíso", "Región Metropolitana", "Región de O'Higgins", "Región del Maule", 
                                                                                   "Región del Ñuble", "Región del Biobío", "Región de La Araucanía", "Región de Los Ríos", "Región de Los Lagos", "Región de Aysén", "Región de Magallanes"))

bd <- bd %>% mutate(run_alu2 = ifelse(dgv_alu2022 != "", paste0(run_alu, "-", dgv_alu2022), run_alu))

bd <- bd %>% mutate(gen_alu_2022r = case_when(gen_alu_2022r == 1 ~ "M", gen_alu_2022r == 2 ~ "F", gen_alu_2022r == 0 ~ "",))
#table(bd$cod_reg_rbd2022r2, bd$cod_reg_rbd2022r)

bd <- bd %>% mutate(sit_fin_r_2022r = case_when(is.na(sit_fin_r_2022r) | sit_fin_r_2022r == "" ~ "Sin Registro", sit_fin_r_2022r == "P" ~ "Promovido", sit_fin_r_2022r == "R" ~ "Reprobado", sit_fin_r_2022r == "Y" ~ "Retirado"))


colnames(bd)

#Se agrega la columna nombre_grado del estudiante en el año 2022
bd <- left_join(bd, nom_grad, by = c("cod_ense_2022r" = "cod_ense", "cod_grado_2022r" = "cod_grado"))
bd_asis <- left_join(bd_asis, nom_grad, by = c("cod_ense" = "cod_ense", "cod_grado" = "cod_grado"))

#Ajustes en renombre de etiquetas de la base de dobles desvinculados
doble_desvinc <- left_join(doble_desvinc, nom_grad, by = c("cod_ense" = "cod_ense", "cod_grado" = "cod_grado"))
doble_desvinc <- doble_desvinc %>% mutate(gen_alu = case_when(gen_alu == 1 ~ "M", gen_alu == 2 ~ "F", gen_alu == 0 ~ "",))
doble_desvinc <- doble_desvinc %>% mutate(sit_fin_r = case_when(is.na(sit_fin_r) | sit_fin_r == "" | sit_fin_r == "Z" ~ "Sin Registro", sit_fin_r == "P" ~ "Promovido", sit_fin_r == "R" ~ "Reprobado", sit_fin_r == "Y" ~ "Retirado"))
doble_desvinc <- doble_desvinc %>% mutate(run_alu2 = ifelse(dgv_alu != "", paste0(run_alu, "-", dgv_alu), run_alu))

doble_desvinc <- doble_desvinc %>% mutate(cert_val_21 = if_else(cert_val_21 == 1, "Sí", "No"))
doble_desvinc <- doble_desvinc %>% mutate(cert_val_22 = if_else(cert_val_22 == 1, "Sí", "No"))
doble_desvinc <- doble_desvinc %>% mutate(insc_val_23 = if_else(insc_val_23 == 1, "Sí", "No"))


#Se cambia el nombre de la columna
bd <- bd %>% mutate(nom_grado_2022r = nom_grado)

bd <- bd %>% mutate(prom_gral2022 = ifelse(sit_fin_r_2022r == "Retirado" & prom_gral2022 == 0, NA, prom_gral2022))
bd <- bd %>% mutate(asistencia2022 = ifelse(sit_fin_r_2022r == "Retirado" & asistencia2022 == 0, NA, asistencia2022))



bd_asis <- bd_asis %>% mutate(run_alu2 = ifelse(dgv_alu != "", paste0(run_alu, "-", dgv_alu), run_alu))

# bd_asis$cod_curso <- factor(bd_asis$cod_curso, levels = c("1° básico", "2° básico", "3° básico", "4° básico", "5° básico", "6° básico", "7° básico", "8° básico", 
#                                                           "1° Medio", "2° Medio", "3° Medio HC", "3° Medio TP", "4° Medio HC", "4° Medio TP"), labels = c("1° básico", "2° básico", "3° básico", "4° básico", "5° básico", "6° básico", "7° básico", "8° básico", 
#                                                                     "1° medio", "2° medio", "3° medio HC", "3° medio TP", "4° medio HC", "4° medio TP"))
bd_asis$cod_curso <- bd_asis$nom_grado

colnames(bd)

# bd_asis <- bd_asis %>% mutate(tipo_asis_3 = case_when(porcentage_asistencia_acumulada >= 97 ~ 'Asistencia destacada', porcentage_asistencia_acumulada < 97 & porcentage_asistencia_acumulada >= 90 ~ 'Asistencia normal', porcentage_asistencia_acumulada < 90 & porcentage_asistencia_acumulada >= 85 ~ 'Inasistencia reiterada', porcentage_asistencia_acumulada < 85  ~ 'Inasistencia grave'))
bd_asis <- bd_asis %>% mutate(tipo_asis_2 = case_when(asistencia_categorias_acumulada == 4 ~ 'Asist. esperada (90%-100% asist.)', asistencia_categorias_acumulada == 3 ~ 'Inasist. reiterada (85%-89% asist.)', asistencia_categorias_acumulada == 2  ~ 'Inasist. grave (50%-84% asist.)', asistencia_categorias_acumulada == 1  ~ 'Inasist. crítica (0%-49% asist.)', is.na(asistencia_categorias_acumulada) ~ 'Sin información'))


#bd_asis <- bd_asis %>% filter(cod_depe2 != 3)


#Se pegan los ruts de sost 2023 a la bd 2022
bd <- bd %>% select(-c(rut_sost_rbd2022))

bd <- left_join(bd, bd_rbd_rutsost, by = c("rbd_2022r" = "rbd"))
doble_desvinc <- left_join(doble_desvinc, bd_rbd_rutsost, by = c("rbd" = "rbd"))



#FILTRO para educación de adultos
bd_asis <- bd_asis %>% filter(grepl("Adult.", nom_grado, fixed=TRUE))
bd <- bd %>% filter(grepl("Adult.", nom_grado_2022r, fixed=TRUE))
doble_desvinc <- doble_desvinc %>% filter(grepl("Adult.", nom_grado, fixed=TRUE))



#Corrección de errores
bd_asis <- bd_asis %>% mutate(nom_rbd = ifelse(rbd == 1684, "ESCUELA LIBERTADOR BERNARDO OHIGGINS", nom_rbd))
bd_asis$nom_rbd <- gsub('_','',bd_asis$nom_rbd)
bd_asis$nom_rbd <- gsub('^','', bd_asis$nom_rbd)
bd_asis$nom_rbd <- gsub('&','Y',bd_asis$nom_rbd)
bd$nom_rbd2022r <- gsub('_','',bd$nom_rbd2022r)
bd$nom_rbd2022r <- gsub('^','', bd$nom_rbd2022r)
bd$nom_rbd2022r <- gsub('&','Y',bd$nom_rbd2022r)
bd_asis$nombre_sost_rbd2022 <- gsub('_','',bd_asis$nombre_sost_rbd2022)
bd_asis$nombre_sost_rbd2022 <- gsub('^','', bd_asis$nombre_sost_rbd2022)
bd_asis$nombre_sost_rbd2022 <- gsub('&','Y',bd_asis$nombre_sost_rbd2022)
#Se corrige nombre que tira error en Latex
bd <- bd %>% mutate(nom_alu2022 = ifelse(run_alu == "100565271","DJOULISSA",nom_alu2022))
bd_asis <- bd_asis %>% mutate(nom_alu = ifelse(run_alu == "100565271","DJOULISSA", nom_alu))

#**************************************************************************************************************************/
# 2.1 Lectura de las BD pre procesadas y continuacion de gestión de BBDDs ----------------------------------------------------------------
#**************************************************************************************************************************/


# #Se crea BD de reg por rbd
deprov_sost <- bd %>% group_by(cod_reg_rbd2022r2, rbd_2022r) %>% summarise(n = n())
deprov_sost <- deprov_sost %>% group_by(rbd_2022r) %>% filter(n == max(n))
deprov_sost <- deprov_sost %>% ungroup()

#Se pegan los datos de Deprov a bd_asis
nrow(bd_asis)
bd_asis <- left_join(bd_asis, deprov_sost, by = c("rbd" = "rbd_2022r"))
nrow(bd_asis)

sin_datos <- bd_asis %>% filter(is.na(cod_reg_rbd2022r2))
unique(sin_datos$rbd)
## *****FALTAN 80 RBDs CON DATOS DEPROV. PUEDEN SER EE NUEVOS.**********************************************************************


# #Se filtran ee que sean pp
# bd <- bd %>% filter(cod_depe2_2022r != 3)
bd_asis <- bd_asis %>% filter(cod_depe2 == 3)


data_plot4_n <- bd %>% group_by(rut_sost_rbd2022, nom_rbd_2022r, rbd_2022r) %>% summarise(n_total = n()) %>% ungroup()  #**
data_plot4_n_2 <- bd_asis %>% group_by(rut_sost_rbd2022, nom_rbd, rbd) %>% summarise(n_total = n())  %>% ungroup()   #**
data_plot6_n <- bd_asis %>% group_by(rut_sost_rbd2022, nom_rbd, rbd) %>% summarise(n_total = n()) %>% ungroup()   #**
#data_plot3_n <- bd_asis %>% group_by(rbd, cod_curso) %>% summarise(n_total = n()) %>% ungroup()

#Se arma bd para retirados
bd2 <- bd %>% filter(categoria_desert == "Retirado 2023")



bd2 <- left_join(bd2, bd_desvinc_intran_r2, by = c("run_alu" = "run_alu_ret"))
#bd2 <- left_join(bd2, nom_grad, by = c("cod_ense_ret" = "cod_ense", "cod_grado_ret" = "cod_grado"))
head(bd2$fec_ret_rbd_ret)
bd2$fec_ret_rbd_ret <- format(as.Date(as.character(bd2$fec_ret_rbd_ret), "%Y%m%d"),"%d/%m/%Y")
head(bd2$fec_ret_rbd_ret)
#Ajuste nombre que tira error
bd2 <- bd2 %>% mutate(nom_alu_ret = ifelse(run_alu == "100565271","DJOULISSA",nom_alu_ret))

bd2 <- bd2 %>% filter(grepl("Adult.", nom_grado, fixed=TRUE))
bd2 <- bd2 %>% filter(grepl("Adult.", nom_grado_ret, fixed=TRUE))


#Datos de asistencia por EE para tabla 2.1 y 2.2
#resumen_asis_ee <- bd_asis %>% filter(cod_depe != 4) %>% group_by(rut_sost_rbd2022, nom_rbd, rbd) %>% summarise(asis_marzo= mean(porcentage_asistencia_marzo, na.rm = TRUE), asis_abril = mean(porcentage_asistencia_abril, na.rm = TRUE), asis_mayo = mean(porcentage_asistencia_mayo, na.rm = TRUE), asis_promedio = mean(porcentage_asistencia_acumulada, na.rm = TRUE), n_total = n(), n = sum(inasistencia_grave_acumulada))
resumen_asis_ee <- bd_asis %>% filter(cod_depe2 == 3) %>% filter(!is.na(inasistencia_grave_acumulada)) %>% group_by(rut_sost_rbd2022, nom_rbd, rbd) %>% summarise(asis_marzo= mean(porcentage_asistencia_marzo, na.rm = TRUE), 
                                                                                                                                                                  # asis_abril = mean(porcentage_asistencia_abril, na.rm = TRUE), asis_mayo = mean(porcentage_asistencia_mayo, na.rm = TRUE), 
                                                                                                                                                                  # asis_junio = mean(porcentage_asistencia_junio, na.rm = TRUE), asis_julio = mean(porcentage_asistencia_julio, na.rm = TRUE), asis_agosto = mean(porcentage_asistencia_agosto, na.rm = TRUE),
                                                                                                                                                                  # asis_septiembre = mean(porcentage_asistencia_septiembre, na.rm = TRUE), asis_octubre = mean(porcentage_asistencia_octubre, na.rm = TRUE), asis_noviembre = mean(porcentage_asistencia_noviembre, na.rm = TRUE),
                                                                                                                                                                  asis_promedio = mean(porcentage_asistencia_acumulada, na.rm = TRUE), n_total = n(), n = sum(inasistencia_grave_acumulada, na.rm = T))



table(bd_asis$inasistencia_grave_acumulada, useNA = "a")
table(bd_asis$cod_deprov_rbd, useNA = "a")


resumen_asis_ee <- resumen_asis_ee %>% mutate(porc_asis_crit = ifelse(n_total > 0, round(n/n_total,3), 0))
resumen_asis_ee <- resumen_asis_ee %>% ungroup()
resumen_asis_ee <- resumen_asis_ee %>% arrange(desc(porc_asis_crit))
resumen_asis_ee <- resumen_asis_ee %>% mutate(asis_marzo = paste0(round(asis_marzo*100, 1),"%"))
# resumen_asis_ee <- resumen_asis_ee %>% mutate(asis_abril = paste0(round(asis_abril*100, 1),"%"))
# resumen_asis_ee <- resumen_asis_ee %>% mutate(asis_mayo = paste0(round(asis_mayo*100, 1),"%"))
# resumen_asis_ee <- resumen_asis_ee %>% mutate(asis_junio = paste0(round(asis_junio*100, 1),"%"))
# resumen_asis_ee <- resumen_asis_ee %>% mutate(asis_julio = paste0(round(asis_julio*100, 1),"%"))
# resumen_asis_ee <- resumen_asis_ee %>% mutate(asis_agosto = paste0(round(asis_agosto*100, 1),"%"))
# resumen_asis_ee <- resumen_asis_ee %>% mutate(asis_septiembre = paste0(round(asis_septiembre*100, 1),"%"))
# resumen_asis_ee <- resumen_asis_ee %>% mutate(asis_octubre = paste0(round(asis_octubre*100, 1),"%"))
# resumen_asis_ee <- resumen_asis_ee %>% mutate(asis_noviembre = paste0(round(asis_noviembre*100, 1),"%"))
resumen_asis_ee <- resumen_asis_ee %>% mutate(asis_promedio = paste0(round(asis_promedio*100, 1),"%"))
resumen_asis_ee <- resumen_asis_ee %>% mutate(porc_asis_crit = paste0(round(porc_asis_crit*100, 1),"%"))
print(resumen_asis_ee)


#resumen_asis_ee2 <- bd_asis %>% filter(cod_depe != 4) %>% group_by(rut_sost_rbd2022, nom_rbd, rbd, asistencia_categorias1) %>% summarise(n_cat = n())
resumen_asis_ee2 <- bd_asis %>% filter(cod_depe2 == 3) %>% group_by(rut_sost_rbd2022, nom_rbd, rbd, asistencia_categorias_acumulada) %>% summarise(n_cat = n())
resumen_asis_ee2 <- resumen_asis_ee2 %>% ungroup()
resumen_asis_ee2
resumen_asis_ee2 <- resumen_asis_ee2 %>% pivot_wider(names_from = asistencia_categorias_acumulada, values_from = n_cat)
#resumen_asis_ee2 <- resumen_asis_ee2 %>% mutate(total)
resumen_asis_ee2 %>% filter(`NA` > 0)
nrow(resumen_asis_ee2)
nrow(resumen_asis_ee)
#resumen_asis_ee = left_join(resumen_asis_ee, resumen_asis_ee2, by = c("rut_sost_rbd2022", "nom_rbd", "rbd"))
resumen_asis_ee = left_join(resumen_asis_ee, resumen_asis_ee2, by = c("rut_sost_rbd2022", "nom_rbd", "rbd"))


resumen_asis_ee

resumen_asis_ee <- resumen_asis_ee %>% mutate(`3` = 0, `4` = 0) 
# resumen_asis_ee <- resumen_asis_ee %>% rowwise() %>% mutate(n_total2 = sum(`1`, `2`, `3`, `4`, `5`, na.rm = T))
resumen_asis_ee <- resumen_asis_ee %>% rowwise() %>% mutate(n_total2 = sum(`1`, `2`, `3`, `4`, na.rm = T))

resumen_asis_ee <- resumen_asis_ee %>% mutate(porc1 = ifelse(!is.na(`1`), paste0(100*round(`1`/n_total2, 3),"%"), "0%"))
resumen_asis_ee <- resumen_asis_ee %>% mutate(porc2 = ifelse(!is.na(`2`), paste0(100*round(`2`/n_total2, 3),"%"), "0%"))
resumen_asis_ee <- resumen_asis_ee %>% mutate(porc3 = ifelse(!is.na(`3`), paste0(100*round(`3`/n_total2, 3),"%"), "0%"))
resumen_asis_ee <- resumen_asis_ee %>% mutate(porc4 = ifelse(!is.na(`4`), paste0(100*round(`4`/n_total2, 3),"%"), "0%"))
# resumen_asis_ee <- resumen_asis_ee %>% mutate(porc5 = ifelse(!is.na(`5`), paste0(100*round(`5`/n_total2, 3),"%"), "0%"))



#Datos de desvinculados por EE para tabla 1.1
bd <- bd %>% mutate(retirados = ifelse(categoria_desert == "Retirado 2023",1,0))
bd2 <- bd2 %>% mutate(retirados = ifelse(categoria_desert == "Retirado 2023",1,0))
bd <- bd %>% mutate(desvinculados = ifelse(categoria_desert == "Desercion incidencia",1,0))

#Se calculan los retirados por rbd
n_ret_rbd <- bd2 %>%  group_by(rbd_ret) %>% summarise(n_ret = sum(retirados)) %>% ungroup() %>% select(rbd_ret, n_ret)
data_plot4_n_2 <- left_join(data_plot4_n_2, n_ret_rbd, by=c("rbd" = "rbd_ret"))
data_plot4_n_2 <- data_plot4_n_2 %>% rename(n_total0 = n_total)
data_plot4_n_2 <- data_plot4_n_2 %>% mutate(n_ret = ifelse(is.na(n_ret),0,n_ret))
data_plot4_n_2 <- data_plot4_n_2 %>% mutate(n_total = n_total0 + n_ret)


mat_2023_ee <- bd_asis %>% group_by(rbd) %>% summarise(n_total23 = n()) %>% ungroup()
mat_2023_ee <- mat_2023_ee %>% rename(rbd_2022r = rbd)


#resumen_desvinc_ee <- bd  %>%  group_by(rut_sost_rbd2022, nom_rbd2022r, rbd2022r, curso2022r) %>% summarise(n_total = n(), n = sum(deserta_aju_mayo))  #Para informe sostenedores
resumen_desvinc_ee <- bd %>% group_by(rut_sost_rbd2022, nom_rbd_2022r, rbd_2022r) %>% summarise(n_total = n(), n_desv = sum(desvinculados))#summarise(n_total = n(), n = sum(desert_glob_total_aju_marzo_2023))
resumen_desvinc_ee2 <- bd2 %>% group_by(rut_sost_rbd2022_ret, nom_rbd_ret, rbd_ret) %>% summarise(n_ret = sum(retirados)) %>% ungroup() %>% select(rbd_ret, n_ret)#summarise(n_total = n(), n = sum(desert_glob_total_aju_marzo_2023))
resumen_dobledesvinc_ee <- doble_desvinc %>% group_by(rbd) %>% summarise(n_dobledesv = n()) %>% ungroup() %>% select(rbd, n_dobledesv)#summarise(n_total = n(), n = sum(desert_glob_total_aju_marzo_2023))

resumen_desvinc_ee <- full_join(mat_2023_ee, resumen_desvinc_ee, by = c("rbd_2022r" = "rbd_2022r"))
resumen_desvinc_ee <- full_join(resumen_desvinc_ee, resumen_desvinc_ee2, by = c("rbd_2022r" = "rbd_ret"))
resumen_desvinc_ee <- left_join(resumen_desvinc_ee, resumen_dobledesvinc_ee, by = c("rbd_2022r" = "rbd"))



resumen_desvinc_ee <- resumen_desvinc_ee %>% ungroup()


resumen_desvinc_ee <- resumen_desvinc_ee%>% mutate(n_ret = ifelse(is.na(n_ret), 0, n_ret))
resumen_desvinc_ee <- resumen_desvinc_ee %>% mutate(n_desv = ifelse(is.na(n_desv), 0, n_desv))
resumen_desvinc_ee <- resumen_desvinc_ee %>% mutate(n_dobledesv = ifelse(is.na(n_dobledesv), 0, n_dobledesv))


resumen_desvinc_ee <- resumen_desvinc_ee %>% mutate(n_total23 = n_total23 + n_ret)
resumen_desvinc_ee <- resumen_desvinc_ee %>% mutate(n_total23 = ifelse(is.na(n_total23), 0, n_total23))

resumen_desvinc_ee <- resumen_desvinc_ee %>% mutate(porc_desvinc = ifelse(n_total > 0, round(n_desv/n_total,3), 0))
resumen_desvinc_ee <- resumen_desvinc_ee %>% mutate(porc_ret = ifelse(n_total23 > 0, round(n_ret/n_total23,3), 0))



###Se crean las variables con el cálculo usado para la desvinculación 2023
resumen_desvinc_ee <- resumen_desvinc_ee %>% mutate(n_mat_teo_total23 = n_total23 + n_desv)
resumen_desvinc_ee <- resumen_desvinc_ee %>% mutate(n_mat_teo_total23 = ifelse(is.na(n_mat_teo_total23), 0, n_mat_teo_total23))
resumen_desvinc_ee <- resumen_desvinc_ee %>% mutate(desv_total23 = n_ret + n_desv)
resumen_desvinc_ee <- resumen_desvinc_ee %>% mutate(porc_total23 = ifelse(n_mat_teo_total23 != 0, round(desv_total23/n_mat_teo_total23, 3), 0))


resumen_desvinc_ee <- resumen_desvinc_ee %>% mutate(porc_desvinc = ifelse(!is.na(porc_desvinc), paste0(round(porc_desvinc*100, 1),"%"), NA))
resumen_desvinc_ee <- resumen_desvinc_ee %>% mutate(porc_ret = ifelse(!is.na(porc_ret), paste0(round(porc_ret*100, 1),"%"), NA))
resumen_desvinc_ee <- resumen_desvinc_ee %>% mutate(porc_total23 = ifelse(!is.na(porc_total23), paste0(round(porc_total23*100, 1),"%"), NA))


#resumen_desvinc_ee <- resumen_desvinc_ee %>% arrange(nom_rbd_2022r, curso2022r) #Para informe sostenedores
#resumen_desvinc_ee <- resumen_desvinc_ee %>% arrange(porc_total23)


# 
# 
# 
# resumen_desvinc_ee <-resumen_desvinc_ee %>% mutate(n_ret = ifelse(is.na(n_ret), 0, n_ret))
# resumen_desvinc_ee <-resumen_desvinc_ee %>% mutate(n_dobledesv = ifelse(is.na(n_dobledesv), 0, n_dobledesv))
# 
# resumen_desvinc_ee <- left_join(mat_2023_com, resumen_desvinc_ee, by=c("rbd" =  "rbd_2022r"))
# resumen_desvinc_ee <- resumen_desvinc_ee %>% mutate(n_total23 = n_total23 + n_ret)
# 
# resumen_desvinc_ee <- resumen_desvinc_ee %>% mutate(porc_ret = ifelse(n_total23 > 0, round(n_ret/n_total23,3), 0))
# resumen_desvinc_ee <- resumen_desvinc_ee %>% mutate(porc_ret = ifelse(!is.na(porc_ret), paste0(round(porc_ret*100, 1),"%"), NA))

#resumen_desvinc_ee <- resumen_desvinc_ee %>% arrange(nom_rbd2022r, curso2022r) #Para informe sostenedores

#resumen_desvinc_ee$cod_depe2_2022r <- factor(resumen_desvinc_ee$cod_depe2_2022r, levels = c(1,2,3,4,5), labels = c("Municipal", "Particular Subvencionado", "Particular Pagado", "Administración Delegada", "Servicio Local de Educación"))

#resumen_desvinc_ee <- resumen_desvinc_ee %>% mutate(nom_rbd2022r = paste0(nom_rbd2022r, " (RBD: ", rbd2022r, ")") )  #Para informe sostenedores
#print(resumen_desvinc_ee)
## Se ajusta el nombre de los EEs en las bases de datos agregando el rbd
#resumen_desvinc_ee <- resumen_desvinc_ee %>% mutate(nom_rbd_2022r = paste0(nom_rbd_2022r, " (RBD: ", rbd_2022r, ")") )
#resumen_asis_ee <- resumen_asis_ee %>% mutate(nom_rbd = paste0(nom_rbd, " (RBD: ", rbd, ")") )


# table(bd_asis$nom_deprov_rbd, useNA = "a")
# table(bd_asis$nombre_sost_rbd, useNA = "a")
# colnames(bd)

#Se pegan los datos de matrícula 2023 a la bd de desvinculados y retirados

mat_2023_ee <- bd_asis %>% group_by(rbd) %>% summarise(n_total23 = n()) %>% ungroup()
mat_2023_ee_cur <- bd_asis %>% group_by(rbd, cod_curso) %>% summarise(n_total23 = n()) %>% ungroup()
mat_2023_ee <- mat_2023_ee %>% rename(rbd_2022r = rbd)
mat_2023_ee_cur <- mat_2023_ee_cur %>% rename(nom_grado = cod_curso, rbd_2022r = rbd)

#colnames(bd_asis)
datos_rbd2023 <- bd_asis %>% group_by(rbd) %>% slice(1) %>% select(rbd, cod_depe2, nom_rbd, cod_deprov_rbd, nom_deprov_rbd, cod_com_rbd, nom_com_rbd, cod_reg_rbd, cod_reg_rbd2022r2, rut_sost_rbd2022, nombre_sost_rbd2022, email_sost_rbd2022, tel_sost_rbd)
datos_rbd2022 <- bd %>% group_by(rbd_2022r) %>% slice(1) %>% select(rbd_2022r, nom_deprov_rbd2022, cod_deprov_rbd2022, cod_depe2_2022r, nom_rbd_2022r, cod_com_rbd_2022r, nom_com_rbd_2022r, cod_reg_rbd2022r2, rut_sost_rbd2022, nombre_sost_rbd2022, email_sost_rbd2022, tel_sost_rbd)
colnames(datos_rbd2022) <- paste0(colnames(datos_rbd2022),"_t_1")

#Datos de desvinculados por EE y curso para EXCEL
# xls_desvinc_ee_cur_pre <- bd_asis  %>%  group_by(rut_sost_rbd2022, rbd_2022r, nom_grado)
xls_desvinc_ee_cur <- bd  %>%  group_by(rbd_2022r, nom_grado) %>% summarise(n_total = n(), n_desv = sum(desvinculados, na.rm = T))  
xls_desvinc_ee_cur2 <- bd2  %>%  group_by(rbd_ret, nom_grado_ret) %>% summarise(n_ret = sum(retirados, na.rm = T)) %>% ungroup() %>% select(rbd_ret, nom_grado_ret, n_ret)
xls_dobledesvinc_ee_cur <- doble_desvinc %>% group_by(rbd, nom_grado) %>% summarise(n_dobledesv = n()) %>% ungroup() %>% select(rbd, nom_grado, n_dobledesv)

xls_desvinc_ee_cur <- full_join(mat_2023_ee_cur, xls_desvinc_ee_cur, by = c("rbd_2022r" = "rbd_2022r", "nom_grado" = "nom_grado"))
xls_desvinc_ee_cur <- full_join(xls_desvinc_ee_cur, xls_desvinc_ee_cur2, by = c("rbd_2022r" = "rbd_ret", "nom_grado" = "nom_grado_ret"))
xls_desvinc_ee_cur <- left_join(xls_desvinc_ee_cur, xls_dobledesvinc_ee_cur, by = c("rbd_2022r" = "rbd", "nom_grado" = "nom_grado"))



colnames(datos_rbd2022)
xls_desvinc_ee_cur <- left_join(xls_desvinc_ee_cur, datos_rbd2023, by = c("rbd_2022r" = "rbd"))
xls_desvinc_ee_cur <- left_join(xls_desvinc_ee_cur, datos_rbd2022, by = c("rbd_2022r" = "rbd_2022r_t_1"))
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% arrange(nom_rbd, nom_grado)


xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% ungroup()

xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(n_ret = ifelse(is.na(n_ret), 0, n_ret))
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(n_desv = ifelse(is.na(n_desv), 0, n_desv))
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(n_dobledesv = ifelse(is.na(n_dobledesv), 0, n_dobledesv))

xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(porc_desvinc = ifelse(n_total > 0, round(n_desv/n_total,3), 0))

colnames(xls_desvinc_ee_cur)


xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(cod_reg_rbd2022r2 = if_else(is.na(cod_reg_rbd2022r2), cod_reg_rbd2022r2_t_1, cod_reg_rbd2022r2))  #TAGDEPROV
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(nom_deprov_rbd = ifelse(is.na(nom_deprov_rbd), nom_deprov_rbd2022_t_1, nom_deprov_rbd))
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(nom_com_rbd = ifelse(is.na(nom_com_rbd), nom_com_rbd_2022r_t_1, nom_com_rbd))
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(nombre_sost_rbd2022 = ifelse(is.na(nombre_sost_rbd2022), nombre_sost_rbd2022_t_1, nombre_sost_rbd2022))
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(rut_sost_rbd2022 = ifelse(is.na(rut_sost_rbd2022), rut_sost_rbd2022_t_1, rut_sost_rbd2022))
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(email_sost_rbd2022 = ifelse(is.na(email_sost_rbd2022), email_sost_rbd2022_t_1, email_sost_rbd2022))
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(tel_sost_rbd = ifelse(is.na(tel_sost_rbd), tel_sost_rbd_t_1, tel_sost_rbd))
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(cod_depe2 = ifelse(is.na(cod_depe2), cod_depe2_2022r_t_1, cod_depe2))
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(nom_rbd = ifelse(is.na(nom_rbd), nom_rbd_2022r_t_1, nom_rbd))




#Datos de desvinculados por EE para EXCEL
xls_desvinc_ee <- bd %>%  group_by(rbd_2022r) %>% summarise(n_total = n(), n_desv = sum(desvinculados, na.rm = T)) # sum(desert_glob_total_aju_marzo_2023))
xls_desvinc_ee2 <- bd2  %>%  group_by(rbd_ret) %>% summarise(n_ret = sum(retirados, na.rm = T)) %>% ungroup() %>% select(rbd_ret, n_ret)
xls_dobledesvinc_ee <- doble_desvinc %>% group_by(rbd) %>% summarise(n_dobledesv = n()) %>% ungroup() %>% select(rbd, n_dobledesv)

xls_desvinc_ee <- full_join(mat_2023_ee, xls_desvinc_ee, by = c("rbd_2022r" = "rbd_2022r"))
xls_desvinc_ee <- full_join(xls_desvinc_ee, xls_desvinc_ee2, by = c("rbd_2022r" = "rbd_ret"))
xls_desvinc_ee <- left_join(xls_desvinc_ee, xls_dobledesvinc_ee, by = c("rbd_2022r" = "rbd"))


#nombre_reg <- bd_asis %>% group_by(cod_reg_rbd) %>% slice(1) %>% select(cod_reg_rbd, cod_reg_rbd2022r2)
xls_desvinc_ee <- left_join(xls_desvinc_ee, datos_rbd2023, by = c("rbd_2022r" = "rbd"))
xls_desvinc_ee <- left_join(xls_desvinc_ee, datos_rbd2022, by = c("rbd_2022r" = "rbd_2022r_t_1"))
xls_desvinc_ee <- xls_desvinc_ee %>% arrange(nom_rbd)


xls_desvinc_ee <- xls_desvinc_ee %>% ungroup()



xls_desvinc_ee <- xls_desvinc_ee %>% mutate(cod_reg_rbd2022r2 = if_else(is.na(cod_reg_rbd2022r2), cod_reg_rbd2022r2_t_1, cod_reg_rbd2022r2))  #TAGDEPROV
xls_desvinc_ee <- xls_desvinc_ee %>% mutate(nom_deprov_rbd = ifelse(is.na(nom_deprov_rbd), nom_deprov_rbd2022_t_1, nom_deprov_rbd))
xls_desvinc_ee <- xls_desvinc_ee %>% mutate(nom_com_rbd = ifelse(is.na(nom_com_rbd), nom_com_rbd_2022r_t_1, nom_com_rbd))
xls_desvinc_ee <- xls_desvinc_ee %>% mutate(nombre_sost_rbd2022 = ifelse(is.na(nombre_sost_rbd2022), nombre_sost_rbd2022_t_1, nombre_sost_rbd2022))
xls_desvinc_ee <- xls_desvinc_ee %>% mutate(rut_sost_rbd2022 = ifelse(is.na(rut_sost_rbd2022), rut_sost_rbd2022_t_1, rut_sost_rbd2022))
xls_desvinc_ee <- xls_desvinc_ee %>% mutate(email_sost_rbd2022 = ifelse(is.na(email_sost_rbd2022), email_sost_rbd2022_t_1, email_sost_rbd2022))
xls_desvinc_ee <- xls_desvinc_ee %>% mutate(tel_sost_rbd = ifelse(is.na(tel_sost_rbd), tel_sost_rbd_t_1, tel_sost_rbd))
xls_desvinc_ee <- xls_desvinc_ee %>% mutate(cod_depe2 = ifelse(is.na(cod_depe2), cod_depe2_2022r_t_1, cod_depe2))
xls_desvinc_ee <- xls_desvinc_ee %>% mutate(nom_rbd = ifelse(is.na(nom_rbd), nom_rbd_2022r_t_1, nom_rbd))





xls_desvinc_ee <- xls_desvinc_ee %>% mutate(n_ret = ifelse(is.na(n_ret), 0, n_ret))
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(n_ret = ifelse(is.na(n_ret), 0, n_ret))
xls_desvinc_ee <- xls_desvinc_ee %>% mutate(n_desv = ifelse(is.na(n_desv), 0, n_desv))
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(n_desv = ifelse(is.na(n_desv), 0, n_desv))
xls_desvinc_ee <- xls_desvinc_ee %>% mutate(n_dobledesv = ifelse(is.na(n_dobledesv), 0, n_dobledesv))
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(n_dobledesv = ifelse(is.na(n_dobledesv), 0, n_dobledesv))

xls_desvinc_ee <- xls_desvinc_ee %>% mutate(porc_desvinc = ifelse(n_total > 0, round(n_desv/n_total,3), 0))
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(porc_desvinc = ifelse(n_total > 0, round(n_desv/n_total,3), 0))

xls_desvinc_ee <- xls_desvinc_ee %>% mutate(n_total23 = n_total23 + n_ret)
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(n_total23 = n_total23 + n_ret)

xls_desvinc_ee <- xls_desvinc_ee %>% mutate(n_total23 = ifelse(is.na(n_total23), 0, n_total23))
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(n_total23 = ifelse(is.na(n_total23), 0, n_total23))

xls_desvinc_ee <- xls_desvinc_ee %>% mutate(porc_ret = ifelse(n_total23 > 0, round(n_ret/n_total23,3), 0))
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(porc_ret = ifelse(n_total23 > 0, round(n_ret/n_total23,3), 0))

###Se crean las variables con el cálculo usado para la desvinculación 2023
xls_desvinc_ee <-   xls_desvinc_ee %>% mutate(n_mat_teo_total23 = n_total23 + n_desv)
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(n_mat_teo_total23 = n_total23 + n_desv)

xls_desvinc_ee <- xls_desvinc_ee %>% mutate(n_mat_teo_total23 = ifelse(is.na(n_mat_teo_total23), 0, n_mat_teo_total23))
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(n_mat_teo_total23 = ifelse(is.na(n_mat_teo_total23), 0, n_mat_teo_total23))

xls_desvinc_ee <-   xls_desvinc_ee %>% mutate(desv_total23 = n_ret + n_desv)
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(desv_total23 = n_ret + n_desv + n_dobledesv)

xls_desvinc_ee <-   xls_desvinc_ee %>% mutate(porc_total23 = ifelse(n_mat_teo_total23 != 0, round(desv_total23/n_mat_teo_total23, 3), 0))
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate(porc_total23 = ifelse(n_mat_teo_total23 != 0, round(desv_total23/n_mat_teo_total23, 3), 0))



#Datos de asistencia por EE y curso para EXCEL

xsl_asis_ee_cur <- bd_asis %>% filter(cod_depe2 == 3) %>% group_by(rbd, cod_curso) %>% summarise(asis_marzo= mean(porcentage_asistencia_marzo, na.rm = TRUE), 
                                                                                                 # asis_abril = mean(porcentage_asistencia_abril, na.rm = TRUE), asis_mayo = mean(porcentage_asistencia_mayo, na.rm = TRUE), asis_junio = mean(porcentage_asistencia_junio, na.rm = TRUE),
                                                                                                 # asis_julio = mean(porcentage_asistencia_julio, na.rm = TRUE), asis_agosto = mean(porcentage_asistencia_agosto, na.rm = TRUE), 
                                                                                                 # asis_septiembre = mean(porcentage_asistencia_septiembre, na.rm = TRUE), asis_octubre = mean(porcentage_asistencia_octubre, na.rm = TRUE), asis_noviembre = mean(porcentage_asistencia_noviembre, na.rm = TRUE),
                                                                                                 asis_promedio = mean(porcentage_asistencia_acumulada, na.rm = TRUE), n_total = n(), n = sum(inasistencia_grave_acumulada, na.rm = T))
xsl_asis_ee_cur <- xsl_asis_ee_cur %>% mutate(porc_asis_crit = ifelse(n_total > 0, round(n/n_total,3), 0))
xsl_asis_ee_cur <- xsl_asis_ee_cur %>% ungroup()
xsl_asis_ee_cur <- xsl_asis_ee_cur %>% arrange(asis_promedio)
xsl_asis_ee_cur <- xsl_asis_ee_cur %>% mutate(asis_marzo = round(asis_marzo, 3))           #paste0(round(asis_marzo*100, 1),"%"))
# xsl_asis_ee_cur <- xsl_asis_ee_cur %>% mutate(asis_abril = round(asis_abril, 3))           #paste0(round(asis_abril*100, 1),"%"))
# xsl_asis_ee_cur <- xsl_asis_ee_cur %>% mutate(asis_mayo = round(asis_mayo, 3))
# xsl_asis_ee_cur <- xsl_asis_ee_cur %>% mutate(asis_junio = round(asis_junio, 3)) #paste0(round(asis_mayo*100, 1),"%"))
# xsl_asis_ee_cur <- xsl_asis_ee_cur %>% mutate(asis_julio = round(asis_julio, 3))
# xsl_asis_ee_cur <- xsl_asis_ee_cur %>% mutate(asis_agosto = round(asis_agosto, 3))
# xsl_asis_ee_cur <- xsl_asis_ee_cur %>% mutate(asis_septiembre = round(asis_septiembre, 3)) 
# xsl_asis_ee_cur <- xsl_asis_ee_cur %>% mutate(asis_octubre = round(asis_octubre, 3))
# xsl_asis_ee_cur <- xsl_asis_ee_cur %>% mutate(asis_noviembre = round(asis_noviembre, 3))
xsl_asis_ee_cur <- xsl_asis_ee_cur %>% mutate(asis_promedio = round(asis_promedio, 3))        #paste0(round(asis_promedio*100, 1),"%"))
xsl_asis_ee_cur <- xsl_asis_ee_cur %>% mutate(porc_asis_crit = round(porc_asis_crit, 3))       #paste0(round(porc_asis_crit*100, 1),"%"))
print(xsl_asis_ee_cur)


xsl_asis_ee_cur2 <- bd_asis %>% filter(cod_depe2 == 3) %>% group_by(rbd, cod_curso, asistencia_categorias_acumulada) %>% summarise(n_cat = n())
xsl_asis_ee_cur2 <- xsl_asis_ee_cur2 %>% ungroup()
xsl_asis_ee_cur2
xsl_asis_ee_cur2 <- xsl_asis_ee_cur2 %>% pivot_wider(names_from = asistencia_categorias_acumulada, values_from = n_cat)
xsl_asis_ee_cur2 %>% filter(`NA` > 0)
nrow(xsl_asis_ee_cur2)
nrow(xsl_asis_ee_cur)
xsl_asis_ee_cur = left_join(xsl_asis_ee_cur, xsl_asis_ee_cur2, by = c("rbd", "cod_curso"))

xsl_asis_ee_cur

xsl_asis_ee_cur <- xsl_asis_ee_cur %>% mutate(`3` = 0, `4` = 0) 
# xsl_asis_ee_cur <- xsl_asis_ee_cur %>% rowwise() %>% mutate(n_total2 = sum(`1`, `2`, `3`, `4`, `5`, na.rm = T))
xsl_asis_ee_cur <- xsl_asis_ee_cur %>% rowwise() %>% mutate(n_total2 = sum(`1`, `2`, `3`, `4`, na.rm = T))

xsl_asis_ee_cur <- xsl_asis_ee_cur %>% mutate(porc1 = ifelse(!is.na(`1`), round(`1`/n_total2, 3), 0))      #ifelse(!is.na(`1`), paste0(100*round(`1`/n_total2, 3),"%"), "0%"))
xsl_asis_ee_cur <- xsl_asis_ee_cur %>% mutate(porc2 = ifelse(!is.na(`2`), round(`2`/n_total2, 3), 0))      #ifelse(!is.na(`2`), paste0(100*round(`2`/n_total2, 3),"%"), "0%"))
xsl_asis_ee_cur <- xsl_asis_ee_cur %>% mutate(porc3 = ifelse(!is.na(`3`), round(`3`/n_total2, 3), 0))      #ifelse(!is.na(`3`), paste0(100*round(`3`/n_total2, 3),"%"), "0%"))
xsl_asis_ee_cur <- xsl_asis_ee_cur %>% mutate(porc4 = ifelse(!is.na(`4`), round(`4`/n_total2, 3), 0))      #ifelse(!is.na(`4`), paste0(100*round(`4`/n_total2, 3),"%"), "0%"))
# xsl_asis_ee_cur <- xsl_asis_ee_cur %>% mutate(porc5 = ifelse(!is.na(`5`), round(`5`/n_total2, 3), 0))      #ifelse(!is.na(`5`), paste0(100*round(`5`/n_total2, 3),"%"), "0%"))
xsl_asis_ee_cur <- xsl_asis_ee_cur %>% arrange(desc(porc_asis_crit))


xsl_asis_ee_cur <- left_join(xsl_asis_ee_cur, datos_rbd2023, by = c("rbd"))
xsl_asis_ee_cur <- xsl_asis_ee_cur %>% arrange(nom_rbd)
print(xsl_asis_ee_cur)


#Datos de asistencia por EE para EXCEL


xsl_asis_ee <- bd_asis %>% filter(cod_depe2 == 3) %>% group_by(rbd) %>% summarise(asis_marzo= mean(porcentage_asistencia_marzo, na.rm = TRUE), 
                                                                                  # asis_abril = mean(porcentage_asistencia_abril, na.rm = TRUE), asis_mayo = mean(porcentage_asistencia_mayo, na.rm = TRUE), asis_junio = mean(porcentage_asistencia_junio, na.rm = TRUE), 
                                                                                  # asis_julio = mean(porcentage_asistencia_julio, na.rm = TRUE), asis_agosto = mean(porcentage_asistencia_agosto, na.rm = TRUE), 
                                                                                  # asis_septiembre = mean(porcentage_asistencia_septiembre, na.rm = TRUE), asis_octubre = mean(porcentage_asistencia_octubre, na.rm = TRUE), asis_noviembre = mean(porcentage_asistencia_noviembre, na.rm = TRUE),
                                                                                  asis_promedio = mean(porcentage_asistencia_acumulada, na.rm = TRUE), n_total = n(), n = sum(inasistencia_grave_acumulada, na.rm = T))
xsl_asis_ee <- xsl_asis_ee %>% mutate(porc_asis_crit = ifelse(n_total > 0, round(n/n_total,3), 0))
xsl_asis_ee <- xsl_asis_ee %>% ungroup()
xsl_asis_ee <- xsl_asis_ee %>% arrange(asis_promedio)
xsl_asis_ee <- xsl_asis_ee %>% mutate(asis_marzo = round(asis_marzo, 3))           #paste0(round(asis_marzo*100, 1),"%"))
# xsl_asis_ee <- xsl_asis_ee %>% mutate(asis_abril = round(asis_abril, 3))           #paste0(round(asis_abril*100, 1),"%"))
# xsl_asis_ee <- xsl_asis_ee %>% mutate(asis_mayo = round(asis_mayo, 3))
# xsl_asis_ee <- xsl_asis_ee %>% mutate(asis_junio = round(asis_junio, 3))
# xsl_asis_ee <- xsl_asis_ee %>% mutate(asis_julio = round(asis_julio, 3))
# xsl_asis_ee <- xsl_asis_ee %>% mutate(asis_agosto = round(asis_agosto, 3))
# xsl_asis_ee <- xsl_asis_ee %>% mutate(asis_septiembre = round(asis_septiembre, 3))
# xsl_asis_ee <- xsl_asis_ee %>% mutate(asis_octubre = round(asis_octubre, 3))
# xsl_asis_ee <- xsl_asis_ee %>% mutate(asis_noviembre = round(asis_noviembre, 3))
xsl_asis_ee <- xsl_asis_ee %>% mutate(asis_promedio = round(asis_promedio, 3))        #paste0(round(asis_promedio*100, 1),"%"))
xsl_asis_ee <- xsl_asis_ee %>% mutate(porc_asis_crit = round(porc_asis_crit, 3))       #paste0(round(porc_asis_crit*100, 1),"%"))
print(xsl_asis_ee)


xsl_asis_ee2 <- bd_asis %>% filter(cod_depe2 == 3) %>% group_by(rbd, asistencia_categorias_acumulada) %>% summarise(n_cat = n())
xsl_asis_ee2 <- xsl_asis_ee2 %>% ungroup()
xsl_asis_ee2
xsl_asis_ee2 <- xsl_asis_ee2 %>% pivot_wider(names_from = asistencia_categorias_acumulada, values_from = n_cat)
xsl_asis_ee2 %>% filter(`NA` > 0)
nrow(xsl_asis_ee2)
nrow(xsl_asis_ee)
xsl_asis_ee = left_join(xsl_asis_ee, xsl_asis_ee2, by = "rbd")

xsl_asis_ee

xsl_asis_ee <- xsl_asis_ee %>% mutate(`3` = 0, `4` = 0) 

# xsl_asis_ee <- xsl_asis_ee %>% rowwise() %>% mutate(n_total2 = sum(`1`, `2`, `3`, `4`, `5`, na.rm = T))
xsl_asis_ee <- xsl_asis_ee %>% rowwise() %>% mutate(n_total2 = sum(`1`, `2`, `3`, `4`, na.rm = T))

xsl_asis_ee <- xsl_asis_ee %>% mutate(porc1 = ifelse(!is.na(`1`), round(`1`/n_total2, 3), 0))      #ifelse(!is.na(`1`), paste0(100*round(`1`/n_total2, 3),"%"), "0%"))
xsl_asis_ee <- xsl_asis_ee %>% mutate(porc2 = ifelse(!is.na(`2`), round(`2`/n_total2, 3), 0))      #ifelse(!is.na(`2`), paste0(100*round(`2`/n_total2, 3),"%"), "0%"))
xsl_asis_ee <- xsl_asis_ee %>% mutate(porc3 = ifelse(!is.na(`3`), round(`3`/n_total2, 3), 0))      #ifelse(!is.na(`3`), paste0(100*round(`3`/n_total2, 3),"%"), "0%"))
xsl_asis_ee <- xsl_asis_ee %>% mutate(porc4 = ifelse(!is.na(`4`), round(`4`/n_total2, 3), 0))      #ifelse(!is.na(`4`), paste0(100*round(`4`/n_total2, 3),"%"), "0%"))
# xsl_asis_ee <- xsl_asis_ee %>% mutate(porc5 = ifelse(!is.na(`5`), round(`5`/n_total2, 3), 0))      #ifelse(!is.na(`5`), paste0(100*round(`5`/n_total2, 3),"%"), "0%"))
xsl_asis_ee <- xsl_asis_ee %>% arrange(desc(porc_asis_crit))


xsl_asis_ee <- left_join(xsl_asis_ee, datos_rbd2023, by = c("rbd"))
xsl_asis_ee <- xsl_asis_ee %>% arrange(nom_rbd)
print(xsl_asis_ee)




#Se reordenan por comuna

xsl_asis_ee <- xsl_asis_ee %>% arrange(nom_rbd)
xsl_asis_ee_cur <- xsl_asis_ee_cur %>% arrange(nom_rbd)
xls_desvinc_ee <- xls_desvinc_ee %>% arrange(nom_rbd)
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% arrange(nom_rbd)

xsl_asis_ee$cod_depe2 <- factor(xsl_asis_ee$cod_depe2, levels = c(1,2,3,4,5), labels = c("Municipal", "Particular Subvencionado", "Particular Pagado", "Administración Delegada", "Servicio Local de Educación"))
xsl_asis_ee_cur$cod_depe2 <- factor(xsl_asis_ee_cur$cod_depe2, levels = c(1,2,3,4,5), labels = c("Municipal", "Particular Subvencionado", "Particular Pagado", "Administración Delegada", "Servicio Local de Educación"))
xls_desvinc_ee$cod_depe2_2022r <- factor(xls_desvinc_ee$cod_depe2, levels = c(1,2,3,4,5), labels = c("Municipal", "Particular Subvencionado", "Particular Pagado", "Administración Delegada", "Servicio Local de Educación"))
xls_desvinc_ee_cur$cod_depe2_2022r <- factor(xls_desvinc_ee_cur$cod_depe2, levels = c(1,2,3,4,5), labels = c("Municipal", "Particular Subvencionado", "Particular Pagado", "Administración Delegada", "Servicio Local de Educación"))

#AJUSTES DE COLUMNAS Y NOMBRES
#ASISTENCIA POR RBD
xsl_asis_ee <- xsl_asis_ee %>% select(rut_sost_rbd2022, rbd, nom_rbd, cod_reg_rbd2022r2, nom_com_rbd, rut_sost_rbd2022, asis_marzo, 
                                      # asis_abril, asis_mayo, asis_junio, asis_julio, asis_agosto, asis_septiembre, asis_octubre, asis_noviembre,
                                      # asis_promedio, n_total, n, porc_asis_crit, "1", "2", "3", "4", "5", "NA", porc1, porc2, porc3,
                                      asis_promedio, n_total, n, porc_asis_crit, "1", "2", "3", "4", "NA", porc1, porc2, porc3, porc4) # n_total2


#ASISTENCIA POR RBD Y CURSO
xsl_asis_ee_cur <- xsl_asis_ee_cur %>% select(rut_sost_rbd2022, rbd, nom_rbd, cod_reg_rbd2022r2, nom_com_rbd, rut_sost_rbd2022, cod_curso, asis_marzo, 
                                              # asis_abril, asis_mayo, asis_junio, asis_julio, asis_agosto, asis_septiembre, asis_octubre, asis_noviembre,
                                              # asis_promedio, n_total, n, porc_asis_crit, "1", "2", "3", "4", "5", "NA", porc1, porc2, porc3, porc4, porc5) # n_total2
                                              asis_promedio, n_total, n, porc_asis_crit, "1", "2", "3", "4", "NA", porc1, porc2, porc3, porc4) # n_total2


#DESVINCULADOS POR RBD
xls_desvinc_ee <- xls_desvinc_ee %>% select(rut_sost_rbd2022, rbd_2022r, nom_rbd, cod_reg_rbd, nom_com_rbd, n_mat_teo_total23, desv_total23, porc_total23, n_ret, n_desv, n_dobledesv)




#DESVINCULADOS POR RBD Y CURSO
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% select(rut_sost_rbd2022, rbd_2022r, nom_rbd, cod_reg_rbd, nom_com_rbd, nom_grado, desv_total23, n_ret, n_desv, n_dobledesv)

#Se construye base de datos para resumen por establecimiento.
tbl_desvinc_ee <- xls_desvinc_ee %>% select(rbd_2022r, n_mat_teo_total23, desv_total23, porc_total23, n_ret, n_desv, n_dobledesv)
#tbl_desvinc_ee <- tbl_desvinc_ee %>% rename(n_descinv = desv_total23, n_total_desvinc = n_mat_teo_total23)

tbl_asis_ee <- xsl_asis_ee %>% select(rut_sost_rbd2022, rbd, nom_rbd, nom_com_rbd, asis_marzo, asis_promedio,
                                      # asis_abril, asis_mayo, asis_junio, asis_julio, asis_agosto, asis_septiembre, asis_octubre, asis_noviembre,
                                      n_total, n, porc_asis_crit)

resumen_rbd <- left_join(tbl_asis_ee, tbl_desvinc_ee, by = c("rbd"="rbd_2022r"))
resumen_rbd <- resumen_rbd %>% arrange(nom_rbd)
colnames(resumen_rbd)
(resumen_rbd)
resumen_rbd <- resumen_rbd %>% mutate(asis_marzo = scales::percent(asis_marzo, accuracy = 0.1))
# resumen_rbd <- resumen_rbd %>% mutate(asis_abril = scales::percent(asis_abril, accuracy = 0.1))
# resumen_rbd <- resumen_rbd %>% mutate(asis_mayo = scales::percent(asis_mayo, accuracy = 0.1))
# resumen_rbd <- resumen_rbd %>% mutate(asis_junio = scales::percent(asis_junio, accuracy = 0.1))
# resumen_rbd <- resumen_rbd %>% mutate(asis_julio = scales::percent(asis_julio, accuracy = 0.1))
# resumen_rbd <- resumen_rbd %>% mutate(asis_agosto = scales::percent(asis_agosto, accuracy = 0.1))
# resumen_rbd <- resumen_rbd %>% mutate(asis_septiembre = scales::percent(asis_septiembre, accuracy = 0.1))
# resumen_rbd <- resumen_rbd %>% mutate(asis_octubre = scales::percent(asis_octubre, accuracy = 0.1))
# resumen_rbd <- resumen_rbd %>% mutate(asis_noviembre = scales::percent(asis_noviembre, accuracy = 0.1))
resumen_rbd <- resumen_rbd %>% mutate(asis_promedio = scales::percent(asis_promedio, accuracy = 0.1))
resumen_rbd <- resumen_rbd %>% mutate(porc_asis_crit = scales::percent(porc_asis_crit, accuracy = 0.1))
resumen_rbd <- resumen_rbd %>% mutate(porc_total23 = scales::percent(porc_total23, accuracy = 0.1))
# resumen_rbd <- resumen_rbd %>% mutate(porc_ret = scales::percent(porc_ret, accuracy = 0.1))

(resumen_rbd)

# resumen_rbd <- resumen_rbd %>% select(-c(rut_sost_rbd2022))
# resumen_rbd <- left_join(resumen_rbd, bd_rbd_rutsost, by = c("rbd" = "rbd"))
# 
# xsl_asis_ee_cur <- xsl_asis_ee_cur %>% select(-c(rut_sost_rbd2022))
# xsl_asis_ee_cur <- left_join(xsl_asis_ee_cur, bd_rbd_rutsost, by = c("rbd" = "rbd"))
# 
# xsl_asis_ee <- xsl_asis_ee %>% select(-c(rut_sost_rbd2022))
# xsl_asis_ee <- left_join(xsl_asis_ee, bd_rbd_rutsost, by = c("rbd" = "rbd"))
# 
# xls_desvinc_ee <- xls_desvinc_ee %>% select(-c(rut_sost_rbd2022))
# xls_desvinc_ee <- left_join(xls_desvinc_ee, bd_rbd_rutsost, by = c("rbd_2022r" = "rbd"))
# 
# xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% select(-c(rut_sost_rbd2022))
# xls_desvinc_ee_cur <- left_join(xls_desvinc_ee_cur, bd_rbd_rutsost, by = c("rbd_2022r" = "rbd"))


xsl_asis_ee <- xsl_asis_ee %>% mutate_if(is.character, ~gsub('[^ -~]', ' ', .))
xsl_asis_ee_cur <- xsl_asis_ee_cur %>% mutate_if(is.character, ~gsub('[^ -~]', ' ', .))
xls_desvinc_ee <- xls_desvinc_ee %>% mutate_if(is.character, ~gsub('[^ -~]', ' ', .))
xls_desvinc_ee_cur <- xls_desvinc_ee_cur %>% mutate_if(is.character, ~gsub('[^ -~]', ' ', .))


colnames(resumen_rbd) <- c("rut_sost_rbd2022", "RBD", "Nombre establecimiento", "Comuna", "Asistencia promedio marzo", "Asistencia promedio acumulada", 
                           #"Asistencia promedio abril", "Asistencia promedio mayo", "Asistencia promedio junio", "Asistencia promedio julio", "Asistencia promedio agosto", "Asistencia promedio sept.", "Asistencia promedio octubre", "Asistencia promedio nov.", 
                           "Matrícula 2023", "N° estud. con asistencia <85%", "% estud. con asistencia <85%", "Matrícula 2023 + estud. sin matrícula", "N° Estud. 2022-2023 sin matricula vigente", "% Estud. 2022-2023 sin matricula vigente", "N° Estud. 2023 retirados en marzo y sin matrícula vigente", "N° Estud. 2022 no matriculados en 2023", "Detalle N° Estud. 2021 no matriculados ni en 2022 ni 2023")
#"Matrícula 2023", "N° estudiantes con asistencia <85%", "% estudiantes con asistencia <85%", "Matrícula 2023 + estud. sin matrícula", "N° Estud. 2022-2023 sin matricula vigente", "% Estud. 2022-2023 sin matricula vigente", "Detalle N° Estudiantes 2023 retirados en marzo y que están sin matrícula vigente", "Detalle N° Estudiantes 2022 que no se han matriculado en marzo 2023", "Detalle N° Estudiantes 2021 que no se han matriculado ni en 2022 ni 2023", "rut_sost_rbd2022")




colnames(xsl_asis_ee_cur) <- c("rut_sost_rbd2022","RBD", "Nombre Establecimiento", "Región", "Comuna", "Grado 2023", "Asistencia promedio marzo",
                               "Asistencia promedio acumulada", "N° Estudiantes totales", "N° estudiantes con asistencia <85%", "% estudiantes con asistencia <85%", "N° Estudiantes con 0-49% asistencia", "N° Estudiantes con 50-84% asistencia", 
                               "N° Estudiantes con 85-89% asistencia", "N° Estudiantes con 90-100% asistencia", "N° Estudiantes sin información", "% Estudiantes con 0-49% asistencia", "% Estudiantes con 50-84% asistencia", 
                               "% Estudiantes con 85-89% asistencia", "% Estudiantes con 90-100% asistencia", "% Estudiantes sin información")



# # "Asistencia promedio abril", "Asistencia promedio mayo", "Asistencia promedio junio","Asistencia promedio julio","Asistencia promedio agosto", "Asistencia promedio septiembre", "Asistencia promedio octubre", "Asistencia promedio noviembre", 
# "Asistencia promedio acumulada", "N° Estudiantes totales",
# "N° estudiantes totales con asistencia <85%", "% estudiantes totales con asistencia <85%", "N° Estudiantes con 0-49% asistencia", "N° Estudiantes con 50-84% asistencia", 
# "N° Estudiantes con 85-89% asistencia", "N° Estudiantes con 90-100% asistencia", 
# #"N° Estudiantes con 85-100% asistencia", 
# "N° Estudiantes sin información",
# "% Estudiantes con 0-49% asistencia", "% Estudiantes con 50-84% asistencia", 
# "% Estudiantes con 85-89% asistencia", "% Estudiantes con 90-100% asistencia", 
# # "% Estudiantes con 85-100% asistencia", 
# "rut_sost_rbd2022")


colnames(xsl_asis_ee) <- c("rut_sost_rbd2022", "RBD", "Nombre Establecimiento", "Región", "Comuna", "Asistencia promedio marzo",
                           # "Asistencia promedio abril", "Asistencia promedio mayo", "Asistencia promedio junio","Asistencia promedio julio","Asistencia promedio agosto", "Asistencia promedio septiembre", "Asistencia promedio octubre", "Asistencia promedio noviembre", 
                           "Asistencia promedio acumulada", "N° Estudiantes totales",  "N° estudiantes con asistencia <85%", "% estudiantes con asistencia <85%", "N° Estudiantes con 0-49% asistencia", "N° Estudiantes con 50-84% asistencia", 
                           "N° Estudiantes con 85-89% asistencia", "N° Estudiantes con 90-100% asistencia", "N° Estudiantes sin información",
                           "% Estudiantes con 0-49% asistencia", "% Estudiantes con 50-84% asistencia", "% Estudiantes con 85-89% asistencia", "% Estudiantes con 90-100% asistencia", "% Estudiantes sin información")


colnames(xls_desvinc_ee) <- c("rut_sost_rbd2022", "RBD", "Nombre Establecimiento", "Región", "Comuna", "Matrícula 2023 + estudiantes sin matrícula", "N° Estudiantes 2022-2023 sin matricula vigente", "% Estudiantes 2022-2023 sin matrícula vigente", "N° Estudiantes 2023 retirados en marzo y sin matrícula vigente", "N° Estudiantes 2022 no matriculados en 2023", "Detalle N° Estudiantes 2021 no matriculados ni en 2022 ni 2023")

colnames(xls_desvinc_ee_cur) <- c("rut_sost_rbd2022", "RBD", "Nombre Establecimiento", "Región", "Comuna", "Último grado registrado", "N° Total Estudiantes sin matricula vigente",
                                  "Detalle N° Estudiantes 2023 retirados en marzo y sin matrícula vigente", "Detalle N° Estudiantes 2022 no matriculados en 2023", "Detalle N° Estudiantes 2021 no matriculados ni en 2022 ni 2023")

#**************************************************************************************************************************/
# 3. Loop de Render para cada RBD  ----------------------------------------------------------------
#**************************************************************************************************************************/

## 3.1 Ajuste fuente gráficos  ----------------------------------------------------------------
#**************************************************************************************************************************/
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
  color = '#004677')
f2 <- list(
  family = "verdana",
  size = 18,
  color = '#004677')

m <- list(
  l = 40,
  pad = 5
)

f3 <- list(
  family = "verdana",
  size = 12,
  color = '#004677')

f4 <- list(
  family = "verdana",
  color = '#007096',
  size = 17)

m2 <- list(
  l = 50,
  r = 50,
  b = 50,
  t = 50,
  pad = 3
)

table(bd_asis$cod_curso)

#**************************************************************************************************************************/
## 3.2 Loop  ----------------------------------------------------------------
#**************************************************************************************************************************/

#se arregla nombre con error de estudiante
bd2 <- bd2 %>% mutate(nom_alu_ret = ifelse(run_alu == "100565271", "DJOULISSA", nom_alu_ret))

ruts_sost <- unique(bd_asis$rut_sost_rbd2022)


i = 0
nn = length(ruts_sost)
nn

rutsost1 <- ruts_sost#[1:1178]  #[1179:2356]   [2357:3534]   #[3535:4712]

i = 0
nn = length(rutsost1)
nn

#Búsqueda pendientes
{
  head(rutsost1)
  #head(ruts_sost,10)
  
  # Se buscan los pendientes. ------
  df <- data.frame(rbd = rutsost1 )
  df
  
  archivos <- list.files("Outputs/Sostenedores 0 (PP EPJA)")
  pdfs <- data.frame(nombre_pdf = archivos)
  head(pdfs)
  pdfs <- pdfs%>% filter(!grepl(".tex", nombre_pdf, fixed = TRUE))
  pdfs <- pdfs%>% filter(!grepl(".xlsx", nombre_pdf, fixed = TRUE))
  colnames(pdfs)
  pdfs <- pdfs %>% filter(nombre_pdf != "NA.pdf") %>% mutate(rbd = gsub("-EPJA.pdf", "", nombre_pdf)) %>% mutate(rbd = as.numeric(rbd))
  #(pdfs)
  head(pdfs)
  nrow(pdfs)
  #
  
  df <- left_join(df, pdfs, by = "rbd")
  errores <- df %>% filter(is.na(nombre_pdf))
  errores <- unique(errores$rbd)
  errores <- errores[!is.na(errores)]
  errores
  
  i = 0
  nn <- length(errores)
  nn
  
  # pendientes <- unique(c(sost_largos$rut_sost_rbd2022, slep))
  # pendientes
}
######

# sost_largos <- bd_asis %>% filter(!is.na(rut_sost_rbd2022)) %>% group_by(rut_sost_rbd2022, rbd) %>% summarise(n = n()) %>% group_by(rut_sost_rbd2022) %>% summarise(n = n()) %>% arrange(desc(n)) %>% filter(n > 35) #%>% nrow()
# sost_largos
#slep <- c(62000660, 62000810, 61999330, 61999320, 62000820, 62000790, 65181672, 65154272, 62000800, 62000650, 65154021)


#######
#rut_sost = 62000660
for(rut_sost in rutsost1){     #rutsost1 #c(42,53,101,136,134)
  try({
    tic()
    #**************************************************************************************************************************/
    ## 3.1 PRE RENDER  ----------------------------------------------------------------
    #**************************************************************************************************************************/
    i = i+1
    pass <- rut_sost
    ########## SE COMIENZA CON DESVINCULADOS
    #desvinc <- bd[bd$rut_sost_rbd2022 == rut_sost,]
    desvinc <- bd[bd$rut_sost_rbd2022 == rut_sost,]
    nrow(desvinc)
    data_plot2 <- data.frame(nom_grado = unique(desvinc$nom_grado))
    
    data_plot4 <- data_plot4_n %>% filter(rbd_2022r %in% unique(desvinc$rbd_2022r))
    
    n_mat2022 <- nrow(desvinc)
    # Dejamos solo estudiantes desertores en mayo y por si acaso se filtra fallecidos, aunque ya vienen como deserción == 0
    desvinc <- desvinc %>% filter(categoria_desert == "Desercion incidencia")    #(desert_glob_total_aju_marzo_2023 == 1 & fallece_tot == 0)
    desvinc2 <- bd2[bd2$rut_sost_rbd2022_ret == rut_sost,]
    desvinc2 <- desvinc2 %>% filter(categoria_desert == "Retirado 2023")
    desvinc3 <- doble_desvinc[doble_desvinc$rut_sost_rbd2022 == rut_sost,]
    
    # n_desvinc <- nrow(desvinc)
    # n_desvinc2 <- nrow(desvinc2)
    # n_desvinc3 <- nrow(desvinc3)
    
    
    n_desvinc <- xls_desvinc_ee[xls_desvinc_ee$rut_sost_rbd2022 == rut_sost,]
    n_desvinc = sum(n_desvinc$`N° Estudiantes 2022 no matriculados en 2023`, na.rm = T) # nrow(desvinc2)
    
    n_desvinc2 <- xls_desvinc_ee[xls_desvinc_ee$rut_sost_rbd2022 == rut_sost,]
    n_desvinc2 = sum(n_desvinc2$`N° Estudiantes 2023 retirados en marzo y sin matrícula vigente`, na.rm = T) # nrow(desvinc2)
    
    n_desvinc3 <- xls_desvinc_ee[xls_desvinc_ee$rut_sost_rbd2022 == rut_sost,]
    n_desvinc3 = sum(n_desvinc3$`Detalle N° Estudiantes 2021 no matriculados ni en 2022 ni 2023`, na.rm = T) # nrow(desvinc2)
    
    
    desvinc <- desvinc %>% select("run_alu2", "nom_alu2022", "app_alu2022", "apm_alu2022", "gen_alu_2022r", "edad_alu_2022r", "nom_com_alu2022", "nom_grado", "prom_gral2022", "asistencia2022", "sit_fin_r_2022r", 
                                  "rbd_2022r", "nom_rbd_2022r", "cod_depe2_2022r", "cod_reg_rbd2022r2", "cod_com_rbd_2022r", "nom_com_rbd_2022r", "cod_deprov_rbd2022", "nom_deprov_rbd2022", "nombre_sost_rbd2022", "rut_sost_rbd2022", "email_sost_rbd2022",  "tel_sost_rbd", "cert_val_22", "valid_estud")
    desvinc <- desvinc %>% arrange(nom_rbd_2022r, nom_grado)
    
    # desvinc2 <- desvinc2 %>% select("run_alu2_ret", "nom_alu_ret", "app_alu_ret", "apm_alu_ret", "gen_alu_ret", "edad_alu_ret", "nom_com_alu_ret", "nom_grado", "fec_ret_rbd_ret", "valid_estud")
    # colnames(desvinc2)
    
    desvinc2 <- desvinc2 %>% select("run_alu2_ret", "nom_alu_ret", "app_alu_ret", "apm_alu_ret", "gen_alu_ret", "edad_alu_ret", "nom_com_alu_ret", "nom_grado_ret", "fec_ret_rbd_ret", "valid_estud",
                                    "rbd_ret", "nom_rbd_ret", "cod_depe2_ret", "cod_reg_rbd_ret", "cod_com_rbd_ret", "nom_com_rbd_ret", "cod_deprov_rbd_ret") 
    desvinc2 <- desvinc2 %>% arrange(nom_rbd_ret, nom_grado_ret)
    
    desvinc3 <- desvinc3 %>% select("run_alu2", "nom_alu", "app_alu", "apm_alu", "gen_alu", "edad_alu", "nom_com_alu", "nom_grado", "sit_fin_r", "rbd", "nom_rbd", "cod_depe2", "cod_reg_rbd", "nom_com_rbd", "cert_val_21", "cert_val_22", "insc_val_23") # cert vald 21? muchos vacios
    desvinc3 <- desvinc3 %>% arrange(nom_rbd, nom_grado)
    
    ########## SE EXTRAEN LOS CASOS DE INASISTENCIA Y PARAMETROS 
    asis_crit <- bd_asis[bd_asis$rut_sost_rbd2022 == rut_sost,]
    
    data_plot2_2 <- data.frame(nom_grado = unique(asis_crit$nom_grado))
    #data_plot4_2 <- data_plot4_n_2 %>% filter(nom_rbd %in% unique(asis_crit$nom_rbd)) #data.frame(nom_rbd2022r = unique(desvinc$nom_rbd2022r))
    data_plot4_2 <- data_plot4_n_2 %>% filter(rbd %in% unique(asis_crit$rbd))
    
    
    data_plot3 <- data.frame(cod_curso = unique(asis_crit$cod_curso))
    data_plot3_n <- asis_crit %>% group_by(cod_curso) %>% summarise(n_total = n())
    #data_plot6 <- data_plot6_n %>% filter(nom_rbd %in% unique(asis_crit$nom_rbd))
    data_plot6 <- data_plot6_n %>% filter(rbd %in% unique(asis_crit$rbd))
    
    
    #nom_rbd <- asis_crit$nom_rbd[1]
    nom_sost <- asis_crit$nombre_sost_rbd2022[1]
    #nom_deprov <- asis_crit$cod_reg_rbd2022r2[1]
    
    ###PRUEBA
    
    # n_mat2023 = nrow(bd_asis_todos[bd_asis_todos$cod_reg_rbd == cod_reg,])
    n_mat2023 = xls_desvinc_ee[xls_desvinc_ee$rut_sost_rbd2022 == rut_sost,] #nrow(asis_crit)
    n_mat2023 = sum(n_mat2023$`Matrícula 2023 + estudiantes sin matrícula`, na.rm = T) #nrow(xls_desvinc_ee[xls_desvinc_ee$cod_reg_rbd2022r2 == cod_reg | xls_desvinc_ee$cod_reg_rbd_ret  == cod_reg,]) #+ nrow(desvinc2)
    # 
    # 
    
    #n_mat2023 = nrow(asis_crit)
    n_mat_asis2023 = xsl_asis_ee[xsl_asis_ee$rut_sost_rbd2022 == rut_sost,] #nrow(asis_crit)
    n_mat_asis2023 = sum(n_mat_asis2023$`N° Estudiantes totales`, na.rm = T) #nrow(xls_desvinc_ee[xls_desvinc_ee$cod_reg_rbd2022r2 == cod_reg | xls_desvinc_ee$cod_reg_rbd_ret  == cod_reg,]) #+ nrow(desvinc2)
    
    
    
    
    
    print("-----")
    print(nom_sost)
    print(rut_sost)
    
    
    #BD resumen EE
    resumen_asis_sost <- resumen_asis_ee %>% filter(rut_sost_rbd2022 == rut_sost)
    
    resumen_asis_sost <- resumen_asis_sost %>% select(rbd, nom_rbd, asis_promedio, asis_marzo, 
                                                      # asis_abril, asis_mayo, asis_junio, asis_julio, asis_agosto, asis_septiembre, asis_octubre, asis_noviembre, 
                                                      #n_total, n, porc_asis_crit, porc1, porc2, porc3, porc4, porc5)
                                                      n_total, n, porc_asis_crit, porc1, porc2, porc3, porc4)
    #resumen_asis_sost$cod_depe2 <- factor(resumen_asis_sost$cod_depe2, levels = c(1,2,3,4,5), labels = c("Municipal", "Particular Subvencionado", "Particular Pagado", "Administración Delegada", "Servicio Local de Educación"))
    
    print("hola")
    resumen_rbd_ee <- resumen_rbd %>% filter(rut_sost_rbd2022 == rut_sost)
    # print("chasaaao")
    resumen_rbd_ee <- resumen_rbd_ee %>% select(-c("rut_sost_rbd2022"))
    # print("chao")
    
    print(resumen_asis_sost)
    print("-----")  
    resumen_desvinc_sost <- resumen_desvinc_ee %>% filter(rut_sost_rbd2022 == rut_sost)# & (n_desv > 0 | n_ret > 0))
    print(resumen_desvinc_sost)
    print("***")
    
    #Datos gráfico de barra de rangos de asistencia
    data_plot5 <- data.frame(tipo_asis_2 = c("Inasist. crítica (0%-49% asist.)", "Inasist. grave (50%-84% asist.)", "Inasist. reiterada (85%-89% asist.)", "Asist. esperada (90%-100% asist.)", "Sin información"), color = c(rojo, amarillo, celeste, azul, gris))
    data_plot5$tipo_asis_2 <- factor(data_plot5$tipo_asis_2, levels = c("Inasist. crítica (0%-49% asist.)", "Inasist. grave (50%-84% asist.)","Inasist. reiterada (85%-89% asist.)", "Asist. esperada (90%-100% asist.)", "Sin información"), labels =  c("Inasist. crítica (0%-49% asist.)", "Inasist. grave (50%-84% asist.)", "Inasist. reiterada (85%-89% asist.)", "Asist. esperada (90%-100% asist.)", "Sin información"))
    
    
    
    data_plot5$tipo_asis_2
    table(asis_crit$tipo_asis_2)
    if(nrow(asis_crit) > 0){
      data_plot5_t <- prop.table(table(asis_crit$tipo_asis_2))
      data_plot5_t <- data.frame("tipo_asis_2"=names(data_plot5_t), "prop" = as.numeric(data_plot5_t))
    } else{
      data_plot5_t <- data.frame(tipo_asis_2= c("Inasist. crítica (0%-49% asist.)", "Inasist. grave (50%-84% asist.)", "Inasist. reiterada (85%-89% asist.)", "Asist. esperada (90%-100% asist.)", "Sin información"), prop = c(0,0,0,0,0))
    }
    data_plot5_t 
    data_plot5 <- left_join(data_plot5, data_plot5_t, by = "tipo_asis_2")
    data_plot5 <- data_plot5 %>% mutate(prop = ifelse(is.na(prop), 0 , prop))
    
    #----
    
    asis_crit <- asis_crit %>% filter(inasistencia_grave_acumulada == TRUE) # | tipo_asis_2 == "Sin información")
    n_inasis2023 = nrow(asis_crit)
    asis_crit <- asis_crit %>% select("run_alu", "nom_alu", "app_alu", "apm_alu", "cod_curso", "rbd", "nom_rbd", "nom_com_rbd", "nom_reg_rbd_a", "porcentage_asistencia_acumulada", "inasistencia_grave_acumulada", "asistencia_categorias_acumulada", "tipo_asis_2")
    asis_crit <- asis_crit %>% arrange(nom_rbd, cod_curso, porcentage_asistencia_acumulada)
    nrow(asis_crit)
    asis_crit <- asis_crit %>% mutate(porcentage_asistencia_acumulada = paste0(round(porcentage_asistencia_acumulada*100,0),"%"))
    
    ######  GRAFICO TORTA SIT_FIN
    ######
    #data_plot1 <- data.frame(sit_fin_r_2022r = c("Promovido", "Reprobado", "Retirado"), color = c('#004677', '#6bccd6', '#e94860'))
    #data_plot1$sit_fin_r_2022r <- factor(data_plot1$sit_fin_r_2022r, levels =  c("Promovido", "Reprobado", "Retirado"), labels =  c("Promovido", "Reprobado", "Retirado"))
    data_plot1 <- data.frame(sit_fin_r_2022r = c("Promovido", "Reprobado", "Retirado", "Sin Registro"), color = c('#004677', '#6bccd6', '#e94860', '#e4db89'))
    data_plot1$sit_fin_r_2022r <- factor(data_plot1$sit_fin_r_2022r, levels =  c("Promovido", "Reprobado", "Retirado", "Sin Registro"), labels =  c("Promovido", "Reprobado", "Retirado", "Sin Registro"))
    
    
    
    if(nrow(desvinc) > 0){
      data_plot1_t <- prop.table(table(desvinc$sit_fin_r_2022r))
      data_plot1_t <- data.frame("sit_fin_r_2022r"=names(data_plot1_t), "prop" = as.numeric(data_plot1_t))
    } else{
      #data_plot1_t <- data.frame(sit_fin_r_2022r= c("Promovido", "Reprobado", "Retirado"), prop = c(0,0,0))
      data_plot1_t <- data.frame(sit_fin_r_2022r= c("Promovido", "Reprobado", "Retirado", "Sin Registro"), prop = c(0,0,0,0))
    }
    
    data_plot1 <- left_join(data_plot1, data_plot1_t, by = "sit_fin_r_2022r")
    data_plot1 <- data_plot1 %>% mutate(prop = ifelse(is.na(prop), 0 , prop))
    
    
    
    ########## GRAFICO TORTA SIT_FIN
    
    
    p <- plot_ly(data_plot1, labels = ~sit_fin_r_2022r, values = ~round(prop,3), type = 'pie', hole = 0, textposition = 'outside', 
                 textinfo = 'label+percent', marker = list(colors = ~color)) %>% 
      layout(legend = list(font = f2, orientation = "h", x=0.1 , y=1.6), font=f2, xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
             paper_bgcolor='transparent', autosize = TRUE
      )
    
    url_sin_fin_g = paste0("/Img temp/",rut_sost,"_sit_fin_g.png")
    orca(p, url_sin_fin_g)
    
    ########## GRAFICO BARRA CURSO DESVINC
    
    
    data_plot2 <- data_plot2 %>% arrange(nom_grado)
    data_plot2_t <- desvinc %>% group_by(nom_grado) %>% summarise(n = n())
    data_plot2  <- left_join(data_plot2, data_plot2_t, by="nom_grado")
    data_plot2  <- data_plot2 %>% mutate(n = ifelse(is.na(n), 0, n))
    
    data_plot2 <- data_plot2 %>% droplevels()
    
    k <- plot_ly(x = ~data_plot2$nom_grado, y = ~data_plot2$n , type = "bar", marker = list(color = '#6bccd6'), text = data_plot2$n, texttemplate = '%{y}', textposition = 'outside') %>%
      layout(title="", xaxis=list(font = list(family = "verdana", color = '#007096', size = 20), title='<b>Grado 2022</b>'), yaxis=list(font = f4, title="<b>N° Estudiantes</b>", autotick = FALSE, dtick = 5), 
             font=list(family = "verdana", color = '#007096', size = 20), plot_bgcolor='transparent', paper_bgcolor='transparent', autosize = F, width = 1400, height = 900, margin = list(l=-0.1, t = -0.1, r=-0.1))
    
    url_cur_desv_g = paste0("/Img temp/", rut_sost,"_cur_desv_g.png")
    orca(k, url_cur_desv_g)
    
    
    ########## GRAFICO BARRA CURSO RETIRADOS
    
    data_plot2_2 <- data_plot2_2 %>% arrange(nom_grado)
    data_plot2_t_2 <- desvinc2 %>% group_by(nom_grado_ret) %>% summarise(n = n())
    data_plot2_2  <- left_join(data_plot2_2, data_plot2_t_2, by=c("nom_grado"="nom_grado_ret"))
    data_plot2_2  <- data_plot2_2 %>% mutate(n = ifelse(is.na(n), 0, n))
    
    data_plot2_2 <- data_plot2_2 %>% droplevels()
    
    k2 <- plot_ly(x = ~data_plot2_2$nom_grado, y = ~data_plot2_2$n , type = "bar", marker = list(color = '#6bccd6'), text = data_plot2_2$n, texttemplate = '%{y}', textposition = 'outside') %>%
      layout(title="", xaxis=list(font = list(family = "verdana", color = '#007096', size = 20), title='<b>Grado 2023</b>'), yaxis=list(font = f4, title="<b>N° Estudiantes</b>", autotick = FALSE, dtick = 5), 
             font=list(family = "verdana", color = '#007096', size = 20), plot_bgcolor='transparent', paper_bgcolor='transparent', autosize = F, width = 1400, height = 900, margin = list(l=-0.1, t = -0.1, r=-0.1))
    
    url_cur_desv_g_2 = paste0("/Img temp/", rut_sost,"_cur_desv_g_2.png")
    orca(k2, url_cur_desv_g_2)
    
    
    
    
    ########## GRAFICO BARRA INASISTENCIA GRAVE
    
    # data_plot3 <- data_plot3 %>% arrange(cod_curso)
    # data_plot3_t <- asis_crit %>% group_by(cod_curso) %>% summarise(n = n())
    # data_plot3  <- left_join(data_plot3, data_plot3_t, by="cod_curso")
    # data_plot3  <- left_join(data_plot3, data_plot3_n, by="cod_curso")
    # data_plot3  <- data_plot3 %>% mutate(n = ifelse(is.na(n), 0, n))
    # data_plot3  <- data_plot3 %>% mutate(n_total = ifelse(is.na(n_total), 0, n_total))
    # data_plot3  <- data_plot3 %>% mutate(porc_asiscrit = ifelse(n_total > 0, round(100*n/n_total,0), 0))
    # data_plot3 <- data_plot3 %>% droplevels()
    # #data_plot3  <- data_plot3 %>% mutate(porc_asiscrit = paste0(porc_asiscrit, "%"))
    # 
    # j <- plot_ly(x=~data_plot3$cod_curso, y = ~data_plot3$porc_asiscrit , type = "bar", marker = list(color = '#6bccd6'), text = data_plot3$n, texttemplate = '%{y}', textposition = 'outside') %>%
    #   layout(title="", xaxis=list(font = list(family = "verdana", color = '#007096', size = 20), title='<b>Grado 2023</b>'), yaxis=list(font = list(family = "verdana", color = '#007096', size = 17), title="<b>% Estudiantes</b>", autotick = T, ticksuffix = "%"), 
    #          font=list(family = "verdana", color = '#007096', size = 20), plot_bgcolor='transparent', paper_bgcolor='transparent', autosize = F, width = 1400, height = 900, margin = list(l=-0.1, t = -0.1, r=-0.1))
    # 
    # url_asis_crit_g = paste0("/Img temp/", rut_sost,"_asis_crit_g.png")
    # orca(j, url_asis_crit_g)
    
    ########## GRAFICO BARRA DESVINCULADOS POR Dependencia.
    
    data_plot4_t <- desvinc %>% group_by(rbd_2022r) %>% summarise(n = n())
    
    print(data_plot4)
    print(data_plot4_t)
    data_plot4  <- left_join(data_plot4, data_plot4_t, by=c("rbd_2022r"))
    data_plot4  <- data_plot4 %>% mutate(n = ifelse(is.na(n), 0, n))
    #data_plot4 <- data_plot4 %>% arrange(desc(n))
    #data_plot4  <- left_join(data_plot4, data_plot4_n, by="nom_rbd2022r")
    data_plot4  <- data_plot4 %>% mutate(n_total = ifelse(is.na(n_total), 0, n_total))
    data_plot4 <- data_plot4 %>% mutate(porc_desvinc = ifelse(n_total != 0, 100*round(n/n_total, 3), 0))
    data_plot4 <- data_plot4 %>% arrange(n)
    
    
    data_plot4$nom_rbd_2022r <- paste0(data_plot4$nom_rbd_2022r, " (RBD: ", data_plot4$rbd_2022r,")")
    #data_plot4$nom_rbd2022r <- factor(data_plot4$nom_rbd2022r, levels = data_plot4$nom_rbd2022r)
    
    #data_plot4$nom_rbd_2022r <- paste0(data_plot4$nom_rbd_2022r, " (RBD: ", data_plot4$rbd_2022r,")")
    # data_plot4$cod_depe2_2022r <- factor(data_plot4$cod_depe2_2022r, levels = c(1,2,3,4,5), labels = c("Municipal", "Particular Subvencionado", "Particular Pagado", "Administración Delegada", "Servicio Local de Educación"))
    
    
    m <- plot_ly() %>% add_trace(x = ~data_plot4$n, y = ~data_plot4$nom_rbd_2022r, marker = list(color = "#6bccd6"), type = "bar", orientation = "h", text = paste0("<b style='color:#007096'>",data_plot4$n,"</b>", "<i style='color:#007096';> (", data_plot4$porc_desvinc,"%)</i>"), textposition = "outside", textfont = list(family = "verdana", color = '#FFFFFF'))
    
    
    if(nrow(data_plot4) < 4){
      m <- m %>% layout(font = list(family = "verdana", color = '#007096', size = 18), title = "", bargap = 0.8,
                        xaxis = list(title="<b>N° estud. 2022' no matriculados en 2023</b> <i>(% de matrícula 2022)</i>", font = list(family = "verdana", size = 16), showlegend = FALSE, showgrid = FALSE, showticklabels = FALSE, side = "top", range = list(0, max(data_plot4$n)*1.22)),
                        yaxis = list(title="<b>Establecimiento</b>", font = list(family = "verdana", size = 18), showlegend = FALSE, categoryorder = "total ascending"),
                        plot_bgcolor='transparent', paper_bgcolor='transparent', autosize = F, width = 1300, height = 1100, margin = list(l=-0.1, t = -0.1, r=-0.1)
      )
    } else if(nrow(data_plot4) > 35 & nrow(data_plot4) < 66){
      m <- m %>% layout(font = list(family = "verdana", color = '#007096', size = 18), title = "", 
                        xaxis = list(title="<b>N° estud. 2022 no matriculados en 2023</b> <i>(% de matrícula 2022)</i>", font = list(family = "verdana", size = 14), showlegend = FALSE, showgrid = FALSE, showticklabels = FALSE, side = "top", range = list(0, max(data_plot4$n)*1.22)),
                        yaxis = list(title="<b>Establecimiento</b>", font = list(family = "verdana", size = 16), showlegend = FALSE, categoryorder = "total ascending"),
                        plot_bgcolor='transparent', paper_bgcolor='transparent', autosize = F, width = 1600, height = 2000, margin = list(l=-0.1, t = -0.1, r=-0.1)
      )  
    } else if(nrow(data_plot4) >= 66){
      m <- m %>% layout(font = list(family = "verdana", color = '#007096', size = 16), title = "", 
                        xaxis = list(title="<b>N° estud. 2022 no matriculados en 2023</b> <i>(% de matrícula 2022)</i>", font = list(family = "verdana", size = 14), showlegend = FALSE, showgrid = FALSE, showticklabels = FALSE, side = "top", range = list(0, max(data_plot4$n)*1.22)),
                        yaxis = list(title="<b>Establecimiento</b>", font = list(family = "verdana", size = 16), showlegend = FALSE, categoryorder = "total ascending"),
                        plot_bgcolor='transparent', paper_bgcolor='transparent', autosize = F, width = 1600, height = 1200, margin = list(l=-0.1, t = -0.1, r=-0.1)
      )  
    } else{
      m <- m %>% layout(font = list(family = "verdana", color = '#007096', size = 18), title = "", 
                        xaxis = list(title="<b>N° estud. 2022 no matriculados en 2023</b> <i>(% de matrícula 2022)</i>", font = list(family = "verdana", size = 16), showlegend = FALSE, showgrid = FALSE, showticklabels = FALSE, side = "top", range = list(0, max(data_plot4$n)*1.22)),
                        yaxis = list(title="<b>Establecimiento</b>", font = list(family = "verdana", size = 18), showlegend = FALSE, categoryorder = "total ascending"),
                        plot_bgcolor='transparent', paper_bgcolor='transparent', autosize = F, width = 1200, height = 1100, margin = list(l=-0.1, t = -0.1, r=-0.1)
      )  
    }
    
    url_sost_desv_g = paste0("/Img temp/", rut_sost,"_sost_desv_g.png")
    orca(m, url_sost_desv_g)
    
    
    ########## GRAFICO BARRA RETIRADOS POR DEPENDENCIA.
    
    data_plot4_t_2 <- desvinc2 %>% group_by(rbd_ret) %>% summarise(n = n())
    data_plot4_2
    data_plot4_2  <- left_join(data_plot4_2, data_plot4_t_2, by=c("rbd"="rbd_ret"))
    data_plot4_2
    data_plot4_2  <- data_plot4_2 %>% mutate(n = ifelse(is.na(n), 0, n))
    #data_plot4 <- data_plot4 %>% arrange(desc(n))
    #data_plot4  <- left_join(data_plot4, data_plot4_n, by="nom_rbd2022r")
    data_plot4_2 <- data_plot4_2 %>% mutate(n_total = ifelse(is.na(n_total), 0, n_total))
    data_plot4_2 <- data_plot4_2 %>% mutate(porc_desvinc = ifelse(n_total != 0, 100*round(n/n_total, 3), 0))
    data_plot4_2 <- data_plot4_2 %>% arrange(n)
    data_plot4_2
    
    data_plot4_2$nom_rbd <- paste0(data_plot4_2$nom_rbd, " (RBD: ", data_plot4_2$rbd,")")
    #data_plot4$nom_rbd2022r <- factor(data_plot4$nom_rbd2022r, levels = data_plot4$nom_rbd2022r)
    
    #data_plot4$nom_rbd_2022r <- paste0(data_plot4$nom_rbd_2022r, " (RBD: ", data_plot4$rbd_2022r,")")
    #data_plot4_2$cod_depe2 <- factor(data_plot4_2$cod_depe2, levels = c(1,2,3,4,5), labels = c("Municipal", "Particular Subvencionado", "Particular Pagado", "Administración Delegada", "Servicio Local de Educación"))
    
    
    m2 <- plot_ly() %>% add_trace(x = ~data_plot4_2$n, y = ~data_plot4_2$nom_rbd, marker = list(color = "#6bccd6"), type = "bar", orientation = "h", text = paste0("<b style='color:#007096'>",data_plot4_2$n,"</b>", "<i style='color:#007096';> (", data_plot4_2$porc_desvinc,"%)</i>"), textposition = "outside", textfont = list(family = "verdana", color = '#FFFFFF'))
    
    
    if(nrow(data_plot4_2) < 4){
      m2 <- m2 %>% layout(font = list(family = "verdana", color = '#007096', size = 18), title = "", bargap = 0.8,
                          xaxis = list(title="<b>N° estud. retirados en 2023 sin matrícula</b> <i>(% de matrícula 2023)</i>", font = list(family = "verdana", size = 16), showlegend = FALSE, showgrid = FALSE, showticklabels = FALSE, side = "top", range = list(0, max(data_plot4$n)*1.22)),
                          yaxis = list(title="<b>Establecimiento</b>", font = list(family = "verdana", size = 18), showlegend = FALSE, categoryorder = "total ascending"),
                          plot_bgcolor='transparent', paper_bgcolor='transparent', autosize = F, width = 1300, height = 1100, margin = list(l=-0.1, t = -0.1, r=-0.1)
      )
    } else if(nrow(data_plot4_2) > 35 & nrow(data_plot4_2) < 66){
      m2 <- m2 %>% layout(font = list(family = "verdana", color = '#007096', size = 18), title = "", 
                          xaxis = list(title="<b>N° estud. retirados en 2023 sin matrícula</b> <i>(% de matrícula 2023)</i>", font = list(family = "verdana", size = 14), showlegend = FALSE, showgrid = FALSE, showticklabels = FALSE, side = "top", range = list(0, max(data_plot4$n)*1.22)),
                          yaxis = list(title="<b>Establecimiento</b>", font = list(family = "verdana", size = 16), showlegend = FALSE, categoryorder = "total ascending"),
                          plot_bgcolor='transparent', paper_bgcolor='transparent', autosize = F, width = 1600, height = 2000, margin = list(l=-0.1, t = -0.1, r=-0.1)
      )  
    } else if(nrow(data_plot4_2) >= 66){
      m2 <- m2 %>% layout(font = list(family = "verdana", color = '#007096', size = 16), title = "", 
                          xaxis = list(title="<b>N° estud. retirados en 2023 sin matrícula</b> <i>(% de matrícula 2023)</i>", font = list(family = "verdana", size = 14), showlegend = FALSE, showgrid = FALSE, showticklabels = FALSE, side = "top", range = list(0, max(data_plot4$n)*1.22)),
                          yaxis = list(title="<b>Establecimiento</b>", font = list(family = "verdana", size = 16), showlegend = FALSE, categoryorder = "total ascending"),
                          plot_bgcolor='transparent', paper_bgcolor='transparent', autosize = F, width = 1600, height = 2000, margin = list(l=-0.1, t = -0.1, r=-0.1)
      )  
    } else{
      m2 <- m2 %>% layout(font = list(family = "verdana", color = '#007096', size = 18), title = "", 
                          xaxis = list(title="<b>N° estud. retirados en 2023 sin matrícula</b> <i>(% de matrícula 2023)</i>", font = list(family = "verdana", size = 16), showlegend = FALSE, showgrid = FALSE, showticklabels = FALSE, side = "top", range = list(0, max(data_plot4$n)*1.22)),
                          yaxis = list(title="<b>Establecimiento</b>", font = list(family = "verdana", size = 18), showlegend = FALSE, categoryorder = "total ascending"),
                          plot_bgcolor='transparent', paper_bgcolor='transparent', autosize = F, width = 1200, height = 1100, margin = list(l=-0.1, t = -0.1, r=-0.1)
      )  
    }
    
    url_sost_desv_g_2 = paste0("/Img temp/", rut_sost,"_sost_desv_g_2.png")
    orca(m2, url_sost_desv_g_2)
    
    ########## GRAFICO TORTA ESTUDIANTES POR EE DEL SOSTENEDOR.
    #####
    
    # o <- plot_ly(data_plot5, labels = ~tipo_asis_2, values = ~round(prop,3), type = 'pie', hole = 0.5, textposition = 'outside', 
    #              textinfo = 'percent', sort = FALSE, marker = list(colors = ~color)) %>% 
    #   layout(legend = list(font = list(family = "verdana", size = 14, color = '#004677'), x=1.3 , y=0.5), font=list(family = "verdana", size = 18, color = '#004677'), xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
    #          paper_bgcolor='transparent', autosize = TRUE, margin = list(l = -5, r = -5)
    #   )
    # 
    # url_sost_asist_g = paste0("/Img temp/", rut_sost,"_sost_asist_g.png")
    # orca(o, url_sost_asist_g)
    
    
    ########## GRAFICO BARRA ASIS CRIT POR EE DEL SOSTENEDOR.
    
    data_plot6_t <- asis_crit %>% group_by(rbd) %>% summarise(n = n()) %>% ungroup()
    data_plot6  <- left_join(data_plot6, data_plot6_t, by=c("rbd"))
    data_plot6  <- data_plot6 %>% mutate(n = ifelse(is.na(n), 0, n))
    
    data_plot6  <- data_plot6 %>% mutate(n_total = ifelse(is.na(n_total), 0, n_total))
    data_plot6 <- data_plot6 %>% mutate(porc_asiscrit = ifelse(n_total != 0, 100*round(n/n_total, 3), 0))
    data_plot6 <- data_plot6 %>% arrange(porc_asiscrit)
    
    #data_plot6$nom_rbd <- factor(data_plot6$nom_rbd, levels = data_plot6$nom_rbd)
    data_plot6$nom_rbd <- paste0(data_plot6$nom_rbd, " (RBD: ", data_plot6$rbd,")")
    #data_plot6$cod_depe2 <- factor(data_plot6$cod_depe2, levels = c(1,2,3,4,5), labels = c("Municipal", "Particular Subvencionado", "Particular Pagado", "Administración Delegada", "Servicio Local de Educación"))
    
    
    # q <- plot_ly() %>% add_trace(x = ~data_plot6$porc_asiscrit, y = ~data_plot6$nom_rbd,  marker = list(color = "#6bccd6"), type = "bar", orientation = "h", text = paste0("<b style='color:#007096'>", data_plot6$porc_asiscrit, "%","</b>", "<i style='color:#007096';> (", data_plot6$n," de ", data_plot6$n_total,")</i>"), textposition = "outside", textfont = list(family = "verdana", color = '#FFFFFF'))
    # 
    # if(nrow(data_plot6) < 4){  #Si son pocos EE se achican las barras
    #   q <- q %>% layout(font = list(family = "verdana", color = '#007096', size = 18), title = "", bargap = 0.8,
    #                     #xaxis2 = list(title="<b>% Estudiantes con asistencia grave 2023</b>", font = list(family = "verdana", size = 8, color = "#e94860"), side = "top", 
    #                     #             overlaying = "x", position=1,  range = list(0, max(data_plot6$porc_asiscrit) + 10), showgrid = FALSE, zeroline = FALSE, 
    #                     #            tickfont = list(color = "#e94860"), ticksuffix = "%", titlefont = list(color = "#e94860")),
    #                     xaxis = list(title="<b>% estud. con asistencia bajo 85%</b> <i> (N° estud. sobre matrícula 2023)</i>", font = list(family = "verdana", size = 16), range = list(0, max(data_plot6$porc_asiscrit) + 26), showlegend = FALSE, ticksuffix = "%", showgrid = FALSE, side = "top"),
    #                     yaxis = list(title="<b>Establecimiento</b>", font = list(family = "verdana", size = 18), showlegend = FALSE, categoryorder = "total ascending"),
    #                     plot_bgcolor='transparent', paper_bgcolor='transparent', autosize = F, width = 1300, height = 1100, margin = list(l=-0.1, t = -0.1, r=-0.1)
    #   )
    # } else if(nrow(data_plot6) > 35 & nrow(data_plot6) < 66){
    #   q <- q %>% layout(font = list(family = "verdana", color = '#007096', size = 18), title = "",
    #                     #xaxis2 = list(title="<b>% Estudiantes con asistencia grave 2023</b>", font = list(family = "verdana", size = 8, color = "#e94860"), side = "top", 
    #                     #             overlaying = "x", position=1,  range = list(0, max(data_plot6$porc_asiscrit) + 10), showgrid = FALSE, zeroline = FALSE, 
    #                     #            tickfont = list(color = "#e94860"), ticksuffix = "%", titlefont = list(color = "#e94860")),
    #                     xaxis = list(title="<b>% estud. con asistencia bajo 85%</b> <i> (N° estud. sobre matrícula 2023)</i>", font = list(family = "verdana", size = 16), range = list(0,  max(data_plot6$porc_asiscrit) + 26), showlegend = FALSE, ticksuffix = "%", showgrid = FALSE, side = "top"),
    #                     yaxis = list(title="<b>Establecimiento</b>", font = list(family = "verdana", size = 16), showlegend = FALSE, categoryorder = "total ascending"),
    #                     plot_bgcolor='transparent', paper_bgcolor='transparent', autosize = F, width = 1600, height = 2000, margin = list(l=-0.1, t = -0.1, r=-0.15)
    #   )
    #   } else if(nrow(data_plot6) >= 66){
    #     q <- q %>% layout(font = list(family = "verdana", color = '#007096', size = 16), title = "",
    #                       #xaxis2 = list(title="<b>% Estudiantes con asistencia grave 2023</b>", font = list(family = "verdana", size = 8, color = "#e94860"), side = "top", 
    #                       #             overlaying = "x", position=1,  range = list(0, max(data_plot6$porc_asiscrit) + 10), showgrid = FALSE, zeroline = FALSE, 
    #                       #            tickfont = list(color = "#e94860"), ticksuffix = "%", titlefont = list(color = "#e94860")),
    #                       xaxis = list(title="<b>% estud. con asistencia bajo 85%</b> <i> (N° estud. sobre matrícula 2023)</i>", font = list(family = "verdana", size = 14), range = list(0,  max(data_plot6$porc_asiscrit) + 26), showlegend = FALSE, ticksuffix = "%", showgrid = FALSE, side = "top"),
    #                       yaxis = list(title="<b>Establecimiento</b>", font = list(family = "verdana", size = 16), showlegend = FALSE, categoryorder = "total ascending"),
    #                       plot_bgcolor='transparent', paper_bgcolor='transparent', autosize = F, width = 1600, height = 2000, margin = list(l=-0.1, t = -0.1, r=-0.15)
    #     )
    #   } else{
    #   q <- q %>% layout(font = list(family = "verdana", color = '#007096', size = 18), title = "",
    #                     #xaxis2 = list(title="<b>% Estudiantes con asistencia grave 2023</b>", font = list(family = "verdana", size = 8, color = "#e94860"), side = "top", 
    #                     #             overlaying = "x", position=1,  range = list(0, max(data_plot6$porc_asiscrit) + 10), showgrid = FALSE, zeroline = FALSE, 
    #                     #            tickfont = list(color = "#e94860"), ticksuffix = "%", titlefont = list(color = "#e94860")),
    #                     xaxis = list(title="<b>% estud. con asistencia bajo 85%</b> <i> (N° estud. sobre matrícula 2023)</i>", font = list(family = "verdana", size = 16), range = list(0,  max(data_plot6$porc_asiscrit) + 26), showlegend = FALSE, ticksuffix = "%", showgrid = FALSE, side = "top"),
    #                     yaxis = list(title="<b>Establecimiento</b>", font = list(family = "verdana", size = 18), showlegend = FALSE, categoryorder = "total ascending"),
    #                     plot_bgcolor='transparent', paper_bgcolor='transparent', autosize = F, width = 1200, height = 1100, margin = list(l=-0.1, t = -0.1, r=-0.15)
    #   )
    # }
    # 
    # 
    # url_sost_asisgrave_g = paste0("/Img temp/", rut_sost,"_sost_asisgrave_g.png")
    # orca(q, url_sost_asisgrave_g)
    
    
    # #**************************************************************************************************************************/
    # ## 3.2 RENDER  ----------------------------------------------------------------
    # #**************************************************************************************************************************/
    #
    rmarkdown::render("RMKD Informe Sostenedor (PP) (EPJA).Rmd",
                      params = list("cod_deprov" = rut_sost, "nom_deprov" = nom_sost, "desvinc" = desvinc, "desvinc2" = desvinc2, "desvinc3" = desvinc3, "asis_crit" = asis_crit, "url_sin_fin_g" = url_sin_fin_g, "url_cur_desv_g" = url_cur_desv_g, "url_sost_desv_g" = url_sost_desv_g, "url_sost_desv_g_2" = url_sost_desv_g_2, "url_cur_desv_g_2" = url_cur_desv_g_2,
                                    "n_mat2022" = n_mat2022, "n_desvinc" = n_desvinc, "n_desvinc2" = n_desvinc2, "n_desvinc3" = n_desvinc3, "n_mat2023" = n_mat2023, "n_mat_asis2023" = n_mat_asis2023, "n_inasis2023" = n_inasis2023, "textos_variables_desvinculados" = textos_variables_desvinculados, "textos_variables_retirados" = textos_variables_retirados, "textos_variables_asistencia" = textos_variables_asistencia, "textos_variables_doble_desvinculados" = textos_variables_doble_desvinculados, "pass" = pass, "resumen_asis_sost" = resumen_asis_sost, "resumen_desvinc_sost" = resumen_desvinc_sost, "resumen_rbd" = resumen_rbd_ee),
                      output_file = paste0("Outputs/Sostenedores 0 (PP EPJA)/", rut_sost, "-EPJA.pdf"))
    print(paste0("Listo ", i ," de ", nn, " Sost. (Codigo: ", rut_sost, ")"))
    
    
    
    #**************************************************************************************************************************/
    ## 3.3 EXCEL DEPROV  ----------------------------------------------------------------
    #**************************************************************************************************************************/
    #*
    
    
    xsl_asis_ee_deprov <- xsl_asis_ee %>% filter(rut_sost_rbd2022 == rut_sost) %>% select(-c(rut_sost_rbd2022))
    xsl_asis_ee_cur_deprov <- xsl_asis_ee_cur %>% filter(rut_sost_rbd2022 == rut_sost) %>% select(-c(rut_sost_rbd2022))
    xls_desvinc_ee_deprov <- xls_desvinc_ee %>% filter(rut_sost_rbd2022 == rut_sost) %>% select(-c(rut_sost_rbd2022))
    xls_desvinc_ee_cur_deprov <- xls_desvinc_ee_cur %>% filter(rut_sost_rbd2022 == rut_sost) %>% select(-c(rut_sost_rbd2022))
    
    
    desvinc0 <- desvinc %>% select("rbd_2022r", "nom_rbd_2022r", "run_alu2", "nom_alu2022", "app_alu2022", "apm_alu2022", "nom_grado", "edad_alu_2022r", "gen_alu_2022r", "nom_com_alu2022", "sit_fin_r_2022r", "cert_val_22", "valid_estud")
    colnames(desvinc0) <- c("RBD", "Nombre Establecimiento", "RUN", "Nombre", "Apellido Parteno", "Apellido Materno", "Grado en 2022", "Edad 2022", "Sexo", "Comuna de residencia 2022", "Situación final 2022", "Validó estudios en 2022", "Inscrita/o para examen de validación 2023")
    
    desvinc2_0 <- desvinc2 %>% select("rbd_ret", "nom_rbd_ret", "run_alu2_ret", "nom_alu_ret", "app_alu_ret", "apm_alu_ret", "nom_grado_ret", "edad_alu_ret", "gen_alu_ret", "nom_com_alu_ret", "fec_ret_rbd_ret", "valid_estud")
    colnames(desvinc2_0) <- c("RBD", "Nombre Establecimiento", "RUN", "Nombre", "Apellido Parteno", "Apellido Materno","Grado en 2023", "Edad", "Sexo","Comuna de residencia 2023", "Fecha de retiro", "Inscrita/o para examen de validación 2023")
    
    desvinc3_0 <- desvinc3 %>% select("rbd", "nom_rbd", "run_alu2", "nom_alu", "app_alu", "apm_alu", "nom_grado", "edad_alu", "gen_alu", "nom_com_alu", "sit_fin_r", "cert_val_21", "cert_val_22", "insc_val_23")
    colnames(desvinc3_0) <- c("RBD", "Nombre Establecimiento", "RUN", "Nombre", "Apellido Parteno", "Apellido Materno", "Grado en 2021", "Edad 2021", "Sexo", "Comuna de residencia 2021", "Situación final 2021", "Validó estudios en 2021", "Validó estudios en 2022", "Inscrita/o para examen de validación 2023")
    
    wb <- createWorkbook()
    addWorksheet(wb, sheetName = "Nómina estud. retirados 2023")
    addWorksheet(wb, sheetName = "Nómina estud. no matric 22-23")
    addWorksheet(wb, sheetName = "Nómina estud. no matric 21-23")
    addWorksheet(wb, sheetName = "Resumen no matric x RBD")
    addWorksheet(wb, sheetName = "Resumen no matric x RBD y grado")
    # addWorksheet(wb, sheetName = "Resumen asist x RBD")
    # addWorksheet(wb, sheetName = "Resumen asist x RBD y grado")
    
    writeData(wb, "Nómina estud. retirados 2023", desvinc2_0) 
    writeData(wb, "Nómina estud. no matric 22-23", desvinc0)
    writeData(wb, "Nómina estud. no matric 21-23", desvinc3_0)
    writeData(wb, "Resumen no matric x RBD", xls_desvinc_ee_deprov)
    writeData(wb, "Resumen no matric x RBD y grado", xls_desvinc_ee_cur_deprov)
    # writeData(wb, "Resumen asist x RBD", xsl_asis_ee_deprov)
    # writeData(wb, "Resumen asist x RBD y grado", xsl_asis_ee_cur_deprov)
    
    pct = createStyle(numFmt="0.0%")
    addStyle(wb, "Resumen no matric x RBD", style=pct, cols=c(7), rows=2:(nrow(xls_desvinc_ee_deprov)+1), gridExpand=TRUE)
    # addStyle(wb, "Resumen no matric x RBD y grado", style=pct, cols=c(8), rows=2:(nrow(xls_desvinc_ee_cur_deprov)+1), gridExpand=TRUE)
    #addStyle(wb, "Resumen asist x RBD", style=pct, cols=c(5:6, 9, 15:19), rows=2:(nrow(xsl_asis_ee_deprov)+1), gridExpand=TRUE)
    #addStyle(wb, "Resumen asist x RBD y grado", style=pct, cols=c(6:7, 10, 16:20), rows=2:(nrow(xsl_asis_ee_cur_deprov)+1), gridExpand=TRUE)
    
    saveWorkbook(wb, paste0("Outputs/Sostenedores 0 (PP EPJA)/", rut_sost, ".xlsx"), overwrite = T)
    
    toc()
  }) #cierre Try
}



