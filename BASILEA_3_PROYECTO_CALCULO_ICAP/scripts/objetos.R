###################################
#Cargamos las siguientes librerias#
###################################
library(shiny)
library(bslib)
library(tidyverse)
library(magrittr)
library(readxl)
library(dplyr)
library(writexl)
library(scales)
library(kableExtra)
library(ggplot2)
########################
#Para Capital Neto
##########################

#Cargamos un catalogo
catalogo<-read_xlsx(path = "data/Catalogo.xlsx")

#Cargamos para calcular el capital neto (solo si no se va usar la app)
#df_1<-read_xlsx(path="data/CAPITAL NETO.xlsx", sheet="datos", range = "C1:D69")

#################################
#Para calcular Riesgo de Credito#
#################################

# Cargamos para Calcular Riesdo d Credito (solo si no va usar la app)
#df_2 <- read_xlsx(
#  "data/Marcial Martínez Alcántara - TAREA EXAMEN RIESGO DE CRÉDITO.xlsx",
#  sheet = 2
#)

# asignacion por grupo de riesgo
Asignacion_Grupo_d_riesgo<-read_xlsx(path = "data/Asignar_Grupo_Riesgo.xlsx")

#Calificaciones
calificaciones<-read_xlsx(path = "data/calificaciones.xlsx")

#Ponderador segun calificacion
Ponderador_segun_cal<-read_xlsx(path = "data/Ponderador_segun_calificacion.xlsx")


