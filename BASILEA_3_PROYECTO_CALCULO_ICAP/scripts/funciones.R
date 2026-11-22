#######################################
# funcion para calcular el capital neto, este regresa 2 graficas y una tabla
#######################################
Capital_Neto<-function(df, ASRT, ASRC_metodo_interno, ASRC_metodo_estandar){
  
  # si la segunda columna tiene otro nombre la nombra como importe1
  colnames(df)[2]<-"importe1"
  ########################
  # Para limpiar los datos
  ########################
  df <- df %>%
    distinct(concepto, .keep_all = TRUE) #revisamos que solo haya un concepto de cada uno
  df <- df %>%                   #redondeamos si el concepto tiene decimal
    mutate(
      importe1 = if_else(
        concepto == 95398,
        sign(importe1) * ceiling(abs(importe1)),
        importe1
      )
    )
  df_2<-df%>%mutate(concepto=as.character(concepto))   #hacemos la plantilla
  df_2<-catalogo %>%
    left_join(df_2, by = "concepto")
  
  ##############
  ### para sumar
  ##############
  
  Suma <- function(df, clase,tipo) { 
    a<-tipo
    if(a==1){
      v <- catalogo %>% 
        filter(Clase == clase) %>% 
        pull(1) %>% 
        as.numeric()
    }else if(a==2){
      x<-catalogo %>% filter(Clase == clase)%>%select(1)%>%pull()
      v <- as.numeric(strsplit(x, ",\\s*")[[1]])
    }
    df %>% 
      filter(concepto %in% v) %>%distinct(concepto, .keep_all = TRUE)%>% 
      pull(2) %>%
      sum()
  }
  
  #################
  ### para restar
  #################
  
  Resta<-function(df,clase){
    x<-catalogo %>% filter(Clase == clase)%>%select(1)%>%pull()
    v <- as.numeric(strsplit(x, ",\\s*")[[1]])
    v<-df %>% filter(concepto %in% v)%>%pull(2)
    y<-v[1]-v[2]
    if(is.na(y)|y==0){
      return(0)
    }else{
      return(y)
    }
  }
  
  #################################
  # Parte Fundamental / el resultado de cada clase
  #################################
  
  I<-Suma(df,"I",1)
  II<-Suma(df,"II",1)
  III<-Suma(df,"III",1)
  
  IV.1<-Suma(df,"IV.1",1)
  IV.2<-Suma(df, "IV.2",1)
  IV.3<-Suma(df,"IV.3",1)
  IV<-IV.1+IV.2+IV.3
  
  V.1<-Suma(df,"V.1",2)
  V.2<-Suma(df,"V.2",2)
  V.3<-max(0,Resta(df,"V.3"))
  V<-V.1+V.2+V.3
  
  VI<-Suma(df,"VI",1)
  
  VII.1<-Suma(df,"VII.1",1)
  df <- df %>%add_row(concepto = 95320,  
                      importe1 = VII.1)
  VII.2<-Suma(df,"VII.2",1)
  df <- df %>%add_row(concepto = 95345,     
                      importe1 = VII.2)
  VII<-Suma(df,"VII",1)
  
  VIII.1<-Suma(df,"VIII.1",1)
  q1<-max(0,Resta(df,"VIII.2"))
  q2<-max(0,0.1*(I+II-III-IV-V-VI-VII-VIII.1))
  VIII.2<-max(0,q1-q2)
  VIII<-VIII.1+VIII.2
  
  IX<-Suma(df,"IX",1)
  
  s.1<-Suma(df,"X",1)
  s.2<-max(0,0.25*(I+II-III-IV-V-VI-VII-VIII.1-VIII.2-IX))
  X<-max(0,s.1-s.2)
  
  XI<-Suma(df,"XI",1)
  
  Parte_Fundamental<-I+II-III-IV-V-VI-VII-VIII-IX-X-XI
  
  ###########################
  #Parte No Fundamental
  ###########################
  
  Coef_Capital_Fundamental<-Parte_Fundamental/ASRT
  XII<-Suma(df,"XII",1)
  b.1<-Suma(df,"XIII.2",1)
  b.2<-if_else(Coef_Capital_Fundamental>=0.1,b.1,0.5*Parte_Fundamental)
  XIII<-max(0,min(b.1,b.2))
  XIV<-Suma(df,"XIV",1)
  Parte_No_Fundamental<-XII+XIII+XIV
  
  Capital_Basico<-Parte_Fundamental+Parte_No_Fundamental
  
  #####################
  # Parte Complementaria
  ####################
  XV<-Suma(df,"XV",1)
  
  XVI.1<-Suma(df,"XVI.2",1)
  XVI.2<-if_else(Coef_Capital_Fundamental>=0.1,XVI.1,0.5*(Coef_Capital_Fundamental-b.1))
  XVI<-max(0,min(XVI.1,XVI.2)) 
  
  XVII<-Suma(df,"XVII",1)
  
  XVIII.a1<-Suma(df,"XVIII.2",1)
  XVIII.a2<-Suma(df,"XVIII.3",1)
  XVIII.a3<-0.006*ASRC_metodo_interno
  XVIII.1<-min(max(0,(XVIII.a1-XVIII.a2)), XVIII.a3)
  
  
  XVIII.b1<-Suma(df,"XVIII.5",1)
  XVIII.b2<-Suma(df,"XVIII.6",1)
  XVIII.b3<-0.0125*ASRC_metodo_estandar
  XVIII.4<-min(max(0,(XVIII.b1-XVIII.b2)), XVIII.b3)
  
  XVIII<-XVIII.1+XVIII.4
  
  Parte_Complementaria<-XV+XVI+XVII+XVIII
  
  Capital_Neto<-Parte_Fundamental+Parte_No_Fundamental+Parte_Complementaria
  
  #####################
  #para el excel, agregamos los resultados a la plantilla que es df_2
  #######################
  
  df_2 <- df_2 %>%
    add_row(
      importe1=I,
      concepto = "resultado.I",
      .after = 3    
    )
  df_2 <- df_2 %>%
    add_row(
      importe1=II,
      concepto = "resultado.II",
      .after = 10    
    )
  df_2 <- df_2 %>%
    add_row(
      importe1=III,
      concepto = "resultado.III",
      .after = 14   
    )
  df_2 <- df_2 %>%
    add_row(
      concepto = "resultado.IV",
      importe1 = IV,
      .after = 31      
    )
  df_2 <- df_2 %>%
    add_row(
      concepto = "resultado.V",
      importe1 = V,
      .after = 37      
    )
  df_2 <- df_2 %>%
    add_row(
      concepto = "resultado.VI",
      importe1 = VI,
      .after = 41      
    )
  df_2 <- df_2 %>%
    add_row(
      concepto = "resultado.VII",
      importe1 = VII,
      .after = 49      
    )
  df_2 <- df_2 %>%
    add_row(
      concepto = "resultado.VIII",
      importe1 = VIII,
      .after = 56      
    )
  df_2 <- df_2 %>%
    add_row(
      concepto = "resultado.IX",
      importe1 = IX,
      .after = 59     
    )
  df_2 <- df_2 %>%
    add_row(
      concepto = "resultado.X",
      importe1 = X,
      .after = 61     
    )
  df_2 <- df_2 %>%
    add_row(
      concepto = "resultado.XI",
      importe1 = XI,
      .after = 65)
  df_2 <- df_2 %>%
    add_row(
      concepto = "Calculo.Parte.Fundamental",
      importe1 = Parte_Fundamental,
      .after = 66)
  df_2<-df_2%>%
    add_row(
      concepto = "resultado.XII",
      importe1 = XII,
      .after = 69)
  df_2<-df_2%>%
    add_row(
      concepto = "resultado.XIII",
      importe1 = XIII,
      .after = 73)
  df_2<-df_2%>%
    add_row(
      concepto = "resultado.XIV",
      importe1 = XIV,
      .after = 75)
  df_2<-df_2%>%
    add_row(
      concepto = "Calculo.Parte.No.Fundamental",
      importe1 = Parte_No_Fundamental,
      .after = 76)
  df_2<-df_2%>%
    add_row(
      concepto = "resultado.XV",
      importe1 = XV,
      .after = 78)
  df_2<-df_2%>%
    add_row(
      concepto = "resultado.XVI",
      importe1 = XVI,
      .after = 83)
  df_2<-df_2%>%
    add_row(
      concepto = "resultado.XVII",
      importe1 = XVII,
      .after = 85)
  df_2<-df_2%>%
    add_row(
      concepto = "resultado.XVIII",
      importe1 = XVIII)
  df_2<-df_2%>%
    add_row(
      concepto = "Calculo.Parte.Complementaria",
      importe1 = Parte_Complementaria)
  df_2<-df_2%>%
    add_row(
      concepto = "capital complementario",
      importe1 = Parte_Complementaria,
      .before = 1)
  df_2<-df_2%>%
    add_row(
      concepto = "basica no fundamental",
      importe1 = Parte_No_Fundamental,
      .before = 1)
  df_2<-df_2%>%
    add_row(
      concepto = "fundamental",
      importe1 = Parte_Fundamental,
      .before = 1)
  df_2<-df_2%>%
    add_row(
      concepto = "capital basico",
      importe1 = Capital_Basico,
      .before = 1)
  df_2<-df_2%>%
    add_row(
      concepto = "capital neto",
      importe1 = Capital_Neto,
      .before = 1)
  ##################
  # cambiamos algunos valores que se calcularon
  ##################
  df_2 <- df_2 %>%
    mutate(
      importe1 = if_else(row_number() == 38, V.1, importe1)
    )
  df_2 <- df_2 %>%
    mutate(
      importe1 = if_else(row_number() == 39, V.2, importe1)
    )
  df_2 <- df_2 %>%
    mutate(
      importe1 = if_else(row_number() == 40, V.3, importe1)
    )
  df_2 <- df_2 %>%
    mutate(
      importe1 = if_else(row_number() == 59, VIII.2, importe1)
    )
  df_2 <- df_2 %>%
    mutate(
      importe1 = if_else(row_number() == 76, XIII, importe1)
    )
  df_2 <- df_2 %>%
    mutate(
      importe1 = if_else(row_number() == 78, b.2, importe1)
    )
  df_2 <- df_2 %>%
    mutate(
      importe1 = if_else(row_number() == 86, XVI, importe1)
    )
  df_2 <- df_2 %>%
    mutate(
      importe1 = if_else(row_number() == 88, XVI.2, importe1)
    )
  df_2 <- df_2 %>%
    mutate(
      importe1 = if_else(row_number() == 92, XVIII.1, importe1)
    )
  df_2 <- df_2 %>%
    mutate(
      importe1 = if_else(row_number() == 95, XVIII.a3, importe1)
    )
  
  df_2 <- df_2 %>%
    mutate(
      importe1 = if_else(row_number() == 96, XVIII.4, importe1)
    )
  df_2 <- df_2 %>%
    mutate(
      importe1 = if_else(row_number() == 99, XVIII.b3, importe1)
    )
  
  ###############
  # calculamos algunos Porcentajes
  #################
  
  Coef_Capital_Basico<-Capital_Basico/ASRT
  
  Indice_d_Capitalizacion<-Capital_Neto/ASRT
  
  ################
  df_2<-df_2%>%
    add_row(
      concepto = "coeficiente de capital basico %",
      importe1 = Coef_Capital_Basico*100,
      .before = 1)
  df_2<-df_2%>%
    add_row(
      concepto = "coeficiente de capital fundamental %",
      importe1 = Coef_Capital_Fundamental*100,
      .before = 1)
  df_2<-df_2%>%
    add_row(
      concepto = "indice de capitalizacion %",
      importe1 = Indice_d_Capitalizacion*100,
      .before = 1)
  
  ############
  # Para las graficas
  ############
  Coef_Capital_Complementario <- 
    Indice_d_Capitalizacion - Coef_Capital_Basico
  
  Coef_Capital_No_Fundamental <- Coef_Capital_Basico - Coef_Capital_Fundamental
  ##############
  df_plot <- tibble(
    Componente = c("Coef Capital Básico", "Coef Capital Complementario"),
    Valor = c(
      Coef_Capital_Basico / Indice_d_Capitalizacion * 100,
      Coef_Capital_Complementario / Indice_d_Capitalizacion * 100
    )
  )
  ######################
  # creamos la primer grafica
  ########################
  gf1<-ggplot(df_plot, aes(x = "Índice de Capitalización", y = Valor, fill = Componente)) +
    geom_col(width = 0.6) +
    geom_text(
      aes(label = paste0(round(Valor, 1), "%")),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 4
    ) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    theme_minimal()
  ###########
  df_plot <- tibble(
    Componente = c(
      "Coef Capital Fundamental",
      "Coef Capital No Fundamental",
      "Coef Capital Complementario"
    ),
    Valor = c(
      Coef_Capital_Fundamental,
      Coef_Capital_No_Fundamental,
      Coef_Capital_Complementario
    ),
    Porcentaje = Valor / Indice_d_Capitalizacion * 100
  )
  
  
  df_plot<-df_plot %>%
    mutate(
      Componente = factor(
        Componente,
        levels = c(
          "Coef Capital Fundamental",
          "Coef Capital No Fundamental",
          "Coef Capital Complementario"
        )
      )
    )
  
  ############3
  #creamos la segunda grafica
  #############
  gf2 <- ggplot(df_plot, aes(x = "Índice de Capitalización", y = Porcentaje, fill = Componente)) +
    geom_col(width = 0.6) +
    geom_text(
      aes(label = paste0(round(Porcentaje, 1), "%")),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 4
    ) +
    scale_y_continuous(
      labels = percent_format(scale = 1),
      limits = c(0, 100)
    ) +
    labs(
      x = NULL,
      y = "Porcentaje (%)",
      fill = "",
      title = "Composición del Índice de Capitalización"
    ) +
    theme_minimal()
  
  #########
  # para imprimir el excel
  ##########
  colnames(df_2)[6]<-"importe ó %"
  
 
  
  df_2 <- df_2 %>%
    mutate(across(everything(), ~ ifelse(is.na(.), " ", .)))
  df_2<-df_2%>%mutate(`importe ó %`=as.numeric(`importe ó %`))
  df_2<-df_2%>%mutate(`importe ó %`=round(`importe ó %`,2))
  
  # creamos el cuadro
  
  x <- df_2%>%slice(1:8) %>%
    kable( digits = 2,
      format   = "html",
      booktabs = TRUE,
      caption  = "Capital Neto (Fuente: Capital_Neto.xlsx)",
      align    = "l"
    ) %>%
    kable_styling(
      full_width = FALSE,
      position   = "center",
      bootstrap_options = c("striped", "hover", "condensed", "responsive")
    )
  #####
  #regresa 2 graficas y un cuadro
  ######
  
  message("se guardo un archivo excel llamado excel_capital_neto")
  
  list(gf1 = gf1, gf2 = gf2, tabla_capital_neto = x, excel=write_xlsx(df_2,"excel_capital_neto.xlsx"))
  

}
#
#
#
#
#
################################################################################################3
# funcion para hacer el calculo de riesgo de credito, regresa 2 tablas
###################################################################################################
Riesgo_d_Credito<-function(df){
  ##########################################
  #Pre-procesamiento para Riesgo de Credito#
  ##########################################
  df <- df %>% select(1:27)
  colnames(df)[9]<-"Grupo de Riesgo"
  df <- df %>%
    mutate(
      `Grupo de Riesgo` = as.character(`Grupo de Riesgo`),
      `Nomenclatura de la calificación` = as.character(`Nomenclatura de la calificación`)
    ) %>%
    mutate(
      across(
        c(3, 5, 7, 11, 12, 14, 15, 16, 17:27),
        as.numeric
      )
    )
  
  #################################
  #Asignamos el grupo de riesgo
  ################################
  
  df <- df %>%
    left_join(
      Asignacion_Grupo_d_riesgo %>%
        select(`Tipo de Cartera`, `Actividad Económica`, `Clave del tamaño de la empresa`, `Grupo de Riesgo`),
      by = c(
        "Tipo de Cartera",
        "Actividad Económica",
        "Clave del tamaño de la empresa"
      )
    ) %>%
    mutate(
      `Grupo de Riesgo` = coalesce(`Grupo de Riesgo.y`, `Grupo de Riesgo.x`)
    ) %>%
    select(-`Grupo de Riesgo.x`, -`Grupo de Riesgo.y`)
  ##################################################
  #asignamos la nomenclatura de la calificacion
  ##################################################
  df <- df %>%
    left_join(
      calificaciones %>%
        select(Clave, Calificación),
      by = c("Clave Calificación" = "Clave")
    ) %>%
    mutate(
      `Nomenclatura de la calificación` =
        coalesce(Calificación, `Nomenclatura de la calificación`)
    ) %>%
    select(-Calificación)
  
  
  ########################################################
  #asignamos el ponderador de riesgo según la calificación
  ########################################################
  
  df <- df %>%
    left_join(
      calificaciones %>% 
        select(Clave, Ponderador),
      by = c("Clave Calificación" = "Clave")
    ) %>%
    mutate(
      `Ponderador de Riesgo` = coalesce(Ponderador, `Ponderador de Riesgo`)
    ) %>%
    select(-Ponderador)
  #############
  #Grupo I-A
  #############
  df <- df %>%
    mutate(
      `Ponderador de Riesgo` = if_else(
        `Grupo de Riesgo` == "I-A",
        0,
        `Ponderador de Riesgo`
      )
    )
  
  ###############
  #Grupo III
  ###############
  
  df <- df %>%
    left_join(
      Ponderador_segun_cal %>%
        select(Clave, Ponderador_para_grupo_III_segun_calificacion),
      by = c("Clave Calificación" = "Clave")
    ) %>%
    mutate(
      `Ponderador de Riesgo` = case_when(
        `Grupo de Riesgo` == "III" &
          !is.na(Ponderador_para_grupo_III_segun_calificacion) ~
          Ponderador_para_grupo_III_segun_calificacion,
        
        TRUE ~ `Ponderador de Riesgo`
      )
    ) %>%
    select(-Ponderador_para_grupo_III_segun_calificacion)
  
  df <- df %>%
    mutate(
      `Ponderador de Riesgo` = case_when(
        `Grupo de Riesgo` == "III" &
          (is.na(`Clave Calificación`) | `Clave Calificación` == 0) ~ 1,
        
        TRUE ~ `Ponderador de Riesgo`
      )
    )
  
  ##########
  # Grupo V
  ##########
  df <- df %>%
    left_join(
      Ponderador_segun_cal %>%
        select(Clave, Ponderador_Grupo_V_segun_calificacion),
      by = c("Clave Calificación" = "Clave")
    ) %>%
    mutate(
      `Ponderador de Riesgo` = case_when(
        `Grupo de Riesgo` == "V" &
          !is.na(Ponderador_Grupo_V_segun_calificacion) ~
          Ponderador_Grupo_V_segun_calificacion,
        
        TRUE ~ `Ponderador de Riesgo`
      )
    ) %>%
    select(-Ponderador_Grupo_V_segun_calificacion)
  
  df <- df %>%
    mutate(
      `Ponderador de Riesgo` = case_when(
        `Grupo de Riesgo` == "V" &
          (is.na(`Clave Calificación`) | `Clave Calificación` == 0) ~ 1.5,
        
        TRUE ~ `Ponderador de Riesgo`
      )
    )
  ####################
  ### Ponderador VI 
  #######################
  
  df <- df %>%
    mutate(
      `Ponderador de Riesgo` = case_when(
        # Caso especial: VI, tamaño 1–3 y calificación NA o 0
        `Grupo de Riesgo` == "VI" &
          `Clave del tamaño de la empresa` %in% c(1, 2, 3) &
          (is.na(`Clave Calificación`) | `Clave Calificación` == 0) ~ 0.85,
        
        # Caso VI, tamaño 1–3 y calificación distinta de NA y 0
        `Grupo de Riesgo` == "VI" &
          `Clave del tamaño de la empresa` %in% c(1, 2, 3) &
          !is.na(`Clave Calificación`) &
          `Clave Calificación` != 0 ~ 0.75,
        
        # CUALQUIER OTRO CASO DEL GRUPO VI
        `Grupo de Riesgo` == "VI" ~ 0.75,
        
        # para que todo lo dems queda igual
        TRUE ~ `Ponderador de Riesgo`
      )
    )
  
  #################
  ### grupo VII-A
  ###################
  
  df <- df %>%
    left_join(
      Ponderador_segun_cal %>%
        select(Clave, Ponderador_para_grupo_VII_A_segun_calificacion),
      by = c("Clave Calificación" = "Clave")
    ) %>%
    mutate(
      `Ponderador de Riesgo` = case_when(
        `Grupo de Riesgo` == "VII-A" &
          !is.na(Ponderador_para_grupo_VII_A_segun_calificacion) ~
          Ponderador_para_grupo_VII_A_segun_calificacion,
        
        TRUE ~ `Ponderador de Riesgo`
      )
    ) %>%
    select(-Ponderador_para_grupo_VII_A_segun_calificacion)
  
  df <- df %>%
    mutate(
      `Ponderador de Riesgo` = case_when(
        `Grupo de Riesgo` == "VII-A" &
          (is.na(`Clave Calificación`) | `Clave Calificación` == 0) ~ 1,
        
        TRUE ~ `Ponderador de Riesgo`
      )
    )
  
  ##########################################
  ### factor de conversion por tipo de linea
  ############################################
  
  df <- df %>%
    mutate(
      `Factor de Conversión de la Línea` = case_when(
        `Tipo de línea` == 1   ~ 0,
        `Tipo de línea` == 2   ~ 1,
        `Tipo de línea` == 181 ~ 0.5,
        `Tipo de línea` == 3   ~ 0,
        `Tipo de línea` == 4   ~ 1,
        `Tipo de línea` == 5   ~ 1.25,
        TRUE ~ `Factor de Conversión de la Línea` 
      )
    )
  
  ###############################################
  ### factor de conversion por tipo de relacion
  ###############################################
  
  df <- df %>%
    mutate(
      `Factor de conversión por tipo de relación` = case_when(
        `Tipo de línea` %in% c(1, 2, 181) ~ 1,
        `Tipo de línea` %in% c(3, 4, 5)   ~ 1.15,
        TRUE ~ `Factor de conversión por tipo de relación`  
      )
    )
  
  ###########################
  ### calculo final, recolocamos grupo de riesgo y eliminamos una columna 
  ###########################
  df <- df %>%
    relocate(`Grupo de Riesgo`, .before = 9)%>% select(-`Justificar si considera necesario`)
  
  ########################################################
  #convertimos a 0 aquellos valores que pudieran tener NA
  ########################################################
  df <- df %>%
    mutate(
      across(
        c(
          `Monto no dispuesto (remanente de la línea de crédito que aún podría ejercer)`,
          `Monto dispuesto por el acreditado`,
          dat_rvas_totales_estandar,
          `Valor de la Garantía`,
          dat_expos_incump_total
        ),
        ~ if_else(is.na(.x), 0, .x)
      )
    )
  
  ########################################################
  # Exposicion al incumplimiento de la parte no dispuesta
  ######################################################
  
  df <- df %>%
    mutate(
      `Exposicion al incumplimiento de la  parte no dispuesta` =
        `Factor de Conversión de la Línea` *
        `Monto no dispuesto (remanente de la línea de crédito que aún podría ejercer)`
    )
  
  #####################################################
  #Activo Ponderado por Riesgo de la parte no dispuesta
  #####################################################
  
  df <- df %>%
    mutate(
      `Activo Ponderado por Riesgo de la parte no dispuesta` =
        `Exposicion al incumplimiento de la  parte no dispuesta` *
        `Ponderador de Riesgo` *
        `Factor de Conversión de la Línea`/100
    )
  
  
  ###################################################
  #Exposicion al incumplimiento de la parte dispuesta
  ###################################################
  
  df <- df %>%
    mutate(
      `Exposición al incumplimiento de la parte dispuest` =
        `Monto dispuesto por el acreditado` -
        dat_rvas_totales_estandar -
        `Valor de la Garantía`
    )
  
  #######################################################
  #Activos Convertidos y Ponderados de la parte dispuesta
  #######################################################
  df <- df %>%
    mutate(
      `Activos Convertidos y Ponderados de la parte dispuesta` =
        (`Exposición al incumplimiento de la parte dispuest` *
           `Factor de conversión por tipo de relación` *
           `Ponderador de Riesgo`)/100
    )
  ######################################################
  #Total activos ponderados
  ######################################################
  df <- df %>%
    mutate(
      `Total activos ponderados` =
        `Activos Convertidos y Ponderados de la parte dispuesta` +
        `Activo Ponderado por Riesgo de la parte no dispuesta`
    )
  ##########################
  #cambiaremos nombres
  ##########################
  colnames(df)[19]<-"APSRT de la parte no dispuesta"
  colnames(df)[24]<-"Exposición al incumplimiento de la parte dispuesta"
  colnames(df)[25]<-"APSRT de la parte dispuesta"
  #########################
  #hacemos el cruadro CREDITOS_DISPUESTOS
  ########################
  CREDITOS_DISPUESTOS<-df %>%
    mutate(
      saldo_insoluto =
        `Exposición al incumplimiento de la parte dispuesta` +
        dat_rvas_totales_estandar +
        `Valor de la Garantía`
    ) %>%
    group_by(
      `Grupo de Riesgo`,
      `Ponderador de Riesgo`,
      `Factor de Conversión de la Línea`
    ) %>%
    summarise(
      saldo_insoluto = sum(saldo_insoluto, na.rm = TRUE),
      reservas       = sum(dat_rvas_totales_estandar, na.rm = TRUE),
      garantias      = sum(`Valor de la Garantía`, na.rm = TRUE),
      
      exposicion_al_incumplimiento = sum(
        `Exposición al incumplimiento de la parte dispuesta`,
        na.rm = TRUE
      ),
      
      activos_sujetos_a_riesgo = sum(`Total activos ponderados`,
                                     na.rm = TRUE
      ),
      
      .groups = "drop"
    )
  # cambiamos decimales en valor de porcenaje %
  CREDITOS_DISPUESTOS<-CREDITOS_DISPUESTOS%>%mutate(`Ponderador de Riesgo`=`Ponderador de Riesgo`*100,
                               `Factor de Conversión de la Línea`=`Factor de Conversión de la Línea`*100)
  # cambiamos algunos nombres para que se vea mejor
  colnames(CREDITOS_DISPUESTOS)[2]<-"Ponderador de Riesgo %"
  colnames(CREDITOS_DISPUESTOS)[3]<-"Factor de conversión de la línea %"
  colnames(CREDITOS_DISPUESTOS)[4]<-"Saldo insoluto"
  colnames(CREDITOS_DISPUESTOS)[7]<-"Exposición al incumplimiento"
  colnames(CREDITOS_DISPUESTOS)[8]<-"Activos Sujetos a Riesgo"
#mejorando con kable
  CREDITOS_DISPUESTOS_2<-CREDITOS_DISPUESTOS %>%
    kable(
      format = "html",
      booktabs = TRUE,
      caption = "Créditos dispuestos",
      align = "l"
    ) %>%
    kable_styling(
      full_width = FALSE,
      position = "center",
      bootstrap_options = c("striped", "hover", "condensed", "responsive")
    ) %>%
    # Encabezado agrupado (solo visual)
    add_header_above(c(" " = 3, "Montos" = ncol(CREDITOS_DISPUESTOS) - 3)) %>%
    # Resalta primeras columnas (claves)
    column_spec(1, bold = TRUE) %>%
    column_spec(2, width = "8em") %>%
    column_spec(3, width = "10em") %>%
    # Scroll si es larga
    scroll_box(width = "100%", height = "450px")
  ################################
  #hacemos el cuadro LINEAS DE CREDITO
  ##############################
  LINEAS_DE_CREDITO<-df %>%
    mutate(
      Tipo = case_when(
        `Tipo de línea` %in% c(1, 3) ~ "Revocable",
        TRUE ~ "Irrevocable"
      ),
      ponderador_de_riesgo = as.numeric(`Ponderador de Riesgo`),
      factor_de_conversion = as.numeric(`Factor de Conversión de la Línea`)
    ) %>%
    group_by(Tipo, ponderador_de_riesgo, factor_de_conversion) %>%
    summarise(
      monto_no_dispuesto = sum(
        `Monto no dispuesto (remanente de la línea de crédito que aún podría ejercer)`,
        na.rm = TRUE
      ),
      Total_activos_ponderados = sum(`Total activos ponderados`, na.rm = TRUE),
      .groups = "drop"
    )
  # cambiamos el valor decimal a porcetaje %
  LINEAS_DE_CREDITO<-LINEAS_DE_CREDITO%>%mutate(ponderador_de_riesgo=ponderador_de_riesgo*100,
                             factor_de_conversion=factor_de_conversion*100)
  # cambiamos algunos nombres para que se vea mejor
  colnames(LINEAS_DE_CREDITO)[2]<-"Ponderador de riesgo %"
  colnames(LINEAS_DE_CREDITO)[3]<-"Factor de conversión %"
  colnames(LINEAS_DE_CREDITO)[4]<-"Monto no dispuesto"
  colnames(LINEAS_DE_CREDITO)[5]<-"Total Activos Ponderados"
  
  
  #mejoramos con kable
  LINEAS_DE_CREDITO_2<-LINEAS_DE_CREDITO %>%
    kable(
      format = "html",
      booktabs = TRUE,
      caption = "Líneas de crédito (no dispuestas)",
      align = "l"
    ) %>%
    kable_styling(
      full_width = FALSE,
      position = "center",
      bootstrap_options = c("striped", "hover", "condensed", "responsive")
    ) %>%
    add_header_above(c(" " = 3, "Montos" = ncol(LINEAS_DE_CREDITO) - 3)) %>%
    column_spec(1, bold = TRUE) %>%
    # Si quieres que se vea más “reporte”, colapsa el Tipo (solo visual)
    collapse_rows(columns = 1, valign = "top") %>%
    scroll_box(width = "100%", height = "450px")
##########
  # lo que regresa 2 cuadros
  message("se aguardaron 2 archivos excel llamados excel_creditos_dispuesto y excel_lineas_d_credito")
  
  list(tabla_creditos_dispuestos=CREDITOS_DISPUESTOS_2,tabla_lineas_d_credito=LINEAS_DE_CREDITO_2,excel_1=write_xlsx(CREDITOS_DISPUESTOS,"excel_creditos_dispuestos.xlsx"),
        excel_2=  write_xlsx(LINEAS_DE_CREDITO,"excel_lineas_d_credito.xlsx"))
  
  
}

