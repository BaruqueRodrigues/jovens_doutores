library(tidyverse)
library(labelled)

dataset <- haven::read_sav("Base de Dados - Pesquisa Metodologia - 26.03.2019.sav") %>% 
  janitor::clean_names() %>% 
  mutate(
    across(where(is.labelled), to_character),
    uf_universidade = recode(universidade,
                             "UERJ"= "RJ",
                             "UFC" = "CE",
                             "UFG" = "GO",
                             "UFMG" = "MG",
                             "UFPB" = "PB",
                             "UFPE" = "PE",
                             "UFPR" = "PR",
                             "UFRGS" = "RS",
                             "UFSCar" = "SP",
                             "UFS" = "SE",
                             "Unb" = "DF",
                             "Unicamp" = "SP",
                             "USP" = "SP")
    ) 
  # Banco de trabalho
df_uso<- dataset %>% 
  select(ano_tese,sexo, universidade, regiao, quant_discip,
         perspectiva, dados,tecnica_analise,tipo_emprego,indicador_ocupacao_lab,
         gestao, nota_capes_trab,napresentacoes, nlivros, n_capitulos_livros,
         n_total_artigos, n_web_qualis,parecerista,qualis_superior,qualis_inferior) %>% 
  mutate(tempo_doutor = 2018 - as.numeric(ano_tese))

df_uso %>% glimpse()

# Tabela descritiva -------------------------------------------------------

df_uso %>% 
  summarise(across(c(n_total_artigos:qualis_inferior, -parecerista),
                   list(min, mean, median, sd, max),
                   na.rm = TRUE)
            ) %>% 
  rename_with(., ~str_replace_all(., c("_1", "_2",
                                       "_3", "_4",
                                       "_5"),
                                  c("_min", "_mean",
                                    "_median", "_sd",
                                    "_max")
                                  )
              ) %>% 
  pivot_longer(c(1:20)) %>% 
  mutate(estatistica = str_extract(name, c("min|mean|median|sd|max")),
         name = str_remove(name, c("_min|_mean|_median|_sd|_max"))) %>% 
  pivot_wider(names_from = "estatistica",
              values_from = "value") %>% 
  mutate("Distribui????o" = list(df_uso$n_total_artigos,
                      df_uso$n_web_qualis,
                      df_uso$qualis_superior,
                      df_uso$qualis_inferior),
         "Boxplot" = `Distribui????o`) %>%
  select("Vari??vel" = name,
         "M??nimo" = min,
         "M??dia" = mean,
         "Mediana" = median,
         "Desvio Padr??o" = sd,
         "M??ximo" = max,
         "Distribui????o",
         "Boxplot" ) %>% 
  gt::gt() %>% 
  gtExtras::gt_plt_dist("Distribui????o") %>% 
  gtExtras::gt_plt_dist("Boxplot", type = "boxplot")


df_uso %>%
select(n_total_artigos:qualis_inferior, -parecerista) %>% 
  pivot_longer(c(1:4))
# Datavis -----------------------------------------------------------------
df_uso %>% 
  janitor::tabyl(sexo) %>% 
  ggplot(aes(x = reorder(sexo, -n), y = percent))+
  geom_col(fill = "lightgrey", width = .4)+
  geom_text(aes(label = round(percent, 3)*100), vjust = -.5)+
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 55, .05))+
  theme_minimal()+
  theme(panel.background = element_rect(color = "lightgrey"))+
  labs(y= NULL, x= "Sexo dos Jovens Doutores")

#Gr??fico formacao

df_uso %>% 
  replace_na(list(tipo_emprego ="N??o Identificado")) %>% 
  janitor::tabyl(tipo_emprego) %>% 
  ggplot(aes(x = reorder(tipo_emprego, n),
             y = percent)
           )+
  geom_col(
             fill = "lightgrey")+
    geom_text(aes(label = round(percent,3)*100), hjust = -.2)+
    #scale_x_continuous(breaks = seq(0, 140, 20))+
    theme()+
  
    labs(x = NULL, 
         y = "Tipo de Emprego" )+
    theme_minimal()+
  theme(panel.background = element_rect(color = "lightgrey"),
        axis.text.y = element_text(size = 12))+
  scale_y_continuous(labels = scales::percent)+
  coord_flip()
  

## An??lise por Sexo --------------------------------------------------------

#Painel por sexo
df_uso %>% 
  pivot_longer(c(quant_discip, napresentacoes:tempo_doutor)) %>% 
  group_by(sexo, name) %>% 
  summarise(desvio_padrao = sd(value, na.rm = TRUE),
         media = mean(value, na.rm = TRUE)) %>%
  drop_na() %>% 
  ggplot(aes(x = sexo,
             y = media,
             group = sexo))+
  geom_pointrange(aes(ymin = desvio_padrao-media,
                      ymax = desvio_padrao+media))+
  facet_wrap(~name,nrow = 2)+
  theme_minimal()+
  labs(x= NULL, y = NULL)+
  theme(panel.border = element_rect(fill = NA, color = "lightgrey"))




## An??lise por regiao ------------------------------------------------------
# Tipo de Emprego
df_uso %>% 
  janitor::tabyl(regiao) %>% 
  ggplot(aes(x = reorder(regiao, percent),
             y = percent))+
  geom_col(fill = "lightgray")+
  geom_text(aes(label = round(percent,3)*100), vjust = -.2)+
  scale_y_continuous(breaks = seq(0, .5, .05))+
  theme()+
  labs(y = NULL, 
       x = "Percentual de Jovens Doutores por Regi??o" )+
  theme_minimal()+
  theme(panel.background = element_rect(color = "lightgrey"))

#Painel Regi??o
df_uso %>% 
  pivot_longer(c(quant_discip, napresentacoes:tempo_doutor)) %>% 
  group_by(regiao, name) %>% 
  summarise(desvio_padrao = sd(value, na.rm = TRUE),
            media = mean(value, na.rm = TRUE)) %>% 
  
  drop_na() %>% 
  ggplot(aes(x = regiao,
             y = media))+
  geom_pointrange(aes(ymin = desvio_padrao-media,
                      ymax = desvio_padrao+media))+
  facet_wrap(~name,nrow = 2)+
  theme_minimal()+
  labs(x= NULL, y = NULL)+
  theme(panel.border = element_rect(fill = NA, color = "lightgrey"),
        axis.text.x = element_text(angle = 90))


## An??lise por regi??o e por sexo -------------------------------------------


  df_uso %>% 
    ggplot(aes(x=regiao, fill = sexo))+
    geom_bar(position = "dodge")+
    stat_count(aes(label = ..count..),
               geom = "text", vjust = -.5, position =  position_dodge(width = 1))+
    #scale_x_continuous(breaks = seq(0, 140, 20))+
    theme()+
    scale_fill_manual(values = c("deeppink", "steelblue"))+
    labs(x = NULL, 
         y = "" )+
    theme_minimal()+
    theme(legend.position = "top")+
  theme(panel.background = element_rect(color = "lightgrey"),
        legend.position = "bottom")
    

#Visualiza????o regi??o + sexo  
  df_uso %>% 
    pivot_longer(c(quant_discip, napresentacoes:tempo_doutor)) %>% 
    group_by(regiao, name, sexo) %>% 
    summarise(desvio_padrao = sd(value, na.rm = TRUE),
              media = mean(value, na.rm = TRUE)) %>% 
  drop_na() %>% 
    ggplot(aes(x = regiao,
               y = media,
               color = sexo))+
    geom_pointrange(aes(ymin = desvio_padrao-media,
                        ymax = desvio_padrao+media))+
    facet_wrap(~name,nrow = 2)+
    scale_color_manual(values = c("deeppink", "steelblue"))+
    theme_minimal()+
    labs(x= NULL, y = NULL)+
    theme(panel.border = element_rect(fill = NA, color = "black"),
          axis.text.x = element_text(angle = 90),
          legend.position = "top")+
    theme(panel.background = element_rect(color = "lightgrey"),
          legend.position = "bottom")

  # Correla????o
  
 cor_n_total_artigos <- df_uso %>% 
    pivot_longer(c(napresentacoes,n_capitulos_livros,
                   nlivros,parecerista,quant_discip ,tempo_doutor)) %>% 
    ggplot(aes(x = n_total_artigos, y =value ))+
    geom_smooth(method = "lm")+
    facet_wrap(~name)+
    labs(y=NULL)+
    theme_minimal()+
    ggpubr::stat_cor(method = "pearson", label.x = 3, label.y = 130)+
    theme(panel.border = element_rect(fill = NA, color = "lightgrey")) 
  
  
 cor_n_web_qualis <- df_uso %>% 
    pivot_longer(c(napresentacoes,n_capitulos_livros,
                   nlivros,parecerista,quant_discip ,tempo_doutor)) %>% 
    ggplot(aes(x = n_web_qualis, y =value ))+
    geom_smooth(method = "lm")+
    facet_wrap(~name)+
    labs(y=NULL)+
    theme_minimal()+
    ggpubr::stat_cor(method = "pearson", label.x = 3, label.y = 100)+
    theme(panel.border = element_rect(fill = NA, color = "lightgrey")) 
  
  cor_qualis_superior <- df_uso %>% 
    pivot_longer(c(napresentacoes,n_capitulos_livros,
                   nlivros,parecerista,quant_discip ,tempo_doutor)) %>% 
    ggplot(aes(x = qualis_superior, y =value ))+
    geom_smooth(method = "lm")+
    facet_wrap(~name)+
    labs(y=NULL)+
    theme_minimal()+
    ggpubr::stat_cor(method = "pearson", label.x = 3, label.y = 50)+
    theme(panel.border = element_rect(fill = NA, color = "lightgrey")) 
  
  cor_qualis_inferior <-df_uso %>% 
    pivot_longer(c(napresentacoes,n_capitulos_livros,
                   nlivros,parecerista,quant_discip ,tempo_doutor)) %>% 
    ggplot(aes(x = qualis_inferior, y =value ))+
    geom_smooth(method = "lm")+
    facet_wrap(~name)+
    labs(y=NULL)+
    theme_minimal()+
    ggpubr::stat_cor(method = "pearson", label.x = 3, label.y = 90)+
    theme(panel.border = element_rect(fill = NA, color = "lightgrey")) 
  
  library(patchwork)
  
  cor_n_total_artigos+cor_n_web_qualis+cor_qualis_inferior+cor_qualis_superior
  #Correla????o por sexo
  #Correla????o
  
  cor_n_total_artigos_genero <- df_uso %>% 
    pivot_longer(c(napresentacoes,n_capitulos_livros,
                   nlivros,parecerista,quant_discip ,tempo_doutor)) %>% 
    ggplot(aes(x = n_total_artigos, y =value , color = sexo))+
    geom_smooth(method = "lm")+
    facet_wrap(~name)+
    labs(y=NULL)+
    theme_minimal()+
    ggpubr::stat_cor(aes(color = sexo),
                     method = "pearson")+
    scale_color_manual(values = c("deeppink", "steelblue"))+
    theme(panel.border = element_rect(fill = NA, color = "lightgrey"),
          legend.position = "none") 
  
  
  cor_n_web_qualis_genero <- df_uso %>% 
    pivot_longer(c(napresentacoes,n_capitulos_livros,
                   nlivros,parecerista,quant_discip ,tempo_doutor)) %>% 
    ggplot(aes(x = n_web_qualis, y =value , color = sexo))+
    geom_smooth(method = "lm")+
    facet_wrap(~name)+
    labs(y=NULL)+
    theme_minimal()+
    scale_color_manual(values = c("deeppink", "steelblue"))+
    ggpubr::stat_cor(aes(color = sexo),
                     method = "pearson")+
    theme(panel.border = element_rect(fill = NA, color = "lightgrey"),
          legend.position = "none") 
  
  cor_qualis_superior_genero <- df_uso %>% 
    pivot_longer(c(napresentacoes,n_capitulos_livros,
                   nlivros,parecerista,quant_discip ,tempo_doutor)) %>% 
    ggplot(aes(x = qualis_superior, y =value , color = sexo))+
    geom_smooth(method = "lm")+
    facet_wrap(~name)+
    labs(y=NULL)+
    scale_color_manual(values = c("deeppink", "steelblue"))+
    theme_minimal()+
    ggpubr::stat_cor(aes(color = sexo),
                     method = "pearson")+
    theme(panel.border = element_rect(fill = NA, color = "lightgrey"),
          legend.position = "none") 
  
  cor_qualis_inferior_genero <-df_uso %>% 
    pivot_longer(c(napresentacoes,n_capitulos_livros,
                   nlivros,parecerista,quant_discip ,tempo_doutor)) %>% 
    ggplot(aes(x = qualis_inferior, y =value , color = sexo))+
    geom_smooth(method = "lm")+
    facet_wrap(~name)+
    labs(y=NULL)+
    scale_color_manual(values = c("deeppink", "steelblue"))+
    theme_minimal()+
    ggpubr::stat_cor(aes(color = sexo),
                     method = "pearson")+
    theme(panel.border = element_rect(fill = NA, color = "lightgrey"),
          legend.position = "none")  

  cor_n_total_artigos_genero+cor_n_web_qualis_genero+
    cor_qualis_inferior_genero+cor_qualis_superior_genero
  
df_uso %>% 
  select(quant_discip, napresentacoes:tempo_doutor) %>% 
  mutate(across(c(quant_discip, napresentacoes:tempo_doutor), ~replace_na(., 0 ))) %>% 
  cor() %>% 
  corrplot::corrplot(method = "number")
# Modelos
  
  modelos <- df_uso %>% 
    pivot_longer(c(n_total_artigos, n_web_qualis,qualis_inferior, qualis_superior),
                 names_to = "modelo") %>% 
    nest(-modelo) %>% 
    mutate(model = map(data, ~lm(value~napresentacoes+n_capitulos_livros+
                                    nlivros+parecerista+quant_discip+tempo_doutor+
                                    sexo+regiao,
                                  data = .)),
           
           
           info_modelo = map(model, broom::glance),
           performance_model = map(model, performance::check_model),
           model = map(model, broom::tidy)
           ) %>% 
    select(-data) 
  
  #Checando as pressuposi????es do modelo
  modelos %>% 
    pull(performance_model) %>% 
    pluck
  
  # Gerando coeficientess
  modelo_coef_empilhado <- modelos %>% 
    unnest(model, info_modelo) %>% 
    select(-c(9:19))
  
  modelo_coef_empilhado %>% 
  pivot_longer(c(estimate:adj.r.squared),
               names_to = "estatistica", values_to = "score"
               )  %>% 
  pivot_wider(names_from = modelo,
              values_from =  score) %>% View()
    

    
  
    
#Gerando os modelos para visualiza????o no sjPlot  
  modelo_n_total_artigos <- lm(formula = n_total_artigos~napresentacoes+n_capitulos_livros+
               nlivros+parecerista+quant_discip+tempo_doutor+
       sexo+regiao,
     data = df_uso)
  
  modelo_n_web_qualis <- lm(formula = n_web_qualis~napresentacoes+n_capitulos_livros+
       nlivros+parecerista+quant_discip+tempo_doutor+
       sexo+regiao,
     data = df_uso)
  
  modelo_qualis_superior <-   lm(formula = qualis_superior~napresentacoes+n_capitulos_livros+
       nlivros+parecerista+quant_discip+tempo_doutor+
       sexo+regiao,
     data = df_uso)
  
 modelo_qualis_inferior <- lm(formula = qualis_inferior~napresentacoes+n_capitulos_livros+
       nlivros+parecerista+quant_discip+tempo_doutor+
       sexo+regiao,
     data = df_uso)
  
 #plot coeficientes Modelo
 sjPlot::plot_models(modelo_n_total_artigos, modelo_n_web_qualis,
                     modelo_qualis_inferior, modelo_qualis_superior,
                     legend.title = "Vari??veis Dependentes",
                     colors = "Dark2")+
   theme_minimal()+
   theme(legend.position = "bottom",
         panel.background = element_rect(color = "lightgrey"))+
   labs(y = "Betas Estimados")
    
# Gerar MOdelos Word
 jtools::export_summs(modelo_n_total_artigos, modelo_n_web_qualis,
                      modelo_qualis_inferior, modelo_qualis_superior) %>% 
   huxtable::quick_docx()
  