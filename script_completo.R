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

#Gráfico formacao

df_uso %>% 
  replace_na(list(tipo_emprego ="Não Identificado")) %>% 
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
  

## Análise por Sexo --------------------------------------------------------

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




## Análise por regiao ------------------------------------------------------
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
       x = "Percentual de Jovens Doutores por Região" )+
  theme_minimal()+
  theme(panel.background = element_rect(color = "lightgrey"))

#Painel Região
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


## Análise por região e por sexo -------------------------------------------


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
    

#Visualização região + sexo  
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

  # Correlação
  df_uso %>%
    pivot_longer(c(quant_discip, napresentacoes:tempo_doutor, -n_total_artigos)) %>% 
    ggplot(aes(x = n_total_artigos, y =value ))+
    geom_smooth(method = "lm")+
    facet_wrap(~name)+
    theme_minimal()+
    ggpubr::stat_cor(method = "pearson", label.x = 3, label.y = 130)+
    theme(panel.border = element_rect(fill = NA, color = "lightgrey"))  
  
  #Correlação por sexo
  #Correlação
  df_uso %>%
    pivot_longer(c(quant_discip, napresentacoes:tempo_doutor, -n_total_artigos)) %>% 
    ggplot(aes(x = n_total_artigos, y =value, color = sexo))+
    geom_smooth(method = "lm")+
    facet_wrap(~name)+
    theme_minimal()+
    ggpubr::stat_cor(method = "pearson")+
    scale_color_manual(values = c("deeppink", "steelblue"))+
    labs(x = "N Total de Artigos", 
         y = NULL)+
    theme(panel.border = element_rect(fill = NA, color = "lightgrey"),
          legend.position = "bottom") 

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
  
  #Checando as pressuposições do modelo
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
    

    
  
    
#Gerando os modelos para visualização no sjPlot  
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
                     legend.title = "Variáveis Dependentes",
                     colors = "Dark2")+
   theme_minimal()+
   theme(legend.position = "bottom",
         panel.background = element_rect(color = "lightgrey"))+
   labs(y = "Betas Estimados")
    
# Gerar MOdelos Word
 jtools::export_summs(modelo_n_total_artigos, modelo_n_web_qualis,
                      modelo_qualis_inferior, modelo_qualis_superior) %>% 
   huxtable::quick_docx()
  