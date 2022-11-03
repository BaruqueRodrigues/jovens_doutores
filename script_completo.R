library(tidyverse)
library(labelled)

dataset <- haven::read_sav("Base de Dados - Pesquisa Metodologia - 26.03.2019.sav") %>% 
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
    ) %>% 
  janitor::clean_names() 
  
  # Banco de trabalho
df_uso<- dataset %>% 
  select(ano_tese,sexo, universidade, regiao, quant_discip,
         perspectiva, dados,tecnica_analise,tipo_emprego,indicador_ocupacao_lab,
         gestao, nota_capes_trab,napresentacoes, nlivros, n_capitulos_livros,
         n_total_artigos, n_web_qualis,parecerista,qualis_superior,qualis_inferior) %>% 
  mutate(tempo_doutor = 2018 - as.numeric(ano_tese))

df_uso %>% glimpse()

# Datavis -----------------------------------------------------------------


#Gráfico formacao

df_uso %>% 
  ggplot(aes(y= tipo_emprego)
           )+
    
    geom_bar(#stat = "identity",
             fill = "lightgrey")+
    stat_count(aes(label = ..count..),
               geom = "text", hjust = -.05)+
    scale_x_continuous(breaks = seq(0, 140, 20))+
    theme()+
    labs(x = NULL, 
         y = "Tipo de Emprego" )+
    theme_minimal()
  

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
  theme(panel.border = element_rect(fill = NA, color = "black"))




## Análise por regiao ------------------------------------------------------
# Tipo de Emprego
df_uso %>% 
  ggplot(aes(x=regiao))+
  geom_bar(fill = "lightgray")+
  stat_count(aes(label = ..count..),
                       geom = "text", vjust = -.5)+
  #scale_x_continuous(breaks = seq(0, 140, 20))+
  theme()+
  labs(x = NULL, 
       y = "Número de Jovens Doutores por Região" )+
  theme_minimal()

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
  theme(panel.border = element_rect(fill = NA, color = "black"),
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
    theme(legend.position = "top")
    

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
          legend.position = "top")

  # Correlação
  df_uso %>%
    pivot_longer(c(quant_discip, napresentacoes:tempo_doutor, -n_total_artigos)) %>% 
    ggplot(aes(x = n_total_artigos, y =value ))+
    geom_smooth(method = "lm")+
    facet_wrap(~name)+
    theme_minimal()+
    ggpubr::stat_cor(method = "pearson", label.x = 3, label.y = 130)+
    theme(panel.border = element_rect(fill = NA, color = "black"))    

# Modelos
  
  modelo_coef_empilhado <-df_uso %>% 
    pivot_longer(c(n_total_artigos, n_web_qualis,qualis_inferior, qualis_superior),
                 names_to = "modelo") %>% 
    nest(-modelo) %>% 
    mutate(model = map(data, ~lm(value~napresentacoes+n_capitulos_livros+
                                    nlivros+parecerista+quant_discip+tempo_doutor+
                                    sexo+regiao,
                                  data = .)),
           info_modelo = map(model, broom::glance),
           model = map(model, broom::tidy)
           
           ) %>% 
    select(-data) %>% 
    unnest(model, info_modelo) %>% 
    select(-c(9:18))
  
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
     data = df_uso) %>% 
    summary()
  
 modelo_qualis_inferior <- lm(formula = qualis_inferior~napresentacoes+n_capitulos_livros+
       nlivros+parecerista+quant_discip+tempo_doutor+
       sexo+regiao,
     data = df_uso)
  
 
 sjPlot::plot_models(modelo_n_total_artigos, modelo_n_web_qualis,
                     modelo_qualis_inferior, modelo_qualis_superior)+
   theme(legend.position = "top")
  