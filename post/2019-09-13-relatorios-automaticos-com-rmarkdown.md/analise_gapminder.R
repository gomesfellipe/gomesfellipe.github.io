# Funcao para analise por ano:
analise_gapminder <- function(x, title){
  
  # Carregar dependencias:
  require(broom)
  require(ggforce)
  require(ggpmisc)
  require(ggExtra)
  
  # Funcao para customizar legendas:
  custom_legend <- function(x){comma(x, big.mark = ".",decimal.mark = ",")}
  
  # Obter dados do Brasil:
  brazil <- x %>% filter(country == "Brazil")
  
  # Resultados do ajuste de regressao ---------------------------------------
  mytable <- 
    lm(lifeExp ~ gdpPercap, data = x) %>% 
    tidy() %>% 
    mutate_if(is.numeric, ~round(.x, 4)) %>% 
    `colnames<-`(c("Termo", "Estimativa", "Desv.Pad.", "Estatistica", "Valor p"))
  
  # r2:
  r2 <- round(summary(lm(lifeExp ~ gdpPercap, data = x))$r.squared,4)*100
  
  # residuos do modelo:
  res <- lm(lifeExp ~ gdpPercap, data = x)$residuals
  
  # resutado para teste de kolmogorov-smirnov
  ks_test <- ks.test(res, "pnorm", mean(res), sd(res))$p.value %>% round(5)
  
  # Grafico geral com regressao e boxplots ----------------------------------
  grafico_geral_regressao <- 
    x %>% 
    ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, label = country, color = country)) %+%
    geom_point(show.legend = F) %+%
    geom_text(show.legend = F, size = 3, nudge_y = -0.5) %+%
    scale_size_continuous(labels = custom_legend) %+%
    scale_x_log10(labels = custom_legend) %+%
    scale_color_manual(values = country_colors) %+%
    geom_smooth(se=F, color = "black", show.legend = F, method = "lm") %+%
    annotate("segment", color="blue", arrow=arrow(length=unit(0.05,"npc")),
             x=brazil$gdpPercap, xend=brazil$gdpPercap,
             y=brazil$lifeExp-6, yend=brazil$lifeExp-1) %+%
    annotate("text", color="blue", label = "Brasil",
             x=brazil$gdpPercap, y=brazil$lifeExp-7) %+%
    labs(title = paste0(title, ": lifeExp ~ gdpPercap"),
         subtitle = "Regressão linear e destaque no Brasil",
         caption = paste0("R² do modelo: ", r2, "\n","p valor para ks.test: ", ks_test),
         x = "gdpPercap (Transformação log10)") %+%
    annotate(geom = "table", x = Inf, y = -Inf,
             label = list(mytable), 
             vjust = 0, hjust = 1) %>%  
    ggMarginal(type = "boxplot", fill="transparent",size = 10)
  
  # Comportamento separado por continente -----------------------------------
  grafico_por_continente <- 
    x %>% 
    filter(continent != "Oceania") %>% 
    ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, label = country, color = country)) %+%
    geom_point(show.legend = F) %+%
    geom_text(show.legend = F, size = 3, nudge_y = -0.5) %+%
    facet_wrap(~continent, scales = "free") %+%
    scale_x_continuous(labels = custom_legend) %+%
    scale_color_manual(values = country_colors) %+% 
    geom_smooth(method = "lm", color = "black", se=F, show.legend = F) %+%
    labs(title = paste0(title, ": lifeExp ~ gdpPercap, por continente"))
  
  # Acima da media ----------------------------------------------------------
  grafico_zoom_acima_media <- 
    x %>% 
    ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, label = country, color = country)) %+%
    geom_point(show.legend = F) %+% 
    geom_text(show.legend = F, size = 3, nudge_y = -0.5) %+%
    scale_size_continuous(labels = custom_legend) %+%
    scale_x_continuous(labels = custom_legend) %+%
    scale_color_manual(values = country_colors) %+%
    facet_zoom(y = lifeExp   > median(x$lifeExp),
               x = gdpPercap > median(x$gdpPercap), split = T) %+%
    geom_smooth(se=F, color = "red", show.legend = F, method = "loess")  %+%
    labs(title = paste0(title, ": lifeExp ~ gdpPercap com zoom nos países acima da mediana"))
  
  # Abaixo da media ---------------------------------------------------------
  grafico_zoom_abaixo_media <- 
    x %>% 
    ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, label = country, color = country)) %+%
    geom_point(show.legend = F) %+%
    geom_text(show.legend = F, size = 3, nudge_y = -0.5) %+%
    scale_size_continuous(labels = custom_legend) %+%
    scale_x_continuous(labels = custom_legend) %+%
    scale_color_manual(values = country_colors) %+%
    facet_zoom(y = lifeExp   < median(x$lifeExp),
               x = gdpPercap < median(x$gdpPercap), split = T) %+%
    geom_smooth(se=F, color = "red", show.legend = F, method = "loess")   %+%
    labs(title = paste0(title, ": lifeExp ~ gdpPercap com zoom nos países abaixo da mediana"))
  
  # Output ------------------------------------------------------------------
  list(
    brazil  = brazil,
    mytable = mytable,
    r2      = r2,
    grafico_geral_regressao   = grafico_geral_regressao,
    grafico_por_continente    = grafico_por_continente,
    grafico_zoom_acima_media  = grafico_zoom_acima_media,
    grafico_zoom_abaixo_media = grafico_zoom_abaixo_media,
    ks_test = ks_test
  )
  
}