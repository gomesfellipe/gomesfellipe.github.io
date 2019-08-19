# Fonte: Vinheta do GGally 

# Funcao de correlacoes -------------------------------------------------------------

my_fn <- function(data, mapping, method="glm", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=method, ...)
  p
}

my_bin <- function(data, mapping, ..., low = "#132B43", high = "#56B1F7") {
  ggplot(data = data, mapping = mapping) +
    geom_bin2d(...) +
    scale_fill_gradient(low = low, high = high)
}

# Diagnostico do modelo linear -----------------------------------------------------

# http://ggobi.github.io/ggally/#regression_analysis
# custom function to display continuous data. If the y variable is "Residual", do custom work.
lm_or_resid <- function(data, mapping, ..., line_color = "red", line_size = 1) {
  if (as.character(mapping$y) != "Residual") {
    return(ggally_smooth_lm(data, mapping, ...))
  }
  
  # make residual data to display
  resid_data <- data.frame(
    x = data[[as.character(mapping$x)]],
    y = residuals[[as.character(mapping$x)]]
  )
  
  ggplot(data = data, mapping = mapping) +
    geom_hline(yintercept = 0, color = line_color, size = line_size) +
    ylim(y_range) +
    geom_point(data = resid_data, mapping = aes(x = x, y = y), ...)
  
}

# Plot kmeans ----------------------------------------------------------------------
# https://github.com/gomesfellipe/functions/blob/master/plot_kmeans.R
plot_kmeans = function(df, clusters, runs, standardize=F) {
  # library(psych)
  # library(ggplot2)
  
  #standardize
  if (standardize) df = std_df(df)
  
  #cluster
  tmp_k = kmeans(as.data.frame(df), centers = clusters, nstart = 100)
  
  #factor
  tmp_f = psych::fa(df, 2)
  
  #collect data
  tmp_d = data.frame(matrix(ncol=0, nrow=nrow(df)))
  tmp_d$cluster = as.factor(tmp_k$cluster)
  tmp_d$fact_1 = as.numeric(tmp_f$scores[, 1])
  tmp_d$fact_2 = as.numeric(tmp_f$scores[, 2])
  tmp_d$label = rownames(df)
  
  #plot
  g = ggplot(tmp_d, aes(fact_1, fact_2, color = cluster)) + 
    geom_point() + 
    geom_text(aes(label = label), size = 3, vjust = 1, color = "black") + 
    theme_bw()
  
  return(g)
}
