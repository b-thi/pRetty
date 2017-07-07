#### Heat Map Function ####

# heatmap Function
heatmap <- function(df, titlename, l1 = "yellow", 
                    m1 = "orange", h1 = "red", xname = "Set of Variables [1]",
                    yname = "Set of Variables [2]", corr_method = "pearson") {
  
  # loading libraries
  library(tidyverse)
  library(reshape2)
  
  # let's first look at numeric variables
  df_num <- select_if(df, is.numeric)
  
  # looking for correlations between our target variable and the other numeric variables
  df_narm <- na.omit(df_num)
  corr.df <- cor(df_narm, method = corr_method)
  
  # lower triangular function
  get_lower_tri <- function(data){
    data[upper.tri(data)] <- NA
    return(data)
  }
  
  # applying the function and melting the data
  melt.corr.df <- melt(get_lower_tri(corr.df), na.rm = T)
  
  # creating heatmap
  heat_map <- ggplot(data = melt.corr.df, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "black") + scale_fill_gradient2(low = l1, high = h1, mid = m1, 
                                                      midpoint = 0, limit = c(-1,1), space = "Lab", 
                                                      name="Correlation") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
    coord_fixed() + 
    labs(x = xname, y = yname) +
    ggtitle(titlename) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    guides(fill = guide_colorbar(barwidth = 3, barheight = 7,
                                 title.position = "top", title.hjust = 0.5))
  return(heat_map)
}
