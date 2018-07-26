# Heatmap of catagorical variables
ggplot(nba.m, aes(variable, Name)) + geom_tile(aes(fill = rescale),
+     colour = "white") + scale_fill_gradient(low = "white",
+     high = "steelblue"))
