create_dendro <- function(bgc.choose, unit.compare){
  compared <- unit.compare %>% filter(bgc1 == bgc2) %>% filter(bgc1 == bgc.choose)
  dis.matrix <- bec_dist_matrix(compared) 
  ss_clst <- agnes(dis.matrix,
                   diss = TRUE, stand = TRUE,
                   method = "average")
  dendro_hc <- as.hclust(ss_clst)
  dend.co <- stats::cophenetic(dendro_hc)
  dend.dis <- as.dist(dis.matrix)
  cophenetic <- cor(dend.dis, dend.co) %>% round(2)
  cophenetic ## 0.891 this value shows how well the clusters align with the data
  # ### >0.7 is considered good
  coph_annotation <- data.frame(x = 9, y=.85, label = paste0("Cophenetic:",cophenetic))
  hcdata <- dendro_data(dendro_hc, type = "rectangle")
  yy <- ggplot() +
    geom_segment(data = segment(hcdata), 
                 aes(x = x, y = y, xend = xend, yend = yend)
    ) +
    geom_text(data = label(hcdata), 
              aes(x = x, y = y, label = label, hjust = 0), 
              size = 3
    ) +
    geom_hline(yintercept = .07, linetype = "dashed", color = "red")+
    
    
    geom_hline(yintercept = .2, linetype = "dashed", color = "green")+
    # add value label to hline
    geom_text(aes(x = 0, y = .07, label = "7%", hjust = 0), angle = 90,color = "red", size = 3)+
    geom_text(aes(x = 0, y = .2, label = "20%", hjust = 0), angle = 90, color = "green", size = 3)+
    
    # add label to graph 
    geom_text(data=coph_annotation, aes( x=x, y=y, label=label), 
              color="black", 
              size=3 , angle=0, fontface="bold" ) +
    # annotate(cophenetic, x = .1, y = .1,
    #      label = "Cophonetic" , color="orange",
    #       size=7 , angle=0, fontface="bold")+
    coord_flip()+
    scale_y_reverse(limits = c(1, -.3))+
    labs(x = "", y = "Difference")+
    theme_minimal()+
    theme(axis.text.y=element_blank(), axis.title.y = element_blank())+
    ggtitle(paste0("Cluster Dendrogram of ", bgc.choose, " Site Series") )
  print(yy)
  
  
}