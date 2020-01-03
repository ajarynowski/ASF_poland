load("D:/asf/wszystko.RData")
library(igraph)
gra
#install.packages("EpiSignalDetection")


fst=0.3
scale=0.18
hum=0.6
pork=0.7
# p1=matrix(0, nrow=380, ncol = 380)
# 
#   for (i in 1:379){
#     for (j in (i+1):380) {
# p1[i,j]=scale*(hum*(counties_data$hum[i]*counties_data$hum[j])/(1+distance_C[i,j])+pork*(counties_data$pig[i]*counties_data$pig[j])/(1+distance_C[i,j])+fst*counties_data$forest[i]*counties_data$forest[j]/(1+distance_C[i,j])^2)
# p1[j,i]=p1[i,j]
#     }
#   }

mult_g=matrix(1, nrow=380, ncol = 380)
mult_a1=matrix(1, nrow=380, ncol = 380)
mult_g2=matrix(1, nrow=380, ncol = 380)
mult_a12=matrix(1, nrow=380, ncol = 380)
# gra 22  23  26  29  70  83 101 133 184 252 255 363
A1=c(62,193,283,9,196,21,212,123,49,268,55,348)
p1p=p1
p1l=p1
pgp=p1
pgl=p1
for (i in A1) {
  for (j in 1:380) {
    p1p[i,j]=p1[i,j] -scale*fst*counties_data$forest[i]*counties_data$forest[j]/(1+distance_C[i,j])^2
    mult_a1[i,j] =p1p[i,j]/p1[i,j]
    p1l[j,i]=p1[i,j] -scale*fst*counties_data$forest[j]*counties_data$forest[i]/(1+distance_C[i,j])^2
    mult_a12[j,i] =p1l[j,i]/p1[j,i]
  }
  
  
}
for (i in gra) {
  for (j in 1:380) {
    pgp[i,j]=p1[i,j] -scale*fst*counties_data$forest[i]*counties_data$forest[j]/(1+distance_C[i,j])^2
    mult_g2[i,j] =pgp[i,j]/p1[i,j]
    pgl[j,i]=p1[i,j] -scale*fst*counties_data$forest[j]*counties_data$forest[i]/(1+distance_C[i,j])^2
    mult_g[j,i] =pgl[j,i]/p1[j,i]
    
    
  }
  
  
}
for (j in 1:380) {
  mult_a1[j,j]=1
  mult_g[j,j]=1
  mult_a12[j,j]=1
  mult_g2[j,j]=1
}
#Poland_fully=graph_from_adjacency_matrix(p1p,  weighted=TRUE)
#write.csv(p1, file="Pol_fully.csv")
sciezki=graph_from_adjacency_matrix(sciezki2,  weighted=TRUE)
pr_sci=page.rank(sciezki, directed = TRUE)
pageranki$sci_deg=strength(sciezki, mode = "in")
pr_sci$vector[74]
p2=floor(mult_a1*sciezki2)
sciezki_a1=graph_from_adjacency_matrix(p2,  weighted=TRUE)
pr_sci_a1=page.rank(sciezki_a1, directed = TRUE)
pageranki$sci_deg_a1=strength(sciezki_a1, mode = "in")

pr_sci_a1$vector[74]

p22=floor(mult_a12*sciezki2)
sciezki_a12=graph_from_adjacency_matrix(p22,  weighted=TRUE)
pr_sci_a12=page.rank(sciezki_a12, directed = TRUE)
pr_sci_a12$vector[74]
pageranki$sci_deg_a12=strength(sciezki_a12, mode = "in")


p2g2=floor(mult_g2*sciezki2)
sciezki_g2=graph_from_adjacency_matrix(p2g2,  weighted=TRUE)
pr_sci_g2=page.rank(sciezki_g2, directed = TRUE)
pr_sci_g2$vector[74]
pageranki$sci_deg_g2=strength(sciezki_g2, mode = "in")


p2g=floor(mult_g*sciezki2)
sciezki_g=graph_from_adjacency_matrix(p2g,  weighted=TRUE)
pr_sci_g=page.rank(sciezki_g, directed = TRUE)
pr_sci_g$vector[74]
pageranki$sci_deg_g=strength(sciezki_g, mode = "in")

#read_file=read.csv("Pol_fully.csv", header = FALSE)
#treshold
#E(Poland_fully)
Poland_fully_no_barrier=graph_from_adjacency_matrix(p1,  weighted=TRUE)
Poland_smaller=delete.edges(Poland_fully_no_barrier, which(E(Poland_fully_no_barrier)$weight<0.001))
#E(Poland_smaller)
pr=page.rank(Poland_fully_no_barrier, directed = TRUE)

V(Poland_fully)$names=counties_data$county
pr2=page.rank(Poland_fully, directed = TRUE)
Poland_fully_l=graph_from_adjacency_matrix(p1l,  weighted=TRUE)

V(Poland_fully_l)$names=counties_data$county
pr2_l=page.rank(Poland_fully_l, directed = TRUE)
Poland_fully_gl=graph_from_adjacency_matrix(pgl,  weighted=TRUE)

V(Poland_fully_gl)$names=counties_data$county
prg_l=page.rank(Poland_fully_gl, directed = TRUE)
Poland_fully_gp=graph_from_adjacency_matrix(pgp,  weighted=TRUE)

V(Poland_fully_gp)$names=counties_data$county
prg_p=page.rank(Poland_fully_gp, directed = TRUE)
prg_p$vector[74]
prg_l$vector[74]
pr2_l$vector[74]
pr2$vector[74]
pr$vector[74]
pageranki=data.frame(idi=numeric(380))
pr$id1=c(0:379)

pageranki$idi=pr$id1
pageranki$prg_p=prg_p$vector
pageranki$prg_l=prg_l$vector
pageranki$pr2_l=pr2_l$vector
pageranki$pr2=pr2$vector
pageranki$pr=pr$vector

pageranki$pr_sci=pr_sci$vector
pageranki$pr_sci_a1=pr_sci_a1$vector
pageranki$pr_sci_a12=pr_sci_a12$vector
pageranki$pr_sci_g=pr_sci_g$vector
pageranki$pr_sci_g2=pr_sci_g2$vector
#install.packages("tidygraph")
library(tidygraph)
bet=betweenness(Poland_fully_no_barrier, directed = TRUE, weights = E(Poland_fully_no_barrier)$weight)
degi=degree(Poland_fully_no_barrier, normalized = TRUE)
alfa=igraph::alpha_centrality((Poland_fully_no_barrier))
clo=igraph::closeness(Poland_fully_no_barrier, mode = "in")

pageranki$clo=clo

map_pr <- inner_join(map, pageranki, by = c("id" = "idi"))
p<-ggplot(data = map_pr, mapping = aes(x = long, y = lat)) +
  geom_polygon(mapping = aes(group = group, fill = clo))+
  scale_fill_distiller("pr", palette = "RdPu")
p