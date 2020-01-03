library(igraph)
args = commandArgs(TRUE)

load('podstawowe_wschowa.Rdata')
load('barriers.Rdata')
#library(igraph)

# Set the proportion rate
# Set the removal rate
# Set how many timesteps you want to pass through
n_timesteps = 200
# Start from the node you have chosen using edge centrality
seedsy=a[,2]
licz=1
licz_0=1
k=0
# hum=0.3
zakazenia=data.frame(idki=c(0:379))
# kto<-c(0,seedsy,0,seedsy-1,k)
# kto<-rbind(kto,c(0,seedsy,0,seedsy-1,k))
kary<-c(0,0,0,0,0,0,0,0)
  #seed=sample(seeds, 1)
# pork=0.6
aa1=0
gran=0

if (as.numeric(args[1])==1){
  fst=0.3
  inf=0.16
  hum=0.6
  pork=0.7
  far=0.12
  aa1=1
}

if (as.numeric(args[1])==2){
  fst=0.3
  inf=0.16
  hum=0.6
  pork=0.7
  far=0.12
  gran=1
}

if (as.numeric(args[1])==3){
  pork=0.3
  inf=0.16
  far=0.14
  hum=0.6
  aa1=1
}
if (as.numeric(args[1])==4){
  
  pork=0.3
  inf=0.16
  far=0.12
  hum=0.6
  gran=1
}
if (as.numeric(args[1])==5){
  
  pork=0.3
  inf=0.16
  far=0.10
  hum=0.5
  aa1=1
}
if (as.numeric(args[1])==6){
  
  pork=0.3
  inf=0.16
  far=0.12
  hum=0.6
  gran=1
}
if (as.numeric(args[1])==7){
  
  pork=0.4
  inf=0.16
  far=0.12
  hum=0.6
  aa1=1
}
if (as.numeric(args[1])==8){
  
  pork=0.6
  inf=0.16
  far=0.16
  hum=0.3
  gran=1
}

if (as.numeric(args[1])==9){
  
  pork=0.4
  inf=0.16
  far=0.11
  hum=0.4
  aa1=1
}
if (as.numeric(args[1])==10){
  
  pork=0.3
  inf=0.16
  far=0.18
  hum=0.3
  gran=1
}
if (as.numeric(args[1])==12){
  
  pork=0.32
  inf=0.16
  far=0.14
  hum=0.3
  gran=1
}
if (as.numeric(args[1])==11){
  
  pork=0.32
  inf=0.16
  far=0.16
  hum=0.3
  aa1=1
}


if (as.numeric(args[1])==13){
  pork=0.32
  inf=0.16
  far=0.14
  hum=0.6
  aa1=1
}
if (as.numeric(args[1])==14){
  
  pork=0.32
  inf=0.16
  far=0.12
  hum=0.6
  gran=1
}
if (as.numeric(args[1])==15){
  
  pork=0.32
  inf=0.16
  far=0.10
  hum=0.5
  aa1=1
}
if (as.numeric(args[1])==16){
  
  pork=0.32
  inf=0.16
  far=0.12
  hum=0.6
  gran=1
}
if (as.numeric(args[1])==17){
  
  pork=0.32
  inf=0.10
  far=0.12
  hum=0.6
  aa1=1
}
if (as.numeric(args[1])==18){
  
  pork=0.32
  inf=0.10
  far=0.18
  hum=0.3
  gran=1
}

if (as.numeric(args[1])==19){
  
  pork=0.32
  inf=0.10
  far=0.11
  hum=0.4
  aa1=1
}
if (as.numeric(args[1])==20){
  
  pork=0.32
  inf=0.10
  far=0.18
  hum=0.3
  gran=1
}


if (as.numeric(args[1])==21){
  fst=0.3
  inf=0.16
  hum=0.6
  pork=0.7
  far=0.12
}

if (as.numeric(args[1])==22){
  fst=0.3
  inf=0.16
  hum=0.6
  pork=0.7
  far=0.12
}

if (as.numeric(args[1])==23){
  pork=0.3
  inf=0.16
  far=0.14
  hum=0.6
}
if (as.numeric(args[1])==24){
  
  pork=0.3
  inf=0.16
  far=0.12
  hum=0.6
}
if (as.numeric(args[1])==25){
  
  pork=0.3
  inf=0.16
  far=0.10
  hum=0.5
}
if (as.numeric(args[1])==26){
  
  pork=0.3
  inf=0.16
  far=0.12
  hum=0.6
}
if (as.numeric(args[1])==27){
  
  pork=0.4
  inf=0.16
  far=0.12
  hum=0.6
}
if (as.numeric(args[1])==28){
  
  pork=0.6
  inf=0.16
  far=0.16
  hum=0.3
}

if (as.numeric(args[1])==29){
  
  pork=0.4
  inf=0.16
  far=0.11
  hum=0.4
}
if (as.numeric(args[1])==30){
  
  pork=0.3
  inf=0.16
  far=0.18
  hum=0.3

}
if (as.numeric(args[1])==32){
  
  pork=0.32
  inf=0.16
  far=0.14
  hum=0.3
}
if (as.numeric(args[1])==31){
  
  pork=0.32
  inf=0.16
  far=0.16
  hum=0.3
}


if (as.numeric(args[1])==33){
  pork=0.32
  inf=0.16
  far=0.14
  hum=0.6

}
if (as.numeric(args[1])==34){
  
  pork=0.32
  inf=0.16
  far=0.12
  hum=0.6

}
if (as.numeric(args[1])==35){
  
  pork=0.32
  inf=0.16
  far=0.10
  hum=0.5

}
if (as.numeric(args[1])==36){
  
  pork=0.32
  inf=0.16
  far=0.12
  hum=0.6
  
}
if (as.numeric(args[1])==37){
  
  pork=0.32
  inf=0.10
  far=0.12
  hum=0.6
  
}
if (as.numeric(args[1])==38){
  
  pork=0.32
  inf=0.10
  far=0.18
  hum=0.3
  
}

if (as.numeric(args[1])==39){
  
  pork=0.32
  inf=0.10
  far=0.11
  hum=0.4
  
}
if (as.numeric(args[1])==40){
  
  pork=0.32
  inf=0.10
  far=0.18
  hum=0.3
  
}
fst=1-pork





  p1=matrix(0, nrow=380, ncol = 380)
  for (i in 1:379){
    for (j in (i+1):380) {
      p1[i,j]=hum*(V(net2)$lud[i]*V(net2)$lud[j])/(1+macierz[i,j])+pork*(V(net2)$swi[i]*V(net2)$swi[j])/(1+macierz[i,j])+fst*V(net2)$las[i]*V(net2)$las[j]/(1+macierz[i,j])^2
    } }

  if (aa1==1){
  for (i in A1) {
    for (j in 1:380) {
     p1[i,j]=p1[j,i] -fst*V(net2)$las[i]*V(net2)$las[j]/(1+macierz[i,j])^2
    }
  }
  }
  if (gran==1){
  for (i in gra) {
    for (j in 1:380) {
     p1[i,j]=p1[i,j]  -fst*V(net2)$las[i]*V(net2)$las[j]/(1+macierz[i,j])^2
    }
  }
  }
  kolej=rep(0,380)
  kolej[a[,2]]=a[1,]

      for (realiz in 1:20000){
        
  
infected_nodes<- vector()
infected_nodes=append(infected_nodes, seedsy)
# kto<-rbind(kto, c(0,seedsy,0,seedsy-1,k))
licz=licz+1
kto2=c(0,seedsy[1],0,seedsy[1]-1,k)
for (j in 1:length(seedsy)){
  kto2<-rbind(kto2, c(0,seedsy[j],0,seedsy[j]-1,k))
}
licz_0=1

for (i in 1:n_timesteps){
  # Infection stage
  for (node in infected_nodes){
    r=ceiling(380*runif(1))
    czasowy=1/(1+0.002*abs(-kolej[node]+(230+i))-0.3)
    if (runif(1) < czasowy*(p1[node,r]+p1[r,node])*far*inf && (!(r %in% infected_nodes))) {
      infected_nodes=append(infected_nodes, r)
      licz=licz+1
      licz_0=licz_0+1
     # kto<-rbind(kto,c(node,r,i,r-1,k))
      
      kto2<-rbind(kto2,licz_0+1)
      kto2[licz_0+1,1]=node
      kto2[licz_0+1,2]=r
      kto2[licz_0+1,3]=i
      kto2[licz_0+1,4]=r-1
      kto2[licz_0+1,5]=k
      kolej[r]=i+230
    }
    for (neighbor in neighbors(net2,as.character(node-1))){
      # random.random simply returns a number between [0,1)
      czasowy=1/(1+0.002*abs(-kolej[node]+(230+i))-0.3)
      if (runif(1) < czasowy*(p1[neighbor,node]+p1[node, neighbor])*inf && (!(neighbor %in% infected_nodes))) {
        infected_nodes=append(infected_nodes, neighbor)
        licz=licz+1
        licz_0=licz_0+1
      #  kto<-rbind(kto,c(node,neighbor,i,neighbor-1,k))
        kto2<-rbind(kto2,licz_0+1)
        kto2[licz_0+1,1]=node
        kto2[licz_0+1,2]=neighbor
        kto2[licz_0+1,3]=i
        kto2[licz_0+1,4]=neighbor-1
        kto2[licz_0+1,5]=k
        kolej[neighbor]=i+230
        
        
        # infected_nodes=unique(infected_nodes)
      }
    }
  }
}
    zakazenia$czas=NA

 if (length(infected_nodes)<2) 
 {
      zakazenia$czas[kto2[2]]=kto2[3]
    }
 else
    {
    for (licznik in 1:length(infected_nodes))
      {zakazenia$czas[kto2[licznik,2]]=kto2[licznik,3]
    }
    }
    
    # suma=0
    # for (x in 1:dim(a)[1]) {
    #   roznica=abs(zakazenia$czas[a[x,2]+1]-a[x,1])
    #   if (is.na(roznica)) {roznica =200}
    #   suma= suma+roznica
    # }
    # suma= suma + abs(dim(kto2)[1]-dim(a)[1])*200
    # 
    # 
    # kary<-rbind(kary, c(suma,pork,inf,far,hum,dim(kto2)[1],k,realiz))
    # k=k+1
     write.table(kto2, file = paste(args[1],"/kto.txt", sep=''), append = TRUE)
    # 
    
    
    
    #write.table(kary, file = paste(args[1],"/kary.txt", sep=''))
    
  }  
 
# Removal stage

#write.table(kary, file = paste(args[1],"/kary.txt", sep=''))
  #write.table(kto2,file = "kto_sdsd.txt")
  
  
