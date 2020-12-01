#Azterketako ariketak R

#2.praktikakoak


#7.ariketa
mean()
median()
variantza<-var(arrailak)-n-1/n
sqrt(variantza)

#8.ariketa
kotxeak<-read.table("kotxeak.txt",header=T)
kotxeak
gasolina<-kotxeak$Gasolina.kotxeak
gasolina
gasoil<-kotxeak$Gas.oil.kotxeak
gasoil

#Merge 2 arrays into 1
a<-cbind(gasolina,gasoil)
a
mean(a)
median(a)
variantza<-var(a)-(length(a)-1/length(a))
variantza


gasoil<-