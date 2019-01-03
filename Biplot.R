#set default directory
setwd('C:/Users/940088/Desktop')

#baca data, sesuaikan separator sama decimal sesuai settingan excel di laptop masing2
data<-read.table(file.choose(),header=TRUE, sep=";", fill=TRUE, dec=",")
View(data)

#subset data (buang nama kolom dan baris)
matriks<-as.matrix.data.frame(data[,3:11])
class(matriks)
View(matriks)

#buat label (nama observasi dan nama variabel)
xlabel<-data[1:20,1]
ylabel<-colnames(data[,3:11])

#Single Value Decomposition
y<-svd(matriks)
U<-y$u
L<-diag(y$d)
A<-y$v

#Matriks G H untuk biplot
G<-U%*%sqrt(L)
Ht<-L%*%t(A)
H<-t(Ht)
G2<-G[,1:2]
H2<-H[,1:2]

#Cek besar keragaman yang mampu dijelaskan biplot (baiknya > 70%)
cek<-eigen(t(matriks)%*%matriks)$values
dim1<-sum(cek[1])/sum(cek)*100
dim2<-sum(cek[2])/sum(cek)*100
dim1+dim2

#buat keterangan di sumbu x dan y tenatang keragaman
xlab<-paste("Dimension 1(",round(dim1,2),"%)")
ylab<-paste("Dimension 2(",round(dim2,2),"%)")

#buat biplot
biplot (G2, H2, xlabs=xlabel, ylabs = ylabel, cex=0.8, main="Biplot Analysis", xlab = xlab, ylab=ylab)

#eksport ke jpeg
png(file="biplot.png",width=800,height=750,res=100)
biplot(G2, H2, xlabs=xlabel, ylabs = ylabel, cex=0.8, main="Biplot Analysis", xlab = xlab, ylab=ylab)
abline(h=0)
abline(v=0)

#tambahan
#matirks var covar (analisa keragaman alias panjang panah di biplot jika mau)
var<-H%*%t(H)
sqrt(diag(var))
cov2cor(var)

#matriks jarak euclid (analisa jarak dari tiap posisi observasi)
euclid<-G%*%t(G)
euclid





