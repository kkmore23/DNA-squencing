# DNA-squencing
sequence1<- c('0', '0', my.name)
sequence2<-c('0','0', my.name2)
x<-unlist(strsplit( sequence1,''))
y<-unlist(strsplit(sequence2,''))
#Mymat1<-matrix(data=sequence1,nrow=8,ncol = 5 )
#X <- matrix(1:9, nrow = 3, dimnames = list(c("X","Y","Z"), c("A","B","C")))
#empty=matrix( data = NA, nrow = 6,ncol =9, Â  byrow = TRUE) Â 
A = matrix( data = x, nrow = length(y),ncol =length(x),  byrow = TRUE)
for(i in 2:length(y)){
 for(j in 3:length(x)){
  A[i,j]<-0
 }
}
j<-1
for(i in 3:length(y)){
  A[i,j]<-y[i]
}

j<-2
for(i in 3:length(y)){
  A[i,j]<-(2-i)  
}

i<-2
for(j in 3:length(x)){
  A[i,j]<-(2-j)
}
l<-2
#Final logic
d<-c(0,0,0)
for (i in 3:length(y)){
	k<-2
	for(j in 3:length(x)){
		if(A[i,j-k]==A[i-l,j]){
			d[1]<-1+as.numeric(A[i-1,j-1])
			d[2]<-as.numeric(A[i,j-1])-1
			d[3]<-as.numeric(A[i-1,j])-1
			A[i,j]<-max(d)
		}
		if(A[i,j-k]!=A[i-l,j]){
			d[1]<-0+as.numeric(A[i-1,j-1])
			d[2]<-as.numeric(A[i,j-1])-1
			d[3]<-as.numeric(A[i-1,j])-1
			A[i,j]<-max(d)	
		}
		k<-k+1
		}
		l<-l+1
}
