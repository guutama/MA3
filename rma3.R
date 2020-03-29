data <- matrix(1:36,nrow=6)
data[1,]<-c(0,5558,3469,214,5074,5959)

data[2,]<-c(5558,0,2090,5725,7753,7035)

data[3,]<-c(3469,2090,0,3636,6844,6757)

data[4,]<-c(214,5725,3636,0,5120,6053)

data[5,]<-c(5074,7753,6844,5120,0,1307)
data[6,]<-c(5959,7035,6757,6053,1307,0)
cities <-c("London","Mexico city","New York","Paris","Peking","Tokyo")
colnames(data) <- c("London","Mexico city","New York","Paris","Peking","Tokyo")
rownames(data) <- c("London","Mexico city","New York","Paris","Peking","Tokyo")

	


random.vector<-function(n){
	r<-vector(length=n)
	r[1]<-1
	r[2:n]<-sample(2:6)
	return (r)			
}

			


		

	



route.distance <- function(route)
{	
  km = NULL
  n = 6
 # print(route)
  for (i in 1:5)
  {
    km[i] = data[route[i],route[i+1]]
    
     
  
}
  
  
  return(sum(km) + data[route[6],route[1]])
}




swap.randomly<-function(route){

	x<-sample(2:6,2)
	return(replace(route,c(x[1],x[2]),route[c(x[2],x[1])]))
	
	
}

simanneal.travelling.salesman<-function(n){

		
		#step1: start random route, temperature, K
	
		r<-random.vector(6)
		
		R<-cities[r]
		T<-1000
		
		#step2: evaluate cost fuction
		F<-route.distance(R)
		for (i in 1:n){
		

	
		# step3: define new route by randomly defining new route
		rk <-swap.randomly(r)
		Rk<-cities[rk]
		

		#step 4
		Fk<-route.distance(Rk)

		if (Fk <F){
			R<-Rk
			F<-Fk
		}
		else{
			if(runif(1) < exp((F-Fk)/T)){
				R<-Rk
				F<-Fk
				}
			
		}
		T<-0.9*T
		print(exp((F-Fk)/T))
		print(i)
		print(F)
		
		}
		print("Final:")
		return(F)
		
			
}
simanneal.travelling.salesman(100)
