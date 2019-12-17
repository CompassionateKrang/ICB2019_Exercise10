#Set initial values, where N[1] = 99; M[1] = 1 
N=99
M=1
rM= 0.1
rN= 0.1
K= 1000000

#Growth function:
for (t in 2:1000){
  N[t]<-N[t-1]+rN*N[t-1]*(1-(N[t-1]+M[t-1])/K)
  M[t]<-M[t-1]+rM*M[t-1]*(1-(N[t-1]+M[t-1])/K)
  #d value for detecting delta (change in non-mutant cell count is tracked to stop the function once at final equilibrium)
  d<-abs(N[t]-N[t-1])
  #S value for detecting sum of both cell types (drug is applied when sum is approximately = carrying capacity of 1,000,000)
  S<<-N[t] + M[t]
  #If statement added to apply the drug, and change growth rates
  if (S > 999999){
    rN <<- -0.1
    rM <<- 0.5
  }
  #"If statement" added to detect whether graph is complete; "if(t>300)" is added to ommit the first equilibrium which is prior to t = 300
if(t>300)
  #stops the program if the delta value for non-mutants drops below 1 cell change
    stopifnot(d>1)
}
#Creating a data frame for subsequent plot generation
tumor2<-data.frame(time=1:length(M),N,M)

#To assign values for plot:
CellGrowth<-ggplot(data=tumor2, mapping=aes(x=Time,y=Cell_Count))+geom_line(aes(x=time,y=M))+geom_line(aes(x=time,y=N))
#To generate plot:
CellGrowth
