
####################################################################################################
####################################################################################################
####################################################################################################
### 3 categories
####################################################################################################
####################################################################################################
####################################################################################################

f.OD3=function(p1,p2,p3,OR,ratio)
{
  p.c=c(p1,p2,p3)
  cp.c=cumsum(p.c)
  cp.c=cp.c[cp.c<1]
  logit.cp.c=log(cp.c/(1-cp.c))
  logit.cp.i=logit.cp.c+log(OR)
  cp.i=exp(logit.cp.i)/(1+exp(logit.cp.i))
  p.i=rep(0,3)				### response probabilities intervention
  p.i[1]=cp.i[1]
  p.i[2]=cp.i[2]-p.i[1]
  p.i[3]=1-p.i[2]-p.i[1]

  ####################################################################################################
  ### derivation of correlation matrix - Bu 2019 paper cumulative model
  ####################################################################################################
  #### control condition
  ### page S5 matrix inverse CDL for cumulative model
  CDLinv=matrix(0,3,3)
  CDLinv[1,1]=cp.c[1]*(1-cp.c[1])
  CDLinv[2,2]=cp.c[2]*(1-cp.c[2])
  CDLinv[2,1]=-1*cp.c[1]*(1-cp.c[1])
  CDLinv[3,2]=-1*(cp.c[2])*(1-cp.c[2])
  CDLinv[1,3]=p.c[1]
  CDLinv[2,3]=p.c[2]
  CDLinv[3,3]=p.c[3]
  
  ### page S10 matrix X for cumulative model from equation (S.4)
  X=matrix(0,3,3)
  X[1,1]=1
  X[1,3]=0
  X[2,2]=1
  X[2,3]=0
  
  ### page 6, diag(pi) for equation (4)
  invdiagpi=diag(1/p.c)
  CDLinvX=CDLinv%*%X
  F.c=t(CDLinvX)%*%invdiagpi%*%(CDLinvX)
  
  #### intervention condition
  ### page S5 matrix inverse CDL for cumulative model
  CDLinv=matrix(0,3,3)
  CDLinv[1,1]=cp.i[1]*(1-cp.i[1])
  CDLinv[2,2]=cp.i[2]*(1-cp.i[2])
  CDLinv[2,1]=-1*cp.i[1]*(1-cp.i[1])
  CDLinv[3,2]=-1*(cp.i[2])*(1-cp.i[2])
  CDLinv[1,3]=p.i[1]
  CDLinv[2,3]=p.i[2]
  CDLinv[3,3]=p.i[3]
  
  ### page S10 matrix X for cumulative model from equation (S.4)
  X=matrix(0,3,3)
  X[1,1]=1
  X[1,3]=1
  X[2,2]=1
  X[2,3]=1
  
  ### page 6, diag(pi) for equation (4)
  invdiagpi=diag(1/p.i)
  CDLinvX=CDLinv%*%X
  F.i=t(CDLinvX)%*%invdiagpi%*%(CDLinvX)
  
  ####################################################################################################
  ### optimality criterion as a function of prop.i ### fixed budget
  ####################################################################################################
  prop.i=seq(0.01,0.99,by=0.01)
  c.opt=rep(0,length(prop.i))
  
  B=1					### budget
  c.c=1					### costs per subject in control
  c.i=ratio					### costs per subject in intervention
  N=B/(prop.i*c.i+(1-prop.i)*c.c)	### total number of subjects as function of costs
  n.i=N*prop.i				### number of subjects intervention
  n.c=N*(1-prop.i)				### number of subjectse control
  
  for (ii in 1:length(prop.i))
  {
    Fisher=n.i[ii]*F.i+n.c[ii]*F.c
    covmat=solve(Fisher)
    c.opt[ii]=covmat[3,3]
   }
   RE.c=min(c.opt)/c.opt
   output=cbind(prop.i,c.opt,RE.c)
   return(output)
}


####################################################################################################
####################################################################################################
####################################################################################################
### 4 categories
####################################################################################################
####################################################################################################
####################################################################################################

f.OD4=function(p1,p2,p3,p4,OR,ratio)
{
  p.c=c(p1,p2,p3,p4) 
  cp.c=cumsum(p.c)
  cp.c=cp.c[cp.c<1]
  logit.cp.c=log(cp.c/(1-cp.c))
  logit.cp.i=logit.cp.c+log(OR)
  cp.i=exp(logit.cp.i)/(1+exp(logit.cp.i))
  p.i=rep(0,4)				### response probabilities intervention
  p.i[1]=cp.i[1]
  p.i[2]=cp.i[2]-cp.i[1]
  p.i[3]=cp.i[3]-cp.i[2]
  p.i[4]=1-cp.i[3]
  
  ####################################################################################################
  ### derivation of correlation matrix - Bu 2019 paper cumulative model
  ####################################################################################################
  #### control condition
  ### page S5 matrix inverse CDL for cumulative model
  CDLinv=matrix(0,4,4)
  CDLinv[1,1]=cp.c[1]*(1-cp.c[1])
  CDLinv[2,2]=cp.c[2]*(1-cp.c[2])
  CDLinv[3,3]=cp.c[3]*(1-cp.c[3])
  CDLinv[2,1]=-1*cp.c[1]*(1-cp.c[1])
  CDLinv[3,2]=-1*cp.c[2]*(1-cp.c[2])
  CDLinv[4,3]=-1*cp.c[3]*(1-cp.c[3])
  CDLinv[1,4]=p.c[1]
  CDLinv[2,4]=p.c[2]
  CDLinv[3,4]=p.c[3]
  CDLinv[4,4]=p.c[4]
  
  ### page S10 matrix X for cumulative model from equation (S.4)
  X=matrix(0,4,4)
  X[1,1]=1
  X[1,4]=0
  X[2,2]=1
  X[2,4]=0
  X[3,3]=1
  X[3,4]=0
  
  ### page 6, diag(pi) for equation (4)
  invdiagpi=diag(1/p.c)
  CDLinvX=CDLinv%*%X
  F.c=t(CDLinvX)%*%invdiagpi%*%(CDLinvX)
  
  #### intervention condition
  ### page S5 matrix inverse CDL for cumulative model
  CDLinv=matrix(0,4,4)
  CDLinv[1,1]=cp.i[1]*(1-cp.i[1])
  CDLinv[2,2]=cp.i[2]*(1-cp.i[2])
  CDLinv[3,3]=cp.i[3]*(1-cp.i[3])
  CDLinv[2,1]=-1*cp.i[1]*(1-cp.i[1])
  CDLinv[3,2]=-1*cp.i[2]*(1-cp.i[2])
  CDLinv[4,3]=-1*cp.i[3]*(1-cp.i[3])
  CDLinv[1,4]=p.i[1]
  CDLinv[2,4]=p.i[2]
  CDLinv[3,4]=p.i[3]
  CDLinv[4,4]=p.i[4]
  
  ### page S10 matrix X for cumulative model from equation (S.4)
  X=matrix(0,4,4)
  X[1,1]=1
  X[1,4]=1
  X[2,2]=1
  X[2,4]=1
  X[3,3]=1
  X[3,4]=1
  
  ### page 6, diag(pi) for equation (4)
  invdiagpi=diag(1/p.i)
  CDLinvX=CDLinv%*%X
  F.i=t(CDLinvX)%*%invdiagpi%*%(CDLinvX)
  
  ####################################################################################################
  ### optimality criterion as a function of prop.i ### fixed budget
  ####################################################################################################
  prop.i=seq(0.01,0.99,by=0.01)
  c.opt=rep(0,length(prop.i))
  
  B=1					### budget
  c.c=1						### costs per subject in control
  c.i=ratio						### costs per subject in intervention
  N=B/(prop.i*c.i+(1-prop.i)*c.c)		### total number of subjects as function of costs
  n.i=N*prop.i					### number of subjects intervention
  n.c=N*(1-prop.i)				### number of subjectse control
  
  for (ii in 1:length(prop.i))
  {
    Fisher=n.i[ii]*F.i+n.c[ii]*F.c
    covmat=solve(Fisher)
    c.opt[ii]=covmat[4,4]
  }
  RE.c=min(c.opt)/c.opt
  output=cbind(prop.i,c.opt,RE.c)
  return(output)
  
}


####################################################################################################
####################################################################################################
####################################################################################################
### 5 categories
####################################################################################################
####################################################################################################
####################################################################################################

f.OD5=function(p1,p2,p3,p4,p5,OR,ratio)
{
  p.c=c(p1,p2,p3,p4,p5)
  cp.c=cumsum(p.c)
  cp.c=cp.c[cp.c<1]
  logit.cp.c=log(cp.c/(1-cp.c))
  logit.cp.i=logit.cp.c+log(OR)
  cp.i=exp(logit.cp.i)/(1+exp(logit.cp.i))
  p.i=rep(0,5)				### response probabilities intervention
  p.i[1]=cp.i[1]
  p.i[2]=cp.i[2]-cp.i[1]
  p.i[3]=cp.i[3]-cp.i[2]
  p.i[4]=cp.i[4]-cp.i[3]
  p.i[5]=1-cp.i[4]
  
  ####################################################################################################
  ### derivation of correlation matrix - Bu 2019 paper cumulative model
  ####################################################################################################
  #### control condition
  ### page S5 matrix inverse CDL for cumulative model
  CDLinv=matrix(0,5,5)
  CDLinv[1,1]=cp.c[1]*(1-cp.c[1])
  CDLinv[2,2]=cp.c[2]*(1-cp.c[2])
  CDLinv[3,3]=cp.c[3]*(1-cp.c[3])
  CDLinv[4,4]=cp.c[4]*(1-cp.c[4])
  CDLinv[2,1]=-1*cp.c[1]*(1-cp.c[1])
  CDLinv[3,2]=-1*cp.c[2]*(1-cp.c[2])
  CDLinv[4,3]=-1*cp.c[3]*(1-cp.c[3])
  CDLinv[5,4]=-1*cp.c[4]*(1-cp.c[4])
  CDLinv[1,5]=p.c[1]
  CDLinv[2,5]=p.c[2]
  CDLinv[3,5]=p.c[3]
  CDLinv[4,5]=p.c[4]
  CDLinv[5,5]=p.c[5]
  
  ### page S10 matrix X for cumulative model from equation (S.4)
  X=matrix(0,5,5)
  X[1,1]=1
  X[1,5]=0
  X[2,2]=1
  X[2,5]=0
  X[3,3]=1
  X[3,5]=0
  X[4,4]=1
  X[4,5]=0
  
  ### page 6, diag(pi) for equation (4)
  invdiagpi=diag(1/p.c)
  CDLinvX=CDLinv%*%X
  F.c=t(CDLinvX)%*%invdiagpi%*%(CDLinvX)
  
  #### intervention condition
  ### page S5 matrix inverse CDL for cumulative model
  CDLinv=matrix(0,5,5)
  CDLinv[1,1]=cp.i[1]*(1-cp.i[1])
  CDLinv[2,2]=cp.i[2]*(1-cp.i[2])
  CDLinv[3,3]=cp.i[3]*(1-cp.i[3])
  CDLinv[4,4]=cp.i[4]*(1-cp.i[4])
  CDLinv[2,1]=-1*cp.i[1]*(1-cp.i[1])
  CDLinv[3,2]=-1*cp.i[2]*(1-cp.i[2])
  CDLinv[4,3]=-1*cp.i[3]*(1-cp.i[3])
  CDLinv[5,4]=-1*cp.i[4]*(1-cp.i[4])
  CDLinv[1,5]=p.i[1]
  CDLinv[2,5]=p.i[2]
  CDLinv[3,5]=p.i[3]
  CDLinv[4,5]=p.i[4]
  CDLinv[5,5]=p.i[5]
  
  ### page S10 matrix X for cumulative model from equation (S.4)
  X[1,1]=1
  X[1,5]=1
  X[2,2]=1
  X[2,5]=1
  X[3,3]=1
  X[3,5]=1
  X[4,4]=1
  X[4,5]=1
  
  ### page 6, diag(pi) for equation (4)
  invdiagpi=diag(1/p.i)
  CDLinvX=CDLinv%*%X
  F.i=t(CDLinvX)%*%invdiagpi%*%(CDLinvX)
  
  ####################################################################################################
  ### optimality criterion as a function of prop.i ### fixed budget
  ####################################################################################################
  prop.i=seq(0.01,0.99,by=0.01)
  c.opt=rep(0,length(prop.i))
  
  B=1					### budget
  c.c=1						### costs per subject in control
  c.i=ratio						### costs per subject in intervention
  N=B/(prop.i*c.i+(1-prop.i)*c.c)		### total number of subjects as function of costs
  n.i=N*prop.i					### number of subjects intervention
  n.c=N*(1-prop.i)				### number of subjectse control
  
  for (ii in 1:length(prop.i))
  {
    Fisher=n.i[ii]*F.i+n.c[ii]*F.c
    covmat=solve(Fisher)
    c.opt[ii]=covmat[5,5]
  }
  
  RE.c=min(c.opt)/c.opt
  output=cbind(prop.i,c.opt,RE.c)
  return(output)
}


####################################################################################################
####################################################################################################
####################################################################################################
### 6 categories
####################################################################################################
####################################################################################################
####################################################################################################

f.OD6=function(p1,p2,p3,p4,p5,p6,OR,ratio){

  p.c=c(1/21,3/21,5/21,6/21,4/21,2/21)	### bell-shaped
  cp.c=cumsum(p.c)
  cp.c=cp.c[cp.c<1]
  logit.cp.c=log(cp.c/(1-cp.c))
  logit.cp.i=logit.cp.c+log(OR)
  cp.i=exp(logit.cp.i)/(1+exp(logit.cp.i))
  p.i=rep(0,6)				### response probabilities intervention
  p.i[1]=cp.i[1]
  p.i[2]=cp.i[2]-cp.i[1]
  p.i[3]=cp.i[3]-cp.i[2]
  p.i[4]=cp.i[4]-cp.i[3]
  p.i[5]=cp.i[5]-cp.i[4]
  p.i[6]=1-cp.i[5]

  ####################################################################################################
  ### derivation of correlation matrix - Bu 2019 paper cumulative model
  ####################################################################################################
  #### control condition
  ### page S5 matrix inverse CDL for cumulative model
  CDLinv=matrix(0,6,6)
  CDLinv[1,1]=cp.c[1]*(1-cp.c[1])
  CDLinv[2,2]=cp.c[2]*(1-cp.c[2])
  CDLinv[3,3]=cp.c[3]*(1-cp.c[3])
  CDLinv[4,4]=cp.c[4]*(1-cp.c[4])
  CDLinv[5,5]=cp.c[5]*(1-cp.c[5])
  CDLinv[2,1]=-1*cp.c[1]*(1-cp.c[1])
  CDLinv[3,2]=-1*cp.c[2]*(1-cp.c[2])
  CDLinv[4,3]=-1*cp.c[3]*(1-cp.c[3])
  CDLinv[5,4]=-1*cp.c[4]*(1-cp.c[4])
  CDLinv[6,5]=-1*cp.c[5]*(1-cp.c[5])
  CDLinv[1,6]=p.c[1]
  CDLinv[2,6]=p.c[2]
  CDLinv[3,6]=p.c[3]
  CDLinv[4,6]=p.c[4]
  CDLinv[5,6]=p.c[5]
  CDLinv[6,6]=p.c[6]
  
  ### page S10 matrix X for cumulative model from equation (S.4)
  X=matrix(0,6,6)
  X[1,1]=1
  X[1,6]=0
  X[2,2]=1
  X[2,6]=0
  X[3,3]=1
  X[3,6]=0
  X[4,4]=1
  X[4,6]=0
  X[5,5]=1
  X[5,6]=0
  
  
  ### page 6, diag(pi) for equation (4)
  invdiagpi=diag(1/p.c)
  CDLinvX=CDLinv%*%X
  F.c=t(CDLinvX)%*%invdiagpi%*%(CDLinvX)
  
  #### intervention condition
  ### page S5 matrix inverse CDL for cumulative model
  CDLinv=matrix(0,6,6)
  CDLinv[1,1]=cp.i[1]*(1-cp.i[1])
  CDLinv[2,2]=cp.i[2]*(1-cp.i[2])
  CDLinv[3,3]=cp.i[3]*(1-cp.i[3])
  CDLinv[4,4]=cp.i[4]*(1-cp.i[4])
  CDLinv[5,5]=cp.i[5]*(1-cp.i[5])
  CDLinv[2,1]=-1*cp.i[1]*(1-cp.i[1])
  CDLinv[3,2]=-1*cp.i[2]*(1-cp.i[2])
  CDLinv[4,3]=-1*cp.i[3]*(1-cp.i[3])
  CDLinv[5,4]=-1*cp.i[4]*(1-cp.i[4])
  CDLinv[6,5]=-1*cp.i[5]*(1-cp.i[5])
  CDLinv[1,6]=p.i[1]
  CDLinv[2,6]=p.i[2]
  CDLinv[3,6]=p.i[3]
  CDLinv[4,6]=p.i[4]
  CDLinv[5,6]=p.i[5]
  CDLinv[6,6]=p.i[6]
  
  ### page S10 matrix X for cumulative model from equation (S.4)
  X=matrix(0,6,6)
  X[1,1]=1
  X[1,6]=1
  X[2,2]=1
  X[2,6]=1
  X[3,3]=1
  X[3,6]=1
  X[4,4]=1
  X[4,6]=1
  X[5,5]=1
  X[5,6]=1
  
  ### page 6, diag(pi) for equation (4)
  invdiagpi=diag(1/p.i)
  CDLinvX=CDLinv%*%X
  F.i=t(CDLinvX)%*%invdiagpi%*%(CDLinvX)
  
  ####################################################################################################
  ### optimality criterion as a function of prop.i ### fixed budget
  ####################################################################################################
  prop.i=seq(0.01,0.99,by=0.01)
  c.opt=rep(0,length(prop.i))
  
  B=1					### budget
  c.c=1 					### costs per subject in control
  c.i=ratio					### costs per subject in intervention
  N=B/(prop.i*c.i+(1-prop.i)*c.c)	### total number of subjects as function of costs
  n.i=N*prop.i				### number of subjects intervention
  n.c=N*(1-prop.i)				### number of subjectse control
  
  for (ii in 1:length(prop.i))
  {
    Fisher=n.i[ii]*F.i+n.c[ii]*F.c
    covmat=solve(Fisher)
    c.opt[ii]=covmat[6,6]
  }
  
  
  RE.c=min(c.opt)/c.opt
  output=cbind(prop.i,c.opt,RE.c)
  
}
  
  
####################################################################################################
####################################################################################################
####################################################################################################
### 7 categories
####################################################################################################
####################################################################################################
####################################################################################################

f.OD7=function(p1,p2,p3,p4,p5,p6,p7,OR,ratio){
  
  p.c=c(p1,p2,p3,p4,p5,p6,p7)
  cp.c=cumsum(p.c)
  cp.c=cp.c[cp.c<1]
  logit.cp.c=log(cp.c/(1-cp.c))
  logit.cp.i=logit.cp.c+log(OR)
  cp.i=exp(logit.cp.i)/(1+exp(logit.cp.i))
  p.i=rep(0,7)				### response probabilities intervention
  p.i[1]=cp.i[1]
  p.i[2]=cp.i[2]-cp.i[1]
  p.i[3]=cp.i[3]-cp.i[2]
  p.i[4]=cp.i[4]-cp.i[3]
  p.i[5]=cp.i[5]-cp.i[4]
  p.i[6]=cp.i[6]-cp.i[5]
  p.i[7]=1-cp.i[6]
  
  ####################################################################################################
  ### derivation of correlation matrix - Bu 2019 paper cumulative model
  ####################################################################################################
  #### control condition
  ### page S5 matrix inverse CDL for cumulative model
  CDLinv=matrix(0,7,7)
  CDLinv[1,1]=cp.c[1]*(1-cp.c[1])
  CDLinv[2,2]=cp.c[2]*(1-cp.c[2])
  CDLinv[3,3]=cp.c[3]*(1-cp.c[3])
  CDLinv[4,4]=cp.c[4]*(1-cp.c[4])
  CDLinv[5,5]=cp.c[5]*(1-cp.c[5])
  CDLinv[6,6]=cp.c[6]*(1-cp.c[6])
  CDLinv[2,1]=-1*cp.c[1]*(1-cp.c[1])
  CDLinv[3,2]=-1*cp.c[2]*(1-cp.c[2])
  CDLinv[4,3]=-1*cp.c[3]*(1-cp.c[3])
  CDLinv[5,4]=-1*cp.c[4]*(1-cp.c[4])
  CDLinv[6,5]=-1*cp.c[5]*(1-cp.c[5])
  CDLinv[7,6]=-1*cp.c[6]*(1-cp.c[6])
  CDLinv[1,7]=p.c[1]
  CDLinv[2,7]=p.c[2]
  CDLinv[3,7]=p.c[3]
  CDLinv[4,7]=p.c[4]
  CDLinv[5,7]=p.c[5]
  CDLinv[6,7]=p.c[6]
  CDLinv[7,7]=p.c[7]
  
  ### page S10 matrix X for cumulative model from equation (S.4)
  X=matrix(0,7,7)
  X[1,1]=1
  X[1,7]=0
  X[2,2]=1
  X[2,7]=0
  X[3,3]=1
  X[3,7]=0
  X[4,4]=1
  X[4,7]=0
  X[5,5]=1
  X[5,7]=0
  X[6,6]=1
  X[6,7]=0
  
  ### page 6, diag(pi) for equation (4)
  invdiagpi=diag(1/p.c)
  CDLinvX=CDLinv%*%X
  F.c=t(CDLinvX)%*%invdiagpi%*%(CDLinvX)
  
  #### intervention condition
  ### page S5 matrix inverse CDL for cumulative model
  CDLinv=matrix(0,7,7)
  CDLinv[1,1]=cp.i[1]*(1-cp.i[1])
  CDLinv[2,2]=cp.i[2]*(1-cp.i[2])
  CDLinv[3,3]=cp.i[3]*(1-cp.i[3])
  CDLinv[4,4]=cp.i[4]*(1-cp.i[4])
  CDLinv[5,5]=cp.i[5]*(1-cp.i[5])
  CDLinv[6,6]=cp.i[6]*(1-cp.i[6])
  CDLinv[2,1]=-1*cp.i[1]*(1-cp.i[1])
  CDLinv[3,2]=-1*cp.i[2]*(1-cp.i[2])
  CDLinv[4,3]=-1*cp.i[3]*(1-cp.i[3])
  CDLinv[5,4]=-1*cp.i[4]*(1-cp.i[4])
  CDLinv[6,5]=-1*cp.i[5]*(1-cp.i[5])
  CDLinv[7,6]=-1*cp.i[6]*(1-cp.i[6])
  CDLinv[1,7]=p.i[1]
  CDLinv[2,7]=p.i[2]
  CDLinv[3,7]=p.i[3]
  CDLinv[4,7]=p.i[4]
  CDLinv[5,7]=p.i[5]
  CDLinv[6,7]=p.i[6]
  CDLinv[7,7]=p.i[7]
  
  ### page S10 matrix X for cumulative model from equation (S.4)
  X=matrix(0,7,7)
  X[1,1]=1
  X[1,7]=1
  X[2,2]=1
  X[2,7]=1
  X[3,3]=1
  X[3,7]=1
  X[4,4]=1
  X[4,7]=1
  X[5,5]=1
  X[5,7]=1
  X[6,6]=1
  X[6,7]=1
  
  ### page 6, diag(pi) for equation (4)
  invdiagpi=diag(1/p.i)
  CDLinvX=CDLinv%*%X
  F.i=t(CDLinvX)%*%invdiagpi%*%(CDLinvX)
  
  ####################################################################################################
  ### optimality criterion as a function of prop.i ### fixed budget
  ####################################################################################################
  prop.i=seq(0.01,0.99,by=0.01)
  c.opt=rep(0,length(prop.i))
  
  B=1					### budget
  c.c=1 					### costs per subject in control
  c.i=ratio					### costs per subject in intervention
  N=B/(prop.i*c.i+(1-prop.i)*c.c)	### total number of subjects as function of costs
  n.i=N*prop.i				### number of subjects intervention
  n.c=N*(1-prop.i)				### number of subjectse control
  
  for (ii in 1:length(prop.i))
  {
    Fisher=n.i[ii]*F.i+n.c[ii]*F.c
    covmat=solve(Fisher)
    c.opt[ii]=covmat[7,7]
  }
  
  RE.c=min(c.opt)/c.opt
  output=cbind(prop.i,c.opt,RE.c)
  return(output)
}
