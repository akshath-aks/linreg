linreg<-function(formula,data){
  X<-model.matrix(formula,data)
  y<-data[,all.vars(formula)[1]]
  B_h<-solve(t(X)%*%X)%*%t(X)%*%y
  # B_h<-as.vector(B_h)
  y_h<-X%*%B_h
  e<-y-y_h
  df<-nrow(X)-nrow(B_h)
  e_var<-(t(e)%*%e)/df
  B_h_var<-c(e_var)*diag(solve(t(X)%*%X))
  t_value<-B_h/sqrt(B_h_var)
  
  B_h<-as.vector(B_h)
  names(B_h)<-colnames(X)
  linreg_class<-setRefClass('linreg_class',fields=list(B_h='vector',e='matrix',
                                           e_var='matrix',
                                           B_h_var='vector',
                                           t_value='matrix'
                                           ))
  linreg_obj<-linreg_class$new(B_h=B_h,e=e,e_var=e_var,B_h_var=B_h_var,t_value=t_value)
  return(linreg_obj)
}
linreg_call_func<-linreg(formula = Petal.Length ~ Species, data = iris)
linreg_obj$B_h
