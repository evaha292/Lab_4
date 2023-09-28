#Installed packages: library(stringr), library(ggplot2), library(gridExtra)

  
#' Linear Regression
#'
#' @name linreg 
#' 
#' 
#' @field formula defines a formula of how we want to model the relationship between different variables. 
#' @field data It is a dataframe containing multiple variables in the columns and different observations of those variables in the rows.  
#' @field coeff Regressions coefficients. 
#' @field fitted The fitted values.  
#' @field residuals The residuals.  
#' @field dof The degrees of freedom.  
#' @field rvariance The residual variance. 
#' @field varcoeff The variance of the regression coefficients. 
#' @field tvalues The t-values for each coefficient.  
#' 
#' 
#' @section Methods: 
#' print(): print out the coefficients and coefficient names  \cr
#' \cr
#' plot(): outputs 2 plots 1) Residuls vs Fitted Values and 2) Standardized Residuals  vs Fitted Values \cr
#' \cr
#' resid()  returns the vector of the residuals \cr
#' \cr 
#' pred() returns the predicted values \cr
#' \cr
#' coef()  returns the coefficients as a named vector \cr
#' \cr
#' summary() presents the coefficients with their standard error, t-value and p-value as well as the estimate of ˆσ and the degrees of freedom in the model.\cr
#' \cr
#' 
#' @return A class with multiple fields and methods to use on. 
#' 
#' @import stringr ggplot2 gridExtra
#' @export linreg 
#' 
#' 

  linreg<-setRefClass("linreg",
                      fields = list(formula="formula",data="data.frame",d = "character",X="matrix",y="vector",coeff="matrix",fitted="matrix",
                                    residuals="matrix", dof="numeric", rvariance="matrix", varcoeff="matrix", tvalues="matrix",pvalues="matrix"),
                      methods=list(
                        initialize = function(formula,data){
                          #.self$formula <<-formula
                          #.self$data <<- data
                          formula <<- formula 
                          d<<-deparse(substitute(data))
                          X <<- model.matrix(formula,data)
                          y <<- data[[all.vars(formula)[1]]]
                          coeff <<- solve((t(X)%*%X))%*%t(X)%*%y 
                          fitted <<- X%*%coeff
                          residuals <<- y - fitted
                          e<- as.character(formula)
                          e<-as.vector(e)
                          e<-unlist(strsplit(e[3],split = " "))
                          e<-e[e!="+"]
                          dof <<- nrow(data)-length(e)-1
                          rvariance <<- (t(residuals)%*%residuals)/dof
                          varcoeff <<- as.numeric(rvariance)* solve(t(X)%*%X)
                          tvalues <<- coeff/sqrt(diag(varcoeff))
                          f<- abs(tvalues)
                          pvalues <<- 2*(pt(f,dof,lower.tail=FALSE))
                          #how to find p-values using pt()?Is that something that needs to be calculated?  
                        },
                        print = function(){
                          #a <- as.vector(coeff)
                          #names(a)<-row.names(coeff)
                          e<- as.character(formula)
                          e<-paste(e[2],"~",e[3])
                          c<- stringr::str_glue("linreg(formula = {e}, data = {d})")
                          #b<-list(Call = c , Coefficients = a)
                          #stringr::str_glue("Call:","{c}",.sep = "\n")
                          a<-setNames(coeff[1:nrow(coeff)],nm = rownames(coeff))
                          cat("Call:\n")
                          cat(c)
                          cat("\n\n")
                          cat("\nCoefficients:\n")
                          print.default(a)
                        },
                        plot = function(){
                          a<- as.data.frame(residuals)
                          b<- as.data.frame(fitted)
                          c<-cbind(a,b)
                          names(c)<-c("Residuals","Fitted")
                          e<- as.character(formula)
                          e<-paste(e[2],"~",e[3])
                          f<- stringr::str_glue("Fitted values linreg({e})", .sep = "\n")
                          z<-sqrt(abs(scale(residuals)))
                          plot1<-ggplot2::ggplot(c,aes(Fitted,Residuals))+geom_point()+stat_summary(fun = "median", geom = "line", aes(group = 1), color = "red")+labs(title="Residuals vs Fitted", x = f)
                          plot2<-ggplot2::ggplot(c,aes(Fitted,z))+geom_point()+stat_summary(fun = "mean", geom = "line", aes(group = 1), color = "red")+labs(title="Standardized Residuals vs Fitted", x = f, y = "Standardized Residuals")
                          gridExtra::grid.arrange(plot1,plot2,ncol=2)
                        },
                        resid = function(){
                          return(as.vector(residuals))
                        },
                        pred = function(){
                          return(fitted)
                        },
                        coef = function(){
                          a <- as.vector(coeff)
                          names(a)<-row.names(coeff)
                          return(a)
                        },
                        summary = function(){
                          Std_error <- sqrt(diag(varcoeff))
                          #names(a)<-row.names(coeff)
                          e<- as.character(formula)
                          e<-paste(e[2],"~",e[3])
                          c<-stringr::str_glue("linreg(formula = {e}, data = {d})")
                          a <- as.vector(tvalues)
                          names(a)<-row.names(tvalues)
                          b <- as.vector(pvalues)
                          names(b)<-row.names(pvalues)
                          d<<-stringr::str_glue("Residual standard error: {sqrt(rvariance)} on {dof} degrees of freedom ")
                          
                          i <- ifelse(pvalues > 0.1, " ",(ifelse(pvalues > 0.05, " . ",(ifelse(pvalues > 0.01, "*",(ifelse(pvalues > 0.001, "**","***")))))))
                          x<-data.frame(coeff,Std_error,tvalues,pvalues)
                          x<-cbind(x,i)
                          x$p_values<-paste0(x$pvalues,x$i,sep = " ")
                          x<-x[-c(4,5)]
                          #data.frame(Std_Coefficients,tvalues,pvalues)
                          #stringr::str_glue("Standard residual:{sqrt(rvariance)} and DegreeOfFreedom: {dof}")
                          #b<-list(Call = c , Std_Coefficients = a, tvalue = tvalues, p_value = pvalues ,StandardDev = sqrt(rvariance), DegreeOfFreedom = dof)
                          cat("Call:\n")
                          cat(c)
                          cat("\n\n")
                          cat("Coefficients:\n")
                          print.data.frame(x)
                          cat("\n\n")
                          cat(d)
                        }
                      )
                    )
  
            
  
  #l1<-linreg(formula = Petal.Length~Species + Petal.Width, data = iris)
  l1<-linreg(formula = Petal.Length~Sepal.Width + Sepal.Length, data = iris)
  
  l1<-linreg(formula = Petal.Length~Species + Petal.Width, data = iris)
  
