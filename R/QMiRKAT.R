QMiRKAT <-
function(r,ker,model){
	if(model =='gaussian'){
		Q = as.numeric((1/2)*(1/var(r))*t(r)%*%ker%*%r)
	}else{
		Q <- as.numeric((1/2)*t(r)%*%ker%*%r)
	}
	Q	
}
