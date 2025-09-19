# "boutliers: R package of outlier detection and influence diagnostics for meta-analysis"
#  by Hisashi Noma, Kazushi Maruo and Masahiko Gosho

#  R example code for implementing the influence diagnostics using the "boutliers" package

# The "boutliers" package
# CRAN webpage: https://doi.org/10.32614/CRAN.package.boutliers

###

install.packages("boutliers")		# install the "boutliers" package from CRAN
install.packages("meta")			# install the "meta" package from CRAN

library("boutliers")
library("meta")

##

# Conducting meta-analysis and creating a forest plot

data(SMT)

meta01 <- metacont(n1, m1, s1, n2, m2, s2, studlab = Source, sm = "MD",
			   comb.random = TRUE, comb.fixed = FALSE, data=SMT)

forest(meta01, digits.mean = 1, digits.sd   = 1, rightcols = c("effect", "ci"),
	   col.square = "#6495ED", col.square.lines = "darkblue",  
       col.diamond = "blue", col.diamond.lines = "darkblue")

##

# Outlier detection and influence diagnostics by "boutliers" package

edat1 <- escalc(m1i=m1,sd1i=s1,n1i=n1,m2i=m2,sd2i=s2,n2i=n2,measure="MD",data=SMT)
# Data editing by "metafor" package tool

STR(yi,vi,data=edat1,B=2000)				# Studentized residuals; random-effects model
STR(yi,vi,data=edat1,method="FE",B=2000)	# Studentized residuals; fixed-effect model

VRATIO(yi,vi,data=edat1,B=2000)				# Variance ratio statistics (VRATIO, TRATIO)

LRT(yi,vi,data=edat1,B=2000)				# Likelihood-ratio test; random-effects model
LRT(yi,vi,data=edat1,model="FE",B=2000)		# Likelihood-ratio test; fixed-effect model


