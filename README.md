This package constitutes an interactive R problem set based on the RTutor package (https://github.com/skranz/RTutor).

This problem set analyses loans issued by <a href="https://www.lendingclub.com/" target = "_blank">Lending Club</a> and their probability of default. Lending Club acts as intermediary connecting borrowers and investors by helping people to access or grant credits via peer-to-peer lending.

The focal point of this problem set is to present the application of machine learning in an accessible and reproducible manner. Therefore, we will not focus on experiencing complex statistical formulae or well-established credit default research.

Concerning the problem set most exercises depend to a certain extent on each other. As tasks become trickier and more challenging or may require some previously gained knowledge, it is highly recommended to solve the problem set in its predetermined order. Possessing prior knowledge of R is not mandatory, yet experience with R or other equivalent statistical software, as well as a genuine interest in econometrics would be of advantage. Since most code will be provided, lots of challenging quizzes have been added to make up for the lack of producing written code.


## 1. Installation

RTutor and this package is hosted on Github. To install everything, run the following code in your R console.
```s
if (!require(devtools))
  install.packages("devtools")
source_gist("gist.github.com/skranz/fad6062e5462c9d0efe4")
install.rtutor(update.github=TRUE)

devtools::install_github("rotterp/RTutorMachineLearningAndCreditDefault", upgrade_dependencies=FALSE)
```

## 2. Show and work on the problem set
To start the problem set first create a working directory in which files like the data sets and your solution will be stored. Then adapt and run the following code.
```s
library(RTutorMachineLearningAndCreditDefault)

# Adapt your working directory to an existing folder
setwd("C:/problemsets/RTutorMachineLearningAndCreditDefault")
# Adapt your user name
run.ps(user.name="YOURNAME", package="RTutorMachineLearningAndCreditDefault",
       load.sav=TRUE, sample.solution=FALSE)
```
If everything works fine, a browser window should open, in which you can start exploring the problem set.
