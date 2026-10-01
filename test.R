install.packages("shiny")

getwd()
list.files()


#dir.create("app")
#file.rename("app.R", "app/app.R")
library(shiny)
shinylive::export("app", "docs")
file.create("docs/.nojekyll")
httpuv::runStaticServer("docs")


shinylive::export("app", "docs")
