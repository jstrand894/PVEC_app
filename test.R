install.packages("shiny")

getwd()
list.files()


#dir.create("app")
#file.rename("app.R", "app/app.R")
shinylive::export("app", "docs")
file.create("docs/.nojekyll")
httpuv::runStaticServer("docs")
