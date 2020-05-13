

rmarkdown::render("Rmd/mtcarsparamtest.Rmd",
                  params = list(xaxis = "wt",
                                yaxis = "disp"))
