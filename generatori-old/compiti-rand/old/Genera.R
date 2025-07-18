# Esegui lo script Python per generare il documento Rmd
library(reticulate)
iii <- 1
for (iii in 1:5){
    py_run_file("save_ex.py")
    reticulate::source_python('Random-ex2.py')
    dir()
    # Compila il documento Rmd in HTML
    rm(list=setdiff(ls(),"iii"))
    out <- paste0("compito_com_",iii,".html")
    inp <- paste0("compito_com_",iii,".Rmd")
    system(paste("cp","compito_com.Rmd",inp))
    rmarkdown::render("compito_com.Rmd", output_file = out,envir = globalenv())
    detach(RGX)
    out <- paste0("compito_sol_",iii,".html")
    inp <- paste0("compito_sol_",iii,".Rmd")
    system(paste("cp","compito_sol.Rmd",inp))
    rmarkdown::render("compito_sol.Rmd", output_file = out,envir = globalenv())
 detach(RGX)
    
    out <- paste0("compito_sol_",iii,".html")
    rstudioapi::viewer(out)
#    rstudioapi::viewer("www/compito_com.html")
}
