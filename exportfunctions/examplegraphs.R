require(gMCP)

source('~/newLife/code/gMCP/exportfunctions/functions.R')
graphGUI()

e <- environment()
sapply(c('full4.Rd','base4.Rd','cycle4.Rd','claw4.Rd'),load,env=e)


exportUDgraph(full4,file='full4.tex')
exportUDgraph(cycle4,file='cycle4.tex')
exportUDgraph(base4,file='base4.tex')
exportUDgraph(claw4,file='claw4.tex')


system('pdflatex full4.tex') 
system('pdflatex cycle4.tex')
system('pdflatex base4.tex')
system('pdflatex claw4.tex') 

