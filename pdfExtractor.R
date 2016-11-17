library(pdftools)
extractedtext=pdftools::pdf_text("C:/Users/imtiaz/Documents/pdf.pdf")
content=paste(extractedtext, collapse = '')
write(content,"C:/Users/imtiaz/Documents/data.txt")

