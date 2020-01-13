baixar_paginas_reporterbrasil <- function(diretorio = ".",caderno = "reportagens", paginas = 10){
  
  url <- paste0("https://reporterbrasil.org.br/categorias/noticias/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}

p <- baixar_paginas_reporterbrasil(diretorio = "pagina")


ler_paginas_reporterbrasil <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//h2/a') %>% 
        xml2::xml_attr("href")
  
  headline <- x %>% 
    xml2::xml_find_all('//h2/a') %>% 
    xml2::xml_text()

  descricao <- x %>% 
    xml2::xml_find_all('//article/div/following-sibling::text()') %>% 
    xml2::xml_text()
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//span[@class="date"]') %>% 
    xml2::xml_text() %>% 
    lubridate::dmy()
  
  tibble::tibble(headline,descricao,data_publicacao,url)
    
  }),NULL))
  
}

rtb <- ler_paginas_reporterbrasil(arquivos = arquivos)  


baixar_noticias_reporterbrasil <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_reporterbrasil_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}



baixar_noticias_reporterbrasil(rtb$url,"noticias")



ler_noticias_reporterbrasil <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
         xml2::xml_find_all('//div[@id="full-img"]/div[@id="post-cover"]') %>% 
         xml2::xml_text()
    
    headline2 <- x %>% 
      xml2::xml_find_all('//h1[@class="post-title"]') %>% 
      xml2::xml_text()
    
    
    headline <- c(headline2,headline)
    
    descricao <- x %>% 
      xml2::xml_find_all('//div[@class="excerpt"]/p') %>% 
      xml2::xml_text()
    
   texto <- x %>% 
      xml2::xml_find_all('//div[@class="entry-content"]//p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   autor <- x %>% 
     xml2::xml_find_all('//div[@class="metadata"]/span[@class="author"]') %>% 
     xml2::xml_text()
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//span[@class="date"]') %>% 
     xml2::xml_text()%>% 
     lubridate::dmy()
   

   
   tibble::tibble(headline,texto,autor,data_publicacao)
   
  }),NULL))
    
}


noticias <- ler_noticias_reporterbrasil(arquivos = arquivos)
