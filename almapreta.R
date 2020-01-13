baixar_paginas_almapreta <- function(diretorio = ".",caderno = "realidade", paginas = 10){
  
  url <- paste0("https://almapreta.com/editorias/",caderno,"?start=")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}

b <- baixar_paginas_almapreta(diretorio = "pagina")

#diretorio <- "pagina" e "noticias"

#arquivos <- list-files

#.x <- arquivos[NUMERO]

ler_paginas_almapreta <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//h2[@itemprop="name"]/a') %>% 
        xml2::xml_attr("href") %>% 
        paste0("https://almapreta.com",.)
  
  headline <- x %>% 
    xml2::xml_find_all('//div[@class="page-header"]//a') %>% 
    xml2::xml_text()
  
  descricao <- x %>% 
    xml2::xml_find_all('//strong') %>% 
    xml2::xml_text()
  
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//dd[@class="published"]/time') %>% 
    xml2::xml_attr("datetime")
  
  data_modificacao <- x %>% 
    xml2::xml_find_all('//dd[@class="modified"]/time') %>% 
    xml2::xml_attr("datetime")
  
  tibble::tibble(headline,descricao,data_publicacao,data_modificacao,url)
    
  }),NULL)) %>% 
    dplyr::distinct()
  
}

 
p <- ler_paginas_almapreta(diretorio = "pagina")

baixar_noticias_almapreta <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"(?<=editorias/.{1,20}/).+") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_almapreta_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}

alma<-ler_paginas_almapreta()

baixar_noticias_almapreta(p$url,"noticias")



ler_noticias_almapreta <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
         xml2::xml_find_all('//div[@class="page-header"]/h2') %>% 
         xml2::xml_text() 
    
   texto <- x %>% 
      xml2::xml_find_all('//div[@itemprop="articleBody"]//p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//dd[@class="published"]/time') %>% 
     xml2::xml_attr("datetime")
   
   data_modificacao <- x %>% 
     xml2::xml_find_all('//dd[@class="modified"]/time') %>% 
     xml2::xml_attr("datetime")
   
   autor <- x %>% 
     xml2::xml_find_first('//span[@itemprop="name"]') %>% 
     xml2::xml_text()
   
   tibble::tibble(headline,texto,data_publicacao,data_modificacao,autor)
   
  }),NULL))
    
}


noticias <- ler_noticias_almapreta(arquivos = arquivos)
