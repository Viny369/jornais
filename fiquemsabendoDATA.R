baixar_paginas_fiquemsabendo <- function(diretorio = ".",caderno = "seguranca", paginas = 10){
  
  url <- paste0("https://fiquemsabendo.com.br/category/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}

p <- baixar_paginas_fiquemsabendo(diretorio = "pagina")


ler_paginas_fiquemsabendo <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//h3[@class="gdlr-blog-title"]/a') %>% 
        xml2::xml_attr("href")
  
  headline <- x %>% 
    xml2::xml_find_all('//h3[@class="gdlr-blog-title"]/a') %>% 
    xml2::xml_text()
  
  autor <- x %>% 
    xml2::xml_find_all('//div[@class="gdlr-blog-info gdlr-info"]/div[@class="blog-info blog-author"]') %>% 
    xml2::xml_text()
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//div[@class="blog-info blog-date"]//a') %>% 
    xml2::xml_text()
  
  tibble::tibble(headline,data_publicacao,autor,url)
    
  }),NULL))
  
}

fqs <- ler_paginas_fiquemsabendo(arquivos = arquivos)  


baixar_noticias_fiquemsabendo <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_fiquemsabendo_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}



baixar_noticias_fiquemsabendo(fqs$url,"noticias")



ler_noticias_fiquemsabendo <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
         xml2::xml_find_all('//h1[@class="gdlr-page-title"]') %>% 
         xml2::xml_text()
    
   texto <- x %>% 
      xml2::xml_find_all('//div[@class="gdlr-blog-content"]//p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//div[@class="blog-info blog-date"]/a') %>% 
     xml2::xml_text()
   
   autor <- x %>% 
     xml2::xml_find_first('//div[@class="blog-info blog-author"]/a') %>% 
     xml2::xml_text()
   
   tibble::tibble(headline,texto,data_publicacao,autor)
   
  }),NULL))
    
}


noticias <- ler_noticias_fiquemsabendo(arquivos = arquivos)
