library(dplyr)
library(shiny)
library(tidyr)
library(stringdist)

# Define UI
ui <- fluidPage(
  titlePanel("OpenAlex Title Search"),
  
  # Application content
  mainPanel(width = 12,
    
    textAreaInput("id_title_search",
                  "Title Search", 
                  width = '90%'
                  ),
    
    actionButton("find_titles", "Load my titles", class = "btn-info"),

    DT::dataTableOutput("pop_table", width = "100%"),
    verbatimTextOutput("text")
    
    )
)

server <- function(input, output) {
    
  observeEvent(input$find_titles, {
    
    title_input <- input$id_title_search
    
    new <- openalexR::oa_fetch(
      identifier = NULL,
      entity = "works",
      title.search = title_input
    )
    
    new_df <- new %>% 
      unnest(author) %>% 
      filter(author_position == "first") %>%
      rename(first_author = au_display_name) %>%
      select(doi, title = display_name, first_author, journal = so, url)
    
    result_list <- list()
    
    if (nrow(new_df) > 1){
      for (i in 1:nrow(new_df)){
      titles <- new_df[["title"]][[i]]
      distance <- stringdistmatrix(title_input, titles, method = "lv")
      result_list[[i]] <- data.frame(title = titles, distance = distance)
      }
    
    result_df <- do.call(rbind, result_list)
    
    results_order <- result_df %>% 
      arrange(distance) %>% 
      slice_head(n = 5) %>% 
      left_join(new_df, by = "title") %>% 
      mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)) %>% 
      mutate(link = ifelse(is.na(doi), "Unavailable", link))
    
    
    results_order$link <- paste0("<a href='",results_order$link, "' target='_blank'>",results_order$title,"</a>")
    
    results_final <- results_order %>%
      mutate(link = ifelse(is.na(doi), "No Link", link)) %>% 
      select(title, first_author, journal, link)
    } else{
      
      results_order <- new_df %>%
        mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url))
      
      results_order$link <- paste0("<a href='",results_order$link, "' target='_blank'>",results_order$title,"</a>")
      
      results_final <- results_order %>%
        mutate(link = ifelse(is.na(doi), "No Link", link)) %>% 
        select(title, first_author, journal, link)
      
    }
    
    
    output$pop_table <- DT::renderDataTable({

      DT::datatable(
        results_final,
        selection = "multiple",
        rownames = FALSE,
        escape = FALSE,
        # extensions = c('Buttons'),
        options = list(
          language = list(
            zeroRecords = "No records found",
            emptyTable = "No records found"),
          deferRender = FALSE,
          scrollY = 600,
          scrollX = 100,
          scroller = TRUE
          
        )
        
      )
    })
    observeEvent(input$pop_table_rows_selected, {
      selected_rows <- input$pop_table_rows_selected
      
    })

    output$text <- renderText({ 
      
      selected_rows <- input$pop_table_rows_selected
      
      results_selected <- results_final[selected_rows, ]
      
      paste("Citations Selected: ", results_selected)
      
    })
    
  })
  
  
}


# Run the application
shinyApp(ui, server)
