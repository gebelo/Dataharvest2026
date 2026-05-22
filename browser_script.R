# run this line in terminal to launch a browser instance:
# mac:
# /Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --remote-debugging-port=9222 --user-data-dir="/tmp/chrome_debug_profile"

#pc
#"C:\Program Files\Google\Chrome\Application\chrome.exe" --remote-debugging-port=9222 --user-data-dir="C:\chrome_debug_profile"



library(tidyverse)
#install.packages("chromote")
library(chromote)
#install.packages("rvest")
library(rvest)

rc <- ChromeRemote$new(host = "localhost", port = 9222)
cm <- Chromote$new(browser = rc)

b <- cm$new_session()
b$go_to("https://www.accessdata.fda.gov/scripts/cber/CFAppsPub/")


b$Runtime$evaluate(expression = "
  var inputField = document.querySelector('input[name*=\"Establishment\"]');
  if (inputField) {
    inputField.value = '%';
  } else {
    console.error('Could not find the Establishment Name field.');
  }
")

b$Runtime$evaluate(expression = "
  var selectEl = document.querySelector('select[name*=\"EstablishmentStatus\"]');
  
  if (selectEl) {
    selectEl.value = 'ACTIVE';
    selectEl.dispatchEvent(new Event('change'));
    console.log('Successfully set dropdown to Active');
  } else {
    console.error('Could not find the EstablishmentStatus dropdown.');
  }
")

b$Runtime$evaluate(expression = "
  var selectC = document.querySelector('select[name*=\"Country\"]');
  
  if (selectC) {
    selectC.value = 'US';
    select.dispatchEvent(new Event('change'));
    console.log('Successfully set dropdown to Active');
  } else {
    console.error('Could not find the EstablishmentStatus dropdown.');
  }
")

b$Runtime$evaluate(expression = "
  var selectET = document.querySelector('select[name*=\"EstablishmentType\"]');
  
  if (selectET) {
    selectET.value = '3';
    select.dispatchEvent(new Event('change'));
    console.log('Successfully set dropdown to Active');
  } else {
    console.error('Could not find the EstablishmentStatus dropdown.');
  }
")

b$Runtime$evaluate(expression = "
  var selectN = document.querySelector('select[name*=\"nrecords\"]');
  
  if (selectN) {
    selectN.value = '100';
    selectEl.dispatchEvent(new Event('change'));
    console.log('Successfully set dropdown to Active');
  } else {
    console.error('Could not find the EstablishmentStatus dropdown.');
  }
")


b$Runtime$evaluate(expression = "
  var submitBtn = document.querySelector('[name=\"SubmitButton\"]');
  
  if (submitBtn) {
    submitBtn.click();
    console.log('SubmitButton successfully clicked!');
  } else {
    console.error('Could not find an element with name=\"SubmitButton\"');
  }
")

# IMPORTANT: Pause R for a few seconds to let the search results load in Chrome

Sys.sleep(3)

# 1. Initialize an empty list and a tracking counter
all_pages_list <- list()
page_counter <- 1
has_next_page <- TRUE

message("Starting loop...")


while (has_next_page) {
  message(paste("Processing Page:", page_counter))
  
  # -------------------------------------------------------------
  # STEP 1: CAPTURE AND PARSE HTML (YOUR EXACT CODE)
  # -------------------------------------------------------------
  doc <- b$DOM$getDocument() # grab page and filter down to the html
  html_list <- b$DOM$getOuterHTML(nodeId = doc$root$nodeId) 
  fda_html <- html_list$outerHTML
  parsed_page <- read_html(fda_html)
  
  #get the table with the relevent data
  raw_table <- parsed_page %>%
    html_element("table.tbl") %>%
    html_table()
  
  #get the first column as a list
  first_column_nodes <- parsed_page %>%
    html_elements("table.tbl td:first-child a") 
  
  #pull out the urls from the first column as a list
  urls <- first_column_nodes %>%
    html_attr("href")
  
  #get the text from the first column as a list
  text <- first_column_nodes %>%
    html_text()
  
  #create a table out of those lists
  url_data <- tibble(
    text = text,
    url = urls
  )
  
  #create a final table by merging those two columns with the main table
  final_table <- raw_table %>%
    bind_cols(url_data)
  
  # Store this individual page's table inside our tracker list
  all_pages_list[[page_counter]] <- final_table
  
  
  # -------------------------------------------------------------
  # STEP 2: TARGET THE 'Display next' ID AND CLICK IT
  # -------------------------------------------------------------
  js_click_result <- b$Runtime$evaluate(expression = "
    var nextBtn = document.getElementById('Display next');
    if (nextBtn) {
      nextBtn.click();
      true;  // Tells R the button was found and clicked
    } else {
      false; // Tells R the button doesn't exist anymore
    }
  ")
  
  # Extract the true/false logical result from JavaScript
  button_was_clicked <- js_click_result$result$value
  
  
  # -------------------------------------------------------------
  # STEP 3: REPEAT OR BREAK THE LOOP
  # -------------------------------------------------------------
  if (button_was_clicked) {
    page_counter <- page_counter + 1
    
    # CRITICAL: Pause for 4 seconds to give Chrome time to pull 
    # the next page down from the FDA servers before R tries to parse again
    Sys.sleep(4) 
  } else {
    message("Finished! 'Display next' button is no longer on the page.")
    has_next_page <- FALSE
  }
}

# -------------------------------------------------------------
# STEP 4: COMBINE ALL COLLECTED TABLES INTO A SINGLE DATA FRAME
# -------------------------------------------------------------
master_results_table <- bind_rows(all_pages_list)


#mutate the URL to add the prefix

final_table<-master_results_table%>%
  mutate(full_url=paste("https://www.accessdata.fda.gov/scripts/cber/CFAppsPub/",url,sep=""))

write.csv(final_table,"finaltable.csv")


# 1. Create an empty list to store the detailed data from each page
details_list <- list()

# 2. Get the total number of URLs to scrape for the progress monitor
total_urls <- nrow(final_table)

detailed_entities <- data.frame(category=character(),
                                value=character(),
                                facility_id=character())


message(paste("Starting deep scrape of", total_urls, "URLs..."))

#for (i in 1:total_urls) {
for (i in 1:10) {
  # Get the current URL
  current_url <- final_table$full_url[i]
  
  # Optional: Print progress so you know it hasn't frozen
  message(paste0("Scraping page ", i, " of ", total_urls, ": ", final_table$text[i]))
  
  # -------------------------------------------------------------
  # STEP 1: NAVIGATE TO THE DETAIL PAGE
  # -------------------------------------------------------------
  # Wrap this in a tryCatch in case a single link is broken, so it won't crash the whole loop
  tryCatch({
    b$go_to(current_url)
    
    # -------------------------------------------------------------
    # STEP 2: PARSE THE CONTENT
    # -------------------------------------------------------------
    doc <- b$DOM$getDocument()
    html_list <- b$DOM$getOuterHTML(nodeId = doc$root$nodeId)
    page_html <- read_html(html_list$outerHTML)
    
    raw_table <- page_html %>%
      html_element("table.StandardTable")%>%
      html_table()
    
    selected_rows <- raw_table[c(6:19), ]%>%
      select(X1,X2)%>%
      rename(category=X1,value=X2)%>%
      mutate(facility_id=strsplit(current_url, "facility_id=")[[1]][2])
    
    detailed_entities<-detailed_entities%>%
      bind_rows(selected_rows)
    
    
  }, error = function(e) {
    message(paste("Error scraping row", i, ":", e$message))
    detailed_entities <- detailed_entities
  })
  
  # -------------------------------------------------------------
  # STEP 3: THE "POLITE" PAUSE (Crucial)
  # -------------------------------------------------------------
  # This generates a random pause between 2.5 and 5 seconds per page.
  # It breaks up the rhythmic typing pattern that firewalls look for.
  Sys.sleep(runif(1, min = 2.5, max = 5.0))
}

# -------------------------------------------------------------
# STEP 4: COMBINE AND MERGE BACK TO ORIGINAL DATA
# -------------------------------------------------------------
# Bind all the detail rows into one dataframe
all_details_table <- bind_rows(details_list)


b$close()

  




