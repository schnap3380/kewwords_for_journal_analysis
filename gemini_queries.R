source("journal_finder.R")
library(gemini.R)

form$gemini_response <- NA_character_
form$gemini_response_time <- NA_POSIXct_
for (i in 1356:nrow(form)) {
  q <- queries[i]
  cat("\n\nQuery", i, "/", nrow(form), "\n")
	response <- gemini(q, model = "1.5-flash")
	if (is.null(response)) {
	  # try again in a few seconds
	  Sys.sleep(4)
	  response <- gemini(q, model = "1.5-flash")
	}
	form$gemini_response[i] <- response
	form$gemini_response_time[i] <- Sys.time()
	cat(q, "\n\n", response)
}

write_csv(form, "data/gemini_responses.csv")



