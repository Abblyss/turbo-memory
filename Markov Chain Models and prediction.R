#Markov Chain Models
install.packages("clickstream")
install.packages("superheat")
library(superheat) # for cluster visualization
library(clickstream)  # for transition probabilities
#import data
epadata <- readRDS(gzcon(url("https://goo.gl/s5vjWz"))) 
summary(epadata)
View(epadata)
#2.understanding data
head(sort(table(epadata$page), decreasing = TRUE), 5)
head(sort(table(epadata$page[epadata$pagetype == "html"]), decreasing = TRUE), 5)
hist(epadata$datetime, breaks = 25,
     main = "User Requests Over Time", xlab = "Date & Time")
host_tab <- sort(table(epadata$host), decreasing = TRUE)
host_tab
plot(host_tab, main = "Activity of Unique Users",
     ylab = "Number of Requests by User") 
host_cdf <- ecdf(cumsum(host_tab) / sum(host_tab))
host_cdf(0.8) 
# Only 40% of users account for 80% of transactions. 
plot(host_cdf, main = "Requests by Unique Users",
     xlab = "Total Requests (%)", ylab="Unique Users (%)")
abline(v = 0.8)
abline(h = host_cdf(0.8))
## preparing for markov chain model
epadata_ord <- epadata[order(epadata$host, epadata$datetime),]
View(epadata_ord)
epadata_ord$timediff <- 
  c(NA, 
    as.numeric(epadata_ord$datetime[2:nrow(epadata_ord)] - 
                 epadata_ord$datetime[1:(nrow(epadata_ord)-1)], units = "mins"))
View(epadata_ord)
epadata_ord$newsession <- NA
epadata_ord$newsession[1] <- TRUE
epadata_ord$newsession[2:nrow(epadata_ord)] <- 
  ifelse(epadata_ord$host[2:nrow(epadata_ord)] != 
           epadata_ord$host[1:(nrow(epadata_ord)-1)],     # test: different host?
         TRUE,                                            # yes: TRUE
         epadata_ord$timediff[2:nrow(epadata_ord)] >= 15) # no: compare time                                 # no: compare time
epadata_ord[1:16, c("host", "datetime", "newsession")]
epadata_ord$sessionnum <- cumsum(epadata_ord$newsession)
epadata_ord$timediff[epadata_ord$newsession == 1] <- NA
nrow(epadata_ord) / sum(epadata_ord$newsession)
View(epadata_ord)
session_length <- rle(epadata_ord$sessionnum)$length
table(session_length)
plot(table(session_length), 
     main = "Distribution",
     xlab = "Number of Requests per Session", ylab = "Number of Sessions")
top_pages <- names(head(sort(table(epadata$page[epadata$pagetype == "html"]),
                             decreasing = TRUE), 20))
epadata_html20 <- subset(epadata_ord, pagetype == "html" &
                           page %in% top_pages)
epadata_session <- split(epadata_html20, epadata_html20$sessionnum)
View(epadata_session)
htmlsession_length <- lapply(epadata_session, nrow)
View(htmlsession_length)
epadata_session <- epadata_session[htmlsession_length > 1]
epadata_lines <- unlist(lapply(epadata_session,
                               function(x)
                                 paste0(unique(x$host), ",",
                                        paste0(unlist(x$page), collapse = ","),
                                        ",END")))
head(epadata_lines, 4)

# Estimate the Markov Chain
click_tempfile <- tempfile()
writeLines(epadata_lines, click_tempfile)
View(click_tempfile)
epa_lines <- readClickstreams(click_tempfile, header = TRUE)
head(epa_lines, 4)
epa_mc <- fitMarkovChain(epa_lines, order = 1)
epa_mc@transitions


#visualize outcome
epa_mc_trans <- t(epa_mc@transitions[[1]])
set.seed(59911)
superheat(epa_mc_trans[-1, ],                
          bottom.label.size = 0.4,
          bottom.label.text.size = 3.5,
          bottom.label.text.angle = 270,
          left.label.size = 0.3,
          left.label.text.size = 4,
          heat.col.scheme = "red", 
          n.clusters.rows = 5, n.clusters.cols = 5,
          left.label = "variable", bottom.label = "variable", 
          title = "Transition Matrix in Sequences of Top 20 HTML Pages")
set.seed(70510)
plot(epa_mc, minProbability = 0.25)

##predict the next page
epa_lines[110]
epa_pred <- new("Pattern", sequence = head(unlist(epa_lines[110]), -1))
predict(epa_mc, epa_pred, dist = 1)

epa_lines[160]
epa_pred <- new("Pattern", sequence = head(unlist(epa_lines[160]), -1))
predict(epa_mc, epa_pred, dist = 2)
##predict the next web page of this host using model epa_mc from markov chain
# With 10% probability the sequence continues with the Rules page before reaching
# the end state.
