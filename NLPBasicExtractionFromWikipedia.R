rm(list=ls())
library("rvest")
library(rJava)
library(NLP)
library(openNLP)
library(RWeka)
library(ggmap)
library(rworldmap)
library(rworldxtra)


library(rvest)
page = read_html('https://en.wikipedia.org/wiki/Alphabet_Inc.')
text = html_text(html_nodes(page,'p'))# 'p' is the htmal tag for paragraph <p>,  
text = text[text != ""]     # drop empty rows


# increase java heap memory space
options(java.parameters = "- Xmx2048m")    # since openNLP relies on java,  makes sense to make more meory space (2gigs here) available to java. 

texts = list(NULL)

# Do only for 20 documents

for (i in 1:length(text)){
  texts[i] = as.String(text[i])
}

texts[[1]]  # view what texts contains
texts[[2]]  # view what texts contains

# names(texts) <- basename(filenames)	# use base file names as R's object names too
# names(texts)  # view names just to make sure
# str(texts, max.level = 1)		# ?


## Now, time to code a func that'll take in corpus and return an annotated textfile based on annotations we want

annotate_entities <- function(doc, annotation_pipeline) {   # open func
  
  annotations <- annotate(doc, annotation_pipeline)
  
  AnnotatedPlainTextDocument(doc, annotations)
  
} # end func

# now let's define the annotations we want. same as last time, or could be new also.

new_pipeline <- list(			# open annotation list	
  Maxent_Entity_Annotator(language="en",kind = "person"),
  Maxent_Entity_Annotator(language="en",kind = "location"),
  Maxent_Sent_Token_Annotator(),	
  Maxent_Word_Token_Annotator())
  
  #Maxent_POS_Tag_Annotator())		
  #	# anotate person entities
  #	# annotate location entities # close annotation list

# Apply annotate_entities() to the corpus. Warning: may take-up time depending on corpus size
# so its a good practice to store results obtained in an RDA file to reload later as required.

#texts_annotated <- texts %>%
#  lapply(annotate_entities, 
#         new_pipeline)

t = Sys.time()

itinerants_pipeline <- list(
  Maxent_Sent_Token_Annotator(),
  Maxent_Word_Token_Annotator(),
  Maxent_Entity_Annotator(kind = "person"),
  Maxent_Entity_Annotator(kind = "location")
 )

Sys.time() - t

itinerants_pipeline

require(magrittr) || install.packages("magrittr")
library(magrittr)

texts_annotated <- texts[1:10] %>%
  lapply(annotate_entities, itinerants_pipeline)

location <- texts_annotated %>%
  lapply(entities, kind = "location")

person <- texts_annotated %>%
  lapply(entities, kind = "person")


Sys.time() - t

location
person




# now possible to use our entities() func defined above to extract the relevant info. 
# Should keep these all in a single list object

places <- texts_annotated %>%		# the data object on which to lapply
  lapply(entities, 		# the func to apply on above data obj	
         kind = "location")	# the argument for the above func

places # view contents of places object

people <- texts_annotated %>%
  lapply(entities, 
         kind = "person")

people # view contents of people object


# some statistics to give a sense of what we extracted. 
# We count up #items + the #unique items for each text.

# Total place mentions 
places %>%
  sapply(length)

# Unique places
places %>%
  lapply(unique) %>%	# apply func unique() on places obj
  sapply(length)	# apply length() func to unique places list



# Total mentions of people
people %>%
  sapply(length)

# Unique people mentioned
people %>%
  lapply(unique) %>%
  sapply(length)


# Geo map allocation

text = paste(text,collapse = " ") 

text = as.String(text)

sent_annot = Maxent_Sent_Token_Annotator()
word_annot = Maxent_Word_Token_Annotator()
loc_annot = Maxent_Entity_Annotator(kind = "location")

annot.l1 = NLP::annotate(text, list(sent_annot,word_annot,loc_annot))

k <- sapply(annot.l1$features, `[[`, "kind")
coke_locations = text[annot.l1[k == "location"]]


## +++ some downstream analysis with above data +++ ##
##

# We could do much with this info, e.g., improve lists by editing them with external domain knowledge, etc. 
# E.g., geocode the locations and create a map of the world of each article. 

all_places = unique(coke_locations) # view contents of this obj

all_places_geocoded <- geocode(all_places) #[1:10]
all_places_geocoded # view contents of this obj

newmap <- getMap(resolution = "high")
plot(newmap, 
     # xlim = c(-20, 59), ylim = c(35, 71),   # can select 'boxes' of lat-lon to focus on
     asp = 1)

points(all_places_geocoded$lon, 
       all_places_geocoded$lat, 
       col = "red", cex = 1.2, pch = 19)
