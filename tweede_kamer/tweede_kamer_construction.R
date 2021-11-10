# -------------------------------
# Counterfactual Bill Proposals
# Tweede Kamer
# Bill construction
# Lion Behrens
# -------------------------------

# packages and data
library(tidyverse)
library(readstata13)
library(stringr)   # for string manipulation
library(qdapRegex) # to remove text between brackets with rm_between
library(lme4)      # for multilevel modeling
library(stargazer) 
library(MASS)
library(tikzDevice)
load("tweede_kamer/data_NED.RData")
#file.edit("bundestag_weird_proposals_pre.R") # execute before continuing

#' ---------------------------------
## 0. add amend_lists to amends ----
#' ---------------------------------
amends$amend_list <- amend_lists

#' ----------------------------
## 1. create action_matrix ----
#' ----------------------------
amends$action_matrix <- NA

# -------------------------------------------------------
## 1.1 create empty action matrix for each amendment ----
# -------------------------------------------------------
for (row in 1:nrow(amends)) {
  
  list_dims <- names(unlist(rapply(amends$amend_list[row], length, 
                                   how="list")))
  action_matrix <- matrix(NA, nrow=length(list_dims), ncol=14) #attention: added another column for ref.art2
  colnames(action_matrix) <- 
    c("ref.art", "ref.art2", "ref.num", "ref.buch", "ref.buchbuch", 
      "ref.para", "ref.absatz", "ref.satz", "text.spec", "text.cite", 
      "header", "loeschen", "neufassen", "hinzufuegen")
  amends$action_matrix[row] <- list(action_matrix)
  
} 

# here: begin for()-loop over all amendments
for (row in 1:nrow(amends)) {
  # row <- 1
  
  # define objects to work with in specific row
  amend_list <- amends$amend_list[[row]]
  action_matrix <- amends$action_matrix[[row]]
  
  # --------------------------------------
  ## 1.2 identify function activation ----
  # --------------------------------------
  loeschen_words <-    
    # translation of german words
    # NOT FINISHED
    c("streichen", "gestrichen", 
      "aufheben", "aufgehoben", "aufzuheben", 
      "löschen", "gelöscht", 
      "entfallen", "entfällt",
    # words that appear empirically 
      "vervallen", "vervalt")
  neufassen_words <-  
    # translation of german words
    # NOT FINISHED
    c("fassen", "gefasst", 
      "neufassen", "neugefasst", "neuzufassen", 
      "Fassung",
      "ersetzen", "ersetzt",
    # words that appear empirically
      "vervangen", "vervangt", "vervanging",
      "komen", "komt")
  hinzufuegen_words <- 
    # translation of german words
    # NOT FINISHED
    c("anfügen", "angefügt", "anzufügen", 
      "hinzufügen", "hinzugefügt", "hinzuzufügen", 
      "ergänzen", "ergänzt", 
      "erweitern", "erweitert",
      "einfügen", "eingefügt", "einzufügen", 
      "einschieben", "eingeschoben", "einzuschieben",
      "voranstellen", "vorangestellt", "voranzustellen",
    # words that appear empirically
      "invoegen", "ingevoegd",
      "toevoegen", "toegevoegd")
  
  # NEED CLARIFICATION / FIXES:
  # 1. "vervalt, onder vervanging" (amend_lists[[40]])
  # ein Teil verfällt, ein anderer wird geändert
  # 2. "telkens vervangen" (amend_lists[[502]])
  # "immer geändert" -> wiederholte Änderung?
  # 3. "onder vervanging [...] toegevoegd" (amend_lists[[514]])
  # 4. "gewijzigd" (amend_lists[[514]])
  # "geändert"; eigentlich Einleitung für konkrete Änderungen;
  # 5. "geplaatst" -> hinzugefügt? (amend_lists[[517]])
  # 6. "komt [...] vervallen" -> gestrichen (amend_lists[[1008]])
  # 7. "vervalt telkens" -> verfällt jedes Mal (amend_lists[[1016]])
  # 8. Artikel ID in name of list_element (amend_lists[[25]])
  # 9. collapsing within list_element when another layer in list (amend_lists[[23]]); maybe another loop just a layer beneath?
  # 10. vervallen de tweede en derde volzin (= zweiter und dritter Satz) (47)
  
  # NOTES:
  # 2x "Article": Article I, Artikel 4; first in roman number; 
  # een volzin toegevoegd (amend_lists[[25]]) = ein Satz
  # aan het slot een volzin toegevoegd (37) = am Ende ein Satz
  # some amendments seem to be duplicates -> unique()? 
  
  
  # collapse lines within a list element
  for(list_el in 1:nrow(action_matrix)){
    amend_list[list_el] <- str_c(amend_list[list_el][[1]], collapse = " ")
  }

  # remove cited text
  amend_text <- unlist(amend_list, use.names = TRUE) 
  action_text <- str_replace(amend_text, "«.*»", "")
  #action_text <- str_replace(amend_text, ",.*ʻ", "")
  #action_text <- str_replace(action_text, "‚.*‘", "")
  #action_text <- str_replace(action_text, ",.*‘", "")
  #action_text <- str_replace(action_text, "‚.*’", "")
  #action_text <- str_replace(action_text, "‘.*’", "")
  #action_text <- str_replace(action_text, "„.*“", "")
  #action_text <- str_replace(action_text, "[(].*[)]", "")
  
  # search for action words in each list element
  action_matrix[,"loeschen"] <- F
  action_matrix[,"neufassen"] <- F
  action_matrix[,"hinzufuegen"] <- F
  for (list_el in 1:nrow(action_matrix)) {
    if(length(which(str_detect(action_text[list_el], loeschen_words))) > 0)
      action_matrix[list_el,"loeschen"] <- T
    if(length(which(str_detect(action_text[list_el], neufassen_words))) > 0)
      action_matrix[list_el,"neufassen"] <- T
    if(length(which(str_detect(action_text[list_el], hinzufuegen_words))) > 0)
      action_matrix[list_el,"hinzufuegen"] <- T
  }
  
  
  # ----------------------------------
  ## 1.3 identify article (roman) ----
  # ----------------------------------
  
  for (list_el in 1:length(action_text)) {
    
    # "Artikel I"
    if (str_detect(action_text[list_el], "(\\bArtikel\\s[MDCLXVI]+[:lower:]\\b|\\bArtikel\\s[MDCLXVI]+\\b)")) 
      action_matrix[list_el,"ref.art"] <- 
        str_extract(action_text[list_el], "(\\bArtikel\\s[MDCLXVI]+[:lower:]\\b|\\bArtikel\\s[MDCLXVI]+\\b)")
    
    # "artikel I"
    if (str_detect(action_text[list_el], "(\\bartikel\\s[MDCLXVI]+[:lower:]\\b|\\bartikel\\s[MDCLXVI]+\\b)")) 
      action_matrix[list_el,"ref.art"] <- 
        str_extract(action_text[list_el], "(\\bartikel\\s[MDCLXVI]+[:lower:]\\b|\\bartikel\\s[MDCLXVI]+\\b)")
    
    # ATTENTION: NOT FINISHED; LOOK FOR FURTHER MUTATIONS
    
    
  } # end for loop over action text list elements
  
  
  # --------------------------------------
  ## 1.3.1 identify article (numeric) ----
  # --------------------------------------
  
  # "artikel 1"
  if (str_detect(action_text[list_el], "(\\bartikel\\s[:digit:]+[:lower:]\\b|\\bartikel\\s[:digit:]+\\b)")) 
    action_matrix[list_el,"ref.art"] <- 
    str_extract(action_text[list_el], "(\\bartikel\\s[:digit:]+[:lower:]\\b|\\bartikel\\s[:digit:]+\\b)")
  
  # "Artikel 44"
  if (str_detect(action_text[list_el], "(\\bArtikel\\s[:digit:]+[:lower:]\\b|\\bArtikel\\s[:digit:]+\\b)")) 
    action_matrix[list_el,"ref.art"] <- 
    str_extract(action_text[list_el], "(\\bArtikel\\s[:digit:]+[:lower:]\\b|\\bArtikel\\s[:digit:]+\\b)")
  
  # ATTENTION: NOT FINISHED; LOOK FOR FURTHER MUTATIONS
  
  
  
  # -------------------------
  ## 1.4 identify Nummer ----
  # -------------------------
  
  for (list_el in 1:length(action_text)) {
    
    # "Nummer 1"
    if (str_detect(action_text[list_el], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)")) 
      action_matrix[list_el,"ref.num"] <-  
        str_extract(action_text[list_el], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+")
    
    # "Nach Nummer 1 wird Nummer 2 eingefügt" | "Nummer 1 wird Nummer 01 vorangestellt"
    if (str_count(action_text[list_el], "\\bNummer\\b") == 2 & (str_detect(action_text[list_el], "Nach|nach") == T | str_detect(action_text[list_el], "voran") == T ))  
      action_matrix[list_el,"ref.num"] <- 
        str_extract_all(action_text[list_el], "\\bNummer\\s[:digit:]+\\b")[[1]][1]
    
    
    
    # "Nummer 1 und Nummer 2"
    if (str_detect(action_text[list_el], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]\\sund\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+")) 
      action_matrix[list_el,"ref.num"] <-  
        str_extract(action_text[list_el], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sund\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+")
    
    # "Nummer 1 und 2"
    if (str_detect(action_text[list_el], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]\\sund\\s[:digit:]+")) 
      action_matrix[list_el,"ref.num"] <-  
        str_extract(action_text[list_el], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sund\\s[:digit:]+")
    
    # "Nummer 1, 2 und 3"
    if (str_detect(action_text[list_el], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:],\\s[:digit:](,)\\sund\\s[:digit:]+")) 
      action_matrix[list_el,"ref.num"] <-  
        str_extract(action_text[list_el], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+,\\s[:digit:](,)\\sund\\s[:digit:]+")
    
    # "Nummer 1 bis Nummer 3"
    if (str_detect(action_text[list_el], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]\\sbis\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+")) 
      action_matrix[list_el,"ref.num"] <-  
        str_extract(action_text[list_el], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sbis\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+")
    
    # "Nummer 1 bis 3"
    if (str_detect(action_text[list_el], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]\\sbis\\s[:digit:]+")) 
      action_matrix[list_el,"ref.num"] <-  
        str_extract(action_text[list_el], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sbis\\s[:digit:]+")
    
    # Nummer(n) 1 bis 4 und 7
    if (str_detect(action_text[list_el], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sbis\\s[:digit:]+\\sund\\s[:digit:]+")) 
      action_matrix[list_el,"ref.num"] <-  
        str_extract(action_text[list_el], "\\b(Nummer|Nummern|Nr.|Ziffer|Ziffern)\\s[:digit:]+\\sbis\\s[:digit:]+\\sund\\s[:digit:]+")
    
    
  } # end for loop over action text list elements
  
  
  # ----------------------------
  ## 1.5 identify Buchstabe ----
  # ----------------------------
  
  for (list_el in 1:length(action_text)) {
    
    if (str_detect(action_text[list_el], "\\s[:lower:][)]\\s")) 
      action_matrix[list_el,"ref.buch"] <-  
        str_extract(action_text[list_el], "\\s[:lower:][)]\\s")
    
    # "Buchstabe a"
    if (str_detect(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.)")) 
      action_matrix[list_el,"ref.buch"] <-  
        str_extract(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]")
    
    # "Nach Buchstabe a wird Buchstabe b eingefügt" | "Buchstabe a wird Buchstabe b vorangestellt"
    if (str_count(action_text[list_el], "\\bBuchstabe\\b") == 2 & (str_detect(action_text[list_el], "Nach|nach") == T | str_detect(action_text[list_el], "voran") == T ))  
      action_matrix[list_el,"ref.buch"] <- 
        str_extract_all(action_text[list_el], "\\bBuchstabe\\s[:lower:]\\b")[[1]][1]
    
    # "Buchstabe a und Buchstabe b"
    if (str_detect(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sund\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]")) 
      action_matrix[list_el,"ref.buch"] <-  
        str_extract(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sund\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]")
    
    # "Buchstabe a und b"
    if (str_detect(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sund\\s[:lower:]\\s")) 
      action_matrix[list_el,"ref.buch"] <-  
        str_extract(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sund\\s[:lower:]\\s")
    
    # "Buchstabe a, b und c"
    if (str_detect(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:],\\s[:lower:]\\sund\\s[:lower:]\\s")) 
      action_matrix[list_el,"ref.buch"] <-  
        str_extract(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:],\\s[:lower:]\\sund\\s[:lower:]\\s")
    
    # "Buchstabe a bis Buchstabe c"
    if (str_detect(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sbis\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]")) 
      action_matrix[list_el,"ref.buch"] <-  
        str_extract(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sbis\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]")
    
    # "Buchstabe a bis c"
    if (str_detect(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sbis\\s[:lower:]\\s")) 
      action_matrix[list_el,"ref.buch"] <-  
        str_extract(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.)\\s[:lower:]\\sbis\\s[:lower:]\\s")
    
    
  } # end for loop over action text list elements
  
  
  # --------------------------------------
  ## 1.6 identify Buchstabe/Buchstabe ----
  # --------------------------------------
  
  for (list_el in 1:length(action_text)) {
    
    # "Buchstabe aa"
    if (str_detect(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]")) 
      action_matrix[list_el,"ref.buchbuch"] <-  
        str_extract(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]")
    
    # "Nach Buchstabe aa wird Buchstabe bb eingefügt" | "Buchstabe aa wird Buchstabe bb vorangestellt"
    if (str_count(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\b") == 2 & (str_detect(action_text[list_el], "Nach|nach") == T | str_detect(action_text[list_el], "voran") == T ))  
      action_matrix[list_el,"ref.buchbuch"] <- 
        str_extract_all(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\b")[[1]][1]
    
    # "Buchstabe aa und Buchstabe bb"
    if (str_detect(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sund\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]")) 
      action_matrix[list_el,"ref.buchbuch"] <-  
        str_extract(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sund\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]")
    
    # "Buchstabe aa und bb"
    if (str_detect(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sund\\s[:lower:][:lower:]")) 
      action_matrix[list_el,"ref.buchbuch"] <-   
        str_extract(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sund\\s[:lower:][:lower:]")
    
    # "Buchstabe aa, bb und cc"
    if (str_detect(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:],\\s[:lower:][:lower:]\\sund\\s[:lower:][:lower:]")) 
      action_matrix[list_el,"ref.buchbuch"] <-  
        str_extract(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:],\\s[:lower:][:lower:]\\sund\\s[:lower:][:lower:]")
    
    # "Buchstabe aa bis Buchstabe cc"
    if (str_detect(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sbis\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]")) 
      action_matrix[list_el,"ref.buchbuch"] <-  
        str_extract(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sbis\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]")
    
    # "Buchstabe aa bis cc"
    if (str_detect(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sbis\\s[:lower:][:lower:]")) 
      action_matrix[list_el,"ref.buchbuch"] <-  
        str_extract(action_text[list_el], "\\b(Buchstabe|Buchstaben|Buchst.|Doppelbuchstabe)\\s[:lower:][:lower:]\\sbis\\s[:lower:][:lower:]")
    
  } # end for loop over action text list elements
  
  
  # --------------------------------------
  ## 1.7 identify paragraph --------------
  # --------------------------------------
  
  for (list_el in 1:length(action_text)) {
    
    # Referenz: "§ 1"
    if (str_detect(action_text[list_el], "([§]\\s[:digit:]+([:lower:])\\b)|((^|\\s)[§]\\s[:digit:]+\\b)")) 
      action_matrix[list_el,"ref.para"] <-  
        str_extract(action_text[list_el], "([§]\\s[:digit:]+([:lower:])\\b)|((^|\\s)[§]\\s[:digit:]+\\b)")
    
    # Referenz: "§ 1 und § 2"
    if (str_detect(action_text[list_el], "((^|\\s)[§]\\s[:digit:]+([:lower:])\\sund\\s[§]\\s[:digit:]+([:lower:])\\b|(^|\\s)[§]\\s[:digit:]+\\sund\\s[§]\\s[:digit:]+\\b)")) 
      action_matrix[list_el,"ref.para"] <-   
        str_extract(action_text[list_el], "((^|\\s)[§]\\s[:digit:]+([:lower:])\\sund\\s[§]\\s[:digit:]+([:lower:])\\b|(^|\\s)[§]\\s[:digit:]+\\sund\\s[§]\\s[:digit:]+\\b)")
    
    # Referenz: "§§ 1 und 2"
    if (str_detect(action_text[list_el], "((^|\\s)[§][§]\\s[:digit:]+([:lower:])\\sund\\s[:digit:]+([:lower:])\\b|(^|\\s)[§][§]\\s[:digit:]+\\sund\\s[:digit:]+\\b)")) 
      action_matrix[list_el,"ref.para"] <-  
        str_extract(action_text[list_el], "((^|\\s)[§][§]\\s[:digit:]+([:lower:])\\sund\\s[:digit:]+([:lower:])\\b|(^|\\s)[§][§]\\s[:digit:]+\\sund\\s[:digit:]+\\b)")
    
    # Referenz: "§§ 1, 2 und 3"
    if (str_detect(action_text[list_el], "((^|\\s)[§][§]\\s[:digit:]+([:lower:]),\\s[:digit:]+([:lower:])\\sund\\s[:digit:]+([:lower:])\\b|(^|\\s)[§][§]\\s[:digit:]+,\\s[:digit:]+\\sund\\s[:digit:]+\\b)")) 
      action_matrix[list_el,"ref.para"] <-  
        str_extract(action_text[list_el], "((^|\\s)[§][§]\\s[:digit:]+([:lower:]),\\s[:digit:]+([:lower:])\\sund\\s[:digit:]+([:lower:])\\b|(^|\\s)[§][§]\\s[:digit:]+,\\s[:digit:]+\\sund\\s[:digit:]+\\b)")
    
    # Referenz: "§ 1 bis § 3"
    if (str_detect(action_text[list_el], "((^|\\s)[§]\\s[:digit:]+([:lower:])\\sbis\\s[§]\\s[:digit:]+([:lower:])\\b|(^|\\s)[§]\\s[:digit:]+\\sbis\\s[§]\\s[:digit:]+\\b)")) 
      action_matrix[list_el,"ref.para"] <-  
        str_extract(action_text[list_el], "((^|\\s)[§]\\s[:digit:]+([:lower:])\\sbis\\s[§]\\s[:digit:]+([:lower:])\\b|(^|\\s)[§]\\s[:digit:]+\\sbis\\s[§]\\s[:digit:]+\\b)")
    
    # Referenz: "§§ 1 bis 3"
    if (str_detect(action_text[list_el], "((^|\\s)[§][§]\\s[:digit:]+([:lower:])\\sbis\\s[:digit:]+([:lower:])\\b|(^|\\s)[§][§]\\s[:digit:]+\\sbis\\s[:digit:]+\\b)")) 
      action_matrix[list_el,"ref.para"] <-  
        str_extract(action_text[list_el], "((^|\\s)[§][§]\\s[:digit:]+([:lower:])\\sbis\\s[:digit:]+([:lower:])\\b|(^|\\s)[§][§]\\s[:digit:]+\\sbis\\s[:digit:]+\\b)")
    
  } # end for loop over action text list elements
  
  
  # --------------------------------------
  ## 1.8 identify absatz -----------------
  # --------------------------------------
  
  for (list_el in 1:length(action_text)) {
    
    # Referenz: "Absatz 1 und Absatz 2"
    if (str_detect(action_text[list_el], "\\b(Absatz|Abs.)\\s[:digit:]\\sund\\b(Absatz|Abs.)\\s[:digit:]+")) 
      action_matrix[list_el,"ref.absatz"] <-   
        str_extract(action_text[list_el], "\\b(Absatz|Abs.)\\s[:digit:]+\\sund\\b(Absatz|Abs.)\\s[:digit:]+")
    
    # Referenz: "Absatz 1 und 2"
    if (str_detect(action_text[list_el], "\\b(Absatz|Abs.)\\s[:digit:]\\sund\\s[:digit:]+")) 
      action_matrix[list_el,"ref.absatz"] <-   
        str_extract(action_text[list_el], "\\b(Absatz|Abs.)\\s[:digit:]+\\sund\\s[:digit:]+")
    
    # Referenz: "Absätze 1 und 2"
    if (str_detect(action_text[list_el], "\\b(Absätze|Abs.)\\s[:digit:]\\sund\\s[:digit:]+")) 
      action_matrix[list_el,"ref.absatz"] <-    
        str_extract(action_text[list_el], "\\b(Absätze|Abs.)\\s[:digit:]+\\sund\\s[:digit:]+")
    
    # Referenz: "Absatz 1, 2 und 3"
    if (str_detect(action_text[list_el], "\\b(Absatz|Abs.)\\s[:digit:],\\s[:digit:]\\sund\\s[:digit:]+")) 
      action_matrix[list_el,"ref.absatz"] <-   
        str_extract(action_text[list_el], "\\b(Absatz|Abs.)\\s[:digit:]+,\\s[:digit:]\\sund\\s[:digit:]+")
    
    # Referenz: "Absätze 1, 2 und 3"
    if (str_detect(action_text[list_el], "\\b(Absätze(n)|Abs.)\\s[:digit:],\\s[:digit:]\\sund\\s[:digit:]+")) 
      action_matrix[list_el,"ref.absatz"] <-   
        str_extract(action_text[list_el], "\\b(Absätze(n)|Abs.)\\s[:digit:]+,\\s[:digit:]\\sund\\s[:digit:]+")
    
    # Referenz: "Absatz 1 bis Absatz 3"
    if (str_detect(action_text[list_el], "\\b(Absatz|Abs.)\\s[:digit:]\\sbis\\b(Absatz|Abs.)\\s[:digit:]+")) 
      action_matrix[list_el,"ref.absatz"] <-   
        str_extract(action_text[list_el], "\\b(Absatz|Abs.)\\s[:digit:]+\\sbis\\b(Absatz|Abs.)\\s[:digit:]+")
    
    # Referenz: "Absatz 1 bis 3"
    if (str_detect(action_text[list_el], "\\b(Absatz|Abs.)\\s[:digit:]\\sbis\\s[:digit:]+")) 
      action_matrix[list_el,"ref.absatz"] <-    
        str_extract(action_text[list_el], "\\b(Absatz|Abs.)\\s[:digit:]+\\sbis\\s[:digit:]+")
    
    # Referenz: "Absätze 1 bis 3"
    if (str_detect(action_text[list_el], "\\b(Absätze|Abs.)\\s[:digit:]\\sbis\\s[:digit:]+")) 
      action_matrix[list_el,"ref.absatz"] <-   
        str_extract(action_text[list_el], "\\b(Absätze|Abs.)\\s[:digit:]+\\sbis\\s[:digit:]+")
    
    # Referenz: "Absatz 1"
    if (str_detect(action_text[list_el], "\\b(Absatz|Abs[:punct:])")) 
      action_matrix[list_el,"ref.absatz"] <-   
        str_extract(action_text[list_el], "\\b(Absatz|Abs[:punct:])\\s[:digit:]+")
    
    
  } # end for loop over action text list elements
  
  
  #' --------------------------------------
  ## 1.9 identify Satz -------------------
  #' --------------------------------------
  
  for (list_el in 1:length(action_text)) {
    
    # Referenz: "Satz 1"
    if (str_detect(action_text[list_el], "\\bSatz\\s[:digit:]+\\b")) 
      action_matrix[list_el,"ref.satz"] <- 
        str_extract(action_text[list_el], "\\bSatz\\s[:digit:]+\\b")
    
    # Referenz: "Satz 1 und Satz 2"
    if (str_detect(action_text[list_el], "\\bSatz\\s[:digit:]\\sund\\bSatz\\s[:digit:]+\\b")) 
      action_matrix[list_el,"ref.satz"] <- 
        str_extract(action_text[list_el], "\\bSatz\\s[:digit:]+\\sund\\bSatz\\s[:digit:]+\\b")
    
    # Referenz: "Satz 1 und 2"
    if (str_detect(action_text[list_el], "\\bSatz\\s[:digit:]\\sund\\s[:digit:]+\\b")) 
      action_matrix[list_el,"ref.satz"] <- 
        str_extract(action_text[list_el], "\\bSatz\\s[:digit:]+\\sund\\s[:digit:]+\\b")
    
    # Referenz: "Sätze(n) 1 und 2"
    if (str_detect(action_text[list_el], "\\b(Sätze|Sätzen)\\s[:digit:]\\sund\\s[:digit:]+\\b")) 
      action_matrix[list_el,"ref.satz"] <- 
        str_extract(action_text[list_el], "\\bSätze\\s[:digit:]+\\sund\\s[:digit:]+\\b")
    
    # Referenz: "Satz 1, 2 und 3"
    if (str_detect(action_text[list_el], "\\bSatz\\s[:digit:],\\s[:digit:]\\sund\\s[:digit:]+\\b")) 
      action_matrix[list_el,"ref.satz"] <- 
        str_extract(action_text[list_el], "\\bSatz\\s[:digit:]+,\\s[:digit:]\\sund\\s[:digit:]+\\b")
    
    # Referenz: "Sätze(n) 1, 2 und 3"
    if (str_detect(action_text[list_el], "\\b(Sätze|Sätzen)\\s[:digit:],\\s[:digit:]\\sund\\s[:digit:]+\\b")) 
      action_matrix[list_el,"ref.satz"] <- 
        str_extract(action_text[list_el], "\\bSätze\\s[:digit:]+,\\s[:digit:]\\sund\\s[:digit:]+\\b")
    
    # Referenz: "Satz 1 bis Satz 3"
    if (str_detect(action_text[list_el], "\\bSatz\\s[:digit:]\\sbis\\bSatz\\s[:digit:]+\\b")) 
      action_matrix[list_el,"ref.satz"] <- 
        str_extract(action_text[list_el], "\\bSatz\\s[:digit:]+\\sbis\\bSatz\\s[:digit:]+\\b")
    
    # Referenz: "Satz 1 bis 3"
    if (str_detect(action_text[list_el], "\\bSatz\\s[:digit:]\\sbis\\s[:digit:]+\\b")) 
      action_matrix[list_el,"ref.satz"] <- 
        str_extract(action_text[list_el], "\\bSatz\\s[:digit:]+\\sbis\\s[:digit:]+\\b")
    
    # Referenz: "Sätze(n) 1 bis 3"
    if (str_detect(action_text[list_el], "\\b(Sätze|Sätzen)\\s[:digit:]\\sbis\\s[:digit:]+\\b")) 
      action_matrix[list_el,"ref.satz"] <- 
        str_extract(action_text[list_el], "\\bSätze\\s[:digit:]+\\sbis\\s[:digit:]+\\b")
    
  } # end for loop over action text list elements
  
  
  #' -------------------------------------------
  ## 1.10 identify text.spec -------------------
  #' -------------------------------------------
  amend_list <- unlist(amend_list)
  for (list_el in 1:length(action_text)) {
    
    if(str_detect(amend_list[list_el], "(Wort|Worte|Worten|Wörter|Wörtern|Satz|Angabe|Angaben)\\s(‚|,|‘|‘‘|„)")) {
      
      if(lengths(rm_between(amend_list[list_el], "‚", "‘", extract = T)) == 2)
        action_matrix[list_el, "text.spec"] <- rm_between(amend_list[list_el], "‚", "‘", extract = T)[[1]][1]
      
      if(lengths(rm_between(amend_list[list_el], "‚", "‘", extract = T)) == 2)
        action_matrix[list_el, "text.spec"] <- rm_between(amend_list[list_el], "‚", "‘", extract = T)[[1]][1]
      
      if(lengths(rm_between(amend_list[list_el], "‘‘", "‘‘", extract = T)) == 2)
        action_matrix[list_el, "text.spec"] <- rm_between(amend_list[list_el], "‘‘", "‘‘", extract = T)[[1]][1]
      
      if(lengths(rm_between(amend_list[list_el], "„", "“", extract = T)) == 2)
        action_matrix[list_el, "text.spec"] <- rm_between(amend_list[list_el], "„", "“", extract = T)[[1]][1]
      
    }
    
  } # end for loop over action text list elements
  
  
  #' --------------------------------------
  ## 1.11 identify text.cite -------------
  #' --------------------------------------
  amend_list <- unlist(amend_list)
  for (list_el in 1:length(action_text)) {
    
    # attention: this "," is a genuine quotation mark
    if(str_detect(amend_list[list_el], "‚")) {
      ifelse(lengths(rm_between(amend_list[list_el], "‚", "‘", extract = T)) == 2,
             action_matrix[list_el, "text.cite"] <- rm_between(amend_list[list_el], "‚", "‘", extract = T)[[1]][2], 
             action_matrix[list_el, "text.cite"] <- str_extract(amend_list[list_el], "(?<=‚).*(?=‘)")[[1]][1]
      )
      amend_list[list_el] <- rm_between(amend_list[list_el], "‚", "‘")
    }
    
    # attention: this "," is just a comma, needs to go after
    if(str_detect(amend_list[list_el], ",")) {
      ifelse(lengths(rm_between(amend_list[list_el], "‚", "‘", extract = T)) == 2,
             action_matrix[list_el, "text.cite"] <- rm_between(amend_list[list_el], ",", "‘", extract = T)[[1]][2], 
             action_matrix[list_el, "text.cite"] <- str_extract(amend_list[list_el], "(?<=,).*(?=‘)")[[1]][1]
      )                                                                             
      amend_list[list_el] <- rm_between(amend_list[list_el], ",", "‘")
    }
    
    
    
    # attention: this "," is a genuine quotation mark
    if(str_detect(amend_list[list_el], "‚")) {
      ifelse(lengths(rm_between(amend_list[list_el], "‚", "’", extract = T)) == 2,
             action_matrix[list_el, "text.cite"] <- rm_between(amend_list[list_el], "‚", "’", extract = T)[[1]][2], 
             action_matrix[list_el, "text.cite"] <- str_extract(amend_list[list_el], "(?<=‚).*(?=’)")[[1]][1]
      )
      amend_list[list_el] <- rm_between(amend_list[list_el], "‚", "’")
    }
    
    # attention: this "," is just a comma, needs to go after
    if(str_detect(amend_list[list_el], ",")) {
      ifelse(lengths(rm_between(amend_list[list_el], "‚", "’", extract = T)) == 2,
             action_matrix[list_el, "text.cite"] <- rm_between(amend_list[list_el], ",", "’", extract = T)[[1]][2], 
             action_matrix[list_el, "text.cite"] <- str_extract(amend_list[list_el], "(?<=,).*(?=’)")[[1]][1]
      )                                                                             
      amend_list[list_el] <- rm_between(amend_list[list_el], ",", "’")
    }
    
    
    
    
    if(str_detect(amend_list[list_el], "ʻ")) {
      ifelse(lengths(rm_between(amend_list[list_el], ",", "ʻ", extract = T)) == 2,
             action_matrix[list_el, "text.cite"] <- rm_between(amend_list[list_el], ",", "ʻ", extract = T)[[1]][2], 
             action_matrix[list_el, "text.cite"] <- str_extract(amend_list[list_el], "(?<=,).*(?=ʻ)")[[1]][1]
      )                                                                             
      amend_list[list_el] <- rm_between(amend_list[list_el], ",", "ʻ")
    }
    
    
    if(str_detect(amend_list[list_el], "‘‘")) {
      ifelse(lengths(rm_between(amend_list[list_el], "‘‘", "‘‘", extract = T)) == 2,
             action_matrix[list_el, "text.cite"] <- rm_between(amend_list[list_el], "‘‘", "‘‘", extract = T)[[1]][2], 
             action_matrix[list_el, "text.cite"] <- str_extract(amend_list[list_el], "(?<=‘‘).*(?<=‘‘)")[[1]][1]
      )
      amend_list[list_el] <- rm_between(amend_list[list_el], "‘‘", "‘‘")
    }
    
    if(str_detect(amend_list[list_el], "„")) {
      ifelse(lengths(rm_between(amend_list[list_el], "„", "“", extract = T)) == 2,
             action_matrix[list_el, "text.cite"] <- rm_between(amend_list[list_el], "„", "“", extract = T)[[1]][2], 
             action_matrix[list_el, "text.cite"] <- str_extract(amend_list[list_el], "(?<=„).*(?<=“)")[[1]][1]
      )
      amend_list[list_el] <- rm_between(amend_list[list_el], "„", "“")
    }
    
  } # end for loop over action text list elements
  
  
  #' --------------------------------
  ## 1.12 identify header ----------
  #' --------------------------------
  # should change happen to law or article header?
  
  for (list_el in 1:length(action_text)) {
    
    action_matrix[list_el, "header"] <- str_detect(action_text[list_el], "(Überschrift|überschrift)") 
    
  }
  
  
  #' --------------------------------------
  ## 1.13 fill up action_matrix ----------
  #' --------------------------------------
  
  if (nrow(action_matrix) > 1) {  
    
    # ref.art
    for (i in 2:nrow(action_matrix)){
      if(is.na(action_matrix[,"ref.art"])[i] == T)
        action_matrix[i,"ref.art"] <- action_matrix[i-1,"ref.art"]
    }  
    # ref.num
    for (i in 2:nrow(action_matrix)){
      if(is.na(action_matrix[,"ref.num"])[i] == T & identical(action_matrix[i,"ref.art"], action_matrix[i-1,"ref.art"]))
        action_matrix[i,"ref.num"] <- action_matrix[i-1,"ref.num"]
    }
    # ref.buch
    for (i in 2:nrow(action_matrix)){
      if(is.na(action_matrix[,"ref.buch"])[i] == T & identical(action_matrix[i,"ref.art"], action_matrix[i-1,"ref.art"]) & identical(action_matrix[i,"ref.num"], action_matrix[i-1,"ref.num"]))
        action_matrix[i,"ref.buch"] <- action_matrix[i-1,"ref.buch"]
    }  
    # ref.buchbuch 
    for (i in 2:nrow(action_matrix)){
      if(is.na(action_matrix[,"ref.buchbuch"])[i] == T & identical(action_matrix[i,"ref.art"], action_matrix[i-1,"ref.art"]) & identical(action_matrix[i,"ref.num"], action_matrix[i-1,"ref.num"]) & identical(action_matrix[i,"ref.buch"], action_matrix[i-1,"ref.buch"]))
        action_matrix[i,"ref.buchbuch"] <- action_matrix[i-1,"ref.buchbuch"]
    } 
    # ref.para
    for (i in 2:nrow(action_matrix)){
      if(is.na(action_matrix[,"ref.para"])[i] == T & identical(action_matrix[i,"ref.art"], action_matrix[i-1,"ref.art"]) & identical(action_matrix[i,"ref.num"], action_matrix[i-1,"ref.num"]) & identical(action_matrix[i,"ref.buch"], action_matrix[i-1,"ref.buch"]) & identical(action_matrix[i,"ref.buchbuch"], action_matrix[i-1,"ref.buchbuch"]))
        action_matrix[i,"ref.para"] <- action_matrix[i-1,"ref.para"]
    } 
    # ref.absatz
    for (i in 2:nrow(action_matrix)){
      if(is.na(action_matrix[,"ref.absatz"])[i] == T & identical(action_matrix[i,"ref.art"], action_matrix[i-1,"ref.art"]) & identical(action_matrix[i,"ref.num"], action_matrix[i-1,"ref.num"]) & identical(action_matrix[i,"ref.buch"], action_matrix[i-1,"ref.buch"]) & identical(action_matrix[i,"ref.buchbuch"], action_matrix[i-1,"ref.buchbuch"]) & identical(action_matrix[i,"ref.para"], action_matrix[i-1,"ref.para"]))
        action_matrix[i,"ref.absatz"] <- action_matrix[i-1,"ref.absatz"]
    } 
    # ref.satz
    for (i in 2:nrow(action_matrix)){
      if(is.na(action_matrix[,"ref.satz"])[i] == T & identical(action_matrix[i,"ref.art"], action_matrix[i-1,"ref.art"]) & identical(action_matrix[i,"ref.num"], action_matrix[i-1,"ref.num"]) & identical(action_matrix[i,"ref.buch"], action_matrix[i-1,"ref.buch"]) & identical(action_matrix[i,"ref.buchbuch"], action_matrix[i-1,"ref.buchbuch"]) & identical(action_matrix[i,"ref.para"], action_matrix[i-1,"ref.para"]) & identical(action_matrix[i,"ref.absatz"], action_matrix[i-1,"ref.absatz"]))
        action_matrix[i,"ref.satz"] <- action_matrix[i-1,"ref.satz"]
    } 
    
  } # end if nrow(action_matrix) > 1
  
  
  #' ------------------------------------------------------------------
  ## 1.14 make action_matrix compatible with function input ----------
  #' ------------------------------------------------------------------
  action_matrix[,"ref.art"] <- gsub("(Artikeln|Artikel|Art[.])", "", action_matrix[,"ref.art"])
  action_matrix[,"ref.art"] <- gsub(" ", "", action_matrix[,"ref.art"])
  action_matrix[,"ref.num"] <- gsub("(Nummern|Nummer|Nr[.]|Nr)", "", action_matrix[,"ref.num"])
  action_matrix[,"ref.num"] <- gsub(" ", "", action_matrix[,"ref.num"])
  action_matrix[,"ref.buch"] <- gsub("(Buchstaben|Buchstabe|Buchst[.]|[)])", "", action_matrix[,"ref.buch"])
  action_matrix[,"ref.buch"] <- gsub(" ", "", action_matrix[,"ref.buch"])
  action_matrix[,"ref.buchbuch"] <- gsub("(Doppelbuchstaben|Doppelbuchstabe|Buchstaben|Buchstabe|Buchst[.])", "", action_matrix[,"ref.buchbuch"])
  action_matrix[,"ref.buchbuch"] <- gsub(" ", "", action_matrix[,"ref.buchbuch"])
  action_matrix[,"ref.para"] <- gsub("(Paragraphen|Paragraph|Par[.]|[§§]|[§])", "", action_matrix[,"ref.para"])
  action_matrix[,"ref.para"] <- gsub(" ", "", action_matrix[,"ref.para"])
  action_matrix[,"ref.absatz"] <- gsub("(Absätzen|Absätze|Absatz|Abs[.])", "", action_matrix[,"ref.absatz"])
  action_matrix[,"ref.absatz"] <- gsub(" ", "", action_matrix[,"ref.absatz"])
  action_matrix[,"ref.satz"] <- gsub("(Sätzen|Sätze|Satz)", "", action_matrix[,"ref.satz"])
  action_matrix[,"ref.satz"] <- gsub(" ", "", action_matrix[,"ref.satz"])
  
  for (i in 1:nrow(action_matrix)) {
    
    if (action_matrix[i, "neufassen"] == T)
      next
    
    for (var in c("ref.art", "ref.num")) {
      
      elements <- str_extract_all(action_matrix[i, var], "[:digit:]+")
      
      # just one mention
      if (lengths(elements) == 1)
        action_matrix[i, var] <- gsub(" ", "", elements)
      
      # two mentions
      if (!is.na(action_matrix[i, var]) & str_detect(action_matrix[i, var], "und")) {
        action_matrix <- rbind(action_matrix[1:i,], action_matrix[i:nrow(action_matrix),])
        
        action_matrix[i, var] <- elements[[1]][1]
        action_matrix[i, var] <- gsub(" ", "", action_matrix[i, var])
        
        action_matrix[i+1, var] <- elements[[1]][2]
        action_matrix[i+1, var] <- gsub(" ", "", action_matrix[i+1, var])
      } # end if
      
      # more than two mentions
      if (!is.na(action_matrix[i, var]) & str_detect(action_matrix[i, var], "bis")) {
        nr <- as.numeric(elements[[1]][2]) - as.numeric(elements[[1]][1])
        
        action_matrix <- rbind(action_matrix[1:i,], 
                               action_matrix[rep(i, nr-1),],
                               action_matrix[i:nrow(action_matrix),]
        )
        action_matrix[i:(i+nr), var] <- seq(as.numeric(elements[[1]][1]), 
                                            as.numeric(elements[[1]][2]), 
                                            1)
        action_matrix[, var] <- gsub(" ", "", action_matrix[, var])
      } # end if
      
    } # end for var in 
    
  } # end for i in 1:nrow(action_matrix)
  
  
  
  amends$action_matrix[row] <- list(action_matrix)  
} # end for loop over all amendments 1.2
file.edit("U:/SFB 884, C7/C7 Subproject GER NED/bundestag_weird_proposals_post.R") # execute before continuing


#### current status
# basically, this is all fine, below are some things that I could still look into
# implement changes now and see how far I come, rest manually?


#### das generelle Auslesen der action_matrix-Elemente ist done
#### filling up action_matrix anders für neu_gesetze? Zur Zeit basiert Logik auf Änderungsgesetzen. 

#### nochmal zum reinsehen: 
#### Änderungen in der Überschrift müssen separat eingearbeitet werden, bspw. 60
#### bei ref.para ($ 1), ref.absatz (Absatz 1), ref.satz (Satz 1) noch 287 188 ansehen
#### Die letzte Zeile der action_matrix 92 zeigt mir, wie ich neufassen implementieren muss wenn mehrere Nummern angesprochen werden. In Implementeirung: Ist alle Nummern löschen, dann neuen Text einfügen.

#### dann nach: 
#### nochmal 30 action_matrices samplen und durchchecken?
#### mal alle amends$amend_lists anschauen, die length = 1 haben. Manche müssen noch manuell in Listen gesplittet werden


#### one thing really needs to change still: sometimes the amendment proposals have quite some whitespace in them
#### this is a problem when text is cited (!) that should be identified in bills. So in amendment proposals, 
#### whitespace needs to deleted. In bills, whitespace often needs to be added still. 

### warum wurde die action_matrix von 128 nicht erweitert??????

### zitierter Text muss auch wie folgt erkannt werden: ‘ text ’

#  "Die Nummern 1, 2, 3, 4 und 7" - das muss ausgelesen werden, Zeile 214
# §§ 53, 85, 95 und 98 muss ausgelesen werden können 404

# nur "a)" muss als Buchstabe ausgelesen werden, siehe 244

#' --------------------------
## 2 implement changes ------
#' --------------------------

amends$artikel_list_hypo <- NA

# here: begin for()-loop over all amendments
errors <- rep(NA, nrow(amends))
for (row in 1:nrow(amends)) {
  
  tryCatch({
    action_matrix <- amends$action_matrix[[row]]
    bills_row <- which(bills$Drucksache == amends$idDrucksacheLegis[row])
    amends$artikel_list_hypo[row] <- bills$artikel_list[bills_row]
  }, error = function(e) { errors[row] <<- str_c("row=", row, " matrix_row=none")}) 
  
  # here: begin for()-loop over rows of action_matrix
  for (matrix_row in 1:nrow(action_matrix)) {
    
    tryCatch({
      
      
      #' -----------------------------------------------------------
      ## 2.0 special case: whole bill is re-written  ---------------
      #' -----------------------------------------------------------
      
      if (sum(is.na(action_matrix[matrix_row, 1:8])) == 8 & action_matrix[matrix_row, "neufassen"] == T) {
        amends$artikel_list_hypo[row][[1]] <- action_matrix[matrix_row, "text.cite"]
        next
      }
      
      
      #' --------------------------------
      ## 2.1 adding text  ---------------
      #' --------------------------------
      # newly added text is added at end of article 
      
      if (action_matrix[matrix_row, "hinzufuegen"] == T) {
        
        if(is.na(action_matrix[matrix_row, "ref.art"])) {
          art.length <- 1
        }
        
        if(!is.na(action_matrix[matrix_row, "ref.art"])) {
          ref.art <- as.numeric(action_matrix[matrix_row, "ref.art"])
          length_command <- str_c("art.length <- length(amends$artikel_list_hypo[", row, "][[1]]$Artikel_", action_matrix[matrix_row, "ref.art"], ")")
          eval(parse(text=length_command))
        }
        
        ifelse(is.na(action_matrix[matrix_row, "ref.art"]), 
               command <- str_c("amends$artikel_list_hypo[", row, "][[1]]$Artikel_fehlt",
                                "[", art.length+1, "] <- '", action_matrix[matrix_row, "text.cite"], "'"),
               command <- str_c("amends$artikel_list_hypo[", row, "][[1]]$Artikel_", action_matrix[matrix_row, "ref.art"],
                                "[", art.length+1, "] <- '", action_matrix[matrix_row, "text.cite"], "'") 
        )
        
        eval(parse(text=command))
        
      } # end if
      
      
      #' ------------------------
      ## 2.2 delete/rewrite  ----
      #' ------------------------
      if (action_matrix[matrix_row, "loeschen"] == T | action_matrix[matrix_row, "neufassen"] == T) { 
        
        if(is.na(action_matrix[matrix_row, "ref.art"]))
          action_matrix[matrix_row, "ref.art"] <- "NA"
        if(is.na(action_matrix[matrix_row, "ref.num"]))
          action_matrix[matrix_row, "ref.num"] <- "NA"
        if(is.na(action_matrix[matrix_row, "ref.buch"]))
          action_matrix[matrix_row, "ref.buch"] <- "NA"
        if(is.na(action_matrix[matrix_row, "ref.buchbuch"]))
          action_matrix[matrix_row, "ref.buchbuch"] <- "NA"     
        if(is.na(action_matrix[matrix_row, "ref.para"]))
          action_matrix[matrix_row, "ref.para"] <- "NA"     
        if(is.na(action_matrix[matrix_row, "ref.absatz"]))
          action_matrix[matrix_row, "ref.absatz"] <- "NA"     
        
        
        #' -------------------------
        ## 2.2.1 list elements  ----
        #' -------------------------
        
        # prepare command
        art_string <- str_c("amends$artikel_list_hypo[", row, "][[1]]$Artikel_", action_matrix[matrix_row, "ref.art"])
        art_fehlt_string <- str_c("amends$artikel_list_hypo[", row, "][[1]]$Artikel_fehlt")
        num_string <- str_c("$Nr_", action_matrix[matrix_row, "ref.num"])
        buch_string <- str_c("$", as.character(action_matrix[matrix_row, "ref.buch"]))
        buchbuch_string <- str_c("$", action_matrix[matrix_row, "ref.buchbuch"])
        par_string <- str_c("$`§", action_matrix[matrix_row, "ref.para"], "`")
        absatz_string <- str_c("$Absatz_", action_matrix[matrix_row, "ref.absatz"])
        
        ifelse(is.na(action_matrix[matrix_row, "ref.art"]) | action_matrix[matrix_row, "ref.art"] == "NA", 
               command_prep <- str_c(art_fehlt_string, num_string, buch_string, buchbuch_string, par_string, absatz_string),
               command_prep <- str_c(art_string, num_string, buch_string, buchbuch_string, par_string, absatz_string)
        )
        
        command_prep <- gsub("[$]Nr_NA", "", command_prep)
        command_prep <- gsub("[$]NA", "", command_prep)
        command_prep <- gsub("[$]`[§]NA`", "", command_prep)
        command_prep <- gsub("[$]Absatz_NA", "", command_prep)
        
        
        #' --------------------------------------------------------------------------------------------------
        ## 2.2.1.1 senario 1 - all list elements of the command are also list elements of the bill list  ----
        #' --------------------------------------------------------------------------------------------------
        if(!is.null(eval(parse(text=command_prep))) & is.na(action_matrix[matrix_row, "text.spec"])) {
          
          # execute command
          if (is.na(action_matrix[matrix_row, "ref.satz"]) & is.na(action_matrix[matrix_row, "text.spec"]) & action_matrix[matrix_row, "neufassen"] == "TRUE") {
            command <- str_c(command_prep, " <- '", action_matrix[matrix_row, "text.cite"], "'")
            eval(parse(text=command))  
          }
          
          if (is.na(action_matrix[matrix_row, "ref.satz"]) & is.na(action_matrix[matrix_row, "text.spec"]) & is.na(action_matrix[matrix_row, "text.cite"]) & action_matrix[matrix_row, "loeschen"] == "TRUE") {
            command <- str_c(command_prep, " <- ", "NULL")
            eval(parse(text=command))  
          }
          
        }
        
        #' -------------------------------------------------------------------------------------------------------------------------------------
        ## 2.2.1.2 scenario 2 - amendment proposal is referring to element (paragaph or absatz) that is not a list element of the bill list ----
        #' -------------------------------------------------------------------------------------------------------------------------------------
        
        
        if(is.null(eval(parse(text=command_prep))) & ((action_matrix[matrix_row, "neufassen"] == "TRUE" & is.na(action_matrix[matrix_row, "text.spec"]) | (action_matrix[matrix_row, "loeschen"] == "TRUE" & is.na(action_matrix[matrix_row, "text.cite"]))))) {
          
          
          ### delete paragraph or absatz
          
          # trim towards structural elements that should work 
          command_prep <- gsub("[$][§][0-9]+", "", command_prep)
          command_prep <- gsub("[$]Absatz_[0-9]+", "", command_prep)
          
          # reduce text to vector
          leg_text <- paste(unlist(eval(parse(text=command_prep))), collapse=" ")
          
          # removing paragraph
          if(!is.na(action_matrix[matrix_row, "ref.para"]) & is.na(action_matrix[matrix_row, "ref.absatz"])) {
            
            ifelse(str_detect(leg_text, str_c(as.numeric(action_matrix[matrix_row, "ref.para"])+1)),
                   leg_text <- str_remove(leg_text, str_c("[§]", action_matrix[matrix_row, "ref.para"], 
                                                          ".+[§]", as.numeric(action_matrix[matrix_row, "ref.para"])+1)),
                   leg_text <- str_remove(leg_text, str_c("[§]", action_matrix[matrix_row, "ref.para"], ".+$"))
            )
            
            command <- str_c(command_prep, " <- ", str_c("'", leg_text, "'"))
            eval(parse(text=command))   
          }
          
          # removing absatz 
          if(!is.na(action_matrix[matrix_row, "ref.absatz"])) {
            
            ifelse(str_detect(leg_text, str_c("[(]", as.numeric(action_matrix[matrix_row, "ref.absatz"])+1, "[)]")),
                   leg_text <- str_remove(leg_text, str_c("[(]", action_matrix[matrix_row, "ref.absatz"], "[)]",
                                                          ".+[()]", as.numeric(action_matrix[matrix_row, "ref.absatz"])+1, "[)]")),
                   leg_text <- str_remove(leg_text, str_c("[(]", action_matrix[matrix_row, "ref.absatz"], "[)].+$"))
            )
            
            command <- str_c(command_prep, " <- ", str_c("'", leg_text, "'"))
            eval(parse(text=command))   
          }
          
          
          ### if paragraph or absatz should be re-written, add text at end of article
          if (action_matrix[matrix_row, "neufassen"] == "TRUE") {
            
            if(is.na(action_matrix[matrix_row, "ref.art"])) {
              art.length <- 1
            }
            
            if(!is.na(action_matrix[matrix_row, "ref.art"])) {
              ref.art <- as.numeric(action_matrix[matrix_row, "ref.art"])
              art.length <- length(amends$artikel_list_hypo[[row]][[ref.art]])
            }
            
            ifelse(is.na(action_matrix[matrix_row, "ref.art"]), 
                   command <- str_c("amends$artikel_list_hypo[", row, "][[1]]$Artikel_fehlt",
                                    "[", art.length+1, "] <- '", action_matrix[matrix_row, "text.cite"], "'"),
                   command <- str_c("amends$artikel_list_hypo[", row, "][[1]]$Artikel_", action_matrix[matrix_row, "ref.art"],
                                    "[", art.length+1, "] <- '", action_matrix[matrix_row, "text.cite"], "'") 
            )
            
            eval(parse(text=command))
            
          }
          
          
        } # end is.null 
        
        
        
        #' -------------------------
        ## 2.2.2 specific text  ----
        #' -------------------------
        if ((action_matrix[matrix_row, "neufassen"] == "TRUE" & !is.na(action_matrix[matrix_row, "text.spec"]) | (action_matrix[matrix_row, "loeschen"] == "TRUE" & !is.na(action_matrix[matrix_row, "text.cite"])))) {
          
          
          #' (i) first remove text --------------
          
          # text to be removed
          if(action_matrix[matrix_row, "loeschen"] == T)
            text_remove <- action_matrix[matrix_row, "text.cite"]
          
          if(action_matrix[matrix_row, "neufassen"] == T)
            text_remove <- action_matrix[matrix_row, "text.spec"]
          
          # execute removal
          if(action_matrix[matrix_row, "ref.art"] == "NA") {
            element <- which(str_detect(unlist(amends$artikel_list_hypo[row][[1]]$Artikel_fehlt), text_remove))[1]
            amends$artikel_list_hypo[row][[1]]$Artikel_fehlt[element] <- 
              str_remove(unlist(amends$artikel_list_hypo[row][[1]]$Artikel_fehlt)[element], text_remove)     
          }
          
          if(action_matrix[matrix_row, "ref.art"] != "NA") {
            ref.art <- as.numeric(action_matrix[matrix_row, "ref.art"])
            
            command1 <- str_c("element <- which(str_detect(unlist(amends$artikel_list_hypo[row][[1]]$Artikel_",
                              ref.art, "), text_remove))[1]")
            eval(parse(text=command1))  
            
            command2 <- str_c("amends$artikel_list_hypo[row][[1]]$Artikel_", ref.art, 
                              "[element] <- str_remove(unlist(amends$artikel_list_hypo[row][[1]]$Artikel_", ref.art, 
                              ")[element], '", text_remove, "')")
            eval(parse(text=command2))  
            
            # element <- which(str_detect(unlist(amends$artikel_list_hypo[row][[1]]$Artikel_3), text_remove))[1]
            # amends$artikel_list_hypo[row][[1]]$Artikel_3[element] <- 
            # str_remove(unlist(amends$artikel_list_hypo[row][[1]]$Artikel_3)[element], "April2014inKraft")
            
          }
          
          #' (ii) if text is re-written, add new text to end of article ----------------
          
          if (action_matrix[matrix_row, "neufassen"] == "TRUE") {
            
            ifelse(is.na(action_matrix[matrix_row, "ref.art"]), 
                   ref.art <- 1,
                   ref.art <- as.numeric(action_matrix[matrix_row, "ref.art"])
            )
            art.length <- length(amends$artikel_list_hypo[[row]][[ref.art]])
            
            ifelse(is.na(action_matrix[matrix_row, "ref.art"]), 
                   command <- str_c("amends$artikel_list_hypo[", row, "][[1]]$Artikel_fehlt",
                                    "[", art.length+1, "] <- '", action_matrix[matrix_row, "text.cite"], "'"),
                   command <- str_c("amends$artikel_list_hypo[", row, "][[1]]$Artikel_", action_matrix[matrix_row, "ref.art"],
                                    "[", art.length+1, "] <- '", action_matrix[matrix_row, "text.cite"], "'")
            )
            
            eval(parse(text=command))  
            
          }
          
        } # end of 2.2.2 specific text
        
      } # end if deleting/rewriting     
      
    }, error = function(e) {
      
      errors[row] <<- str_c("row=", row, " matrix_row=", matrix_row)
      
    }) # end tryCatch
    
  } # end for matrix_row loop over action_matrix rows
  
} # end for row loop over all amendments 2

errors <- errors[which(!is.na(errors))]

# STATUS
# - generell steht der Code
# - doppelte Leerzeichen in amend_lists und bills löschen 
# - Bei der Auslesung der Bills Leerzeichen reinkriegen
# - Auf einem sample checken, wie die Pipeline performt




# some amends$idDrucksacheLegis are not found in bills
# where are the bills that they refer to?
bills_row <- rep(NA, nrow(amends))
for (row in 1:nrow(amends)) 
  if (length(which(bills$Drucksache == amends$idDrucksacheLegis[row])) > 0)
    bills_row[row] <- which(bills$Drucksache == amends$idDrucksacheLegis[row])
which(is.na(bills_row))

# some laws are read out incorrectly
# e.g. amends_row 3




# performance anhand eines samples überprüfen
sam <- sample(1:nrow(amends), 25)

row <- 6
amends$amend_list[row]
View(amends$action_matrix[row][[1]])




#' -----------------------------------------------------
## 3. calculate Levenshtein distance (word level) ------
#' -----------------------------------------------------

amends$worddist <- NA
amends$worddist_weighted <- NA

#' ---------------------------------
## 3.1 reduce lists to strings -----
#' ---------------------------------

amends$artikel_list_hypo_string <- NA
for (row in 1:nrow(amends)) {
  
  # unlist strings
  x <- unlist(amends$artikel_list_hypo[row])
  names(x) <- NULL
  
  # delete all metacharacters
  x <- gsub(paste(str_c(letters, letters, letters, "[)]"), collapse = "|"), "", x) #subsubletters
  x <- gsub(paste(str_c(letters, letters, "[)]"), collapse = "|"), "", x) #subletters
  x <- gsub(paste(str_c(letters, "[)]"), collapse = "|"), "", x) #letters
  x <- gsub(paste(str_c("^Artikel ", 1:200), collapse="|"), "", x) #articles
  x <- gsub(paste(str_c(0:200, "."), collapse = "|"), "", x) #numbers
  x <- gsub("[§]", "", x) #paragraphs
  x <- gsub(paste(str_c("[(]", 1:200, "[)]"), collapse = "|"), "", x) #absätze
  x <- gsub("„|“|[.]|[,]|[;]|[:]|[(]|[)]|[/]|[–]|[…]", "", x) #punctuation
  x <- gsub('[[:punct:] ]+',' ',x)
  x <- gsub("1[0-9][0-9]", "", x) #numbers
  x <- gsub("2[0-4][0-9]", "", x) #numbers
  x <- gsub("25[0-5]", "", x) #numbers
  x_string <- paste(x, collapse = " ") #collapse to string
  amends$artikel_list_hypo_string[row] <- gsub("\\s+", " ", x_string) # delete any white space which is more than one
}

bills$artikel_list_string <- NA
for (row in 1:nrow(bills)) {
  
  # unlist strings
  x <- unlist(bills$artikel_list[row])
  names(x) <- NULL
  
  # delete all metacharacters
  x <- gsub(paste(str_c(letters, letters, letters, "[)]"), collapse = "|"), "", x) #subsubletters
  x <- gsub(paste(str_c(letters, letters, "[)]"), collapse = "|"), "", x) #subletters
  x <- gsub(paste(str_c(letters, "[)]"), collapse = "|"), "", x) #letters
  x <- gsub(paste(str_c("^Artikel ", 1:200), collapse="|"), "", x) #articles
  x <- gsub(paste(str_c(0:200, "."), collapse = "|"), "", x) #numbers
  x <- gsub("[§]", "", x) #paragraphs
  x <- gsub(paste(str_c("[(]", 1:200, "[)]"), collapse = "|"), "", x) #absätze
  x <- gsub("„|“|[.]|[,]|[;]|[:]|[(]|[)]|[/]|[–]|[…]", "", x) #punctuation
  x <- gsub('[[:punct:] ]+',' ',x)
  x <- gsub("1[0-9][0-9]", "", x) #numbers
  x <- gsub("2[0-4][0-9]", "", x) #numbers
  x <- gsub("25[0-5]", "", x) #numbers
  x_string <- paste(x, collapse = " ") #collapse to string
  bills$artikel_list_string[row] <- gsub("\\s+", " ", x_string) # delete any white space which is more than one
}


#' -----------------------------------
## 3.2 calculate string distance -----
#' -----------------------------------
ld_error <- rep(0, nrow(amends))

for (row in 284:nrow(amends)) {
  
  tryCatch({
    
    # define rows
    amend_row <- row
    bills_row <- which(bills$Drucksache==amends$idDrucksacheLegis[amend_row])
    
    # define strings
    x_string <- amends$artikel_list_hypo_string[amend_row]
    y_string <- bills$artikel_list_string[bills_row]
    
    # split up string1
    str1 <- strsplit(as.character(x_string), "\\s+")[[1]]
    if (length(which(str1=="")) > 0)
      str1 <- str1[-which(str1=="")]
    
    # split up string2
    str2 <- strsplit(as.character(y_string), "\\s+")[[1]]
    if (length(which(str2=="")) > 0)
      str2 <- str2[-which(str2=="")]
    
    text <- str1
    text2 <- str2
    
    text_merged <- c(text, text2)
    text_merged_num <- as.numeric(as.factor(text_merged))
    
    text_num <- rep(NA, length(text))
    text_num2 <- rep(NA, length(text2))
    
    for (i in 1:length(text))
      text_num[i] <- text_merged_num[which(text[i] == text_merged)[1]]
    
    for (i in 1:length(text2))
      text_num2[i] <- text_merged_num[which(text2[i] == text_merged)[1]]
    
    
    
    # word distance
    amends$worddist[amend_row] <- adist(intToUtf8(text_num2), intToUtf8(text_num), counts=TRUE)
    
    # weighted word distance, every deletion gets value of -1
    amends$worddist_weighted[amend_row] <-
      attributes(adist(intToUtf8(text_num2), intToUtf8(text_num), counts=TRUE))[[2]][1] +
      attributes(adist(intToUtf8(text_num2), intToUtf8(text_num), counts=TRUE))[[2]][3] -
      attributes(adist(intToUtf8(text_num2), intToUtf8(text_num), counts=TRUE))[[2]][2]
    
  }, error = function(e){
    
    ld_error[row] <<- 1
    
  }) # end tryCatch
  
  saveRDS(amends, file = "amends.rds")
  print(row)
}


#' ------------------------------------
## 4. pre-prepare CMP & EES data ------
#' ------------------------------------

#' -----------------
# 4.1 CMP data -----
#' -----------------

cmp <- read.csv("MPDataset_MPDS2020b.csv")
cmp <- cmp %>% 
  select(1:81) %>% 
  filter(countryname == "Germany", 
         edate == "22/09/2013" | edate == "24/09/2017") %>% 
  rowwise() %>% 
  mutate(cap1_share = sum(per404, per408, per410, per415, per416), # macroeconomy 
         cap2_share = sum(per201, per202, per503), # civil rights
         cap3_share = sum(per504, per505), # healthcare
         cap4_share = per703, # agriculture
         cap5_share = sum(per701, per702, per704, per601, per602, per607, per608), # labor, employment, immigration
         cap6_share = sum(per506, per507), # education
         cap7_share = sum(per416, per501), # environment
         cap8_share = per501, # energy
         cap10_share = per411, # transportation
         cap12_share = per605, # law, crime and family issues
         cap13_share = sum(per503, per504, per505, per705, per706), # social welfare
         cap14_share = sum(per504, per505), # community development and housing issues
         cap15_share = sum(per401, per402, per403, per405, per409, per412, per414), # banking, finance, and domestic commerce
         cap16_share = sum(per103, per104, per105, per106), # defense
         cap17_share = per411, # space, science, technology and communications 
         cap18_share = sum(per406, per407), # foreign trade
         cap19_share = sum(per101, per102, per107, per108, per109, per110), # international affairs
         cap20_share = sum(per203, per204, per301, per302, per303, per304, per305, per413), # government operations
         cap21_share = per501, # public lands
         cap24_share = sum(per301, per302) # state and local government administration
  )

#' -----------------
# 4.2 EES data -----
#' -----------------

#' --------------------
# 4.2.1 EES 2019 ------
#' --------------------

ees19 <- read.dta13("ees19_ZA7581_v1-0-0.dta")
ees19 <- ees19 %>% 
  filter(countrycode == 1276) %>% 
  select(Q1_1, Q9, Q25, Q26)

ees19$last_vote <- NA
ees19$last_vote[ees19$Q9==801] <- "Union"
ees19$last_vote[ees19$Q9==802] <- "SPD"
ees19$last_vote[ees19$Q9==803] <- "FDP"
ees19$last_vote[ees19$Q9==804] <- "Gruene"
ees19$last_vote[ees19$Q9==805] <- "Linke"
ees19$last_vote[ees19$Q9==806] <- "AfD"

ees19$party_close <- NA
ees19$party_close[ees19$Q25==802] <- "Union"
ees19$party_close[ees19$Q25==803] <- "SPD"
ees19$party_close[ees19$Q25==806] <- "FDP"
ees19$party_close[ees19$Q25==804] <- "Gruene"
ees19$party_close[ees19$Q25==805] <- "Linke"
ees19$party_close[ees19$Q25==808] <- "AfD"

ees19$how_close <- NA
ees19$how_close[ees19$Q26==1] <- "very close"
ees19$how_close[ees19$Q26==2] <- "fairly close"
ees19$how_close[ees19$Q26==3] <- "merely a sympathiser"

ees19$problem1 <- ees19$Q1_1
ees19$problem2 <- NA
ees19$tomorrow_vote <- NA
ees19 <- ees19 %>% 
  select(problem1, problem2, last_vote, tomorrow_vote, party_close, how_close)
ees19$wave <- 19


#' --------------------
# 4.2.2 EES 2014 ------
#' --------------------

ees14 <- read.dta13("ees14_ZA5160_v4-0-0.dta")
ees14 <- ees14 %>% 
  filter(countrycode == "Germany") %>% 
  select(qpp1aO, qpp1bO, qpp5, qpp6, qpp21, qpp22) 

ees14$last_vote <- NA
ees14$last_vote[ees14$qpp5=="Party 1"] <- "Union"
ees14$last_vote[ees14$qpp5=="Party 3"] <- "SPD"
ees14$last_vote[ees14$qpp5=="Party 4"] <- "FDP"
ees14$last_vote[ees14$qpp5=="Party 5"] <- "Gruene"
ees14$last_vote[ees14$qpp5=="Party 7"] <- "Linke"
ees14$last_vote[ees14$qpp5=="Party 8"] <- "AfD"

ees14$tomorrow_vote <- NA
ees14$tomorrow_vote[ees14$qpp6=="Party 1"] <- "Union"
ees14$tomorrow_vote[ees14$qpp6=="Party 2"] <- "SPD"
ees14$tomorrow_vote[ees14$qpp6=="Party 5"] <- "FDP"
ees14$tomorrow_vote[ees14$qpp6=="Party 3"] <- "Gruene"
ees14$tomorrow_vote[ees14$qpp6=="Party 4"] <- "Linke"
ees14$tomorrow_vote[ees14$qpp6=="Party 7"] <- "AfD"

ees14$party_close <- NA
ees14$party_close[ees14$qpp21=="Party 2"] <- "Union"
ees14$party_close[ees14$qpp21=="Party 3"] <- "SPD"
ees14$party_close[ees14$qpp21=="Party 6"] <- "FDP"
ees14$party_close[ees14$qpp21=="Party 4"] <- "Gruene"
ees14$party_close[ees14$qpp21=="Party 5"] <- "Linke"
ees14$party_close[ees14$qpp21=="Party 8"] <- "AfD"

ees14$how_close <- ees14$qpp22
ees14$problem1 <- ees14$qpp1aO
ees14$problem2 <- ees14$qpp1bO
ees14 <- ees14 %>% 
  select(problem1, problem2, last_vote, tomorrow_vote, party_close, how_close) 
ees14$wave <- 14

ees1419 <- rbind(ees14, ees19)


#' ----------------------------------------
## 5. bring dataset into final shape ------
#' ----------------------------------------

# currently I have all bills in there - I could reduce it to only gov bills like in the first paper
# --------------------------------------------------------------------------------------------------------

#' -----------------------------------
# 5.1 load manually prepared data ----
#' -----------------------------------

bills_manual <- read.delim("U:/SFB 884, C7/C7 Subproject GER NED/final_ziad/gesetzentwuerfe/Phase IV_List/GE_reduced_1819_manual.txt")    
bills <- cbind(bills, bills_manual[,7:15])
amends <- readRDS("amends.rds")
ees1419 <- read.csv2("ees1419.csv")   

######### at which stage do I delete those rows in amends that are not really amendments?

rm(list=setdiff(ls(), c("amends", "bills", "cmp", "ees1419")))


#' ------------------------------------
# 5.2 construct bill x party level ---- (only opposition parties)
#' ------------------------------------     

# legislative period identifier
wp18 <- which(bills$LP=="18")
wp19 <- which(bills$LP=="19")

# construct bill x party level
billxparty <- rbind(bills[wp18,][rep(seq_len(nrow(bills[wp18,])), each=2),], 
                    bills[wp19,][rep(seq_len(nrow(bills[wp19,])), each=4),])
rownames(billxparty) <- 1:nrow(billxparty)

billxparty$party <- c(rep(c("GRUENE", "Linke"), length(wp18)), 
                      rep(c("GRUENE", "Linke", "FDP", "AfD"), length(wp19))
)
billxparty <- billxparty %>% 
  relocate(ID, filename, Drucksache, party)


#' ---------------------------
# 5.3 dependent variables ----
#' ---------------------------    

########## for these amendments, there are no bills
########## check if these are real amendments. if yes, trace bills and include
########## for now just delete
amends <- amends[-which(!is.element(amends$idDrucksacheLegis, billxparty$Drucksache)),]


billxparty$worddist <- 0
billxparty$worddist_weighted <- 0

# construct word distances
for (row in 1:nrow(billxparty)) {
  
  if (billxparty$party[row] == "GRUENE") 
    amend_ids <- which(amends$idDrucksacheLegis == billxparty$Drucksache[row] & amends$GRUENE == 1)
  if (billxparty$party[row] == "Linke") 
    amend_ids <- which(amends$idDrucksacheLegis == billxparty$Drucksache[row] & amends$Linke == 1)
  if (billxparty$party[row] == "AfD") 
    amend_ids <- which(amends$idDrucksacheLegis == billxparty$Drucksache[row] & amends$AfD == 1)
  if (billxparty$party[row] == "FDP") 
    amend_ids <- which(amends$idDrucksacheLegis == billxparty$Drucksache[row] & amends$FDP == 1)
  
  if (length(amend_ids) > 0) {
    billxparty$worddist[row] <- sum(amends$worddist[amend_ids])
    billxparty$worddist_weighted[row] <- sum(amends$worddist_weighted[amend_ids])
  }
  
}


#' ----------------------------------
# 5.4 main independent variables ----
#' ----------------------------------   

#' ------------------------------------------
# share of manifesto on policy field of bill 
#' ------------------------------------------

cmp$LP <- NA
cmp$LP[cmp$date == "201309"] <- 18
cmp$LP[cmp$date == "201709"] <- 19
cmp$partyabbrev[cmp$partyabbrev=="90/Greens"] <- "GRUENE"
cmp$partyabbrev[cmp$partyabbrev=="LINKE"] <- "Linke"

billxparty$cmp_share <- NA    

for (row in 1:nrow(billxparty)) {
  
  cmp_row <- which(cmp$LP == billxparty$LP[row] & cmp$partyabbrev == billxparty$party[row])
  command <- str_c("billxparty$cmp_share[row] <- cmp$cap", billxparty$cap1_code[row], "_share[cmp_row]")
  
  eval(parse(text=command))  
  
}

### ich könnte die 0er noch zuordnen
####### irgendwas an cmp-share ändern?



#' ----------------------------------------------------------------
# salience attribution of voter base
# share of supporters that deem field to be most important problem
#' ----------------------------------------------------------------
ees1419$last_vote[ees1419$last_vote == "Gruene"] <- "GRUENE"
ees1419$tomorrow_vote[ees1419$tomorrow_vote == "Gruene"] <- "GRUENE"
ees1419$party_close[ees1419$party_close == "Gruene"] <- "GRUENE"

ees1419$LP <- NA
ees1419$LP[ees1419$wave==14] <- 18
ees1419$LP[ees1419$wave==19] <- 19

salience_party_close <- ees1419 %>% 
  group_by(party_close, LP) %>% 
  summarise(most_important = names(sort(-table(problem1_cap1_code)))[1], 
            second_important = names(sort(-table(problem1_cap1_code)))[2], 
            third_important = names(sort(-table(problem1_cap1_code)))[3])  

salience_tomorrow_vote <- ees1419 %>% 
  group_by(tomorrow_vote, LP) %>% 
  summarise(most_important = names(sort(-table(problem1_cap1_code)))[1], 
            second_important = names(sort(-table(problem1_cap1_code)))[2], 
            third_important = names(sort(-table(problem1_cap1_code)))[3])  

salience_last_vote <- ees1419 %>% 
  group_by(last_vote, LP) %>% 
  summarise(most_important = names(sort(-table(problem1_cap1_code)))[1], 
            second_important = names(sort(-table(problem1_cap1_code)))[2], 
            third_important = names(sort(-table(problem1_cap1_code)))[3])

billxparty$voter_share1 <- billxparty$voter_share2 <- billxparty$voter_share3 <- 
  billxparty$voter_share4 <- billxparty$voter_share5 <- billxparty$voter_share6 <- NA
billxparty$voter_share7 <- billxparty$voter_share8 <- billxparty$voter_share9 <- 
  billxparty$voter_share10 <- billxparty$voter_share11 <- billxparty$voter_share12 <- 0

for (row in 1:nrow(billxparty)) {
  
  # quantitative measure, only cap1_code, party_close
  billxparty$voter_share1[row] <- length(which(ees1419[ees1419$party_close == billxparty$party[row] & ees1419$LP == billxparty$LP[row], "problem1_cap1_code"] == billxparty$cap1_code[row])) /
    nrow(ees1419[ees1419$party_close == billxparty$party[row] & ees1419$LP == billxparty$LP[row],])
  # quantitative measure, only cap1_code, tomorrow_vote
  billxparty$voter_share2[row] <- length(which(ees1419[ees1419$tomorrow_vote == billxparty$party[row] & ees1419$LP == billxparty$LP[row], "problem1_cap1_code"] == billxparty$cap1_code[row])) /
    nrow(ees1419[ees1419$tomorrow_vote == billxparty$party[row] & ees1419$LP == billxparty$LP[row],])
  # quantitative measure, only cap1_code, last_vote
  billxparty$voter_share3[row] <- length(which(ees1419[ees1419$last_vote == billxparty$party[row] & ees1419$LP == billxparty$LP[row], "problem1_cap1_code"] == billxparty$cap1_code[row])) /
    nrow(ees1419[ees1419$last_vote == billxparty$party[row] & ees1419$LP == billxparty$LP[row],])
  
  # quantitative measure, cap1_code or cap2_code, party_close
  billxparty$voter_share4[row] <- 
    (length(which(ees1419[ees1419$party_close == billxparty$party[row] & ees1419$LP == billxparty$LP[row], "problem1_cap1_code"] == billxparty$cap1_code[row])) +
       length(which(ees1419[ees1419$party_close == billxparty$party[row] & ees1419$LP == billxparty$LP[row], "problem1_cap2_code"] == billxparty$cap1_code[row])) ) /
    nrow(ees1419[ees1419$party_close == billxparty$party[row] & ees1419$LP == billxparty$LP[row],])
  # quantitative measure, cap1_code or cap2_code, tomorrow_vote
  billxparty$voter_share5[row] <- 
    (length(which(ees1419[ees1419$tomorrow_vote == billxparty$party[row] & ees1419$LP == billxparty$LP[row], "problem1_cap1_code"] == billxparty$cap1_code[row])) +
       length(which(ees1419[ees1419$tomorrow_vote == billxparty$party[row] & ees1419$LP == billxparty$LP[row], "problem1_cap2_code"] == billxparty$cap1_code[row])) ) /
    nrow(ees1419[ees1419$tomorrow_vote == billxparty$party[row] & ees1419$LP == billxparty$LP[row],])
  # quantitative measure, cap1_code or cap2_code, last_vote
  billxparty$voter_share6[row] <- 
    (length(which(ees1419[ees1419$last_vote == billxparty$party[row] & ees1419$LP == billxparty$LP[row], "problem1_cap1_code"] == billxparty$cap1_code[row])) +
       length(which(ees1419[ees1419$last_vote == billxparty$party[row] & ees1419$LP == billxparty$LP[row], "problem1_cap2_code"] == billxparty$cap1_code[row])) ) /
    nrow(ees1419[ees1419$last_vote == billxparty$party[row] & ees1419$LP == billxparty$LP[row],])
  
  # categorical measure 1 field, only cap1_code, party_close
  if(billxparty$party[row]=="AfD" & billxparty$LP[row] == 18 & billxparty$cap1_code[row] == 1) billxparty$voter_share7[row] <- 1
  if(billxparty$party[row]=="AfD" & billxparty$LP[row] == 19 & billxparty$cap1_code[row] == 5) billxparty$voter_share7[row] <- 1
  if(billxparty$party[row]=="FDP" & billxparty$LP[row] == 18 & billxparty$cap1_code[row] == 1) billxparty$voter_share7[row] <- 1
  if(billxparty$party[row]=="FDP" & billxparty$LP[row] == 19 & billxparty$cap1_code[row] == 5) billxparty$voter_share7[row] <- 1
  if(billxparty$party[row]=="GRUENE" & billxparty$LP[row] == 18 & billxparty$cap1_code[row] == 1) billxparty$voter_share7[row] <- 1
  if(billxparty$party[row]=="GRUENE" & billxparty$LP[row] == 19 & billxparty$cap1_code[row] == 7) billxparty$voter_share7[row] <- 1
  if(billxparty$party[row]=="Linke" & billxparty$LP[row] == 18 & billxparty$cap1_code[row] == 1) billxparty$voter_share7[row] <- 1
  if(billxparty$party[row]=="Linke" & billxparty$LP[row] == 19 & billxparty$cap1_code[row] == 13) billxparty$voter_share7[row] <- 1
  # categorical measure 1 field, only cap1_code, last_vote
  if(billxparty$party[row]=="AfD" & billxparty$LP[row] == 18 & billxparty$cap1_code[row] == 1) billxparty$voter_share8[row] <- 1
  if(billxparty$party[row]=="AfD" & billxparty$LP[row] == 19 & billxparty$cap1_code[row] == 5) billxparty$voter_share8[row] <- 1
  if(billxparty$party[row]=="FDP" & billxparty$LP[row] == 18 & billxparty$cap1_code[row] == 1) billxparty$voter_share8[row] <- 1
  if(billxparty$party[row]=="FDP" & billxparty$LP[row] == 19 & billxparty$cap1_code[row] == 5) billxparty$voter_share8[row] <- 1
  if(billxparty$party[row]=="GRUENE" & billxparty$LP[row] == 18 & billxparty$cap1_code[row] == 1) billxparty$voter_share8[row] <- 1
  if(billxparty$party[row]=="GRUENE" & billxparty$LP[row] == 19 & billxparty$cap1_code[row] == 7) billxparty$voter_share8[row] <- 1
  if(billxparty$party[row]=="Linke" & billxparty$LP[row] == 18 & billxparty$cap1_code[row] == 1) billxparty$voter_share8[row] <- 1
  if(billxparty$party[row]=="Linke" & billxparty$LP[row] == 19 & billxparty$cap1_code[row] == 13) billxparty$voter_share8[row] <- 1
  
  # categorical measure 2 fields, only cap1_code, party_close
  if(billxparty$party[row]=="AfD" & billxparty$LP[row] == 18 & (billxparty$cap1_code[row] == 1 | billxparty$cap1_code[row] == 1)) billxparty$voter_share9[row] <- 1
  if(billxparty$party[row]=="AfD" & billxparty$LP[row] == 19 & (billxparty$cap1_code[row] == 5 | billxparty$cap1_code[row] == 13)) billxparty$voter_share9[row] <- 1
  if(billxparty$party[row]=="FDP" & billxparty$LP[row] == 18 & (billxparty$cap1_code[row] == 1 | billxparty$cap1_code[row] == 19)) billxparty$voter_share9[row] <- 1
  if(billxparty$party[row]=="FDP" & billxparty$LP[row] == 19 & (billxparty$cap1_code[row] == 5 | billxparty$cap1_code[row] == 7)) billxparty$voter_share9[row] <- 1
  if(billxparty$party[row]=="GRUENE" & billxparty$LP[row] == 18 & (billxparty$cap1_code[row] == 1 | billxparty$cap1_code[row] == 5)) billxparty$voter_share9[row] <- 1
  if(billxparty$party[row]=="GRUENE" & billxparty$LP[row] == 19 & (billxparty$cap1_code[row] == 7 | billxparty$cap1_code[row] == 2)) billxparty$voter_share9[row] <- 1
  if(billxparty$party[row]=="Linke" & billxparty$LP[row] == 18 & (billxparty$cap1_code[row] == 1 | billxparty$cap1_code[row] == 5)) billxparty$voter_share9[row] <- 1
  if(billxparty$party[row]=="Linke" & billxparty$LP[row] == 19 & (billxparty$cap1_code[row] == 13 | billxparty$cap1_code[row] == 7)) billxparty$voter_share9[row] <- 1
  # categorical measure 2 fields, only cap1_code, last_vote
  if(billxparty$party[row]=="AfD" & billxparty$LP[row] == 18 & (billxparty$cap1_code[row] == 1 | billxparty$cap1_code[row] == 5)) billxparty$voter_share10[row] <- 1
  if(billxparty$party[row]=="AfD" & billxparty$LP[row] == 19 & (billxparty$cap1_code[row] == 5 | billxparty$cap1_code[row] == 13)) billxparty$voter_share10[row] <- 1
  if(billxparty$party[row]=="FDP" & billxparty$LP[row] == 18 & (billxparty$cap1_code[row] == 5 | billxparty$cap1_code[row] == 13)) billxparty$voter_share10[row] <- 1
  if(billxparty$party[row]=="FDP" & billxparty$LP[row] == 19 & (billxparty$cap1_code[row] == 1 | billxparty$cap1_code[row] == 7)) billxparty$voter_share10[row] <- 1
  if(billxparty$party[row]=="GRUENE" & billxparty$LP[row] == 18 & (billxparty$cap1_code[row] == 1 | billxparty$cap1_code[row] == 5)) billxparty$voter_share10[row] <- 1
  if(billxparty$party[row]=="GRUENE" & billxparty$LP[row] == 19 & (billxparty$cap1_code[row] == 7 | billxparty$cap1_code[row] == 2)) billxparty$voter_share10[row] <- 1
  if(billxparty$party[row]=="Linke" & billxparty$LP[row] == 18 & (billxparty$cap1_code[row] == 1 | billxparty$cap1_code[row] == 5)) billxparty$voter_share10[row] <- 1
  if(billxparty$party[row]=="Linke" & billxparty$LP[row] == 19 & (billxparty$cap1_code[row] == 13 | billxparty$cap1_code[row] == 7)) billxparty$voter_share10[row] <- 1
  
  # categorical measure 3 fields, only cap1_code, party_close
  if(billxparty$party[row]=="AfD" & billxparty$LP[row] == 18 & (billxparty$cap1_code[row] == 1 | billxparty$cap1_code[row] == 1 | billxparty$cap1_code[row] == 1)) billxparty$voter_share11[row] <- 1
  if(billxparty$party[row]=="AfD" & billxparty$LP[row] == 19 & (billxparty$cap1_code[row] == 5 | billxparty$cap1_code[row] == 13 | billxparty$cap1_code[row] == 7)) billxparty$voter_share11[row] <- 1
  if(billxparty$party[row]=="FDP" & billxparty$LP[row] == 18 & (billxparty$cap1_code[row] == 1 | billxparty$cap1_code[row] == 19 | billxparty$cap1_code[row] == 3)) billxparty$voter_share11[row] <- 1
  if(billxparty$party[row]=="FDP" & billxparty$LP[row] == 19 & (billxparty$cap1_code[row] == 5 | billxparty$cap1_code[row] == 7 | billxparty$cap1_code[row] == 13)) billxparty$voter_share11[row] <- 1
  if(billxparty$party[row]=="GRUENE" & billxparty$LP[row] == 18 & (billxparty$cap1_code[row] == 1 | billxparty$cap1_code[row] == 5 | billxparty$cap1_code[row] == 13)) billxparty$voter_share11[row] <- 1
  if(billxparty$party[row]=="GRUENE" & billxparty$LP[row] == 19 & (billxparty$cap1_code[row] == 7 | billxparty$cap1_code[row] == 2 | billxparty$cap1_code[row] == 5)) billxparty$voter_share11[row] <- 1
  if(billxparty$party[row]=="Linke" & billxparty$LP[row] == 18 & (billxparty$cap1_code[row] == 1 | billxparty$cap1_code[row] == 5 | billxparty$cap1_code[row] == 2)) billxparty$voter_share11[row] <- 1
  if(billxparty$party[row]=="Linke" & billxparty$LP[row] == 19 & (billxparty$cap1_code[row] == 13 | billxparty$cap1_code[row] == 7 | billxparty$cap1_code[row] == 5)) billxparty$voter_share11[row] <- 1
  # categorical measure 3 fields, only cap1_code, last_vote
  if(billxparty$party[row]=="AfD" & billxparty$LP[row] == 18 & (billxparty$cap1_code[row] == 1 | billxparty$cap1_code[row] == 5 | billxparty$cap1_code[row] == 19)) billxparty$voter_share12[row] <- 1
  if(billxparty$party[row]=="AfD" & billxparty$LP[row] == 19 & (billxparty$cap1_code[row] == 5 | billxparty$cap1_code[row] == 13 | billxparty$cap1_code[row] == 7)) billxparty$voter_share12[row] <- 1
  if(billxparty$party[row]=="FDP" & billxparty$LP[row] == 18 & (billxparty$cap1_code[row] == 5 | billxparty$cap1_code[row] == 13 | billxparty$cap1_code[row] == 5)) billxparty$voter_share12[row] <- 1
  if(billxparty$party[row]=="FDP" & billxparty$LP[row] == 19 & (billxparty$cap1_code[row] == 1 | billxparty$cap1_code[row] == 7 | billxparty$cap1_code[row] == 13)) billxparty$voter_share12[row] <- 1
  if(billxparty$party[row]=="GRUENE" & billxparty$LP[row] == 18 & (billxparty$cap1_code[row] == 1 | billxparty$cap1_code[row] == 5 | billxparty$cap1_code[row] == 13)) billxparty$voter_share12[row] <- 1
  if(billxparty$party[row]=="GRUENE" & billxparty$LP[row] == 19 & (billxparty$cap1_code[row] == 7 | billxparty$cap1_code[row] == 2 | billxparty$cap1_code[row] == 5)) billxparty$voter_share12[row] <- 1
  if(billxparty$party[row]=="Linke" & billxparty$LP[row] == 18 & (billxparty$cap1_code[row] == 1 | billxparty$cap1_code[row] == 5 | billxparty$cap1_code[row] == 13)) billxparty$voter_share12[row] <- 1
  if(billxparty$party[row]=="Linke" & billxparty$LP[row] == 19 & (billxparty$cap1_code[row] == 13 | billxparty$cap1_code[row] == 7 | billxparty$cap1_code[row] == 5)) billxparty$voter_share12[row] <- 1
  
}


#' -------------------------
# 5.5 control variables ----
#' -------------------------    

#' ----------------------
# size of party group
#' ----------------------

billxparty$groupsize <- NA
billxparty$groupsize[billxparty$LP == 18 & billxparty$party == "GRUENE"] <- 63
billxparty$groupsize[billxparty$LP == 18 & billxparty$party == "Linke"] <- 64
billxparty$groupsize[billxparty$LP == 19 & billxparty$party == "GRUENE"] <- 67
billxparty$groupsize[billxparty$LP == 19 & billxparty$party == "Linke"] <- 69
billxparty$groupsize[billxparty$LP == 19 & billxparty$party == "FDP"] <- 80
billxparty$groupsize[billxparty$LP == 19 & billxparty$party == "AfD"] <- 94


#' ----------------------
# bill importance
#' ----------------------

table(bills$word_count)

#' ---------------------------
# opposition chair committee
#' ---------------------------

billxparty$committee_chair_opp[billxparty$committee_chair_opp=="Not applicable"] <- 0
billxparty$committee_chair_opp[is.na(billxparty$committee_chair_opp)] <- 0

#' -------------------------------
# specific party chair committee
#' -------------------------------

billxparty$party_committee_chair[billxparty$party_committee_chair=="Not applicable"] <- "Changing"
billxparty$party_committee_chair <- as.factor(billxparty$party_committee_chair)


#' -------------------------------
# bill length
#' -------------------------------

billxparty$bill_length <- NA

for (row in 1: nrow(billxparty)) {
  
  #  billxparty$bill_length[row] <- 
  #    length(which(!is.na(str_extract(billxparty$GE_text_merged[row][[1]],  
  #                            str_c("\nArtikel ", 1:300)))))
  
  billxparty$bill_length[row] <- length(billxparty$artikel_list[row][[1]])
  
}
# if there are no articles, perceive law as one article
billxparty$bill_length[which(billxparty$bill_length == 0)] <- 1


#' ----------------------------------
# time until next election (in days)
#' ----------------------------------

billxparty$Datum[billxparty$Drucksache=="18/8246"] <- "27.04.2016"

billxparty$datedist <- NA

billxparty$Datum <- gsub("[.]", "/", billxparty$Datum)
billxparty$Datum <- gsub(" ", "", billxparty$Datum)

billxparty$datedist[billxparty$LP==18] <- as.Date("2017-09-24") - 
  as.Date(as.character(billxparty$Datum[billxparty$LP==18]), format="%d/%m/%Y")

billxparty$datedist[billxparty$LP==19] <- as.Date("2021-09-26") - 
  as.Date(as.character(billxparty$Datum[billxparty$LP==19]), format="%d/%m/%Y")


#' ----------------------------------
# CAP field dummies
#' ----------------------------------
billxparty$capcoding1 <- billxparty$capcoding2 <- billxparty$capcoding3 <- 
  billxparty$capcoding4 <- billxparty$capcoding5 <- billxparty$capcoding6 <- billxparty$cap1_code

billxparty$capcoding2[billxparty$capcoding2==18 | 
                        billxparty$capcoding2==21 |
                        billxparty$capcoding2==24] <- 88

billxparty$capcoding3[billxparty$capcoding3==18 | 
                        billxparty$capcoding3==21 |
                        billxparty$capcoding3==24 |
                        billxparty$capcoding3==6  |
                        billxparty$capcoding3==14] <- 88


billxparty$capcoding4[billxparty$capcoding4==18 | 
                        billxparty$capcoding4==21 |
                        billxparty$capcoding4==24 |
                        billxparty$capcoding4==6  |
                        billxparty$capcoding4==14 |
                        billxparty$capcoding4==16 |
                        billxparty$capcoding4==17] <- 88

billxparty$capcoding5[billxparty$capcoding5==18 | 
                        billxparty$capcoding5==21 |
                        billxparty$capcoding5==24 |
                        billxparty$capcoding5==6  |
                        billxparty$capcoding5==14 |
                        billxparty$capcoding5==16 |
                        billxparty$capcoding5==17 |
                        billxparty$capcoding5==4] <- 88

billxparty$capcoding6[billxparty$capcoding6==15 |
                        billxparty$capcoding6==18] <- 1
billxparty$capcoding6[billxparty$capcoding6==7 |
                        billxparty$capcoding6==8] <- 4
billxparty$capcoding6[billxparty$capcoding6==13] <- 5
billxparty$capcoding6[billxparty$capcoding6==24 |
                        billxparty$capcoding6==21] <- 20


#' ------------------------------
## 6. statistical modeling ------
#' ------------------------------     

#' ---------------------------------------
# 6.0 construct four different data sets
#' ---------------------------------------

# reduce data to government bills 
gov1 <- which(str_detect(billxparty$Unterzeichner, "Bundesregierung"))
gov2 <- which(str_detect(billxparty$Unterzeichner, "CDU"))
gov3 <- which(str_detect(billxparty$Unterzeichner, "CSU"))    
gov4 <- which(str_detect(billxparty$Unterzeichner, "SPD"))   
gov_rows <- unique(c(gov1, gov2, gov3, gov4))  

# data1, gov and opp bills, amended and non-amended
data1 <- billxparty

# data2, gov and opp bills, only amended
data2 <- billxparty[-which(billxparty$worddist_weighted==0),]

# data3, only gov bills, amended and non-amended
data3 <- billxparty[gov_rows,]

# data4, only gov bills, only amended
data4 <- data3[-which(data3$worddist_weighted==0),]




#### missing: other ways to operationalize cmp_share



#### maybe first: bei worddist_weighted die 0er und NAs tracen, make sure I have all variables correct
### -> be aware that some of these will be manually coded by HiWis
#### alternative measurements für cmp_share und und voter_share generieren
#### einen loop schreiben über lauter Modellspezifikationen, inkl. verschiedener 
# - x-measurements 
# - Kontrollvariablen Transformationen (logs)
# - Datensätze (mit 0en, ohne 0en; nur gov-bills vs. alle bills)
#### mit expand.grid für jede mögliche Kombination ein Modell schätzen?

## note: cap1_code ändert die Richtung von cmp_share


#' ---------------------------------------------------
## 6.1 prepare universe of model specifications ------
#' ---------------------------------------------------

cmp_share <- c("cmp_share", "log(cmp_share+0.1)") 
voter_measure <- c(str_c("voter_share", 1:12), str_c("log(voter_share", 1:6, "+0.01)"))
datedist <- c("datedist", "log(datedist)")
wordcount <- c("word_count","log(word_count+1)")
bill_length <- c("bill_length", "log(bill_length)")
groupsize <- c("groupsize", "log(groupsize)")
opposition_chair <- c("committee_chair_opp", "party_committee_chair")
capcoding <- str_c("as.factor(capcoding", 1:6, ")")
data <- str_c("data", 1:4)

# with both cmp_share and voter_share in model
modelspecs_both <- expand.grid(cmp_share, voter_measure, datedist, wordcount, groupsize, opposition_chair, 
                               bill_length, capcoding, data)  
colnames(modelspecs_both) <- c("cmp_share", "voter_measure", "datedist", 
                               "wordcount", "groupsize", "opposition_chair", "bill_length", "capcoding", "data")
modelspecs_both$cmp_share_coef <- modelspecs_both$cmp_share_sdterr <- modelspecs_both$cmp_share_t <- 
  modelspecs_both$voter_share_coef <- modelspecs_both$voter_share_sdterr <- modelspecs_both$voter_share_t <- NA

# only cmp_share in model
modelspecs_cmp <- expand.grid(cmp_share, datedist, wordcount, groupsize, opposition_chair,
                              bill_length, capcoding, data)  
colnames(modelspecs_cmp) <- c("cmp_share", "datedist", 
                              "wordcount", "groupsize", "opposition_chair", "bill_length", "capcoding", "data")
modelspecs_cmp$cmp_share_coef <- modelspecs_cmp$cmp_share_sdterr <- modelspecs_cmp$cmp_share_t <- NA

# only voter_share in model
modelspecs_voter <- expand.grid(voter_measure, datedist, wordcount, groupsize, opposition_chair,
                                bill_length, capcoding, data)  
colnames(modelspecs_voter) <- c("voter_measure", "datedist", 
                                "wordcount", "groupsize", "opposition_chair", "bill_length", "capcoding", "data")
modelspecs_voter$voter_share_coef <- modelspecs_voter$voter_share_sdterr <- modelspecs_voter$voter_share_t <- NA


# save this data needed to run models
save.image(file = "modelingData.RData")


#' ---------------------
## 6.2 run models ------
#' ---------------------

modelspecs_both$break_down <- F
for (row in 1:nrow(modelspecs_both)) {
  
  tryCatch({
    
    command <- str_c("model <- summary(lmer(paste('worddist_weighted', paste(c(as.character(as.matrix(modelspecs_both[row, 1])), as.character(as.matrix(modelspecs_both[row, 2:8])), '(1 | Drucksache)'), collapse='+'), sep='~'), data=", modelspecs_both$data[row], "))")
    eval(parse(text=command))  
    
    modelspecs_both[row ,c("cmp_share_coef", "cmp_share_sdterr", "cmp_share_t")] <- model$coefficients[2,]
    modelspecs_both[row ,c("voter_share_coef", "voter_share_sdterr", "voter_share_t")] <- model$coefficients[3,]
    
    saveRDS(modelspecs_both, file = "modelspecs_both.rds")
    print(str_c("row ", row, " of ", nrow(modelspecs_both), " in modelspecs_both"))
    
  }, error = function(e) {
    
    modelspecs_both$break_down[row] <<- T
    
  }) # end tryCatch  
  
}

# for row in 1:nrow(modelspecs_cmp)
modelspecs_cmp$break_down <- F
for (row in 1:nrow(modelspecs_cmp)) {
  
  tryCatch({
    
    command <- str_c("model <- summary(lmer(paste('worddist_weighted', paste(c(as.character(as.matrix(modelspecs_cmp[row, 1])), as.character(as.matrix(modelspecs_cmp[row, 2:7])), '(1 | Drucksache)'), collapse='+'), sep='~'), data=", modelspecs_cmp$data[row], "))")
    eval(parse(text=command))  
    
    modelspecs_cmp[row ,c("cmp_share_coef", "cmp_share_sdterr", "cmp_share_t")] <- model$coefficients[2,]
    
    saveRDS(modelspecs_cmp, file = "modelspecs_cmp.rds")
    print(str_c("row ", row, " of ", nrow(modelspecs_cmp), " in modelspecs_cmp"))
    
  }, error = function(e) {
    
    modelspecs_cmp$break_down[row] <<- T
    
  }) # end tryCatch  
  
}


# for row in 1:nrow(modelspecs_voter)
modelspecs_voter$break_down <- F
for (row in 1:nrow(modelspecs_voter)) {
  
  tryCatch({
    
    command <- str_c("model <- summary(lmer(paste('worddist_weighted', paste(c(as.character(as.matrix(modelspecs_voter[row, 1])), as.character(as.matrix(modelspecs_voter[row, 2:7])), '(1 | Drucksache)'), collapse='+'), sep='~'), data=", modelspecs_voter$data[row], "))")
    eval(parse(text=command))  
    
    modelspecs_voter[row ,c("voter_share_coef", "voter_share_sdterr", "voter_share_t")] <- model$coefficients[2,]
    
    saveRDS(modelspecs_voter, file = "modelspecs_voter.rds")
    print(str_c("row ", row, " of ", nrow(modelspecs_voter), " in modelspecs_voter"))
    
  }, error = function(e) {
    
    modelspecs_voter$break_down[row] <<- T
    
  }) # end tryCatch  
  
}


#' -----------------------------------------------
## 6.3 construct final model specifications ------
#' -----------------------------------------------

# since opposition bills receive almost no amendment proposals (compare nrow(data2) and nrow(data4)), 
# I only use gov bills, both amended and non-amended ones: data3

modelspecs_both <- readRDS("modelspecs_both.rds")
modelspecs_both <- modelspecs_both %>% 
  filter(data == "data3",
         cmp_share_t > 0)

modelspecs_cmp <- readRDS("modelspecs_cmp.rds")
modelspecs_cmp <- modelspecs_cmp %>% 
  filter(data == "data3",
         cmp_share_t > 0)

modelspecs_voter <- readRDS("modelspecs_voter.rds")
modelspecs_voter <- modelspecs_voter %>% 
  filter(data == "data3",
         voter_share_t > 1)

load("modelingData.Rdata")


# m1: only cmp_share
m1 <- lmer(worddist_weighted ~ cmp_share + 
             groupsize + committee_chair_opp + log(datedist) + log(word_count+1) + log(bill_length) + party + 
             (1 | Drucksache), data = data3)

# m2: only voter_share
m2 <- lmer(worddist_weighted ~ voter_share7 + 
             groupsize + committee_chair_opp + log(datedist) + log(word_count+1) + log(bill_length) + party + 
             (1 | Drucksache), data = data3)    

# m3: both
m3 <- lmer(worddist_weighted ~ cmp_share + voter_share7 + 
             groupsize + committee_chair_opp + log(datedist) + log(word_count+1) + log(bill_length) + party + 
             (1 | Drucksache), data = data3)    

stargazer(m1, m2, m3, type = "latex", star.cutoffs = c(.05, .01, .001))

#' ----------------------------
## 6.4 visualize effects ------
#' ----------------------------

#' ----------------------------------
## 6.4.1 effect of voter_share ------
#' ----------------------------------

# get coefficients and variances
coef <- coef(m3)
coef <- colMeans(coef$Drucksache)
vhat <- vcov(m3)
sigma <- sqrt(sum(residuals(m3)^2) / (nrow(data3) - length(coef)))  
draws <- mvrnorm(500000, coef, vhat)

# set covariate values, rest to means, field to 'Education', vary over bill importance
mus <- matrix(NA, 
              nrow=500000, 
              ncol=2)
colnames(mus) <- c("salient", "nonsalient")

salient_setting <- c(1, 
                     mean(data3$cmp_share),
                     1,
                     mean(data3$groupsize), 
                     1,
                     mean(log(data3$datedist)),
                     mean(log(data3$word_count+1)), 
                     mean(log(data3$bill_length)), 
                     0, 1, 0)

nonsalient_setting <- c(1, 
                        mean(data3$cmp_share),
                        0,
                        mean(data3$groupsize), 
                        1,
                        mean(log(data3$datedist)),
                        mean(log(data3$word_count+1)), 
                        mean(log(data3$bill_length)), 
                        0, 1, 0)

mus[,1] <- draws %*% as.matrix(salient_setting)
mus[,2] <- draws %*% as.matrix(nonsalient_setting)
colnames(mus) <- c("salient", "non-salient")

plot_mus <- as.data.frame(matrix(NA, nrow=nrow(mus)*2, ncol=2))
plot_mus[,1] <- c(mus[,1], mus[,2])
plot_mus[,2] <- c(rep("Issue Salience\nin Voter Base", nrow(mus)), rep("No Issue Salience\nin Voter Base", nrow(mus)))
colnames(plot_mus) <- c("exp", "Group")

means <- plot_mus %>% 
  group_by(Group) %>% 
  summarize(mean = mean(exp))

tikz('voter_salience.tex', standAlone = TRUE, width=15, height=9)
ggplot(plot_mus, aes(x=exp, fill=Group)) +
  geom_density(alpha=0.4) +
  geom_vline(data=means, aes(xintercept=mean, color=Group),
             linetype="dashed", lwd=1.5) +
  scale_color_grey() + scale_fill_grey() + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),  panel.border = element_blank(), 
        axis.text=element_text(size=25), axis.title=element_text(size=25),
        legend.text=element_text(size=25), legend.title=element_text(size=25)) +
  xlab("Constructiveness of Legislative Review") + ylab("Density")
dev.off()
tools::texi2dvi('voter_salience.tex',pdf=T)
system(paste(getOption('pdfviewer'),'voter_salience.pdf'))



#' --------------------------------
## 6.4.2 effect of cmp_share ------
#' --------------------------------

# get coefficients and variances
coef <- coef(m3)
coef <- colMeans(coef$Drucksache)
vhat <- vcov(m3)
sigma <- sqrt(sum(residuals(m3)^2) / (nrow(data3) - length(coef)))  
draws <- mvrnorm(10000, coef, vhat)

# set covariate values, rest to means, field to 'Education', vary over bill importance
mus <- matrix(NA, 
              nrow=10000, 
              ncol=length(unique(data3$cmp_share)))

for (i in 1:length(unique(data3$cmp_share))) {
  
  setting <- c(1, sort(unique(data3$cmp_share))[i], 1, 
               mean(data3$groupsize), 
               1,
               mean(log(data3$datedist)),
               mean(log(data3$word_count+1)), 
               mean(log(data3$bill_length)), 
               0, 1, 0)
  
  mus[,i] <- draws %*% as.matrix(setting)
  
}

# visualize expected values
ses <- rep(NA, length(unique(data3$cmp_share)))
for (i in 1:ncol(mus)) 
  ses[i] <- sd(mus[,i])



exp_values <- as.data.frame(cbind(colMeans(mus), 
                                  sort(unique(data3$cmp_share)),
                                  ses)
)


colnames(exp_values) <- c("exp", "cmp_share", "sd")
exp_values$exp <- as.numeric(as.character(exp_values$exp))
exp_values$cmp_share <- as.numeric(as.character(exp_values$cmp_share))
exp_values$sd <- as.numeric(as.character(exp_values$sd))

tikz('cmp_share.tex', standAlone = TRUE, width=15, height=9)
ggplot(exp_values, aes(x=cmp_share, y=exp)) +
  geom_smooth(method=lm, color="black", alpha=0.1) +
  geom_ribbon(aes(ymin=exp-sd/2, 
                  ymax=exp+sd/2), alpha=0.1) + 
  # geom_hline(yintercept=0, size=1.1, linetype="dashed", color = "black") +
  geom_rug(sides="lb") + 
  scale_colour_manual(values = c("darkgrey"), name = "", labels = c("", "")) +
  scale_fill_manual(values = c("darkgrey"), name = "", labels = c("", "")) + 
  guides(color=F, fill=F, fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) + 
  # scale_x_continuous(breaks=seq(0, 45, 5), limits=c(0,45),) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),  panel.border = element_blank(), 
        axis.text=element_text(size=25), axis.title=element_text(size=25),
        legend.text=element_text(size=25), legend.title=element_text(size=25)) +
  xlab("Share of Bill Policy Field Among Party Manifesto") + ylab("Constructiveness of Legislative Review") 
dev.off()
tools::texi2dvi('cmp_share.tex',pdf=T)
system(paste(getOption('pdfviewer'),'cmp_share.pdf'))



































