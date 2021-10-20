# -------------------------------
# Counterfactual Bill Proposals
# Deutscher Bundestag 
# weird amendment proposals
# Lion Behrens
# -------------------------------

### run after constructing action_matrix

  # 55 (corrected)
  amends$action_matrix[55][[1]][2, "text.cite"] <- "und Nummer 13"

  # 76 (corrected)
  amends$action_matrix[76][[1]][6:23, "ref.num"] <- "Nummer 2"

  # 92 (corrected)
  # amends$action_matrix[92][[1]][14, "text.cite"] <- "der Absätze 2 und 3"

  # 124 (corrected)
  amends$action_matrix[124][[1]][1,"neufassen"] <- T

  # 131 (corrected)
  amends$action_matrix[131][[1]][1,"ref.art"] <- NA

  # 217 (corrected)
  amends$action_matrix[217][[1]][2,"neufassen"] <- F
  
  # 231 (corrected)
  amends$action_matrix[231][[1]][1,"ref.absatz"] <- NA
  amends$action_matrix[231][[1]][1,"hinzufuegen"] <- T

  # 251 (corrected)
  amends$action_matrix[251][[1]][, "ref.absatz"] <- NA
  amends$action_matrix[251][[1]][4, "ref.satz"] <- NA

  # 305 (corrected)
  amends$action_matrix[305][[1]][1, "neufassen"] <- T
  
  # 391 (corrected)
  amends$action_matrix[391][[1]][5, "ref.buch"] <- "Buchstabe d"

  # 407 (no amendment proposal)
  amends$action_matrix[407][[1]][1,] <- NA









