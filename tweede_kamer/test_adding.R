# -------------------------------------------------------
# Tweede Kamer 
# Testing out pipeline performance: adding text
# August 2022
# -------------------------------------------------------

#' ------------------------------------------
# 0. inspecting relevant objects ------------
#' ------------------------------------------

# legistext_NL
# - 1,223 bill proposals
# - variable artikel_list: bill split up in structured list

# amends 
# - 5,750 amendment proposals
# - amend_list: amendment split up in structured list
# - action_matrix: amendment translated into action matrix
# - artikel_list_hypo: counterfactual bill (final) that is to be tested


#' ----------------------------------------------------
# 1. checking amendment proposals, example ------------
#' ----------------------------------------------------

# amend_row = 2
amend_row <- 2 # define amendment number
amends$action_matrix[amend_row] # check action_matrix
                                # in rows 1-2, hinzufuegen = T
                                # ref.art = I, so it refers to article 1
                                # text that should be added is in text.cite
                                # so two text snippets should be added
amends$artikel_list_hypo[amend_row][[1]]$ARTIKEL_I # check whether text was actually added at end of Article I
                                                   # yes, correct
# amend_row = 3
amend_row <- 3 # define amendment number
amends$action_matrix[amend_row] # check action_matrix
                                # in row 1, hinzufuegen = T
                                # ref.art = I, so it refers to article 1
                                # text that should be added is in text.cite
                                # so one text snippet hould be added
amends$artikel_list_hypo[amend_row][[1]]$ARTIKEL_I # check whether text was actually added at end of Article I
                                                   # yes, correct


#' ------------------------------------------------------
# 2. sample 50 amendment proposals at random ------------
#' ------------------------------------------------------

set.seed(12345) # important: execute this command before sampling for reproducability
sample(1:nrow(amends), 50)
