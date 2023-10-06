createID <-
function(df){
    df$ID <- paste0("v",df$voteringlista..villkor,
                    "j", df$voteringlista.votering.Ja,
                    "n", df$voteringlista.votering.Nej,
                    "f", df$voteringlista.votering.Frånvarande,
                    "a", df$voteringlista.votering.Avstår)
    df
  }
