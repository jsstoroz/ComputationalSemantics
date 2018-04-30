module Lexicon where

type CatLabel = String
type Phon     = String

data Cat      = Cat Phon CatLabel Agreement [Cat]
              deriving Eq

type Agreement = [Feat]

data Feat = Masc  | Fem  | Neutr | MascOrFem 
          | Sg    | Pl 
          | Fst   | Snd  | Thrd 
          | Nom   | AccOrDat 
          | Pers  | Refl | Wh 
          | Past  | Pres | Fut   | Perf | Infl
          | On    | With | By    | To   | From  
          deriving (Eq,Show,Ord)

lexicon :: String -> [Cat]

lexicon "i"   = [Cat "i"   "NP" [Pers,Fst,Sg,Nom]        []]
lexicon "me"  = [Cat "me"  "NP" [Pers,Fst,Sg,AccOrDat]   []]
lexicon "we"  = [Cat "we"  "NP" [Pers,Fst,Pl,Nom]        []]
lexicon "us"  = [Cat "us"  "NP" [Pers,Fst,Pl,AccOrDat]   []]
lexicon "you" = [Cat "you" "NP" [Pers,Snd]               []]
lexicon "he"  = [Cat "he"  "NP" [Pers,Thrd,Sg,Nom,Masc]  []]
lexicon "him" = [Cat "him" "NP" [Pers,Thrd,Sg,AccOrDat,Masc] 
                           []]
lexicon "she" = [Cat "she" "NP" [Pers,Thrd,Sg,Nom,Fem]   []]
lexicon "her" = [Cat "her" "NP" [Pers,Thrd,Sg,AccOrDat,Fem] 
                           []]
lexicon "it"  = [Cat "it"  "NP" [Pers,Thrd,Sg,Neutr]     []]
lexicon "they" = [Cat "they" "NP" [Pers,Thrd,Pl,Nom]     []]
lexicon "them" = [Cat "them" "NP" [Pers,Thrd,Pl,AccOrDat] 
                               []]

lexicon "myself"     = 
 [Cat "myself"     "NP" [Refl,Sg,Fst,AccOrDat] []]
lexicon "ourselves"  = 
 [Cat "ourselves"  "NP" [Refl,Pl,Fst,AccOrDat] []]
lexicon "yourself"   = 
 [Cat "yourself"   "NP" [Refl,Sg,Snd,AccOrDat] []]
lexicon "yourselves" = 
 [Cat "yourselves" "NP" [Refl,Pl,Snd,AccOrDat] []]
lexicon "himself"    = 
 [Cat "himself"    "NP" [Refl,Sg,Thrd,AccOrDat,Masc]  []]
lexicon "herself"    = 
 [Cat "herself"    "NP" [Refl,Sg,Thrd,AccOrDat,Fem]   []]
lexicon "itself"     = 
 [Cat "itself"     "NP" [Refl,Sg,Thrd,AccOrDat,Neutr] []]
lexicon "themselves" = 
 [Cat "themselves" "NP" [Refl,Pl,Thrd,AccOrDat] []]

lexicon "who"     = [Cat "who" "NP"  [Wh,Thrd,MascOrFem] [], 
     Cat "who" "REL" [MascOrFem]         []]
lexicon "whom"    = 
 [Cat "whom" "NP"  [Sg,Wh,Thrd,AccOrDat,MascOrFem] [], 
  Cat "whom" "REL" [Sg,MascOrFem,AccOrDat]         []]
lexicon "what"    = 
 [Cat "what" "NP"  [Wh,Thrd,AccOrDat,Neutr]    []]
lexicon "that"    = [Cat "that"  "REL" []      [], 
                     Cat "that"  "DET" [Sg]    []]
lexicon "which"   = [Cat "which" "REL" [Neutr] [], 
                     Cat "which" "DET" [Wh]    []]

lexicon "snowwhite"    = 
 [Cat "snowwhite"  "NP" [Thrd,Fem,Sg]  []]
lexicon "alice"        = 
 [Cat "alice"      "NP" [Thrd,Fem,Sg]  []]
lexicon "dorothy"      = 
 [Cat "dorothy"    "NP" [Thrd,Fem,Sg]  []]
lexicon "goldilocks"   = 
 [Cat "goldilocks" "NP" [Thrd,Fem,Sg]  []]
lexicon "littlemook"   = 
 [Cat "littlemook" "NP" [Thrd,Masc,Sg] []]
lexicon "atreyu"       = 
 [Cat "atreyu"     "NP" [Thrd,Masc,Sg] []]

lexicon "every"   = [Cat "every"   "DET" [Sg]  []]
lexicon "all"     = [Cat "all"     "DET" [Pl]  []]
lexicon "some"    = [Cat "some"    "DET" []    []]
lexicon "several" = [Cat "several" "DET" [Pl]  []]
lexicon "a"       = [Cat "a"       "DET" [Sg]  []]
lexicon "no"      = [Cat "no"      "DET" []    []]
lexicon "the"     = [Cat "the"     "DET" []    []]

lexicon "most"    = [Cat "most"    "DET" [Pl]  []]
lexicon "many"    = [Cat "many"    "DET" [Pl]  []]
lexicon "few"     = [Cat "few"     "DET" [Pl]  []]
lexicon "this"    = [Cat "this"    "DET" [Sg]  []]
lexicon "these"   = [Cat "these"   "DET" [Pl]  []]
lexicon "those"   = [Cat "those"   "DET" [Pl]  []]

lexicon "less_than" = [Cat "less_than" "DF" [Pl] []]
lexicon "more_than" = [Cat "more_than" "DF" [Pl] []]

lexicon "thing"   = [Cat "thing"   "CN" [Sg,Neutr,Thrd] []]
lexicon "things"  = [Cat "things"  "CN" [Pl,Neutr,Thrd] []]
lexicon "person"  = [Cat "person"  "CN" [Sg,Masc,Thrd]  []]
lexicon "persons" = [Cat "persons" "CN" [Pl,Masc,Thrd]  []]
lexicon "boy"     = [Cat "boy"     "CN" [Sg,Masc,Thrd]  []]
lexicon "boys"    = [Cat "boys"    "CN" [Pl,Masc,Thrd]  []]
lexicon "man"     = [Cat "man"     "CN" [Sg,Masc,Thrd]  []]
lexicon "men"     = [Cat "men"     "CN" [Pl,Masc,Thrd]  []]
lexicon "girl"    = [Cat "girl"    "CN" [Sg,Fem,Thrd]   []]
lexicon "girls"   = [Cat "girls"   "CN" [Pl,Fem,Thrd]   []]
lexicon "woman"   = [Cat "woman"   "CN" [Sg,Fem,Thrd]   []]
lexicon "women"   = [Cat "women"   "CN" [Pl,Fem,Thrd]   []]
lexicon "princess" = [Cat "princess" "CN" [Sg,Fem,Thrd] []]
lexicon "princesses" = [Cat "princesses" "CN" [Pl,Fem,Thrd] []]
lexicon "dwarf"    = [Cat "dwarf"    "CN" [Sg,Masc,Thrd] []]
lexicon "dwarfs"   = [Cat "dwarfs"   "CN" [Pl,Masc,Thrd] []]
lexicon "dwarves"  = [Cat "dwarves"  "CN" [Pl,Masc,Thrd] []]
lexicon "giant"    = [Cat "giant"    "CN" [Sg,Masc,Thrd] []]
lexicon "giants"   = [Cat "giants"   "CN" [Pl,Masc,Thrd] []]

lexicon "wizard"   = [Cat "wizard"   "CN" [Sg,Masc,Thrd]  []]
lexicon "wizards"  = [Cat "wizards"  "CN" [Pl,Masc,Thrd]  []]
lexicon "sword"    = [Cat "sword"    "CN" [Sg,Neutr,Thrd] []]
lexicon "swords"   = [Cat "swords"   "CN" [Pl,Neutr,Thrd] []]
lexicon "dagger"   = [Cat "dagger"   "CN" [Sg,Neutr,Thrd] []]
lexicon "daggers"  = [Cat "daggers"  "CN" [Pl,Neutr,Thrd] []]

lexicon "did"    = [Cat "did"    "AUX" [] []]
lexicon "didn't" = [Cat "didn't" "AUX" [] []]

lexicon "smiled"    = [Cat "smiled"    "VP" [Past] []]
lexicon "smile"     = [Cat "smile"     "VP" [Infl]  []]
lexicon "cheered"   = [Cat "cheered"   "VP" [Past] []]
lexicon "cheer"     = [Cat "cheer"     "VP" [Infl]  []]
lexicon "shuddered" = [Cat "shuddered" "VP" [Past] []]
lexicon "shudder"   = [Cat "shudder"   "VP" [Infl]  []]

lexicon "defeated"       = 
 [Cat "defeated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "defeat"         = 
 [Cat "defeat"   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]

lexicon "gave"         = 
 [Cat "gave" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
                    Cat "_" "PP" [To]       []], 
  Cat "gave" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
                     Cat "_" "NP" [AccOrDat]  []]]
lexicon "give"         = 
 [Cat "give" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [To]       []],
  Cat "give" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "NP" [AccOrDat] []]]
lexicon "kicked" = 
 [Cat "kicked" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
                             Cat "_" "PP" [With]     []], 
  Cat "kicked" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "kick" = 
 [Cat "kick"   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                      Cat "_" "PP" [With]     []], 
  Cat "kick"   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]] 

lexicon "on"   = [Cat "on"   "PREP" [On]   []]
lexicon "with" = [Cat "with" "PREP" [With] []]
lexicon "by"   = [Cat "by"   "PREP" [By]   []]
lexicon "to"   = [Cat "to"   "PREP" [To]   []]
lexicon "from" = [Cat "from" "PREP" [From] []]

lexicon "and"   = [Cat "and"  "CONJ" [] []]
lexicon "."     = [Cat "."    "CONJ" [] []]
lexicon "if"    = [Cat "if"   "COND" [] []]
lexicon "then"  = [Cat "then" "THEN" [] []]

lexicon "shouted"    = [Cat "shouted"    "VP" [Past] []]
lexicon "shout"    = [Cat "shout"    "VP" [Pres,Sg,Fst] [],
    Cat "shout"    "VP" [Pres,Sg,Snd] [],
    Cat "shout"     "VP" [Pres,Pl]  [],
    Cat "shout"     "VP" [Infl]  []]
lexicon "shouts"    = [Cat "shouts"    "VP" [Pres,Sg,Thrd] []]
lexicon "will_shout"    = [Cat "will_shout"    "VP" [Fut] []]
lexicon "have_shouted"    = [Cat "have_shouted"  "VP" [Perf,Sg,Fst] [],
    Cat "have_shouted"     "VP" [Perf,Sg,Snd] [],
    Cat "have_shouted"     "VP" [Perf,Pl] []]
lexicon "has_shouted"   = [Cat "has_shouted"   "VP" [Perf,Sg,Thrd] []]

lexicon "accepted" = [Cat "accepted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "accept" = [Cat "accept" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "accept" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "accept" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "accept" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "accepts" = [Cat "accepts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_accept" = [Cat "will_accept" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_accepted" = [Cat "have_accepted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_accepted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_accepted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_accepted" = [Cat "has_accepted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "approached" = [Cat "approached" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "approach" = [Cat "approach" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "approach" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "approach" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "approach" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "approaches" = [Cat "approaches" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_approach" = [Cat "will_approach" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_approached" = [Cat "have_approached" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_approached" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_approached" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_approached" = [Cat "has_approached" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "despised" = [Cat "despised" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "despise" = [Cat "despise" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "despise" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "despise" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "despise" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "despises" = [Cat "despises" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_despise" = [Cat "will_despise" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_despised" = [Cat "have_despised" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_despised" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_despised" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_despised" = [Cat "has_despised" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "earned" = [Cat "earned" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "earn" = [Cat "earn" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "earn" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "earn" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "earn" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "earns" = [Cat "earns" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_earn" = [Cat "will_earn" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_earned" = [Cat "have_earned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_earned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_earned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_earned" = [Cat "has_earned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "echoed" = [Cat "echoed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "echo" = [Cat "echo" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "echo" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "echo" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "echo" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "echoes" = [Cat "echoes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_echo" = [Cat "will_echo" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_echoed" = [Cat "have_echoed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_echoed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_echoed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_echoed" = [Cat "has_echoed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "implied" = [Cat "implied" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "imply" = [Cat "imply" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "imply" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "imply" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "imply" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "implies" = [Cat "implies" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_imply" = [Cat "will_imply" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_implied" = [Cat "have_implied" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_implied" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_implied" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_implied" = [Cat "has_implied" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "owned" = [Cat "owned" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "own" = [Cat "own" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "own" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "own" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "own" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "owns" = [Cat "owns" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_own" = [Cat "will_own" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_owned" = [Cat "have_owned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_owned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_owned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_owned" = [Cat "has_owned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "pitied" = [Cat "pitied" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "pity" = [Cat "pity" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "pity" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "pity" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "pity" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "pities" = [Cat "pities" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_pity" = [Cat "will_pity" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_pitied" = [Cat "have_pitied" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_pitied" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_pitied" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_pitied" = [Cat "has_pitied" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "sketched" = [Cat "sketched" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "sketch" = [Cat "sketch" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "sketch" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "sketch" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "sketch" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "sketches" = [Cat "sketches" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_sketch" = [Cat "will_sketch" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_sketched" = [Cat "have_sketched" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_sketched" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_sketched" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_sketched" = [Cat "has_sketched" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "reasoned" = [Cat "reasoned" "VP" [Past] [Cat "_" "PP" [With] []]]
lexicon "reason" = [Cat "reason" "VP" [Pres,Sg,Fst] [Cat "_" "PP" [With] []],
    Cat "reason" "VP" [Pres,Sg,Snd] [Cat "_" "PP" [With] []],
    Cat "reason" "VP" [Pres,Pl] [Cat "_" "PP" [With] []],
    Cat "reason" "VP" [Infl] [Cat "_" "PP" [With] []]]
lexicon "reasons" = [Cat "reasons" "VP" [Pres,Sg,Thrd] [Cat "_" "PP" [With] []]]
lexicon "will_reason" = [Cat "will_reason" "VP" [Fut] [Cat "_" "PP" [With] []]]
lexicon "have_reasoned" = [Cat "have_reasoned" "VP" [Perf,Sg,Fst] [Cat "_" "PP" [With] []],
    Cat "have_reasoned" "VP" [Perf,Sg,Snd] [Cat "_" "PP" [With] []],
    Cat "have_reasoned" "VP" [Perf,Pl] [Cat "_" "PP" [With] []]]
lexicon "has_reasoned" = [Cat "has_reasoned" "VP" [Perf,Sg,Thrd] [Cat "_" "PP" [With] []]]

lexicon "recognized" = [Cat "recognized" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "recognize" = [Cat "recognize" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "recognize" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "recognize" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "recognize" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "recognizes" = [Cat "recognizes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_recognize" = [Cat "will_recognize" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_recognized" = [Cat "have_recognized" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_recognized" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_recognized" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_recognized" = [Cat "has_recognized" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "squeezed" = [Cat "squeezed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "squeeze" = [Cat "squeeze" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "squeeze" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "squeeze" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "squeeze" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "squeezes" = [Cat "squeezes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_squeeze" = [Cat "will_squeeze" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_squeezed" = [Cat "have_squeezed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_squeezed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_squeezed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_squeezed" = [Cat "has_squeezed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "solved" = [Cat "solved" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "solve" = [Cat "solve" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "solve" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "solve" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "solve" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "solves" = [Cat "solves" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_solve" = [Cat "will_solve" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_solved" = [Cat "have_solved" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_solved" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_solved" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_solved" = [Cat "has_solved" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "waited" = [Cat "waited" "VP" [Past] []]
lexicon "wait" = [Cat "wait" "VP" [Pres,Sg,Fst] [],
    Cat "wait" "VP" [Pres,Sg,Snd] [],
    Cat "wait" "VP" [Pres,Pl] [],
    Cat "wait" "VP" [Infl] []]
lexicon "waits" = [Cat "waits" "VP" [Pres,Sg,Thrd] []]
lexicon "will_wait" = [Cat "will_wait" "VP" [Fut] []]
lexicon "have_waited" = [Cat "have_waited" "VP" [Perf,Sg,Fst] [],
    Cat "have_waited" "VP" [Perf,Sg,Snd] [],
    Cat "have_waited" "VP" [Perf,Pl] []]
lexicon "has_waited" = [Cat "has_waited" "VP" [Perf,Sg,Thrd] []]

lexicon "urged" = [Cat "urged" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "urge" = [Cat "urge" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "urge" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "urge" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "urge" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "urges" = [Cat "urges" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_urge" = [Cat "will_urge" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_urged" = [Cat "have_urged" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_urged" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_urged" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_urged" = [Cat "has_urged" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "warned" = [Cat "warned" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "warn" = [Cat "warn" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "warn" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "warn" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "warn" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "warns" = [Cat "warns" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_warn" = [Cat "will_warn" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_warned" = [Cat "have_warned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_warned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_warned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_warned" = [Cat "has_warned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "winked" = [Cat "winked" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "wink" = [Cat "wink" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "wink" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "wink" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "wink" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "winks" = [Cat "winks" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_wink" = [Cat "will_wink" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_winked" = [Cat "have_winked" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_winked" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_winked" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_winked" = [Cat "has_winked" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "loved" = [Cat "loved" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "love" = [Cat "love" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "love" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "love" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "love" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "loves" = [Cat "loves" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_love" = [Cat "will_love" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_loved" = [Cat "have_loved" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_loved" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_loved" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_loved" = [Cat "has_loved" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "admired" = [Cat "admired" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "admire" = [Cat "admire" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "admire" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "admire" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "admire" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "admires" = [Cat "admires" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_admire" = [Cat "will_admire" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_admired" = [Cat "have_admired" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_admired" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_admired" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_admired" = [Cat "has_admired" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "laughed" = [Cat "laughed" "VP" [Past] []]
lexicon "laugh" = [Cat "laugh" "VP" [Pres,Sg,Fst] [],
    Cat "laugh" "VP" [Pres,Sg,Snd] [],
    Cat "laugh" "VP" [Pres,Pl] [],
    Cat "laugh" "VP" [Infl] []]
lexicon "laughs" = [Cat "laughs" "VP" [Pres,Sg,Thrd] []]
lexicon "will_laugh" = [Cat "will_laugh" "VP" [Fut] []]
lexicon "have_laughed" = [Cat "have_laughed" "VP" [Perf,Sg,Fst] [],
    Cat "have_laughed" "VP" [Perf,Sg,Snd] [],
    Cat "have_laughed" "VP" [Perf,Pl] []]
lexicon "has_laughed" = [Cat "has_laughed" "VP" [Perf,Sg,Thrd] []]

lexicon "alarmed" = [Cat "alarmed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "alarm" = [Cat "alarm" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "alarm" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "alarm" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "alarm" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "alarms" = [Cat "alarms" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_alarm" = [Cat "will_alarm" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_alarmed" = [Cat "have_alarmed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_alarmed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_alarmed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_alarmed" = [Cat "has_alarmed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "annoyed" = [Cat "annoyed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "annoy" = [Cat "annoy" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "annoy" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "annoy" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "annoy" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "annoys" = [Cat "annoys" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_annoy" = [Cat "will_annoy" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_annoyed" = [Cat "have_annoyed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_annoyed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_annoyed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_annoyed" = [Cat "has_annoyed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "nudged" = [Cat "nudged" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "nudge" = [Cat "nudge" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "nudge" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "nudge" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "nudge" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "nudges" = [Cat "nudges" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_nudge" = [Cat "will_nudge" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_nudged" = [Cat "have_nudged" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_nudged" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_nudged" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_nudged" = [Cat "has_nudged" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "conveyed" = [Cat "conveyed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "convey" = [Cat "convey" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "convey" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "convey" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "convey" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "conveys" = [Cat "conveys" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_convey" = [Cat "will_convey" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_conveyed" = [Cat "have_conveyed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_conveyed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_conveyed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_conveyed" = [Cat "has_conveyed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "dismissed" = [Cat "dismissed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "dismiss" = [Cat "dismiss" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "dismiss" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "dismiss" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "dismiss" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "dismisses" = [Cat "dismisses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_dismiss" = [Cat "will_dismiss" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_dismissed" = [Cat "have_dismissed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_dismissed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_dismissed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_dismissed" = [Cat "has_dismissed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "repeated" = [Cat "repeated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "repeat" = [Cat "repeat" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "repeat" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "repeat" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "repeat" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "repeats" = [Cat "repeats" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_repeat" = [Cat "will_repeat" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_repeated" = [Cat "have_repeated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_repeated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_repeated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_repeated" = [Cat "has_repeated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "got" = [Cat "got" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "get" = [Cat "get" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "get" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "get" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "get" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "gets" = [Cat "gets" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_get" = [Cat "will_get" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_got" = [Cat "have_got" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_got" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_got" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_got" = [Cat "has_got" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "made" = [Cat "made" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "make" = [Cat "make" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "make" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "make" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "make" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "makes" = [Cat "makes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_make" = [Cat "will_make" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_made" = [Cat "have_made" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_made" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_made" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_made" = [Cat "has_made" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "knew" = [Cat "knew" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "know" = [Cat "know" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "know" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "know" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "know" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "knows" = [Cat "knows" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_know" = [Cat "will_know" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_known" = [Cat "have_known" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_known" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_known" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_known" = [Cat "has_known" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "thought" = [Cat "thought" "VP" [Past] [Cat "_" "PP" [On] []]]
lexicon "think" = [Cat "think" "VP" [Pres,Sg,Fst] [Cat "_" "PP" [On] []],
    Cat "think" "VP" [Pres,Sg,Snd] [Cat "_" "PP" [On] []],
    Cat "think" "VP" [Pres,Pl] [Cat "_" "PP" [On] []],
    Cat "think" "VP" [Infl] [Cat "_" "PP" [On] []]]
lexicon "thinks" = [Cat "thinks" "VP" [Pres,Sg,Thrd] [Cat "_" "PP" [On] []]]
lexicon "will_think" = [Cat "will_think" "VP" [Fut] [Cat "_" "PP" [On] []]]
lexicon "have_thought" = [Cat "have_thought" "VP" [Perf,Sg,Fst] [Cat "_" "PP" [On] []],
    Cat "have_thought" "VP" [Perf,Sg,Snd] [Cat "_" "PP" [On] []],
    Cat "have_thought" "VP" [Perf,Pl] [Cat "_" "PP" [On] []]]
lexicon "has_thought" = [Cat "has_thought" "VP" [Perf,Sg,Thrd] [Cat "_" "PP" [On] []]]

lexicon "took" = [Cat "took" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "take" = [Cat "take" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "take" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "take" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "take" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "takes" = [Cat "takes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_take" = [Cat "will_take" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_taken" = [Cat "have_taken" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_taken" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_taken" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_taken" = [Cat "has_taken" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "saw" = [Cat "saw" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "see" = [Cat "see" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "see" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "see" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "see" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "sees" = [Cat "sees" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_see" = [Cat "will_see" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_seen" = [Cat "have_seen" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_seen" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_seen" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_seen" = [Cat "has_seen" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "came" = [Cat "came" "VP" [Past] [Cat "_" "PP" [To] []]]
lexicon "come" = [Cat "come" "VP" [Pres,Sg,Fst] [Cat "_" "PP" [To] []],
    Cat "come" "VP" [Pres,Sg,Snd] [Cat "_" "PP" [To] []],
    Cat "come" "VP" [Pres,Pl] [Cat "_" "PP" [To] []],
    Cat "come" "VP" [Infl] [Cat "_" "PP" [To] []]]
lexicon "comes" = [Cat "comes" "VP" [Pres,Sg,Thrd] [Cat "_" "PP" [To] []]]
lexicon "will_come" = [Cat "will_come" "VP" [Fut] [Cat "_" "PP" [To] []]]
lexicon "have_come" = [Cat "have_come" "VP" [Perf,Sg,Fst] [Cat "_" "PP" [To] []],
    Cat "have_come" "VP" [Perf,Sg,Snd] [Cat "_" "PP" [To] []],
    Cat "have_come" "VP" [Perf,Pl] [Cat "_" "PP" [To] []]]
lexicon "has_come" = [Cat "has_come" "VP" [Perf,Sg,Thrd] [Cat "_" "PP" [To] []]]

lexicon "wanted" = [Cat "wanted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "want" = [Cat "want" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "want" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "want" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "want" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "wants" = [Cat "wants" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_want" = [Cat "will_want" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_wanted" = [Cat "have_wanted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_wanted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_wanted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_wanted" = [Cat "has_wanted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "found" = [Cat "found" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "find" = [Cat "find" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "find" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "find" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "find" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "finds" = [Cat "finds" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_find" = [Cat "will_find" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_found" = [Cat "have_found" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_found" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_found" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_found" = [Cat "has_found" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "told" = [Cat "told" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "tell" = [Cat "tell" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "tell" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "tell" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "tell" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "tells" = [Cat "tells" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_tell" = [Cat "will_tell" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_told" = [Cat "have_told" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_told" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_told" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_told" = [Cat "has_told" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "worked" =
    [Cat "worked" "VP" [Past] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []]]
lexicon "work" =
    [Cat "work" "VP" [Pres,Sg,Fst] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []],
    Cat "work" "VP" [Pres,Sg,Snd] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []],
    Cat "work" "VP" [Pres,Pl] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []],
    Cat "work" "VP" [Pres,Sg,Snd] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []]]
lexicon "works" =
    [Cat "works" "VP" [Pres,Sg,Thrd] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []]]
lexicon "will_work" =
    [Cat "will_work" "VP" [Fut] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []]]
lexicon "have_worked" =
    [Cat "have_worked" "VP" [Perf,Sg,Fst] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []],
    Cat "have_worked" "VP" [Perf,Sg,Snd] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []],
    Cat "have_worked" "VP" [Perf,Pl] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []]]
lexicon "has_worked" =
    [Cat "has_worked" "VP" [Perf,Sg,Thrd] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []]]

lexicon "called" = [Cat "called" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "call" = [Cat "call" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "call" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "call" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "call" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "calls" = [Cat "calls" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_call" = [Cat "will_call" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_called" = [Cat "have_called" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_called" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_called" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_called" = [Cat "has_called" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "tried" = [Cat "tried" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "try" = [Cat "try" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "try" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "try" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "try" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "tries" = [Cat "tries" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_try" = [Cat "will_try" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_tried" = [Cat "have_tried" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_tried" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_tried" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_tried" = [Cat "has_tried" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "asked" = [Cat "asked" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "ask" = [Cat "ask" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "ask" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "ask" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "ask" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "asks" = [Cat "asks" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_ask" = [Cat "will_ask" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_asked" = [Cat "have_asked" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_asked" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_asked" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_asked" = [Cat "has_asked" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "needed" = [Cat "needed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "need" = [Cat "need" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "need" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "need" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "need" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "needs" = [Cat "needs" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_need" = [Cat "will_need" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_needed" = [Cat "have_needed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_needed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_needed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_needed" = [Cat "has_needed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "felt" = [Cat "felt" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "feel" = [Cat "feel" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "feel" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "feel" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "feel" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "feels" = [Cat "feels" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_feel" = [Cat "will_feel" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_felt" = [Cat "have_felt" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_felt" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_felt" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_felt" = [Cat "has_felt" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "became" = [Cat "became" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "become" = [Cat "become" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "become" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "become" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "become" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "becomes" = [Cat "becomes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_become" = [Cat "will_become" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_become" = [Cat "have_become" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_become" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_become" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_become" = [Cat "has_become" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "left" = [Cat "left" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "leave" = [Cat "leave" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "leave" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "leave" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "leave" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "leaves" = [Cat "leaves" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_leave" = [Cat "will_leave" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_left" = [Cat "have_left" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_left" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_left" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_left" = [Cat "has_left" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "kept" = [Cat "kept" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "keep" = [Cat "keep" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "keep" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "keep" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "keep" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "keeps" = [Cat "keeps" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_keep" = [Cat "will_keep" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_kept" = [Cat "have_kept" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_kept" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_kept" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_kept" = [Cat "has_kept" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "began" = [Cat "began" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "begin" = [Cat "begin" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "begin" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "begin" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "begin" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "begins" = [Cat "begins" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_begin" = [Cat "will_begin" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_begun" = [Cat "have_begun" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_begun" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_begun" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_begun" = [Cat "has_begun" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "helped" = [Cat "helped" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "help" = [Cat "help" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "help" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "help" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "help" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "helps" = [Cat "helps" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_help" = [Cat "will_help" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_helped" = [Cat "have_helped" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_helped" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_helped" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_helped" = [Cat "has_helped" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "showed" = [Cat "showed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "show" = [Cat "show" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "show" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "show" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "show" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "shows" = [Cat "shows" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_show" = [Cat "will_show" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_shown" = [Cat "have_shown" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_shown" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_shown" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_shown" = [Cat "has_shown" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "heard" = [Cat "heard" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "hear" = [Cat "hear" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "hear" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "hear" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "hear" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "hears" = [Cat "hears" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_hear" = [Cat "will_hear" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_heard" = [Cat "have_heard" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_heard" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_heard" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_heard" = [Cat "has_heard" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "played" =
    [Cat "played" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []]]
lexicon "play" =
    [Cat "play" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []],
    Cat "play" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []],
    Cat "play" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []],
    Cat "play" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []]]
lexicon "plays" =
    [Cat "plays" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []]]
lexicon "will_play" =
    [Cat "will_play" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []]]
lexicon "have_played" =
    [Cat "have_played" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []],
    Cat "have_played" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []],
    Cat "have_played" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []]]
lexicon "has_played" =
    [Cat "has_played" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []]]

lexicon "ran" = [Cat "ran" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "run" = [Cat "run" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "run" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "run" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "run" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "runs" = [Cat "runs" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_run" = [Cat "will_run" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_run" = [Cat "have_run" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_run" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_run" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_run" = [Cat "has_run" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "moved" =
    [Cat "moved" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]
lexicon "move" =
    [Cat "move" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []],
    Cat "move" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []],
    Cat "move" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []],
    Cat "move" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]
lexicon "moves" =
    [Cat "moves" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]
lexicon "will_move" =
    [Cat "will_move" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]
lexicon "have_moved" =
    [Cat "have_moved" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []],
    Cat "have_moved" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []],
    Cat "have_moved" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]
lexicon "has_moved" =
    [Cat "has_moved" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]

lexicon "lived" = [Cat "lived" "VP" [Past] []]
lexicon "live" = [Cat "live" "VP" [Pres,Sg,Fst] [],
    Cat "live" "VP" [Pres,Sg,Snd] [],
    Cat "live" "VP" [Pres,Pl] [],
    Cat "live" "VP" [Infl] []]
lexicon "lives" = [Cat "lives" "VP" [Pres,Sg,Thrd] []]
lexicon "will_live" = [Cat "will_live" "VP" [Fut] []]
lexicon "have_lived" = [Cat "have_lived" "VP" [Perf,Sg,Fst] [],
    Cat "have_lived" "VP" [Perf,Sg,Snd] [],
    Cat "have_lived" "VP" [Perf,Pl] []]
lexicon "has_lived" = [Cat "has_lived" "VP" [Perf,Sg,Thrd] []]

lexicon "believed" = [Cat "believed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "believe" = [Cat "believe" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "believe" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "believe" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "believe" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "believes" = [Cat "believes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_believe" = [Cat "will_believe" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_believed" = [Cat "have_believed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_believed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_believed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_believed" = [Cat "has_believed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "brought" = [Cat "brought" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "bring" = [Cat "bring" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "bring" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "bring" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "bring" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "brings" = [Cat "brings" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_bring" = [Cat "will_bring" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_brought" = [Cat "have_brought" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_brought" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_brought" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_brought" = [Cat "has_brought" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "happened" = [Cat "happened" "VP" [Past] []]
lexicon "happen" = [Cat "happen" "VP" [Pres,Sg,Fst] [],
    Cat "happen" "VP" [Pres,Sg,Snd] [],
    Cat "happen" "VP" [Pres,Pl] [],
    Cat "happen" "VP" [Infl] []]
lexicon "happens" = [Cat "happens" "VP" [Pres,Sg,Thrd] []]
lexicon "will_happen" = [Cat "will_happen" "VP" [Fut] []]
lexicon "have_happened" = [Cat "have_happened" "VP" [Perf,Sg,Fst] [],
    Cat "have_happened" "VP" [Perf,Sg,Snd] [],
    Cat "have_happened" "VP" [Perf,Pl] []]
lexicon "has_happened" = [Cat "has_happened" "VP" [Perf,Sg,Thrd] []]

lexicon "wrote" =
    [Cat "wrote" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]
lexicon "write" =
    [Cat "write" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []],
    Cat "write" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []],
    Cat "write" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []],
    Cat "write" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]
lexicon "writes" =
    [Cat "writes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]
lexicon "will_write" =
    [Cat "will_write" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]
lexicon "have_written" =
    [Cat "have_written" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []],
    Cat "have_written" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []],
    Cat "have_written" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]
lexicon "has_written" =
    [Cat "has_written" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]

lexicon "sat" = [Cat "sat" "VP" [Past] []]
lexicon "sit" = [Cat "sit" "VP" [Pres,Sg,Fst] [],
    Cat "sit" "VP" [Pres,Sg,Snd] [],
    Cat "sit" "VP" [Pres,Pl] [],
    Cat "sit" "VP" [Infl] []]
lexicon "sits" = [Cat "sits" "VP" [Pres,Sg,Thrd] []]
lexicon "will_sit" = [Cat "will_sit" "VP" [Fut] []]
lexicon "have_sat" = [Cat "have_sat" "VP" [Perf,Sg,Fst] [],
    Cat "have_sat" "VP" [Perf,Sg,Snd] [],
    Cat "have_sat" "VP" [Perf,Pl] []]
lexicon "has_sat" = [Cat "has_sat" "VP" [Perf,Sg,Thrd] []]

lexicon "lost" = [Cat "lost" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "lose" = [Cat "lose" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "lose" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "lose" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "lose" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "loses" = [Cat "loses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_lose" = [Cat "will_lose" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_lost" = [Cat "have_lost" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_lost" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_lost" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_lost" = [Cat "has_lost" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "paid" = [Cat "paid" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "pay" = [Cat "pay" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "pay" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "pay" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "pay" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "pays" = [Cat "pays" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_pay" = [Cat "will_pay" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_paid" = [Cat "have_paid" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_paid" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_paid" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_paid" = [Cat "has_paid" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "met" = [Cat "met" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "meet" = [Cat "meet" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "meet" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "meet" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "meet" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "meets" = [Cat "meets" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_meet" = [Cat "will_meet" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_met" = [Cat "have_met" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_met" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_met" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_met" = [Cat "has_met" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "included" = [Cat "included" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "include" = [Cat "include" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "include" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "include" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "include" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "includes" = [Cat "includes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_include" = [Cat "will_include" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_included" = [Cat "have_included" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_included" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_included" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_included" = [Cat "has_included" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "continued" =
    [Cat "continued" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []]]
lexicon "continue" =
    [Cat "continue" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []],
    Cat "continue" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []],
    Cat "continue" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []],
    Cat "continue" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []]]
lexicon "continues" =
    [Cat "continues" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []]]
lexicon "will_continue" =
    [Cat "will_continue" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []]]
lexicon "have_continued" =
    [Cat "have_continued" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []],
    Cat "have_continued" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []],
    Cat "have_continued" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []]]
lexicon "has_continued" =
    [Cat "has_continued" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [With] []]]

lexicon "learnt" = [Cat "learnt" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "learn" = [Cat "learn" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "learn" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "learn" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "learn" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "learns" = [Cat "learns" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_learn" = [Cat "will_learn" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_learnt" = [Cat "have_learnt" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_learnt" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_learnt" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_learnt" = [Cat "has_learnt" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "led" = [Cat "led" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "lead" = [Cat "lead" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "lead" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "lead" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "lead" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "leads" = [Cat "leads" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_lead" = [Cat "will_lead" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_led" = [Cat "have_led" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_led" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_led" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_led" = [Cat "has_led" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "understood" = [Cat "understood" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "understand" = [Cat "understand" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "understand" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "understand" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "understand" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "understands" = [Cat "understands" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_understand" = [Cat "will_understand" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_understood" = [Cat "have_understood" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_understood" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_understood" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_understood" = [Cat "has_understood" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "watched" = [Cat "watched" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "watch" = [Cat "watch" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "watch" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "watch" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "watch" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "watches" = [Cat "watches" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_watch" = [Cat "will_watch" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_watched" = [Cat "have_watched" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_watched" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_watched" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_watched" = [Cat "has_watched" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "followed" = [Cat "followed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "follow" = [Cat "follow" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "follow" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "follow" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "follow" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "follows" = [Cat "follows" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_follow" = [Cat "will_follow" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_followed" = [Cat "have_followed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_followed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_followed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_followed" = [Cat "has_followed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "stopped" = [Cat "stopped" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "stop" = [Cat "stop" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "stop" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "stop" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "stop" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "stops" = [Cat "stops" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_stop" = [Cat "will_stop" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_stopped" = [Cat "have_stopped" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_stopped" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_stopped" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_stopped" = [Cat "has_stopped" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "spoke" =
    [Cat "spoke" "VP" [Past] [Cat "_" "PP" [To] [],
        Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]
lexicon "speak" =
    [Cat "speak" "VP" [Pres,Sg,Fst] [Cat "_" "PP" [To] [],
        Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []],
    Cat "speak" "VP" [Pres,Sg,Snd] [Cat "_" "PP" [To] [],
        Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []],
    Cat "speak" "VP" [Pres,Pl] [Cat "_" "PP" [To] [],
        Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []],
    Cat "speak" "VP" [Pres,Sg,Snd] [Cat "_" "PP" [To] [],
        Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]
lexicon "speaks" =
    [Cat "speaks" "VP" [Pres,Sg,Thrd] [Cat "_" "PP" [To] [],
        Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]
lexicon "will_speak" =
    [Cat "will_speak" "VP" [Fut] [Cat "_" "PP" [To] [],
        Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]
lexicon "have_spoken" =
    [Cat "have_spoken" "VP" [Perf,Sg,Fst] [Cat "_" "PP" [To] [],
        Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []],
    Cat "have_spoken" "VP" [Perf,Sg,Snd] [Cat "_" "PP" [To] [],
        Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []],
    Cat "have_spoken" "VP" [Perf,Pl] [Cat "_" "PP" [To] [],
        Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]
lexicon "has_spoken" =
    [Cat "has_spoken" "VP" [Perf,Sg,Thrd] [Cat "_" "PP" [To] [],
        Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]

lexicon "read" =
    [Cat "read" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]
lexicon "read" =
    [Cat "read" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []],
    Cat "read" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []],
    Cat "read" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []],
    Cat "read" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]
lexicon "reads" =
    [Cat "reads" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]
lexicon "will_read" =
    [Cat "will_read" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]
lexicon "have_read" =
    [Cat "have_read" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []],
    Cat "have_read" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []],
    Cat "have_read" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]
lexicon "has_read" =
    [Cat "has_read" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [To] []]]

lexicon "spent" = [Cat "spent" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "spend" = [Cat "spend" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "spend" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "spend" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "spend" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "spends" = [Cat "spends" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_spend" = [Cat "will_spend" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_spent" = [Cat "have_spent" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_spent" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_spent" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_spent" = [Cat "has_spent" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "grew" = [Cat "grew" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "grow" = [Cat "grow" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "grow" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "grow" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "grow" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "grows" = [Cat "grows" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_grow" = [Cat "will_grow" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_grown" = [Cat "have_grown" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_grown" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_grown" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_grown" = [Cat "has_grown" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "opened" = [Cat "opened" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "open" = [Cat "open" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "open" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "open" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "open" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "opens" = [Cat "opens" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_open" = [Cat "will_open" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_opened" = [Cat "have_opened" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_opened" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_opened" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_opened" = [Cat "has_opened" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "walked" = [Cat "walked" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "walk" = [Cat "walk" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "walk" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "walk" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "walk" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "walks" = [Cat "walks" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_walk" = [Cat "will_walk" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_walked" = [Cat "have_walked" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_walked" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_walked" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_walked" = [Cat "has_walked" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "taught" = [Cat "taught" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "teach" = [Cat "teach" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "teach" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "teach" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "teach" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "teaches" = [Cat "teaches" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_teach" = [Cat "will_teach" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_taught" = [Cat "have_taught" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_taught" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_taught" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_taught" = [Cat "has_taught" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "remembered" = [Cat "remembered" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "remember" = [Cat "remember" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "remember" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "remember" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "remember" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "remembers" = [Cat "remembers" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_remember" = [Cat "will_remember" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_remembered" = [Cat "have_remembered" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_remembered" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_remembered" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_remembered" = [Cat "has_remembered" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "considered" = [Cat "considered" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "consider" = [Cat "consider" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "consider" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "consider" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "consider" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "considers" = [Cat "considers" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_consider" = [Cat "will_consider" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_considered" = [Cat "have_considered" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_considered" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_considered" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_considered" = [Cat "has_considered" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "appeared" = [Cat "appeared" "VP" [Past] [Cat "_" "PP" [To] []]]
lexicon "appear" = [Cat "appear" "VP" [Pres,Sg,Fst] [Cat "_" "PP" [To] []],
    Cat "appear" "VP" [Pres,Sg,Snd] [Cat "_" "PP" [To] []],
    Cat "appear" "VP" [Pres,Pl] [Cat "_" "PP" [To] []],
    Cat "appear" "VP" [Infl] [Cat "_" "PP" [To] []]]
lexicon "appears" = [Cat "appears" "VP" [Pres,Sg,Thrd] [Cat "_" "PP" [To] []]]
lexicon "will_appear" = [Cat "will_appear" "VP" [Fut] [Cat "_" "PP" [To] []]]
lexicon "have_appeared" = [Cat "have_appeared" "VP" [Perf,Sg,Fst] [Cat "_" "PP" [To] []],
    Cat "have_appeared" "VP" [Perf,Sg,Snd] [Cat "_" "PP" [To] []],
    Cat "have_appeared" "VP" [Perf,Pl] [Cat "_" "PP" [To] []]]
lexicon "has_appeared" = [Cat "has_appeared" "VP" [Perf,Sg,Thrd] [Cat "_" "PP" [To] []]]

lexicon "bought" = [Cat "bought" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "buy" = [Cat "buy" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "buy" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "buy" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "buy" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "buys" = [Cat "buys" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_buy" = [Cat "will_buy" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_bought" = [Cat "have_bought" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_bought" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_bought" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_bought" = [Cat "has_bought" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "served" = [Cat "served" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "serve" = [Cat "serve" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "serve" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "serve" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "serve" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "serves" = [Cat "serves" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_serve" = [Cat "will_serve" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_served" = [Cat "have_served" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_served" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_served" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_served" = [Cat "has_served" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "sent" = [Cat "sent" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "send" = [Cat "send" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "send" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "send" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "send" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "sends" = [Cat "sends" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_send" = [Cat "will_send" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_sent" = [Cat "have_sent" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_sent" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_sent" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_sent" = [Cat "has_sent" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "built" = [Cat "built" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "build" = [Cat "build" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "build" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "build" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "build" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "builds" = [Cat "builds" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_build" = [Cat "will_build" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_built" = [Cat "have_built" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_built" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_built" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_built" = [Cat "has_built" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "stayed" =
    [Cat "stayed" "VP" [Past] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []]]
lexicon "stay" =
    [Cat "stay" "VP" [Pres,Sg,Fst] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []],
    Cat "stay" "VP" [Pres,Sg,Snd] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []],
    Cat "stay" "VP" [Pres,Pl] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []],
    Cat "stay" "VP" [Pres,Sg,Snd] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []]]
lexicon "stays" =
    [Cat "stays" "VP" [Pres,Sg,Thrd] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []]]
lexicon "will_stay" =
    [Cat "will_stay" "VP" [Fut] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []]]
lexicon "have_stayed" =
    [Cat "have_stayed" "VP" [Perf,Sg,Fst] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []],
    Cat "have_stayed" "VP" [Perf,Sg,Snd] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []],
    Cat "have_stayed" "VP" [Perf,Pl] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []]]
lexicon "has_stayed" =
    [Cat "has_stayed" "VP" [Perf,Sg,Thrd] [Cat "_" "PP" [On] [],
        Cat "_" "PP" [With] []]]

lexicon "reached" =
    [Cat "reached" "VP" [Past] [Cat "_" "PP" [From] [],
        Cat "_" "PP" [To] []]]
lexicon "reach" =
    [Cat "reach" "VP" [Pres,Sg,Fst] [Cat "_" "PP" [From] [],
        Cat "_" "PP" [To] []],
    Cat "reach" "VP" [Pres,Sg,Snd] [Cat "_" "PP" [From] [],
        Cat "_" "PP" [To] []],
    Cat "reach" "VP" [Pres,Pl] [Cat "_" "PP" [From] [],
        Cat "_" "PP" [To] []],
    Cat "reach" "VP" [Pres,Sg,Snd] [Cat "_" "PP" [From] [],
        Cat "_" "PP" [To] []]]
lexicon "reaches" =
    [Cat "reaches" "VP" [Pres,Sg,Thrd] [Cat "_" "PP" [From] [],
        Cat "_" "PP" [To] []]]
lexicon "will_reach" =
    [Cat "will_reach" "VP" [Fut] [Cat "_" "PP" [From] [],
        Cat "_" "PP" [To] []]]
lexicon "have_reached" =
    [Cat "have_reached" "VP" [Perf,Sg,Fst] [Cat "_" "PP" [From] [],
        Cat "_" "PP" [To] []],
    Cat "have_reached" "VP" [Perf,Sg,Snd] [Cat "_" "PP" [From] [],
        Cat "_" "PP" [To] []],
    Cat "have_reached" "VP" [Perf,Pl] [Cat "_" "PP" [From] [],
        Cat "_" "PP" [To] []]]
lexicon "has_reached" =
    [Cat "has_reached" "VP" [Perf,Sg,Thrd] [Cat "_" "PP" [From] [],
        Cat "_" "PP" [To] []]]

lexicon "killed" = [Cat "killed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "kill" = [Cat "kill" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "kill" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "kill" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "kill" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "kills" = [Cat "kills" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_kill" = [Cat "will_kill" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_killed" = [Cat "have_killed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_killed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_killed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_killed" = [Cat "has_killed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "raised" = [Cat "raised" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "raise" = [Cat "raise" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "raise" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "raise" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "raise" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "raises" = [Cat "raises" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_raise" = [Cat "will_raise" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_raised" = [Cat "have_raised" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_raised" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_raised" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_raised" = [Cat "has_raised" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "passed" = [Cat "passed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "pass" = [Cat "pass" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "pass" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "pass" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "pass" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "passes" = [Cat "passes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_pass" = [Cat "will_pass" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_passed" = [Cat "have_passed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_passed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_passed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_passed" = [Cat "has_passed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "sold" = [Cat "sold" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "sell" = [Cat "sell" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "sell" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "sell" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "sell" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "sells" = [Cat "sells" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_sell" = [Cat "will_sell" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_sold" = [Cat "have_sold" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_sold" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_sold" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_sold" = [Cat "has_sold" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "decided" = [Cat "decided" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "decide" = [Cat "decide" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "decide" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "decide" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "decide" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "decides" = [Cat "decides" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_decide" = [Cat "will_decide" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_decided" = [Cat "have_decided" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_decided" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_decided" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_decided" = [Cat "has_decided" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "returned" =
    [Cat "returned" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] []]]
lexicon "return" =
    [Cat "return" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] []],
    Cat "return" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] []],
    Cat "return" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] []],
    Cat "return" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] []]]
lexicon "returns" =
    [Cat "returns" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] []]]
lexicon "will_return" =
    [Cat "will_return" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] []]]
lexicon "have_returned" =
    [Cat "have_returned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] []],
    Cat "have_returned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] []],
    Cat "have_returned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] []]]
lexicon "has_returned" =
    [Cat "has_returned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] []]]

lexicon "explained" = [Cat "explained" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "explain" = [Cat "explain" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "explain" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "explain" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "explain" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "explains" = [Cat "explains" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_explain" = [Cat "will_explain" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_explained" = [Cat "have_explained" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_explained" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_explained" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_explained" = [Cat "has_explained" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "developed" = [Cat "developed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "develop" = [Cat "develop" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "develop" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "develop" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "develop" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "develops" = [Cat "develops" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_develop" = [Cat "will_develop" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_developed" = [Cat "have_developed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_developed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_developed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_developed" = [Cat "has_developed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "broke" = [Cat "broke" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "break" = [Cat "break" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "break" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "break" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "break" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "breaks" = [Cat "breaks" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_break" = [Cat "will_break" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_broken" = [Cat "have_broken" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_broken" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_broken" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_broken" = [Cat "has_broken" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "received" = [Cat "received" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "receive" = [Cat "receive" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "receive" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "receive" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "receive" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "receives" = [Cat "receives" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_receive" = [Cat "will_receive" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_received" = [Cat "have_received" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_received" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_received" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_received" = [Cat "has_received" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "hit" = [Cat "hit" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "hit" = [Cat "hit" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "hit" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "hit" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "hit" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "hits" = [Cat "hits" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_hit" = [Cat "will_hit" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_hit" = [Cat "have_hit" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_hit" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_hit" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_hit" = [Cat "has_hit" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "ate" = [Cat "ate" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "eat" = [Cat "eat" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "eat" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "eat" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "eat" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "eats" = [Cat "eats" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_eat" = [Cat "will_eat" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_eaten" = [Cat "have_eaten" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_eaten" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_eaten" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_eaten" = [Cat "has_eaten" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "covered" = [Cat "covered" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "cover" = [Cat "cover" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "cover" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "cover" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "cover" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "covers" = [Cat "covers" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_cover" = [Cat "will_cover" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_covered" = [Cat "have_covered" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_covered" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_covered" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_covered" = [Cat "has_covered" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "caught" = [Cat "caught" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "catch" = [Cat "catch" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "catch" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "catch" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "catch" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "catches" = [Cat "catches" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_catch" = [Cat "will_catch" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_caught" = [Cat "have_caught" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_caught" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_caught" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_caught" = [Cat "has_caught" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "drew" =
    [Cat "drew" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] [],
        Cat "_" "PP" [On] []]]
lexicon "draw" =
    [Cat "draw" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] [],
        Cat "_" "PP" [On] []],
    Cat "draw" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] [],
        Cat "_" "PP" [On] []],
    Cat "draw" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] [],
        Cat "_" "PP" [On] []],
    Cat "draw" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] [],
        Cat "_" "PP" [On] []]]
lexicon "draws" =
    [Cat "draws" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] [],
        Cat "_" "PP" [On] []]]
lexicon "will_draw" =
    [Cat "will_draw" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] [],
        Cat "_" "PP" [On] []]]
lexicon "have_drawn" =
    [Cat "have_drawn" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] [],
        Cat "_" "PP" [On] []],
    Cat "have_drawn" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] [],
        Cat "_" "PP" [On] []],
    Cat "have_drawn" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] [],
        Cat "_" "PP" [On] []]]
lexicon "has_drawn" =
    [Cat "has_drawn" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
        Cat "_" "PP" [From] [],
        Cat "_" "PP" [On] []]]

lexicon "chose" = [Cat "chose" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "choose" = [Cat "choose" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "choose" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "choose" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
    Cat "choose" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "chooses" = [Cat "chooses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_choose" = [Cat "will_choose" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_chosen" = [Cat "have_chosen" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_chosen" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
    Cat "have_chosen" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_chosen" = [Cat "has_chosen" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon _ = []
