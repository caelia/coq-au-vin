(use srfi-19)
(use srfi-13)
(use sql-de-lite)

(define tag-sets
 '(("%ANONYMOUS:1" "chocolate" "brazil")
   ("%ANONYMOUS:2" "chocolate" "beer" "typography" "brazil")
   ("%ANONYMOUS:3" "chocolate" "typography" "sex")
   ("%ANONYMOUS:4" "chocolate" "typography" "sex")
   ("%ANONYMOUS:5" "chocolate" "typography" "sex")
   ("%ANONYMOUS:6" "butterflies" "postmodernism")
   ("%ANONYMOUS:7" "chocolate" "typography" "sex")
   ("%ANONYMOUS:8" "sushi" "radiation" "queer" "pizza")
   ("%ANONYMOUS:9" "chocolate" "typography" "sex")
   ("%ANONYMOUS:a" "chocolate" "crocodiles" "postmodernism" "bicycles")
   ("%ANONYMOUS:b" "chocolate" "typography" "sex")
   ("%ANONYMOUS:c" "chocolate" "typography")
   ("%ANONYMOUS:d" "chocolate" "beer" "brazil")
   ("%ANONYMOUS:e" "chocolate" "typography" "sex")
   ("%ANONYMOUS:f" "typography")
   ("%ANONYMOUS:10" "propaganda" "vegetables")
   ("%ANONYMOUS:11" "chocolate" "beer")
   ("%ANONYMOUS:12" "crocodiles" "typography" "radiation" "oysters")
   ("%ANONYMOUS:13" "beer" "postmodernism" "korea")
   ("%ANONYMOUS:14" "beer" "hazardous waste" "typography" "art deco" "radiation")
   ("%ANONYMOUS:15" "typography" "brazil" "fear" "queer")
   ("%ANONYMOUS:16" "fashion" "bicycles" "hazardous waste" "celiac disease" "art deco")
   ("%ANONYMOUS:17" "birth control")
   ("%ANONYMOUS:18" "crocodiles" "typography" "brazil")
   ("%ANONYMOUS:19" "fashion" "brazil" "queer" "vegetables")
   ("%ANONYMOUS:1a" "hazardous waste")
   ("%ANONYMOUS:1b" "beer")
   ("%ANONYMOUS:1c" "fashion" "hazardous waste" "pizza" "art deco" "sex")
   ("%ANONYMOUS:1d" "fashion")
   ("%ANONYMOUS:1e" "typography")
   ("%ANONYMOUS:1f" "beer" "art deco" "queer")
   ("%ANONYMOUS:20" "korea")
   ("%ANONYMOUS:21" "chocolate" "fashion" "brazil")
   ("%ANONYMOUS:22" "beer")
   ("%ANONYMOUS:23" "fashion" "hazardous waste" "pizza" "economics" "art deco")
   ("%ANONYMOUS:24" "chocolate" "typography" "sex")
   ("%ANONYMOUS:25" "typography" "sex")
   ("%ANONYMOUS:26" "beer" "fashion" "birth control" "typography" "queer")
   ("%ANONYMOUS:27" "chocolate")
   ("%ANONYMOUS:28" "beer" "fashion" "korea" "pizza")
   ("%ANONYMOUS:29" "fashion" "korea" "queer" "pizza")
   ("%ANONYMOUS:2a" "crocodiles" "fashion" "butterflies" "bicycles" "hazardous waste" "fear")
   ("%ANONYMOUS:2b" "chocolate" "beer" "brazil" "pizza")
   ("%ANONYMOUS:2c" "crocodiles" "sushi" "propaganda" "chocolate" "art deco" "korea")
   ("%ANONYMOUS:2d" "chocolate" "typography" "sex")
   ("%ANONYMOUS:2e" "chocolate" "typography" "sex")
   ("%ANONYMOUS:2f" "sex" "pizza")
   ("%ANONYMOUS:30" "pizza" "vegetables" "fear" "typography" "art deco")
   ("%ANONYMOUS:31" "crocodiles" "greece" "fashion" "butterflies" "pizza" "chocolate")
   ("%ANONYMOUS:32" "postmodernism" "radiation" "hazardous waste" "autism")
   ("%ANONYMOUS:33" "chocolate" "beer" "fashion")
   ("%ANONYMOUS:34" "beer" "typography" "sushi" "brazil")
   ("%ANONYMOUS:35" "fashion" "hazardous waste" "brazil" "fear" "art deco")
   ("%ANONYMOUS:36" "typography" "sex")
   ("%ANONYMOUS:37" "chocolate" "beer" "sushi")
   ("%ANONYMOUS:38" "beer")
   ("%ANONYMOUS:39" "fashion" "butterflies" "pizza" "vegetables" "postmodernism" "queer")
   ("%ANONYMOUS:3a" "beer" "art deco" "queer")
   ("%ANONYMOUS:3b" "radiation" "birth control" "pizza")
   ("%ANONYMOUS:3c" "chocolate" "sushi" "brazil")
   ("%ANONYMOUS:3d" "hazardous waste" "fear" "economics" "chocolate" "typography" "sex")
   ("%ANONYMOUS:3e" "bunnies" "hazardous waste" "sex")
   ("%ANONYMOUS:3f" "beer" "typography" "radiation" "brazil")
   ("%ANONYMOUS:40" "typography" "hazardous waste" "queer")
   ("%ANONYMOUS:41" "postmodernism")
   ("%ANONYMOUS:42" "chocolate" "beer" "radiation" "korea")))

(define articles
 '(("/articles/a3b28001"
    ("author" . "ali") ("timestamp" . #,(date 0 16 29 16 6 12 2010 -25200 MST #f 1 340 #f))
    ("title" . "Duane Slats Plans Misgendered Systems") ("tags" . "%ANONYMOUS:1")
    ("content" . "a3b28001") ("%TYPE" . "blog-post"))
   ("/articles/a3b28002"
    ("content" . "a3b28002") ("%TYPE" . "blog-post") ("author" . "matt")
    ("timestamp" . #,(date 0 27 39 5 30 5 2011 -25200 MST #f 1 150 #f))
    ("title" . "Rasputin Fiddles with Revolutionary Canvases") ("tags" . "%ANONYMOUS:2"))
   ("/articles/a3b28003"
    ("content" . "a3b28003") ("%TYPE" . "blog-post") ("author" . "madeleine")
    ("timestamp" . #,(date 0 3 8 15 29 7 2012 -25200 MST #f 0 211 #f))
    ("title" . "Senator Spiegel Spews Spotless Mirrors") ("tags" . "%ANONYMOUS:3"))
   ("/articles/a3b28004"
    ("content" . "a3b28004") ("%TYPE" . "blog-post") ("author" . "megan")
    ("timestamp" . #,(date 0 11 22 19 11 4 2011 -25200 MST #f 1 101 #f))
    ("title" . "Cheesemakers Anonymous Uproots Smutty Monuments") ("tags" . "%ANONYMOUS:4"))
   ("/articles/a3b28005"
    ("content" . "a3b28005") ("%TYPE" . "blog-post") ("author" . "valerie")
    ("timestamp" . #,(date 0 12 2 14 9 6 2012 -25200 MST #f 6 161 #f))
    ("title" . "Rasputin Endures Oblique Lectures") ("tags" . "%ANONYMOUS:5"))
   ("/articles/a3b28006"
    ("content" . "a3b28006") ("%TYPE" . "blog-post") ("author" . "valerie")
    ("timestamp" . #,(date 0 7 31 14 18 10 2012 -25200 MST #f 4 292 #f))
    ("title" . "Dr. Frankenstein Fiddles with Smarmy Systems") ("tags" . "%ANONYMOUS:6"))
   ("/articles/a3b28011"
    ("content" . "a3b28011") ("%TYPE" . "blog-post") ("author" . "ali")
    ("timestamp" . #,(date 0 31 12 15 26 12 2011 -25200 MST #f 1 360 #f))
    ("title" . "Zortheimer Cashes in on Significant Children") ("tags" . "%ANONYMOUS:7"))
   ("/articles/a3b28012"
    ("content" . "a3b28012") ("%TYPE" . "blog-post") ("author" . "megan")
    ("timestamp" . #,(date 0 47 50 5 24 3 2012 -25200 MST #f 6 84 #f))
    ("title" . "Cargill Cashes in on Evil Palimpsests") ("tags" . "%ANONYMOUS:8"))
   ("/articles/a3b28013"
    ("content" . "a3b28013") ("%TYPE" . "blog-post") ("author" . "matt")
    ("timestamp" . #,(date 0 18 12 8 24 9 2011 -25200 MST #f 6 267 #f))
    ("title" . "Lao Tzu Flirts with Imaginative Caribou") ("tags" . "%ANONYMOUS:9"))
   ("/articles/a3b28014"
    ("content" . "a3b28014") ("%TYPE" . "blog-post") ("author" . "jason")
    ("timestamp" . #,(date 0 55 58 17 2 4 2011 -25200 MST #f 6 92 #f))
    ("title" . "Cargill Plans Revolutionary Whores") ("tags" . "%ANONYMOUS:a"))
   ("/articles/a3b28015"
    ("content" . "a3b28015") ("%TYPE" . "blog-post") ("author" . "jonathan")
    ("timestamp" . #,(date 0 51 7 1 15 10 2012 -25200 MST #f 1 289 #f))
    ("title" . "Acme Industries Takes on Northerly Systems") ("tags" . "%ANONYMOUS:b"))
   ("/articles/a3b28016"
    ("content" . "a3b28016") ("%TYPE" . "blog-post") ("author" . "ali")
    ("timestamp" . #,(date 0 46 6 5 30 12 2011 -25200 MST #f 5 364 #f))
    ("title" . "The Warthog Destroys Entombed Palimpsests") ("tags" . "%ANONYMOUS:c"))
   ("/articles/a3b28021"
    ("content" . "a3b28021") ("%TYPE" . "blog-post") ("author" . "judy")
    ("timestamp" . #,(date 0 16 36 4 10 2 2012 -25200 MST #f 5 41 #f))
    ("title" . "Senator Spiegel Flirts with Significant Shipments") ("tags" . "%ANONYMOUS:d"))
   ("/articles/a3b28022"
    ("content" . "a3b28022") ("%TYPE" . "blog-post") ("author" . "madeleine")
    ("timestamp" . #,(date 0 29 42 13 10 2 2012 -25200 MST #f 5 41 #f))
    ("title" . "Drano Society Overlooks Evil Mausoleums") ("tags" . "%ANONYMOUS:e"))
   ("/articles/a3b28023"
    ("content" . "a3b28023") ("%TYPE" . "blog-post") ("author" . "valerie")
    ("timestamp" . #,(date 0 12 51 9 11 7 2012 -25200 MST #f 3 193 #f))
    ("title" . "Mr. Stools Adores Useless Corporations") ("tags" . "%ANONYMOUS:f"))
   ("/articles/a3b28024"
    ("content" . "a3b28024") ("%TYPE" . "blog-post") ("author" . "judy")
    ("timestamp" . #,(date 0 4 59 9 12 3 2012 -25200 MST #f 1 72 #f))
    ("title" . "Western Willywhackers Patents Obvious Children") ("tags" . "%ANONYMOUS:10"))
   ("/articles/a3b28025"
    ("content" . "a3b28025") ("%TYPE" . "blog-post") ("author" . "valerie")
    ("timestamp" . #,(date 0 44 59 19 24 2 2011 -25200 MST #f 4 55 #f))
    ("title" . "Barton Fink Plans Abstruse Tartlets") ("tags" . "%ANONYMOUS:11"))
   ("/articles/a3b28026"
    ("content" . "a3b28026") ("%TYPE" . "blog-post") ("author" . "matt")
    ("timestamp" . #,(date 0 48 47 0 15 4 2011 -25200 MST #f 5 105 #f))
    ("title" . "Mozart Creates Decaying Shipments") ("tags" . "%ANONYMOUS:12"))
   ("/articles/a3b28031"
    ("content" . "a3b28031") ("%TYPE" . "blog-post") ("author" . "jason")
    ("timestamp" . #,(date 0 31 15 4 24 7 2012 -25200 MST #f 2 206 #f))
    ("title" . "Duane Slats Ignores Decaying Conspiracies") ("tags" . "%ANONYMOUS:13"))
   ("/articles/a3b28032"
    ("content" . "a3b28032") ("%TYPE" . "blog-post") ("author" . "judy")
    ("timestamp" . #,(date 0 21 42 19 14 2 2012 -25200 MST #f 2 45 #f))
    ("title" . "Johnny English Criticizes Northerly Funds") ("tags" . "%ANONYMOUS:14"))
   ("/articles/a3b28033"
    ("content" . "a3b28033") ("%TYPE" . "blog-post") ("author" . "matt")
    ("timestamp" . #,(date 0 20 34 16 9 1 2011 -25200 MST #f 0 9 #f))
    ("title" . "Zortheimer Fiddles with Northerly Mirrors") ("tags" . "%ANONYMOUS:15"))
   ("/articles/a3b28034"
    ("content" . "a3b28034") ("%TYPE" . "blog-post") ("author" . "madeleine")
    ("timestamp" . #,(date 0 24 26 4 11 10 2011 -25200 MST #f 2 284 #f))
    ("title" . "Duane Slats Works on Imaginary Monuments") ("tags" . "%ANONYMOUS:16"))
   ("/articles/a3b28035"
    ("content" . "a3b28035") ("%TYPE" . "blog-post") ("author" . "judy")
    ("timestamp" . #,(date 0 2 29 6 18 10 2011 -25200 MST #f 2 291 #f))
    ("title" . "Norton Dork Destroys Misgendered Smokers") ("tags" . "%ANONYMOUS:17"))
   ("/articles/a3b28036"
    ("content" . "a3b28036") ("%TYPE" . "blog-post") ("author" . "ali")
    ("timestamp" . #,(date 0 49 43 21 19 4 2011 -25200 MST #f 2 109 #f))
    ("title" . "Norton Dork Ignores Green Monuments") ("tags" . "%ANONYMOUS:18"))
   ("/articles/a3b28041"
    ("content" . "a3b28041") ("%TYPE" . "blog-post") ("author" . "judy")
    ("timestamp" . #,(date 0 42 0 9 17 9 2011 -25200 MST #f 6 260 #f))
    ("title" . "Johnny English Works on Useless Strategies") ("tags" . "%ANONYMOUS:19"))
   ("/articles/a3b28042"
    ("content" . "a3b28042") ("%TYPE" . "blog-post") ("author" . "madeleine")
    ("timestamp" . #,(date 0 44 17 19 6 3 2012 -25200 MST #f 2 66 #f))
    ("title" . "Johnny English Works on Groovy Gales") ("tags" . "%ANONYMOUS:1a"))
   ("/articles/a3b28043"
    ("content" . "a3b28043") ("%TYPE" . "blog-post") ("author" . "madeleine")
    ("timestamp" . #,(date 0 28 2 4 8 10 2012 -25200 MST #f 1 282 #f))
    ("title" . "Lao Tzu Uproots Misgendered Countertops") ("tags" . "%ANONYMOUS:1b"))
   ("/articles/a3b28044"
    ("content" . "a3b28044") ("%TYPE" . "blog-post") ("author" . "jason")
    ("timestamp" . #,(date 0 13 9 6 28 6 2011 -25200 MST #f 2 179 #f))
    ("title" . "Father Randy Buys Smutty Gales") ("tags" . "%ANONYMOUS:1c"))
   ("/articles/a3b28045"
    ("content" . "a3b28045") ("%TYPE" . "blog-post") ("author" . "valerie")
    ("timestamp" . #,(date 0 28 3 1 10 3 2011 -25200 MST #f 4 69 #f))
    ("title" . "Johnny English Works on Arrogant Emotions") ("tags" . "%ANONYMOUS:1d"))
   ("/articles/a3b28046"
    ("content" . "a3b28046") ("%TYPE" . "blog-post") ("author" . "jason")
    ("timestamp" . #,(date 0 43 44 1 31 10 2012 -25200 MST #f 3 305 #f))
    ("title" . "Barton Fink Creates Avid Systems") ("tags" . "%ANONYMOUS:1e"))
   ("/articles/a3b28051"
    ("content" . "a3b28051") ("%TYPE" . "blog-post") ("author" . "valerie")
    ("timestamp" . #,(date 0 41 20 1 4 9 2011 -25200 MST #f 0 247 #f))
    ("title" . "Duane Slats Ignores Unpleasant Lectures") ("tags" . "%ANONYMOUS:1f"))
   ("/articles/a3b28052"
    ("content" . "a3b28052") ("%TYPE" . "blog-post") ("author" . "megan")
    ("timestamp" . #,(date 0 44 11 8 12 7 2012 -25200 MST #f 4 194 #f))
    ("title" . "Father Randy Overlooks Significant Cartridges") ("tags" . "%ANONYMOUS:20"))
   ("/articles/a3b28053"
    ("content" . "a3b28053") ("%TYPE" . "blog-post") ("author" . "jason")
    ("timestamp" . #,(date 0 35 1 18 7 8 2011 -25200 MST #f 0 219 #f))
    ("title" . "Drano Society Praises Oblique Antelope") ("tags" . "%ANONYMOUS:21"))
   ("/articles/a3b28054"
    ("content" . "a3b28054") ("%TYPE" . "blog-post") ("author" . "judy")
    ("timestamp" . #,(date 0 12 1 14 7 12 2011 -25200 MST #f 3 341 #f))
    ("title" . "Mr. Stools Ignores Entombed Escalators") ("tags" . "%ANONYMOUS:22"))
   ("/articles/a3b28055"
    ("content" . "a3b28055") ("%TYPE" . "blog-post") ("author" . "jonathan")
    ("timestamp" . #,(date 0 19 37 11 20 3 2012 -25200 MST #f 2 80 #f))
    ("title" . "Acme Industries Fiddles with Evil Canvases") ("tags" . "%ANONYMOUS:23"))
   ("/articles/a3b28056"
    ("content" . "a3b28056") ("%TYPE" . "blog-post") ("author" . "valerie")
    ("timestamp" . #,(date 0 4 11 10 11 1 2012 -25200 MST #f 3 11 #f))
    ("title" . "Jennifer Cashes in on Obtuse Canvases") ("tags" . "%ANONYMOUS:24"))
   ("/articles/a3b28061"
    ("content" . "a3b28061") ("%TYPE" . "blog-post") ("author" . "valerie")
    ("timestamp" . #,(date 0 1 56 23 6 1 2011 -25200 MST #f 4 6 #f))
    ("title" . "Novartis Creates Passionate Emotions") ("tags" . "%ANONYMOUS:25"))
   ("/articles/a3b28062"
    ("content" . "a3b28062") ("%TYPE" . "blog-post") ("author" . "matt")
    ("timestamp" . #,(date 0 59 48 18 27 9 2012 -25200 MST #f 4 271 #f))
    ("title" . "Duane Slats Screws up Arrogant Muffins") ("tags" . "%ANONYMOUS:26"))
   ("/articles/a3b28063"
    ("content" . "a3b28063") ("%TYPE" . "blog-post") ("author" . "megan")
    ("timestamp" . #,(date 0 43 28 16 10 2 2012 -25200 MST #f 5 41 #f))
    ("title" . "Johnny English Takes on Obsequious Mine Shafts") ("tags" . "%ANONYMOUS:27"))
   ("/articles/a3b28064"
    ("content" . "a3b28064") ("%TYPE" . "blog-post") ("author" . "jason")
    ("timestamp" . #,(date 0 34 56 19 17 5 2011 -25200 MST #f 2 137 #f))
    ("title" . "Jennifer Criticizes Obtuse Mirrors") ("tags" . "%ANONYMOUS:28"))
   ("/articles/a3b28065"
    ("content" . "a3b28065") ("%TYPE" . "blog-post") ("author" . "madeleine")
    ("timestamp" . #,(date 0 34 52 17 4 7 2012 -25200 MST #f 3 186 #f))
    ("title" . "Mr. Stools Adores Decaying Ad Campaigns") ("tags" . "%ANONYMOUS:29"))
   ("/articles/a3b28066"
    ("content" . "a3b28066") ("%TYPE" . "blog-post") ("author" . "megan")
    ("timestamp" . #,(date 0 18 41 20 26 12 2011 -25200 MST #f 1 360 #f))
    ("title" . "Zortheimer Works on Misgendered Mirrors") ("tags" . "%ANONYMOUS:2a"))
   ("/articles/a3b28071"
    ("content" . "a3b28071") ("%TYPE" . "blog-post") ("author" . "judy")
    ("timestamp" . #,(date 0 34 59 0 19 2 2011 -25200 MST #f 6 50 #f))
    ("title" . "Senator Spiegel Borrows Worthless Chimneys") ("tags" . "%ANONYMOUS:2b"))
   ("/articles/a3b28072"
    ("content" . "a3b28072") ("%TYPE" . "blog-post") ("author" . "valerie")
    ("timestamp" . #,(date 0 38 10 3 17 9 2012 -25200 MST #f 1 261 #f))
    ("title" . "Lao Tzu Plans Unpleasant Crystals") ("tags" . "%ANONYMOUS:2c"))
   ("/articles/a3b28073"
    ("content" . "a3b28073") ("%TYPE" . "blog-post") ("author" . "valerie")
    ("timestamp" . #,(date 0 8 50 13 29 8 2012 -25200 MST #f 3 242 #f))
    ("title" . "Sam Smoot Patents Revolutionary Monuments") ("tags" . "%ANONYMOUS:2d"))
   ("/articles/a3b28074"
    ("content" . "a3b28074") ("%TYPE" . "blog-post") ("author" . "ali")
    ("timestamp" . #,(date 0 16 33 18 2 10 2012 -25200 MST #f 2 276 #f))
    ("title" . "Drano Society Misses Avid Missives") ("tags" . "%ANONYMOUS:2e"))
   ("/articles/a3b28075"
    ("content" . "a3b28075") ("%TYPE" . "blog-post") ("author" . "valerie")
    ("timestamp" . #,(date 0 23 33 5 13 12 2011 -25200 MST #f 2 347 #f))
    ("title" . "Acme Industries Adores Spotless Monuments") ("tags" . "%ANONYMOUS:2f"))
   ("/articles/a3b28076"
    ("content" . "a3b28076") ("%TYPE" . "blog-post") ("author" . "megan")
    ("timestamp" . #,(date 0 28 16 16 3 4 2012 -25200 MST #f 2 94 #f))
    ("title" . "Zortheimer Fiddles with Evil Ad Campaigns") ("tags" . "%ANONYMOUS:30"))
   ("/articles/a3b28081"
    ("content" . "a3b28081") ("%TYPE" . "blog-post") ("author" . "madeleine")
    ("timestamp" . #,(date 0 33 53 8 29 1 2012 -25200 MST #f 0 29 #f))
    ("title" . "Western Willywhackers Focuses on Evil Whores") ("tags" . "%ANONYMOUS:31"))
   ("/articles/a3b28082"
    ("content" . "a3b28082") ("%TYPE" . "blog-post") ("author" . "madeleine")
    ("timestamp" . #,(date 0 0 8 1 27 6 2011 -25200 MST #f 1 178 #f))
    ("title" . "Jim James Destroys Worthless Children") ("tags" . "%ANONYMOUS:32"))
   ("/articles/a3b28083"
    ("content" . "a3b28083") ("%TYPE" . "blog-post") ("author" . "judy")
    ("timestamp" . #,(date 0 41 27 15 21 9 2011 -25200 MST #f 3 264 #f))
    ("title" . "Jim James Criticizes Obtuse Emotions") ("tags" . "%ANONYMOUS:33"))
   ("/articles/a3b28084"
    ("content" . "a3b28084") ("%TYPE" . "blog-post") ("author" . "judy")
    ("timestamp" . #,(date 0 57 57 4 16 5 2011 -25200 MST #f 1 136 #f))
    ("title" . "Novartis Buys Northerly Countertops") ("tags" . "%ANONYMOUS:34"))
   ("/articles/a3b28085"
    ("content" . "a3b28085") ("%TYPE" . "blog-post") ("author" . "megan")
    ("timestamp" . #,(date 0 43 56 8 19 3 2012 -25200 MST #f 1 79 #f))
    ("title" . "Barton Fink Borrows Entombed Monuments") ("tags" . "%ANONYMOUS:35"))
   ("/articles/a3b28086"
    ("content" . "a3b28086") ("%TYPE" . "blog-post") ("author" . "jason")
    ("timestamp" . #,(date 0 45 35 21 1 5 2011 -25200 MST #f 0 121 #f))
    ("title" . "Jennifer Patents Groovy Antelope") ("tags" . "%ANONYMOUS:36"))
   ("/articles/a3b28091"
    ("content" . "a3b28091") ("%TYPE" . "blog-post") ("author" . "valerie")
    ("timestamp" . #,(date 0 16 37 14 29 11 2011 -25200 MST #f 2 333 #f))
    ("title" . "Jennifer Spews Green Manuscripts") ("tags" . "%ANONYMOUS:37"))
   ("/articles/a3b28092"
    ("content" . "a3b28092") ("%TYPE" . "blog-post") ("author" . "judy")
    ("timestamp" . #,(date 0 9 26 14 3 3 2012 -25200 MST #f 6 63 #f))
    ("title" . "Senator Spiegel Renews Passionate Lectures") ("tags" . "%ANONYMOUS:38"))
   ("/articles/a3b28093"
    ("content" . "a3b28093") ("%TYPE" . "blog-post") ("author" . "madeleine")
    ("timestamp" . #,(date 0 49 53 13 3 6 2011 -25200 MST #f 5 154 #f))
    ("title" . "Cheesemakers Anonymous Borrows Misgendered Exhibitions") ("tags" . "%ANONYMOUS:39"))
   ("/articles/a3b28094"
    ("content" . "a3b28094") ("%TYPE" . "blog-post") ("author" . "megan")
    ("timestamp" . #,(date 0 5 41 20 18 5 2012 -25200 MST #f 5 139 #f))
    ("title" . "Jim James Buys Abstruse Emotions") ("tags" . "%ANONYMOUS:3a"))
   ("/articles/a3b28095"
    ("content" . "a3b28095") ("%TYPE" . "blog-post") ("author" . "madeleine")
    ("timestamp" . #,(date 0 27 6 22 6 5 2011 -25200 MST #f 5 126 #f))
    ("title" . "Mr. Stools Praises Passionate Diagrams") ("tags" . "%ANONYMOUS:3b"))
   ("/articles/a3b28096"
    ("content" . "a3b28096") ("%TYPE" . "blog-post") ("author" . "ali")
    ("timestamp" . #,(date 0 26 32 2 6 2 2012 -25200 MST #f 1 37 #f))
    ("title" . "Lao Tzu Criticizes Imaginary Mine Shafts") ("tags" . "%ANONYMOUS:3c"))
   ("/articles/a3b28101"
    ("content" . "a3b28101") ("%TYPE" . "blog-post") ("author" . "jason")
    ("timestamp" . #,(date 0 54 21 8 15 3 2011 -25200 MST #f 2 74 #f))
    ("title" . "Father Randy Ignores Scrofulous Subway Trains") ("tags" . "%ANONYMOUS:3d"))
   ("/articles/a3b28102"
    ("content" . "a3b28102") ("%TYPE" . "blog-post") ("author" . "valerie")
    ("timestamp" . #,(date 0 14 15 1 10 9 2012 -25200 MST #f 1 254 #f))
    ("title" . "Senator Spiegel Ignores Smarmy Palimpsests") ("tags" . "%ANONYMOUS:3e"))
   ("/articles/a3b28103"
    ("content" . "a3b28103") ("%TYPE" . "blog-post") ("author" . "valerie")
    ("timestamp" . #,(date 0 53 10 9 19 9 2012 -25200 MST #f 3 263 #f))
    ("title" . "Novartis Takes on Arrogant Cartridges") ("tags" . "%ANONYMOUS:3f"))
   ("/articles/a3b28104"
    ("content" . "a3b28104") ("%TYPE" . "blog-post") ("author" . "megan")
    ("timestamp" . #,(date 0 2 44 10 20 11 2011 -25200 MST #f 0 324 #f))
    ("title" . "Zortheimer Overlooks Groovy Mausoleums") ("tags" . "%ANONYMOUS:40"))
   ("/articles/a3b28105"
    ("content" . "a3b28105") ("%TYPE" . "blog-post") ("author" . "valerie")
    ("timestamp" . #,(date 0 32 15 12 26 12 2011 -25200 MST #f 1 360 #f))
    ("title" . "Rasputin Endures Entombed Children") ("tags" . "%ANONYMOUS:41"))
   ("/articles/a3b28106"
    ("content" . "a3b28106") ("%TYPE" . "blog-post") ("author" . "matt")
    ("timestamp" . #,(date 0 27 25 5 19 1 2012 -25200 MST #f 4 19 #f))
    ("title" . "Father Randy Creates Revolutionary Antelope") ("tags" . "%ANONYMOUS:42"))))

(define (anon-hex-string anon-id)
  (let* ((n* (cadr (string-split anon-id ":")))
         (n (string->number n* 16)))
    (string-append "#x" (string-pad (sprintf "~X" n) 8 #\0))))

(define tag-sets-renumbered
  (map
    (lambda (elt)
      (let ((id (car elt))
            (tags (cdr elt)))
        (cons (anon-hex-string id) tags)))
    tag-sets))

(define (store-tag-sets)
  (call-with-database
    "examples/demo-site1/data/preloaded.db"
    (lambda (db)
      (let ((st (sql db "INSERT INTO sets (set_id, item) VALUES (?, ?);")))
        (for-each
          (lambda (elt)
            (let ((id (car elt))
                  (tags (cdr elt)))
              (for-each
                (lambda (tag) (exec st id tag))
                tags)))
          tag-sets-renumbered)))))
