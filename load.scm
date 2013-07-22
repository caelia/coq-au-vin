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

(define authors
  '(((uname . "ali") (password . "griot") (email . "ali@mali.net") (role . "author") (display_name . "Ali Farka Toure"))
    ((uname . "jason") (password . "argonaut") (email . "jason@athens.org") (role . "editor") (display_name . "Just Jason"))
    ((uname . "jonathan") (password . "appleseed") (email . "johnny@appleseed.com") (role . "author"))
    ((uname . "judy") (password . "inthesky") (email . "judyblue@gmail.com") (role . "author") (display_name . "Judy Blue Eyes"))
    ((uname . "madeleine") (password . "awesome") (email . "msc@gmail.com") (role . "editor") (display_name . "Madeleine St. Clair"))
    ((uname . "matt") (password . "daman") (email . "matt@matt.org") (role . "admin") (display_name . "Matt the Cat"))
    ((uname . "megan") (password . "shirley") (email . "megan@megan.net") (role . "author"))
    ((uname . "valerie") (password . "actress") (email . "vb@gmail.com") (role . "author") (display_name . "Valerie Bertinelli"))))

(define series
  '(("Evil Corporadoes Perpetrate Evil" . 0) ("New Frontiers in Conceptual Art" . 0)))

(define (bump-partno series-name)
  (let ((current-series (assoc series-name series))
        (incr-cdr (lambda (x) (set-cdr! x (+ (cdr x) 1)) (cdr x))))
    (incr-cdr current-series)))

(define articles
 '((("author" . "ali") ("created_dt" . #,(date 0 16 29 16 6 12 2010 -25200 MST #f 1 340 #f))
    ("title" . "Duane Slats Plans Misgendered Systems") ("tags" . "%ANONYMOUS:1")
    ("series" . "Evil Corporadoes Perpetrate Evil")
    ("node_id" . "a3b28001") ("%TYPE" . "blog-post"))
   (("node_id" . "a3b28002") ("author" . "matt")
    ("created_dt" . #,(date 0 27 39 5 30 5 2011 -25200 MST #f 1 150 #f))
    ("series" . "New Frontiers in Conceptual Art")
    ("title" . "Rasputin Fiddles with Revolutionary Canvases") ("tags" . "%ANONYMOUS:2"))
   (("node_id" . "a3b28003") ("author" . "madeleine")
    ("created_dt" . #,(date 0 3 8 15 29 7 2012 -25200 MST #f 0 211 #f))
    ("title" . "Senator Spiegel Spews Spotless Mirrors") ("tags" . "%ANONYMOUS:3"))
   (("node_id" . "a3b28004") ("author" . "megan")
    ("created_dt" . #,(date 0 11 22 19 11 4 2011 -25200 MST #f 1 101 #f))
    ("title" . "Cheesemakers Anonymous Uproots Smutty Monuments") ("tags" . "%ANONYMOUS:4"))
   (("node_id" . "a3b28005") ("author" . "valerie")
    ("created_dt" . #,(date 0 12 2 14 9 6 2012 -25200 MST #f 6 161 #f))
    ("title" . "Rasputin Endures Oblique Lectures") ("tags" . "%ANONYMOUS:5"))
   (("node_id" . "a3b28006") ("author" . "valerie")
    ("created_dt" . #,(date 0 7 31 14 18 10 2012 -25200 MST #f 4 292 #f))
    ("series" . "Evil Corporadoes Perpetrate Evil")
    ("title" . "Dr. Frankenstein Fiddles with Smarmy Systems") ("tags" . "%ANONYMOUS:6"))
   (("node_id" . "a3b28011") ("author" . "ali")
    ("created_dt" . #,(date 0 31 12 15 26 12 2011 -25200 MST #f 1 360 #f))
    ("title" . "Zortheimer Cashes in on Significant Children") ("tags" . "%ANONYMOUS:7"))
   (("node_id" . "a3b28012") ("author" . "megan")
    ("created_dt" . #,(date 0 47 50 5 24 3 2012 -25200 MST #f 6 84 #f))
    ("series" . "Evil Corporadoes Perpetrate Evil")
    ("title" . "Cargill Cashes in on Evil Palimpsests") ("tags" . "%ANONYMOUS:8"))
   (("node_id" . "a3b28013") ("author" . "matt")
    ("created_dt" . #,(date 0 18 12 8 24 9 2011 -25200 MST #f 6 267 #f))
    ("series" . "New Frontiers in Conceptual Art")
    ("title" . "Lao Tzu Flirts with Imaginative Caribou") ("tags" . "%ANONYMOUS:9"))
   (("node_id" . "a3b28014") ("author" . "jason")
    ("created_dt" . #,(date 0 55 58 17 2 4 2011 -25200 MST #f 6 92 #f))
    ("series" . "Evil Corporadoes Perpetrate Evil")
    ("title" . "Cargill Plans Revolutionary Whores") ("tags" . "%ANONYMOUS:a"))
   (("node_id" . "a3b28015") ("author" . "jonathan")
    ("created_dt" . #,(date 0 51 7 1 15 10 2012 -25200 MST #f 1 289 #f))
    ("title" . "Acme Industries Takes on Northerly Systems") ("tags" . "%ANONYMOUS:b"))
   (("node_id" . "a3b28016") ("author" . "ali")
    ("created_dt" . #,(date 0 46 6 5 30 12 2011 -25200 MST #f 5 364 #f))
    ("title" . "The Warthog Destroys Entombed Palimpsests") ("tags" . "%ANONYMOUS:c"))
   (("node_id" . "a3b28021") ("author" . "judy")
    ("created_dt" . #,(date 0 16 36 4 10 2 2012 -25200 MST #f 5 41 #f))
    ("title" . "Senator Spiegel Flirts with Significant Shipments") ("tags" . "%ANONYMOUS:d"))
   (("node_id" . "a3b28022") ("author" . "madeleine")
    ("created_dt" . #,(date 0 29 42 13 10 2 2012 -25200 MST #f 5 41 #f))
    ("series" . "Evil Corporadoes Perpetrate Evil")
    ("title" . "Drano Society Overlooks Evil Mausoleums") ("tags" . "%ANONYMOUS:e"))
   (("node_id" . "a3b28023") ("author" . "valerie")
    ("created_dt" . #,(date 0 12 51 9 11 7 2012 -25200 MST #f 3 193 #f))
    ("title" . "Mr. Stools Adores Useless Corporations") ("tags" . "%ANONYMOUS:f"))
   (("node_id" . "a3b28024") ("author" . "judy")
    ("created_dt" . #,(date 0 4 59 9 12 3 2012 -25200 MST #f 1 72 #f))
    ("series" . "Evil Corporadoes Perpetrate Evil")
    ("title" . "Western Willywhackers Patents Obvious Children") ("tags" . "%ANONYMOUS:10"))
   (("node_id" . "a3b28025") ("author" . "valerie")
    ("created_dt" . #,(date 0 44 59 19 24 2 2011 -25200 MST #f 4 55 #f))
    ("series" . "New Frontiers in Conceptual Art")
    ("title" . "Barton Fink Plans Abstruse Tartlets") ("tags" . "%ANONYMOUS:11"))
   (("node_id" . "a3b28026") ("author" . "matt")
    ("created_dt" . #,(date 0 48 47 0 15 4 2011 -25200 MST #f 5 105 #f))
    ("series" . "New Frontiers in Conceptual Art")
    ("title" . "Mozart Creates Decaying Shipments") ("tags" . "%ANONYMOUS:12"))
   (("node_id" . "a3b28031") ("author" . "jason")
    ("created_dt" . #,(date 0 31 15 4 24 7 2012 -25200 MST #f 2 206 #f))
    ("title" . "Duane Slats Ignores Decaying Conspiracies") ("tags" . "%ANONYMOUS:13"))
   (("node_id" . "a3b28032") ("author" . "judy")
    ("created_dt" . #,(date 0 21 42 19 14 2 2012 -25200 MST #f 2 45 #f))
    ("title" . "Johnny English Criticizes Northerly Funds") ("tags" . "%ANONYMOUS:14"))
   (("node_id" . "a3b28033") ("author" . "matt")
    ("created_dt" . #,(date 0 20 34 16 9 1 2011 -25200 MST #f 0 9 #f))
    ("title" . "Zortheimer Fiddles with Northerly Mirrors") ("tags" . "%ANONYMOUS:15"))
   (("node_id" . "a3b28034") ("author" . "madeleine")
    ("created_dt" . #,(date 0 24 26 4 11 10 2011 -25200 MST #f 2 284 #f))
    ("series" . "New Frontiers in Conceptual Art")
    ("title" . "Duane Slats Works on Imaginary Monuments") ("tags" . "%ANONYMOUS:16"))
   (("node_id" . "a3b28035") ("author" . "judy")
    ("created_dt" . #,(date 0 2 29 6 18 10 2011 -25200 MST #f 2 291 #f))
    ("title" . "Norton Dork Destroys Misgendered Smokers") ("tags" . "%ANONYMOUS:17"))
   (("node_id" . "a3b28036") ("author" . "ali")
    ("created_dt" . #,(date 0 49 43 21 19 4 2011 -25200 MST #f 2 109 #f))
    ("title" . "Norton Dork Ignores Green Monuments") ("tags" . "%ANONYMOUS:18"))
   (("node_id" . "a3b28041") ("author" . "judy")
    ("created_dt" . #,(date 0 42 0 9 17 9 2011 -25200 MST #f 6 260 #f))
    ("title" . "Johnny English Works on Useless Strategies") ("tags" . "%ANONYMOUS:19"))
   (("node_id" . "a3b28042") ("author" . "madeleine")
    ("created_dt" . #,(date 0 44 17 19 6 3 2012 -25200 MST #f 2 66 #f))
    ("title" . "Johnny English Works on Groovy Gales") ("tags" . "%ANONYMOUS:1a"))
   (("node_id" . "a3b28043") ("author" . "madeleine")
    ("created_dt" . #,(date 0 28 2 4 8 10 2012 -25200 MST #f 1 282 #f))
    ("title" . "Lao Tzu Uproots Misgendered Countertops") ("tags" . "%ANONYMOUS:1b"))
   (("node_id" . "a3b28044") ("author" . "jason")
    ("created_dt" . #,(date 0 13 9 6 28 6 2011 -25200 MST #f 2 179 #f))
    ("title" . "Father Randy Buys Smutty Gales") ("tags" . "%ANONYMOUS:1c"))
   (("node_id" . "a3b28045") ("author" . "valerie")
    ("created_dt" . #,(date 0 28 3 1 10 3 2011 -25200 MST #f 4 69 #f))
    ("title" . "Johnny English Works on Arrogant Emotions") ("tags" . "%ANONYMOUS:1d"))
   (("node_id" . "a3b28046") ("author" . "jason")
    ("created_dt" . #,(date 0 43 44 1 31 10 2012 -25200 MST #f 3 305 #f))
    ("title" . "Barton Fink Creates Avid Systems") ("tags" . "%ANONYMOUS:1e"))
   (("node_id" . "a3b28051") ("author" . "valerie")
    ("created_dt" . #,(date 0 41 20 1 4 9 2011 -25200 MST #f 0 247 #f))
    ("title" . "Duane Slats Ignores Unpleasant Lectures") ("tags" . "%ANONYMOUS:1f"))
   (("node_id" . "a3b28052") ("author" . "megan")
    ("created_dt" . #,(date 0 44 11 8 12 7 2012 -25200 MST #f 4 194 #f))
    ("title" . "Father Randy Overlooks Significant Cartridges") ("tags" . "%ANONYMOUS:20"))
   (("node_id" . "a3b28053") ("author" . "jason")
    ("created_dt" . #,(date 0 35 1 18 7 8 2011 -25200 MST #f 0 219 #f))
    ("title" . "Drano Society Praises Oblique Antelope") ("tags" . "%ANONYMOUS:21"))
   (("node_id" . "a3b28054") ("author" . "judy")
    ("created_dt" . #,(date 0 12 1 14 7 12 2011 -25200 MST #f 3 341 #f))
    ("title" . "Mr. Stools Ignores Entombed Escalators") ("tags" . "%ANONYMOUS:22"))
   (("node_id" . "a3b28055") ("author" . "jonathan")
    ("created_dt" . #,(date 0 19 37 11 20 3 2012 -25200 MST #f 2 80 #f))
    ("series" . "Evil Corporadoes Perpetrate Evil")
    ("title" . "Acme Industries Fiddles with Evil Canvases") ("tags" . "%ANONYMOUS:23"))
   (("node_id" . "a3b28056") ("author" . "valerie")
    ("created_dt" . #,(date 0 4 11 10 11 1 2012 -25200 MST #f 3 11 #f))
    ("title" . "Jennifer Cashes in on Obtuse Canvases") ("tags" . "%ANONYMOUS:24"))
   (("node_id" . "a3b28061") ("author" . "valerie")
    ("created_dt" . #,(date 0 1 56 23 6 1 2011 -25200 MST #f 4 6 #f))
    ("title" . "Novartis Creates Passionate Emotions") ("tags" . "%ANONYMOUS:25"))
   (("node_id" . "a3b28062") ("author" . "matt")
    ("created_dt" . #,(date 0 59 48 18 27 9 2012 -25200 MST #f 4 271 #f))
    ("title" . "Duane Slats Screws up Arrogant Muffins") ("tags" . "%ANONYMOUS:26"))
   (("node_id" . "a3b28063") ("author" . "megan")
    ("created_dt" . #,(date 0 43 28 16 10 2 2012 -25200 MST #f 5 41 #f))
    ("title" . "Johnny English Takes on Obsequious Mine Shafts") ("tags" . "%ANONYMOUS:27"))
   (("node_id" . "a3b28064") ("author" . "jason")
    ("created_dt" . #,(date 0 34 56 19 17 5 2011 -25200 MST #f 2 137 #f))
    ("title" . "Jennifer Criticizes Obtuse Mirrors") ("tags" . "%ANONYMOUS:28"))
   (("node_id" . "a3b28065") ("author" . "madeleine")
    ("created_dt" . #,(date 0 34 52 17 4 7 2012 -25200 MST #f 3 186 #f))
    ("title" . "Mr. Stools Adores Decaying Ad Campaigns") ("tags" . "%ANONYMOUS:29"))
   (("node_id" . "a3b28066") ("author" . "megan")
    ("created_dt" . #,(date 0 18 41 20 26 12 2011 -25200 MST #f 1 360 #f))
    ("title" . "Zortheimer Works on Misgendered Mirrors") ("tags" . "%ANONYMOUS:2a"))
   (("node_id" . "a3b28071") ("author" . "judy")
    ("created_dt" . #,(date 0 34 59 0 19 2 2011 -25200 MST #f 6 50 #f))
    ("title" . "Senator Spiegel Borrows Worthless Chimneys") ("tags" . "%ANONYMOUS:2b"))
   (("node_id" . "a3b28072") ("author" . "valerie")
    ("created_dt" . #,(date 0 38 10 3 17 9 2012 -25200 MST #f 1 261 #f))
    ("title" . "Lao Tzu Plans Unpleasant Crystals") ("tags" . "%ANONYMOUS:2c"))
   (("node_id" . "a3b28073") ("author" . "valerie")
    ("created_dt" . #,(date 0 8 50 13 29 8 2012 -25200 MST #f 3 242 #f))
    ("title" . "Sam Smoot Patents Revolutionary Monuments") ("tags" . "%ANONYMOUS:2d"))
   (("node_id" . "a3b28074") ("author" . "ali")
    ("created_dt" . #,(date 0 16 33 18 2 10 2012 -25200 MST #f 2 276 #f))
    ("title" . "Drano Society Misses Avid Missives") ("tags" . "%ANONYMOUS:2e"))
   (("node_id" . "a3b28075") ("author" . "valerie")
    ("created_dt" . #,(date 0 23 33 5 13 12 2011 -25200 MST #f 2 347 #f))
    ("title" . "Acme Industries Adores Spotless Monuments") ("tags" . "%ANONYMOUS:2f"))
   (("node_id" . "a3b28076") ("author" . "megan")
    ("created_dt" . #,(date 0 28 16 16 3 4 2012 -25200 MST #f 2 94 #f))
    ("title" . "Zortheimer Fiddles with Evil Ad Campaigns") ("tags" . "%ANONYMOUS:30"))
   (("node_id" . "a3b28081") ("author" . "madeleine")
    ("created_dt" . #,(date 0 33 53 8 29 1 2012 -25200 MST #f 0 29 #f))
    ("series" . "Evil Corporadoes Perpetrate Evil")
    ("title" . "Western Willywhackers Focuses on Evil Whores") ("tags" . "%ANONYMOUS:31"))
   (("node_id" . "a3b28082") ("author" . "madeleine")
    ("created_dt" . #,(date 0 0 8 1 27 6 2011 -25200 MST #f 1 178 #f))
    ("title" . "Jim James Destroys Worthless Children") ("tags" . "%ANONYMOUS:32"))
   (("node_id" . "a3b28083") ("author" . "judy")
    ("created_dt" . #,(date 0 41 27 15 21 9 2011 -25200 MST #f 3 264 #f))
    ("title" . "Jim James Criticizes Obtuse Emotions") ("tags" . "%ANONYMOUS:33"))
   (("node_id" . "a3b28084") ("author" . "judy")
    ("created_dt" . #,(date 0 57 57 4 16 5 2011 -25200 MST #f 1 136 #f))
    ("title" . "Novartis Buys Northerly Countertops") ("tags" . "%ANONYMOUS:34"))
   (("node_id" . "a3b28085") ("author" . "megan")
    ("created_dt" . #,(date 0 43 56 8 19 3 2012 -25200 MST #f 1 79 #f))
    ("title" . "Barton Fink Borrows Entombed Monuments") ("tags" . "%ANONYMOUS:35"))
   (("node_id" . "a3b28086") ("author" . "jason")
    ("created_dt" . #,(date 0 45 35 21 1 5 2011 -25200 MST #f 0 121 #f))
    ("title" . "Jennifer Patents Groovy Antelope") ("tags" . "%ANONYMOUS:36"))
   (("node_id" . "a3b28091") ("author" . "valerie")
    ("created_dt" . #,(date 0 16 37 14 29 11 2011 -25200 MST #f 2 333 #f))
    ("title" . "Jennifer Spews Green Manuscripts") ("tags" . "%ANONYMOUS:37"))
   (("node_id" . "a3b28092") ("author" . "judy")
    ("created_dt" . #,(date 0 9 26 14 3 3 2012 -25200 MST #f 6 63 #f))
    ("title" . "Senator Spiegel Renews Passionate Lectures") ("tags" . "%ANONYMOUS:38"))
   (("node_id" . "a3b28093") ("author" . "madeleine")
    ("created_dt" . #,(date 0 49 53 13 3 6 2011 -25200 MST #f 5 154 #f))
    ("title" . "Cheesemakers Anonymous Borrows Misgendered Exhibitions") ("tags" . "%ANONYMOUS:39"))
   (("node_id" . "a3b28094") ("author" . "megan")
    ("created_dt" . #,(date 0 5 41 20 18 5 2012 -25200 MST #f 5 139 #f))
    ("title" . "Jim James Buys Abstruse Emotions") ("tags" . "%ANONYMOUS:3a"))
   (("node_id" . "a3b28095") ("author" . "madeleine")
    ("created_dt" . #,(date 0 27 6 22 6 5 2011 -25200 MST #f 5 126 #f))
    ("title" . "Mr. Stools Praises Passionate Diagrams") ("tags" . "%ANONYMOUS:3b"))
   (("node_id" . "a3b28096") ("author" . "ali")
    ("created_dt" . #,(date 0 26 32 2 6 2 2012 -25200 MST #f 1 37 #f))
    ("title" . "Lao Tzu Criticizes Imaginary Mine Shafts") ("tags" . "%ANONYMOUS:3c"))
   (("node_id" . "a3b28101") ("author" . "jason")
    ("created_dt" . #,(date 0 54 21 8 15 3 2011 -25200 MST #f 2 74 #f))
    ("title" . "Father Randy Ignores Scrofulous Subway Trains") ("tags" . "%ANONYMOUS:3d"))
   (("node_id" . "a3b28102") ("author" . "valerie")
    ("created_dt" . #,(date 0 14 15 1 10 9 2012 -25200 MST #f 1 254 #f))
    ("title" . "Senator Spiegel Ignores Smarmy Palimpsests") ("tags" . "%ANONYMOUS:3e"))
   (("node_id" . "a3b28103") ("author" . "valerie")
    ("created_dt" . #,(date 0 53 10 9 19 9 2012 -25200 MST #f 3 263 #f))
    ("title" . "Novartis Takes on Arrogant Cartridges") ("tags" . "%ANONYMOUS:3f"))
   (("node_id" . "a3b28104") ("author" . "megan")
    ("created_dt" . #,(date 0 2 44 10 20 11 2011 -25200 MST #f 0 324 #f))
    ("title" . "Zortheimer Overlooks Groovy Mausoleums") ("tags" . "%ANONYMOUS:40"))
   (("node_id" . "a3b28105") ("author" . "valerie")
    ("created_dt" . #,(date 0 32 15 12 26 12 2011 -25200 MST #f 1 360 #f))
    ("title" . "Rasputin Endures Entombed Children") ("tags" . "%ANONYMOUS:41"))
   (("node_id" . "a3b28106") ("author" . "matt")
    ("created_dt" . #,(date 0 27 25 5 19 1 2012 -25200 MST #f 4 19 #f))
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

(define (union-tags)
  (let loop ((tag-sets tag-sets-renumbered)
             (all-tags '()))
    (if (null? tag-sets)
      all-tags
      (let ((new-tags (cdar tag-sets)))
        (loop
          (cdr tag-sets)
          (foldl
            (lambda (all tag)
              (if (member tag all)
                all
                (cons tag all)))
            all-tags
            new-tags))))))

(define (store-tags)
  (call-with-database
    "examples/demo-site1/data/preloaded.db"
    (lambda (db)
      (let ((st (sql db "INSERT INTO tags (tag) VALUES (?);")))
        (for-each
          (lambda (tag) (exec st tag))
          (union-tags))))))

(define (store-series)
  (call-with-database
    "examples/demo-site1/data/preloaded.db"
    (lambda (db)
      (let ((st (sql db "INSERT INTO series (title, description) VALUES (?, 'A series');")))
        (for-each
          (lambda (s) (exec st (car s)))
          series)))))

(define (fix-article-datum ad)
  (let ((key (car ad))
        (value (cdr ad)))
    (cond
      ((string=? key "created_dt")
       (cons key (time->seconds (date->time value))))
      ((string=? key "tags")
       (cons key (anon-hex-string value)))
      (else
        ad))))

(define fixed-articles
  (map
    (lambda (art)
      (let ((id (car art))
            (data (cdr art)))
        (cons id (map fix-article-datum data))))
    articles))

(define (store-articles)
  (call-with-database
    "examples/demo-site1/data/preloaded.db"
    (lambda (db)
      (let ((st-art/series
              (sql db
                   "INSERT INTO articles (node_id, title, series, series_pt, created_dt)
                     SELECT ?, ?, series(id), ?, ? FROM series
                     WHERE series(title) = ?;"))
            (st-art/standalone
              (sql db
                   "INSERT INTO articles (node_id, title, created_dt) VALUES (?, ?, ?);"))
            (st-auth
              (sql db
                   "INSERT INTO articles_x_authors (article, author)
                     SELECT articles(id), authors(id) FROM articles, authors
                     WHERE articles(node_id) = ? AND authors(uname) = ?;"))
            (st-tag
              (sql db
                   "INSERT INTO articles_x_tags (article, tag)
                     SELECT articles(id), tags(id) FROM articles, tags
                     WHERE articles(node_id) = ? AND tags(tag) = ?;")))

        (for-each
          (lambda (art)
            (let* ((node-id (alist-ref "node_id" art))
                   (title (alist-ref "title" art))
                   (created-dt (alist-ref "created_dt" art))
                   (series-title (alist-ref "series" art))
                   (series-pt (and series (bump-partno series-title)))
                   (tag-set-id (alist-ref "tags" art))
                   (tags (alist-ref tag-set-id tag-sets-renumbered)))
              (if series-title
                (exec st-art/series node-id title series-pt created-dt series-title)
                (exec st-art/standalone node-id title created-dt))
              (exec st-auth node-id author)
              (for-each
                (lambda (t) (exec st-tag node-id t))
                tags)))
          fixed-articles)))))
