module Emojis where 

 type Peliculas = [(String, String)]

 rand :: Int -> (String, String)
 rand n = p !! n
    where 
        p = [("life of pi", "\128047 \128675 \128115"),
             ("princess and the frog", "\128120 \128056"),
             ("psycho", "\128298 \128105 \128703"),
             ("gravity", "\128640 \127776 \128105 \127758"),
             ("500 days of summer", "5\65039\8419 0\65039\8419 0\65039\8419 \127774 \10084\65039"),
             ("airplane", "\9992\65039"),
             ("brokeback mountain", "\128108 \9968 \10084\65039"),
             ("pearl harbour", "\127471\127477 \128163 \127482\127480 \9875\65039"),
             ("cinderella", "\128096 \128112 \128347 \128001"),
             ("home alone", "\128102 \127968 \127876 \10052\65039"),
             ("angels and demons", "\128124 \9962\65039 \128121"),
             ("ratatouille", "\128000 \127837 \127467\127479"),
             ("the notebook", "\9999\65039 \128211 \128116 \128117"),
             ("free willy", "\128051 \127379 \127754"),
             ("thor", "\9889\65039 \128113\127995 \128296 \128293"),
             ("blood diamond", "\128137 \128142"),
             ("scary movie", "\128123 \127909"),
             ("the social network", "\128100 \128187"),
             ("the santa clause", "\128104 \127877"),
             ("planet of the apes", "\127758 \128053 \128053 \128018 \128018"),
             ("kung fu panda", "\128060 \128074"),
             ("ted", "\128104 \127866 \128059"),
             ("charlie and the chocolate factory", "\128102 \127851 \127981"),
             ("devil wears prada", "\128520 \128087 \128096"),
             ("titanic", "\128674 \10084\65039 \128142"),
             ("lord of the rings", "\128102 \128141 \127755"),
             ("et", "\128125 \128102 \128692 \127764"),
             ("eat pray love", "\127860 \128591 \10084\65039"),
             ("les miserables", "\127467\127479 \127926 \128135"),
             ("the king speech", "\128081 \128172 \127897"),
             ("night at the museum", "\127747 \127963 \128018 \128294"),
             ("super 8", "\127909 \128643 \128125"),
             ("bohemian rhapsody", "\128104\127995 \128081 \127929 \128131"),
             ("lady and the tramp", "\128021 \127837 \128041"),
             ("snakes on a plane", "\128013 \9992\65039"),
             ("project x", "\127881 \127866 \127882 \10060"),
             ("a bugs life", "\127793 \128028 \128030 \128038"),
             ("jobs", "\128241 \127822 \128104"),
             ("bridesmaids", "\128112 \128129 \128581 \128582 \128589 \128587"),
             ("apollo 13", "\128640 \127764 1\65039\8419 3\65039\8419"),
             ("27 dresses", "2\65039\8419 7\65039\8419 \128087"),
             ("a clockwork orange", "\9200 \128295 \127818"),
             ("a perfect storm", "\128076 \127783 \9889\65039"),
             ("blair witch project", "\128249 \127939 \127794 \127794"),
             ("the ring", "\128250 \128141"),
             ("polar express", "\10052\65039 \128642 \127877"),
             ("princess diaries", "\128120 \128211"),
             ("edward scissorhands", "\128400 \9986\65039"),
             ("taxi driver", "\128661 \128104"),
             ("ant man", "\128028 \128104"),
             ("sleeping beauty", "\128105 \128164"),
             ("mean girls", "\129318 \128129\128129\128129\128133 \128293\128214\128394"),
             ("the little mermaid", "\129500 \128305 \129408 \128031"),
             ("the gift", "\128230 \127872"),
             ("brother bear" ,"\128059 \128059  \128697"),
             ("willy wonka and the chocolate factory", " \128697 \127851 \127981"),
             ("scarface","\128372 \128544 \128298"),
             ("la la land","\127932 \127929 \128145"),
             ("tres metros sobre el cielo","3\65039\8419 \128207 \128070 \9729\65039"),
             ("up", " \127880 \127880 \127968 \127880 \127880"),
             ("harry potter", "\128102 \128083 \9889\65039"),
             ("high school musical", "\127891 \127936 \127926"),
             ("Men in black", "\128104\128104\9899\65039\128125"),
             ("Moneyball","\128176\9918\65039"),
             ("The wolf of wall street", "\128058\128509\128176"),
             ("deadpool", "\128128 \128169 \9876"),
             ("matrix", "\128187 \128138 \128526 \128138"),
             ("saw", "\127918 \128128 \127919"),
             ("death race", "\128663 \128128"),
             ("finding nemo", "\128031 \127754 \128269"),
             ("the bucket list" , "\128196  \128221"),
             ("death note" , "\128211  \9760"),
             ("the nightmare before chrismas","\128128 \127875 \127761 \127876"),
             ("halloween", "\127875 \128298"),
             ("21 blackjack", "\9824\65039 \9827\65039 \9829\65039 \9830\65039 \128102"),
             ("the terminator", "\129302 \128299 \128103"),
             ("train to busan", "\128651 \127471\127477 \128128"),
             ("pacific rim", "\129302 \127754\128050 \128009 "),
             ("arrival", "\128025\128125\128395\128105"),
             ("a star is born", "\127775\128118\127908"),
             ("amores perros", "\128147\128021"),
             ("charlottes web", "\128022\128376\128375"),
             ("first man",  "1\65039\8419 \127766 \128104 \128640"),
             ("breakfast at tiffany's", "\128141 \9749\65039 \128112 \127860 \128142"),
             ("the breakfast club", "\127859 \9749\65039 \127849 \9827\65039"),
             ("crossroads", "\127928 \9766 \128102 \128116"),
             ("rocky", "\128276 \127894 \128170"),
             ("happy feet", "\128039 \128131"),
             ("surfs up","\127754 \128039 \127940"),
             ("dawn of the dead", "\127748 \128128"),
             ("101 dalmatians", "1\65039\8419 0\65039\8419 1\65039\8419 \128054"),
             ("freeheld", "\128105 \10084 \65039 \128105"),
             ("mcfarland",  "\127939 \127939 \127939"),
             ("bicentennial man", "\129302 \128116 \129299"),
             ("rio", "\128038 \128107 \128131"),
             ("tangled", "\127756 \128145 \128052"),
             ("puss in boots", "\128570 \128098" ),
             ("the lion king", "\129409 \128081" ),
             ("twister", "\127786"),
             ("ghostbusters", "\128123 \128123 \128299 \128694 \128694 \128694 \128694"),
             ("now you see me", "\127913 \9824\65039 \9830\65039 \9827\65039 \9829\65039"),
             ("sherlock holmes","\128373 \129300 \128299"),
             ("signing in the rain", "\128131 \9748\65039 \127926"),
             ("back to the future", "\9203 \128102 \128116 \128663"),
             ("shrek" ,"\128056 \128052 \128120"),
             ("wall-e" , "\129302 \127793 \128640"),
             ("the shape of water", "\128031 \10084\65039 \128105"),
             ("annie hall","\128509 \127934 \128145"),
             ("her","\128241 \10084\65039 \128104"),
             ("across the universe","\127827 \127925 \127929  \128145"),
             ("it","\127914 \127880 \128298 \128102"),
             ("trainspotting","\128137 \128137 \128701 \127468\127463 \128104"),
             ("leon the professional","\128103 \127470\127481 \128104 \128299"),
             ("the grand budapest hotel","\127976 \127469\127482 \128155"),
             ("moulin rogue","\127467\127479 \128142 \10024 \127925 \128131"),
             ("hugo","\9201 \9201 \9203 \128102 \129302 \127909 \127902"),
             ("mulan","\128050 \128103 \10145\65039 \128102 \126980\65039 \127834 \127887"),
             ("inception","\128564 \128259 \128173 \128104 \128102 \128103"),
             ("little miss sunshine","\128103 \127942 \128081 \128133 \128652"),
             ("whiplash","\127932 \128116 \128102")]