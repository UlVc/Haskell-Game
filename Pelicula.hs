module Main(main) where

    import Control.Monad 
    import System.IO
    import System.Random
    import Emojis
    import Data.Emoji
    import System.Exit (exitSuccess)
    import ModificadorTexto

    main :: IO ()
    main = do
     menu

    -- | Function that prints the menu of the game.
    menu :: IO ()
    menu = do
     putStrLn "[1] Jugar \n[2] Jugar con 5 vidas \n[3] Instrucciones \n[4] Salir"
     x <- getLine
     case x of 
        "1" -> jugar
        "2" -> vidas 4 0
        "3" -> instrucciones
        "4" -> do putStrLn "Ok ciao \128075"; exitSuccess
        otherwise -> do putStrLn "Elige una opción válida."; menu

    -- | Function that shows the instructions.
    instrucciones :: IO ()
    instrucciones = do 
     putStrLn "Instrucciones: \n\nLorem ipsum dolor sit amet, at est odio corpora invidunt, ornatus voluptatum ei eos. Id quo partem sapientem gubergren, vim ei dico quidam aperiri. Natum exerci appellantur ne vix, ut mei utamur disputationi. Ut graeci eruditi mea. Minim elitr apeirian ei his, ut falli temporibus vel. Cu ius solum fugit sapientem. Iriure nusquam at eum, mel dicam efficiendi neglegentur te, eum munere complectitur no. \n\nPresiona ñ para continuar"
     c <- getLine
     case c of
        "ñ" -> main
        otherwise -> instrucciones

    -- | Function that starts the game. (one life)
    jugar :: IO ()
    jugar = do
     x <- randomRIO (0,119) :: IO Int
     let a = rand x
     putStrLn "¿Cuál es el nombre de la película?"
     putStrLn $ snd a
     u <- getLine
     let opcion = modificarTexto u
         respuesta = modificarTexto $ fst a

     if opcion == respuesta then do
        putStrLn "\9989"
     else do
        putStrLn "\10060"
     repetir

    -- | Function that starts the game. (five lives)
    vidas :: Int -> Int -> IO ()
    vidas n s = do
        x <- randomRIO (0,119) :: IO Int
        let a = rand x
        putStr "¿Cuál es el nombre de la película? "
        putStrLn $ duplicar "\10084\65039" $ n + 1
        putStrLn $ snd a
        u <- getLine
        let opcion = modificarTexto u
            respuesta = modificarTexto $ fst a

        if opcion /= respuesta then do

            if n > 0 then do
                putStrLn "\10060"
                vidas (n-1) s
            else
                putStrLn "Game Over \128557\128557\128557";
                putStr $ "Llevabas " ++ (show s) ++ " respuestas correctas.";
                putStrLn "\n";
                repetir
        else
            putStrLn "\9989";
            vidas n (s + 1)
        repetir

    -- | Function that acts when the user lose or wins the game.
    repetir :: IO ()
    repetir = do
        putStrLn "[1] Volver a jugar con una vida.\n[2] Volver a jugar con 5 vidas.\n[3] Regresar al menú."
        opc <- getLine
        case opc of
            "1" -> jugar
            "2" -> vidas 4 0
            "3" -> menu
            otherwise -> do putStrLn "Opción inválida."; repetir