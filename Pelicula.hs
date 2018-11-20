module Main(main) where

    import Control.Monad 
    import System.IO
    import System.Random
    import Emojis
    import Data.Emoji
    import Data.Char
    import System.Exit (exitSuccess)
    import Data.List.Split

    main :: IO ()
    main = do
     menu

    menu :: IO ()
    menu = do
     putStrLn "[1] Jugar \n[2] Jugar con 5 vidas \n[3] Instrucciones \n[4] Salir"
     x <- getLine
     case x of 
        "1" -> jugar
        "2" -> vidas 4
        "3" -> instrucciones
        "4" -> do putStrLn "Ok ciao \128075"; exitSuccess
        otherwise -> do putStrLn "Elige una opción válida."; menu

    instrucciones :: IO ()
    instrucciones = do 
     putStrLn "Instrucciones: \n\nLorem ipsum dolor sit amet, at est odio corpora invidunt, ornatus voluptatum ei eos. Id quo partem sapientem gubergren, vim ei dico quidam aperiri. Natum exerci appellantur ne vix, ut mei utamur disputationi. Ut graeci eruditi mea. Minim elitr apeirian ei his, ut falli temporibus vel. Cu ius solum fugit sapientem. Iriure nusquam at eum, mel dicam efficiendi neglegentur te, eum munere complectitur no. \n\nPresiona ñ para continuar"
     c <- getLine
     case c of
        "ñ" -> main
        otherwise -> instrucciones

    jugar :: IO ()
    jugar = do
     x <- randomRIO (0,120) :: IO Int
     let a = rand x
     putStrLn "¿Cuál es el nombre de la película?"
     putStrLn $ snd a ++ "  " ++ fst a
     u <- getLine
     let opcion = modificarTexto u
         respuesta = modificarTexto $ fst a

     if opcion == respuesta then do
        putStrLn "\9989"
     else do
        putStrLn "\10060"
     repetir

    vidas :: Int -> IO ()
    vidas n = do
        x <- randomRIO (0,120) :: IO Int
        let a = rand x
        putStrLn "¿Cuál es el nombre de la película?"
        putStrLn $ snd a ++ "  " ++ fst a
        u <- getLine
        let opcion = modificarTexto u
            respuesta = modificarTexto $ fst a
        if opcion /= respuesta then do
            print n
            if n > 0 then do
                putStrLn "\10060"
                vidas $ n - 1
            else
                putStrLn "Game Over \128557 x3"
        else
            putStrLn "\9989"
        repetir

    repetir :: IO ()
    repetir = do
        putStrLn "[1] Volver a jugar con una vida.\n[2] Volver a jugar con 5 vidas.\n[3] Regresar al menú."
        opc <- getLine
        case opc of
            "1" -> jugar
            "2" -> vidas 4
            "3" -> menu
            otherwise -> do putStrLn "Opción inválida."; repetir

    modificarTexto :: String -> String
    modificarTexto s = filter (/=' ') $ elimina (elimina (elimina (filter (/= '.') s) "an") "a") "the"

    elimina :: String -> String -> String
    elimina c s = map toLower $ unir $ elElemento s $ splitOn " " c

    elElemento :: (Eq a) => a -> [a] -> [a]
    elElemento e [] = []
    elElemento e l = filter (\x -> x /= e) l

    unir :: [String] -> String
    unir [] = ""
    unir (x:xs) = x ++ " " ++ (unir xs)