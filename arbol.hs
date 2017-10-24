import Graphics.Gloss
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Árbol fractal.  Introduce el paso [0..6]: "
  paso <- readLn
  display (InWindow "Arbol fractal" (700,800) (20,20)) black (dibujo paso)

dibujo :: Int -> Picture
dibujo paso = Translate 0 (-300) (arbol paso marron)

tronco :: Color -> Picture
tronco color = Color color (Polygon [(30,0), (15,300), (-15,300), (-30,0)])

arbol :: Int -> Color -> Picture
arbol 0 color = tronco color
arbol n color = Pictures [tronco color,
                          Translate 0 270 arbolMenor,
                          Translate 0 180 (Rotate   20  arbolMenor),
                          Translate 0 180 (Rotate (-20) arbolMenor),
                          Translate 0 90 (Rotate   40  arbolMenor),
                          Translate 0 90 (Rotate (-40) arbolMenor) ]
    where arbolMenor = Scale 0.5 0.5 (arbol (n-1) (masVerde color))

marron :: Color
marron = makeColor 139 100  35 255

-- (masVerde c) es el color obtenido mezclando los colores c y verde en
-- las proporciones 1 y 0.1.
masVerde :: Color -> Color
masVerde color = mixColors 1.0 0.1 color green