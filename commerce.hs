type Producto = (String, Int)

precioTotal:: Producto -> Int -> Int -> Int -> Producto
precioTotal unProducto unaCantidad unDescuento unCostoEnvio = (aplicarCostoDeEnvio unCostoEnvio).(aplicarUnaCantidad unaCantidad).(aplicarDescuento unDescuento) $ unProducto

aplicarDescuento:: Int -> Producto -> Producto
aplicarDescuento unDescuento unProducto = (conseguirNombre unProducto, (subtract unDescuento).conseguirPrecio $ unProducto)


aplicarFuncionProducto:: Producto -> a -> (a -> String -> String) -> Producto
aplicarFuncionProducto unProducto unValor unaFuncion  = ((unaFuncion unValor).conseguirNombre $ unProducto, conseguirPrecio unProducto)

descodiciarProducto:: Producto -> Producto
descodiciarProducto unProducto = aplicarFuncionProducto unProducto 10 (take)
-- ((take 10).conseguirNombre $ unProducto, conseguirPrecio unProducto)

versionBarata:: Producto -> Producto
versionBarata unProducto = (reverse.conseguirNombre.descodiciarProducto $ unProducto , conseguirPrecio unProducto)

productoXL:: Producto -> Producto
productoXL unProducto = aplicarFuncionProducto unProducto "XL" (++)
--((++ "XL").conseguirNombre $ unProducto , conseguirPrecio unProducto)


entregaSencilla:: String -> Bool
entregaSencilla unaFecha = esPar.length $ unaFecha

productoDeElite:: Producto -> Bool
productoDeElite unProducto = productoDeLujo unProducto && productoCodiciado unProducto && not(productoCorriente unProducto)

productoCorriente:: Producto -> Bool
productoCorriente unProducto = empiezaConVocal.conseguirNombre $ unProducto

productoCodiciado:: Producto -> Bool
productoCodiciado unProducto = (>10).length.conseguirNombre $ unProducto

productoDeLujo:: Producto -> Bool
productoDeLujo unProducto = nombreDeLujo.conseguirNombre $ unProducto
nombreDeLujo:: String -> Bool
nombreDeLujo unNombre =  elem 'x' unNombre || elem 'z' unNombre

aplicarCostoDeEnvio:: Int -> Producto -> Producto
aplicarCostoDeEnvio unCostoEnvio unProducto = (conseguirNombre unProducto, (+ unCostoEnvio).conseguirPrecio $ unProducto)



conseguirNombre:: Producto -> String
conseguirNombre (nombreProducto, _) = nombreProducto 

conseguirPrecio:: Producto -> Int
conseguirPrecio (_ , unPrecio) = unPrecio

empiezaConVocal:: String -> Bool
empiezaConVocal unNombre = (`elem` vocales).head $ unNombre
vocales:: String
vocales = "AEIOUaeiouÁÉÍÓÚáéíóú"

esPar:: Int -> Bool
esPar unValor = even.(rem unValor) $ 2

aplicarUnaCantidad::  Int -> Producto -> Producto
aplicarUnaCantidad unaCantidad unProducto = (conseguirNombre unProducto , (* unaCantidad).conseguirPrecio $ unProducto)



