type Producto = (String, Int)

precioTotal:: Producto -> Int -> Int -> Int -> Producto
precioTotal unProducto unaCantidad unDescuento unCostoEnvio = (aplicarCostoDeEnvio unCostoEnvio).(aplicarUnaCantidad unaCantidad).(aplicarDescuento unDescuento) $ unProducto

aplicarDescuento:: Int -> Producto -> Producto
aplicarDescuento unDescuento unProducto = (conseguirNombre unProducto, (restar unDescuento).conseguirPrecio $ unProducto)

descodiciarProducto:: Producto -> Producto
descodiciarProducto unProducto = ((take 10).conseguirNombre $ unProducto, conseguirPrecio unProducto)

versionBarata:: Producto -> Producto
versionBarata unProducto = (reverse.conseguirNombre.descodiciarProducto $ unProducto , conseguirPrecio unProducto)

productoXL:: Producto -> Producto
productoXL unProducto = ((++ "XL").conseguirNombre $ unProducto , conseguirPrecio unProducto)



entregaSencilla:: String -> Bool
entregaSencilla unaFecha = esPar.length $ unaFecha

productoDeElite:: Producto -> Bool
productoDeElite unProducto = productoDeLujo unProducto && productoCodiciado unProducto && not(productoCorriente unProducto)

productoCorriente:: Producto -> Bool
productoCorriente unProducto = empiezaConVocal.conseguirNombre $ unProducto

productoCodiciado:: Producto -> Bool
productoCodiciado unProducto = (>10).length.conseguirNombre $ unProducto

productoDeLujo:: Producto -> Bool
productoDeLujo unProducto = ((contieneCaracter 'x').conseguirNombre $ unProducto) || ((contieneCaracter 'z').conseguirNombre $ unProducto)

aplicarCostoDeEnvio:: Int -> Producto -> Producto
aplicarCostoDeEnvio unCostoEnvio unProducto = (conseguirNombre unProducto, (+ unCostoEnvio).conseguirPrecio $ unProducto)



conseguirNombre:: Producto -> String
conseguirNombre (nombreProducto, _) = nombreProducto 

conseguirPrecio:: Producto -> Int
conseguirPrecio (_ , unPrecio) = unPrecio

empiezaConVocal:: String -> Bool
empiezaConVocal unNombre = (`elem` vocal).head $ unNombre
vocal:: String
vocal = "AEIOUaeiouÁÉÍÓÚáéíóú"

contieneCaracter:: Char -> String -> Bool
contieneCaracter unCaracter unString = elem unCaracter unString

esPar:: Int -> Bool
esPar unValor = (==0).(rem unValor) $ 2

aplicarUnaCantidad::  Int -> Producto -> Producto
aplicarUnaCantidad unaCantidad unProducto = (conseguirNombre unProducto , (* unaCantidad).conseguirPrecio $ unProducto)

restar:: Int -> Int -> Int
restar unDescuento unPrecio = unPrecio - unDescuento

