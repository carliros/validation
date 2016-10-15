## Validación

Utilice el archivo Validation.hs. Para empezar todos las funciones están undefined,
pero el archivo puede ser cargado en el intérprete GHCi a pesar de que todos los ejercicios
no están completos.

### Validar números de tarjetas de crédito

¿Alguna vez te as preguntado como las aplicaciones web validan el número de la tarjeta de crédito
cuando se hace una compra on-line? Ellos no verifican en una gran base de datos de números y tampoco 
hacen magia. De hecho, la mayoría de los proveedores de crédito se basan en una fórmula de suma de 
comprobación para distinguir los números válidos de la colección de dígitos al azar (o errores de escritura).

En esta sección, podrás implementar el algoritmo de validación de tarjetas de crédito. Sigue los siguientes pasos:

* Duplicar el valor de cada segunda cifra empezando por la derecha.
* Suma los dígitos de los valores que se duplicaron y los dígitos no duplicados del número original.
* Calcular el módulo de la suma dividiendo por 10.

Si el resultado es igual a 0, entonces el numero es válido. He aquí un ejemplo de los resultados de cada paso en el número 4012888888881881.

* Con el fin de comenzar con el dígito más a la derecha, producimos una lista inversa de dígitos. A continuación, duplicamos cada segundo dígito. 

```
Resultado: [1, 16, 8, 2, 8, 16, 8, 16, 8, 16, 8, 16, 2, 2, 0, 8].
```

* Se suma todos los dígitos de la lista del resultado anterior. Tenga en cuenta que tenemos que volver
 a dividir los elementos de la lista en sus dígitos (por ejemplo, 16 se convierte en [1, 6]).

```
Resultado: 90.
```

* Por último, se calcula el módulo de 90 sobre 10.
```
Resultado: 0.
```

Dado que el último valor es 0, sabemos que el número que introducimos es un número de tarjeta de crédito válido. 
Si cometemos un error de transcripción en el número de tarjeta de crédito y en su lugar proporcionamos 4012888888881891, 
el resultado del último paso es 2, lo que demuestra que el número no es válido.

1. Primero necesitamos encontrar los dígitos de un número. Definir una función que retorna una
       lista de dígitos positivos, decimales (base 10) en orden invertido.
	   ```
		toDigitsRev :: Integer -> [ Integer ]
	   ```
   (Recordemos que hay que comenzar los duplicados desde la derecha). Puedes definir
       **toDigitsRev** directamente o con la función:
	   ```
        toDigits :: Integer -> [ Integer  ]
	   ```
       de tal manera que toDigitsRev se define de la siguiente manera
	   ```
       toDigitsRev = reverse . toDigits
	   ```
       Ejemplo: El resultado de toDigitsRev 1234 es [4,3,2,1].
       Buen estilo de programación. Si bien esto puede no es necesario para los números
       de tarjeta de crédito, hacer que toDigitsRev controle correctamente las entradas que son
       números negativos o ceros.

2. Una vez que tengamos los dígitos en el orden correcto, tenemos que duplicar cada uno. Define la función
	```
	   doubleSecond :: (Num a) => [ a  ] -> [ a  ]
	```
       que duplique cada segundo dígito de la lista de entrada.

	   Ejemplo: El resultado de doubleSecond [8,7,6,5] es [8,14,6,10].
 
3. La salida de doubleSecond tiene una mezcla de un dígito y dos dígitos. Define una
       función que calcule la suma de todos los dígitos.
	   ```
       sumDigits :: [ Integer  ] -> Integer
	   ```
       Ejemplo: El resultado de sumDigits [8,14,6,10] es 20.

4. Define la función
	```
        validate :: Integer -> Bool
	```
    que indica que cualquier entrada positiva podría ser un número de tarjeta válida.
    Esta función se utiliza en todas las funciones definidas en los ejercicios anteriores.

    Explique: ¿Por que utilizamos Integer aquí en lugar de un Int o incluso un Integral a => a?

### Leyendo y mostrando números de tarjeta de crédito

Está bien utilizar un Integer para un número de tarjeta de crédito internamente, pero nosotros (los consumidores) estamos acostumbrados a ver el número con un formato determinado. En los siguientes ejercicios queremos traducir entre el valor entero 4012888888881881 a la cadena ``4012 8888 8888 1881'' de modo que tenemos un espacio cada cuatro dígitos hacia la derecha. Vamos a suponer que todos los números de tarjetas de crédito en la mayoría son de 16 dígitos. Para los números que tienen menos dígitos, hay que utilizar ceros para llenar los dígitos que faltan. Por lo tanto, 123456789 se convierte en ``0000 0001 2345 6789''.

5. Definir la función
	```
	    readCC :: String -> Integer
	```
    que analiza el número en el formato descrito anteriormente. Tenga en cuenta que puede
    utilizar la función ``read :: (Read a) => String -> a`` para convertir cadenas de valores.
    Consulte la documentación del Prelude para ver la usabilidad de otras funciones que podrían
    ser útiles.

	Explique: ¿Cómo puede la función fallar?

6.  Definir la función
	```
		showCC :: Integer -> String
	```
    que imprime el número en el formato descrito anteriormente. Tenga en cuenta que puede
    utilizar la función de show: (Show a) => a -> String para convertir los valores a
    cadenas. Consulte la documentación del Prelude para ver la usabilidad de otras funciones
    que podrían ser útiles.

    Explique: ¿Cómo puede su función fallar?

### Identificación del tipo de la tarjeta de crédito

    Las tarjetas de crédito no sólo tienen una fórmula para la validación de los dígitos,
    sino que también tienen fórmulas para la determinación del tipo de tarjeta. El tipo
    se distingue por el emisor y la longitud:

	* Un prefijo de hasta seis dígitos que sirve como un número de identificación único para la emisor.
    * La longitud puede variar dentro de los límites específicos de cada prefijo.

    En esta sección, se pondrá en práctica la identificación de un número de tarjeta de
    crédito. Con el fin de recopilar la información para la identificación, tendrá que
    definir operaciones de I/O.

7. Define la función
	```
	    lookupIssuer :: String -> Integer -> IO String
	```
     que lee los datos de tipo de tarjeta de un archivo (cuyo nombre figura en el primer
     argumento) y devuelve el emisor, del número de la tarjeta (en el segundo argumento).

    Un archivo de ejemplo que vamos a llamar data.txt aparece como sigue:
	```
      34 15 American Express
      37 15 American Express
      560221 16 Bankcard
      6011 16 Discover Card
      65 16 Discover Card
      51 16 Master Card
      52 16 Master Card
      4 13 Visa
      4 16 Visa
      417500 16 Visa Electron
	```

    Cada línea contiene un prefijo, un espacio, una longitud(número de dígitos), un
    espacio y el nombre del emisor. Si el número coincide con el prefijo y la longitud,
    devuelve el tercer campo, si la búsqueda falla devuelve Unknown.

	Ejemplo: El resultado de lookupIssuer ``data.txt'' 4012888888881881 es Visa.

8. Este ejercicio utiliza todas las funciones que usted ha escrito en esta práctica.
   Define la función
   ```
		checkCC :: String -> IO ()
   ```
   que toma el nombre de un archivo que contiene los datos del tipo de tarjeta. Esta
   función proporciona un programa interactivo en la terminal y sigue estos pasos:

   a) Pide al usuario un número de tarjeta de crédito en el formato utilizado en el inciso cinco.

   b) Validar el número.

   c) Busque el emisor de la tarjeta de crédito.

   d) Imprimir el siguiente texto:
	* El número reformateado según lo definido en el inciso seis.
	* El estado de la validación.
    * Nombre del emisor o el mensaje de error.

   e) Volver al primer paso.

	Puedes terminar el programa tecleando Crtl-C.

    Ejemplo: Esto es lo que una sesión del programa interactivo podría ser:

	```
      ghci> checkCC "data.txt"
      Enter credit card number: 0004 2222 2222 2222
      The number 0004 2222 2222 2222 is valid and the type is Visa.
      Enter credit card number:
	```

    En el caso de que una tarjeta sea inválida, no hay necesidad de buscar el tipo:

	```
      Enter credit card number: 0004 2222 2222 2223
      The number 0004 2222 2222 2223 is not a valid credit card number.
      Enter credit card number:
	```
9. (optional). Define la función
	```
      toDigitsRevG :: (Integral a) => a -> a -> [ a  ]
	```

      que tome una base, un entero y devuelva una lista de cifras positivas.
      La base (cualquier número entero mayor que 1) representa la raíz del número.
      En el inciso uno, asumimos que los números están en base 10 (decimal),
      pero ahora podemos tomar en cuenta otras bases, por ejemplo, 2 (binario), 8
      (octal), 16 (hexadecimal), y muchas otras que no tienen nombres.
      La función toDigitsRevG es más general que toDigitsRev, ya que
      soporta otros tipos de números enteros como Int. ¿Qué hace que la
      definición de esta función sea diferente de toDigitsRev? Describe
      esto en tus propios comentarios.

      En general, queremos funciones Haskell que sean $totales$. Es decir, la función
      no debe producir errores o valores incorrectos. ¿Cómo se hace de toDigitsRevG
      una función total? A menudo, una lista vacía ([]) de salida indica que los argumentos
      no son válidos. Por lo tanto, si la función verifica que cuando los argumentos son
      inválidos se devuelve una lista vacía, todavía será total. Explica cómo hacer esto
      para toDigitsRevG en tus propios comentarios.
