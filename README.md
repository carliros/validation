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
