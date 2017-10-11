# Блок "Обзор встроенных библиотек языка"

Данный блок содержит материалы по базовым типам контейнеров языка, рекурсии и
особенности её использования в языке, а также лямбда вычислениям.

### Материалы для изучения:

 - [Recursion](http://learnyousomeerlang.com/recursion)
 - [Lambda calculation](http://learnyousomeerlang.com/higher-order-functions)
 - [Lists](http://erlang.org/doc/man/lists.html)
 - [Proplists](http://erlang.org/doc/man/proplists.html)
 - [Binary](http://erlang.org/doc/man/binary.html)
 - [Sets](http://erlang.org/doc/man/sets.html)
 - [Dict](http://erlang.org/doc/man/dict.html)
 - [Maps](http://erlang.org/doc/man/maps.html)

### Вопросы:

 - Что такая хвостовая рекурсия, почему именно её следует использовать при
   работе на языке Erlang?
 - Что такое лямбда функция? Что такое замыкание контекса функции? Что такое
   анонимная функция?
 - Приведите примеры решаемых задач, где оправдано использование различных типов
   базовых контейнеров языка.
 - Почему необходимо использовать встроенные функции для базовых контейнеров?

### Упражнения

#### 2.0. База данных

Напишите модуль db.erl, создающий базу данных, в которой можно хранить,
записывать и удалять элементы.  Функция `destroy/1` удаляет базу данных. Сборщик
мусора выполнит всю работу за вас. Но в том случае, если база хранится в файле,
при вызове функции `destroy` вам придётся удалить этот файл. Функция `destroy`
включена для полноты интерфейса. При выполнении этого упражнения функциями из
модулей *[lists](http://erlang.org/doc/man/lists.html)*,
*[proplists](http://erlang.org/doc/man/proplists.html)*,
*[dict](http://erlang.org/doc/man/dict.html)* пользоваться нельзя. Все
рекурсивные функции должны быть написаны вами. Подсказка: используйте списки и
кортежи в качестве основного типа данных. При тестировании помните, что все
переменные могут связываться со значением только один раз.

Интерфейс:
```erlang
db:new() -> Db.
db:destroy(Db) -> ok.
db:write(Key, Element, Db) -> NewDb.
db:delete(Key, Db) -> NewDb.
db:read(Key, Db) -> {ok, Element} | {error, instance}.
db:match(Element, Db) -> [Keyl, ..., KeyN].
```

Пример использования в интерпретаторе:
```erlang
1> c(db).
{ok,db}
2> Db = db:new().
[]
3> Dbl = db:write(francesco, london, Db).
[{francesco,london}]
4> Db2 = db:write(lelle, 'Stockholm', Dbl).
[{lelle,'Stockholm'},{francesco,london}]
5> db:read(francesco, Db2).
{ok,london}
6> Db3 = db:write(joern, 'Stockholm', Db2).
[{joern,'Stockholm'}, {lelle,'Stockholm'}, {francesco,london}]
7> db:read(ola, Db3).
{error,instance}
8> db:match('Stockholm', Db3).
[joern,lelle]
9> Db4 = db:delete(lelle, Db3).
[{joern,'Stockholm'}, {francesco,london}]
10> db:match('Stockholm', Db4).
[joern]
```

### 2.1 Новая функциональность в базе данных

Реализуйте следующие возможности в нашей базе данных.

Интерфейс:
```erlang
Parameters = [Opt | Parameters].
Opt = {append, allow|deny} | {batch, Number :: non_neg_integer()}.
KeyList = [Key1,...KeyN].

db:new(Parameters) -> Db.
db:append(Key, Element, Db) -> NewDb.
db:batch_delete(KeyList, Db) -> NewDb | {error, batch_limit}.
db:batch_read(KeyList, Db) -> [{Key, Element}] | {error, instance} | {error, batch_limit}.
```

### 2.2 Сделайте так, чтобы база данных работала с JSON объектами, реализованными ранее в п.1.8

### 2.3 Lambda-вычисления

Реализуйте следующие функции, используя рекурсию:
* Напишите lambda-функцию, которая осуществляет произвольную операцию ``Operation(A, B) -> C`` (где
``A, B, C`` - числа), над двумя числовыми списками попарно, возвращая список результатов операции также
в виде списка. Проверьте вашу функцию на разных операциях (``erlang:'+'``, ``erlang:'xor'``, ``erlang:'rem'``,
``erlang:'/'`` и собственной фунции, которая возвращает среднее гармоническое двух чисел ``H = 2/(1/A + 1/B)``).
* Напишите lambda-функцию, которая для каждой точки точки из списка ``dotsA`` вычисляет расстояние до всех точек
из списка точек ``dotsB`` в пространстве размерности N.
Напишите функцию, которая читает следующую нотацию:
```erlang
[
    {dimension, 5},
    {dotsA, [{1, 2, 3, 4 5}, {7, 8, 9, 10, 11}]},
    {dotsB, [{0, 0, 0, 0, 0}, {-1, -2, -3, -4, -5}]}
]
```
и возвращает:
[ 5.360220495669696, 10.720440991339393, 12.988650063170537, 18.14700750425752 ]

### 2.4 Библиотечные функции

Реализуйте следующие те же самые lambda-функции, реализованные в п.2.3, используя
библиотечные функции: ``foldl``, ``map``

Дополнительно:
* Реализуйте собственную функцию ``my_lists:filtermap`` через ``lists:foldl``. Синтаксис
``my_lists:filtermap`` должен совпадать с синтаксисом ``lists:filtermap``

### 2.5 Задача повышенной сложности - ленивые вычисления

Ознакомьтесь с концепцией ленивых вычислений.

* Скачайте и установите ``cowboy``  web-сервер, написанный на ``erlang``. Реализуйте модуль HTTP ответа,
который будет возвращать ``200 OK`` для HTTP ``GET`` запросов на страницу ``http://localhost:8080/simple_response``.
Используйте следующую [инструкцию](https://github.com/ninenines/cowboy/blob/master/doc/src/guide/getting_started.asciidoc)

* Дополнительно реализуйте модуль HTTP ответа, который будет возвращать ``200 OK`` для HTTP ``GET`` запросов на страницу
``http://localhost:8080/chunked_response`` и выводить 10 chunk ответов типа ``chunk #1``, ``chunk #2``. Симулируйте работу
каждого chunk функцией ``timer:sleet(1000)``. Используйте следующий
[пример](https://github.com/ninenines/cowboy/tree/master/examples/chunked_hello_world) для старта работы

* Дополнительно переработайте модуль HTTP ответа ``chunked_response``, чтобы отделить логику выполнения запроса от логики
посылки ответа (функция для отправки chunks ``cowboy_req:stream_body`` не должна вызывать в коде, где осуществляется подготовка
тела chunk ответа). Подсказка: напишите lambda-функцию, которая будет принимать и выполнять ленивую функцию для генерации каждого
chunk. В качестве одного из значений, что возвращает эта функция - функция, которая выполняет генерацию следующего chunk.
