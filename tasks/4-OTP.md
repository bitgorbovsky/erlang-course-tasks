# Блок "OTP"

## Стандартная документация

 - [OTP design principles](http://erlang.org/doc/design_principles/des_princ.html)
 - [Общие сведения](http://erlang.org/doc/reference_manual/processes.html)

### Материалы для изучения

#### Лекция 1:
 - [What is OTP?](http://learnyousomeerlang.com/what-is-otp)
 - [Clients and Servers](http://learnyousomeerlang.com/clients-and-servers)
 - [Rage Against The Finite-State Machines](http://learnyousomeerlang.com/finite-state-machines)

#### Лекция 2:
 - [Who Supervises The Supervisors?](http://learnyousomeerlang.com/supervisors)
 - [Building an Application With OTP](http://learnyousomeerlang.com/building-applications-with-otp)
 - [Building OTP Applications](http://learnyousomeerlang.com/building-otp-applications)
 - [Release is the Word](http://learnyousomeerlang.com/release-is-the-word)

#### Лекция 3:
 - [Event Handlers](http://learnyousomeerlang.com/event-handlers)
 - [Behaviour Module Attribute](http://erlang.org/doc/reference_manual/modules.html)
 - [Building your own behaviour](https://github.com/erlang/otp/blob/master/lib/stdlib/src/gen_server.erl)

### Упражнения

#### 4.1. Db on gen_server

Все то же хранилище ключ-значение, но поверх ОТР.
Требуемый API:
```erlang
%% Создает новую таблицу (стартует процес)
-spec new(Name :: atom()) -> ok |{error, Reason :: term()}.

%% Удаляет таблицу (останавливает процесс)
-spec delete(Name :: atom()) -> ok.

%% Удаляет запись из таблицы по ключу
-spec delete(Name :: atom(), Key :: term()) -> ok.

%% Удаляет все записи из таблицы
-spec delete_all_objects(Name :: atom()) -> ok.

%% Добавляет запись в таблицу или заменяет если ключ уже существует  
-spec insert(Name :: atom(), Key :: term(), Value :: term()) -> ok.

%% Ищет запись по ключу
-spec find(Name :: atom(), Value :: term()) -> 
    {ok, Value :: term()} | not_found.
```

Сравните код с заданием 3.1

#### 4.2 Банкомат (gen_statem)

Реализуйте процесс, симулирующий банкомат, который позволяет снять деньги.
Последовательность действий:
- При старте банкомату передается список c номерами карточек, пин кодами и балансами (см. API ниже). 
- Банкомат находится в режиме ожидания.
- Пользователь вставляет карточку (номер) - банкомат запрашивает код.
- При неправильно введенном пин коде банкомат возвращает карточку.
- Если код не введен и в течении 10 секунд ни одна клавиша не 
нажата - банкомат возвращает карту.
- Если правильно введена - просит ввести сумму для снятия.
- если сумма доступна - обновляет баланс + выдает деньги и карточку
- если не доступна - предлагает ввести еще раз
- По нажатию кнопки отмены возвращает карту.

Требуемый API:
```erlang
%% Создает Банкомат (стартует процес)
-spec start_link([{CardNo :: integer(), Pin :: list(integer()), Balance :: integer()}]) 
-> {ok, pid()} | {error, any()}.

%% Вставить карту
-spec insert_card(CardNo :: integer()) -> ok  | {error, Reason :: term()}.

%% Нажать кнопку
-spec push_button(Button ::  enter, cancel, '0','1','2','3','4','5','6','7','8', '9') -> 
continue | {ok, Result :: term()} | {error, Reason :: term()}.
```

Дополнительные задания:
- Изымать карточку если пароль введен не правильно 3 раза подряд
- Позволить вносить деньги на счет
- Реализовать выдачу денег и карты через отдельные состояния 
в каждом из которых ждать действия от пользователя и завершать 
операцию по таймеру.

#### 4.3 Open Telecom Bank (application + supervisor)

Напишите приложение позволяющее создавать сеть банкоматов.
Процессы, обеспечивающие работу банкоматов, должны быть независимыми.
При смерти такого процесса он должен быть перезапущен.
Информацию о счетах (карточках) предлагается хранить в коллекции ключ-значение.
При смерти такого процесса он должен быть рестартован вместе с банкоматами.
Начальное состояние счетов может быть фиксированным. 

Добавьте отладочный вывод при запуске и смерти процессов.
Поэкспериментируйте с процессами, уничтожая их один за другим 
при помощи exit(Pid, kill).

Доп задания:
- Храните информацию о счетах в файле. 
Считывайте его при старте и обновляйте при каждой записи.
- Реализуйте атомарные операции снятия/внесения денег на счет чтобы избежать проблем при
паралельной работе.

