# Начало работы

Для запуска приложения
1) для запуска сервиса в корне проекта запустить `stack run`
Бексенд запустится на порту 8080
2) Для запуска фронтенда перейти в каталог frontend (должен быть установлен elm), выполнить команду `elm reactor` 
Фронтенд запустится локально на порту 8000
Перейти в браузере на http://localhost:8000, открыть папку src, файл Main.elm


## examples

GET
curl http://localhost:8080/messages/all

curl http://localhost:8080/messages/sortedby/approve?isapproved=false

curl http://localhost:8080/messages/sortedby/author?authorname="Me"


POST
curl -X POST -d '{"content":"message test text", "approved" : false , "author":"MyName"}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8080/messages/create
