# examples

GET
curl http://localhost:8080/messages/all

curl http://localhost:8080/messages/sortedby/approve?isapproved=false

curl http://localhost:8080/messages/sortedby/author?authorname="Me"


POST
curl -X POST -d '{"content":"message test text", "approved" : false , "author":"MyName"}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8080/messages/create
