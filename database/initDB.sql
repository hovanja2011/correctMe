CREATE TABLE IF NOT EXISTS "messages"
  ( id       SERIAL PRIMARY KEY UNIQUE
  , content  text NOT NULL
  , approved BOOLEAN NOT NULL DEFAULT false
  , author   text NOT NULL 
  );

insert into "messages" (id, content, approved, author) values
  (1, 'not_approved_message', false, 'Me')
, (2, 'approved_message', true, 'boss')