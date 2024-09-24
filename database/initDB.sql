CREATE TABLE IF NOT EXISTS "messages"
  ( id       SERIAL PRIMARY KEY UNIQUE
  , content  text NOT NULL
  , approved BOOLEAN NOT NULL DEFAULT false
  , author   text NOT NULL 
  );

