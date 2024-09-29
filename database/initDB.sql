CREATE TABLE IF NOT EXISTS "messages"
  ( id       SERIAL PRIMARY KEY UNIQUE
  , content  text NOT NULL
  , approved BOOLEAN NOT NULL DEFAULT false
  , author   text NOT NULL 
  );

INSERT INTO "messages" (content, approved, author) VALUES 
  ('Тестовое подтвержденное сообщение', true, 'Boss'),
  ('Тестовое неподтвержденное сообщение', false, 'Worker');
