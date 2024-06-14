DROP TABLE IF EXISTS edgar;

CREATE TABLE edgar (
  id serial PRIMARY KEY,
  edgar_id TEXT NOT NULL,
  cik TEXT NOT NULL,
  ticker TEXT NOT NULL,
  title TEXT NOT NULL
);
