DROP TABLE games CASCADE;
DROP TABLE words_per_game CASCADE;
DROP SEQUENCE games_id_seq;
DROP TABLE games_to_use;

CREATE TABLE games (id bigint NOT NULL, name text);
CREATE SEQUENCE games_id_seq
                     START WITH 1
                     INCREMENT BY 1
                     NO MINVALUE
                     NO MAXVALUE
                     CACHE 1;
ALTER TABLE ONLY games ALTER COLUMN id SET DEFAULT nextval('games_id_seq'::regclass);
ALTER TABLE ONLY games ADD CONSTRAINT games_pkey PRIMARY KEY (id);

CREATE TABLE words_per_game (game bigint REFERENCES games(id),word text);

CREATE TABLE games_to_use (game bigint REFERENCES games(id));

CREATE TABLE inflections_per_game (game bigint REFERENCES games(id),inflection text);

ALTER TABLE games ADD COLUMN target text;
ALTER TABLE games ADD COLUMN source text;

CREATE TABLE translation_select (id bigint NOT NULL);
ALTER TABLE translation_select ADD CONSTRAINT select_pkey PRIMARY KEY (id);
CREATE SEQUENCE translation_select_id_seq
                     START WITH 1
                     INCREMENT BY 1
                     NO MINVALUE
                     NO MAXVALUE
                     CACHE 1;
ALTER TABLE ONLY translation_select ALTER COLUMN id SET DEFAULT nextval('translation_select_id_seq'::regclass);

ALTER TABLE translation_select ADD COLUMN name TEXT;
ALTER TABLE translation_select ADD COLUMN source TEXT;
ALTER TABLE translation_select ADD COLUMN target TEXT;
ALTER TABLE translation_select ADD COLUMN source_spec JSONB; 
ALTER TABLE translation_select ADD COLUMN target_spec JSONB; 

CREATE SEQUENCE game_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

CREATE TABLE game (
    id integer DEFAULT nextval('game_id_seq'::regclass) NOT NULL,
    name text,
    source text,   -- source language
    target text);  -- target language

ALTER TABLE ONLY game ADD CONSTRAINT game_pkey PRIMARY KEY (id);

INSERT INTO game (name,source,target) VALUES ('default English to Italian','en','it');
INSERT INTO game (name,source,target) VALUES ('default English to Spanish','en','es');

-- join table with translation_select.
CREATE TABLE game_selects (
    game bigint REFERENCES game(id),
    translation_select bigint REFERENCES translation_select(id));

CREATE SEQUENCE city_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

CREATE TABLE city (
    id integer DEFAULT nextval('city_id_seq'::regclass) NOT NULL,
    name text,
    country text);

ALTER TABLE ONLY city ADD CONSTRAINT city_pkey PRIMARY KEY (id);

-- join table: which game to show for which city
CREATE TABLE city_game (
    game bigint REFERENCES game(id),
    city bigint REFERENCES city(id));

INSERT INTO city (name,country) VALUES ('Firenze','it');
INSERT INTO city (name,country) VALUES ('Barcelona','es');
INSERT INTO city (name,country) VALUES ('Mexico, D.F.','mx');

--INSERT INTO city_game
TRUNCATE city_game;

INSERT INTO city_game (city,game) 
      SELECT city.id AS city,game.id AS game 
        FROM game 
  INNER JOIN city 
          ON (game.source = 'en' AND game.target='it' AND city.country='it');

INSERT INTO city_game (city,game) 
      SELECT city.id AS city,game.id AS game 
        FROM game 
  INNER JOIN city 
          ON (game.source = 'en' AND game.target='es' AND city.country='es');

INSERT INTO city_game (city,game) 
      SELECT city.id AS city,game.id AS game 
        FROM game 
  INNER JOIN city 
          ON (game.source = 'en' AND game.target='es' AND city.country='mx');
