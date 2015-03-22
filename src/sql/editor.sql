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
