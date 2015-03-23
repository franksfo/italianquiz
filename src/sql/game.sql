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

-- join table with translation_select (defined in editor.sql)
CREATE TABLE game_selects (
    game bigint REFERENCES game(id),
    translation_select bigint REFERENCES translation_select(id));


