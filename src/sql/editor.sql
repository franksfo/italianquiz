DROP TABLE grouping CASCADE;
DROP TABLE game CASCADE;
CREATE TABLE grouping (id bigint NOT NULL, name text, any_of jsonb[]);
DROP SEQUENCE grouping_id_seq;
CREATE SEQUENCE grouping_id_seq
                     START WITH 1
                     INCREMENT BY 1
                     NO MINVALUE
                     NO MAXVALUE
                     CACHE 1;
ALTER TABLE ONLY grouping ALTER COLUMN id SET DEFAULT nextval('grouping_id_seq'::regclass);
ALTER TABLE ONLY grouping ADD CONSTRAINT grouping_key PRIMARY KEY (id);

CREATE TABLE game (id bigint NOT NULL, 
                   source_groupings bigint[], -- the game's source_groupings is the set of groupings that select the source sentences.
       		   target_groupings bigint[], -- the game's target_groupings is the set of groupings that select the target sentences.
                   name text, source text,target text);

DROP SEQUENCE game_id_seq;
CREATE SEQUENCE game_id_seq
                     START WITH 1
                     INCREMENT BY 1
                     NO MINVALUE
                     NO MAXVALUE
                     CACHE 1;

ALTER TABLE ONLY game ALTER COLUMN id SET DEFAULT nextval('game_id_seq'::regclass);
ALTER TABLE ONLY game ADD CONSTRAINT game_pkey PRIMARY KEY (id);

DROP TABLE city CASCADE;
DROP SEQUENCE city_id_seq CASCADE;
DROP TABLE city_game CASCADE;

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

