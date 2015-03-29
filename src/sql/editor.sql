DROP TABLE or_group CASCADE;
DROP TABLE game CASCADE;
CREATE TABLE or_group (id bigint NOT NULL, name text, selects jsonb[]);
DROP SEQUENCE or_group_id_seq;
CREATE SEQUENCE or_group_id_seq
                     START WITH 1
                     INCREMENT BY 1
                     NO MINVALUE
                     NO MAXVALUE
                     CACHE 1;
ALTER TABLE ONLY or_group ALTER COLUMN id SET DEFAULT nextval('or_group_id_seq'::regclass);
ALTER TABLE ONLY or_group ADD CONSTRAINT or_group_key PRIMARY KEY (id);

CREATE TABLE game (id bigint NOT NULL, 
                   source_set bigint[], -- the game's source_set is the set of (keys of) or_groups.
       		   target_set bigint[],
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

