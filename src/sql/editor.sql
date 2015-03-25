DROP TABLE or_group CASCADE;
DROP TABLE game CASCADE;
CREATE TABLE or_group (id bigint NOT NULL, name text, selects jsonb[]);
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

INSERT INTO or_group (name,selects) VALUES ('Useful Spanish Verbs',
       ARRAY['{"espanol":{"espanol": "ayudar" }}'::jsonb,
             '{"espanol":{"espanol": "comer" }}'::jsonb,
	     -- ... etc .. add more verbs here..
             '{"espanol":{"espanol": "hablar" }}'::jsonb]);

INSERT INTO or_group (name,selects) VALUES ('Present tense',
                                            ARRAY['{"synsem":{"sem":{"infl":"tense"}}}'::jsonb]);

INSERT INTO or_group (name,selects) VALUES ('Empty','{}');

INSERT INTO game (name,source_set,target_set,source,target) 
     SELECT 'The Useful Spanish game', ARRAY[source.id],ARRAY[target.id],'en','es' 
       FROM or_group AS source
 INNER JOIN or_group AS target
         ON source.name = 'Empty'
        AND (target.name = 'Useful Spanish Verbs'
         OR  target.name = 'Present tense');

-- Get selects for all or_groups for the game called 'The Useful Spanish game':
-- 
    SELECT source.selects AS source,target.selects AS target
      FROM game 
INNER JOIN or_group AS source
        ON source.id = ANY(game.source_set) 
INNER JOIN or_group AS target
        ON target.id = ANY(game.target_set) 
     WHERE game.name='The Useful Spanish game';

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

