TRUNCATE grouping;

INSERT INTO grouping (name,selects) VALUES ('Useful Spanish Verbs',
       ARRAY['{"espanol":{"espanol": "ayudar" }}'::jsonb,
             '{"espanol":{"espanol": "comer" }}'::jsonb,
	     -- ... etc .. add more verbs here..
             '{"espanol":{"espanol": "hablar" }}'::jsonb]);

INSERT INTO grouping (name,selects) VALUES ('Present tense',
                                            ARRAY['{"synsem":{"sem":{"infl":"tense"}}}'::jsonb]);

INSERT INTO grouping (name,selects) VALUES ('Empty','{}');

TRUNCATE game;

INSERT INTO game (name,source_set,target_set,source,target) 
     SELECT 'The Useful Spanish game', ARRAY[source.id],ARRAY[target.id],'en','es' 
       FROM grouping AS source
 INNER JOIN grouping AS target
         ON source.name = 'Empty'
        AND (target.name = 'Useful Spanish Verbs'
         OR  target.name = 'Present tense');

-- Get selects for all groupings for the game called 'The Useful Spanish game':
-- 
    SELECT source.selects AS source,target.selects AS target
      FROM game 
INNER JOIN grouping AS source
        ON source.id = ANY(game.source_set) 
INNER JOIN grouping AS target
        ON target.id = ANY(game.target_set) 
     WHERE game.name='The Useful Spanish game';

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

