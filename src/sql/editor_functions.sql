-- this function does the same thing as editor.clj/(expressions-for-game).
-- TODO: editor.clj/(expressions-for-game) should just be a wrapper for
-- calling this sql function expressions_for_game().

CREATE OR REPLACE FUNCTION expressions_for_game(game_id int) 
        RETURNS TABLE (
    source text, target text) AS 
$body$
BEGIN
RETURN QUERY

  SELECT source.surface AS source,target.surface AS target

    FROM (SELECT surface AS surface,structure AS structure
            FROM (SELECT DISTINCT surface,structure,count(target_grouping.id) AS groups
                             FROM (SELECT DISTINCT surface,structure,language
                                              FROM expression) AS target_expression
                       INNER JOIN grouping AS target_grouping
                               ON target_expression.structure @> ANY(target_grouping.any_of)
                       INNER JOIN game
                               ON game.id = game_id
                              AND target_grouping.id = ANY(game.target_groupings)
                              AND game.target = target_expression.language
                         GROUP BY surface,structure) AS targets
                            WHERE groups = (SELECT COUNT(*)
                                              FROM grouping AS target_grouping
                                        INNER JOIN game
                                                ON target_grouping.id = ANY(game.target_groupings)
                                               AND game.id = game_id)) AS target


  INNER JOIN 

         (SELECT surface AS surface,structure AS structure
            FROM (SELECT DISTINCT surface,structure,count(source_grouping.id) AS groups
                             FROM (SELECT DISTINCT surface,structure,language
                                              FROM expression) AS source_expression
                       INNER JOIN grouping AS source_grouping
                               ON source_expression.structure @> ANY(source_grouping.any_of)
                       INNER JOIN game
                               ON game.id = game_id
                              AND source_grouping.id = ANY(game.source_groupings)
                              AND game.source = source_expression.language
                         GROUP BY surface,structure) AS sources
                            WHERE groups = (SELECT COUNT(*)
                                              FROM grouping AS source_grouping
                                        INNER JOIN game
                                                ON source_grouping.id = ANY(game.source_groupings)
                                               AND game.id = game_id)) AS source

          ON ((target.structure->'synsem'->'sem') @> (source.structure->'synsem'->'sem')
              OR
              (source.structure->'synsem'->'sem') @> (target.structure->'synsem'->'sem'));

END;

$body$ LANGUAGE plpgsql;

-- thanks to http://stackoverflow.com/questions/3994556/eliminate-duplicate-array-values-in-postgres
CREATE OR REPLACE FUNCTION array_sort_unique (ANYARRAY) RETURNS ANYARRAY
LANGUAGE SQL
AS $body$
  SELECT ARRAY(
    SELECT DISTINCT $1[s.i]
    FROM generate_series(array_lower($1,1), array_upper($1,1)) AS s(i)
    ORDER BY 1
  );
$body$;
