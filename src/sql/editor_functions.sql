SELECT source_expression.surface AS source,
       target_expression.surface AS target

  FROM (SELECT surface,structure,COUNT(grouping) AS groups
                                          FROM (  SELECT DISTINCT surface,structure,target_grouping.id AS grouping
                                                    FROM expression AS target_expression
                                              INNER JOIN grouping AS target_grouping
                                                      ON target_expression.structure @> ANY(target_grouping.any_of)
                                              INNER JOIN game
                                                      ON game.id = 1 AND target_grouping.id = ANY(game.target_groupings)
                                                     AND game.target = target_expression.language)
                                                      AS surface_per_grouping
                                                GROUP BY surface,structure
                                                ORDER BY groups desc) AS target_expression

  INNER JOIN (SELECT surface,structure,COUNT(grouping) AS groups
                                         FROM (  SELECT DISTINCT surface,structure, source_grouping.id AS grouping
                                                    FROM expression AS source_expression
                                              INNER JOIN grouping AS source_grouping
                                                      ON source_expression.structure @> ANY(source_grouping.any_of)
                                              INNER JOIN game
                                                      ON game.id = 1 AND source_grouping.id = ANY(game.source_groupings)
                                                     AND game.source = source_expression.language)
                                                      AS surface_per_grouping
                                                GROUP BY surface,structure
                                                ORDER BY groups desc) AS source_expression
          ON ((target_expression.structure->'synsem'->'sem') @> (source_expression.structure->'synsem'->'sem'))

        WHERE target_expression.groups=(SELECT COUNT(*)
                                          FROM grouping AS target_grouping
                                    INNER JOIN game
                                            ON target_grouping.id = ANY(game.target_groupings)
                                           AND game.id = 1)
         AND source_expression.groups=(SELECT COUNT(*)
                                         FROM grouping AS source_grouping
                                   INNER JOIN game
                                           ON source_grouping.id = ANY(game.source_groupings)
                                          AND game.id = 1);

SELECT surface AS source,count(group_id) AS num_source_groups FROM (SELECT DISTINCT surface,structure,source_grouping.id AS group_id
          FROM expression AS source_expression
    INNER JOIN grouping AS source_grouping
            ON source_expression.structure @> ANY(source_grouping.any_of)
    INNER JOIN game
            ON game.id = 1
           AND source_grouping.id = ANY(game.source_groupings)
           AND game.source = source_expression.language
           ORDER BY surface,source_grouping.id) AS sources GROUP BY surface ORDER BY num_source_groups DESC;

SELECT surface AS target,structure,count(group_id) AS num_target_groups 
          FROM (SELECT DISTINCT surface,structure,target_grouping.id AS group_id
                           FROM expression AS target_expression
                     INNER JOIN grouping AS target_grouping
                             ON target_expression.structure @> ANY(target_grouping.any_of)
                     INNER JOIN game
                             ON game.id = 1
                            AND target_grouping.id = ANY(game.target_groupings)
                            AND game.target = target_expression.language
   
                       ORDER BY surface) AS target_expression
          WHERE target_expression.groups=(SELECT COUNT(*)
                                                 FROM grouping AS target_grouping
                                           INNER JOIN game
                                                   ON target_grouping.id = ANY(game.num_target_groups)
                                                  AND game.id = 1)
      GROUP BY surface ,structure
      ORDER BY num_target_groups DESC;


SELECT surface,groups
  FROM (SELECT DISTINCT surface,structure,count(target_grouping.id) AS groups
                   FROM expression AS target_expression
             INNER JOIN grouping AS target_grouping
                     ON target_expression.structure @> ANY(target_grouping.any_of)
             INNER JOIN game
                     ON game.id = 1
                    AND target_grouping.id = ANY(game.target_groupings)
                    AND game.target = target_expression.language
               GROUP BY surface,structure) AS targets
  WHERE groups = (SELECT COUNT(*)
                    FROM grouping AS target_grouping
              INNER JOIN game
                      ON target_grouping.id = ANY(game.target_groupings)
                     AND game.id = 1);




SELECT DISTINCT surface,count(target_grouping.id) AS groups
                   FROM (SELECT DISTINCT surface,structure,language
                                    FROM expression) AS target_expression
             INNER JOIN grouping AS target_grouping
                     ON target_expression.structure @> ANY(target_grouping.any_of)
             INNER JOIN game
                     ON game.id = 1
                    AND target_grouping.id = ANY(game.target_groupings)
                    AND game.target = target_expression.language
               GROUP BY surface,structure;


SELECT source.surface AS source,target.surface AS target
  FROM (SELECT surface AS surface,structure AS structure
          FROM (SELECT DISTINCT surface,structure,count(target_grouping.id) AS groups
                           FROM (SELECT DISTINCT surface,structure,language
                                            FROM expression) AS target_expression
                     INNER JOIN grouping AS target_grouping
                             ON target_expression.structure @> ANY(target_grouping.any_of)
                     INNER JOIN game
                             ON game.id = 1
                            AND target_grouping.id = ANY(game.target_groupings)
                            AND game.target = target_expression.language
                       GROUP BY surface,structure) AS targets
         WHERE groups = (SELECT COUNT(*)
                           FROM grouping AS target_grouping
                     INNER JOIN game
                             ON target_grouping.id = ANY(game.target_groupings)
                            AND game.id = 1)) AS target

INNER JOIN (SELECT surface AS surface,structure AS structure
              FROM (SELECT DISTINCT surface,structure,count(source_grouping.id) AS groups
                               FROM (SELECT DISTINCT surface,structure,language
                                                FROM expression) AS source_expression
                         INNER JOIN grouping AS source_grouping
                                 ON source_expression.structure @> ANY(source_grouping.any_of)
                         INNER JOIN game
                                 ON game.id = 1
                                AND source_grouping.id = ANY(game.source_groupings)
                                AND game.source = source_expression.language
                           GROUP BY surface,structure) AS sources
             WHERE groups = (SELECT COUNT(*)
                               FROM grouping AS source_grouping
                         INNER JOIN game
                                 ON source_grouping.id = ANY(game.source_groupings)
                                AND game.id = 1)) AS source
       ON (target.structure->'synsem'->'sem') @> (source.structure->'synsem'->'sem');


