SELECT source_expression.surface AS source,
       target_expression.surface AS target

  FROM (SELECT surface,structure,COUNT(grouping) AS groups
                                          FROM (  SELECT DISTINCT surface,structure,target_grouping.id AS grouping
                                                    FROM expression AS target_expression
                                              INNER JOIN spec AS target_grouping
                                                      ON target_expression.structure @> ANY(target_grouping.any_of)
                                              INNER JOIN game
                                                      ON game.id = 1 AND target_grouping.id = ANY(game.target_specs)
                                                     AND game.target = target_expression.language)
                                                      AS surface_per_grouping
                                                GROUP BY surface,structure
                                                ORDER BY groups desc) AS target_expression

  INNER JOIN (SELECT surface,structure,COUNT(grouping) AS groups
                                         FROM (  SELECT DISTINCT surface,structure, source_grouping.id AS grouping
                                                    FROM expression AS source_expression
                                              INNER JOIN spec AS source_grouping
                                                      ON source_expression.structure @> ANY(source_grouping.any_of)
                                              INNER JOIN game
                                                      ON game.id = 1 AND source_grouping.id = ANY(game.source_specs)
                                                     AND game.source = source_expression.language)
                                                      AS surface_per_grouping
                                                GROUP BY surface,structure
                                                ORDER BY groups desc) AS source_expression
          ON ((target_expression.structure->'synsem'->'sem') @> (source_expression.structure->'synsem'->'sem'))

        WHERE target_expression.groups=(SELECT COUNT(*)
                                          FROM spec AS target_grouping
                                    INNER JOIN game
                                            ON target_grouping.id = ANY(game.target_specs)
                                           AND game.id = 1)
         AND source_expression.groups=(SELECT COUNT(*)
                                         FROM spec AS source_grouping
                                   INNER JOIN game
                                           ON source_grouping.id = ANY(game.source_specs)
                                          AND game.id = 1);

