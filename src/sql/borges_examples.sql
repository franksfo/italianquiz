-- distinct english <-> italiano translation pairs
SELECT DISTINCT * FROM (SELECT english.surface AS en, 
                              italiano.surface AS it,
                              italiano.structure->'synsem'->'sem'->'subj'->'pred' AS subject_it,
                              italiano.structure->'synsem'->'sem'->'pred' AS pred_it,
                              italiano.structure->'synsem'->'sem'->'subj'->'pred' AS subject_it,
                              italiano.structure->'synsem'->'sem'->'pred' AS pred_it
                                             FROM expression AS italiano 
                                       INNER JOIN expression AS english 
                                               ON italiano.language = 'it' 
                                              AND english.language = 'en'
                                              AND italiano.structure->'synsem'->'sem' = english.structure->'synsem'->'sem') AS pairs;

-- distinct espanol <-> italiano translation pairs
SELECT DISTINCT * FROM (SELECT english.surface AS en, 
                               espanol.surface AS es,
                               espanol.structure->'synsem'->'sem'->'subj'->'pred' AS subject_es,
                               espanol.structure->'synsem'->'sem'->'pred' AS pred_es,
                                espanol.structure->'synsem'->'sem'->'subj'->'pred' AS subject_es,
                               espanol.structure->'synsem'->'sem'->'pred' AS pred_es,
                              english.structure->'synsem'->'sem',
                              espanol.structure->'synsem'->'sem'
                                              FROM expression AS espanol 
                                        INNER JOIN expression AS english 
                                                ON espanol.language = 'es' 
                                               AND english.language = 'en'
                                               AND espanol.structure->'synsem'->'sem' = english.structure->'synsem'->'sem') AS pairs;

-- same but containment:
-- distinct english <-> italiano translation pairs
SELECT DISTINCT * FROM (SELECT english.surface AS en, 
                              italiano.surface AS it
                                             FROM expression AS italiano 
                                       INNER JOIN expression AS english 
                                               ON italiano.language = 'it' 
                                              AND english.language = 'en'
                                              AND (italiano.structure->'synsem'->'sem') @> (english.structure->'synsem'->'sem')) AS pairs;

-- distinct espanol <-> italiano translation pairs
SELECT DISTINCT * FROM (SELECT english.surface AS en, 
                               espanol.surface AS es,
                                              FROM expression AS espanol 
                                        INNER JOIN expression AS english 
                                                ON espanol.language = 'es' 
                                               AND english.language = 'en'
                                               AND (espanol.structure->'synsem'->'sem') @> (english.structure->'synsem'->'sem')) AS pairs;

-- examples where there is no English equivalent (an erroroneous situation)
     SELECT italiano.surface,english.surface 
       FROM expression AS italiano 
  LEFT JOIN expression AS english 
         ON italiano.structure->'synsem'->'sem' = english.structure->'synsem'->'sem' 
        AND italiano.language = 'it' 
        AND english.language='en'
      WHERE english IS NULL AND italiano.language='it';


