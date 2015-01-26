-- number of distinct english <-> italiano translation pairs
SELECT DISTINCT * FROM (SELECT english.surface AS en, italiano.surface AS it 
                                             FROM expression AS italiano 
                                       INNER JOIN expression AS english 
                                               ON italiano.language = 'it' 
                                              AND english.language = 'en'
                                              AND italiano.structure->'synsem'->'sem' = english.structure->'synsem'->'sem') AS pairs;
