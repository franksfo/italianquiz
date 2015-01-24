
;; SELECT * FROM (SELECT synsem->'sem'->'pred'::text AS pred,surface FROM english) AS en WHERE en.pred='"andare"';
