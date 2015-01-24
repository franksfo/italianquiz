
;; SELECT * FROM (SELECT synsem->'sem'->'pred'::text AS pred,surface FROM english) AS en WHERE en.pred='"andare"';
;; SELECT it.surface  FROM (SELECT synsem->'sem'->'pred' AS pred,surface,synsem FROM italiano) AS it WHERE it.synsem->'sem' @> '{"pred":"andare"}';

;; SELECT surface FROM italiano WHERE synsem->'sem' @> '{"pred":"andare"}';

;;SELECT italiano.surface,english.surface FROM italiano INNER JOIN english ON italiano.synsem->'sem' = english.synsem->'sem';

;; number of distinct english <-> italiano translation pairs
;; SELECT count(*) FROM (SELECT DISTINCT english.surface AS en, italiano.surface AS it FROM italiano INNER JOIN english ON italiano.synsem->'sem' = english.synsem->'sem' ORDER BY english.surface) AS foo;
(ns italianverbs.borges)

(declare lookup)
(declare generate-from-request)
(declare resolve-model)

(defn generate-from-request [request]
  "respond to an HTTP client's request with a generated sentence, given the client's desired spec, language name, and language model name."
)

(defn generate [spec language-model]
  "generate a sentence matching 'spec' given the supplied language model.")

