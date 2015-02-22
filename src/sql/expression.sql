CREATE SEQUENCE expression_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

CREATE TABLE expression (
    id integer DEFAULT nextval('expression_id_seq'::regclass) NOT NULL,
    created timestamp without time zone DEFAULT now(),
    language text,
    model text,
    surface text,
    structure jsonb,
    serialized text
);

CREATE INDEX ON expression USING gin ((((structure -> 'synsem'::text) -> 'sem'::text)));

-- see all the english->italian pairs:
-- SELECT DISTINCT en.surface AS en, it.surface AS it FROM expression AS en RIGHT JOIN expression AS it ON (en.structure->'synsem'->'sem') @> (it.structure->'synsem'->'sem') AND en.language='en' AND en.language='en' WHERE it.language='it';

