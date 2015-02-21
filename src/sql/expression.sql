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
