DROP TABLE english;

DROP TABLE italiano;

DROP TABLE espanol;

CREATE TABLE expression (id INTEGER, PRIMARY KEY(id), 
       created TIMESTAMP DEFAULT now(), 
       language TEXT, model TEXT,
       surface TEXT, synsem JSONB);

CREATE SEQUENCE expression_id_seq
                     START WITH 1
                     INCREMENT BY 1
                     NO MINVALUE
                     NO MAXVALUE
                     CACHE 1;

ALTER TABLE ONLY expression ALTER COLUMN id SET DEFAULT nextval('expression_id_seq'::regclass);


