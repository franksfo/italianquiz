<<<<<<< HEAD
CREATE TABLE english (id INTEGER, PRIMARY KEY(id), created TIMESTAMP DEFAULT now(), surface TEXT, syntax JSON, semantics JSON);

CREATE TABLE italiano (id INTEGER, PRIMARY KEY(id), created TIMESTAMP DEFAULT now(), surface TEXT, syntax JSON, semantics JSON);

CREATE TABLE espanol (id INTEGER, PRIMARY KEY(id), created TIMESTAMP DEFAULT now(), surface TEXT, syntax JSON, semantics JSON);

CREATE SEQUENCE english_id_seq
=======
DROP TABLE expression; 

CREATE TABLE expression (id INTEGER, PRIMARY KEY(id), 
       created TIMESTAMP DEFAULT now(), 
       language TEXT, model TEXT,
       surface TEXT, structure JSONB,
       serialized TEXT);

CREATE SEQUENCE expression_id_seq
>>>>>>> borges
                     START WITH 1
                     INCREMENT BY 1
                     NO MINVALUE
                     NO MAXVALUE
                     CACHE 1;
<<<<<<< HEAD
ALTER TABLE ONLY english ALTER COLUMN id SET DEFAULT nextval('english_id_seq'::regclass);


CREATE SEQUENCE italiano_id_seq
                     START WITH 1
                     INCREMENT BY 1
                     NO MINVALUE
                     NO MAXVALUE
                     CACHE 1;
ALTER TABLE ONLY italiano ALTER COLUMN id SET DEFAULT nextval('italiano_id_seq'::regclass);

CREATE SEQUENCE espanol_id_seq
                     START WITH 1
                     INCREMENT BY 1
                     NO MINVALUE
                     NO MAXVALUE
                     CACHE 1;
ALTER TABLE ONLY espanol ALTER COLUMN id SET DEFAULT nextval('espanol_id_seq'::regclass);
=======

ALTER TABLE ONLY expression ALTER COLUMN id SET DEFAULT nextval('expression_id_seq'::regclass);
>>>>>>> borges


