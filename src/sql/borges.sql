CREATE TABLE english (id INTEGER, PRIMARY KEY(id), created TIMESTAMP DEFAULT now(), surface TEXT, syntax JSON, semantics JSON);

CREATE TABLE italiano (id INTEGER, PRIMARY KEY(id), created TIMESTAMP DEFAULT now(), surface TEXT, syntax JSON, semantics JSON);

CREATE TABLE espanol (id INTEGER, PRIMARY KEY(id), created TIMESTAMP DEFAULT now(), surface TEXT, syntax JSON, semantics JSON);

CREATE SEQUENCE english_id_seq
                     START WITH 1
                     INCREMENT BY 1
                     NO MINVALUE
                     NO MAXVALUE
                     CACHE 1;
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


