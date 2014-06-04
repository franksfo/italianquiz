-- Must do this with DB superuser privileges:
--
-- CREATE DATABASE verbcoach OWNER verbcoach;
-- 
--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

-- CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

-- COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: verb; Type: TABLE; Schema: public; Owner: verbcoach; Tablespace: 
--

CREATE TABLE verb (
    id bigint NOT NULL,
    created timestamp without time zone DEFAULT now(),
    updated timestamp without time zone,
    value text
);


ALTER TABLE public.verb OWNER TO verbcoach;

--
-- Name: vgroup; Type: TABLE; Schema: public; Owner: verbcoach; Tablespace: 
--

CREATE TABLE vgroup (
    id bigint NOT NULL,
    created timestamp without time zone DEFAULT now(),
    updated timestamp without time zone,
    verbs bigint ARRAY,
    name text
);


ALTER TABLE public.vgroup OWNER TO verbcoach;

--
-- Name: id; Type: DEFAULT; Schema: public; Owner: verbcoach
--

-- Name: verb_id_seq; Type: SEQUENCE; Schema: public; Owner: verbcoach
--

CREATE SEQUENCE verb_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

CREATE SEQUENCE vgroup_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER TABLE public.verb_id_seq OWNER TO verbcoach;

ALTER TABLE ONLY verb ALTER COLUMN id SET DEFAULT nextval('verb_id_seq'::regclass);

ALTER TABLE public.vgroup_id_seq OWNER TO verbcoach;

ALTER TABLE ONLY vgroup ALTER COLUMN id SET DEFAULT nextval('vgroup_id_seq'::regclass);

--
-- Name: verb_pkey; Type: CONSTRAINT; Schema: public; Owner: verbcoach; Tablespace: 
--

ALTER TABLE ONLY verb
    ADD CONSTRAINT verb_pkey PRIMARY KEY (id);

--
-- Name: vgroup_pkey; Type: CONSTRAINT; Schema: public; Owner: verbcoach; Tablespace: 
--

ALTER TABLE ONLY vgroup
    ADD CONSTRAINT vgroup_pkey PRIMARY KEY (id);

--
-- Name: verb_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: verbcoach
--

ALTER SEQUENCE verb_id_seq OWNED BY verb.id;

--
-- Name: vgroup_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: verbcoach
--

ALTER SEQUENCE vgroup_id_seq OWNED BY vgroup.id;

CREATE TABLE test (
    id bigint NOT NULL,
    created timestamp without time zone DEFAULT now(),
    updated timestamp without time zone,
    name text
);

ALTER TABLE public.test OWNER TO verbcoach;
CREATE SEQUENCE test_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE public.test_id_seq OWNER TO verbcoach;
ALTER TABLE ONLY test ALTER COLUMN id SET DEFAULT nextval('test_id_seq'::regclass);
ALTER TABLE ONLY test
    ADD CONSTRAINT test_pkey PRIMARY KEY (id);

CREATE TABLE question (
    created timestamp without time zone DEFAULT now(),
    english text,
    id bigint NOT NULL,
    italian text,
    test bigint REFERENCES test(id),
    updated timestamp without time zone
);

ALTER TABLE public.question OWNER TO verbcoach;
CREATE SEQUENCE question_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE public.question_id_seq OWNER TO verbcoach;
ALTER TABLE ONLY question ALTER COLUMN id SET DEFAULT nextval('question_id_seq'::regclass);
ALTER TABLE ONLY question
    ADD CONSTRAINT question_pkey PRIMARY KEY (id);

ALTER TABLE ONLY question ADD COLUMN index INTEGER NOT NULL;

ALTER TABLE question DROP COLUMN index;

CREATE TABLE vc_user (
       id bigint NOT NULL,
       created timestamp without time zone DEFAULT now(),
       updated timestamp without time zone,
       fullname text,
       email text,
       username text
);
CREATE SEQUENCE user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE public.user_id_seq OWNER TO verbcoach;
ALTER TABLE ONLY vc_user ALTER COLUMN id SET DEFAULT nextval('user_id_seq'::regclass);
ALTER TABLE ONLY vc_user
    ADD CONSTRAINT user_pkey PRIMARY KEY (id);

CREATE TABLE tsubmit (
       id bigint NOT NULL,
       created timestamp without time zone DEFAULT now(),
       updated timestamp without time zone,
       test bigint REFERENCES test(id),
       student bigint REFERENCES vc_user(id)     
);
CREATE SEQUENCE tsubmit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE public.tsubmit_id_seq OWNER TO verbcoach;
ALTER TABLE ONLY tsubmit ALTER COLUMN id SET DEFAULT nextval('tsubmit_id_seq'::regclass);
ALTER TABLE ONLY tsubmit
    ADD CONSTRAINT tsubmit_pkey PRIMARY KEY (id);

CREATE TABLE qsubmit (
       id bigint NOT NULL,
       created timestamp without time zone DEFAULT now(),
       updated timestamp without time zone,
       question bigint REFERENCES question(id),
       tsubmit bigint REFERENCES tsubmit(id)     
);
CREATE SEQUENCE qsubmit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE public.qsubmit_id_seq OWNER TO verbcoach;
ALTER TABLE ONLY qsubmit ALTER COLUMN id SET DEFAULT nextval('qsubmit_id_seq'::regclass);
ALTER TABLE ONLY qsubmit
    ADD CONSTRAINT qsubmit_pkey PRIMARY KEY (id);

CREATE TYPE user_type AS ENUM ('teacher','student');

ALTER TABLE ONLY vc_user ADD COLUMN type user_type DEFAULT 'student';

INSERT INTO vc_user (username,type) VALUES ('franco','teacher');
INSERT INTO vc_user (username,type) VALUES ('gino','student');
