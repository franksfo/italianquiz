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
--
-- PostgreSQL database dump complete
--

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


