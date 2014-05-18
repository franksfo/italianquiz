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

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


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
-- Name: verb_id_seq; Type: SEQUENCE; Schema: public; Owner: verbcoach
--

CREATE SEQUENCE verb_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.verb_id_seq OWNER TO verbcoach;

--
-- Name: verb_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: verbcoach
--

ALTER SEQUENCE verb_id_seq OWNED BY verb.id;


--
-- Name: vgroup; Type: TABLE; Schema: public; Owner: verbcoach; Tablespace: 
--

CREATE TABLE vgroup (
    id integer NOT NULL,
    created timestamp without time zone DEFAULT now(),
    updated timestamp without time zone,
    name text
);


ALTER TABLE public.vgroup OWNER TO verbcoach;

--
-- Name: id; Type: DEFAULT; Schema: public; Owner: verbcoach
--

ALTER TABLE ONLY verb ALTER COLUMN id SET DEFAULT nextval('verb_id_seq'::regclass);


--
-- Data for Name: verb; Type: TABLE DATA; Schema: public; Owner: verbcoach
--

COPY verb (id, created, updated, value) FROM stdin;
8	2014-05-17 20:29:51.006055	\N	{:italian "parlare"}
12	2014-05-18 10:37:42.228481	\N	{:italian "dormire"}
17	2014-05-18 13:37:32.398871	\N	{:italian "dire"}
\.


--
-- Name: verb_id_seq; Type: SEQUENCE SET; Schema: public; Owner: verbcoach
--

SELECT pg_catalog.setval('verb_id_seq', 17, true);


--
-- Data for Name: vgroup; Type: TABLE DATA; Schema: public; Owner: verbcoach
--

COPY vgroup (id, created, updated, name) FROM stdin;
\.


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
-- Name: public; Type: ACL; Schema: -; Owner: ekoontz
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM ekoontz;
GRANT ALL ON SCHEMA public TO ekoontz;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

