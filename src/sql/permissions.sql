-- Run as psql superuser
--
GRANT ALL ON TABLE vgroup TO verbcoach;
GRANT ALL ON SEQUENCE verb_id_seq TO verbcoach;
ALTER TABLE verb OWNER TO verbcoach;
ALTER TABLE vgroup OWNER TO verbcoach;
