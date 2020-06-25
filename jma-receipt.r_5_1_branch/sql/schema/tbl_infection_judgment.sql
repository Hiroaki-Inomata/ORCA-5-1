CREATE TABLE tbl_infection_judgment (
    id integer NOT NULL,
    disease smallint NOT NULL,
    judgment_code character varying(9),
    code_kbn smallint,
    srykbn character varying(2),
    creymd character varying(8),
    upymd character varying(8),
    uphms character varying(6)
);

ALTER TABLE ONLY tbl_infection_judgment
    ADD CONSTRAINT tbl_infection_judgment_primary_key PRIMARY KEY (id);

CREATE INDEX idx_infection_judgment_judgment_code ON tbl_infection_judgment USING btree (disease, judgment_code);

