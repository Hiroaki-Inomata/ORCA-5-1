CREATE TABLE tbl_adrs (
    lpubcd character varying(5) NOT NULL,
    post character varying(7) NOT NULL,
    rennum smallint NOT NULL,
    prefkana character varying(40),
    citykana character varying(200),
    townkana character varying(400),
    prefname character varying(20),
    cityname character varying(100),
    townname character varying(200),
    editadrs_kana character varying(640),
    editadrs_name character varying(320),
    towndivflg character varying(1) DEFAULT '0'::bpchar,
    manytownflg character varying(1) DEFAULT '0'::bpchar,
    hospnum smallint NOT NULL,
    townname2 character varying(1200)
);

ALTER TABLE ONLY tbl_adrs
    ADD CONSTRAINT tbl_adrs_primary_key PRIMARY KEY (hospnum, lpubcd, post, rennum);

CREATE INDEX idx_adrs_post ON tbl_adrs USING btree (hospnum, post);

