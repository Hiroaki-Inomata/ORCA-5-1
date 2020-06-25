CREATE TABLE tbl_help (
    helpkbn character(1) NOT NULL,
    key_opid character varying(16) NOT NULL,
    ldname character varying(20) NOT NULL,
    menunum1 numeric(4,0) NOT NULL,
    menunum2 numeric(4,0) NOT NULL,
    edanum numeric(4,0) NOT NULL,
    menu2flg numeric(1,0),
    menu character varying(60),
    help character varying(100),
    termid character varying(64),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_help
    ADD CONSTRAINT tbl_help_primary_key PRIMARY KEY (hospnum, helpkbn, key_opid, ldname, menunum1, menunum2, edanum);

CREATE INDEX idx_help_key_opid ON tbl_help USING btree (hospnum, key_opid);

CREATE INDEX idx_help_menu ON tbl_help USING btree (hospnum, menu);

