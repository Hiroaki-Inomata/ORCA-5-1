CREATE TABLE tbl_shoukaidata (
    hospnum numeric(2,0) NOT NULL,
    uid character(36) NOT NULL,
    itemid numeric(5,0) NOT NULL,
    item character varying(100),
    termid character varying(32),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_shoukaidata
    ADD CONSTRAINT tbl_shoukaidata_primary_key PRIMARY KEY (hospnum, uid, itemid);

