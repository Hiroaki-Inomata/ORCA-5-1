
CREATE TABLE tbl_ptconf (
    hospnum smallint DEFAULT 0 NOT NULL,
    ptid    bigint   DEFAULT 0 NOT NULL,
    ckey    character varying(25) NOT NULL,
    cdata   character varying(10),
    termid  character varying(16),
    opid    character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);


ALTER TABLE ONLY tbl_ptconf
    ADD CONSTRAINT tbl_ptconf_primary_key PRIMARY KEY (hospnum,ptid,ckey);
