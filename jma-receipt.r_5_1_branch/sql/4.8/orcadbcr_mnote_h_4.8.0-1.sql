--
-- PostgreSQL database dump
--

CREATE TABLE tbl_mnote_h (
    hospnum smallint DEFAULT 0 NOT NULL,
    tbl_key character(8) NOT NULL,
    rennum smallint DEFAULT 0 NOT NULL,
    tbl_group character(14) NOT NULL,
    tbl_uuid character varying(36),
    shori_rennum smallint DEFAULT 0 NOT NULL,
    gyoumu_cd smallint DEFAULT 0,
    location_cd char(2),
    shellid character varying(20),
    sryym character varying(6),
    sryymd character varying(8),
    cnt integer DEFAULT 0,
    nyugaikbn character(1),
    ptid bigint DEFAULT 0,
    drcd character(5),
    to_folder character varying(200),
    to_data character varying(200),
    code_type character(1),
    putflg character(1),
    title character varying(100),
	data_version char(1),
	opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);


ALTER TABLE ONLY tbl_mnote_h
    ADD CONSTRAINT tbl_mnote_h_primary_key PRIMARY KEY (hospnum, tbl_key, rennum, tbl_group,tbl_uuid, shori_rennum);

