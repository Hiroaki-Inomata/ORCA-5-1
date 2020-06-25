
CREATE TABLE tbl_mnote_b (
    hospnum smallint DEFAULT 0 NOT NULL,
    tbl_key character(8) NOT NULL,
    rennum smallint DEFAULT 0 NOT NULL,
    tbl_group character(14) NOT NULL,
    tbl_uuid character varying(36),
    shori_rennum smallint DEFAULT 0 NOT NULL,
    shellid character varying(20) NOT NULL,
    sryym character varying(6) NOT NULL,
    sryymd character varying(8) NOT NULL,
    seq_no integer DEFAULT 0 NOT NULL,
    csvdata character varying(20000) NOT NULL,
    creymd character(8),
    upymd character(8),
    uphms character(6)
);



ALTER TABLE ONLY tbl_mnote_b
    ADD CONSTRAINT tbl_mnote_b_primary_key PRIMARY KEY (hospnum, tbl_key, rennum, tbl_group,tbl_uuid, shori_rennum, seq_no);

