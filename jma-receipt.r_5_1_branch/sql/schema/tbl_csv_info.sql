CREATE TABLE tbl_csv_info (
    hospnum numeric(2,0) NOT NULL,
    tbl_key character(8) NOT NULL,
    sryym character varying(6) NOT NULL,
    shellid character varying(20) NOT NULL,
    rennum numeric(3,0) NOT NULL,
    shori_rennum numeric(3,0) NOT NULL,
    seq_no numeric(6,0) NOT NULL,
    nyugaikbn character(1),
    skyymd character varying(8),
    teisyutusaki character(1),
    ptid numeric(10,0),
    csvdata character varying(2000),
    creymd character varying(8),
    crehms character varying(6)
);

ALTER TABLE ONLY tbl_csv_info
    ADD CONSTRAINT tbl_csv_info_primary_key PRIMARY KEY (hospnum, tbl_key, sryym, shellid, rennum, shori_rennum, seq_no);

