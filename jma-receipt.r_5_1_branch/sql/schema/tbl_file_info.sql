CREATE TABLE tbl_file_info (
    hospnum numeric(2,0) NOT NULL,
    tbl_key character(8) NOT NULL,
    sryym character varying(6) NOT NULL,
    shellid character varying(20) NOT NULL,
    rennum numeric(3,0) NOT NULL,
    shori_rennum numeric(3,0) NOT NULL,
    skyymd character varying(8),
    title character varying(100),
    teisyutusaki character(1),
    nyugaikbn character(1),
    cnt_all numeric(7,0),
    cnt_nyuin numeric(7,0),
    cnt_gairai numeric(7,0),
    for_folder character varying(200),
    for_data character varying(200),
    to_folder character varying(200),
    to_data character varying(200),
    code_type character(1),
    data_type character(1),
    err_code character(1),
    err_msg character varying(200),
    syoymd character varying(8),
    creymd character varying(8),
    crehms character varying(6)
);

ALTER TABLE ONLY tbl_file_info
    ADD CONSTRAINT tbl_file_info_primary_key PRIMARY KEY (hospnum, tbl_key, sryym, shellid, rennum, shori_rennum);

