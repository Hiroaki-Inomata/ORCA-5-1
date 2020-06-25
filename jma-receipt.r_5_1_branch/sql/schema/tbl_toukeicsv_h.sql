CREATE TABLE tbl_toukeicsv_h (
    hospnum smallint DEFAULT 0 NOT NULL,
    tbl_key character(8) NOT NULL,
    rennum smallint DEFAULT 0 NOT NULL,
    tbl_group character(14) NOT NULL,
    shori_rennum smallint DEFAULT 0 NOT NULL,
    shellid character varying(20) NOT NULL,
    sryym character varying(6) NOT NULL,
    sryymd character varying(8) NOT NULL,
    cnt integer DEFAULT 0,
    nyugaikbn character(1) NOT NULL,
    to_folder character varying(200) NOT NULL,
    to_data character varying(200) NOT NULL,
    code_type character(1) NOT NULL,
    putflg character(1) NOT NULL,
    title character varying(100) NOT NULL,
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_toukeicsv_h
    ADD CONSTRAINT tbl_toukeicsv_h_primary_key PRIMARY KEY (hospnum, tbl_key, rennum, tbl_group, shori_rennum);

