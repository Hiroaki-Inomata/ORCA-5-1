DROP TABLE IF EXISTS tbl_okusuri_kanri;

CREATE TABLE tbl_okusuri_kanri (
    hospnum smallint DEFAULT 0 NOT NULL,
    ptid    bigint   DEFAULT 0 NOT NULL,
    sryymd character varying(8) NOT NULL,
    hospcd character varying(7) NOT NULL,
    rennum smallint DEFAULT 0 NOT NULL,
    name character varying(100),
    sex character varying(1),
    birthday character varying(8),
    hospname character varying(200),
    data_key	bigint   DEFAULT 0 NOT NULL,
    termid  character varying(16),
    opid    character varying(16),
    creymd character varying(8),
    upymd character varying(8),
    uphms character varying(6)
);

ALTER TABLE ONLY tbl_okusuri_kanri
    ADD CONSTRAINT tbl_okusuri_kanri_primary_key PRIMARY KEY (hospnum,ptid,sryymd,hospcd,rennum);
