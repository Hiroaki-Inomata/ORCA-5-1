CREATE TABLE tbl_henrei_kanri (
    hospnum smallint NOT NULL,
    teisyutusaki character(1) NOT NULL,
    syoriym character(6) NOT NULL,
    recv_total_cnt integer,
    recv_patient_cnt integer,
    recv_go_cnt integer,
    recv_go_ten bigint,
    termid character varying(64),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    teisyutusaki2 character(1) NOT NULL
);

ALTER TABLE ONLY tbl_henrei_kanri
    ADD CONSTRAINT tbl_henrei_kanri_primary_key PRIMARY KEY (hospnum, teisyutusaki, teisyutusaki2, syoriym);

