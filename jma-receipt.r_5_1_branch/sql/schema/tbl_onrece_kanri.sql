CREATE TABLE tbl_onrece_kanri (
    hospnum numeric(2,0) NOT NULL,
    teisyutusaki character(1) NOT NULL,
    sryym character(6) NOT NULL,
    ucnt numeric(3,0) NOT NULL,
    scnt numeric(3,0) NOT NULL,
    rennum numeric(3,0) NOT NULL,
    syoriymd character(8),
    ukeymd character(8),
    skyymd character(8),
    recv_flg numeric(1,0),
    amount_flg numeric(1,0),
    send_patient_cnt numeric(5,0),
    nosend_patient_cnt numeric(5,0),
    send_total_cnt numeric(5,0),
    termid character varying(64),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_onrece_kanri
    ADD CONSTRAINT tbl_onrece_kanri_primary_key PRIMARY KEY (hospnum, teisyutusaki, sryym, ucnt, scnt, rennum);

