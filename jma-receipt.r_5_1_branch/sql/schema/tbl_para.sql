CREATE TABLE tbl_para (
    gyoumuid character(3) NOT NULL,
    termid character(64) NOT NULL,
    filemei character(20) NOT NULL,
    edanum numeric(3,0) NOT NULL,
    data_rec character(5000),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_para
    ADD CONSTRAINT tbl_para_primary_key PRIMARY KEY (hospnum, gyoumuid, termid, filemei, edanum);

