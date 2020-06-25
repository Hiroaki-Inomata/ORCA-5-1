CREATE TABLE tbl_onrece_body (
    hospnum numeric(2,0) NOT NULL,
    teisyutusaki character(1) NOT NULL,
    sryym character(6) NOT NULL,
    ucnt numeric(3,0) NOT NULL,
    scnt numeric(3,0) NOT NULL,
    rennum numeric(3,0) NOT NULL,
    receipt_no numeric(6,0) NOT NULL,
    syoriymd character(8),
    ukeymd character(8),
    skyymd character(8),
    recv_flg numeric(1,0),
    amount_flg numeric(1,0),
    nyugaikbn character(1),
    recesyubetu character(4),
    sryym_b character(6),
    name character varying(40),
    sex character(1),
    birthday character(8),
    ptnum character(20),
    ptid numeric(10,0),
    hknjanum character(8),
    kigo character varying(40),
    num character varying(40),
    hknnum character(3),
    teisyutusaki_b character(1),
    receka character(2),
    hojokbn character(1),
    tekstymd character(8),
    h_ttl_ten numeric(8,0),
    ftnnum_1 character varying(8),
    jkynum_1 character varying(7),
    k_ttl_ten_1 numeric(8,0),
    ftnnum_2 character varying(8),
    jkynum_2 character varying(7),
    k_ttl_ten_2 numeric(8,0),
    ftnnum_3 character varying(8),
    jkynum_3 character varying(7),
    k_ttl_ten_3 numeric(8,0),
    ftnnum_4 character varying(8),
    jkynum_4 character varying(7),
    k_ttl_ten_4 numeric(8,0),
    kakutei_kbn character(1),
    err_kbn character(2),
    seikyu_info character varying(40),
    u_err_code character(4),
    u_err_msg_1 character varying(400),
    u_err_msg_2 character varying(512),
    u_err_srykbn character(2),
    u_err_info character varying(400),
    u_ten_ng numeric(8,0),
    u_ten_ok numeric(8,0),
    u_henrei_flg character(1),
    u_yobi1 character varying(10),
    u_yobi2 character varying(10),
    end_flg numeric(1,0),
    termid character varying(64),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    kohid bigint DEFAULT 0
);

ALTER TABLE ONLY tbl_onrece_body
    ADD CONSTRAINT tbl_onrece_body_primary_key PRIMARY KEY (hospnum, teisyutusaki, sryym, ucnt, scnt, rennum, receipt_no);

CREATE INDEX idx_onrece_body_ptid ON tbl_onrece_body USING btree (hospnum, teisyutusaki, sryym, ptid);
