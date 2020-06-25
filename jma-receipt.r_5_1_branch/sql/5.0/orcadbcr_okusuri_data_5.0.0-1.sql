DROP TABLE IF EXISTS tbl_okusuri_data;

CREATE TABLE tbl_okusuri_data (
    data_key bigint DEFAULT 0 NOT NULL,
    rp smallint DEFAULT 0 NOT NULL,
    rp_seq smallint DEFAULT 0 NOT NULL,
    data_class smallint DEFAULT 0 NOT NULL,
    name character varying(200),
    suryo numeric(10,5) DEFAULT 0 NOT NULL,
    taniname character varying(20),
    code_class smallint DEFAULT 0 NOT NULL,
    m_code character varying(20),
    termid  character varying(16),
    opid    character varying(16),
    creymd character varying(8),
    upymd character varying(8),
    uphms character varying(6)
);

ALTER TABLE ONLY tbl_okusuri_data
    ADD CONSTRAINT tbl_okusuri_data_primary_key PRIMARY KEY (data_key,rp,rp_seq);
