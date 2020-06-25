CREATE TABLE tbl_etensu_2_off (
    hospnum smallint NOT NULL,
    h_group character varying(7) NOT NULL,
    srycd character varying(9) NOT NULL,
    yukostymd character varying(8) NOT NULL,
    yukoedymd character varying(8) NOT NULL,
    termid character varying(16),
    opid character varying(16),
    creymd character varying(8),
    upymd character varying(8),
    uphms character varying(6)
);

ALTER TABLE ONLY tbl_etensu_2_off
    ADD CONSTRAINT tbl_etensu_2_off_primary_key PRIMARY KEY (hospnum, h_group, srycd, yukostymd, yukoedymd);

