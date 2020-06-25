CREATE TABLE tbl_sskijyo (
    syojyoucd character(7) NOT NULL,
    syojyou character varying(2048),
    sayokijyo character varying(2048),
    termid character varying(32),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_sskijyo
    ADD CONSTRAINT tbl_sskijyo_primary_key PRIMARY KEY (syojyoucd);

