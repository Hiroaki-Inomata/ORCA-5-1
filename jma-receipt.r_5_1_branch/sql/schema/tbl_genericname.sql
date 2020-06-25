CREATE TABLE tbl_genericname (
    yakkakjncd character(9) NOT NULL,
    yukoketa numeric(2,0),
    genericname character varying(200),
    tanicd character(3),
    taniname character varying(24),
    kanzanchi numeric(10,5),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_genericname
    ADD CONSTRAINT tbl_genericname_primary_key PRIMARY KEY (yakkakjncd);

