CREATE TABLE tbl_sysuser (
    userid character varying(16) NOT NULL,
    hospnum numeric(2,0) NOT NULL,
    supervisor character(1),
    upymd character(8)
);

ALTER TABLE ONLY tbl_sysuser
    ADD CONSTRAINT tbl_sysuser_primary_key PRIMARY KEY (userid);

