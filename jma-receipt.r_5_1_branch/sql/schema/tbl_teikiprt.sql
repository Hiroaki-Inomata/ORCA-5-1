CREATE TABLE tbl_teikiprt (
    hospnum numeric(2,0) NOT NULL,
    btunum character(2) NOT NULL,
    denpnum numeric(7,0) NOT NULL,
    ptid numeric(10,0),
    ptnum character(20),
    name character varying(100)
);

ALTER TABLE ONLY tbl_teikiprt
    ADD CONSTRAINT tbl_teikiprt_primary_key PRIMARY KEY (hospnum, btunum, denpnum);

