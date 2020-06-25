--                            --
-- tbl_ptmynumber             --
--                            --
-- Create Date : 2016/12/02   --
--                            --

CREATE TABLE tbl_ptmynumber (
	hospnum		smallint NOT NULL,
	ptid		bigint,
	id_key		character varying(10),
	my_number	character varying(20),
	description	character varying(100),
	termid		character varying(16),
	opid		character varying(16),
	creymd		character(8),
	upymd		character(8),
	uphms		character(6)
);

ALTER TABLE ONLY tbl_ptmynumber
    ADD CONSTRAINT tbl_ptmynumber_primary_key PRIMARY KEY (hospnum,ptid,id_key);
