--                            --
-- tbl_access_key             --
--                            --
-- Create Date : 2016/12/19   --
--                            --

CREATE TABLE tbl_access_key (
	hospnum		smallint NOT NULL,
	access_key_1	character varying(128),
	creymd		character(8),
	uphms		character(6)
);

ALTER TABLE ONLY tbl_access_key
    ADD CONSTRAINT tbl_access_key_primary_key PRIMARY KEY (hospnum);
