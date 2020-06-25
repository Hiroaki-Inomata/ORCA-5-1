--                            --
-- tbl_tensu_price            --
--                            --
-- Create Date : 2016/12/20   --
--                            --

CREATE TABLE tbl_tensu_price (
	hospnum		smallint NOT NULL,
	srycd		character varying(9) NOT NULL,
	yukostymd	character varying(8) NOT NULL,
	yukoedymd	character varying(8) NOT NULL,
	price	numeric(11,2) DEFAULT 0,
	termid		character varying(16),
	opid		character varying(16),
	creymd		character varying(8),
	upymd		character varying(8),
	uphms		character varying(6)
);

ALTER TABLE ONLY tbl_tensu_price
    ADD CONSTRAINT tbl_tensu_price_primary_key PRIMARY KEY (hospnum,srycd,yukostymd,yukoedymd);
