\set ON_ERROR_STOP

--				--
-- �ޥ��������ơ��֥�κ���	--
--				--
-- Create Date : 2007/01/26	--
--				--

-- TBL_MSTKANRI ����		--

CREATE TABLE tbl_mstkanri (
	hospnum		numeric(2,0) NOT NULL,
	kanricd		character(8) NOT NULL,
	dbrversion1	character varying(64),
	dbrversion2	character varying(64),
	termid		character varying(32),
	opid		character varying(16),
	creymd		character(8),
	upymd		character(8),
	uphms		character(6)
);

ALTER TABLE ONLY tbl_mstkanri
    ADD CONSTRAINT tbl_mstkanri_primary_key PRIMARY KEY (hospnum, kanricd);
