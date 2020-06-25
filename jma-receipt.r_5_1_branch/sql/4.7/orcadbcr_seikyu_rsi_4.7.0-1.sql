\set ON_ERROR_STOP
--
-- ÀÁµá´ÉÍý(Ï«ºÒ¾ðÊó)
--
-- Create Date : 2011/09/01 --
--
create	table	tbl_seikyu_rsi	(
	hospnum	 	smallint	 DEFAULT 0 NOT NULL,
	sryym	 	integer		 DEFAULT 0 NOT NULL,
	nyugaikbn	character(1)	 NOT NULL,
	ptid	 	bigint		 DEFAULT 0 NOT NULL,
	receka	 	character(2)	 NOT NULL,
	teisyutusaki	smallint	 DEFAULT 0 NOT NULL,
	recesyubetu	character(4)	 NOT NULL,
	hknjanum	character(8)	 NOT NULL,
	hojokbn_key	character(1)	 NOT NULL,
	kohid_key	bigint		 DEFAULT 0 NOT NULL,
	tekstymd	character(8)	 NOT NULL,
	hknkbn		character(1),
	recekbn		character(1),
	syokaiflg	character(1),
	syocd		character(5),
	kofunum		character varying(14),
	saigaikbn	character(1),
	comment		character varying(200)
);

ALTER TABLE ONLY tbl_seikyu_rsi
    ADD CONSTRAINT tbl_seikyu_rsi_primary_key PRIMARY KEY (hospnum, sryym, nyugaikbn, ptid, receka, teisyutusaki, recesyubetu, hknjanum, hojokbn_key, kohid_key, tekstymd);
