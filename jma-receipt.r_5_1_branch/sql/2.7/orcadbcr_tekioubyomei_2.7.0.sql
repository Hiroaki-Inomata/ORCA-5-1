--                                --
-- ORCA DataBase Table Definition --
--                                --
-- Table Name  : 適応病名マスタ   --
--               TBL_TEKIOUBYOMEI --
-- Create Date : 2005/12/19       --

CREATE TABLE "tbl_tekioubyomei" (
	"chkkbn" character(1) NOT NULL,
	"srycd"  character(9) NOT NULL,
	"rennum" numeric(4,0) NOT NULL,
	"byomei" character varying(400),
	"byomeicdsu" numeric(2,0) DEFAULT 0,
	"byomeicd_1" character(7),
	"byomeicd_2" character(7),
	"byomeicd_3" character(7),
	"byomeicd_4" character(7),
	"byomeicd_5" character(7),
	"byomeicd_6" character(7),
	"byomeicd_7" character(7),
	"byomeicd_8" character(7),
	"byomeicd_9" character(7),
	"byomeicd_10" character(7),
	"byomeicd_11" character(7),
	"byomeicd_12" character(7),
	"byomeicd_13" character(7),
	"byomeicd_14" character(7),
	"byomeicd_15" character(7),
	"byomeicd_16" character(7),
	"byomeicd_17" character(7),
	"byomeicd_18" character(7),
	"byomeicd_19" character(7),
	"byomeicd_20" character(7),
	"byomeicd_21" character(7),
	"byomeihenflg" character(1),
	"khnbyomeicd" character(7),
	"khnbuicd_1" character(7),
	"khnbuicd_2" character(7),
	"khnbuicd_3" character(7),
	"termid" character varying(32),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_tekioubyomei_primary_key" Primary Key ("chkkbn","srycd", "rennum")
);

drop table tbl_medyakko;
