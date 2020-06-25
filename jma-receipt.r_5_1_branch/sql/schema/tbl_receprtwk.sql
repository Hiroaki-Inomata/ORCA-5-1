CREATE TABLE "tbl_receprtwk" (
	"prtid" character(4) NOT NULL,
	"receym" character(6) NOT NULL,
	"creymd" character(8) NOT NULL,
	"crehms" character(6) NOT NULL,
	"prefkbn" character(1) NOT NULL,
	"prefnum" character(2) NOT NULL,
	"hknjanum" character(8) NOT NULL,
	"tekstymd" character(8) NOT NULL,
	"teisyutusaki" character(1) NOT NULL,
	"recesyubetu" character(4) NOT NULL,
	"ptnum" character(20) NOT NULL,
	"sryym" character(6) DEFAULT '000000' NOT NULL,
	"rennum" numeric(2,0) DEFAULT 0 NOT NULL,
	"recenum" numeric(6,0) DEFAULT 0 NOT NULL,
	"hknkbn" character(1),
	"kananame" character varying(100),
	"termid" character varying(32),
	"prtjyun" numeric(2,0),
	"prtrennum" numeric(6,0),
	"prtdata" character varying(10000),
	Constraint "tbl_receprtwk_primary_key" Primary Key ("prtid", "receym", "creymd", "crehms", "prefkbn", "prefnum", "hknjanum", "tekstymd", "teisyutusaki", "recesyubetu", "ptnum", "sryym", "rennum", "recenum")
);

