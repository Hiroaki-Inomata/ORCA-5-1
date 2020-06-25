CREATE TABLE "tbl_chk005" (
	"chkkbn" character(1) NOT NULL,
	"id"  integer NOT NULL,
	"yukostymd" character(8) NOT NULL,
	"yukoedymd" character(8) NOT NULL,
	"cdkbn" character(1) NOT NULL,
	"rennum" integer NOT NULL,
	"srycd" character(9) NOT NULL,
	"cd" character varying(170),
	"byomei" character(160),
	"compkbn" character(1),
	"byomeihenflg" character(1),
	"nyugaikbn" character(1),
	"nyugaibndkbn" character(1),
	"daymonthkbn" character(1),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_chk005_primary_key" Primary Key ("chkkbn", "id",  "rennum")
);
	create index idx_chk005_rennum on tbl_chk005  ("chkkbn", "rennum") ;

COMMENT ON TABLE "tbl_chk005" IS 'チェック5';

