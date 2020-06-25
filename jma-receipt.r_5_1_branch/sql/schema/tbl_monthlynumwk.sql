CREATE TABLE "tbl_monthlynumwk" (
	"hospid" character(24) NOT NULL,
	"ptid" numeric(10,0) NOT NULL,
	"kohnum" character(3) NOT NULL,
	"sryym" character(6) NOT NULL,
	"jkysnum" character(20),
	"termid" character varying(32),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_monthlynumwk_primary_key" Primary Key ("hospid", "ptid", "kohnum", "sryym")
);

