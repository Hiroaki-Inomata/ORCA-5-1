\set ON_ERROR_STOP

drop table tbl_order ;

CREATE TABLE "tbl_order" (
	"hospid" character(24) NOT NULL,
	"karte_key" character varying(36) NOT NULL,
	"ordernum" numeric(2,0) NOT NULL,
	"nyugaikbn" character(1),
	"ptnum" character(20),
	"ptid" numeric(10,0),
	"sryka" character(2),
	"sryymd" character(8),
	"hkncombi" numeric(4,0),
	"orderid" character(4),
	"ukeymd" character(8),
	"ukehms" character(6),
	"errcd1" character(4),
	"errcd2" character(4),
	"status" numeric(1,0),
	"modify" numeric(1,0),
	"keygenerate" numeric(1,0),
	"rrknum" numeric(3,0),
	"rrkedanum" numeric(3,0),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_order_primary_key" Primary Key ("hospid","karte_key","ordernum")
);

CREATE INDEX idx_order_ukeymd ON tbl_order USING btree (ukeymd);

CREATE INDEX idx_order_sryymd ON tbl_order USING btree (sryymd);

CREATE INDEX idx_order_ptnum ON tbl_order USING btree (ptnum);

COMMENT ON TABLE "tbl_order" IS '入院オーダ';

