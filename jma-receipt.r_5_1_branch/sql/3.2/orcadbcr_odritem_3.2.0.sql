\set ON_ERROR_STOP

CREATE TABLE "tbl_odritem" (
	"hospid" character(24) NOT NULL,
	"karte_key" character varying(36) NOT NULL,
	"ordernum" numeric(2,0) NOT NULL,
	"itemnum" numeric(2,0) NOT NULL,
	"itemid" character(5),
	"item" character varying(10),
	"memo" character varying(80),
	"errcd" character(4),
	"status" numeric(1,0),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_odritem_primary_key" Primary Key ("hospid", "karte_key", "ordernum", "itemnum")
);

COMMENT ON TABLE "tbl_odritem" IS '入院オーダ項目';

