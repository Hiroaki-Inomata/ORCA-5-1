\set ON_ERROR_STOP

CREATE TABLE "tbl_chk004" (
	"kanricd" character(4) NOT NULL,
	"kbncd" character(8) NOT NULL,
	"styukymd" character(8) NOT NULL,
	"edyukymd" character(8) NOT NULL,
	"kanritbl" character varying(500),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_chk004_primary_key" Primary Key ("kanricd", "kbncd", "styukymd", "edyukymd")
);

COMMENT ON TABLE "tbl_chk004" IS 'チェックマスタ４';

COPY "tbl_chk004" FROM stdin;
1102	000001  	20040401	99999999	        00010311			20060609	20060609	183108
1102	000002  	20040401	99999999	        00010321			20060609	20060609	183112
1102	000003  	20040401	99999999	        00010341			20060609	20060609	183116
1102	000004  	20040401	99999999	        00010601			20060609	20060609	172252
1102	000005  	20040401	99999999	        00010671			20060609	20060609	172258
\.

