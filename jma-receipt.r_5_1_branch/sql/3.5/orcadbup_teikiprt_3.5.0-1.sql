\set ON_ERROR_STOP

CREATE TABLE "tbl_teikiprtwk" (
	"btunum" character(2) NOT NULL,
	"denpnum" numeric(7,0) NOT NULL,
	"hospid" character(24),
	"ptid" numeric(10,0),
	"ptnum" character(20),
	"name" character(100),
	Constraint "tbl_teikiprtwk_primary_key" Primary Key ("btunum", "denpnum")
);

insert into tbl_teikiprtwk
select * from tbl_teikiprt ;

drop table tbl_teikiprt ;


CREATE TABLE "tbl_teikiprt" (
	"hospnum" numeric(2,0) NOT NULL,
	"btunum" character(2) NOT NULL,
	"denpnum" numeric(7,0) NOT NULL,
	"ptid" numeric(10,0),
	"ptnum" character(20),
	"name" varchar(100),
	Constraint "tbl_teikiprt_primary_key" Primary Key ("hospnum", "btunum", "denpnum")
	);

insert into tbl_teikiprt (
hospnum,
btunum,
denpnum,
ptid,
ptnum,
name)
select 
1,
btunum,
denpnum,
ptid,
ptnum,
name from tbl_teikiprtwk;

drop table tbl_teikiprtwk ;

