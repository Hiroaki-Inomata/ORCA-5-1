--                                --
-- ORCA DataBase Table Definition --
--                                --
-- 入院オーダー情報       --
--                                --
-- Table Name  : TBL_ORDER        --
-- Create Date : 2005/03/15       --
--                                --

create table TBL_ORDER (
	HOSPID			 	char(24) 		not null,
	KARTE_KEY			varchar(36) 	not null,
	NYUGAIKBN			char(1) 		not null,
	PTNUM				char(20) 		not null,
	PTID				numeric(10,0) 	not null,
	SRYKA				char(2) 		not null,
	SRYYMD				char(8) 		not null,
	HKNCOMBI			numeric(4,0) 	not null,
	DRCD				char(5)			not null,
	ORDERID				char(4)			not null,
	ORDERNUM			numeric(2,0) 	not null,
	ITEMNUM				numeric(2,0) 	not null,
	UKEYMD   			char(8) 		not null,
	UKEHMS   			char(6) 		not null,
	ERRCD1				char(4),
	ERRCD2				char(4),
	ITEMID				char(5),
	ITEM				varchar(10),
	REMARKS				varchar(80),
	ERRCD_ORDER			char(4),
	ERRCD_ITEM			char(4),
	STATUS_ORDER		char(1),
	STATUS_ITEM			char(1),
	MODIFY				char(1),
	KEYGENERATE			char(1),
	RRKNUM				numeric(3,0),
	RRKEDANUM			numeric(3,0),
	TERMID   			varchar(16),
	OPID     			varchar(16),
	CREYMD   			char(8),
	UPYMD    			char(8),
	UPHMS				char(6),
constraint TBL_ORDER_primary_key primary key (HOSPID,KARTE_KEY,NYUGAIKBN,PTNUM,PTID,SRYKA,SRYYMD,HKNCOMBI,
DRCD,ORDERID,ORDERNUM,ITEMNUM,UKEYMD,UKEHMS));

create index idx_order_ukeymd ON tbl_order USING btree (ukeymd);
create index idx_order_sryymd ON tbl_order USING btree (sryymd);
create index idx_order_ptid   ON tbl_order USING btree (ptid);
