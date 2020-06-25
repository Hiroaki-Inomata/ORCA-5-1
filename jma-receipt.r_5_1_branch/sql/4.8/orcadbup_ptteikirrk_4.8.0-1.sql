\set ON_ERROR_STOP

-- tbl_ptteikirrk --
alter table tbl_ptteikirrk alter column ptid             type bigint;
alter table tbl_ptteikirrk alter column hkncombinum      type smallint;
alter table tbl_ptteikirrk alter column denpnum          type integer;
alter table tbl_ptteikirrk alter column delflg           type smallint;
alter table tbl_ptteikirrk alter column skyten           type integer;
alter table tbl_ptteikirrk alter column skymoney_tax_sai type integer;
alter table tbl_ptteikirrk alter column skymoney         type integer;
alter table tbl_ptteikirrk alter column rrknum           type smallint;
alter table tbl_ptteikirrk alter column denpedanum       type smallint;
alter table tbl_ptteikirrk alter column skyupdflg        type smallint;
alter table tbl_ptteikirrk alter column hospnum          type smallint;

