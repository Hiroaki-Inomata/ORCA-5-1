-- 入院オーダー情報       --
--                                --
-- Table Name  : TBL_ORDER        --
-- Create Date : 2005/03/29       --
--                                --

drop index idx_order_ptid ;
create index idx_order_ptnum  ON tbl_order USING btree (ptnum);
