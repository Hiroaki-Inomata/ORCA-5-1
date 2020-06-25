--                                    --
-- オンラインレセ電明細データ更新     --
--                                    --
-- Create Date : 2008/11/17           --
--                                    --

\set ON_ERROR_STOP

CREATE INDEX idx_onrece_body_ptid ON tbl_onrece_body USING btree (hospnum,teisyutusaki, sryym,ptid);
