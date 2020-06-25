--                                    --
-- 請求管理テーブルの変更             --
--                                    --
-- Create Date : 2008/05/29           --
--                                    --
\set ON_ERROR_STOP

CREATE INDEX idx_seikyu_skyym on tbl_seikyu using btree (hospnum,skyym,skykbn);

