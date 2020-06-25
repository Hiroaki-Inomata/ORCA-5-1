-- マスター系のスキーマ作成

CREATE SCHEMA IF NOT EXISTS
    master
;
-- 点数テーブル(TBL_TENSU)

-- ユーザーによりメンテナンスするレコードを分離し別テーブルを作成する
-- 診療行為コードの先頭が「０」且つ先頭３桁が「０９９」でない

CREATE TABLE
    tbl_tensu_user
AS
SELECT
    *
FROM
    tbl_tensu
WHERE
    SUBSTR(srycd,1,1) = '0' AND
    SUBSTR(srycd,1,3) <> '099'
;

DELETE FROM
    tbl_tensu
WHERE
    SUBSTR(srycd,1,1) = '0' AND
    SUBSTR(srycd,1,3) <> '099'
;

ALTER TABLE ONLY tbl_tensu_user
    ADD CONSTRAINT tbl_tensu_user_primary_key PRIMARY KEY (hospnum, srycd, yukostymd, yukoedymd);

CREATE INDEX idx_tensu_user_formalname ON tbl_tensu_user USING btree (formalname);
CREATE INDEX idx_tensu_user_kananame ON tbl_tensu_user USING btree (kananame);
CREATE INDEX idx_tensu_user_kensagrp ON tbl_tensu_user USING btree (hospnum, srykbn, knsjisgrpkbn);
CREATE INDEX idx_tensu_user_name ON tbl_tensu_user USING btree (name);
CREATE INDEX idx_tensu_user_yakkakjncd ON tbl_tensu_user USING btree (yakkakjncd);

-- 点数マスタをスキーマmasterへ移動する

ALTER TABLE IF EXISTS tbl_tensu SET SCHEMA master;

-- テーブル名を変更する

ALTER TABLE IF EXISTS master.tbl_tensu RENAME TO tbl_tensu_master;

-- プライマリキー、インデックスの再作成

ALTER TABLE master.tbl_tensu_master DROP CONSTRAINT tbl_tensu_primary_key;
ALTER TABLE master.tbl_tensu_master
    ADD CONSTRAINT tbl_tensu_master_primary_key PRIMARY KEY (hospnum, srycd, yukostymd, yukoedymd);

DROP INDEX master.idx_tensu_formalname;
CREATE INDEX idx_tensu_master_formalname ON master.tbl_tensu_master USING btree (formalname);
DROP INDEX master.idx_tensu_kananame;
CREATE INDEX idx_tensu_master_kananame ON master.tbl_tensu_master USING btree (kananame);
DROP INDEX master.idx_tensu_kensagrp;
CREATE INDEX idx_tensu_master_kensagrp ON master.tbl_tensu_master USING btree (hospnum, srykbn, knsjisgrpkbn);
DROP INDEX master.idx_tensu_name;
CREATE INDEX idx_tensu_master_name ON master.tbl_tensu_master USING btree (name);
DROP INDEX master.idx_tensu_yakkakjncd;
CREATE INDEX idx_tensu_master_yakkakjncd ON master.tbl_tensu_master USING btree (yakkakjncd);

--DROP VIEW IF EXISTS
--    public.tbl_tensu
--;

--CREATE VIEW
--    tbl_tensu
--AS
--(
--SELECT
--    tbl_tensu_user.*,
--    '2'::smallint AS master_class
--FROM
--    tbl_tensu_user
--UNION ALL
--SELECT
--    tbl_tensu_master.*,
--    '1'::smallint AS master_class
--FROM
--    master.tbl_tensu_master
--WHERE
--    NOT EXISTS (
--        SELECT
--            1
--        FROM
--            tbl_tensu_user
--        WHERE
--            master.tbl_tensu_master.srycd = tbl_tensu_user.srycd
--        )
--ORDER BY
--    srycd,
--    yukostymd,
--    yukoedymd
--)
--;
