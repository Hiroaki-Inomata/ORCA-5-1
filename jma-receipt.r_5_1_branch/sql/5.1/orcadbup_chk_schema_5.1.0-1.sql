\set ON_ERROR_STOP

SET client_encoding = 'EUC_JP';

-- マスタ管理（一時）テーブルを作成する
CREATE TABLE IF NOT EXISTS tbl_mstkanri_org (
    hospnum numeric(2,0) NOT NULL,
    kanricd character(8) NOT NULL,
    dbrversion1 character varying(64),
    dbrversion2 character varying(64),
    termid character varying(32),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    name character varying(64)
);

--ALTER TABLE ONLY tbl_mstkanri_org
--    ADD CONSTRAINT tbl_mstkanri_org_primary_key PRIMARY KEY (hospnum, kanricd);

-- マスタ管理レコードを移動 --
INSERT INTO
    tbl_mstkanri_org
SELECT
    *
FROM
    tbl_mstkanri_public
WHERE
    kanricd = 'ORCADB04'
;

DELETE FROM
    tbl_mstkanri_public
WHERE
    kanricd = 'ORCADB04'
;

-- 現行テーブル名を変更する

ALTER TABLE IF EXISTS tbl_chk DROP CONSTRAINT tbl_chk_primary_key;
ALTER TABLE IF EXISTS tbl_chk RENAME TO tbl_chk_org;
ALTER TABLE tbl_chk_org
    ADD CONSTRAINT tbl_chk_org_primary_key PRIMARY KEY (hospnum, chkkbn, srycd, grpnum, yukostymd, yukoedymd, cdkbn, cd);

-- ユーザ用テーブル作成

CREATE TABLE IF NOT EXISTS tbl_chk_user (
    hospnum smallint NOT NULL,
    chkkbn character varying(1) NOT NULL,
    srycd character varying(9) NOT NULL,
    grpnum smallint NOT NULL,
    yukostymd character varying(8) NOT NULL,
    yukoedymd character varying(8) NOT NULL,
    cdkbn character varying(1) NOT NULL,
    cd character varying(9) NOT NULL,
    nyugaikbn character varying(1),
    nyugaibndkbn character varying(1),
    syorikbn character varying(1),
    chkerr smallint,
    datakbn character varying(1),
    rennum smallint,
    termid character varying(16),
    opid character varying(16),
    creymd character varying(8),
    upymd character varying(8),
    uphms character varying(6)
);

ALTER TABLE IF EXISTS ONLY tbl_chk_user
    ADD CONSTRAINT tbl_chk_user_primary_key PRIMARY KEY (hospnum, chkkbn, srycd, grpnum, yukostymd, yukoedymd, cdkbn, cd);

-- マスター用テーブル作成

CREATE TABLE IF NOT EXISTS master.tbl_chk_master (
    hospnum smallint NOT NULL,
    chkkbn character varying(1) NOT NULL,
    srycd character varying(9) NOT NULL,
    grpnum smallint NOT NULL,
    yukostymd character varying(8) NOT NULL,
    yukoedymd character varying(8) NOT NULL,
    cdkbn character varying(1) NOT NULL,
    cd character varying(9) NOT NULL,
    nyugaikbn character varying(1),
    nyugaibndkbn character varying(1),
    syorikbn character varying(1),
    chkerr smallint,
    datakbn character varying(1),
    rennum smallint,
    termid character varying(16),
    opid character varying(16),
    creymd character varying(8),
    upymd character varying(8),
    uphms character varying(6)
);

ALTER TABLE IF EXISTS ONLY master.tbl_chk_master
    ADD CONSTRAINT tbl_chk_master_primary_key PRIMARY KEY (hospnum, chkkbn, srycd, grpnum, yukostymd, yukoedymd, cdkbn, cd);

-- ビュー作成

DROP VIEW IF EXISTS
    tbl_chk
;

DROP TABLE IF EXISTS
    view_chk_master
;

CREATE VIEW
    view_chk_master
AS
SELECT
    b.hospnum::smallint,
    a.chkkbn,
    a.srycd,
    a.grpnum,
    a.yukostymd,
    a.yukoedymd,
    a.cdkbn,
    a.cd,
    a.nyugaikbn,
    a.nyugaibndkbn,
    a.syorikbn,
    a.chkerr,
    a.datakbn,
    a.rennum,
    a.termid,
    a.opid,
    a.creymd,
    a.upymd,
    a.uphms
FROM
    master.tbl_chk_master a
    CROSS JOIN tbl_sysbase b
;

--CREATE VIEW
--    tbl_chk
--AS
--(
--SELECT
--    *
--FROM
--    tbl_chk_user
--UNION ALL
--SELECT
--    *
--FROM
--    view_chk_master
--WHERE
--    NOT EXISTS (
--        SELECT
--            1
--        FROM
--            tbl_chk_user
--        WHERE
--            ( view_chk_master.hospnum = tbl_chk_user.hospnum ) AND
--            ( view_chk_master.chkkbn = tbl_chk_user.chkkbn ) AND
--            ( view_chk_master.srycd = tbl_chk_user.srycd ) AND
--            ( view_chk_master.grpnum = tbl_chk_user.grpnum ) AND
--            ( view_chk_master.yukostymd = tbl_chk_user.yukostymd ) AND
--            ( view_chk_master.yukoedymd = tbl_chk_user.yukoedymd ) AND
--            ( view_chk_master.cdkbn = tbl_chk_user.cdkbn ) AND
--            ( view_chk_master.cd = tbl_chk_user.cd )
--    )
-- ORDER BY
--);
