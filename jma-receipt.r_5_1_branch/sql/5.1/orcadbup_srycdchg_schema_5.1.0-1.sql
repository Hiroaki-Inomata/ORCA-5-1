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

ALTER TABLE ONLY tbl_mstkanri_org
    ADD CONSTRAINT tbl_mstkanri_org_primary_key PRIMARY KEY (hospnum, kanricd);

-- マスタ管理レコードを移動 --
INSERT INTO
    tbl_mstkanri_org
SELECT
    *
FROM
    tbl_mstkanri_public
WHERE
    kanricd = 'ORCADB05'
;

DELETE FROM
    tbl_mstkanri_public
WHERE
    kanricd = 'ORCADB05'
;

-- 現行テーブル名を変更する

ALTER TABLE IF EXISTS tbl_srycdchg DROP CONSTRAINT tbl_srycdchg_primary_key;
DROP INDEX idx_srycdchg_rjnsrycd;

ALTER TABLE IF EXISTS tbl_srycdchg RENAME TO tbl_srycdchg_org;
ALTER TABLE tbl_srycdchg_org
    ADD CONSTRAINT tbl_srycdchg_org_primary_key PRIMARY KEY (hospnum, ipnsrycd, rjnsrycd);
CREATE INDEX idx_srycdchg_org_rjnsrycd ON tbl_srycdchg_org USING btree (hospnum, rjnsrycd);

-- ユーザ用テーブル作成

CREATE TABLE IF NOT EXISTS tbl_srycdchg_user (
    ipnsrycd character varying(9) NOT NULL,
    rjnsrycd character varying(9) NOT NULL,
    termid character varying(16),
    opid character varying(16),
    creymd character varying(8),
    upymd character varying(8),
    uphms character varying(6),
    hospnum smallint NOT NULL
);

ALTER TABLE IF EXISTS ONLY tbl_srycdchg_user
    ADD CONSTRAINT tbl_srycdchg_user_primary_key PRIMARY KEY (hospnum, ipnsrycd, rjnsrycd);

CREATE INDEX idx_srycdchg_user_rjnsrycd ON tbl_srycdchg_user USING btree (hospnum, rjnsrycd);

-- マスター用テーブル作成

CREATE TABLE IF NOT EXISTS master.tbl_srycdchg_master (
    ipnsrycd character varying(9) NOT NULL,
    rjnsrycd character varying(9) NOT NULL,
    termid character varying(16),
    opid character varying(16),
    creymd character varying(8),
    upymd character varying(8),
    uphms character varying(6),
    hospnum smallint NOT NULL
);

ALTER TABLE IF EXISTS ONLY master.tbl_srycdchg_master
    ADD CONSTRAINT tbl_srycdchg_master_primary_key PRIMARY KEY (hospnum, ipnsrycd, rjnsrycd);

CREATE INDEX idx_srycdchg_master_rjnsrycd ON master.tbl_srycdchg_master USING btree (hospnum, rjnsrycd);

-- ビュー作成

DROP VIEW IF EXISTS
    tbl_srycdchg
;

DROP VIEW IF EXISTS
    view_srycdchg_master
;

CREATE VIEW
    view_srycdchg_master
AS
SELECT
    a.ipnsrycd,
    a.rjnsrycd,
    a.termid,
    a.opid,
    a.creymd,
    a.upymd,
    a.uphms,
    b.hospnum::smallint
FROM
    master.tbl_srycdchg_master a
    CROSS JOIN tbl_sysbase b
;

CREATE VIEW
    tbl_srycdchg
AS
(
SELECT
    *
FROM
    tbl_srycdchg_user
WHERE
    rjnsrycd != '999999999'
UNION ALL
SELECT
    *
FROM
    view_srycdchg_master
WHERE
    NOT EXISTS (
        SELECT
            1
        FROM
            tbl_srycdchg_user
        WHERE
            ( view_srycdchg_master.hospnum = tbl_srycdchg_user.hospnum ) AND
            ( view_srycdchg_master.ipnsrycd = tbl_srycdchg_user.ipnsrycd )
        )
ORDER BY
    hospnum,
    ipnsrycd,
    rjnsrycd
)
;
