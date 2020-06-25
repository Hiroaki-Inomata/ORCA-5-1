\set ON_ERROR_STOP
--
-- 診療情報システム提供用
--
-- Create Date : 2018/2/16
--

CREATE TABLE tbl_file_mis (
    hospnum smallint DEFAULT 0 NOT NULL,
    tbl_key character varying(8) NOT NULL,
    rennum smallint DEFAULT 0 NOT NULL,
    tbl_group character varying(14) NOT NULL,
    shori_rennum smallint DEFAULT 0 NOT NULL,
    shellid character varying(20),
    sryym character varying(6),
    sryymd character varying(8),
    cnt integer DEFAULT 0,
    nyugaikbn character varying(1),
    to_folder character varying(200),
    to_data character varying(200),
    code_type character varying(1),
    putflg character varying(1),
    title character varying(100),
    tbl_uuid character varying(36),
    creymd character varying(8),
    upymd character varying(8),
    uphms character varying(6)
);
ALTER TABLE ONLY tbl_file_mis
    ADD CONSTRAINT tbl_file_mis_primary_key PRIMARY KEY (hospnum, tbl_key, rennum, tbl_group, shori_rennum);

CREATE INDEX idx_file_mis_tbl_uuid ON tbl_file_mis USING btree (tbl_uuid);
CREATE INDEX idx_file_mis_sryym ON tbl_file_mis USING btree (sryym);
CREATE INDEX idx_file_mis_sryymd ON tbl_file_mis USING btree (sryymd);
