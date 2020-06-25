\set ON_ERROR_STOP
--
-- 照会用受診履歴テーブル
-- Create Date : 2010/04/09        --
--
create  table  tbl_rrksrh (
        hospnum      numeric(2,0),
        termid       char(64),
        ptid         numeric(10,0),
        nyugaikbn    char(1),
        sryka        char(2),
        hkncombinum  smallint,
        sryymd       char(8),
        creymd       char(8),
        upymd        char(8),
        uphms        char(6)
);

