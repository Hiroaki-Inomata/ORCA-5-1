\set ON_ERROR_STOP
--
-- 照会用診療行為テーブル
-- Create Date : 2010/04/09        --
--
create  table  tbl_srysrh (
        hospnum      numeric(2,0),
        termid       char(64),
        ptid         numeric(10,0),
        nyugaikbn    char(1),
        sryka        char(2),
        srykbn       char(2),
        srysyukbn    char(3),
        sryymd       char(8),
        srycd1       char(9),
        srycd2       char(9),
        srycd3       char(9),
        srycd4       char(9),
        srycd5       char(9),
        creymd       char(8),
        upymd        char(8),
        uphms        char(6)
);

