\set ON_ERROR_STOP

create  table  tbl_ptsrh (
        hospnum      numeric(2,0),
        termid       char(64),
        ptid         numeric(10,0),
        ptnum        varchar(20),
        kananame     varchar(100),
        sex          char(1),
        birthday     char(8),
        jkysnum      varchar(20),
        datakbn      char(1),
        creymd       char(8),
        upymd        char(8),
        uphms        char(6)
);

