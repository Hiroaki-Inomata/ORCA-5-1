---------------------
-- Äê´üÀÁµá´Ø·¸¥Æ¡¼¥Ö¥ë¹àÌÜÂ°À­ÊÑ¹¹
-- Äê´üÀÁµá°õºş¾ğÊó
---------------------

\set ON_ERROR_STOP


-- tbl_teikiprt --
alter table tbl_teikiprt alter column hospnum type smallint;
alter table tbl_teikiprt alter column denpnum type integer;
alter table tbl_teikiprt alter column ptid    type bigint;
