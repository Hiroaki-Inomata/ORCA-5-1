-- tbl_orderprt --

\set ON_ERROR_STOP
begin;
ALTER TABLE tbl_orderprt DROP CONSTRAINT tbl_orderprt_primary_key;
ALTER TABLE tbl_orderprt RENAME TO wrk_orderprt;
CREATE TABLE tbl_orderprt (
ptid bigint DEFAULT 0 NOT NULL,
ptnum character(20) NOT NULL,
nyugaikbn character(1) NOT NULL,
sryka character(2) NOT NULL,
hkncombi character(4) NOT NULL,
sysymd character(8) NOT NULL,
printymd character(8) NOT NULL,
printhms character(6) NOT NULL,
prtkbn character(1) NOT NULL,
ordernum smallint DEFAULT 0 NOT NULL,
rennum smallint DEFAULT 0 NOT NULL,
drcd character(5),
srykbn character(2),
srysyukbn character(3),
sryymd character(8),
day1 smallint DEFAULT 0,
day2 smallint DEFAULT 0,
day3 smallint DEFAULT 0,
day4 smallint DEFAULT 0,
day5 smallint DEFAULT 0,
day6 smallint DEFAULT 0,
day7 smallint DEFAULT 0,
day8 smallint DEFAULT 0,
day9 smallint DEFAULT 0,
day10 smallint DEFAULT 0,
day11 smallint DEFAULT 0,
day12 smallint DEFAULT 0,
day13 smallint DEFAULT 0,
day14 smallint DEFAULT 0,
day15 smallint DEFAULT 0,
day16 smallint DEFAULT 0,
day17 smallint DEFAULT 0,
day18 smallint DEFAULT 0,
day19 smallint DEFAULT 0,
day20 smallint DEFAULT 0,
day21 smallint DEFAULT 0,
day22 smallint DEFAULT 0,
day23 smallint DEFAULT 0,
day24 smallint DEFAULT 0,
day25 smallint DEFAULT 0,
day26 smallint DEFAULT 0,
day27 smallint DEFAULT 0,
day28 smallint DEFAULT 0,
day29 smallint DEFAULT 0,
day30 smallint DEFAULT 0,
day31 smallint DEFAULT 0,
srycd character(9),
srysuryo numeric(10,5) DEFAULT 0,
srysuryo2 numeric(10,5) DEFAULT 0,
srykaisu smallint DEFAULT 0,
inputcoment character varying(80),
termid character varying(64),
opid character varying(16),
creymd character(8),
upymd character(8),
uphms character(6),
hospnum smallint NOT NULL
);
INSERT INTO tbl_orderprt SELECT * FROM wrk_orderprt;
ALTER TABLE ONLY tbl_orderprt
ADD CONSTRAINT tbl_orderprt_primary_key PRIMARY KEY (hospnum, ptid, ptnum, nyugaikbn, sryka, hkncombi, sysymd, printymd, printhms, prtkbn, ordernum, rennum);
DROP TABLE wrk_orderprt;
commit;
