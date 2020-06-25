--                                    --
-- JOB´ÉÍý¥Þ¥¹¥¿                      --
--    °ì³ç¸ÄÊÌ¶èÊ¬ (SYOKBN)           --
--    ¿ÇÎÅÇ¯·î     (SRYYM)            --
--    Æþ³°¶èÊ¬     (NYUGAIKBN)        --
--    ¤òÄÉ²Ã                          --
--                                    --
-- Create Date : 2003/08/29           --
--                                    --

-- TBL_JOBKANRIWK  ºîÀ®              --

create table TBL_JOBKANRIWK (
JOBID 			numeric(7,0) DEFAULT 0 NOT NULL,
SHELLID 		character(8) NOT NULL,
RUN_OPID		character(16) NOT NULL,
SHELLMSG 		character(100),
TERMID 			character varying(32),
OPID 			character varying(16),
PGID 			character(20),
STARTYMD 		character(8),
ENDYMD 			character(8),
JOBSTARTTIME 		character(8),
JOBENDTIME 		character(8),
STEPSTARTTIME 		character(8),
STEPENDTIME 		character(8),
PARAM 			character(256),
ERRCD 			character(4),
SRYYMD 			character(8),
PTID 			numeric(10,0),
UPDCNT 			numeric(10,0),
STEPCNT 		numeric(3,0) DEFAULT 0,
STOPFLG 		numeric(1,0) DEFAULT 0,
PID1	 		numeric(5,0) DEFAULT 0,
PID2	 		numeric(5,0) DEFAULT 0,
PID3	 		numeric(5,0) DEFAULT 0,
PID4	 		numeric(5,0) DEFAULT 0,
YOBI 			character varying(500),
Constraint TBL_JOBKANRIWK_primary_key Primary Key (JOBID, SHELLID, RUN_OPID));
-- TBL_JOBKANRI ¢ª TBL_JOBKANRIWK   --

insert into TBL_JOBKANRIWK
select * from TBL_JOBKANRI;

-- TBL_JOBKANRI ºï½ü               --

drop table TBL_JOBKANRI;

-- TBL_JOBKANRI ºîÀ®               --

create table TBL_JOBKANRI (
JOBID 			numeric(7,0) DEFAULT 0 NOT NULL,
SHELLID 		character(8) NOT NULL,
RUN_OPID		character(16) NOT NULL,
SHELLMSG 		character(100),
TERMID 			character varying(32),
OPID 			character varying(16),
PGID 			character(20),
STARTYMD 		character(8),
ENDYMD 			character(8),
JOBSTARTTIME 		character(8),
JOBENDTIME 		character(8),
STEPSTARTTIME 		character(8),
STEPENDTIME 		character(8),
PARAM 			character(256),
ERRCD 			character(4),
SYOKBN 			character(1),
SRYYM 			character(6),
NYUGAIKBN		character(1),
SRYYMD 			character(8),
PTID 			numeric(10,0),
UPDCNT 			numeric(10,0),
STEPCNT 		numeric(3,0) DEFAULT 0,
STOPFLG 		numeric(1,0) DEFAULT 0,
PID1	 		numeric(5,0) DEFAULT 0,
PID2	 		numeric(5,0) DEFAULT 0,
PID3	 		numeric(5,0) DEFAULT 0,
PID4	 		numeric(5,0) DEFAULT 0,
YOBI 			character varying(500),
Constraint TBL_JOBKANRI_primary_key Primary Key (JOBID, SHELLID, RUN_OPID));

-- TBL_JOBKANRIWK ¢ª TBL_JOBKANRI   --

insert into TBL_JOBKANRI
(JOBID,
SHELLID,
RUN_OPID,
SHELLMSG,
TERMID,
OPID,
PGID,
STARTYMD,
ENDYMD,
JOBSTARTTIME,
JOBENDTIME,
STEPSTARTTIME,
STEPENDTIME,
PARAM,
ERRCD,
SYOKBN,
SRYYM,
NYUGAIKBN,
SRYYMD,
PTID,
UPDCNT,
STEPCNT,
STOPFLG,
PID1,
PID2,
PID3,
PID4,
YOBI
)
select
JOBID,
SHELLID,
RUN_OPID,
SHELLMSG,
TERMID,
OPID,
PGID,
STARTYMD,
ENDYMD,
JOBSTARTTIME,
JOBENDTIME,
STEPSTARTTIME,
STEPENDTIME,
PARAM,
ERRCD,
' ',
'      ',
' ',
SRYYMD,
PTID,
UPDCNT,
STEPCNT,
STOPFLG,
PID1,
PID2,
PID3,
PID4,
YOBI
from TBL_JOBKANRIWK;


-- TBL_JOBKANRIWK ºï½ü             --

drop table TBL_JOBKANRIWK;
