--                                --
-- ORCA DataBase Table Definition --
--                                --
-- バッチ引渡しパラメタ情報       --
--                                --
-- Table Name  : TBL_BTPARA       --
-- Create Date : 2004/10/14       --
--                                --

create table TBL_BTPARA (
SHELLID 		char(8) NOT NULL,
RUN_OPID		char(16) NOT NULL,
DSP_RENNUM		numeric(3,0) DEFAULT 0 NOT NULL,
RENNUM			numeric(4,0) DEFAULT 0 NOT NULL,
JOBID			numeric(3,0),
SCRIPTID		char(20),
INFO_KBN		char(02),
INFO_PARA		varchar(50),
TERMID			varchar(16),
OPID			varchar(16),
CREYMD			char(8),
UPYMD			char(8),
UPHMS			char(6),
constraint TBL_BTPARA_primary_key primary key (SHELLID,RUN_OPID,
DSP_RENNUM,RENNUM));
