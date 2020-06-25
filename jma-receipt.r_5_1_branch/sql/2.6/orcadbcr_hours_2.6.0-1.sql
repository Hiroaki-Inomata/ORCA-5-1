--                                --
-- ORCA DataBase Table Definition --
--                                --
-- Äù¤á»þ´Ö´ÉÍý                   --
--                                --
-- Table Name  : TBL_HOURS        --
-- Create Date : 2005/07/13       --
--                                --

create table tbl_hours (
SRYYMD                          char(8) not null, 
TIME                            char(4),
ID                              char(12), 
HOURSKBN                        char(1), 
TERMID  			varchar(32),
OPID    			varchar(16),
CREYMD  			char(8),
UPYMD    			char(8),
UPHMS				char(6),
constraint tbl_hours_primary_key primary key (SRYYMD,TIME)
);
