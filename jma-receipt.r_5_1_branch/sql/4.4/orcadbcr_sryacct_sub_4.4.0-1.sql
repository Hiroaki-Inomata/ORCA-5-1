\set ON_ERROR_STOP

CREATE temp TABLE tbl_sryacct_sub_temp (
    hospnum,
    ptid ,
    zainum ,
    day2_1 ,
    day2_2  ,
    day2_3  ,
    day2_4  ,
    day2_5  ,
    day2_6  ,
    day2_7  ,
    day2_8  ,
    day2_9  ,
    day2_10 ,
    day2_11 ,
    day2_12 ,
    day2_13 ,
    day2_14 ,
    day2_15 ,
    day2_16 ,
    day2_17 ,
    day2_18 ,
    day2_19 ,
    day2_20 ,
    day2_21 ,
    day2_22 ,
    day2_23 ,
    day2_24 ,
    day2_25 ,
    day2_26 ,
    day2_27 ,
    day2_28 ,
    day2_29 ,
    day2_30 ,
    day2_31 ,
    day3_1  ,
    day3_2  ,
    day3_3  ,
    day3_4  ,
    day3_5  ,
    day3_6  ,
    day3_7  ,
    day3_8  ,
    day3_9  ,
    day3_10 ,
    day3_11 ,
    day3_12 ,
    day3_13 ,
    day3_14 ,
    day3_15 ,
    day3_16 ,
    day3_17 ,
    day3_18 ,
    day3_19 ,
    day3_20 ,
    day3_21 ,
    day3_22 ,
    day3_23 ,
    day3_24 ,
    day3_25 ,
    day3_26 ,
    day3_27 ,
    day3_28 ,
    day3_29 ,
    day3_30 ,
    day3_31 
) as select
    hospnum ,
    ptid    ,
    zainum  ,
    day2_1  ,
    day2_2  ,
    day2_3  ,
    day2_4  ,
    day2_5  ,
    day2_6  ,
    day2_7  ,
    day2_8  ,
    day2_9  ,
    day2_10 ,
    day2_11 ,
    day2_12 ,
    day2_13 ,
    day2_14 ,
    day2_15 ,
    day2_16 ,
    day2_17 ,
    day2_18 ,
    day2_19 ,
    day2_20 ,
    day2_21 ,
    day2_22 ,
    day2_23 ,
    day2_24 ,
    day2_25 ,
    day2_26 ,
    day2_27 ,
    day2_28 ,
    day2_29 ,
    day2_30 ,
    day2_31 ,
    day3_1  ,
    day3_2  ,
    day3_3  ,
    day3_4  ,
    day3_5  ,
    day3_6  ,
    day3_7  ,
    day3_8  ,
    day3_9  ,
    day3_10 ,
    day3_11 ,
    day3_12 ,
    day3_13 ,
    day3_14 ,
    day3_15 ,
    day3_16 ,
    day3_17 ,
    day3_18 ,
    day3_19 ,
    day3_20 ,
    day3_21 ,
    day3_22 ,
    day3_23 ,
    day3_24 ,
    day3_25 ,
    day3_26 ,
    day3_27 ,
    day3_28 ,
    day3_29 ,
    day3_30 ,
    day3_31 
from tbl_sryacct
where
   day2_1        > 0
or day2_2        > 0
or day2_3        > 0
or day2_4        > 0
or day2_5        > 0
or day2_6        > 0
or day2_7        > 0
or day2_8        > 0
or day2_9        > 0
or day2_10       > 0
or day2_11       > 0
or day2_12       > 0
or day2_13       > 0
or day2_14       > 0
or day2_15       > 0
or day2_16       > 0
or day2_17       > 0
or day2_18       > 0
or day2_19       > 0
or day2_20       > 0
or day2_21       > 0
or day2_22       > 0
or day2_23       > 0
or day2_24       > 0
or day2_25       > 0
or day2_26       > 0
or day2_27       > 0
or day2_28       > 0
or day2_29       > 0
or day2_30       > 0
or day2_31       > 0
or day3_1        > 0
or day3_2        > 0
or day3_3        > 0
or day3_4        > 0
or day3_5        > 0
or day3_6        > 0
or day3_7        > 0
or day3_8        > 0
or day3_9        > 0
or day3_10       > 0
or day3_11       > 0
or day3_12       > 0
or day3_13       > 0
or day3_14       > 0
or day3_15       > 0
or day3_16       > 0
or day3_17       > 0
or day3_18       > 0
or day3_19       > 0
or day3_20       > 0
or day3_21       > 0
or day3_22       > 0
or day3_23       > 0
or day3_24       > 0
or day3_25       > 0
or day3_26       > 0
or day3_27       > 0
or day3_28       > 0
or day3_29       > 0
or day3_30       > 0
or day3_31       > 0
;

create table tbl_sryacct_sub(
hospnum      smallint         ,
ptid         bigint   ,
zainum      integer          ,
rennum      smallint         ,
date_hi     smallint         ,
day_cnt     smallint         
);

insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,1 ,day2_1 from tbl_sryacct_sub_temp where day2_1 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,2 ,day2_2 from tbl_sryacct_sub_temp where day2_2 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,3 ,day2_3 from tbl_sryacct_sub_temp where day2_3 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,4 ,day2_4 from tbl_sryacct_sub_temp where day2_4 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,5 ,day2_5 from tbl_sryacct_sub_temp where day2_5 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,6 ,day2_6 from tbl_sryacct_sub_temp where day2_6 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,7 ,day2_7 from tbl_sryacct_sub_temp where day2_7 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,8 ,day2_8 from tbl_sryacct_sub_temp where day2_8 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,9 ,day2_9 from tbl_sryacct_sub_temp where day2_9 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,10,day2_10 from tbl_sryacct_sub_temp where day2_10 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,11,day2_11 from tbl_sryacct_sub_temp where day2_11 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,12,day2_12 from tbl_sryacct_sub_temp where day2_12 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,13,day2_13 from tbl_sryacct_sub_temp where day2_13 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,14,day2_14 from tbl_sryacct_sub_temp where day2_14 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,15,day2_15 from tbl_sryacct_sub_temp where day2_15 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,16,day2_16 from tbl_sryacct_sub_temp where day2_16 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,17,day2_17 from tbl_sryacct_sub_temp where day2_17 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,18,day2_18 from tbl_sryacct_sub_temp where day2_18 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,19,day2_19 from tbl_sryacct_sub_temp where day2_19 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,20,day2_20 from tbl_sryacct_sub_temp where day2_20 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,21,day2_21 from tbl_sryacct_sub_temp where day2_21 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,22,day2_22 from tbl_sryacct_sub_temp where day2_22 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,23,day2_23 from tbl_sryacct_sub_temp where day2_23 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,24,day2_24 from tbl_sryacct_sub_temp where day2_24 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,25,day2_25 from tbl_sryacct_sub_temp where day2_25 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,26,day2_26 from tbl_sryacct_sub_temp where day2_26 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,27,day2_27 from tbl_sryacct_sub_temp where day2_27 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,28,day2_28 from tbl_sryacct_sub_temp where day2_28 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,29,day2_29 from tbl_sryacct_sub_temp where day2_29 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,30,day2_30 from tbl_sryacct_sub_temp where day2_30 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,2,31,day2_31 from tbl_sryacct_sub_temp where day2_31 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,1 ,day3_1 from tbl_sryacct_sub_temp where day3_1 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,2 ,day3_2 from tbl_sryacct_sub_temp where day3_2 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,3 ,day3_3 from tbl_sryacct_sub_temp where day3_3 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,4 ,day3_4 from tbl_sryacct_sub_temp where day3_4 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,5 ,day3_5 from tbl_sryacct_sub_temp where day3_5 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,6 ,day3_6 from tbl_sryacct_sub_temp where day3_6 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,7 ,day3_7 from tbl_sryacct_sub_temp where day3_7 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,8 ,day3_8 from tbl_sryacct_sub_temp where day3_8 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,9 ,day3_9 from tbl_sryacct_sub_temp where day3_9 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,10,day3_10 from tbl_sryacct_sub_temp where day3_10 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,11,day3_11 from tbl_sryacct_sub_temp where day3_11 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,12,day3_12 from tbl_sryacct_sub_temp where day3_12 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,13,day3_13 from tbl_sryacct_sub_temp where day3_13 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,14,day3_14 from tbl_sryacct_sub_temp where day3_14 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,15,day3_15 from tbl_sryacct_sub_temp where day3_15 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,16,day3_16 from tbl_sryacct_sub_temp where day3_16 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,17,day3_17 from tbl_sryacct_sub_temp where day3_17 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,18,day3_18 from tbl_sryacct_sub_temp where day3_18 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,19,day3_19 from tbl_sryacct_sub_temp where day3_19 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,20,day3_20 from tbl_sryacct_sub_temp where day3_20 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,21,day3_21 from tbl_sryacct_sub_temp where day3_21 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,22,day3_22 from tbl_sryacct_sub_temp where day3_22 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,23,day3_23 from tbl_sryacct_sub_temp where day3_23 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,24,day3_24 from tbl_sryacct_sub_temp where day3_24 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,25,day3_25 from tbl_sryacct_sub_temp where day3_25 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,26,day3_26 from tbl_sryacct_sub_temp where day3_26 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,27,day3_27 from tbl_sryacct_sub_temp where day3_27 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,28,day3_28 from tbl_sryacct_sub_temp where day3_28 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,29,day3_29 from tbl_sryacct_sub_temp where day3_29 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,30,day3_30 from tbl_sryacct_sub_temp where day3_30 > 0;
insert into tbl_sryacct_sub(hospnum,ptid,zainum,rennum,date_hi,day_cnt) select hospnum,ptid,zainum,3,31,day3_31 from tbl_sryacct_sub_temp where day3_31 > 0;
ALTER TABLE ONLY tbl_sryacct_sub ADD CONSTRAINT tbl_sryacct_sub_primary_key PRIMARY KEY (hospnum,ptid,zainum,rennum,date_hi);
