--                                    --
-- 診療行為(プレビュー用)             --
--                                    --
-- 項目の追加                         --
--                                    --
-- Create Date : 2017/6/13            --
--                                    --

\set ON_ERROR_STOP

alter table tbl_sryacct_prv add column day4_1 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_2 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_3 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_4 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_5 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_6 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_7 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_8 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_9 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_10 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_11 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_12 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_13 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_14 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_15 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_16 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_17 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_18 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_19 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_20 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_21 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_22 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_23 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_24 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_25 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_26 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_27 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_28 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_29 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_30 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day4_31 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_1 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_2 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_3 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_4 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_5 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_6 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_7 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_8 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_9 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_10 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_11 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_12 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_13 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_14 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_15 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_16 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_17 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_18 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_19 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_20 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_21 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_22 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_23 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_24 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_25 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_26 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_27 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_28 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_29 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_30 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day5_31 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_1 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_2 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_3 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_4 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_5 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_6 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_7 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_8 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_9 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_10 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_11 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_12 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_13 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_14 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_15 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_16 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_17 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_18 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_19 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_20 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_21 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_22 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_23 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_24 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_25 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_26 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_27 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_28 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_29 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_30 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day6_31 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_1 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_2 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_3 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_4 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_5 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_6 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_7 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_8 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_9 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_10 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_11 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_12 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_13 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_14 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_15 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_16 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_17 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_18 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_19 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_20 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_21 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_22 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_23 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_24 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_25 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_26 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_27 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_28 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_29 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_30 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day7_31 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_1 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_2 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_3 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_4 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_5 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_6 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_7 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_8 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_9 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_10 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_11 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_12 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_13 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_14 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_15 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_16 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_17 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_18 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_19 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_20 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_21 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_22 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_23 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_24 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_25 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_26 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_27 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_28 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_29 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_30 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day8_31 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_1 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_2 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_3 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_4 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_5 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_6 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_7 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_8 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_9 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_10 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_11 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_12 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_13 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_14 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_15 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_16 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_17 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_18 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_19 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_20 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_21 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_22 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_23 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_24 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_25 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_26 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_27 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_28 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_29 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_30 numeric(3,0) default 0;
alter table tbl_sryacct_prv add column day9_31 numeric(3,0) default 0;

update tbl_sryacct_prv set day4_1 = 0;
update tbl_sryacct_prv set day4_2 = 0;
update tbl_sryacct_prv set day4_3 = 0;
update tbl_sryacct_prv set day4_4 = 0;
update tbl_sryacct_prv set day4_5 = 0;
update tbl_sryacct_prv set day4_6 = 0;
update tbl_sryacct_prv set day4_7 = 0;
update tbl_sryacct_prv set day4_8 = 0;
update tbl_sryacct_prv set day4_9 = 0;
update tbl_sryacct_prv set day4_10 = 0;
update tbl_sryacct_prv set day4_11 = 0;
update tbl_sryacct_prv set day4_12 = 0;
update tbl_sryacct_prv set day4_13 = 0;
update tbl_sryacct_prv set day4_14 = 0;
update tbl_sryacct_prv set day4_15 = 0;
update tbl_sryacct_prv set day4_16 = 0;
update tbl_sryacct_prv set day4_17 = 0;
update tbl_sryacct_prv set day4_18 = 0;
update tbl_sryacct_prv set day4_19 = 0;
update tbl_sryacct_prv set day4_20 = 0;
update tbl_sryacct_prv set day4_21 = 0;
update tbl_sryacct_prv set day4_22 = 0;
update tbl_sryacct_prv set day4_23 = 0;
update tbl_sryacct_prv set day4_24 = 0;
update tbl_sryacct_prv set day4_25 = 0;
update tbl_sryacct_prv set day4_26 = 0;
update tbl_sryacct_prv set day4_27 = 0;
update tbl_sryacct_prv set day4_28 = 0;
update tbl_sryacct_prv set day4_29 = 0;
update tbl_sryacct_prv set day4_30 = 0;
update tbl_sryacct_prv set day4_31 = 0;
update tbl_sryacct_prv set day5_1 = 0;
update tbl_sryacct_prv set day5_2 = 0;
update tbl_sryacct_prv set day5_3 = 0;
update tbl_sryacct_prv set day5_4 = 0;
update tbl_sryacct_prv set day5_5 = 0;
update tbl_sryacct_prv set day5_6 = 0;
update tbl_sryacct_prv set day5_7 = 0;
update tbl_sryacct_prv set day5_8 = 0;
update tbl_sryacct_prv set day5_9 = 0;
update tbl_sryacct_prv set day5_10 = 0;
update tbl_sryacct_prv set day5_11 = 0;
update tbl_sryacct_prv set day5_12 = 0;
update tbl_sryacct_prv set day5_13 = 0;
update tbl_sryacct_prv set day5_14 = 0;
update tbl_sryacct_prv set day5_15 = 0;
update tbl_sryacct_prv set day5_16 = 0;
update tbl_sryacct_prv set day5_17 = 0;
update tbl_sryacct_prv set day5_18 = 0;
update tbl_sryacct_prv set day5_19 = 0;
update tbl_sryacct_prv set day5_20 = 0;
update tbl_sryacct_prv set day5_21 = 0;
update tbl_sryacct_prv set day5_22 = 0;
update tbl_sryacct_prv set day5_23 = 0;
update tbl_sryacct_prv set day5_24 = 0;
update tbl_sryacct_prv set day5_25 = 0;
update tbl_sryacct_prv set day5_26 = 0;
update tbl_sryacct_prv set day5_27 = 0;
update tbl_sryacct_prv set day5_28 = 0;
update tbl_sryacct_prv set day5_29 = 0;
update tbl_sryacct_prv set day5_30 = 0;
update tbl_sryacct_prv set day5_31 = 0;
update tbl_sryacct_prv set day6_1 = 0;
update tbl_sryacct_prv set day6_2 = 0;
update tbl_sryacct_prv set day6_3 = 0;
update tbl_sryacct_prv set day6_4 = 0;
update tbl_sryacct_prv set day6_5 = 0;
update tbl_sryacct_prv set day6_6 = 0;
update tbl_sryacct_prv set day6_7 = 0;
update tbl_sryacct_prv set day6_8 = 0;
update tbl_sryacct_prv set day6_9 = 0;
update tbl_sryacct_prv set day6_10 = 0;
update tbl_sryacct_prv set day6_11 = 0;
update tbl_sryacct_prv set day6_12 = 0;
update tbl_sryacct_prv set day6_13 = 0;
update tbl_sryacct_prv set day6_14 = 0;
update tbl_sryacct_prv set day6_15 = 0;
update tbl_sryacct_prv set day6_16 = 0;
update tbl_sryacct_prv set day6_17 = 0;
update tbl_sryacct_prv set day6_18 = 0;
update tbl_sryacct_prv set day6_19 = 0;
update tbl_sryacct_prv set day6_20 = 0;
update tbl_sryacct_prv set day6_21 = 0;
update tbl_sryacct_prv set day6_22 = 0;
update tbl_sryacct_prv set day6_23 = 0;
update tbl_sryacct_prv set day6_24 = 0;
update tbl_sryacct_prv set day6_25 = 0;
update tbl_sryacct_prv set day6_26 = 0;
update tbl_sryacct_prv set day6_27 = 0;
update tbl_sryacct_prv set day6_28 = 0;
update tbl_sryacct_prv set day6_29 = 0;
update tbl_sryacct_prv set day6_30 = 0;
update tbl_sryacct_prv set day6_31 = 0;
update tbl_sryacct_prv set day7_1 = 0;
update tbl_sryacct_prv set day7_2 = 0;
update tbl_sryacct_prv set day7_3 = 0;
update tbl_sryacct_prv set day7_4 = 0;
update tbl_sryacct_prv set day7_5 = 0;
update tbl_sryacct_prv set day7_6 = 0;
update tbl_sryacct_prv set day7_7 = 0;
update tbl_sryacct_prv set day7_8 = 0;
update tbl_sryacct_prv set day7_9 = 0;
update tbl_sryacct_prv set day7_10 = 0;
update tbl_sryacct_prv set day7_11 = 0;
update tbl_sryacct_prv set day7_12 = 0;
update tbl_sryacct_prv set day7_13 = 0;
update tbl_sryacct_prv set day7_14 = 0;
update tbl_sryacct_prv set day7_15 = 0;
update tbl_sryacct_prv set day7_16 = 0;
update tbl_sryacct_prv set day7_17 = 0;
update tbl_sryacct_prv set day7_18 = 0;
update tbl_sryacct_prv set day7_19 = 0;
update tbl_sryacct_prv set day7_20 = 0;
update tbl_sryacct_prv set day7_21 = 0;
update tbl_sryacct_prv set day7_22 = 0;
update tbl_sryacct_prv set day7_23 = 0;
update tbl_sryacct_prv set day7_24 = 0;
update tbl_sryacct_prv set day7_25 = 0;
update tbl_sryacct_prv set day7_26 = 0;
update tbl_sryacct_prv set day7_27 = 0;
update tbl_sryacct_prv set day7_28 = 0;
update tbl_sryacct_prv set day7_29 = 0;
update tbl_sryacct_prv set day7_30 = 0;
update tbl_sryacct_prv set day7_31 = 0;
update tbl_sryacct_prv set day8_1 = 0;
update tbl_sryacct_prv set day8_2 = 0;
update tbl_sryacct_prv set day8_3 = 0;
update tbl_sryacct_prv set day8_4 = 0;
update tbl_sryacct_prv set day8_5 = 0;
update tbl_sryacct_prv set day8_6 = 0;
update tbl_sryacct_prv set day8_7 = 0;
update tbl_sryacct_prv set day8_8 = 0;
update tbl_sryacct_prv set day8_9 = 0;
update tbl_sryacct_prv set day8_10 = 0;
update tbl_sryacct_prv set day8_11 = 0;
update tbl_sryacct_prv set day8_12 = 0;
update tbl_sryacct_prv set day8_13 = 0;
update tbl_sryacct_prv set day8_14 = 0;
update tbl_sryacct_prv set day8_15 = 0;
update tbl_sryacct_prv set day8_16 = 0;
update tbl_sryacct_prv set day8_17 = 0;
update tbl_sryacct_prv set day8_18 = 0;
update tbl_sryacct_prv set day8_19 = 0;
update tbl_sryacct_prv set day8_20 = 0;
update tbl_sryacct_prv set day8_21 = 0;
update tbl_sryacct_prv set day8_22 = 0;
update tbl_sryacct_prv set day8_23 = 0;
update tbl_sryacct_prv set day8_24 = 0;
update tbl_sryacct_prv set day8_25 = 0;
update tbl_sryacct_prv set day8_26 = 0;
update tbl_sryacct_prv set day8_27 = 0;
update tbl_sryacct_prv set day8_28 = 0;
update tbl_sryacct_prv set day8_29 = 0;
update tbl_sryacct_prv set day8_30 = 0;
update tbl_sryacct_prv set day8_31 = 0;
update tbl_sryacct_prv set day9_1 = 0;
update tbl_sryacct_prv set day9_2 = 0;
update tbl_sryacct_prv set day9_3 = 0;
update tbl_sryacct_prv set day9_4 = 0;
update tbl_sryacct_prv set day9_5 = 0;
update tbl_sryacct_prv set day9_6 = 0;
update tbl_sryacct_prv set day9_7 = 0;
update tbl_sryacct_prv set day9_8 = 0;
update tbl_sryacct_prv set day9_9 = 0;
update tbl_sryacct_prv set day9_10 = 0;
update tbl_sryacct_prv set day9_11 = 0;
update tbl_sryacct_prv set day9_12 = 0;
update tbl_sryacct_prv set day9_13 = 0;
update tbl_sryacct_prv set day9_14 = 0;
update tbl_sryacct_prv set day9_15 = 0;
update tbl_sryacct_prv set day9_16 = 0;
update tbl_sryacct_prv set day9_17 = 0;
update tbl_sryacct_prv set day9_18 = 0;
update tbl_sryacct_prv set day9_19 = 0;
update tbl_sryacct_prv set day9_20 = 0;
update tbl_sryacct_prv set day9_21 = 0;
update tbl_sryacct_prv set day9_22 = 0;
update tbl_sryacct_prv set day9_23 = 0;
update tbl_sryacct_prv set day9_24 = 0;
update tbl_sryacct_prv set day9_25 = 0;
update tbl_sryacct_prv set day9_26 = 0;
update tbl_sryacct_prv set day9_27 = 0;
update tbl_sryacct_prv set day9_28 = 0;
update tbl_sryacct_prv set day9_29 = 0;
update tbl_sryacct_prv set day9_30 = 0;
update tbl_sryacct_prv set day9_31 = 0;

