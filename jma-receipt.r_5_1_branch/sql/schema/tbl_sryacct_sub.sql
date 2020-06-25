CREATE TABLE tbl_sryacct_sub (
    hospnum smallint NOT NULL,
    ptid bigint NOT NULL,
    zainum integer NOT NULL,
    rennum smallint NOT NULL,
    date_hi smallint NOT NULL,
    day_cnt smallint
);

ALTER TABLE ONLY tbl_sryacct_sub
    ADD CONSTRAINT tbl_sryacct_sub_primary_key PRIMARY KEY (hospnum, ptid, zainum, rennum, date_hi);

