CREATE TABLE tbl_clm_uid (
    hospnum numeric(2,0) NOT NULL,
    ptid numeric(10,0) NOT NULL,
    sryka character(2) NOT NULL,
    uid_new character(36),
    hkncombi1 numeric(4,0),
    uid1 character(36),
    hkncombi2 numeric(4,0),
    uid2 character(36),
    hkncombi3 numeric(4,0),
    uid3 character(36),
    hkncombi4 numeric(4,0),
    uid4 character(36),
    hkncombi5 numeric(4,0),
    uid5 character(36),
    hkncombi6 numeric(4,0),
    uid6 character(36),
    hkncombi7 numeric(4,0),
    uid7 character(36),
    hkncombi8 numeric(4,0),
    uid8 character(36),
    hkncombi9 numeric(4,0),
    uid9 character(36),
    hkncombi10 numeric(4,0),
    uid10 character(36),
    hkncombi11 numeric(4,0),
    uid11 character(36),
    hkncombi12 numeric(4,0),
    uid12 character(36),
    hkncombi13 numeric(4,0),
    uid13 character(36),
    hkncombi14 numeric(4,0),
    uid14 character(36),
    hkncombi15 numeric(4,0),
    uid15 character(36),
    hkncombi16 numeric(4,0),
    uid16 character(36),
    hkncombi17 numeric(4,0),
    uid17 character(36),
    hkncombi18 numeric(4,0),
    uid18 character(36),
    hkncombi19 numeric(4,0),
    uid19 character(36),
    hkncombi20 numeric(4,0),
    uid20 character(36),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_clm_uid
    ADD CONSTRAINT tbl_clm_uid_primary_key PRIMARY KEY (hospnum, ptid, sryka);

