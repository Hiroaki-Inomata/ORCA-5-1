CREATE TABLE tbl_hknnum (
    hknnum character(3) NOT NULL,
    tekstymd character(8) NOT NULL,
    paykbn character(2) NOT NULL,
    tekedymd character(8),
    hbtnum character(2),
    seidoname character varying(100),
    tanseidoname character varying(20),
    hknkohsbtkbn character(1),
    hbtnumchkkbn character(1),
    kensnumchkkbn character(1),
    jkyskensnumchkkbn character(1),
    hknkoukbn character(1),
    genhknnum1 character(3),
    genhknnum2 character(3),
    genhknnum3 character(3),
    joken_styear numeric(3,0) DEFAULT 0,
    joken_edyear numeric(3,0) DEFAULT 0,
    tentnk numeric(3,0) DEFAULT 0,
    receskykbn character(1),
    receftndspkbn character(1),
    recejgndspkbn character(1) DEFAULT '0'::bpchar,
    hon_gaiftnkbn character(1),
    hon_gaikftnrate numeric(3,0) DEFAULT 0,
    hon_gaikkotgak numeric(6,0) DEFAULT 0,
    hon_gaikjgngak numeric(6,0) DEFAULT 0,
    hon_gaidjgngak numeric(6,0) DEFAULT 0,
    hon_gaidjgncnt numeric(2,0) DEFAULT 0,
    hon_gaimnaijgngak numeric(6,0) DEFAULT 0,
    hon_gaimgaijgngak numeric(6,0) DEFAULT 0,
    hon_gaimjgncnt numeric(2,0) DEFAULT 0,
    hon_gaiykzftnkbn character(1),
    hon_nyuftnkbn character(1),
    hon_nyukftnrate numeric(3,0) DEFAULT 0,
    hon_nyukkotgak numeric(6,0) DEFAULT 0,
    hon_nyukjgngak numeric(6,0) DEFAULT 0,
    hon_nyudjgngak numeric(6,0) DEFAULT 0,
    hon_nyudjgncnt numeric(2,0) DEFAULT 0,
    hon_nyumjgngak numeric(6,0) DEFAULT 0,
    hon_nyumjgncnt numeric(2,0) DEFAULT 0,
    hon_nyuskjryykbn character(1),
    hon_nyuskjjyogak numeric(6,0) DEFAULT 0,
    kzk_gaiftnkbn character(1),
    kzk_gaikftnrate numeric(3,0) DEFAULT 0,
    kzk_gaikkotgak numeric(6,0) DEFAULT 0,
    kzk_gaikjgngak numeric(6,0) DEFAULT 0,
    kzk_gaidjgngak numeric(6,0) DEFAULT 0,
    kzk_gaidjgncnt numeric(2,0) DEFAULT 0,
    kzk_gaimnaijgngak numeric(6,0) DEFAULT 0,
    kzk_gaimgaijgngak numeric(6,0) DEFAULT 0,
    kzk_gaimjgncnt numeric(2,0) DEFAULT 0,
    kzk_gaiykzftnkbn character(1),
    kzk_nyuftnkbn character(1),
    kzk_nyukftnrate numeric(3,0) DEFAULT 0,
    kzk_nyukkotgak numeric(6,0) DEFAULT 0,
    kzk_nyukjgngak numeric(6,0) DEFAULT 0,
    kzk_nyudjgngak numeric(6,0) DEFAULT 0,
    kzk_nyudjgncnt numeric(2,0) DEFAULT 0,
    kzk_nyumjgngak numeric(6,0) DEFAULT 0,
    kzk_nyumjgncnt numeric(2,0) DEFAULT 0,
    kzk_nyuskjryykbn character(1),
    kzk_nyuskjjyogak numeric(6,0) DEFAULT 0,
    tsy_gaiftnkbn character(1),
    tsy_gaikftnrate numeric(3,0) DEFAULT 0,
    tsy_gaikkotgak numeric(6,0) DEFAULT 0,
    tsy_gaikjgngak numeric(6,0) DEFAULT 0,
    tsy_gaidjgngak numeric(6,0) DEFAULT 0,
    tsy_gaidjgncnt numeric(2,0) DEFAULT 0,
    tsy_gaimnaijgngak numeric(6,0) DEFAULT 0,
    tsy_gaimgaijgngak numeric(6,0) DEFAULT 0,
    tsy_gaimjgncnt numeric(2,0) DEFAULT 0,
    tsy_gaiykzftnkbn character(1),
    tsy_nyuftnkbn character(1),
    tsy_nyukftnrate numeric(3,0) DEFAULT 0,
    tsy_nyukkotgak numeric(6,0) DEFAULT 0,
    tsy_nyukjgngak numeric(6,0) DEFAULT 0,
    tsy_nyudjgngak numeric(6,0) DEFAULT 0,
    tsy_nyudjgncnt numeric(2,0) DEFAULT 0,
    tsy_nyumjgngak numeric(6,0) DEFAULT 0,
    tsy_nyumjgncnt numeric(2,0) DEFAULT 0,
    tsy_nyuskjryykbn character(1),
    tsy_nyuskjjyogak numeric(6,0) DEFAULT 0,
    tnk_gaiftnkbn character(1),
    tnk_gaikftnrate numeric(3,0) DEFAULT 0,
    tnk_gaikkotgak numeric(6,0) DEFAULT 0,
    tnk_gaikjgngak numeric(6,0) DEFAULT 0,
    tnk_gaidjgngak numeric(6,0) DEFAULT 0,
    tnk_gaidjgncnt numeric(2,0) DEFAULT 0,
    tnk_gaimnaijgngak numeric(6,0) DEFAULT 0,
    tnk_gaimgaijgngak numeric(6,0) DEFAULT 0,
    tnk_gaimjgncnt numeric(2,0) DEFAULT 0,
    tnk_gaiykzftnkbn character(1),
    tnk_nyuftnkbn character(1),
    tnk_nyukftnrate numeric(3,0) DEFAULT 0,
    tnk_nyukkotgak numeric(6,0) DEFAULT 0,
    tnk_nyukjgngak numeric(6,0) DEFAULT 0,
    tnk_nyudjgngak numeric(6,0) DEFAULT 0,
    tnk_nyudjgncnt numeric(2,0) DEFAULT 0,
    tnk_nyumjgngak numeric(6,0) DEFAULT 0,
    tnk_nyumjgncnt numeric(2,0) DEFAULT 0,
    tnk_nyuskjryykbn character(1),
    tnk_nyuskjjyogak numeric(6,0) DEFAULT 0,
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_hknnum
    ADD CONSTRAINT tbl_hknnum_primary_key PRIMARY KEY (hospnum, hknnum, tekstymd, paykbn);
