\set ON_ERROR_STOP

-- チェックマスタビューを作成する

DROP VIEW IF EXISTS
    tbl_chk
;

CREATE VIEW
    tbl_chk
AS
(
SELECT
    m.*
FROM
    view_chk_master m
    LEFT OUTER JOIN tbl_chk_user u
    ON  (m.hospnum = u.hospnum ) AND
        (m.chkkbn = u.chkkbn ) AND
        (m.srycd = u.srycd ) AND
        (m.grpnum = u.grpnum ) AND
        (m.cdkbn = u.cdkbn ) AND
        (m.cd = u.cd ) AND
        ((u.yukostymd >= m.yukostymd AND u.yukostymd <= m.yukoedymd) OR
         (u.yukoedymd >= m.yukostymd AND u.yukoedymd <= m.yukoedymd) OR
         (u.yukostymd <= m.yukostymd AND u.yukoedymd >= m.yukoedymd))
WHERE u.srycd IS NULL
UNION
SELECT * FROM tbl_chk_user
UNION
SELECT
    m.hospnum,
    m.chkkbn,
    m.srycd,
    m.grpnum,
    u.yukostymd,
    u.yukoedymd,
    m.cdkbn,
    m.cd,
    m.nyugaikbn,
    m.nyugaibndkbn,
    m.syorikbn,
    m.chkerr,
    m.datakbn,
    m.rennum,
    m.termid,
    m.opid,
    m.creymd,
    m.upymd,
    m.uphms
FROM
    view_chk_master m
    INNER JOIN ( SELECT
                     m1.hospnum,
                     m1.chkkbn,
                     m1.srycd,
                     m1.grpnum,
                     MIN(m1.yukostymd) yukostymd,
                     TO_CHAR(TO_DATE(MIN(u1.yukostymd),'YYYYMMDD')-1,'YYYYMMDD') yukoedymd,
                     m1.cdkbn,
                     m1.cd
                 FROM view_chk_master m1
                 INNER JOIN tbl_chk_user u1
                 ON  (m1.hospnum = u1.hospnum ) AND
                     (m1.chkkbn = u1.chkkbn ) AND
                     (m1.srycd = u1.srycd ) AND
                     (m1.grpnum = u1.grpnum ) AND
                     (m1.cdkbn = u1.cdkbn ) AND
                     (m1.cd = u1.cd ) AND
                     (m1.yukostymd <= u1.yukostymd) AND
                     (m1.yukoedymd >= u1.yukostymd)
                 GROUP BY
                     m1.hospnum,
                     m1.chkkbn,
                     m1.srycd,
                     m1.grpnum,
                     m1.cdkbn,
                     m1.cd
                 HAVING MIN(m1.yukostymd) < MIN(u1.yukostymd)
               ) u
    ON  (m.hospnum = u.hospnum ) AND
        (m.chkkbn = u.chkkbn ) AND
        (m.srycd = u.srycd ) AND
        (m.grpnum = u.grpnum ) AND
        (m.cdkbn = u.cdkbn ) AND
        (m.cd = u.cd ) AND
        (m.yukostymd <= u.yukostymd) AND
        (m.yukoedymd >= u.yukostymd)
UNION
SELECT
    m.hospnum,
    m.chkkbn,
    m.srycd,
    m.grpnum,
    u.yukostymd,
    u.yukoedymd,
    m.cdkbn,
    m.cd,
    m.nyugaikbn,
    m.nyugaibndkbn,
    m.syorikbn,
    m.chkerr,
    m.datakbn,
    m.rennum,
    m.termid,
    m.opid,
    m.creymd,
    m.upymd,
    m.uphms
FROM
    view_chk_master m
    INNER JOIN ( SELECT
                     m1.hospnum,
                     m1.chkkbn,
                     m1.srycd,
                     m1.grpnum,
                     TO_CHAR(TO_DATE(MAX(u1.yukoedymd),'YYYYMMDD')+1,'YYYYMMDD') yukostymd,
                     MAX(m1.yukoedymd) yukoedymd,
                     m1.cdkbn,
                     m1.cd
                 FROM view_chk_master m1
                 INNER JOIN tbl_chk_user u1
                 ON  (m1.hospnum = u1.hospnum ) AND
                     (m1.chkkbn = u1.chkkbn ) AND
                     (m1.srycd = u1.srycd ) AND
                     (m1.grpnum = u1.grpnum ) AND
                     (m1.cdkbn = u1.cdkbn ) AND
                     (m1.cd = u1.cd ) AND
                     (m1.yukostymd <= u1.yukoedymd) AND
                     (m1.yukoedymd >= u1.yukoedymd)
                 GROUP BY
                     m1.hospnum,
                     m1.chkkbn,
                     m1.srycd,
                     m1.grpnum,
                     m1.cdkbn,
                     m1.cd
                 HAVING MAX(m1.yukoedymd) > MAX(u1.yukoedymd)
               ) u
    ON  (m.hospnum = u.hospnum ) AND
        (m.chkkbn = u.chkkbn ) AND
        (m.srycd = u.srycd ) AND
        (m.grpnum = u.grpnum ) AND
        (m.cdkbn = u.cdkbn ) AND
        (m.cd = u.cd ) AND
        (m.yukostymd <= u.yukoedymd) AND
        (m.yukoedymd >= u.yukoedymd)
ORDER BY
    hospnum,
    srycd,
    yukostymd
);
