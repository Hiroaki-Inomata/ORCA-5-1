CREATE TABLE tbl_nyuinkhn (
    khn_srycd character(9) NOT NULL,
    yukostymd character(8) NOT NULL,
    yukoedymd character(8) NOT NULL,
    iproukbn character(1),
    sstkijunkbn character(1),
    btusbtkbn character(2),
    heikinzaikbn character(2),
    kangohaichikbn character(2),
    kangohiritukbn character(2),
    kangohojohaichikbn character(2),
    khn_ten numeric(6,0),
    kagen_srycd1 character(9),
    kagen_srycd2 character(9),
    kagen_srycd3 character(9),
    kagen_srycd4 character(9),
    kagen_srycd5 character(9),
    kagen_srycd6 character(9),
    kagen_srycd7 character(9),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_nyuinkhn
    ADD CONSTRAINT tbl_nyuinkhn_primary_key PRIMARY KEY (khn_srycd, yukostymd, yukoedymd);

CREATE INDEX idx_nyuinkhn_key2 ON tbl_nyuinkhn USING btree (btusbtkbn, heikinzaikbn, kangohaichikbn, kangohiritukbn, kangohojohaichikbn);

