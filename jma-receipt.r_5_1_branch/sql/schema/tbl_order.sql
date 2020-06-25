CREATE TABLE tbl_order (
    karte_key character varying(36) NOT NULL,
    ordernum numeric(2,0) NOT NULL,
    nyugaikbn character(1),
    ptnum character(20),
    ptid numeric(10,0),
    sryka character(2),
    sryymd character(8),
    hkncombi numeric(4,0),
    orderid character(4),
    ukeymd character(8),
    ukehms character(6),
    errcd1 character(4),
    errcd2 character(4),
    status numeric(1,0),
    modify numeric(1,0),
    keygenerate numeric(1,0),
    rrknum numeric(3,0),
    rrkedanum numeric(3,0),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL
);

COMMENT ON TABLE tbl_order IS '入院オーダ';

ALTER TABLE ONLY tbl_order
    ADD CONSTRAINT tbl_order_primary_key PRIMARY KEY (hospnum, karte_key, ordernum);

CREATE INDEX idx_order_ptnum ON tbl_order USING btree (hospnum, ptnum);

CREATE INDEX idx_order_sryymd ON tbl_order USING btree (hospnum, sryymd);

CREATE INDEX idx_order_ukeymd ON tbl_order USING btree (hospnum, ukeymd);

