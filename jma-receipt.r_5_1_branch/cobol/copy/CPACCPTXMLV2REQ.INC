      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ******************************************************************
      *    ＡＰＩリクエスト受付情報　　       COPY (CPACCPTXMLV2REQ.INC)
      ******************************************************************
        01  XML-ACCEPTREQ.
            02  ACCPTREQ-CONTEXT          PIC S9(9)   BINARY.
            02  ACCPTREQ-OBJECT           PIC X(36).
            02  ACCPTREQ-MODE             PIC S9(9)   BINARY.
            02  ACCPTREQ-RECORDNAME       PIC X(128).
            02  ACCPTREQ-REGIST.
      *       リクエスト番号
              03  ACCPTREQ-REQUEST-NUMBER  PIC X(2).
      *       患者番号
              03  ACCPTREQ-PATIENTID       PIC X(20).
      *       患者氏名
              03  ACCPTREQ-NAME            PIC X(50).
      *
      *       受付日
              03  ACCPTREQ-REGIST-DATE  PIC X(10).
      *       受付時間
              03  ACCPTREQ-REGIST-TIME  PIC X(8).
      *       受付ＩＤ
              03  ACCPTREQ-REGIST-ID    PIC X(5).
      *       診療科
              03  ACCPTREQ-REGIST-DEP-CODE PIC X(2).
      *       ドクターコード
              03  ACCPTREQ-REGIST-DR-CODE  PIC X(5).
      *       診療内容コード
              03  ACCPTREQ-REGIST-DEP-INFO PIC X(2).
      *       保険情報
              03  ACCPTREQ-INSURANCE-INFO.
      *         保険組合せ番号
                04  ACCPTREQ-COMBINATION-NUMBER     PIC X(04).
      *         保険の種類
                04  ACCPTREQ-MAIN-INSURANCE-CLASS   PIC X(03).
      *         保険者番号
                04  ACCPTREQ-MAIN-INSURANCE-NUMBER  PIC X(08).
      *         保険の種類名称
                04  ACCPTREQ-MAIN-INSURANCE-NAME    PIC X(20).
      *         記号
                04  ACCPTREQ-MAIN-MARK              PIC X(80).
      *         番号
                04  ACCPTREQ-MAIN-NUMBER            PIC X(80).
      *         継続区分
                04  ACCPTREQ-MAIN-CONTINUATION      PIC X(01).
      *         保険の種類
                04  ACCPTREQ-MAIN-ASSISTANCE        PIC X(01).
      *         本人家族区分
                04  ACCPTREQ-MAIN-FAMILY            PIC X(01).
      *         被保険者名
                04  ACCPTREQ-MAIN-POLICYHOLDER      PIC X(100).
      *         有効開始日
                04  ACCPTREQ-MAIN-START-DATE        PIC X(10).
      *         有効終了日
                04  ACCPTREQ-MAIN-END-DATE          PIC X(10).
      *         公費情報
                04  ACCPTREQ-PUBLIC-INSURANCE-INFO
                                       OCCURS  4 TIMES.
      *           公費の種類
                  05  ACCPTREQ-INSURANCE-CLASS      PIC X(03).
      *           公費の種類名称
                  05  ACCPTREQ-INSURANCE-NAME       PIC X(20).
      *           負担者番号
                  05  ACCPTREQ-PROVIDER-NUMBER      PIC X(08).
      *           受給者番号
                  05  ACCPTREQ-RECIPIENT-NUMBER     PIC X(20).
      *           有効開始日
                  05  ACCPTREQ-INSURANCE-START-DATE PIC X(10).
      *           有効終了日
                  05  ACCPTREQ-INSURANCE-END-DATE   PIC X(10).
