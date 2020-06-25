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
      *    ＡＰＩリクエスト点数マスタ登録     COPY (CPACCPTLSTV2REQ.INC)
      ******************************************************************
        01  XML-MEDICATIONREQ.
            02  MEDIREQ-CONTEXT            PIC S9(9)   BINARY.
            02  MEDIREQ-OBJECT             PIC X(36).
            02  MEDIREQ-MODE               PIC S9(9)   BINARY.
            02  MEDIREQ-RECORDNAME         PIC X(128).
            02  MEDIREQ-APPOINT.
      *       マスタコード
              03  MEDIREQ-MEDICATION-CODE      PIC X(09).
      *       漢字名称
              03  MEDIREQ-MEDICATION-NAME      PIC X(100).
      *       漢字名称
              03  MEDIREQ-MEDICATION-KNAME     PIC X(100).
      *       開始日
              03  MEDIREQ-START-DATE           PIC X(10).
      *       終了日
              03  MEDIREQ-END-DATE             PIC X(10).
      *       自費金額
              03  MEDIREQ-AMOUNT-MONEY         PIC X(9).
      *       自費コード領域
              03  MEDIREQ-AMOUNT-AREA.
      *           金額内容（4:税込み）
                  05  MEDIREQ-CONTENT-MONEY        PIC X(1).
      *           集計先（外来）
                  05  MEDIREQ-TOTAL-OUT            PIC X(3).
      *           集計先（入院）
                  05  MEDIREQ-TOTAL-IN             PIC X(3).
      *           自賠責集計先（外来）
                  05  MEDIREQ-LIABILITY-TOTAL-OUT  PIC X(1).
      *           自賠責集計先（入院）
                  05  MEDIREQ-LIABILITY-TOTAL-IN   PIC X(1).
      *       部位コード領域
              03  MEDIREQ-LOCATION-AREA.
      *           部位区分
                  05  MEDIREQ-LOCATION-CATEGORY    PIC X(1).
      *       コメントコード領域
              03  MEDIREQ-COMMENT-AREA.
      *           カラム位置
                  05  MEDIREQ-COMMENT-OCC      OCCURS   5.
                      07  MEDIREQ-COLUMN-POSITION  PIC X(3).
                      07  MEDIREQ-DIGIT-NUMBER     PIC X(3).
      *       用法コード領域
              03  MEDIREQ-MEDICATION-AREA.
      *           服用時点
                  05  MEDIREQ-MEDICATION-OCC      OCCURS   5.
                      07  MEDIREQ-MEDICATION-POINT  PIC X(1).
      *           用法コメント区分
                  05  MEDIREQ-MEDICATION-CATEGORY   PIC X(1).
      *       特定器材コード領域
              03  MEDIREQ-EQUIPMENT-AREA.
      *           単位コード
                  05  MEDIREQ-UNIT-CODE            PIC X(3).
      *           データ区分（3:フィルム）
                  05  MEDIREQ-DATA-CATEGORY        PIC X(1).
      *       器材商品名コード領域
              03  MEDIREQ-COMMERCIAL-AREA.
      *           商品名称
                  05  MEDIREQ-COMMERCIALNAME       PIC X(200).
      *           特定器材コード
                  05  MEDIREQ-EQUIPMENT-CODE       PIC X(9).
