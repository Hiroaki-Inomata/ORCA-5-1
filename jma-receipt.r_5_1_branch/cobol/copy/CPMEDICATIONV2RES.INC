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
      *    ＡＰＩスポンス点数マスタ登録     COPY (CPMEDICATIONV2RES.INC)
      ******************************************************************
        01  XML-MEDICATIONRES.
            02  MEDIRES-CONTEXT            PIC S9(9)   BINARY.
            02  MEDIRES-OBJECT             PIC X(36).
            02  MEDIRES-MODE               PIC S9(9)   BINARY.
            02  MEDIRES-RECORDNAME         PIC X(128).
            02  MEDIRES-APPOINTRES.
      *       実施日
              03  MEDIRES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              03  MEDIRES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              03  MEDIRES-API-RESULT          PIC X(2).
      *       返却メッセージ
              03  MEDIRES-API-RESULT-MSG      PIC X(100).
      *
              03  MEDIRES-RESKEY              PIC X(100).
      *
      *       マスタコード
              03  MEDIRES-MEDICATION-CODE      PIC X(09).
      *       漢字名称
              03  MEDIRES-MEDICATION-NAME      PIC X(100).
      *       漢字名称
              03  MEDIRES-MEDICATION-KNAME     PIC X(100).
      *       開始日
              03  MEDIRES-START-DATE           PIC X(10).
      *       終了日
              03  MEDIRES-END-DATE             PIC X(10).
      *       金額（自費・器材）
              03  MEDIRES-AMOUNT-MONEY         PIC X(9).
      *       自費コード領域
              03  MEDIRES-AMOUNT-AREA.
      *           金額内容（4:税込み）
                  05  MEDIRES-CONTENT-MONEY        PIC X(1).
      *           集計先（外来）
                  05  MEDIRES-TOTAL-OUT            PIC X(3).
      *           集計先（入院）
                  05  MEDIRES-TOTAL-IN             PIC X(3).
      *           自賠責集計先（外来）
                  05  MEDIRES-LIABILITY-TOTAL-OUT  PIC X(1).
      *           自賠責集計先（入院）
                  05  MEDIRES-LIABILITY-TOTAL-IN   PIC X(1).
      *       部位コード領域
              03  MEDIRES-LOCATION-AREA.
      *           部位区分
                  05  MEDIRES-LOCATION-CATEGORY    PIC X(1).
      *       コメントコード領域
              03  MEDIRES-COMMENT-AREA.
      *           カラム位置
                  05  MEDIRES-COMMENT-OCC      OCCURS   5.
                      07  MEDIRES-COLUMN-POSITION  PIC X(3).
                      07  MEDIRES-DIGIT-NUMBER     PIC X(3).
      *       用法コード領域
              03  MEDIRES-MEDICATION-AREA.
      *           服用時点
                  05  MEDIRES-MEDICATION-OCC      OCCURS   5.
                      07  MEDIRES-MEDICATION-POINT  PIC X(1).
      *           用法コメント区分
                  05  MEDIRES-MEDICATION-CATEGORY   PIC X(1).
      *       特定器材コード領域
              03  MEDIRES-EQUIPMENT-AREA.
      *           単位コード
                  05  MEDIRES-UNIT-CODE            PIC X(3).
      *           データ区分（3:フィルム）
                  05  MEDIRES-DATA-CATEGORY        PIC X(1).
      *       器材商品名コード領域
              03  MEDIRES-COMMERCIAL-AREA.
      *           商品名称
                  05  MEDIRES-COMMERCIALNAME       PIC X(200).
      *           特定器材コード
                  05  MEDIRES-EQUIPMENT-CODE       PIC X(9).
