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
      *    ＡＰＩレスポンス予約一覧 (xml2)    COPY (CPACCPTLSTV2RES.INC)
      ******************************************************************
        01  XML-APPOINTLSTRES.
            02  APLSTRES-CONTEXT           PIC S9(9)   BINARY.
            02  APLSTRES-OBJECT            PIC X(36).
            02  APLSTRES-MODE              PIC S9(9)   BINARY.
            02  APLSTRES-RECORDNAME        PIC X(128).
            02  APLSTRES-APPOINTRES.
      *       実施日
              03  APLSTRES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              03  APLSTRES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              03  APLSTRES-API-RESULT          PIC X(2).
      *       返却メッセージ
              03  APLSTRES-API-RESULT-MSG      PIC X(100).
      *
              03  APLSTRES-RESKEY              PIC X(100).
      *       予約日
              03  APLSTRES-APPOINT-DATE        PIC X(10).
      *       予約内容
              03  APLSTRES-APPOINT-LST-G.
                  05  APLSTRES-APPOINT-LST     OCCURS   500.
      *               予約時間
                      07  APLSTRES-APPOINT-TIME        PIC X(8).
      *               診療内容区分
                      07  APLSTRES-APPOINT-DEP-INFO    PIC X(2).
      *               診療科
                      07  APLSTRES-APPOINT-DEP-CODE    PIC X(2).
      *               診療科名称
                      07  APLSTRES-APPOINT-DEP-NAME    PIC X(80).
      *               ドクターコード
                      07  APLSTRES-APPOINT-DR-CODE     PIC X(5).
      *               ドクター氏名
                      07  APLSTRES-APPOINT-DR-NAME     PIC X(80).
      *               来院済区分
                      07  APLSTRES-VISIT-INFO          PIC X(1).
      *H29.11         予約ID
                      07  APLSTRES-APPOINT-ID          PIC X(2).
      *               患者情報
                      07  APLSTRES-PATIENT-INFOMATION.
      *                   患者番号
                          09  APLSTRES-PATIENTID         PIC X(20).
      *                   患者氏名
                          09  APLSTRES-NAME              PIC X(100).
      *                   患者カナ氏名
                          09  APLSTRES-KANANAME          PIC X(100).
      *                   誕生日
                          09  APLSTRES-BIRTHDAY          PIC X(10).
      *                   性別
                          09  APLSTRES-SEX               PIC X(1).
      *                   自宅電話番号（昼）
                          09  APLSTRES-HOME-TEL          PIC X(15).
      *
