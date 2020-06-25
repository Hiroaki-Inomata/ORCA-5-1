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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ******************************************************************
      *    ＡＰＩレスポンス来院一覧情報取得（来院日付）(xml2)
      *                                   COPY (CPVISITPTLST01V2RES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
        01  XML-VISITPTLST02RES.
            02  VIS02RES-CONTEXT           PIC S9(9)   BINARY.
            02  VIS02RES-OBJECT            PIC X(36).
            02  VIS02RES-MODE              PIC S9(9)   BINARY.
            02  VIS02RES-RECORDNAME        PIC X(128).
            02  VIS02RES-VISITPTLSTRES.
      *       実施日
              03  VIS02RES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              03  VIS02RES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              03  VIS02RES-API-RESULT          PIC X(2).
      *       返却メッセージ
              03  VIS02RES-API-RESULT-MSG      PIC X(100).
      *
              03  VIS02RES-RESKEY              PIC X(100).
      *
      *       来院年月
              03  VIS02RES-VISIT-DATE          PIC X(07).
      *       診療科
              03  VIS02RES-DEP-CODE            PIC X(02).
      *       診療科短縮名称
              03  VIS02RES-DEP-NAME            PIC X(20).
      *       受診履歴情報
              03  VIS02RES-MEDICAL-INF     OCCURS   2000.
      *           患者情報
                  05  VIS02RES-PATIENT-INFOMATION.
      *               患者番号
                      07  VIS02RES-PATIENTID         PIC X(20).
      *               患者氏名
                      07  VIS02RES-NAME              PIC X(100).
      *               患者カナ氏名
                      07  VIS02RES-KANANAME          PIC X(100).
      *               誕生日
                      07  VIS02RES-BIRTHDAY          PIC X(10).
      *               性別
                      07  VIS02RES-SEX               PIC X(1).
      *           来院日
                  05  VIS02RES-VISIT-CALENDAR.
                      07  VIS02RES-VISIT-DAY       PIC X(01)
                                                   OCCURS   31.
      *
