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
      *    ＡＰＩレスポンス診療行為情報取得（受診年月診療内容２）(xml2)
      *                                      COPY (CPMDCGET04V2RES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
        01  XML-MEDICALGET04RES.
            02  MDC04RES-CONTEXT           PIC S9(9)   BINARY.
            02  MDC04RES-OBJECT            PIC X(36).
            02  MDC04RES-MODE              PIC S9(9)   BINARY.
            02  MDC04RES-RECORDNAME        PIC X(128).
            02  MDC04RES-MEDICALRES.
      *       実施日
              03  MDC04RES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              03  MDC04RES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              03  MDC04RES-API-RESULT          PIC X(2).
      *       返却メッセージ
              03  MDC04RES-API-RESULT-MSG      PIC X(100).
      *
              03  MDC04RES-RESKEY              PIC X(100).
      *       患者情報
              03  MDC04RES-PATIENT-INFOMATION.
      *           患者番号
                  05  MDC04RES-PATIENTID         PIC X(20).
      *           患者氏名
                  05  MDC04RES-NAME              PIC X(100).
      *           患者カナ氏名
                  05  MDC04RES-KANANAME          PIC X(100).
      *           生年月日
                  05  MDC04RES-BIRTHDAY          PIC X(10).
      *           性別
                  05  MDC04RES-SEX               PIC X(1).
      *       受診年月
              03  MDC04RES-PERFORM-DATE        PIC X(07).
      *       診療科
              03  MDC04RES-DEP-CODE            PIC X(2).
      *       診療科短縮名称
              03  MDC04RES-DEP-NAME            PIC X(20).
      *       保険組合せ情報
              03  MDC04RES-COMBINATION-INF     OCCURS   5.
      *           保険組合せ番号
                  05  MDC04RES-COMBINATION-NUMBER  PIC X(04).
      *           保険組合せ情報
                  05  MDC04RES-INSURANCE-INFO.
      *               保険の種類
                      07  MDC04RES-MAIN-INSURANCE-CLASS   PIC X(03).
      *               保険の種類名称
                      07  MDC04RES-MAIN-INSURANCE-NAME    PIC X(20).
      *               保険者番号
                      07  MDC04RES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *               記号
                      07  MDC04RES-MAIN-MARK              PIC X(80).
      *               番号
                      07  MDC04RES-MAIN-NUMBER            PIC X(80).
      *               公費情報
                      07  MDC04RES-PUBLIC-INSURANCE-INFO
                                              OCCURS  4 TIMES.
      *                   公費の種類
                          09  MDC04RES-INSURANCE-CLASS     PIC X(03).
      *                   公費の種類名称
                          09  MDC04RES-INSURANCE-NAME      PIC X(20).
      *                   負担者番号
                          09  MDC04RES-PROVIDER-NUMBER     PIC X(08).
      *                   受給者番号
                          09  MDC04RES-RECIPIENT-NUMBER    PIC X(20).
      *       診療内容情報
              03  MDC04RES-MEDICAL-INF         OCCURS   31.
      *           診療日
                  05  MDC04RES-MDC-DAY             PIC X(02).
                  05  MDC04RES-MEDICAL-INF2        OCCURS   40.
      *               診療区分
                      07  MDC04RES-MDC-CLASS           PIC X(02).
      *               診療種別区分
                      07  MDC04RES-MDC-CLASS2          PIC X(03).
      *               診療種別名称
                      07  MDC04RES-MDC-CLASS-NAME      PIC X(50).
      *               剤情報
                      07  MDC04RES-MEDICAL-INF3        OCCURS   100.
      *               剤点数
                          09  MDC04RES-MDC-POINT           PIC X(08).
      *               金額（自費・労災円）
                          09  MDC04RES-MDC-MOENY           PIC X(08).
      *               回数
                          09  MDC04RES-MDC-CLASS-NUMBER    PIC X(03).
      *               剤区分
                          09  MDC04RES-MDC-CODE            PIC X(01).
      *               包括区分（True：包括剤）
                          09  MDC04RES-INCLUSION-CLASS     PIC X(05).
      *
