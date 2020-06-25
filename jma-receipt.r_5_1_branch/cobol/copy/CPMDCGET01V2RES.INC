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
      *    ＡＰＩレスポンス診療行為情報取得（受診履歴）(xml2)
      *                                      COPY (CPMDCGET01V2RES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  15/12/14  伝票番号追加対応
      *****************************************************************
        01  XML-MEDICALGET01RES.
            02  MDC01RES-CONTEXT           PIC S9(9)   BINARY.
            02  MDC01RES-OBJECT            PIC X(36).
            02  MDC01RES-MODE              PIC S9(9)   BINARY.
            02  MDC01RES-RECORDNAME        PIC X(128).
            02  MDC01RES-MEDICALRES.
      *       実施日
              03  MDC01RES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              03  MDC01RES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              03  MDC01RES-API-RESULT          PIC X(2).
      *       返却メッセージ
              03  MDC01RES-API-RESULT-MSG      PIC X(100).
      *
              03  MDC01RES-RESKEY              PIC X(100).
      *       患者情報
              03  MDC01RES-PATIENT-INFOMATION.
      *           患者番号
                  05  MDC01RES-PATIENTID         PIC X(20).
      *           患者氏名
                  05  MDC01RES-NAME              PIC X(100).
      *           患者カナ氏名
                  05  MDC01RES-KANANAME          PIC X(100).
      *           誕生日
                  05  MDC01RES-BIRTHDAY          PIC X(10).
      *           性別
                  05  MDC01RES-SEX               PIC X(1).
      *       受診履歴情報
              03  MDC01RES-MEDICAL-INF     OCCURS   150.
      *           受診日付
                  05  MDC01RES-PERFORM-DATE        PIC X(10).
      *           診療科
                  05  MDC01RES-DEP-CODE            PIC X(02).
      *           診療科短縮名称
                  05  MDC01RES-DEP-NAME            PIC X(20).
      *           連番
                  05  MDC01RES-NUMBER              PIC X(01).
      *           保険組合せ番号
                  05  MDC01RES-COMBINATION-NUMBER  PIC X(04).
      *           保険組合せ情報
                  05  MDC01RES-INSURANCE-INFO.
      *             保険の種類
                    06  MDC01RES-MAIN-INSURANCE-CLASS   PIC X(03).
      *             保険の種類名称
                    06  MDC01RES-MAIN-INSURANCE-NAME    PIC X(20).
      *             保険者番号
                    06  MDC01RES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             記号
                    06  MDC01RES-MAIN-MARK              PIC X(80).
      *             番号
                    06  MDC01RES-MAIN-NUMBER            PIC X(80).
      *             公費情報
                    06  MDC01RES-PUBLIC-INSURANCE-INFO
                                              OCCURS  4 TIMES.
      *               公費の種類
                      07  MDC01RES-INSURANCE-CLASS         PIC X(03).
      *               公費の種類名称
                      07  MDC01RES-INSURANCE-NAME          PIC X(20).
      *               負担者番号
                      07  MDC01RES-PROVIDER-NUMBER         PIC X(08).
      *               受給者番号
                      07  MDC01RES-RECIPIENT-NUMBER        PIC X(20).
      *           伝票番号
                  05  MDC01RES-INVOICD-NUMBER    PIC X(07).
      *
