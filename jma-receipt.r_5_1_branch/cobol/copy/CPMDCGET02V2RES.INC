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
      *    ＡＰＩレスポンス診療行為情報取得（受診日診療行為内容）(xml2)
      *                                      COPY (CPMDCGET02V2RES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     NACL-多々納  17/08/XX  伝票番号追加対応
      * 05.00.00     NACL-多々納  17/11/XX  点数など追加対応
      *****************************************************************
        01  XML-MEDICALGET02RES.
            02  MDC02RES-CONTEXT           PIC S9(9)   BINARY.
            02  MDC02RES-OBJECT            PIC X(36).
            02  MDC02RES-MODE              PIC S9(9)   BINARY.
            02  MDC02RES-RECORDNAME        PIC X(128).
            02  MDC02RES-MEDICALRES.
      *       実施日
              03  MDC02RES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              03  MDC02RES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              03  MDC02RES-API-RESULT          PIC X(2).
      *       返却メッセージ
              03  MDC02RES-API-RESULT-MSG      PIC X(100).
      *
              03  MDC02RES-RESKEY              PIC X(100).
      *       患者情報
              03  MDC02RES-PATIENT-INFOMATION.
      *           患者番号
                  05  MDC02RES-PATIENTID         PIC X(20).
      *           患者氏名
                  05  MDC02RES-NAME              PIC X(100).
      *           患者カナ氏名
                  05  MDC02RES-KANANAME          PIC X(100).
      *           誕生日
                  05  MDC02RES-BIRTHDAY          PIC X(10).
      *           性別
                  05  MDC02RES-SEX               PIC X(1).
      *       受診日付
              03  MDC02RES-PERFORM-DATE        PIC X(10).
      *       診療科
              03  MDC02RES-DEP-CODE            PIC X(2).
      *       診療科短縮名称
              03  MDC02RES-DEP-NAME            PIC X(20).
      *       連番
              03  MDC02RES-NUMBER              PIC X(01).
      *       診療内容情報
              03  MDC02RES-MEDICAL-INF     OCCURS   5.
      *           保険組合せ番号
                  05  MDC02RES-COMBINATION-NUMBER  PIC X(04).
      *           保険組合せ情報
                  05  MDC02RES-INSURANCE-INFO.
      *               保険の種類
                      07  MDC02RES-MAIN-INSURANCE-CLASS   PIC X(03).
      *               保険の種類名称
                      07  MDC02RES-MAIN-INSURANCE-NAME    PIC X(20).
      *               保険者番号
                      07  MDC02RES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *               記号
                      07  MDC02RES-MAIN-MARK              PIC X(80).
      *               番号
                      07  MDC02RES-MAIN-NUMBER            PIC X(80).
      *               公費情報
                      07  MDC02RES-PUBLIC-INSURANCE-INFO
                                              OCCURS  4 TIMES.
      *                   公費の種類
                          09  MDC02RES-INSURANCE-CLASS     PIC X(03).
      *                   公費の種類名称
                          09  MDC02RES-INSURANCE-NAME      PIC X(20).
      *                   負担者番号
                          09  MDC02RES-PROVIDER-NUMBER     PIC X(08).
      *                   受給者番号
                          09  MDC02RES-RECIPIENT-NUMBER    PIC X(20).
      *           伝票番号
                  05  MDC02RES-INVOICD-NUMBER    PIC X(07).
      *           診療行為内容
                  05  MDC02RES-MDC-INFO        OCCURS   135.
      *               診療種別区分
                      07  MDC02RES-MDC-CLASS           PIC X(03).
      *               診療種別区分名称
                      07  MDC02RES-MDC-CLASS-NAME      PIC X(50).
      *               回数
                      07  MDC02RES-MDC-CLASS-NUMBER    PIC X(03).
      *               点数
                      07  MDC02RES-MDC-POINT           PIC X(08).
      *               金額（自費・労災円）
                      07  MDC02RES-MDC-MOENY           PIC X(08).
      *               剤区分
                      07  MDC02RES-MDC-CODE            PIC X(01).
      *               包括区分（True：包括剤）
                      07  MDC02RES-INCLUSION-CLASS     PIC X(05).
      *H29.11
      *               包括検査の検査数
                      07  MDC02RES-HKTKNSA-CNT         PIC X(02).
      *               剤内容
                      07  MDC02RES-PRESCRIPTION-INFO   OCCURS  50.
      *                   診療コード
                          09  MDC02RES-PRSCRPT-CODE         PIC X(9).
      *                   診療名称（コメント）
                          09  MDC02RES-PRSCRPT-NAME         PIC X(140).
      *                   数量
                          09  MDC02RES-PRSCRPT-NUMBER       PIC X(11).
      *                   単位
                          09  MDC02RES-UNIT-CODE            PIC X(03).
      *                   単位名称
                          09  MDC02RES-UNIT-NAME            PIC X(24).
      *H29.11
      *                   点数マスタの点数識別
                          09  MDC02RES-TENSIKIBETU          PIC X(01).
      *                   点数マスタの点数
                          09  MDC02RES-TEN                  PIC X(10).
      *
