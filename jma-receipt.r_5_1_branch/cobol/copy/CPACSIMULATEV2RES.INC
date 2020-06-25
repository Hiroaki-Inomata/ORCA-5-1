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
      *    ＡＰＩレスポンス負担金計算情報
      *                                 COPY (CPACSIMULATEV2RES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
        01  XML-ACSIMULATERES.
            03  ACSRES-CONTEXT             PIC S9(9)   BINARY.
            03  ACSRES-OBJECT              PIC X(36).
            03  ACSRES-MODE                PIC S9(9)   BINARY.
            03  ACSRES-RECORDNAME          PIC X(128).
            03  ACSRES-MEDICALRES.
              05  ACSRES-INFORMATION-DATE  PIC X(10).
              05  ACSRES-INFORMATION-TIME  PIC X(8).
              05  ACSRES-API-RESULT        PIC X(2).
              05  ACSRES-API-RESULT-MSG    PIC X(100).
              05  ACSRES-RESKEY            PIC X(100).
              05  ACSRES-PERFORM-DATE      PIC X(10).
      *       時間外区分（診察料）
              05  ACSRES-TIME-CLASS        PIC X(1).
      *       診療科
              05  ACSRES-APPOINT-DEP-CODE  PIC X(2).
              05  ACSRES-APPOINT-DEP-NAME  PIC X(80).
      *       患者情報
              05  ACSRES-PATIENT-INFOMATION.
                07  ACSRES-PATIENTID       PIC X(20).
                07  ACSRES-NAME            PIC X(100).
                07  ACSRES-KANANAME        PIC X(100).
                07  ACSRES-BIRTHDAY        PIC X(10).
                07  ACSRES-SEX             PIC X(01).
                07  ACSRES-RATE            PIC X(12).
      *         保険情報
                07  ACSRES-INSURANCE-G.
      *             保険組合せ番号
                    09  ACSRES-COMBINATION-NUMBER  PIC X(4).
      *             保険の種類
                    09  ACSRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *             保険者番号
                    09  ACSRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             保険の種類名称
                    09  ACSRES-MAIN-INSURANCE-NAME    PIC X(20).
      *             記号
                    09  ACSRES-MAIN-MARK              PIC X(80).
      *             番号
                    09  ACSRES-MAIN-NUMBER            PIC X(80).
      *             継続区分
                    09  ACSRES-MAIN-CONTINUATION      PIC X(01).
      *             補助区分
                    09  ACSRES-MAIN-ASSISTANCE        PIC X(01).
      *             補助区分名称
                    09  ACSRES-MAIN-ASSISTANCE-NAME   PIC X(10).
      *             本人家族区分
                    09  ACSRES-MAIN-FAMILY            PIC X(01).
      *             被保険者名
                    09  ACSRES-MAIN-POLICYHOLDER      PIC X(100).
      *             有効開始日
                    09  ACSRES-MAIN-START-DATE        PIC X(10).
      *             有効終了日
                    09  ACSRES-MAIN-END-DATE          PIC X(10).
      *             公費情報
                    09  ACSRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *               公費の種類
                      11  ACSRES-INSURANCE-CLASS         PIC X(03).
      *               公費の種類名称
                      11  ACSRES-INSURANCE-NAME          PIC X(20).
      *               負担者番号
                      11  ACSRES-PROVIDER-NUMBER         PIC X(08).
      *               受給者番号
                      11  ACSRES-RECIPIENT-NUMBER        PIC X(20).
      *               入院−負担率（割）
                      11  ACSRES-RATE-ADMISSION          PIC X(04).
      *               入院−固定額
                      11  ACSRES-MONEY-ADMISSION         PIC X(06).
      *               外来−負担率（割）
                      11  ACSRES-RATE-OUTPATIENT         PIC X(04).
      *               外来−固定額
                      11  ACSRES-MONEY-OUTPATIENT        PIC X(06).
      *               有効開始日
                      11  ACSRES-INSURANCE-START-DATE    PIC X(10).
      *               有効終了日
                      11  ACSRES-INSURANCE-END-DATE      PIC X(10).
      *
      *         負担額情報
                07  ACSRES-SYUNOU-ARA.
      *             請求金額
                    09  ACSRES-SKYMONEY            PIC  X(10).
      *             保険適用金額
                    09  ACSRES-HKNTEKMONEY         PIC  X(10).
      *             自費金額
                    09  ACSRES-JIHIMONEY           PIC  X(10).
      *             薬剤一部負担金
                    09  ACSRES-YKZ-FTN             PIC  X(10).
      *             老人一部負担金
                    09  ACSRES-RJN-FTN             PIC  X(10).
      *             公費一部負担金
                    09  ACSRES-KOH-FTN             PIC  X(10).
      *             労災金額合計
                    09  ACSRES-RSI-TOTALMONEY      PIC  X(10).
      *         請求点数情報
                07  ACSRES-SYUNOU-ARA.
      *             保険合計点数
                    09  ACSRES-TOTAL-HKNTEN        PIC  X(10).
      *             保険点数
                    09  ACSRES-HKNTEN-G.
                        11  ACSRES-HKNTEN-TBL      OCCURS   16.
                            13  ACSRES-HKNTEN-MEI      PIC  X(30).
                            13  ACSRES-HKNTEN          PIC  X(10).
      *
