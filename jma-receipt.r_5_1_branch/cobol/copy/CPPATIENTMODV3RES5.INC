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
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      ******************************************************************
      *    ＡＰＩ患者登録情報（公費負担額情報）(一体化　V3)
      *                                   COPY (CPPATIENTMODV3RES5.INC)
      ******************************************************************
       01  XML-PATIENTMODRES.
           02  PTMODRES-CONTEXT           PIC S9(9)   BINARY.
           02  PTMODRES-OBJECT            PIC X(36).
           02  PTMODRES-MODE              PIC S9(9)   BINARY.
           02  PTMODRES-RECORDNAME        PIC X(128).
           02  PTMODRES-APPOINTRES.
      *      リクエスト番号
             03  PTMODRES-REQUEST-NUMBER    PIC X(2).
      *      レスポンス番号
             03  PTMODRES-RESPONSE-NUMBER   PIC X(2).
      *      UID（電子カルテ）
             03  PTMODRES-KARTE-UID         PIC X(36).
      *      UID（オルカ）
             03  PTMODRES-ORCA-UID          PIC X(36).
      *      継続区分
             03  PTMODRES-CONT-MODE         PIC X(05).
      *      実施日
             03  PTMODRES-INFORMATION-DATE  PIC X(10).
      *      実施時間
             03  PTMODRES-INFORMATION-TIME  PIC X(8).
      *      返却コード
             03  PTMODRES-API-RESULT        PIC X(3).
      *      返却メッセージ
             03  PTMODRES-API-RESULT-MSG    PIC X(100).
      *
             03  PTMODRES-RESKEY            PIC X(100).
      *      患者番号
             03  PTMODRES-PATIENTID         PIC X(20).
      *      患者情報
             03  PTMODRES-PTINF-INFO.
      *        患者氏名
               05  PTMODRES-NAME             PIC X(100).
      *        患者カナ氏名
               05  PTMODRES-KANANAME         PIC X(100).
      *        生年月日
               05  PTMODRES-BIRTHDAY         PIC X(10).
      *        性別
               05  PTMODRES-SEX              PIC X(01).
      *
      * リクエスト＝０１
      *       対象公費一覧
              03  PTMODRES-PUBLIC-INSURANCE-INFG.
                 05  PTMODRES-PUBLIC-INSURANCE-INF  OCCURS  60 TIMES.
      *            公費ＩＤ
                   07  PTMODRES-INSURANCE-ID          PIC X(10).
      *            公費の種類
                   07  PTMODRES-INSURANCE-CLASS       PIC X(03).
      *            公費の種類名称
                   07  PTMODRES-INSURANCE-NAME        PIC X(20).
      *            受給者番号
                   07  PTMODRES-RECIPIENT-NUMBER      PIC X(20).
      *            有効開始日
                   07  PTMODRES-INSURANCE-START-DATE  PIC X(10).
      *            有効終了日
                   07  PTMODRES-INSURANCE-END-DATE    PIC X(10).
      *
      * リクエスト＝０２、０３
      *       公費負担額情報
              03  PTMODRES-KOHFTN-INFO-G.
      *          公費ＩＤ
                 05  PTMODRES-SEL-INSURANCE-ID      PIC X(10).
      *          公費の種類
                 05  PTMODRES-SEL-INSURANCE-CLASS   PIC X(03).
      *          公費の種類
                 05  PTMODRES-SEL-INSURANCE-NAME    PIC X(20).
      *          受給者番号
                 05  PTMODRES-SEL-RECIPIENT-NUMBER  PIC X(20).
      *          有効開始日
                 05  PTMODRES-SEL-INSURANCE-START   PIC X(10).
      *          有効終了日
                 05  PTMODRES-SEL-INSURANCE-END     PIC X(10).
      *          負担額１名称
                 05  PTMODRES-SEL-KOHFTN-NAME1       PIC X(20).
      *          負担額２名称
                 05  PTMODRES-SEL-KOHFTN-NAME2       PIC X(20).
      *          公費状態（０：上限額、１：他一部負担有、２：負担割合）
                 05  PTMODRES-SEL-KOHFTN-CLASS       PIC X(01).
      *
                 05  PTMODRES-PTKOHFTN-INFO        OCCURS   300.
      *            行処理区分
                   07  PTMODRES-PTKOHFTN-LMODE         PIC X(06).
      *            連番
                   07  PTMODRES-PTKOHFTN-RENNUM        PIC X(03).
      *            開始年月日
                   07  PTMODRES-PTKOHFTN-TEKSTYMD      PIC X(10).
      *            終了年月日
                   07  PTMODRES-PTKOHFTN-TEKEDYMD      PIC X(10).
      *            外来上限額
                   07  PTMODRES-PTKOHFTN-GAIJGNGAK     PIC X(07).
      *            入院上限額
                   07  PTMODRES-PTKOHFTN-NYUJGNGAK     PIC X(07).
      *            公費他一部負担額設定有（１：有）
                   07  PTMODRES-PTKOHFTN-ETC-KBN       PIC X(01).
      *            エラーコード
                   07  PTMODRES-PTKOHFTN-ERRCD         PIC X(04).
                   07  PTMODRES-PTKOHFTN-ERRCD-MSG     PIC X(100).
      *
      * リクエスト＝０４、０５
      *       公費他一部負担額
              03  PTMODRES-PTKOHFTNETC-INFO-G.
      *          選択連番
                 05  PTMODRES-FTN-SEL-RENNUM        PIC X(03).
      *          選択開始年月日
                 05  PTMODRES-FTN-SEL-TEKSTYMD      PIC X(10).
      *
      *          期間開始年月日
                 05  PTMODRES-FTNETC-STRYMD         PIC X(10).
      *          期間終了年月日
                 05  PTMODRES-FTNETC-ENDYMD         PIC X(10).
      *          入外上限額
                 05  PTMODRES-FTNETC-GAIJGNGAK     PIC X(07).
      *          他一部負担額累計
                 05  PTMODRES-FTNETC-NYUJGNGAK     PIC X(07).
      *          自院集計額（収納合計額）
                 05  PTMODRES-FTNETC-SYUTOTAL      PIC X(07).
      *
                 05  PTMODRES-FTNETC-INFO-G2.
      *          他医院負担額
                     07  PTMODRES-FTNETC-INFO     OCCURS   100.
      *                 年月日
                        09  PTMODRES-FTNETC-SRYYMD      PIC  X(10).
      *                 入外区分
                        09  PTMODRES-FTNETC-NYUGAIKBN   PIC  X(01).
      *                 負担金額
                        09  PTMODRES-FTNETC-FTNMONEY    PIC  X(07).
      *                 エラーコード
                        09  PTMODRES-FTNETC-ERRCD       PIC X(04).
                        09  PTMODRES-FTNETC-ERRCD-MSG   PIC X(100).
      *
