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
      *    ＡＰＩ患者登録情報（公費負担額）(一体化　V3)
      *                                   COPY (CPPATIENTMODV3REQ5.INC)
      ******************************************************************
        01  XML-PATIENTMODREQ.
            02  PTMODREQ-CONTEXT           PIC S9(9)   BINARY.
            02  PTMODREQ-OBJECT            PIC X(36).
            02  PTMODREQ-MODE              PIC S9(9)   BINARY.
            02  PTMODREQ-RECORDNAME        PIC X(128).
            02  PTMODREQ-MEDICALREQ.
      *       リクエスト番号
              03  PTMODREQ-REQUEST-NUMBER   PIC X(2).
      *       UID（電子カルテ）
              03  PTMODREQ-KARTE-UID        PIC X(36).
      *       UID（オルカ）
              03  PTMODREQ-ORCA-UID         PIC X(36).
      *       継続区分
              03  PTMODREQ-CONT-MODE        PIC X(05).
      *       患者情報
              03  PTMODREQ-PTINF-INFO.
      *         患者番号
                05  PTMODREQ-PATIENTID        PIC X(20).
      *         患者氏名
                05  PTMODREQ-NAME             PIC X(100).
      *         患者カナ氏名
                05  PTMODREQ-KANANAME         PIC X(100).
      *         生年月日
                05  PTMODREQ-BIRTHDAY         PIC X(10).
      *         性別
                05  PTMODREQ-SEX              PIC X(01).
      *
      * リクエスト＝０２から０５
      *       公費選択情報
              03  PTMODREQ-KOHFTN-SELECT-G.
      *          公費ＩＤ
                 05  PTMODREQ-INSURANCE-ID         PIC X(10).
      *          公費の種類
                 05  PTMODREQ-INSURANCE-CLASS      PIC X(03).
      *          受給者番号
                 05  PTMODREQ-RECIPIENT-NUMBER      PIC X(20).
      *          有効開始日
                 05  PTMODREQ-INSURANCE-START-DATE  PIC X(10).
      *
      * リクエスト＝０３
      *       公費負担額情報
              03  PTMODREQ-PTKOHFTN-INFO-G.
      *          処理区分（MODIFY,DELETE)
                 05  PTMODREQ-PTKOHFTN-MODE        PIC X(06).
                 05  PTMODREQ-PTKOHFTN-INFO        OCCURS   300.
      *            行処理区分（DELETE,NEW)
                   07  PTMODREQ-PTKOHFTN-LMODE         PIC X(06).
      *            連番（レスポンス内容）
                   07  PTMODREQ-PTKOHFTN-RENNUM        PIC X(03).
      *            開始年月日
                   07  PTMODREQ-PTKOHFTN-TEKSTYMD      PIC X(10).
      *            終了年月日
                   07  PTMODREQ-PTKOHFTN-TEKEDYMD      PIC X(10).
      *            外来上限額
                   07  PTMODREQ-PTKOHFTN-GAIJGNGAK     PIC X(07).
      *            入院上限額
                   07  PTMODREQ-PTKOHFTN-NYUJGNGAK     PIC X(07).
      *
      * リクエスト＝０４、０５
      *       公費負担額選択情報
              03  PTMODREQ-FTNETC-SELECT-G.
      *           連番
                  05  PTMODREQ-FTN-SEL-RENNUM        PIC X(03).
      *           開始年月日
                  05  PTMODREQ-FTN-SEL-TEKSTYMD      PIC X(10).
      *
      * リクエスト＝０５
      *       他一部負担額
              03  PTMODREQ-FTNETC-INFO-G.
      *          処理区分（MODIFY(NEW),DELETE)
                 05  PTMODREQ-FTNETC-MODE        PIC X(06).
                 05  PTMODREQ-FTNETC-INFO        OCCURS   100.
      *              年月日
                     07  PTMODREQ-FTNETC-SRYYMD      PIC  X(10).
      *              入外区分
                     07  PTMODREQ-FTNETC-NYUGAIKBN   PIC  X(01).
      *              負担金額
                     07  PTMODREQ-FTNETC-FTNMONEY    PIC  X(07).
      *
      *
