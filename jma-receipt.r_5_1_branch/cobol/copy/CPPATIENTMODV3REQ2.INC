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
      *    ＡＰＩ患者登録情報（保険公費情報）(一体化　V3)
      *                                   COPY (CPPATIENTMODV3REQ2.INC)
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
      *       基準日付（システム日付）
              03  PTMODREQ-BASE-DATE        PIC X(10).
      *       確認メッセージ返答
              03  PTMODREQ-SELECT-ANSWER    PIC X(03).
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
      *       保険情報
              03  PTMODREQ-INSURANCE-INFO-G.
                  05  PTMODREQ-INSURANCE-INFO     OCCURS   40.
      *             処理区分
                    07  PTMODREQ-MAIN-MODE               PIC X(06).
      *             保険ＩＤ
                    07  PTMODREQ-MAIN-INSURANCE-ID       PIC X(10).
      *             保険の種類
                    07  PTMODREQ-MAIN-INSURANCE-CLASS    PIC X(03).
      *             保険者番号
                    07  PTMODREQ-MAIN-INSURANCE-NUMBER   PIC X(08).
      *             保険の種類名称
                    07  PTMODREQ-MAIN-INSURANCE-NAME     PIC X(20).
      *             記号
                    07  PTMODREQ-MAIN-MARK               PIC X(80).
      *             番号
                    07  PTMODREQ-MAIN-NUMBER             PIC X(80).
      *             枝番
                    07  PTMODREQ-MAIN-EDABAN             PIC X(02).
      *             継続区分
                    07  PTMODREQ-MAIN-CONTINUATION       PIC X(01).
      *             補助区分
                    07  PTMODREQ-MAIN-ASSISTANCE         PIC X(01).
      *             本人家族区分
                    07  PTMODREQ-MAIN-FAMILY             PIC X(01).
      *             被保険者名
                    07  PTMODREQ-MAIN-POLICYHOLDER       PIC X(100).
      *             有効開始日
                    07  PTMODREQ-MAIN-START-DATE         PIC X(10).
      *             有効終了日
                    07  PTMODREQ-MAIN-END-DATE           PIC X(10).
      *             資格取得日
                    07  PTMODREQ-MAIN-GET-DATE           PIC X(10).
      *             確認日付
                    07  PTMODREQ-MAIN-CHECK-DATE         PIC X(10).
      *             高齢者負担区分
                    07  PTMODREQ-RATE-CLASS              PIC X(01).
      *       公費情報
              03  PTMODREQ-PUBLIC-INSURANCE-INFG.
                  05  PTMODREQ-PUBLIC-INSURANCE-INF  OCCURS  60 TIMES.
      *             処理区分
                    07  PTMODREQ-INSURANCE-MODE        PIC X(06).
      *             公費ＩＤ
                    07  PTMODREQ-INSURANCE-ID          PIC X(10).
      *             公費の種類
                    07  PTMODREQ-INSURANCE-CLASS       PIC X(03).
      *             公費の種類名称
                    07  PTMODREQ-INSURANCE-NAME        PIC X(20).
      *             負担者番号
                    07  PTMODREQ-PROVIDER-NUMBER       PIC X(08).
      *             受給者番号
                    07  PTMODREQ-RECIPIENT-NUMBER      PIC X(20).
      *             有効開始日
                    07  PTMODREQ-INSURANCE-START-DATE  PIC X(10).
      *             有効終了日
                    07  PTMODREQ-INSURANCE-END-DATE    PIC X(10).
      *             確認日付
                    07  PTMODREQ-INSURANCE-CHECK-DATE  PIC X(10).
      *
