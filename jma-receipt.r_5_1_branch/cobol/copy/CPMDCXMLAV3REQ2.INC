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
      *    ＡＰＩリクエスト入院診療行為チェック情報 (一体化)
      *                                       COPY (CPMDCXMLAV3REQ2.INC)
      *         診療行為内容（入院）
      ******************************************************************
       01  XML-MEDICALAV3REQ2.
           03  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
           03  MDCREQ-OBJECT           PIC X(36).
           03  MDCREQ-MODE             PIC S9(9)   BINARY.
           03  MDCREQ-RECORDNAME       PIC X(128).
           03  MDCREQ-MEDICALREQ.
      *       リクエスト番号
              05  MDCREQ-REQUEST-NUMBER    PIC X(2).
      *       UID（電子カルテ）
              05  MDCREQ-KARTE-UID         PIC X(36).
      *       患者番号
              05  MDCREQ-PATIENTID         PIC X(20).
      *       診療日付
              05  MDCREQ-PERFORM-DATE      PIC X(10).
      *       診療時間
              05  MDCREQ-PERFORM-TIME      PIC X(8).
      *       UID（オルカ）
              05  MDCREQ-ORCA-UID          PIC X(36).
      *       処理区分
              05  MDCREQ-PATIENT-MODE      PIC X(06).
      *
      *       同日再入院分
              05  MDCREQ-DOUJITSUKBN        PIC X(1).
      *       訂正前診療科・保険組合せ
              05  MDCREQ-MOD-DEP-CODE       PIC X(2).
              05  MDCREQ-MOD-COMBINATION    PIC X(4).
      *
      *       確認処理区分（１：最初の確認メッセージから）
              05  MDCREQ-SELECT-MODE        PIC X(01).
      *       確認メッセージ返答
              05  MDCREQ-SELECT-ANSWER      PIC X(03).
      *
      *
              05  MDCREQ-DIAGNOSIS-INFO.
      *         診療科
                07  MDCREQ-APPOINT-DEP-CODE    PIC X(2).
      *         ドクターコード
      ****      07  MDCREQ-APPOINT-DR-CODE     PIC X(5).
      *         保険情報
                07  MDCREQ-INSURANCE-INFO.
      *           保険組合せ番号
                  09  MDCREQ-COMBINATION-NUMBER      PIC X(04).
      *           保険の種類
                  09  MDCREQ-MAIN-INSURANCE-CLASS    PIC X(03).
      *           保険者番号
                  09  MDCREQ-MAIN-INSURANCE-NUMBER   PIC X(08).
      *           保険の種類名称
                  09  MDCREQ-MAIN-INSURANCE-NAME     PIC X(20).
      *           記号
                  09  MDCREQ-MAIN-MARK               PIC X(80).
      *           番号
                  09  MDCREQ-MAIN-NUMBER             PIC X(80).
      *           継続区分
                  09  MDCREQ-MAIN-CONTINUATION       PIC X(01).
      *           補助区分
                  09  MDCREQ-MAIN-ASSISTANCE         PIC X(01).
      *           本人家族区分
                  09  MDCREQ-MAIN-FAMILY             PIC X(01).
      *           被保険者名
      ****        09  MDCREQ-MAIN-POLICYHOLDER       PIC X(100).
      *           有効開始日
                  09  MDCREQ-MAIN-START-DATE         PIC X(10).
      *           有効終了日
                  09  MDCREQ-MAIN-END-DATE           PIC X(10).
      *           公費情報
                  09  MDCREQ-PUBLIC-INSURANCE-INF  OCCURS  4 TIMES.
      *             公費の種類
                    11  MDCREQ-INSURANCE-CLASS       PIC X(03).
      *             公費の種類名称
                    11  MDCREQ-INSURANCE-NAME        PIC X(20).
      *             負担者番号
                    11  MDCREQ-PROVIDER-NUMBER       PIC X(08).
      *             受給者番号
                    11  MDCREQ-RECIPIENT-NUMBER      PIC X(20).
      *             有効開始日
                    11  MDCREQ-INSURANCE-START-DATE  PIC X(10).
      *             有効終了日
                    11  MDCREQ-INSURANCE-END-DATE    PIC X(10).
      *
      *         時間外区分
                07  MDCREQ-OFFTIME             PIC X(01).
      *         診療情報
                07  MDCREQ-MDC-INFO-G.
                  09  MDCREQ-MDC-INFO          OCCURS   110.
      *             診療種別区分
                    11  MDCREQ-MDC-CLASS             PIC X(3).
      *             診療種別名称
                    11  MDCREQ-MDC-CLASS-NAME        PIC X(40).
      *             回数
                    11  MDCREQ-MDC-CLASS-NUMBER      PIC X(03).
      *
      *             算定日
                    11  MDCREQ-MDC-DAY-G.
                        13  MDCREQ-MDC-DAY          PIC X(01)
                                                    OCCURS   31.
      *
                    11  MDCREQ-PRESCRIPTION-INFO    OCCURS   50.
      *                 診療コード
                        13  MDCREQ-PRSCRPT-CODE          PIC X(9).
      *                 診療名称
                        13  MDCREQ-PRSCRPT-NAME          PIC X(140).
      *                 数量
                        13  MDCREQ-PRSCRPT-NUMBER        PIC X(11).
      *                 自費金額
                        13  MDCREQ-PRSCRPT-MONEY         PIC X(07).
      *                 コメント埋め込み値
                        13  MDCREQ-PRSCRPT-ATAI-G.
                            15  MDCREQ-PRSCRPT-ATAI      PIC X(10)
                                                         OCCURS   5.
      *                 フィルム分画数
                        13  MDCREQ-PRSCRPT-FILMNUM       PIC X(03).
      *                 継続コメント指示区分
                        13  MDCREQ-PRSCRPT-CONTKBN       PIC X(01).
      *                 内服種類数指示区分
                        13  MDCREQ-PRSCRPT-NAIFKBN       PIC X(01).
      *                 自動算定なし区分（在宅療養実績加算）
                        13  MDCREQ-PRSCRPT-NOAUTO        PIC X(03).
      *
      *                 自動区分
                        13  MDCREQ-PRSCRPT-AUTOKBN       PIC X(01).
      *
