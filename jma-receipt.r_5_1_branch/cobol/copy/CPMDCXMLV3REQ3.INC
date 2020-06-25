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
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     NACL-多々納  19/01/XX  複数科保険対応
      *****************************************************************
      ******************************************************************
      *    ＡＰＩリクエスト診療行為登録情報 (一体化)
      *                                       COPY (CPMDCXMLV3REQ3.INC)
      *         診療行為内容
      ******************************************************************
       01  XML-MEDICALV3REQ3.
           03  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
           03  MDCREQ-OBJECT           PIC X(36).
           03  MDCREQ-MODE             PIC S9(9)   BINARY.
           03  MDCREQ-RECORDNAME       PIC X(128).
           03  MDCREQ-MEDICALREQ.
      *       リクエスト番号
              05  MDCREQ-REQUEST-NUMBER    PIC X(2).
      *       UID（電子カルテ）
              05  MDCREQ-KARTE-UID         PIC X(36).
      *       基準日付（収納入金日）
              05  MDCREQ-BASE-DATE         PIC X(10).
      *       患者番号
              05  MDCREQ-PATIENTID         PIC X(20).
      *       診療日付
              05  MDCREQ-PERFORM-DATE      PIC X(10).
      *       診療時間
              05  MDCREQ-PERFORM-TIME      PIC X(8).
      *       UID（オルカ）
              05  MDCREQ-ORCA-UID          PIC X(36).
      *H27.12
      *       処理区分
              05  MDCREQ-PATIENT-MODE      PIC X(06).
      *
      *       自動発生剤削除区分（１：削除あり）
              05  MDCREQ-MDC-MODE          PIC X(01).
      *       自動発生剤削除連番
              05  MDCREQ-DEL-NUMBER-G.
                  07  MDCREQ-DEL-NUMBER        PIC X(02)
                                               OCCURS   30.
      *H29.7
      *       登録処理区分（Finish:登録）（リクエスト番号=05)
              05  MDCREQ-REQUEST-MODE      PIC X(06).
      *
      *       入金方法
              05  MDCREQ-IC-CODE           PIC X(02).
      *H29.7
      *       入金取り扱い区分
              05  MDCREQ-IC-REQUEST        PIC X(01).
      *       一括入返金区分
              05  MDCREQ-IC-ALLKBN         PIC X(01).
      *
      *       収納内容
              05  MDCREQ-SYUNOU-AREA.
      *!!!!
      *2019.1
      *複数保険・科対応
                  07  MDCREQ-SYUNOU-OCCURS       OCCURS   15.
      *               保険組合せ番号
                      09  MDCREQ-CD-COMBINATION-NUMBER    PIC X(04).
      *               診療科
                      09  MDCREQ-CD-DEP-CODE      PIC X(02).
      *               入金額（訂正）
                      09  MDCREQ-CD-IC-MONEY      PIC X(10).
      *               調整金額１
                      09  MDCREQ-CD-AD-MONEY1     PIC X(10).
      *               調整金額２
                      09  MDCREQ-CD-AD-MONEY2     PIC X(10).
      *!!!!
      *           調整金額１（単独のみ）
                  07  MDCREQ-AD-MONEY1         PIC X(10).
      *           調整金額２（単独のみ）
                  07  MDCREQ-AD-MONEY2         PIC X(10).
      *           入金額（合計と違う時のみ）
                  07  MDCREQ-IC-MONEY          PIC X(10).
      *           返金額（訂正のみ、充当の返金はゼロ）
                  07  MDCREQ-RE-MONEY          PIC X(10).
      *!!!!
      *       印刷モード 2019/09/19 Added
              05  MDCREQ-PRINT-MODE            PIC X(10).
      *H30.XX
              05  MDCREQ-PRINT-INIT-AREA.
      *           請求書兼領収書・診療費明細書　発行区分
                  07  MDCREQ-HAKKOU-KBN        PIC X(01).
      *!!!!
      *H29.6
      *   帳票印刷指示区分
              05  MDCREQ-PRINT-AREA.
      *           処方せん印刷
                  07  MDCREQ-PRINT-SHOHOU      PIC X(01).
      *           請求書兼領収書印刷
                  07  MDCREQ-PRINT-SEIKYU      PIC X(01).
      *           診療費明細書印刷
                  07  MDCREQ-PRINT-MEISAI      PIC X(01).
      *           薬剤情報印刷
                  07  MDCREQ-PRINT-YAKUJYO     PIC X(01).
      *           お薬手帳印刷
                  07  MDCREQ-PRINT-TECHOU      PIC X(01).
      *           予約票印刷
                  07  MDCREQ-PRINT-YOYAKU      PIC X(01).
      *
