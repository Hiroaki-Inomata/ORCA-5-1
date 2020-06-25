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
      *    ＡＰＩレスポンスリハビリコメント等入力情報 (一体化)
      *                                          COPY (CPMDCXMLV3RES5.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  XML-MEDICALV3RES5.
           03  MDCRES-CONTEXT             PIC S9(9)   BINARY.
           03  MDCRES-OBJECT              PIC X(36).
           03  MDCRES-MODE                PIC S9(9)   BINARY.
           03  MDCRES-RECORDNAME          PIC X(128).
           03  MDCRES-MEDICALRES.
      *        リクエスト番号
               05  MDCRES-REQUEST-NUMBER    PIC X(2).
      *        レスポンス番号
               05  MDCRES-RESPONSE-NUMBER   PIC X(2).
      *        UID（電子カルテ）
               05  MDCRES-KARTE-UID         PIC X(36).
      *        UID（オルカ）
               05  MDCRES-ORCA-UID          PIC X(36).
      *        実施日
               05  MDCRES-INFORMATION-DATE  PIC X(10).
      *        実施時間
               05  MDCRES-INFORMATION-TIME  PIC X(8).
      *        返却コード
               05  MDCRES-API-RESULT        PIC X(03).
      *        返却メッセージ
               05  MDCRES-API-RESULT-MSG    PIC X(100).
      *
               05  MDCRES-RESKEY            PIC X(100).
      *
      *        患者情報
               05  MDCRES-PATIENT-INFOMATION.
                   07  MDCRES-PATIENTID       PIC X(20).
                   07  MDCRES-NAME            PIC X(100).
                   07  MDCRES-KANANAME        PIC X(100).
                   07  MDCRES-BIRTHDAY        PIC X(10).
                   07  MDCRES-SEX             PIC X(1).
      *        リハビリテーション等開始日（リクエスト＝０１)
               05  MDCRES-SANTEIRRK-TBLINF.
                   07  MDCRES-SANTEIRRK-TBL   OCCURS   100.
      *                開始日診療コード
                       09  MDCRES-TPRSCRPT-CODE          PIC X(9).
      *                診療名称
                       09  MDCRES-TPRSCRPT-NAME          PIC X(80).
      *                算定年月
                       09  MDCRES-TPERFORM-YM            PIC X(7).
      *                保険組合せ番号
                       09  MDCRES-TCOMBINATION-NUMBER    PIC X(04).
      *                保険組合せ番号名称
                       09  MDCRES-TCOMBINATION-NAME      PIC X(100).
      *                算定日
                       09  MDCRES-TPERFORM-DATE-G.
                           11  MDCRES-TPERFORM-DAY       PIC X(02)
                                                        OCCURS   31.
      *                登録区分(1：診療行為）
                       09  MDCRES-TOROKU-MODE1           PIC X(01).
      *                コメント区分(1：コメントあり）
                       09  MDCRES-TOROKU-MODE2           PIC X(01).
      *                終了日
                       09  MDCRES-TEND-DATE              PIC X(10).
      *                終了日区分(1：診療行為）
                       09  MDCRES-TOROKU-MODE3           PIC X(01).
      *
      *        開始日選択（リクエスト＝０２〜）
               05  MDCRES-SANTEIRRK-INFO.
      *            処理区分
                   07  MDCRES-SANTEI-MODE           PIC X(6).
      *            開始日診療コード
                   07  MDCRES-PRSCRPT-CODE          PIC X(9).
      *            診療名称
                   07  MDCRES-PRSCRPT-NAME          PIC X(80).
      *            算定年月
                   07  MDCRES-PERFORM-YM            PIC X(07).
      *            保険組合せ番号
                   07  MDCRES-COMBINATION-NUMBER    PIC X(04).
      *            保険組合せ番号名称
                   07  MDCRES-COMBINATION-NAME      PIC X(100).
      *            算定日
                   07  MDCRES-PERFORM-DATE-G.
                       09  MDCRES-PERFORM-DAY       PIC X(02)
                                                   OCCURS   31.
      *            エラーコード
                   07  MDCRES-SAN-ERRCD-TBL        OCCURS   50.
                       09  MDCRES-SAN-ERRCD            PIC X(04).
                       09  MDCRES-SAN-ERRCD-MSG        PIC X(100).
      *
      *        リハビリコメント（リクエスト＝０２〜）
               05  MDCRES-COMMNT-INFO.
      *            算定日
                   07  MDCRES-COMMNT-DAY-TBL        OCCURS   31.
      *                算定日
                       09  MDCRES-COMMNT-DAY            PIC X(02).
      *                終了日
                       09  MDCRES-END-DATE              PIC X(10).
      *                コメント１
                       09  MDCRES-COMMNT-TBL-G.
                           11  MDCRES-COMMNT-TBL        OCCURS   50.
                               13  MDCRES-COMMNT        PIC X(80).
                               13  MDCRES-COM-ENDDATE   PIC X(10).
      *                エラーコード
                       09  MDCRES-COM-ERRCD            PIC X(04).
                       09  MDCRES-COM-ERRCD-MSG        PIC X(100).
      *
      *        リハビリテーション等開始日未登録コード一覧
               05  MDCRES-STARTSRYCD-INF.
                   07  MDCRES-STARTSRYCD-TBL   OCCURS   100.
      *                開始日診療コード
                       09  MDCRES-STARTSRYCD-CODE          PIC X(9).
      *                診療名称
                       09  MDCRES-STARTSRYCD-NAME          PIC X(80).
      *                適応開始日
                       09  MDCRES-STARTSRYCD-DATE          PIC X(10).
      *
