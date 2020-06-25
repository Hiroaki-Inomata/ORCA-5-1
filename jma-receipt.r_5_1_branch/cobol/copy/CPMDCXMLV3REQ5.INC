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
      *    HAORIリクエスト　リハビリコメント等入力情報 (一体化)
      *                                     COPY (CPMDCXMLV3REQ5.INC)
      ******************************************************************
       01  XML-MEDICALV3REQ5.
           03  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
           03  MDCREQ-OBJECT           PIC X(36).
           03  MDCREQ-MODE             PIC S9(9)   BINARY.
           03  MDCREQ-RECORDNAME       PIC X(128).
           03  MDCREQ-MEDICALREQ.
      *       リクエスト番号
              05  MDCREQ-REQUEST-NUMBER    PIC X(2).
      *       UID（電子カルテ）
              05  MDCREQ-KARTE-UID         PIC X(36).
      *       基準日付
      *****   05  MDCREQ-BASE-DATE         PIC X(10).
      *       患者番号
              05  MDCREQ-PATIENTID         PIC X(20).
      *       UID（オルカ）
              05  MDCREQ-ORCA-UID          PIC X(36).
      *
      *       開始日選択・更新内容（リクエスト＝０２〜）
              05  MDCREQ-SANTEIRRK-INFO.
      *           処理区分（NEW,MODIFY,DELETE,空白)
                  07  MDCREQ-SANTEI-MODE           PIC X(6).
      *           開始日診療コード
                  07  MDCREQ-PRSCRPT-CODE          PIC X(9).
      *           診療名称
                  07  MDCREQ-PRSCRPT-NAME          PIC X(80).
      *           算定年月
                  07  MDCREQ-PERFORM-YM            PIC X(07).
      *           保険組合せ番号
                  07  MDCREQ-COMBINATION-NUMBER    PIC X(04).
      *           算定日
                  07  MDCREQ-PERFORM-DATE-G.
                      09  MDCREQ-PERFORM-DAY       PIC X(02)
                                                   OCCURS   31.
      *
      *       コメント（リクエスト＝０３）
              05  MDCREQ-COMMNT-INFO.
      *           処理区分（MODIFY)(開始日選択の処理区分＝空白時のみ）
                  07  MDCREQ-COMMNT-MODE           PIC X(6).
      *           算定日
                  07  MDCREQ-COMMNT-DAY-TBL        OCCURS   31.
      *               算定日
                      09  MDCREQ-COMMNT-DAY            PIC X(02).
      *               終了日
                      09  MDCREQ-END-DATE              PIC X(10).
      *               コメント１
                      09  MDCREQ-COMMNT-TBL-G.
                          11  MDCREQ-COMMNT-TBL        OCCURS   50.
                              13  MDCREQ-COMMNT        PIC X(80).
                              13  MDCREQ-COM-END-DATE  PIC X(10).
      *
      *
