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
      *****************************************************************
      ******************************************************************
      *    ＡＰＩレスポンス排他制御一括解除情報  (一体化)
      *                                      COPY (CPMDCXMLV3RES7.INC)
      ******************************************************************
       01  XML-MEDICALV3RES7.
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
               05  MDCRES-API-RESULT        PIC X(3).
      *        返却メッセージ
               05  MDCRES-API-RESULT-MSG    PIC X(100).
      *
               05  MDCRES-RESKEY            PIC X(100).
      *
      *        選択返却あり
      ***      05  MDCRES-SELECT-FLAG       PIC X(05).
      *        削除情報
               05  MDCRES-DELETE-AREA.
      *            削除情報
                   07  MDCRES-DELETE-CLASS          PIC X(03).
      *            UID（電子カルテ）
                   07  MDCRES-DEL-KARTE-UID         PIC X(36).
      *            UID（オルカ）
                   07  MDCRES-DEL-ORCA-UID          PIC X(64).
      *
      *        排他制御一覧
               05  MDCRES-LOCK-LIST-G.
                   07  MDCRES-LOCK-LIST-OCC     OCCURS   100.
      *                カルテUID
                       11  MDCRES-LOCK-KARTE-UID       PIC X(36).
      *                オルカＵＩＤ（ORCA）
                       11  MDCRES-LOCK-TERMID-UID      PIC X(64).
      *                オペーレータID
                       11  MDCRES-LOCK-OPID            PIC X(16).
      *                業務ＩＤ
                       11  MDCRES-LOCK-LDNAME          PIC X(20).
      *                患者番号
                       11  MDCRES-LOCK-PATIENTID       PIC X(20).
      *                患者氏名
                       11  MDCRES-LOCK-NAME            PIC X(100).
      *                排他日付
                       11  MDCRES-LOCK-DATE            PIC X(10).
      *                排他時間
                       11  MDCRES-LOCK-TIME            PIC X(08).
      *
      *       確認メッセージコード（送信コード）
              05  MDCRES-SELECT-CODE         PIC X(04).
      *       確認メッセージ内容
              05  MDCRES-SELECT-MSG          PIC X(100).
      *
