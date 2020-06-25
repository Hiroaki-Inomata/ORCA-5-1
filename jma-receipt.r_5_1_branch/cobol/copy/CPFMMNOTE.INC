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
       01  FMMNOTE-REC.
           03  FMMNOTE-CONTEXT                     PIC S9(9)   BINARY.
           03  FMMNOTE-OBJECT                      PIC X(36).
           03  FMMNOTE-MODE                        PIC S9(9)   BINARY.
           03  FMMNOTE-RECORDNAME                  PIC X(128).
           03  FMMNOTE-DATA.
      *            帳票ＩＤ
               05  FMMNOTE-FORM-ID                PIC X(30).
      *            カスタムＩＤ
               05  FMMNOTE-CUSTOM-ID               PIC X(30).
      *            プリンタ名
               05  FMMNOTE-PRINTER                 PIC X(20).
      *            診療日 YYYY-MM-DD
               05  FMMNOTE-SRYYMD                  PIC X(10).
      *            伝票番号
               05  FMMNOTE-DENPNUM                 PIC X(7).
      *            患者情報
               05  FMMNOTE-PTINF-AREA.
      *                患者番号
                   07  FMMNOTE-PTNUM               PIC X(20).
      *                氏名
                   07  FMMNOTE-NAME                PIC X(50).
      *                カナ氏名
                   07  FMMNOTE-KANANAME            PIC X(50).
      *                誕生日
                   07  FMMNOTE-BIRTHDAY            PIC X(10).
      *                性別区分
                   07  FMMNOTE-SEX                 PIC X(1).
      *            医療機関情報
               05  FMMNOTE-HOSP-AREA.
      *                県番
                   07  FMMNOTE-PREFNUM             PIC X(2).
      *                名称
                   07  FMMNOTE-HOSP-NAME-G.
                       09  FMMNOTE-HOSP-NAME       PIC X(120)
                                                   OCCURS   3.
      *                住所
                   07  FMMNOTE-HOSP-ADRS-G.
                       09  FMMNOTE-HOSP-ADRS       PIC X(200)
                                                   OCCURS   3.
      *                電話番号
                   07  FMMNOTE-HOSP-TEL            PIC X(15).
               05  FMMNOTE-DR-AREA.
      *                医師コード
                   07  FMMNOTE-DR-CODE             PIC X(5).
      *                医師名
                   07  FMMNOTE-DR-NAME             PIC X(50).
               05  FMMNOTE-ZAI-AREA                OCCURS  50 TIMES.
      *                診療区分
                   07  FMMNOTE-SRYKBN              PIC X(2).
      *                剤回数
                   07  FMMNOTE-ZAIKAISU            PIC X(3).
      *                内服点剤区分
                   07  FMMNOTE-NAIHENKBN           PIC X(01).
      *                剤単位名（回数／回分）
                   07  FMMNOTE-ZAITANINAME         PIC X(12).
                   07  FMMNOTE-MEDICATION          OCCURS  50 TIMES.
      *                薬剤名
                       09  FMMNOTE-YKZNAME         PIC X(80).
      *                数量(ZZZZ9.99999)
                       09  FMMNOTE-SURYO           PIC X(12).
      *                単位名
                       09  FMMNOTE-TANINAME        PIC X(12).
      *                診療コード
                       09  FMMNOTE-SRYCD           PIC X(9).
      *        備考
               05  FMMNOTE-MEMO    PIC X(80)       OCCURS  10 TIMES.
      *        ＱＲ取得用ＩＤ
               05  FMMNOTE-QR-ID   PIC X(80).

