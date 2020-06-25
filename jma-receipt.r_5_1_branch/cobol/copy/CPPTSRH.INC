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
      ************************************************************
      *    患者情報照会テーブル    COPY (CPPTSRH.INC)         *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      ************************************************************
      *    MCPDATA-REC (50,000byte)に収まるようにすること!!!
      ************************************************************
      *01  PTSRH-REC.
      *医療機関番号
           03  PTSRH-HOSPNUM      PIC 9(2).
      *端末ＩＤ
           03  PTSRH-TERMID       PIC X(64).
      *患者ＩＤ
           03  PTSRH-PTID         PIC 9(10).
      *患者番号
           03  PTSRH-PTNUM        PIC X(20).
      *カナ氏名
           03  PTSRH-KANANAME     PIC X(100).
      *性別
           03  PTSRH-SEX          PIC X(1).
      *誕生日
           03  PTSRH-BIRTHDAY     PIC X(8).
      *受給者番号
           03  PTSRH-JKYSNUM      PIC X(20).
      *入外区分
           03  PTSRH-NYUGAIKBN    PIC X(01).
      *保険の種類
           03  PTSRH-HKNNUM       PIC X(03).
      *公費の種類１
           03  PTSRH-KOHNUM1      PIC X(03).
      *公費の種類２
           03  PTSRH-KOHNUM2      PIC X(03).
      *点数
           03  PTSRH-TOTALTEN     PIC 9(10).
      *診療行為コード
           03  PTSRH-SRYCD        PIC X(09).
      *診療行為名称
           03  PTSRH-SRYNAME      PIC X(100).
      *診療行為カナ名称
           03  PTSRH-SRYKANANAME  PIC X(100).
      *診療行為入力順番
           03  PTSRH-SRYSEQ       PIC 9(01).
      *メモ
           03  PTSRH-MEMO-G.
               05  PTSRH-MEMO         PIC X(30000).
               05  PTSRH-MEMO-R1      REDEFINES    PTSRH-MEMO.
                   07  PTSRH-SRYYM                 PIC X(06).
                   07  PTSRH-FIRSTDAY              PIC 9(02).
                   07  PTSRH-SRYSURYO              PIC 9(05)V9(05).
                   07  PTSRH-ZAIKAISU              PIC 9(05).
                   07  PTSRH-SURYOTOTAL            PIC 9(07)V9(05).
                   07  PTSRH-SRYYM-SRYSURYO        PIC 9(05)V9(05).
                   07  PTSRH-SRYYM-ZAIKAISU        PIC 9(05).
                   07  PTSRH-SRYYM-SURYOTOTAL      PIC 9(07)V9(05).
                   07  PTSRH-SURYO-EDIT            PIC 9(01).
                   07  PTSRH-TANICD                PIC X(03).
                   07  PTSRH-TANINAME              PIC X(12).
                   07  PTSRH-DAY                   PIC 9(03)
                                                   OCCURS  31.
                   07  PTSRH-RENNUM                PIC 9(05).
                   07  PTSRH-R1-FIL                PIC X(29824).
               05  PTSRH-MEMO-R2            REDEFINES    PTSRH-MEMO.
                   07  PTSRH-CNT-ALL        PIC 9(10).
                   07  PTSRH-CNT-PTID       PIC 9(10).
                   07  PTSRH-CNT-SRYCD      PIC 9(10).
                   07  PTSRH-OUTKBN         PIC X(01).
                   07  PTSRH-DBERR          PIC 9(01).
                   07  PTSRH-ORDER          PIC X(500).
                   07  PTSRH-R2-FIL   PIC X(29468).
               05  PTSRH-MEMO-R3      REDEFINES    PTSRH-MEMO.
                   07  PTSRH-HKNCOMBI       PIC X(04).
                   07  PTSRH-HKNID          PIC 9(10).
                   07  PTSRH-KOH-OCC        OCCURS 4.
                       09  PTSRH-KOHNUM     PIC X(03).
                       09  PTSRH-KOHID      PIC 9(10).
                   07  PTSRH-TOKKI-OCC      OCCURS 5.
                       09  PTSRH-TOKKI      PIC X(02).
                       09  PTSRH-TOKKINM    PIC X(12).
                   07  PTSRH-R3-FIL   PIC X(29864).
               05  PTSRH-MEMO-R4      REDEFINES    PTSRH-MEMO.
                   07  PTSRH-R4-SURYO-EDIT         PIC 9(01).
      *DB格納時データ領域節約の為、数値項目も文字項目(X)で定義
                   07  PTSRH-R4-KAISUTOTAL         PIC X(07).
                   07  PTSRH-R4-SURYOTOTAL         PIC X(12).
                   07  PTSRH-R4-TANICD             PIC X(03).
                   07  PTSRH-R4-TANINAME           PIC X(12).
                   07  PTSRH-R4-OCC                OCCURS 1000.
                       09  PTSRH-R4-SRYYMD         PIC X(08).
                       09  PTSRH-R4-SRYSURYO       PIC X(11).
                       09  PTSRH-R4-ZAIKAISU       PIC X(03).
                   07  PTSRH-R4-FIL                PIC X(7965).
      *データ区分
      *    0:検索中(orca13,orca36)
      *    1:検索結果(orca13,orca36)
      *    2:出力用データ(orca13)
      *    3:検索結果(orca13 API 診療行為 印刷, CSV出力用)
      *    4:出力用データ(orca36)
      *    a:検索中(orca36 入院患者数)
      *    b:検索結果(orca36 入院患者数)
      *    c:出力用データ(orca36 入院患者数)
           03  PTSRH-DATAKBN      PIC X(1).
      *連番
           03  PTSRH-SEQNUM       PIC 9(07).
      *作成日
           03  PTSRH-CREYMD       PIC X(8).
      *更新日
           03  PTSRH-UPYMD        PIC X(8).
      *更新時間
           03  PTSRH-UPHMS        PIC X(6).
      *以下検索用項目
      *データ区分
           03  PTSRH-UPDATAKBN    PIC X(1).
      *SQL
           03  PTSRH-SQL          PIC X(15000).
      *DBESCAPEENCODING
           03  PTSRH-DBSTR        PIC X(500).
      *件数
           03  PTSRH-KENSU        PIC 9(08).
      *テキスト（汎用）
           03  PTSRH-VTEXT        PIC X(1000).
      *文字列（汎用）
           03  PTSRH-VSTR-G.
               05  PTSRH-VSTR     PIC X(64) OCCURS 3.
