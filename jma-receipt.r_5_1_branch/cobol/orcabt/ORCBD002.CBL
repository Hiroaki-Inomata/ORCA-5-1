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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBD002.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次帳票
      *  コンポーネント名  : 日計表（ＨＣ１７）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-藤原      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01.10.03  出力先プリンタ名編集
      *  01.00.02    MCC-藤原     01.10.24  出力先プリンタ名が取得
      *                                     できないときエラーにしない
      *  01.00.03    MCC-藤原     02.03.18  端末ＩＤ
      *                                     X(16)からX(32)に変更
      *  01.00.04    MCC-藤原     02.03/22  パラメタにシステム日付を追加
      *  01.00.05    MCC-藤原     02/05/13  印刷管理・印刷ＤＢの追加
      *  01.00.06    NACL-藤原    02/07/24  労災・自賠責・自費の項目追加
      *  01.00.07    NACL-藤原    02/08/27  行数を３０桁に変更
      *                                    （帳票ＩＤをＨＣ１７Ｖ０１へ変更）
      *  01.01.01    NACL-藤原    02/10/10  サイト区分追加
      *  01.01.02    NACL-藤原    02/10/17  自費の集計方法修正
      *  02.00.01    NACL-森脇    04/01/06  マイナスを考慮
      *  02.00.02    NACL-門脇    05/01/12  自費欄集計修正（端数処理）
      *  02.00.03    NACL-太田    06/07/08  編集はORCBD008で行うよう修正
      *  03.05.00    NACL-太田    07/05/28  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
       WORKING-STORAGE             SECTION.
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID                      PIC 9(07).
           03  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-HOSPNUM                    PIC 9(02).
           03  RECEERR                             PIC X(100).
      *
       01  WRK-PARA-ATAI.
           03  WRK-PARA-SYORIYMD   PIC X(08).
           03  WRK-PARA-SORTKBN    PIC X(01).
      *
      *    メインＰＧ用パラメタ
       COPY    "CPORCBD008.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      ****************************************************************
       PROCEDURE               DIVISION
                                   USING
                                   COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                               INTO    LNK-PRTKANRI-RENNUM
                                       LNK-PRTKANRI-TBL-KEY
                                       LNK-PRTKANRI-TBL-GROUP
                                       LNK-PRTKANRI-SHORI-RENNUM
                                       LNK-PRTKANRI-SRYYM
                                       LNK-PRTKANRI-SKYYMD
                                       LNK-PRTKANRI-SHELLID
                                       LNK-PRTKANRI-PRIORITY
                                       LNK-PRTKANRI-TERMID
                                       LNK-PRTKANRI-OPID
                                       LNK-PRTKANRI-PRTNM
                                       WRK-PARA-JOBID
                                       WRK-PARA-SHELLID
                                       WRK-PARA-HOSPNUM
                                       RECEERR
                                       WRK-PARA-SYORIYMD
                                       WRK-PARA-SORTKBN
           END-UNSTRING
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                  ORCBD008AREA
           MOVE    "3"             TO  ORCBD008-SYORIKBN
           MOVE    WRK-PARA-SYORIYMD
                                   TO  ORCBD008-SYORIYMD
      *
           MOVE    "0"             TO  ORCBD008-NYUGAIKBN
           MOVE    WRK-PARA-SORTKBN TO ORCBD008-SORTKBN
      *
           CALL    "ORCBD008"          USING
                                       WRK-PARA
                                       ORCBD008AREA
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
