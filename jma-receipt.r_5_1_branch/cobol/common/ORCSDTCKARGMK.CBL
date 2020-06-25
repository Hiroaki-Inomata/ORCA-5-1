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
       PROGRAM-ID.             ORCSDTCKARGMK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : リアルタイムプレビュー引数作成処理
      *  コンポーネント名  : 引数作成＆処理実行(データチェック)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/03/10    NACL−竹田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
      *    SELECT  RECEERR-FILE    ASSIGN  ASGNPARA
      *                            FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
      *FD  RECEERR-FILE.
      *01  RECEERR-REC             PIC X(200).
       WORKING-STORAGE         SECTION.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
           COPY    "CPASGNPARA.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-JOBKANRI        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-SHELLID             PIC X(08).
           03  WRK-TERMID.
               05  WRK-TERMID-NUM      PIC 9(16).
      *
      *    プレビュー用ワーク追加
           03  WRK-SHELL-ARG1.
             05  WRK-SHELL-SRYYM   PIC X(06).
             05  WRK-SHELL-TERMID  PIC X(16).
             05  WRK-SHELL-SYSYMD  PIC X(08).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
       01  CONST-AREA.
           03  CONST-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "RECEPTX".
      *
      *    ジョブ管理ＤＢのキー値
           03  CONST-SHELLID
                                   PIC X(08)   VALUE   "RECEPTX".
           03  CONST-JOBID         PIC 9(07)   VALUE   1.
      *
      *    ジョブ管理ＤＢのキー値（レセプト）
           03  CONST-SHELLID1
                                   PIC X(08)   VALUE   "RECEPT1".
           03  CONST-SHELLID2
                                   PIC X(08)   VALUE   "RECEPT2".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
       01  WRK-PARA-REA.
           COPY    "CPORCSPRTLNK.INC".
      *
           COPY    "MCPAREA".
      *
      *    データチェック一括処理パラメータ
           COPY    "CPORCBSD01PARA.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  UIDIO-AREA.
           03  C-RET                   PIC X(2).
           03  C-UID                   PIC X(36).
       01  UIDIO-ST                    PIC 9(2).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *???
       01  LINK-ERRCD                  PIC X(04).
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
      *     LINK-HKNKBN
      *     ０：医保  １：労災  ２：自賠責
      *     LINK-PTRSI-HKNKBN
      *     １：労災  ２：自賠責
       LINKAGE                 SECTION.
       01  LINK-SHORI-KBN              PIC X(01).
       01  LINK-PTID                   PIC 9(10).
       01  LINK-SRYYM                  PIC X(06).
       01  LINK-NYUGAIKBN              PIC X(01).
       01  LINK-HKNKBN                 PIC X(01).
       01  LINK-PTRSI-HKNKBN           PIC X(01).
       01  LINK-PRTKBN                 PIC X(01).
      *01  LINK-ERRCD                  PIC X(04).
           COPY    "COMMON-SPA".
      *
      **************************************************************************
       PROCEDURE           DIVISION
               USING
               LINK-SHORI-KBN
               LINK-PTID
               LINK-SRYYM
               LINK-NYUGAIKBN
               LINK-HKNKBN
               LINK-PTRSI-HKNKBN
               LINK-PRTKBN
      *        LINK-ERRCD
               SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           EXIT PROGRAM
           .
      * 
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *???
           DISPLAY "ORCSDTCKARGMK ST =" LINK-NYUGAIKBN " " LINK-HKNKBN
                                    " " LINK-PTRSI-HKNKBN
                                    " " LINK-PRTKBN
      *
           INITIALIZE                  WRK-AREA
                                       FLG-AREA
                                       LINK-ERRCD
      *
      *    作成日・時間の設定
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYSKANRI-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYSKANRI-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               INITIALIZE                  SYSKANRI-REC
           END-IF
           MOVE    SYSKANRI-REC        TO  SYS-1031-REC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    using ptid as termid
           MOVE    LINK-PTID       TO  WRK-TERMID-NUM
      *
           MOVE    "ORPRVERR"      TO  RECEERR-FILE-ID
           MOVE    WRK-TERMID      TO  RECEERR-TERMID
           MOVE    SPA-HOSPNUM     TO  RECEERR-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR-BASENAME    TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
           MOVE    SGETTEMP-ST         TO  ASGNPARA-ST
      *
      *    OPEN    OUTPUT   RECEERR-FILE
      *    CLOSE            RECEERR-FILE
      *
           MOVE   "dtck_prv.sh"    TO  SHELLTBL-NAME
      *                                     
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
      *
           MOVE    1               TO  LNK-PRTKANRI-RENNUM
           MOVE    CONST-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY
           MOVE    LINK-PTID       TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    1               TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    LINK-SRYYM      TO  LNK-PRTKANRI-SRYYM 
           MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
           MOVE    "dpck_prv.sh"
                                   TO  LNK-PRTKANRI-SHELLID
           IF      LINK-NYUGAIKBN  =   "1"
               MOVE    1           TO  LNK-PRTKANRI-PRIORITY
           ELSE
               MOVE    2           TO  LNK-PRTKANRI-PRIORITY
           END-IF
           MOVE    ALL "Z"         TO  LNK-PRTKANRI-TERMID
           STRING  WRK-TERMID      DELIMITED   BY  SPACE
                   C-UID           DELIMITED   BY  SPACE
           INTO    LNK-PRTKANRI-TERMID
           END-STRING
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           MOVE    SYS-1031-PRTNM (11)
                                   TO  LNK-PRTKANRI-PRTNM
      *
           MOVE    SPACE           TO  ORCBSD01PARA
           INITIALIZE                  ORCBSD01PARA
      *
           MOVE    "2"             TO  ORCBSD01-IKTKBT
           MOVE    LINK-SRYYM      TO  ORCBSD01-SRYYM
           MOVE    LINK-NYUGAIKBN  TO  ORCBSD01-NYUGAIKBN
           MOVE    ALL "2"         TO  ORCBSD01-CHKKBN-G
           MOVE    "1"             TO  ORCBSD01-SYAHOPROCKBN
           MOVE    "1"             TO  ORCBSD01-KOKUHOPROCKBN
           MOVE    "1"             TO  ORCBSD01-RSIPROCKBN
           MOVE    "1"             TO  ORCBSD01-JBIPROCKBN
           MOVE    "1"             TO  ORCBSD01-JIHIPROCKBN
           MOVE    "1"             TO  ORCBSD01-INGAIPROCKBN
           MOVE    "2"             TO  ORCBSD01-PRTSRTKBN
           MOVE    "2"             TO  ORCBSD01-PRTPTLSTKBN
           MOVE    SPACE           TO  ORCBSD01-SRYYMDKBN
           MOVE    00              TO  ORCBSD01-STSRYYMD
           MOVE    00              TO  ORCBSD01-EDSRYYMD
           MOVE    "1"             TO  ORCBSD01-KGIPROCKBN
           MOVE    "1"             TO  ORCBSD01-KOUKIPROCKBN
           MOVE    "1"             TO  ORCBSD01-CHIKENPROCKBN
           MOVE    "1"             TO  ORCBSD01-DAISANPROCKBN
      *
           MOVE   SPA-HOSPNUM      TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG3
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG6
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG10
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG11
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG12
           MOVE    "0"             TO  SHELLTBL-ARG13
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG14
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG15
           MOVE    ASGNPARA (ASGNPARA-ST :)
                                   TO  SHELLTBL-ARG16
           MOVE    ORCBSD01-IKTKBT TO  SHELLTBL-ARG17
           MOVE    ORCBSD01-SRYYM  TO  SHELLTBL-ARG18
           MOVE    ORCBSD01-NYUGAIKBN
                                   TO  SHELLTBL-ARG19
           MOVE    ORCBSD01-CHKKBN-G
                                   TO  SHELLTBL-ARG20
           MOVE    ORCBSD01-SYAHOPROCKBN
                                   TO  SHELLTBL-ARG21
           MOVE    ORCBSD01-KOKUHOPROCKBN
                                   TO  SHELLTBL-ARG22
           MOVE    ORCBSD01-RSIPROCKBN
                                   TO  SHELLTBL-ARG23
           MOVE    ORCBSD01-JBIPROCKBN
                                   TO  SHELLTBL-ARG24
           MOVE    ORCBSD01-JIHIPROCKBN
                                   TO  SHELLTBL-ARG25
           MOVE    ORCBSD01-INGAIPROCKBN
                                   TO  SHELLTBL-ARG26
           MOVE    ORCBSD01-PRTSRTKBN
                                   TO  SHELLTBL-ARG27
           MOVE    ORCBSD01-PRTPTLSTKBN
                                   TO  SHELLTBL-ARG28
           MOVE    ORCBSD01-SRYYMDKBN
                                   TO  SHELLTBL-ARG29
           MOVE    ORCBSD01-STSRYYMD
                                   TO  SHELLTBL-ARG30
           MOVE    ORCBSD01-EDSRYYMD
                                   TO  SHELLTBL-ARG31
           MOVE    ORCBSD01-KGIPROCKBN
                                   TO  SHELLTBL-ARG32
           MOVE    ORCBSD01-KOUKIPROCKBN
                                   TO  SHELLTBL-ARG33
           MOVE    ORCBSD01-CHIKENPROCKBN
                                   TO  SHELLTBL-ARG34
           MOVE    ORCBSD01-DAISANPROCKBN
                                   TO  SHELLTBL-ARG35
           MOVE    LINK-SHORI-KBN  TO  SHELLTBL-ARG36
           MOVE    LINK-HKNKBN     TO  SHELLTBL-ARG37
           MOVE    LINK-PTID       TO  SHELLTBL-ARG38
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "monthly"       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           DISPLAY "ORCSDTCKARGMK ED ="
      *???     
      *
           .
       200-MAIN-EXT.
           EXIT.
           .
      *
      *****************************************************************
      *    UID取得処理
      *****************************************************************
       800-ORCUIDSET-SEC               SECTION.
      *
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       UIDIO-ST
                                       UIDIO-AREA
      *
           .
       800-ORCUIDSET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHを行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *
