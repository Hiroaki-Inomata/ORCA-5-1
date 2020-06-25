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
       PROGRAM-ID.             ORCBZ003.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 随時帳票
      *  コンポーネント名  : 外来診療録（カルテ１号紙）一括印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/02/06    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 03.05.00     NACL-多々納  07/05/24  グループ診療対応
      * 04.03.00     NACL-太田    08/06/20  カスタマイズカルテ印刷対応
      * 04.04.00     NACL-多々納  08/12/04  プログラムオプション対応
      * 04.08.00     NACL-多々納  14/07/17  一時ファイル対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
      *
      *    一時ファイル対応
           COPY    "CPRECEERR.INC".
      *
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-BTPARA              PIC 9(01).
      *
           03  FLG-OK                  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(05).
           03  CNT-BZ003               PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(03).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC X(02).
     ******03  RECEERR                 PIC X(100).
           03  WRK-PARA-SRYYM          PIC X(06).
           03  WRK-PARA-SRYKA          PIC X(02).
           03  WRK-PARA-BYOMEIFLG      PIC X(01).
           03  WRK-PARA-KBT            PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
      *
           03  WRK-BYOMEIFLG           PIC X(01).
           03  WRK-PTID                PIC 9(10).
      *
           03  WRK-HKNCOMBINUM         PIC X(04).
           03  WRK-HKN-CNT1            PIC 9(04).
           03  WRK-HKN-CNT2            PIC 9(04).
           03  WRK-PRTNM               PIC X(16).
           03  WRK-PGID                PIC X(20).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
      *    プログラムオプションパラメタ
       01  PARA-AREA.
           03  PARA-SRYKAKBN           PIC X(01).
      *
           03  WRK-PARA-NAME           PIC X(25).
           03  WRK-PARA-DATA           PIC X(50).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1009.INC".
      *
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    患者登録機能制御情報
           COPY  "CPSK1017.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *    患者病名マスタ−
      *01  PTBYOMEI-REC.
      *    COPY    "CPPTBYOMEI.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
      *01  PRTKANRI-REC.
      *    COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
      *01  PRTDATA-REC.
      *    COPY    "CPPRTDATA.INC".
      *
      *    バッチパラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
           COPY    "CPQTOANY.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    診療録サブ
           COPY    "CPORCHC01.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    全角変換
      **   COPY    "CPORCSKANACHK.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      ***  COPY    "ORCA-DBPATH".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
      *
      ******************************************************************
      *
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  SYS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SPA-AREA
           MOVE    ZERO            TO  CNT-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
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
                                       WRK-PARA-SRYYM
                                       WRK-PARA-SRYKA
                                       WRK-PARA-BYOMEIFLG
                                       WRK-PARA-KBT
           END-UNSTRING
      *    ＳＰＡ設定
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBZ003"          TO  JOB-PGID
           MOVE    "外来診療録（カルテ１号紙）一括印刷"
                                       TO  JOB-SHELLMSG
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
      *    初期値セット
           PERFORM 110-SYOKI-SET-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期値セット
      *****************************************************************
       110-SYOKI-SET-SEC           SECTION.
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  WRK-SYSYMD
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "11"            TO  LNK-DAY1-IRAI
           MOVE    WRK-SYSYMD      TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY1-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY1-YMD (2:7)  TO  SPA-SYSYMDW
               MOVE    WRK-SYSYMD          TO  SPA-SYSYMD
           END-IF
      *
      *    発行日
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYSYMD      TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
      *
      *    患者番号構成管理情報編集
           MOVE    SPACE               TO  SYS-1009-REC
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1009-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1009-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                           TO  SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-JIYKSIKBN
                                       TO  SPA-1009-JIYKSIKBN
               MOVE    SYS-1009-JIYKSIKETA
                                       TO  SPA-1009-JIYKSIKETA
               MOVE    SYS-1009-HJNKSIKBN
                                       TO  SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN
                                       TO  SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                       TO  SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                       TO  SPA-1009-HJNKSIRENNUMKETA
               MOVE    SYS-1009-KKCKSIKBN
                                       TO  SPA-1009-KKCKSIKBN
               MOVE    SYS-1009-KKCKSIMAEKETA
                                       TO  SPA-1009-KKCKSIMAEKETA
               MOVE    SYS-1009-KKCKSIRENNUMKETA
                                       TO  SPA-1009-KKCKSIRENNUMKETA
               MOVE    SYS-1009-KKCKSIATOKETA
                                       TO  SPA-1009-KKCKSIATOKETA
           END-IF
      *    患者登録機能制御情報
           MOVE    SPACE               TO  SYS-1017-REC
           INITIALIZE                  SYS-1017-REC
           MOVE    "1017"              TO  SYS-1017-KANRICD
           MOVE    "*"                 TO  SYS-1017-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1017-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1017-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1017-HOSPNUM
           MOVE    SYS-1017-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1017-REC
           ELSE
               INITIALIZE                  SYS-1017-REC
           END-IF
      *    病名編集
           IF      WRK-PARA-BYOMEIFLG  =   "0"
                                       OR  "1"
                                       OR  "2"
               MOVE    WRK-PARA-BYOMEIFLG      TO  WRK-BYOMEIFLG
           ELSE
               MOVE    SYS-1017-KARUTE-BYOMEI  TO  WRK-BYOMEIFLG
           END-IF
      *
      *    職員情報取得
           PERFORM 1001-SYS1010-SEL-SEC
      *
      *    プリンタ名、プログラム名取得
           PERFORM 1001-ORCSPRTNM-SEC
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
           .
       110-SYOKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員情報取得処理
      *****************************************************************
       1001-SYS1010-SEL-SEC            SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1010"          TO  SYS-KANRICD
           MOVE    WRK-SYSYMD      TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1010-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( LNK-PRTKANRI-OPID
                                       =   SYS-1010-USERID )
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-1010-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF    ( FLG-SYSKANRI    NOT =   ZERO )
               INITIALIZE          SYS-1010-REC
           END-IF
      *
           MOVE    SYS-1010-KBNCD      TO  SPA-STAFFCD
      *
           .
       1001-SYS1010-SEL-EXT.
           EXIT.
      *****************************************************************
      *    プリンタ名、プログラム名取得
      *****************************************************************
       1001-ORCSPRTNM-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-PRTNM
                                           WRK-PGID
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    "00000001"          TO  ORCSPRTNM-KBNCD
           MOVE    WRK-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
           MOVE    SYS-1031-PRTNM(1)   TO  WRK-PRTNM
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000001"          TO  ORCSPRTNM-KBNCD
           MOVE    WRK-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG     TO  WRK-PGID
           ELSE
              MOVE    "ORCHC01"           TO  WRK-PGID
           END-IF
      *
           .
       1001-ORCSPRTNM-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           INITIALIZE                  PARA-AREA
      *    診療科
           MOVE    "SRYKAKBN"          TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA-DATA(1:1)  TO  PARA-SRYKAKBN
      *
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       1001-OPTION-CALL-SEC             SECTION.
      *
           INITIALIZE                  ORCSPRGOPTIONAREA
           MOVE    "ORCHC01"           TO  ORCSPRGOPTION-PRGID
           MOVE    WRK-PARA-NAME       TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
           MOVE    SPACE               TO  WRK-PARA-DATA
           IF      ORCSPRGOPTION-OT-TYPE   =   1
      *        半角で１文字目を値とする
               MOVE    ORCSPRGOPTION-OT-PARA(1:1)
                                           TO  WRK-PARA-DATA(1:1)
           END-IF
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    印刷パラメタ編集
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    WRK-PRTNM           TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
      *
           IF    ( WRK-PARA-KBT        =   "1" )
               PERFORM 210-KBT-SYORI-SEC
           ELSE
               PERFORM 210-ALL-SYORI-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別患者指定処理
      *****************************************************************
       210-KBT-SYORI-SEC                SECTION.
      *
           PERFORM 900-BTPARA-KEY8-SEL-SEC
           MOVE    BTPARA-INFO-PARA
                                   TO  QTOANY-AREA
      *
           PERFORM UNTIL ( FLG-BTPARA  NOT =   ZERO )
      *
               MOVE    QTOANY-PTID TO  WRK-PTID
               PERFORM 900-PTINF-KEY-SEL-SEC
               IF    ( PTINF-TSTPTNUMKBN     = "1" )
                   CONTINUE
               ELSE
      *
      *            受診履歴検索
                   MOVE    SPACE       TO  JYURRK-REC
                   INITIALIZE              JYURRK-REC
                   MOVE    SPA-HOSPNUM TO  JYURRK-HOSPNUM
                   MOVE    QTOANY-PTID TO  JYURRK-PTID
                   MOVE    "2"         TO  JYURRK-NYUGAIKBN
                   MOVE    WRK-PARA-SRYYM
                                       TO  JYURRK-SRYYMD(1:6)
                   MOVE    "01"        TO  JYURRK-SRYYMD(7:2)
      *
                   IF     (PARA-SRYKAKBN       =   "1"  )
                    AND   (WRK-PARA-SRYKA      =   "00" )
      *                全科指定
                       MOVE    "%"     TO  JYURRK-SRYKA
                   ELSE
                       MOVE    WRK-PARA-SRYKA
                                       TO  JYURRK-SRYKA
                   END-IF
      *
                   MOVE    "key45"     TO  WRK-MCP-PATHNAME
      *
                   MOVE    JYURRK-REC  TO  MCPDATA-REC
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC      =   ZERO
                       MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 910-JYURRK-READ-SEC
                   ELSE
                       MOVE    1                   TO  FLG-JYURRK
                       INITIALIZE                      JYURRK-REC
                   END-IF
      *
                   MOVE    ZERO    TO  WRK-PTID
      *
      *            １患者で１枚のカルテを発行する
                   PERFORM UNTIL   FLG-JYURRK  =   1
                            OR     WRK-PTID NOT =   ZERO
      *
                       PERFORM 2001-SYORI-SEC
      *
                       MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 910-JYURRK-READ-SEC
      *
                   END-PERFORM
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
      *
               END-IF
      *
               PERFORM 900-BTPARA-KEY8-FET-SEC
               MOVE    BTPARA-INFO-PARA
                                   TO  QTOANY-AREA
      *
           END-PERFORM
      *
           MOVE    "tbl_btpara"    TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       210-KBT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    全患者指定処理
      *****************************************************************
       210-ALL-SYORI-SEC                SECTION.
      *
      *    受診履歴検索
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    "2"                 TO  JYURRK-NYUGAIKBN
           MOVE    WRK-PARA-SRYKA      TO  JYURRK-SRYKA
           MOVE    WRK-PARA-SRYYM      TO  JYURRK-SRYYMD(1:6)
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
      *
           IF     (PARA-SRYKAKBN       =   "1"  )
             AND  (WRK-PARA-SRYKA      =   "00" )
      *        全科指定
               MOVE    "key44"             TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "key32"             TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
               INITIALIZE                      JYURRK-REC
           END-IF
           MOVE    ZERO                TO  WRK-PTID
           PERFORM UNTIL   FLG-JYURRK  =   1
      *        １患者で１枚のカルテを発行する
               IF      JYURRK-PTID     NOT = WRK-PTID
                   PERFORM 2001-SYORI-SEC
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       210-ALL-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    カルテ発行処理
      *****************************************************************
       2001-SYORI-SEC                 SECTION.
      *
      *    保険組合せの期間判定
           INITIALIZE                      HKNCOMBI-REC
           MOVE    JYURRK-HOSPNUM      TO  COMB-HOSPNUM
           MOVE    JYURRK-PTID         TO  COMB-PTID
           MOVE    JYURRK-HKNCOMBINUM  TO  COMB-HKNCOMBINUM
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    1                   TO  FLG-OK
           MOVE    ZERO                TO  WRK-HKNCOMBINUM
           IF     (FLG-HKNCOMBI        =   ZERO )  AND
                  (COMB-TEKSTYMD       <=  SPA-SYSYMD )  AND
                  (COMB-TEKEDYMD       >=  SPA-SYSYMD )
               MOVE    COMB-HKNCOMBINUM    TO  WRK-HKNCOMBINUM
           ELSE
      *        労災・自賠責、自費の時削除
               IF     (COMB-HKNNUM         =   "971"  OR  "973"  )  OR
                      (COMB-HKNNUM (1:2)   =   "98"  )
                   MOVE    ZERO            TO  WRK-HKNCOMBINUM
               ELSE
      *            対象の保険検索
                   PERFORM 20011-HKNCOMBI-SET-SEC
               END-IF
           END-IF
      *    患者番号
           INITIALIZE                      ORCSPTIDAREA
           MOVE    JYURRK-HOSPNUM      TO  SPTID-HOSPNUM
           MOVE    JYURRK-PTID         TO  SPTID-PTID
           CALL    "ORCSPTID"          USING
                                           ORCSPTIDAREA
                                           SPA-AREA
           MOVE    SPTID-PTNUM         TO  SPA-PTNUM
      *
           IF      FLG-OK              =   1
               MOVE    JYURRK-PTID         TO  WRK-PTID
      *        カルテ発行
               INITIALIZE                  ORCHC01AREA
               MOVE    "3"                 TO  ORCHC01-SYORIKBN
               MOVE    WRK-BYOMEIFLG       TO  ORCHC01-BYOMEIFLG
               MOVE    JYURRK-PTID         TO  SPA-PTID
               MOVE    JYURRK-SRYKA        TO  SPA-SRYKA
               MOVE    "2"                 TO  SPA-NYUGAIKBN
               MOVE    WRK-HKNCOMBINUM     TO  SPA-HKNCOMBINUM
      *        一括パラメタ
               MOVE    ORCSPRTAREA         TO  ORCHC01-PRTAREA
      *        外来
               CALL    WRK-PGID            USING
                                           SPA-AREA
                                           ORCHC01AREA
               IF      ORCHC01-RC          =   ZERO
                   ADD     ORCHC01-CNT         TO  CNT-PAGE
               ELSE
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
            END-IF
      *
           .
       2001-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象の保険組合せ検索処理
      *****************************************************************
       20011-HKNCOMBI-SET-SEC             SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    JYURRK-HOSPNUM      TO  COMB-HOSPNUM
           MOVE    JYURRK-PTID         TO  COMB-PTID
           MOVE    SPA-SYSYMD          TO  COMB-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  COMB-TEKEDYMD
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 920-HKNCOMBI-READ-SEC
          ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
           MOVE    ZERO                TO  WRK-HKNCOMBINUM
           MOVE    ZERO                TO  WRK-HKN-CNT1
           MOVE    ZERO                TO  WRK-HKN-CNT2
           PERFORM UNTIL    FLG-HKNCOMBI   =   1
      *        労災・自賠責、自費
               IF     (COMB-HKNNUM         =   "971"  OR  "973"  )  OR
                      (COMB-HKNNUM (1:2)   =   "98"  )
                   CONTINUE
               ELSE
      *            公費の多いのもを対象とする
                   MOVE    ZERO                TO  WRK-HKN-CNT2
                   IF      COMB-HKNNUM         NOT =   SPACE
                       ADD     1                   TO  WRK-HKN-CNT2
                   END-IF
                   IF      COMB-KOH1HKNNUM     NOT =   SPACE
                       ADD     1                   TO  WRK-HKN-CNT2
                   END-IF
                   IF      COMB-KOH2HKNNUM     NOT =   SPACE
                       ADD     1                   TO  WRK-HKN-CNT2
                   END-IF
                   IF      COMB-KOH3HKNNUM     NOT =   SPACE
                       ADD     1                   TO  WRK-HKN-CNT2
                   END-IF
                   IF      COMB-KOH4HKNNUM     NOT =   SPACE
                       ADD     1                   TO  WRK-HKN-CNT2
                   END-IF
                   IF      WRK-HKN-CNT1        <=  WRK-HKN-CNT2
                       MOVE    WRK-HKN-CNT2        TO  WRK-HKN-CNT1
                       MOVE    COMB-HKNCOMBINUM    TO  WRK-HKNCOMBINUM
                   END-IF
               END-IF
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 920-HKNCOMBI-READ-SEC
           END-PERFORM
           MOVE    "tbl_hkncombi"       TO  MCP-TABLE
           MOVE    "key6"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       20011-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      CNT-PAGE            =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBZ003 PAGE:" CNT-PAGE  " ***"
           DISPLAY "*** ORCBZ003 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END     
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    管理マスタ検索
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    管理マスタ読込
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    バッチパラメタ検索
      *****************************************************************
       900-BTPARA-KEY8-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-BTPARA
      *
           INITIALIZE                      BTPARA-REC
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    "ORCBQL01"          TO  BTPARA-SHELLID
           MOVE    2                   TO  BTPARA-JOBID
           MOVE    2                   TO  BTPARA-DSP-RENNUM
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-BTPARA-KEY8-FET-SEC
           ELSE
               INITIALIZE                  BTPARA-REC
               MOVE    1               TO  FLG-BTPARA
           END-IF
      *
           .
       900-BTPARA-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    バッチパラメタ読込
      *****************************************************************
       900-BTPARA-KEY8-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-BTPARA
      *
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BTPARA-REC
           ELSE
               INITIALIZE                  BTPARA-REC
               MOVE    1               TO  FLG-BTPARA
           END-IF
      *
           .
       900-BTPARA-KEY8-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索
      *****************************************************************
       900-PTINF-KEY-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    WRK-PTID        TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               MOVE    1           TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴読み込み処理
      *****************************************************************
       910-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               MOVE    1               TO  FLG-JYURRK
               INITIALIZE                  JYURRK-REC
           END-IF
      *
           .
       910-JYURRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ読み込み処理
      *****************************************************************
       920-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
           .
       920-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
