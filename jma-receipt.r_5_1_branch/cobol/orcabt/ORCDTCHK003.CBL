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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCDTCHK003.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : チェックリスト作成
      *  管理者            :
      *  作成日付    作業者        記述
      *  03/11/21    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/06/04  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
      *
      *    データチェック中間ファイル（チェック結果）
           SELECT  DTCHK004-FILE
                                   ASSIGN  DTCHK004PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK004-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    エラーファイル
           SELECT  DTCHKERR-FILE   ASSIGN  DTCHKERRPARA
                                   FILE    STATUS  IS  STS-DTCHKERR.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *    データチェック中間ファイル（チェック結果）
       FD  DTCHK004-FILE.
           COPY    "CPDTCHK004.INC".
      *
      *    エラーファイル
       FD  DTCHKERR-FILE.
       01  DTCHKERR-REC                PIC X(200). 
      *
       WORKING-STORAGE                 SECTION.
      *
           COPY    "CPDC.INC".
      *
           COPY    "HCM401V05.INC"
                                   REPLACING  //HCM401V05//
                                   BY         //HCM401//.
      *
      *    エラーファイルファイル名称領域 
           COPY    "CPRECEERR.INC"   REPLACING  //RECEERR//
                                   BY         //DTCHKERRPARA//.
      *
      *    中間ファイルファイル名称領域 
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK004PARA//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-COMMON                              PIC X(02).
           03  STS-DTCHKERR                            PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-ERR                                 PIC 9(01).
           03  FLG-DTCHK004                            PIC 9(01).
           03  FLG-HKNOTHERS                           PIC 9(01).
           03  FLG-FIL                                 PIC 9(01).
           03  FLG-LOOPEND                             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-DTCHK004                            PIC 9(06).
           03  CNT-LINE                                PIC 9(06).
           03  CNT-PAGE                                PIC 9(06).
           03  CNT-TTL-PAGE                            PIC 9(06).
           03  CNT-RENBAN                              PIC 9(06).
           03  CNT-HKNINFEDIT                          PIC 9(06).
           03  CNT-ETC                                 PIC 9(06).
           03  CNT-XMLCOUNT                            PIC 9(06).
      *
       01  IDX-AREA.
           03  IDX0                                    PIC 9(05). 
           03  IDX1                                    PIC 9(05). 
           03  IDXA                                    PIC 9(05). 
           03  IDXB                                    PIC 9(05). 
           03  IDX-HKN                                 PIC 9(05).
           03  IDX-MESG                                PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY                             PIC 9(04).
               05  WRK-SMM                             PIC 9(02).
               05  WRK-SDD                             PIC 9(02).
           03  WRK-DATE.
               05  WRK-DATE-YY                         PIC 9(04).
               05  WRK-DATE-FL1                        PIC X(01).
               05  WRK-DATE-MM                         PIC 9(02).
               05  WRK-DATE-FL2                        PIC X(01).
               05  WRK-DATE-DD                         PIC 9(02).
           03  WRK-EDTYMD1                             PIC X(10).
           03  WRK-EDTYMD3                             PIC X(22).
           03  WRK-MAE-INPUT                           PIC X(300).
           03  WRK-OUT-INPUT                           PIC X(300).
           03  WRK-OUT-MAX                             PIC 9(05).
           03  WRK-NYUGAIMES                           PIC X(08).
           03  WRK-KIKANMES                            PIC X(20).
           03  WRK-SRYYMD.
               05  WRK-SRYYMD-G                        PIC X(04).
               05  WRK-SRYYMD-Y                        PIC X(04).
               05  WRK-SRYYMD-FL01                     PIC X(02).
               05  WRK-SRYYMD-M                        PIC X(04).
               05  WRK-SRYYMD-FL02                     PIC X(02).
               05  WRK-SRYYMD-D                        PIC X(04).
               05  WRK-SRYYMD-FL03                     PIC X(02).
           03  WRK-ZZZZ9                               PIC ZZZZ9.
           03  WRK-ZZ9                                 PIC ZZ9.
           03  WRK-Z9                                  PIC Z9.
           03  WRK-HMS.
               05  WRK-HH                              PIC 9(02).
               05  WRK-MM                              PIC 9(02).
               05  WRK-DD                              PIC 9(02).
           03  WRK-ZHMS.
               05  WRK-ZHH                             PIC Z9.
               05  WRK-ZMM                             PIC Z9.
               05  WRK-ZDD                             PIC Z9.
           03  WRK-NENREI                              PIC 9(03).
           03  WRK-DTCHKERR                            PIC X(200).
           03  WRK-SAKYMD-G.
               05  WRK-SAKYMD                          PIC X(22).
               05  WRK-SAKHM                           PIC X(08).
      *
           03  WRK-YMDS1.
               05  WRK-YYS1                            PIC 9(04).
               05  WRK-MMS1                            PIC 9(02).
               05  WRK-DDS1                            PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2                            PIC 9(04).
               05  WRK-MMS2                            PIC 9(02).
               05  WRK-DDS2                            PIC 9(02).
           03  WRK-SHORI-RENNUM                        PIC 9(04).
           03  WRK-SPRT-TITLE                          PIC X(100).
           03  WRK-HKNMES                              PIC X(20).
           03  WRK-ERRMSG                              PIC X(500).
           03  WRK-HKNKBN                              PIC X(01).
           03  WRK-STDAYMES                            PIC X(04).
           03  WRK-EDDAYMES                            PIC X(04).
           03  WRK-KOHINF-G.
               05  WRK-KOHNUM                          PIC X(03).
               05  WRK-PAYKBN                          PIC X(01).
           03  WRK-MESG                                PIC X(1000).
           03  WRK-FIL                                 PIC X(02).
           03  WRK-CHKKOMOKU-LEN-MAX                   PIC 9(05).
           03  WRK-RENBAN-X.
               05  WRK-RENBAN                          PIC 9(05).
           03  WRK-PTID-X.
               05  WRK-PTID                            PIC 9(10).
           03  WRK-STDD-Z9                             PIC Z9.
           03  WRK-EDDD-Z9                             PIC Z9.
           03  WRK-KANACHK-INPUT                       PIC X(100).
      *
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-N-HKNKBN                        PIC X(01).
           03  KEY-OLD.
               05  KEY-O-HKNKBN                        PIC X(01).
           03  KEY-PTID                                PIC 9(10).
      *
      *    データチェック確認項目
           COPY    "CPDTCHKKOMOKU.INC".
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-LINE-MAX                          PIC 9(05)
                                                       VALUE   20.
           03  CONST-DC-MAX                            PIC 9(05)
                                                       VALUE   1000.
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    データチェック機能情報２
           COPY    "CPSK1101.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
           COPY    "CPORCSDTCHK05.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    半角チェック
           COPY    "CPORCSKANACHK.INC".
           COPY    "CPORCSKANACHK2.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
           COPY    "ORCA-BLOB".
           COPY    "XMLDATA.INC".
      *
      ****************************************************************
       LINKAGE                         SECTION.
       01  DTCHK-RC        PIC S9(09).
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *
      *    エラーファイルファイル名称領域 
           COPY    "CPRECEERR.INC"   REPLACING  //RECEERR//
                                   BY         //LNKDTCHKERRPARA//.
      *
      *    データチェック一括処理パラメータ
           COPY    "CPORCBSD01PARA.INC".
      ****************************************************************
       PROCEDURE                       DIVISION
               USING
                                               DTCHK-RC
                                               WRK-PARA
                                               LNKDTCHKERRPARA
                                               ORCBSD01PARA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL ( FLG-END =   1 )
      *
           PERFORM 300-END-SEC
      *
           MOVE    FLG-ERR         TO  DTCHK-RC
      *
           EXIT    PROGRAM
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  STS-AREA
                                       IDX-AREA
                                       CNT-AREA
                                       FLG-AREA
                                       WRK-AREA
                                       KEY-AREA
                                       SPA-AREA
                                       DC-REC
      *
           MOVE    LNKDTCHKERRPARA     TO  DTCHKERRPARA
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *      
           IF      LNK-PRTKANRI-TBL-KEY  NOT =  "RECEPTX"
               PERFORM 900-DBSTART-SEC
      *
      *    ステップ管理開始処理
               MOVE    "STS"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    "ORCDTCHK003"       TO  JOB-PGID
               MOVE    "チェックリスト作成"
                                           TO  JOB-SHELLMSG
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           CALL    "ORCSENCODING"      USING
                                       MCPAREA
                                       SPA-AREA
      *
           MOVE    ZERO                TO  FLG-END
                                           FLG-ERR
      *
      *    データチェック中間ファイル名
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK004PARA-HOSPNUM
           MOVE    "DTCHK004"          TO  DTCHK004PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK004PARA-TERMID
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    DTCHK004PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  DTCHK004PARA-FULLNAME
      *
      *    データチェック２機能情報
           PERFORM 1001-SYS1101-GET-SEC
      *
      *    初期値設定処理
           PERFORM 1001-INIT-VAL-SET-SEC
      *
           OPEN    INPUT               DTCHK004-FILE
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    データチェック２機能情報
      *****************************************************************
       1001-SYS1101-GET-SEC            SECTION.
      *
           INITIALIZE                      SYS-1101-REC
           MOVE    "1101"              TO  SYS-1101-KANRICD
           MOVE    "*"                 TO  SYS-1101-KBNCD
           MOVE    ORCBSD01-SRYYM      TO  SYS-1101-STYUKYMD
           MOVE    "01"                TO  SYS-1101-STYUKYMD (7:2)
           MOVE    SYS-1101-STYUKYMD   TO  SYS-1101-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1101-HOSPNUM
           MOVE    SYS-1101-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1101-REC
           ELSE
               INITIALIZE                  SYS-1101-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-SYS1101-GET-EXT.
           EXIT.
      *****************************************************************
      *    初期値設定処理
      *****************************************************************
       1001-INIT-VAL-SET-SEC           SECTION.
      *
           MOVE    1                   TO  WRK-SHORI-RENNUM
           MOVE    SPACE               TO  WRK-KIKANMES
                                           WRK-STDAYMES
                                           WRK-EDDAYMES
      *
           MOVE    ORCBSD01-SRYYM      TO  WRK-SYMD
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3         TO  WRK-SRYYMD
      *
           IF    ( ORCBSD01-NYUGAIKBN  =   "1" )
               MOVE    "入院"          TO  WRK-NYUGAIMES
           ELSE
               MOVE    "外来"          TO  WRK-NYUGAIMES
           END-IF
      *
           IF    ( ORCBSD01-SRYYMDKBN  =   "1" )
      *
               MOVE    ORCBSD01-STSRYYMD   TO  WRK-Z9
               MOVE    WRK-Z9              TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  WRK-STDAYMES
      *
               MOVE    ORCBSD01-EDSRYYMD   TO  WRK-Z9
               MOVE    WRK-Z9              TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  WRK-EDDAYMES
      *
               IF    ( WRK-STDAYMES    =   WRK-EDDAYMES )
                   STRING    WRK-STDAYMES      DELIMITED   BY  SIZE
                             "日"              DELIMITED   BY  SIZE
                   INTO      WRK-KIKANMES
                   END-STRING
               ELSE
                   STRING    WRK-STDAYMES      DELIMITED   BY  SIZE
                             "日〜"            DELIMITED   BY  SIZE
                             WRK-EDDAYMES      DELIMITED   BY  SIZE
                             "日"              DELIMITED   BY  SIZE
                   INTO      WRK-KIKANMES
                   END-STRING
               END-IF
      *
           ELSE
               MOVE    WRK-EDTYMD3(1:16)   TO  WRK-KIKANMES
           END-IF
      *
           MOVE    SPACE               TO  WRK-SPRT-TITLE
           STRING  WRK-KIKANMES        DELIMITED   BY  SPACE
                   "分（"              DELIMITED   BY  SIZE
                   WRK-NYUGAIMES       DELIMITED   BY  SPACE
                   "）"                DELIMITED   BY  SIZE
           INTO    WRK-SPRT-TITLE
           END-STRING
      *
           PERFORM 800-MCNDATE-SEC
      *
           .
       1001-INIT-VAL-SET-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK004
           MOVE    ZERO            TO  CNT-PAGE
                                       CNT-TTL-PAGE
           MOVE    ZERO            TO  CNT-RENBAN
      *
      *    チェック結果ファイル読込
           PERFORM 900-DTCHK004-READ-SEC
      *
           PERFORM UNTIL ( FLG-DTCHK004    =   1 )
                     OR  ( FLG-END         =   1 )
      *
               MOVE    KEY-NEW     TO  KEY-OLD
               MOVE    ZERO        TO  FLG-HKNOTHERS
      *
      *        帳票編集<見出し>処理
               PERFORM 210-HEAD-HEN-SEC
      *
               PERFORM UNTIL ( FLG-DTCHK004    =   1 )
                        OR   ( FLG-END         =   1 )
                        OR   ( KEY-NEW     NOT =   KEY-OLD )
                        OR   ( CNT-LINE       >=   CONST-LINE-MAX )
      *
      *            帳票編集<明細>処理
                   IF    ( DTCHK004-ERRSBT     =   "0" )
                       PERFORM 220-BODY-HEN-SEC
                   ELSE
                       PERFORM 221-BODY-HEN-SEC
                   END-IF
      *
                   IF    ( ORCBSD01-FROM-API   =   "1" )
                       PERFORM 2201-API-DATA-EDIT-SEC
                   END-IF
      *
      *            チェック結果ファイル読込
                   PERFORM 900-DTCHK004-READ-SEC
      *
               END-PERFORM
      *
      *        帳票編集<注釈>処理
               PERFORM 230-BIKOMARK-HEN-SEC
      *
      *        帳票印刷処理
               PERFORM 290-PRINT-OUT-SEC
      *
               IF    ( KEY-NEW     NOT =   KEY-OLD )
                   IF    ( ORCBSD01-PRTSRTKBN  =   "3" OR  "4" )
                       IF    ( KEY-N-HKNKBN    NOT =   KEY-O-HKNKBN )
                           COMPUTE WRK-SHORI-RENNUM
                               =   WRK-SHORI-RENNUM    +   1
                           COMPUTE CNT-TTL-PAGE
                               =   CNT-TTL-PAGE    +   CNT-PAGE
                           MOVE    ZERO        TO  CNT-PAGE
                           MOVE    ZERO        TO  CNT-RENBAN
                           MOVE    ZERO        TO  KEY-PTID
                       END-IF
                   ELSE
                           COMPUTE CNT-TTL-PAGE
                               =   CNT-PAGE
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( ORCBSD01-FROM-API   =   "1" )
               IF    ( IDXB            >   ZERO )
                   PERFORM 22011-API-DATA-EXPORT-SEC
               END-IF
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<見出し>処理
      *****************************************************************
       210-HEAD-HEN-SEC                SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
           INITIALIZE                      HCM401
      *
           MOVE    WRK-SRYYMD (1:16)   TO  HCM401-SRYYM
      *
           IF    ( DTCHK004-ERRSBT     =   "0" )
               IF    ( ORCBSD01-PRTSRTKBN  =   "3" OR  "4" )
                   MOVE    DTCHK004-HKNKBN TO  WRK-HKNKBN
                   PERFORM 2201-HKNNAME-GET-SEC
                   STRING  "（"                DELIMITED   BY  SIZE
                           WRK-NYUGAIMES       DELIMITED   BY  SPACE
                           "　"                DELIMITED   BY  SIZE
                           WRK-HKNMES          DELIMITED   BY  SPACE
                            "）"               DELIMITED   BY  SIZE
                   INTO    HCM401-NYUGAIKBN
                   END-STRING
               ELSE
                   STRING  "（"                DELIMITED   BY  SIZE
                           WRK-NYUGAIMES       DELIMITED   BY  SPACE
                            "）"               DELIMITED   BY  SIZE
                   INTO    HCM401-NYUGAIKBN
                   END-STRING
               END-IF
           ELSE
                   STRING  "（"                DELIMITED   BY  SIZE
                           WRK-NYUGAIMES       DELIMITED   BY  SPACE
                           "　"                DELIMITED   BY  SIZE
                           "その他"            DELIMITED   BY  SPACE
                            "）"               DELIMITED   BY  SIZE
                   INTO    HCM401-NYUGAIKBN
                   END-STRING
           END-IF
      *
           MOVE    CNT-PAGE            TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT       TO  HCM401-PAGE
      *
           MOVE    SPACE               TO  WRK-SAKYMD-G
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3         TO  WRK-SAKYMD
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           MOVE    WRK-HH              TO  WRK-ZHH
           MOVE    WRK-MM              TO  WRK-ZMM
           MOVE    WRK-DD              TO  WRK-ZDD
           MOVE    WRK-ZHMS            TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT       TO  WRK-SAKHM
      *
           STRING  WRK-SAKYMD          DELIMITED   BY  SIZE
                   "　"                DELIMITED   BY  SIZE
                   WRK-SAKHM (1:4)     DELIMITED   BY  SIZE
                   "時"                DELIMITED   BY  SIZE
                   WRK-SAKHM (5:4)     DELIMITED   BY  SIZE
                   "分"                DELIMITED   BY  SIZE
           INTO    HCM401-SAKYMD
           END-STRING
      *
           PERFORM 2101-KIKANMSG-HEN-SEC
      *
           PERFORM 2101-INGAIMSG-HEN-SEC
      *
           PERFORM 2101-CHKKOMOKU-HEN-SEC
      *
           .
       210-HEAD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療日指定編集処理
      *****************************************************************
       2101-KIKANMSG-HEN-SEC           SECTION.
      *
           IF    ( ORCBSD01-SRYYMDKBN  =   "1" )
               MOVE    SPACE               TO  WRK-MAE-INPUT
               MOVE    ORCBSD01-STSRYYMD   TO  WRK-STDD-Z9
               MOVE    ORCBSD01-EDSRYYMD   TO  WRK-EDDD-Z9
               IF    ( ORCBSD01-STSRYYMD   =   ORCBSD01-EDSRYYMD )
                   STRING  "診療日指定："      DELIMITED BY SIZE
                           WRK-STDD-Z9         DELIMITED BY SIZE
                           "日"                DELIMITED BY SIZE
                   INTO    WRK-MAE-INPUT
                   END-STRING
               ELSE
                   STRING  "診療日指定："      DELIMITED BY SIZE
                           WRK-STDD-Z9         DELIMITED BY SIZE
                           "日〜"              DELIMITED BY SIZE
                           WRK-EDDD-Z9         DELIMITED BY SIZE
                           "日"                DELIMITED BY SIZE
                   INTO    WRK-MAE-INPUT
                   END-STRING
               END-IF
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  HCM401-KIKANMSG
           END-IF
      *
           .
       2101-KIKANMSG-HEN-EXT.
           EXIT.
      *****************************************************************
      *    院外処方の処理編集処理
      *****************************************************************
       2101-INGAIMSG-HEN-SEC           SECTION.
      *
           EVALUATE    ORCBSD01-INGAIPROCKBN
           WHEN    "0"
               MOVE    "院外処方の処理：含まない"
                                       TO  HCM401-INGAIMSG
           WHEN    "1"
               MOVE    "院外処方の処理：院外処方を含む"
                                       TO  HCM401-INGAIMSG
           WHEN    "2"
               MOVE    "院外処方の処理：院外処方のみ"
                                       TO  HCM401-INGAIMSG
           END-EVALUATE
      *
           .
       2101-INGAIMSG-HEN-EXT.
           EXIT.
      *****************************************************************
      *    確認項目編集処理
      *****************************************************************
       2101-CHKKOMOKU-HEN-SEC          SECTION.
      *
           MOVE    ZERO        TO  HCM401-CHKKOMOKU   (1)
           MOVE    HCM401-CHKKOMOKU   (1)
                               TO  WRK-MAE-INPUT
           PERFORM 800-LENGTH-CHK-SEC
           MOVE    WRK-OUT-MAX TO  WRK-CHKKOMOKU-LEN-MAX
           MOVE    SPACE       TO  HCM401-CHKKOMOKU   (1)
      *
           MOVE    1               TO  IDX-MESG
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   DTCHKKOMOKU-MAX )
      *
               IF    ( ORCBSD01-CHKKBN (IDX0)  =   "1" OR "2" )
                   MOVE    DTCHKKOMOKU-CHKKOMOKU (IDX0)
                                       TO  WRK-MAE-INPUT
                   PERFORM 800-ZENKAKU-HEN-SEC
                   IF    ( IDX-MESG    +   WRK-OUT-MAX +   1
                           >=  WRK-CHKKOMOKU-LEN-MAX )
                       MOVE    HCM401-CHKKOMOKU   (1)
                                           TO  HCM401-CHKKOMOKU   (2)
                       MOVE    SPACE       TO  HCM401-CHKKOMOKU   (1)
                       MOVE    1           TO  IDX-MESG
                       STRING  "　　　　　"    DELIMITED   BY  SIZE
                       INTO    HCM401-CHKKOMOKU   (1)
                       WITH    POINTER IDX-MESG
                       END-STRING
                   ELSE
                       IF    ( IDX-MESG    =   1   )
                           STRING  "確認項目："    DELIMITED   BY  SIZE
                           INTO    HCM401-CHKKOMOKU   (1)
                           WITH    POINTER IDX-MESG
                           END-STRING
                       ELSE
                           STRING  "、"    DELIMITED   BY  SIZE
                           INTO    HCM401-CHKKOMOKU   (1)
                           WITH    POINTER IDX-MESG
                           END-STRING
                       END-IF
                   END-IF
      *
                   STRING  DTCHKKOMOKU-CHKKOMOKU (IDX0)
                                       DELIMITED   BY  SPACE
                   INTO    HCM401-CHKKOMOKU (1)
                   WITH    POINTER IDX-MESG
                   END-STRING
               END-IF
      *
           END-PERFORM
      *
           .
       2101-CHKKOMOKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<明細>処理
      *****************************************************************
       220-BODY-HEN-SEC                SECTION.
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           IF    ( KEY-PTID            =   ZERO )
            OR   ( KEY-PTID        NOT =   DTCHK004-PTID )
            OR   ( CNT-LINE            =   1 )
      *
               IF    ( KEY-PTID            =   ZERO )
                OR   ( KEY-PTID        NOT =   DTCHK004-PTID )
      *
                   COMPUTE CNT-RENBAN  =   CNT-RENBAN  +   1
      *
                   MOVE    DTCHK004-PTID   TO  KEY-PTID
      *
               END-IF
      *
               PERFORM 2201-PTHKN-HEN-SEC
               MOVE    WRK-MESG        TO  HCM401-HKNINF(CNT-LINE)
      *
      *        番号
               MOVE    CNT-RENBAN      TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9       TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT   TO  HCM401-RENBAN(CNT-LINE)
      *
      *        患者番号
               IF    ( DTCHK004-PTNUM      =   SPACE )
                   MOVE   SPACE            TO  WRK-MAE-INPUT
                   STRING "患者ＩＤ"       DELIMITED   BY  SIZE
                          DTCHK004-PTID    DELIMITED   BY  SIZE
                   INTO   WRK-MAE-INPUT
                   END-STRING
               ELSE
                   MOVE    DTCHK004-PTNUM  TO  WRK-MAE-INPUT
               END-IF
               PERFORM 800-ZENKAKU-HEN-SEC
               IF    ( WRK-OUT-INPUT (21:) =   SPACE )
                   MOVE    WRK-OUT-INPUT   TO  HCM401-PTNUM (CNT-LINE)
               ELSE
                   MOVE    WRK-OUT-INPUT   TO  HCM401-PTNUM2(CNT-LINE)
               END-IF
      *
      *        患者氏名
               MOVE    DTCHK004-NAME       TO  HCM401-NAME  (CNT-LINE)
               MOVE    HCM401-NAME  (CNT-LINE)
                                           TO  WRK-KANACHK-INPUT
               PERFORM 800-ZENKAKU-CHK-SEC
               MOVE    WRK-KANACHK-INPUT   TO  HCM401-NAME  (CNT-LINE)
      *
      *        年齢
               IF    ( DTCHK004-BIRTHDAY   NOT =   SPACE )
                   PERFORM 2201-NENREI-HEN-SEC
                   MOVE    WRK-NENREI  TO  WRK-ZZ9
                   MOVE    WRK-ZZ9     TO  WRK-MAE-INPUT
                   PERFORM 800-ZENKAKU-HEN-SEC
                   MOVE    WRK-OUT-INPUT   TO  HCM401-NENREI(CNT-LINE)
               END-IF
      *
      *        他保険あり
               IF    ( ORCBSD01-PRTSRTKBN  =   "3" OR  "4" )
                   IF    ( DTCHK004-HKNOTHERS  =   "1" )
                       MOVE    1       TO  FLG-HKNOTHERS
                       MOVE    "＊"    TO  HCM401-MARK (CNT-LINE)
                   END-IF
               END-IF
      *
           END-IF
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           IF    ( ORCBSD01-PRTSRTKBN  =   "1" OR  "2" )
      *        データチェック内容
               IF    ( DTCHK004-MULHKNKBN  =   "1" )
                   MOVE    DTCHK004-HKNKBN TO  WRK-HKNKBN
                   PERFORM 2201-HKNNAME-GET-SEC
                   STRING  WRK-HKNMES      DELIMITED   BY  SPACE
                           "："            DELIMITED   BY  SIZE
                           DTCHK004-ERRMSG DELIMITED   BY  SIZE
                   INTO    WRK-ERRMSG
                   END-STRING
               ELSE
                   MOVE    DTCHK004-ERRMSG TO  WRK-ERRMSG
               END-IF
           ELSE
                   MOVE    DTCHK004-ERRMSG TO  WRK-ERRMSG
           END-IF
      *
           IF    ( WRK-ERRMSG  (111:)  =   SPACE )
               MOVE    WRK-ERRMSG      TO  HCM401-MESG  (CNT-LINE)
           ELSE
               MOVE    WRK-ERRMSG      TO  HCM401-MESG2 (CNT-LINE)
           END-IF
      *
           .
       220-BODY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<明細>処理
      *****************************************************************
       221-BODY-HEN-SEC                SECTION.
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           COMPUTE CNT-ETC     =   CNT-ETC     +   1
      *
           IF    ( CNT-ETC             =   1 )
            OR   ( CNT-LINE            =   1 )
               IF    ( CNT-ETC         =   1 )
                   COMPUTE CNT-RENBAN  =   CNT-RENBAN  +   1
               END-IF
      *
      *        番号
               MOVE    CNT-RENBAN      TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9       TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT   TO  HCM401-RENBAN(CNT-LINE)
      *
      *        患者番号
               MOVE    DTCHK004-PTNUM  TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT   TO  HCM401-PTNUM (CNT-LINE)
      *
      *        患者氏名
               MOVE    "その他"        TO  HCM401-NAME  (CNT-LINE)
           END-IF
      *
           IF    ( DTCHK004-ERRMSG  (111:)  =   SPACE )
               MOVE    DTCHK004-ERRMSG      TO  HCM401-MESG  (CNT-LINE)
           ELSE
               MOVE    DTCHK004-ERRMSG      TO  HCM401-MESG2 (CNT-LINE)
           END-IF
      *
           .
       221-BODY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報編集処理
      *****************************************************************
       2201-PTHKN-HEN-SEC              SECTION.
      *
           MOVE    1               TO  IDX-MESG
           MOVE    SPACE           TO  WRK-MESG
           MOVE    ZERO            TO  FLG-FIL
           MOVE    ZERO            TO  CNT-HKNINFEDIT
      *
           INITIALIZE                  SDTCHK05AREA
           MOVE    WRK-PARA-HOSPNUM TO  SDTCHK05-HOSPNUM
           MOVE    DTCHK004-PTID   TO  SDTCHK05-PTID
           MOVE    DTCHK004-HKNKBN TO  SDTCHK05-HKNKBN
           CALL    "ORCSDTCHK05"   USING   SDTCHK05AREA
                                           ORCBSD01PARA
                                           SPA-AREA
      *
           PERFORM VARYING IDX-HKN FROM    1   BY  1
                   UNTIL ( IDX-HKN >   SDTCHK05-OT-MAX )
      *
               PERFORM 22011-BODY-HKNINF-HEN-SEC
      *
           END-PERFORM
      *
           .
       2201-PTHKN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報編集処理（明細行に編集）
      *****************************************************************
       22011-BODY-HKNINF-HEN-SEC       SECTION.
      *
           MOVE        ZERO            TO  FLG-FIL
      *
           MOVE        SPACE           TO  WRK-OUT-INPUT
           PERFORM 220111-BODY-HKNINF-HEN-SEC
      *
           IF    ( SDTCHK05-HKNKOHKBN (IDX-HKN)    =   "0" )
      *
               STRING  "【"            DELIMITED   BY  SIZE
               INTO    WRK-MESG
               WITH    POINTER               IDX-MESG
               END-STRING
      *
               MOVE    1               TO  FLG-FIL
      *
               MOVE    SDTCHK05-HKNNUMMEI (IDX-HKN)
                                       TO  WRK-OUT-INPUT
               PERFORM 220111-BODY-HKNINF-HEN-SEC
      *
               MOVE    2               TO  FLG-FIL
      *
               MOVE    SDTCHK05-HKNJANUM (IDX-HKN)
                                       TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               IF    ( WRK-OUT-INPUT   NOT =   SPACE )
                   PERFORM 220111-BODY-HKNINF-HEN-SEC
               END-IF
      *
               MOVE    SPACE           TO  WRK-OUT-INPUT
               EVALUATE    SDTCHK05-HONKZKKBN (IDX-HKN)
               WHEN    "1"
                   MOVE    "本人"      TO  WRK-OUT-INPUT
               WHEN    "2"
                   MOVE    "家族"      TO  WRK-OUT-INPUT
               END-EVALUATE
               IF    ( WRK-OUT-INPUT   NOT =   SPACE )
                   PERFORM 220111-BODY-HKNINF-HEN-SEC
               END-IF
      *
               STRING  "】"                DELIMITED   BY  SIZE
               INTO    WRK-MESG
               WITH    POINTER             IDX-MESG
               END-STRING
      *
           ELSE
      *
               STRING  "（"            DELIMITED   BY  SIZE
               INTO    WRK-MESG
               WITH    POINTER               IDX-MESG
               END-STRING
      *
               MOVE    1               TO  FLG-FIL
      *
               MOVE    SDTCHK05-HKNNUMMEI (IDX-HKN)
                                       TO  WRK-OUT-INPUT
               PERFORM 220111-BODY-HKNINF-HEN-SEC
      *
               STRING  "）"                DELIMITED   BY  SIZE
               INTO    WRK-MESG
               WITH    POINTER             IDX-MESG
               END-STRING
           END-IF
      *
           .
       22011-BODY-HKNINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報編集処理（明細行に編集）
      *****************************************************************
       220111-BODY-HKNINF-HEN-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-FIL
           EVALUATE    TRUE
           WHEN    ( CNT-HKNINFEDIT    =   ZERO )
               CONTINUE
           WHEN    ( FLG-FIL           =   ZERO )
               MOVE    "、"        TO  WRK-FIL
           WHEN    ( FLG-FIL           =   1 )
               CONTINUE
           WHEN    OTHER
               MOVE    "　"        TO  WRK-FIL
           END-EVALUATE
      *
           STRING  WRK-FIL         DELIMITED   BY  SPACE
                   WRK-OUT-INPUT   DELIMITED   BY  SPACE
           INTO    WRK-MESG
           WITH    POINTER               IDX-MESG
           END-STRING
      *
           COMPUTE CNT-HKNINFEDIT  =   CNT-HKNINFEDIT  +   1
      *
           .
       220111-BODY-HKNINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    年齢編集処理
      *****************************************************************
       2201-NENREI-HEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-NENREI
      *
           MOVE    DTCHK004-BIRTHDAY   TO  WRK-YMDS1
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-YMDS2
           COMPUTE WRK-NENREI          =   WRK-YYS2
                                       -   WRK-YYS1
           IF    ( WRK-YMDS1(5:4)      >   WRK-YMDS2(5:4) )
               COMPUTE WRK-NENREI      =   WRK-NENREI
                                       -   1
           END-IF
      *
           .
       2201-NENREI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険名称取得処理
      *****************************************************************
       2201-HKNNAME-GET-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNMES
           EVALUATE    WRK-HKNKBN
           WHEN    "1"
               MOVE    "社保"          TO  WRK-HKNMES
           WHEN    "2"
               MOVE    "国保"          TO  WRK-HKNMES
           WHEN    "3"
               MOVE    "労災"          TO  WRK-HKNMES
           WHEN    "4"
               MOVE    "自賠責"        TO  WRK-HKNMES
           WHEN    "5"
               MOVE    "自費"          TO  WRK-HKNMES
           WHEN    "6"
               MOVE    "公害"          TO  WRK-HKNMES
           WHEN    "7"
               MOVE    "後期高齢者"    TO  WRK-HKNMES
           WHEN    "8"
               MOVE    "治験"          TO  WRK-HKNMES
           WHEN    "9"
               MOVE    "第三者行為"    TO  WRK-HKNMES
           WHEN    "H"
               MOVE    "包括"          TO  WRK-HKNMES
           WHEN    "Z"
               MOVE    "その他"        TO  WRK-HKNMES
           END-EVALUATE
      *
           .
       2201-HKNNAME-GET-EXT.
           EXIT.
      *****************************************************************
      *    API用データ編集処理
      *****************************************************************
       2201-API-DATA-EDIT-SEC          SECTION.
      *
           COMPUTE IDXA = IDXA +   1
           COMPUTE IDXB = IDXB +   1
      *
           MOVE    CNT-DTCHK004            TO  WRK-RENBAN
           MOVE    WRK-RENBAN-X            TO  DC-RENBAN (IDXA)
      *
           IF    ( DTCHK004-ERRSBT         =   "0" )
      *
               IF    ( DTCHK004-PTNUM      =   SPACE )
                   MOVE    "@"             TO  DC-PTNUM (IDXA)
                   MOVE    DTCHK004-PTID   TO  WRK-PTID
                   MOVE    WRK-PTID-X      TO  DC-PTNUM (IDXA)(2:)
               ELSE
                   MOVE    DTCHK004-PTNUM  TO  DC-PTNUM (IDXA)
               END-IF
               MOVE    DTCHK004-NAME       TO  DC-NAME (IDXA)
               MOVE    DTCHK004-KANANAME   TO  DC-KANANAME (IDXA)
               MOVE    DTCHK004-BIRTHDAY   TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE            TO  DC-BIRTHDAY (IDXA)
               MOVE    DTCHK004-SEX        TO  DC-SEX (IDXA)
      *
           END-IF
      *
           MOVE    DTCHK004-HKNKBN         TO  WRK-HKNKBN
           PERFORM 2201-HKNNAME-GET-SEC
           MOVE    WRK-HKNMES              TO  DC-HOKEN-NAME (IDXA)
           MOVE    DTCHK004-ERRCD          TO  DC-ERRCD (IDXA)
           MOVE    DTCHK004-ERRMSG         TO  DC-ERRMSG (IDXA)
      *
           IF    ( IDXB                   >=   CONST-DC-MAX )
               PERFORM 22011-API-DATA-EXPORT-SEC
           END-IF
      *
           .
       2201-API-DATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    API用データ編集処理
      *****************************************************************
       22011-API-DATA-EXPORT-SEC       SECTION.
      *
           COMPUTE CNT-XMLCOUNT  = CNT-XMLCOUNT  +   1
      *
           INITIALIZE                  XMLDATA-REC
           MOVE    DC-REC          TO  XMLDATA-DATA
           MOVE    2               TO  XMLDATA-MODE
           MOVE    "xml_data_check" TO MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "Data_Check"    TO  XMLDATA-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
      *
           INITIALIZE                  ORCA-BLOB
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "blob"          TO  MCP-TABLE
           MOVE    XMLDATA-OBJECT  TO  ORCA-BLOB-OBJECT
           INITIALIZE                  SGETTEMP-AREA
           STRING  LNK-PRTKANRI-TBL-KEY    DELIMITED   BY  SPACE
                   LNK-PRTKANRI-TBL-GROUP  DELIMITED   BY  SPACE
                   "."                 DELIMITED   BY  SIZE
                   CNT-XMLCOUNT        DELIMITED   BY  SIZE
                   ".xml"              DELIMITED   BY  SIZE
           INTO    SGETTEMP-BASENAMES  (1)
           END-STRING
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  ORCA-BLOB-FILE
           PERFORM 910-BLOBEXPORT-SEC
      *
           MOVE    ZERO            TO  IDXB
      *
           .
       22011-API-DATA-EXPORT-EXT.
           EXIT.
      *****************************************************************
      *    注釈編集処理
      *****************************************************************
       230-BIKOMARK-HEN-SEC            SECTION.
      *
           IF    ( FLG-HKNOTHERS       =   1 )
               MOVE    "＊"            TO  HCM401-BIKOMARK
               MOVE    "他保険あり"    TO  HCM401-BIKO
           END-IF
      *
           .
       230-BIKOMARK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       290-PRINT-OUT-SEC               SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNMES
           IF    ( ORCBSD01-PRTSRTKBN  =   "3" OR  "4" )
               MOVE    KEY-O-HKNKBN    TO  WRK-HKNKBN
               PERFORM 2201-HKNNAME-GET-SEC
           END-IF
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    1                   TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    WRK-SHORI-RENNUM    TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE   "HCM401V05.red"      TO  SPRT-PRTID
           IF    ( WRK-HKNMES      NOT =   SPACE )
               STRING  WRK-SPRT-TITLE      DELIMITED   BY  SPACE
                       "（"                DELIMITED   BY  SIZE
                       WRK-HKNMES          DELIMITED   BY  SPACE
                       "）"                DELIMITED   BY  SIZE
               INTO    SPRT-TITLE
               END-STRING
           ELSE
               MOVE    WRK-SPRT-TITLE  TO  SPRT-TITLE
           END-IF
           MOVE    HCM401              TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF     ( SPRT-RETURN        =   ZERO )
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                       TO  WRK-DTCHKERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       290-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           OPEN    INPUT   DTCHKERR-FILE
           IF    ( STS-DTCHKERR        =   ZERO )
               CLOSE   DTCHKERR-FILE
           ELSE
               OPEN    OUTPUT              DTCHKERR-FILE
      *
               MOVE    WRK-DTCHKERR    TO  DTCHKERR-REC
               WRITE   DTCHKERR-REC
               CLOSE   DTCHKERR-FILE
             IF    LNK-PRTKANRI-TBL-KEY  NOT =  "RECEPTX"
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-DTCHKERR    TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
              END-IF
           END-IF
      *
           MOVE    1                   TO  FLG-ERR
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           CLOSE                       DTCHK004-FILE
      *
           IF      LNK-PRTKANRI-TBL-KEY  NOT =  "RECEPTX"
      *    ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    CNT-TTL-PAGE    TO  JOB-UPDCNT
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
      *
               PERFORM 900-DBCOMMIT-SEC
           END-IF
      *     
           DISPLAY "*** ORCDTCHK003 END ***"
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-HEN-SEC         SECTION.
      *
           MOVE    SPACE           TO  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE  500               TO  KANACHK-MAX-CNT
           MOVE  WRK-MAE-INPUT     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF    ( KANACHK-RC      =   ZERO )
               MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-OUT-INPUT
               MOVE    KANACHK-MAX TO  WRK-OUT-MAX
           END-IF
      *
           .
       800-ZENKAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    全角チェック処理
      *****************************************************************
       800-ZENKAKU-CHK-SEC         SECTION.
      *
           IF    ( SPA-ENCODING    =   2 )
               INITIALIZE                  ORCSKANACHK2AREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-KANACHK-INPUT
                                       TO  KANACHK2-MAE-INPUT
               CALL    "ORCSKANACHK2"  USING   ORCSKANACHK2AREA
               MOVE    KANACHK2-MAE-INPUT
                                       TO  WRK-KANACHK-INPUT
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE  WRK-KANACHK-INPUT TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT
                                       TO  WRK-KANACHK-INPUT
           END-IF
      *
           .
       800-ZENKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    文字列長チェック
      *****************************************************************
       800-LENGTH-CHK-SEC              SECTION.
      *
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE  500               TO  KANACHK-MAX-CNT
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-MAX     TO  WRK-OUT-MAX
      *
      *
           .
       800-LENGTH-CHK-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD1
                                           WRK-EDTYMD3
      *
           IF    ( WRK-SYMD            =   SPACE )
               GO  TO  800-SEIWA-HEN-EXT
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
           INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
      *
               EVALUATE    WRK-SYMD
               WHEN    "00000000"
                   MOVE    01              TO  WRK-DATE-MM
                   MOVE    01              TO  WRK-DATE-DD
               WHEN    "99999999"
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-EVALUATE
      *
           END-IF
      *
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理サブ
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
      *
       800-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    チェック結果ファイル読込処理
      *****************************************************************
       900-DTCHK004-READ-SEC           SECTION.
      *
           READ    DTCHK004-FILE       NEXT
           AT  END
               MOVE    1               TO  FLG-DTCHK004
           END-READ
           IF    ( STS-COMMON  >   10 )
               MOVE    1               TO  FLG-DTCHK004
           END-IF
      *
           IF    ( FLG-DTCHK004        =   ZERO )
               IF    ( ORCBSD01-PRTSRTKBN  =   "3" OR  "4" )
                   MOVE    DTCHK004-HKNKBN TO  KEY-N-HKNKBN
               ELSE
                   MOVE    "0"             TO  KEY-N-HKNKBN
               END-IF
               COMPUTE CNT-DTCHK004    =   CNT-DTCHK004    +   1
           ELSE
               MOVE    HIGH-VALUE      TO  KEY-NEW
           END-IF
      *
           .
       900-DTCHK004-READ-EXT.
           EXIT.
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
      *    ＤＢ検索処理（FHETCHは行わない)
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
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       900-DBSTART-SEC                 SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBSTART-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBCOMMIT-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *****************************************************************
      *    XMLWRITE処理
      *****************************************************************
       910-XMLWRITE-SEC                 SECTION.
      *
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XMLDATA-REC
      *
           .
       910-XMLWRITE-EXT.
           EXIT.
      *****************************************************************
      *    BLOBEXPORT処理
      *****************************************************************
       910-BLOBEXPORT-SEC              SECTION.
      *
           MOVE    "BLOBEXPORT"    TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       ORCA-BLOB
      *
           .
       910-BLOBEXPORT-EXT.
           EXIT.
