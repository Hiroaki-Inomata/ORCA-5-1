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
       PROGRAM-ID.             ORCBG013.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 外来月別請求書発行
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/08/22    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-森脇    07/06/06  グループ診療対応
      *  04.08.01    NACL-森脇    14/07/22  一時ファイルディレクトリ設定
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    対象患者ファイル
grpsys*DD  SELECT  BG01301-FILE    ASSIGN   ORCBG01301PARA
grpsyS     SELECT  BG01301-FILE    ASSIGN   BG01301PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BG01301-KEY
                                   FILE    STATUS  IS  STS-BG01301.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    処理対象患者ファイル
       FD  BG01301-FILE.
       01  BG01301-REC.
           03  BG01301-KEY.
               05  BG01301-RENNUM              PIC 9(06).
               05  BG01301-PTNUM               PIC X(20).
           03  BG01301-PTID                    PIC 9(10).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                         PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    対象患者ファイル
           COPY    "CPCOMMONDAT2.INC"
                                           REPLACING   //RECE01//
grpsys*DD                                  BY          //ORCBG01301//.
                                           BY          //BG01301//.
      *----(04.08.01)--UPD-START---
      *     03  FILLER                      PIC X(04)   VALUE   ".dat".
      *----(04.08.01)--UPD-END-----
      *
      *    エラーファイル 名称領域 
grpsys*DD  COPY    "CPCOMMONDAT2.INC"      REPLACING  //RECE01PARA//
grpsys*DD                                  BY         //RECEERR//.
grpsys*DD  03  FILLER                      PIC X(04)   VALUE   ".dat".
      *----(04.08.01)--ADD-START---
           COPY    "CPRECEERR.INC".
      *----(04.08.01)--ADD-END-----
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-RECEERR                         PIC X(02).
           03  STS-BG01301                         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYUNOU                          PIC 9(01).
           03  FLG-BG01301                         PIC 9(01).
           03  FLG-PTNUM                           PIC 9(01).
           03  FLG-PTINF                           PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-BTPARA                          PIC 9(01).
           03  FLG-LOOPEND                         PIC 9(01).
           03  FLG-TARGET                          PIC 9(01).
           03  FLG-PARA                            PIC 9(01).
           03  FLG-SKYPRT                          PIC 9(01).
           03  FLG-SRYPRT                          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-BG01301                         PIC 9(05).
           03  CNT-JOB-UPDCNT                      PIC 9(08).
           03  CNT-RENNUM                          PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
           03  IDXD                                PIC 9(05).
           03  IDXE                                PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID                      PIC 9(07).
           03  WRK-PARA-SHELLID                    PIC X(08).
grpsys*DD  03  WRK-PARA-HOSPID                     PIC X(24).
grpsys     03  WRK-PARA-HOSPNUM                    PIC 9(02).
      *----(04.08.01)--UPD-START---
grpsys*     03  RECEERR                             PIC X(100).
      *----(04.08.01)--UPD-END-----
           03  WRK-PARA-SRYYM                      PIC X(06).
           03  WRK-PARA-HAKUHOUFLG                 PIC X(01).
           03  WRK-PARA-PTNUM                      PIC X(20).
           03  WRK-PARA-HAKYMD                     PIC X(08).
           03  WRK-PARA-ZEROSKYKBN                 PIC X(01).
           03  WRK-PARA-SRYKA                      PIC X(02).
           03  WRK-PARA-ZENMISYUPRTKBN             PIC X(01).
           03  WRK-PARA-SUMKBN                     PIC X(01).
           03  WRK-PARA-PRTFORMKBN                 PIC X(01).
           03  WRK-PARA-PTREFKBN                   PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-PTID                            PIC 9(10).
           03  WRK-PTNUM                           PIC X(20).
           03  WRK-TITLE                           PIC X(100).
           03  WRK-STR                             PIC X(200).
           03  WRK-LEN                             PIC 9(05).
           03  WRK-SHO                             PIC 9(05).
           03  WRK-AMARI                           PIC 9(05).
           03  WRK-MAE-INPUT                       PIC X(300).
           03  WRK-OUT-INPUT                       PIC X(300).
           03  WRK-OUT-MAX                         PIC 9(05).
           03  WRK-STR-LEFT                        PIC X(500).
           03  WRK-STR-LEFT-Z      REDEFINES       WRK-STR-LEFT.
               05  WRK-STR-LEFT-ZX99               PIC Z(9)9.
               05  WRK-STR-LEFT-FL                 PIC X(490).
           03  WRK-STR-LEFT-TMP                    PIC X(500).
      *        全角編集用項目
           03  WRK-N.
               05  WRK-BG01301-KNS-N               PIC X(10).
               05  WRK-BG01301-TIME-N              PIC X(16).
           03  WRK-SEIKYU-PG                       PIC X(20).
           03  WRK-SEIKYU-PRTNM                    PIC X(20).
           03  WRK-MEISAI-PG                       PIC X(20).
           03  WRK-MEISAI-PRTNM                    PIC X(20).
           03  WRK-NYUGAIKBN                       PIC X(01).
           03  WRK-MISYU-G.
               05  WRK-MISYU                       PIC S9(10).
               05  WRK-MISYU-P                     PIC S9(10).
               05  WRK-MISYU-M                     PIC S9(10).
           03  WRK-NYU-MISYU                       PIC S9(10).
           03  WRK-GAI-MISYU                       PIC S9(10).
           03  WRK-GAI-MISYU-P                     PIC S9(10).
           03  WRK-GAI-MISYU-M                     PIC S9(10).
           03  WRK-FILEMEI  .
               05  WRK-FILEMEI-YMD PIC X(08).
               05  WRK-FILEMEI-HMS PIC X(06).
               05  WRK-FILEMEI-CD  PIC X(06).
           03  WRK-SRYYMD                          PIC X(08).
           03  WRK-DENPNUM                         PIC 9(07).
      *
       01  CONST-AREA.
           03  CONST-BTPARA-SHELLID        PIC X(08)
                                           VALUE   "ORCBQL01".
           03  CONST-BTPARA-JOBID          PIC 9(07)
                                           VALUE   2.
           03  CONST-BTPARA-DSP-RENNUM     PIC 9(03)
                                           VALUE   2.
           03  CONST-PRTDEF-G.
               05  CONST-PRTDEF0       PIC X(03)   VALUE   "000".
               05  CONST-PRTDEF1       PIC X(03)   VALUE   "111".
               05  CONST-PRTDEF2       PIC X(03)   VALUE   "211".
               05  CONST-PRTDEF3       PIC X(03)   VALUE   "311".
               05  CONST-PRTDEF4       PIC X(03)   VALUE   "410".
               05  CONST-PRTDEF5       PIC X(03)   VALUE   "510".
               05  CONST-PRTDEF6       PIC X(03)   VALUE   "610".
               05  CONST-PRTDEF7       PIC X(03)   VALUE   "711".
               05  CONST-PRTDEF8       PIC X(03)   VALUE   "801".
               05  CONST-PRTDEF9       PIC X(03)   VALUE   "901".
               05  CONST-PRTDEFA       PIC X(03)   VALUE   "A01".
           03  CONST-PRTDEF-R  REDEFINES   CONST-PRTDEF-G.
      *        0〜7 患者登録業務の請求書発行区分
               05  CONST-PRTDEF-OCC            OCCURS  11.
                   07  CONST-PRTFLG            PIC X(01).
                   07  CONST-SKYPRTOUT         PIC X(01).
                   07  CONST-SRYPRTOUT         PIC X(01).
           03  CONST-PRTDEF-MAX        PIC 9(05)   VALUE   11.
      *
       01  ERR-EDIT-AREA.
           03  ERR-PTID                            PIC 9(10).
           03  ERR-PTNUM                           PIC X(20).
           03  ERR-ERRMSG                          PIC X(80).
           03  ERR-HEN-ERRMSG                      PIC X(80).
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK1031.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    パラメタテーブル	
       01  PARA-REC.
           COPY    "CPPARA.INC".
           COPY    "CPSYUPARA01.INC".
           COPY    "CPSYUPARA02.INC".
           COPY    "CPSYUPARA03.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
           COPY    "CPQTOANY.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    請求書データ編集サブ
           COPY    "CPORCHC03S02.INC".
      *
      *    請求書パラメタ
           COPY    "CPORCHC03.INC".
      *
      *    診療費明細書パラメタ
           COPY    "CPORCHC04.INC".
      *
      *----(04.08.01)--ADD-START---
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *    COPY    "ORCA-DBPATH".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(1000).
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
           .
       000-PROC-EXT.
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  ERR-EDIT-AREA
grpsys     INITIALIZE                      SPA-AREA
      *----(04.08.01)--ADD-START---
           INITIALIZE                  RECEERR
      *----(04.08.01)--ADD-END-----
      *
           PERFORM 100-DBOPEN-SEC
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
grpsys*DD                              WRK-PARA-HOSPID
grpsys                                 WRK-PARA-HOSPNUM
grpsys                                 RECEERR
                                       WRK-PARA-SRYYM
                                       WRK-PARA-HAKUHOUFLG
                                       WRK-PARA-PTNUM
                                       WRK-PARA-HAKYMD
                                       WRK-PARA-ZEROSKYKBN
                                       WRK-PARA-SRYKA
                                       WRK-PARA-ZENMISYUPRTKBN
                                       WRK-PARA-SUMKBN
                                       WRK-PARA-PRTFORMKBN
                                       WRK-PARA-PTREFKBN
grpsys     END-UNSTRING
      *    ＳＰＡエリアセット処理
grpsys     PERFORM 1001-SPA-SET-SEC
      *
           IF    ( WRK-PARA-SRYKA      NOT =   SPACE )
               MOVE    "3"             TO  WRK-PARA-HAKUHOUFLG
           END-IF
      *
           IF    ( WRK-PARA-ZEROSKYKBN =   "0" OR "1" )
               CONTINUE
           ELSE
               MOVE    "0"             TO  WRK-PARA-ZEROSKYKBN
           END-IF
      *
           IF    ( WRK-PARA-SUMKBN     =   "0" OR "1" )
               CONTINUE
           ELSE
               MOVE    "0"             TO  WRK-PARA-SUMKBN
           END-IF
      *
           IF    ( WRK-PARA-PRTFORMKBN =   "0" OR "1" OR "2" )
               CONTINUE
           ELSE
               MOVE    "0"             TO  WRK-PARA-PRTFORMKBN
           END-IF
      *
           IF    ( WRK-PARA-PTREFKBN   =   "0" OR "1" )
               CONTINUE
           ELSE
               MOVE    "0"             TO  WRK-PARA-PTREFKBN
           END-IF
      *
           MOVE    "外来月別請求書"    TO  WRK-TITLE
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBG013"          TO  JOB-PGID
           MOVE    WRK-TITLE           TO  JOB-SHELLMSG
      *
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
grpsys                                     SPA-AREA
      *
grpsys*DD  MOVE    "BG01301"           TO  ORCBG01301PARA-FILE-ID
grpsys*DD  MOVE    LNK-PRTKANRI-TERMID TO  ORCBG01301PARA-TERMID
grpsys     MOVE    "BG01301"           TO  BG01301PARA-FILE-ID
grpsys     MOVE    LNK-PRTKANRI-TERMID TO  BG01301PARA-TERMID
grpsys     MOVE    SPA-HOSPNUM         TO  BG01301PARA-HOSPNUM
      *
grpsys*DD  MOVE    "RECEERR"           TO  RECEERR-FILE-ID
grpsys*DD  MOVE    LNK-PRTKANRI-TERMID TO  RECEERR-TERMID
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    BG01301PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  BG01301PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECEERR
      *----(04.08.01)--ADD-END-----
      *
           MOVE    ZERO                TO  FLG-END
      *
           PERFORM 1001-SYS1010-SEL-SEC
      *
      *    ＳＰＡエリアセット処理
grpsys*DD  PERFORM 1001-SPA-SET-SEC
      *
           PERFORM 1001-PRTNAME-SEC
      *
      *    印刷パラメタエリアセット処理
           PERFORM 1001-ORCSPRTAREA-SET-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    職員情報検索処理
      *****************************************************************
       1001-SYS1010-SEL-SEC            SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1010"              TO  SYS-KANRICD
           MOVE    WRK-PARA-HAKYMD     TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
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
           PERFORM 910-DBCLOSECURSOR-SEC
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
      *    ＳＰＡエリアセット処理
      *****************************************************************
       1001-SPA-SET-SEC                SECTION.
      *
      *    システム日付セット
grpsys*DD  INITIALIZE                      SPA-AREA
           MOVE    "2"                 TO  SPA-NYUGAIKBN
           MOVE    LNK-PRTKANRI-SKYYMD
                                       TO  SPA-SYSYMD
grpsys*DD  MOVE    WRK-PARA-HOSPID     TO  SPA-HOSPID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    LNK-PRTKANRI-TERMID TO  SPA-TERMID
      *
           .
       1001-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    帳票プログラム取得処理
      *****************************************************************
       1001-PRTNAME-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-SEIKYU-PRTNM
                                           WRK-MEISAI-PRTNM
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"             USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYSKANRI-REC
           MOVE    SYSKANRI-REC        TO  SYS-1031-REC
           MOVE    SYS-1031-PRTNM(3)   TO  WRK-SEIKYU-PRTNM
           MOVE    SYS-1031-PRTNM(14)  TO  WRK-MEISAI-PRTNM
      *
           .
       1001-PRTNAME-EXT.
           EXIT.
      *****************************************************************
      *    印刷パラメタエリアセット処理
      *****************************************************************
       1001-ORCSPRTAREA-SET-SEC         SECTION.
      *
      *    印刷パラメタ編集
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    WRK-PARA-SRYYM      TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE   SYS-1031-PRTNM(3)    TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           MOVE    WRK-TITLE           TO  SPRT-TITLE
      *
           .
       1001-ORCSPRTAREA-SET-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE    WRK-PARA-PTNUM
      *    外来全患者
           WHEN    SPACE
               PERFORM 210-GAI-ALL-HEN-SEC
               IF    ( FLG-END     NOT =   ZERO )
                   GO  TO  200-MAIN-EXT
               END-IF
      *    外来照会連携
           WHEN    "*"
               PERFORM 210-QLINK-HEN-SEC
               IF    ( FLG-END     NOT =   ZERO )
                   GO  TO  200-MAIN-EXT
               END-IF
      *    外来指定患者
           WHEN    OTHER
               PERFORM 210-PTSHITEI-HEN-SEC
               IF    ( FLG-END     NOT =   ZERO )
                   GO  TO  200-MAIN-EXT
               END-IF
           END-EVALUATE
      *
           MOVE    1                   TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    −外来対象診療年月全患者−
      *****************************************************************
       210-GAI-ALL-HEN-SEC             SECTION.
      *
      *    処理対象患者ファイル出力処理
           PERFORM 2101-BG01301-OUT-SEC
           IF    ( FLG-END     NOT =   ZERO )
               GO  TO  210-GAI-ALL-HEN-EXT
           END-IF
      *
      *    外来月別請求書編集処理
           PERFORM 2101-TUKISKY-HEN-SEC
           IF    ( FLG-END     NOT =   ZERO )
               GO  TO  210-GAI-ALL-HEN-EXT
           END-IF
      *
           .
       210-GAI-ALL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    処理対象患者ファイル出力処理
      *****************************************************************
       2101-BG01301-OUT-SEC              SECTION.
      *
           OPEN    OUTPUT  BG01301-FILE
           CLOSE   BG01301-FILE
      *
           OPEN    I-O     BG01301-FILE
      *
           PERFORM 900-SYUNOU-KEY52-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
      *        患者番号取得
               MOVE    SYU-PTID    TO  WRK-PTID
               PERFORM 900-PTNUM-KEY-SEL-SEC
               IF    ( FLG-PTNUM   =   ZERO )
                   MOVE    MCPDATA-REC TO  PTNUM-REC
               ELSE
                   PERFORM 21011-PTNUM-ERR-HEN-SEC
                   GO  TO  2101-BG01301-OUT-EXT
               END-IF
      *
               INITIALIZE              BG01301-REC
               MOVE    PTNUM-PTNUM     TO  BG01301-PTNUM
               PERFORM 900-BG01301-READ-DYN-SEC
               IF    ( FLG-BG01301     NOT =   ZERO )
      *
                   INITIALIZE          BG01301-REC
                   MOVE    PTNUM-PTNUM TO  BG01301-PTNUM
                   MOVE    SYU-PTID    TO  BG01301-PTID
                   WRITE   BG01301-REC
               END-IF
      *
               PERFORM 900-SYUNOU-KEY52-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key52"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   BG01301-FILE
      *
           .
       2101-BG01301-OUT-EXT.
           EXIT.
      *****************************************************************
      *    −照会連携−
      *****************************************************************
       210-QLINK-HEN-SEC           SECTION.
      *
      *    処理対象患者ファイル出力処理
           PERFORM 2102-BG01301-OUT-SEC
      *
           IF    ( FLG-END         =   ZERO )
      *        外来月別請求書編集処理
               PERFORM 2101-TUKISKY-HEN-SEC
           END-IF
      *
           .
       210-QLINK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    処理対象患者ファイル出力処理
      *****************************************************************
       2102-BG01301-OUT-SEC              SECTION.
      *
           OPEN    OUTPUT  BG01301-FILE
      *
           PERFORM 900-BTPARA-KEY8-SEL-SEC
           MOVE    BTPARA-INFO-PARA
                                   TO  QTOANY-AREA
      *
           MOVE    ZERO            TO  CNT-RENNUM
           PERFORM UNTIL ( FLG-BTPARA  NOT =   ZERO )
      *
               MOVE    1           TO  FLG-TARGET
      *
               MOVE    QTOANY-PTID TO  WRK-PTID
               PERFORM 900-PTINF-KEY-SEL-SEC
               IF    ( PTINF-TSTPTNUMKBN     = "1" )
                   MOVE    ZERO    TO  FLG-TARGET
               END-IF
      *
               IF    ( FLG-TARGET  =   1 )
      *
                   COMPUTE CNT-RENNUM  =   CNT-RENNUM  +   1
      *
                   INITIALIZE          BG01301-REC
                   MOVE    CNT-RENNUM      TO  BG01301-RENNUM
                   MOVE    QTOANY-PTNUM    TO  BG01301-PTNUM
                   MOVE    QTOANY-PTID     TO  BG01301-PTID
                   WRITE   BG01301-REC
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
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   BG01301-FILE
      *
           .
       2102-BG01301-OUT-EXT.
           EXIT.
      *****************************************************************
      *    患者情報エラー編集処理
      *****************************************************************
       21011-PTINF-ERR-HEN-SEC         SECTION.
      *
           INITIALIZE          ERR-EDIT-AREA
           MOVE    SYU-PTID    TO  ERR-PTID
           MOVE    "患者情報の取得に失敗しました"
                               TO  ERR-ERRMSG
           PERFORM 800-ERRMSG-EDIT-SEC
           MOVE    ERR-HEN-ERRMSG  TO  WRK-RECEERR
           PERFORM 500-ERR-HENSYU-SEC
      *
           .
       21011-PTINF-ERR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者番号エラー編集処理
      *****************************************************************
       21011-PTNUM-ERR-HEN-SEC         SECTION.
      *
           INITIALIZE          ERR-EDIT-AREA
           MOVE    SYU-PTID    TO  ERR-PTID
           MOVE    "患者番号の取得に失敗しました"
                               TO  ERR-ERRMSG
           PERFORM 800-ERRMSG-EDIT-SEC
           MOVE    ERR-HEN-ERRMSG  TO  WRK-RECEERR
           PERFORM 500-ERR-HENSYU-SEC
      *
           .
       21011-PTNUM-ERR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    外来月別請求書編集処理
      *****************************************************************
       2101-TUKISKY-HEN-SEC            SECTION.
      *
           MOVE    ZERO        TO  FLG-BG01301
           OPEN    INPUT  BG01301-FILE
      *
           PERFORM 900-BG01301-READ-SEC
      *
           PERFORM UNTIL ( FLG-BG01301 NOT =   ZERO )
      *
               MOVE    BG01301-PTID    TO  WRK-PTID
               MOVE    BG01301-PTNUM   TO  WRK-PTNUM
               PERFORM 2101-SEIKYU-PG-CALL-SEC
               IF    ( FLG-END     NOT =   ZERO )
                   GO  TO  2101-TUKISKY-HEN-EXT
               END-IF
      *
               PERFORM 2101-SYORI-KEIKA-HEN-SEC
      *
               PERFORM 900-BG01301-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   BG01301-FILE
      *
           .
       2101-TUKISKY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    処理経過編集処理
      *****************************************************************
       2101-SYORI-KEIKA-HEN-SEC        SECTION.
      *
           MOVE    ZERO            TO  WRK-SHO
                                       WRK-AMARI
           DIVIDE  50          INTO    CNT-BG01301 GIVING  WRK-SHO
           REMAINDER WRK-AMARI
           END-DIVIDE
      *
           IF    ( WRK-AMARI       NOT =   ZERO )
               GO  TO  2101-SYORI-KEIKA-HEN-EXT
           END-IF
      *
           MOVE    CNT-BG01301     TO  WRK-STR-LEFT-ZX99
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           MOVE    WRK-STR-LEFT    TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT   TO  WRK-BG01301-KNS-N
           PERFORM 800-MCNDATE-SEC
           MOVE    SPACE           TO  WRK-MAE-INPUT
           MOVE    SMCNDATE-HMS(1:2)
                                   TO  WRK-MAE-INPUT(1:2)
           MOVE    "."             TO  WRK-MAE-INPUT(3:1)
           MOVE    SMCNDATE-HMS(3:2)
                                   TO  WRK-MAE-INPUT(4:2)
           MOVE    "."             TO  WRK-MAE-INPUT(6:1)
           MOVE    SMCNDATE-HMS(5:2)
                                   TO  WRK-MAE-INPUT(7:2)
           MOVE    WRK-MAE-INPUT   TO  WRK-BG01301-TIME-N
      *
      *    ステップ途中処理
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           STRING  "処理患者数："      DELIMITED BY SIZE
                   WRK-BG01301-KNS-N   DELIMITED BY SPACE
                   "件　"              DELIMITED BY SIZE
                   WRK-BG01301-TIME-N  DELIMITED BY SPACE
           INTO    JOB-YOBI
           END-STRING
           MOVE    CNT-JOB-UPDCNT      TO  JOB-UPDCNT
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBG013"          TO  JOB-PGID
           MOVE    WRK-TITLE           TO  JOB-SHELLMSG
      *
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
grpsys                                     SPA-AREA
      *
           MOVE    ZERO                TO  CNT-JOB-UPDCNT
      *
           .
       2101-SYORI-KEIKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    外来月別請求書編集処理−患者指定−
      *****************************************************************
       210-PTSHITEI-HEN-SEC            SECTION.
      *
           MOVE    WRK-PARA-PTNUM      TO  WRK-PTNUM
           PERFORM 900-PTNUM-KEY2-SEL-SEC
      *
           MOVE    PTNUM-PTID          TO  WRK-PTID
           MOVE    PTNUM-PTNUM         TO  WRK-PTNUM
           PERFORM 2101-SEIKYU-PG-CALL-SEC
           IF    ( FLG-END         NOT =   ZERO )
               GO  TO  210-PTSHITEI-HEN-EXT
           END-IF
      *
           .
       210-PTSHITEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    外来月別請求書編集サブ呼出処理
      *****************************************************************
       2101-SEIKYU-PG-CALL-SEC          SECTION.
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    SMCNDATE-YMD    TO  WRK-FILEMEI-YMD
           MOVE    SMCNDATE-HMS    TO  WRK-FILEMEI-HMS
      *
           MOVE    WRK-PTID        TO  SPA-PTID
           MOVE    "2"             TO  SPA-NYUGAIKBN
           MOVE    WRK-PTNUM       TO  SPA-PTNUM
           MOVE    PTINF-KANANAME  TO  SPA-KANANAME
           MOVE    PTINF-NAME      TO  SPA-NAME
      *
           MOVE    "001%"          TO  WRK-FILEMEI-CD
           PERFORM 21011-DELETE-PARA-SEC
           IF    ( FLG-END      NOT =   ZERO )
               GO  TO  2101-SEIKYU-PG-CALL-EXT
           END-IF
      *
      *    帳票プログラム取得（外来）
           PERFORM 21011-PRINT-PG-SEC
      *
           MOVE    "1"                 TO  WRK-NYUGAIKBN
           PERFORM 21011-TUKIMISYU-SUM-SEC
           MOVE    WRK-MISYU           TO  WRK-NYU-MISYU
           MOVE    "2"                 TO  WRK-NYUGAIKBN
           PERFORM 21011-TUKIMISYU-SUM-SEC
           MOVE    WRK-MISYU           TO  WRK-GAI-MISYU
           MOVE    WRK-MISYU-P         TO  WRK-GAI-MISYU-P
           MOVE    WRK-MISYU-M         TO  WRK-GAI-MISYU-M
      *
           INITIALIZE                  SYUPARA01-AREA
                                       SYUPARA02-AREA
      *
           MOVE    "2"                 TO  SYUPARA01-NYUGAIKBN
           MOVE    WRK-NYU-MISYU       TO  SYUPARA01-NYU-MISYU
           MOVE    WRK-GAI-MISYU       TO  SYUPARA01-GAI-MISYU
           MOVE    WRK-GAI-MISYU-P     TO  SYUPARA01-GAI-MISYU-P
           MOVE    WRK-GAI-MISYU-M     TO  SYUPARA01-GAI-MISYU-M
           MOVE    ZERO                TO  SYUPARA01-NYU-NYUKIN
           MOVE    ZERO                TO  SYUPARA01-GAI-NYUKIN
           MOVE    4                   TO  SYUPARA01-HAKKOFLG
           EVALUATE    WRK-PARA-HAKUHOUFLG
           WHEN    "2"
               MOVE    2               TO  SYUPARA01-SKYSUMFLG
           WHEN    "3"
               MOVE    3               TO  SYUPARA01-SKYSUMFLG
           WHEN    "4"
               MOVE    4               TO  SYUPARA01-SKYSUMFLG
           WHEN    OTHER
               MOVE    1               TO  SYUPARA01-SKYSUMFLG
           END-EVALUATE
           MOVE    WRK-PARA-ZEROSKYKBN TO  SYUPARA01-ZEROSKYKBN
           MOVE    WRK-PARA-HAKYMD     TO  SYUPARA01-DENPPRTYMD
      *
           IF    ( WRK-PARA-ZENMISYUPRTKBN =   "1" )
               MOVE    ZERO            TO  SYUPARA01-NYU-MISYU
                                           SYUPARA01-GAI-MISYU
                                           SYUPARA01-GAI-MISYU-P
                                           SYUPARA01-GAI-MISYU-M
           END-IF
           MOVE    WRK-PARA-SUMKBN     TO  SYUPARA01-TUKISUMKBN
      *
           INITIALIZE                      PARA-REC
           MOVE    "G01"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "00101"             TO  WRK-FILEMEI-CD
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
           MOVE    SYUPARA01-AREA      TO  PARA-DATA-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               INITIALIZE              ERR-EDIT-AREA
               MOVE    SPA-PTID        TO  ERR-PTID
               MOVE    "外来月別請求書の作成に失敗しました"
                                       TO  ERR-ERRMSG
               PERFORM 800-ERRMSG-EDIT-SEC
               MOVE    ERR-HEN-ERRMSG  TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  2101-SEIKYU-PG-CALL-EXT
           END-IF
      *
           PERFORM 900-SYUNOU-KEY73-SEL-SEC
      *
           MOVE    SPACE               TO  WRK-SRYYMD
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
               IF    ( WRK-SRYYMD      =   SPACE )
                       MOVE    SYU-SRYYMD  TO  WRK-SRYYMD
               ELSE
                   IF    ( WRK-SRYYMD  >   SYU-SRYYMD )
                       MOVE    SYU-SRYYMD  TO  WRK-SRYYMD
                   END-IF
               END-IF
      *
               PERFORM 21011-SYUPARA02-HEN-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  2101-SEIKYU-PG-CALL-EXT
               END-IF
      *
               PERFORM 900-SYUNOU-KEY73-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key73"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           INITIALIZE                      ORCHC03S02AREA
           MOVE    "G01"           TO      ORCHC03S02-GYOUMUID
           MOVE    SPA-TERMID      TO      ORCHC03S02-TERMID
           MOVE    "00101"         TO      WRK-FILEMEI-CD
           MOVE    WRK-FILEMEI     TO      ORCHC03S02-FILEMEI
           CALL    "ORCHC03S02"    USING   SPA-AREA
                                           ORCHC03S02AREA
           IF    ( ORCHC03S02-RC   NOT =   ZERO )
               INITIALIZE              ERR-EDIT-AREA
               MOVE    SPA-PTID        TO  ERR-PTID
               MOVE    "外来月別請求書の作成に失敗しました"
                                       TO  ERR-ERRMSG
               PERFORM 800-ERRMSG-EDIT-SEC
               MOVE    ERR-HEN-ERRMSG  TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  2101-SEIKYU-PG-CALL-EXT
           END-IF
      *
           MOVE    "00102"         TO      WRK-FILEMEI-CD
      *
           MOVE    1               TO      FLG-SKYPRT
                                           FLG-SRYPRT
      *
           MOVE    ZERO            TO      IDX1
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-PRTDEF-MAX )
                    OR   ( IDX1    >   ZERO )
      *
               IF    ( CONST-PRTFLG (IDX0)
                                   =   PTINF-SKYPRTFLG )
                   MOVE    IDX0    TO  IDX1
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-PARA-PTREFKBN       =   "1" )
            AND  ( IDX1                NOT =   ZERO )
               IF    ( CONST-SKYPRTOUT (IDX1)  =   "1" )
                   MOVE    1       TO      FLG-SKYPRT
               ELSE
                   MOVE    ZERO    TO      FLG-SKYPRT
               END-IF
               IF    ( CONST-SRYPRTOUT (IDX1)  =   "1" )
                   MOVE    1       TO      FLG-SRYPRT
               ELSE
                   MOVE    ZERO    TO      FLG-SRYPRT
               END-IF
           END-IF
      *
           EVALUATE    WRK-PARA-PRTFORMKBN
           WHEN    "1"
               MOVE    ZERO    TO      FLG-SRYPRT
           WHEN    "2"
               MOVE    ZERO    TO      FLG-SKYPRT
           END-EVALUATE
      *
           MOVE    WRK-SRYYMD      TO      SPA-SRYYMD
      *
           IF    ( FLG-SKYPRT      =       1 )
               PERFORM 21011-SEIKYU-PG-CALL-SEC
           END-IF
      *
           IF    ( FLG-SRYPRT      =       1 )
               PERFORM 21011-MEISAI-PG-CALL-SEC
           END-IF
      *
           MOVE    "001%"          TO  WRK-FILEMEI-CD
           PERFORM 21011-DELETE-PARA-SEC
           IF    ( FLG-END      NOT =   ZERO )
               GO  TO  2101-SEIKYU-PG-CALL-EXT
           END-IF
      *
           COMPUTE CNT-BG01301 =   CNT-BG01301 +   1
      *
           .
       2101-SEIKYU-PG-CALL-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書サブ呼出処理
      *****************************************************************
       21011-SEIKYU-PG-CALL-SEC        SECTION.
      *
           INITIALIZE                      ORCHC03AREA
           MOVE    4               TO      ORCHC03-HAKKOFLG
           MOVE    ZERO            TO      ORCHC03-HAKHOUFLG
           MOVE    1               TO      ORCHC03-PARAFLG
      *
           MOVE    "G01"           TO      ORCHC03-PARA-GYOUMUID
           MOVE    SPA-TERMID      TO      ORCHC03-PARA-TERMID
           MOVE    "00102"         TO      WRK-FILEMEI-CD
           MOVE    WRK-FILEMEI     TO      ORCHC03-PARA-FILEMEI
           MOVE    "1"             TO      ORCHC03-PRTKBN
           MOVE    5               TO      ORCHC03-MOTOID
      *
           MOVE    WRK-SEIKYU-PRTNM
                                   TO      SPRT-PRTNM
           MOVE    SPA-PTID        TO      SPRT-PTID
           MOVE    WRK-PARA-SRYYM  TO      SPRT-SRYYM
      *
           MOVE    ORCSPRTAREA     TO      ORCHC03-PRTAREA
           CALL    WRK-SEIKYU-PG           USING
                                           SPA-AREA
                                           ORCHC03AREA
      *
           COMPUTE CNT-JOB-UPDCNT  =   CNT-JOB-UPDCNT
                                   +   ORCHC03S02-CNT
      *
           .
       21011-SEIKYU-PG-CALL-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書サブ呼出処理
      *****************************************************************
       21011-MEISAI-PG-CALL-SEC        SECTION.
      *
           PERFORM 900-PARA-KEY5-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PARA    NOT =   ZERO )
      *
               MOVE    PARA-DATA-REC       TO  SYUPARA03-AREA
      *
               INITIALIZE                      ORCHC04AREA
               MOVE    SYUPARA03-DENPNUM   TO  ORCHC04-DENPNUM
               MOVE    4                   TO  ORCHC04-HAKKOFLG
               MOVE    WRK-PARA-HAKUHOUFLG TO  ORCHC04-HAKHOUFLG
               MOVE    WRK-PARA-HAKYMD     TO  ORCHC04-DENPPRTYMD
               MOVE    SYUPARA03-STSRYYMD  TO  ORCHC04-KIKAN-STYMD
               MOVE    SYUPARA03-EDSRYYMD  TO  ORCHC04-KIKAN-EDYMD
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   SYUPARA03-GRP-DENPNUM-MAX )
      *
                   MOVE    SYUPARA03-GRP-DENPNUM (IDXD)
                                           TO  ORCHC04-KIKAN-DENPNUM
                                                               (IDXD)
               END-PERFORM
      *
               MOVE    "1"             TO  ORCHC04-PRTKBN
      *
               MOVE    WRK-MEISAI-PRTNM
                                       TO  SPRT-PRTNM
               MOVE    SPA-PTID        TO  SPRT-PTID
               MOVE    WRK-PARA-SRYYM  TO  SPRT-SRYYM
               MOVE    ORCSPRTAREA     TO  ORCHC04-PRTAREA
      *
               CALL    WRK-MEISAI-PG       USING
                                           SPA-AREA
                                           ORCHC04AREA
      *
               COMPUTE CNT-JOB-UPDCNT
                   =   CNT-JOB-UPDCNT  +   ORCHC04-PAGE
      *
               PERFORM 900-PARA-KEY5-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       21011-MEISAI-PG-CALL-EXT.
           EXIT.
      *************************************************************
      *    パラメタ削除処理
      *************************************************************
       21011-DELETE-PARA-SEC           SECTION.
      *
           INITIALIZE                  PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "G01"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               INITIALIZE              ERR-EDIT-AREA
               MOVE    SPA-PTID        TO  ERR-PTID
               MOVE    "外来月別請求書の作成に失敗しました"
                                       TO  ERR-ERRMSG
               PERFORM 800-ERRMSG-EDIT-SEC
               MOVE    ERR-HEN-ERRMSG  TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       21011-DELETE-PARA-EXT.
           EXIT.
      *****************************************************************
      *    帳票プログラム取得処理
      *****************************************************************
       21011-PRINT-PG-SEC              SECTION.
      *
      *    請求書
           MOVE    SPACE               TO  WRK-SEIKYU-PG
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000003"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYSKANRI-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG     TO  WRK-SEIKYU-PG
           ELSE
              MOVE    "ORCHC03"           TO  WRK-SEIKYU-PG
           END-IF
      *
      *    明細書
           MOVE    SPACE               TO  WRK-MEISAI-PG
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000008"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYSKANRI-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG  TO  WRK-MEISAI-PG
           ELSE
              MOVE    "ORCHC04"        TO  WRK-MEISAI-PG
           END-IF
      *
           .
      *
       21011-PRINT-PG-EXT.
           EXIT.
      **************************************************************
      *   未収金集計処理（月別）
      **************************************************************
       21011-TUKIMISYU-SUM-SEC          SECTION.
      *
           INITIALIZE                  WRK-MISYU-G
      *
           INITIALIZE                  SYUNOU-REC
grpsys*DD  MOVE    SPA-HOSPID          TO  SYU-HOSPID
grpsys     MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           STRING  WRK-PARA-SRYYM      DELIMITED   BY  SPACE
                   "01"                DELIMITED   BY  SIZE
           INTO    SYU-SRYYMD
           END-STRING
      *
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key55"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key55"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           COMPUTE WRK-MISYU-P
               =   SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL
      *
           INITIALIZE                  SYUNOU-REC
grpsys*DD  MOVE    SPA-HOSPID          TO  SYU-HOSPID
grpsys     MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           STRING  WRK-PARA-SRYYM      DELIMITED   BY  SPACE
                   "01"                DELIMITED   BY  SIZE
           INTO    SYU-SRYYMD
           END-STRING
      *
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key56"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key56"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           COMPUTE WRK-MISYU-M
               =   SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL
      *
           COMPUTE WRK-MISYU
               =   WRK-MISYU-P
               +   WRK-MISYU-M
      *
           .
       21011-TUKIMISYU-SUM-EXT.
           EXIT.
      **************************************************************
      *    パラメタ編集処理
      **************************************************************
       21011-SYUPARA02-HEN-SEC         SECTION.
      *
           COMPUTE PARA-EDANUM =   PARA-EDANUM +   1
      *
           INITIALIZE              SYUPARA02-AREA
           MOVE    SYU-DENPNUM     TO  SYUPARA02-DENPNUM
           MOVE    SYU-NYUKIN-TOTAL TO  SYUPARA02-NYUKIN
           MOVE    SYUPARA02-AREA  TO  PARA-DATA-REC
grpsys     MOVE    SPA-HOSPNUM     TO  PARA-HOSPNUM
           MOVE    PARA-REC    TO  MCPDATA-REC
           MOVE    "tbl_para"      TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4002"      TO  SPA-ERRCD
           END-IF
      *
           .
       21011-SYUPARA02-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF        ( STS-RECEERR     =   ZERO )
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
grpsys         MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
grpsys                                     SPA-AREA
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ファイル削除
           PERFORM 800-BG01301-DEL-SEC
      *
           IF    ( CNT-BG01301     =   ZERO )
               MOVE    "処理対象のデータがありませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBG01301 IN " CNT-BG01301  " ***"
           DISPLAY "*** ORCBG01301 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-JOB-UPDCNT  TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
grpsys                                 SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
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
      *    文字列左詰処理
      *****************************************************************
       800-STR-HIDARIZUME-HEN-SEC      SECTION.
      *
           MOVE    WRK-STR-LEFT    TO  IDXE
           PERFORM VARYING IDXE    FROM    1   BY  1
                   UNTIL ( IDXE    >       500 )
      *
               IF    ( WRK-STR-LEFT (IDXE:1)
                                       NOT =   SPACE )
                   MOVE    WRK-STR-LEFT (IDXE:)
                                           TO  WRK-STR-LEFT-TMP
                   MOVE    WRK-STR-LEFT-TMP
                                           TO  WRK-STR-LEFT
                   MOVE    500             TO  IDXE
               END-IF
      *
           END-PERFORM
      *
           .
       800-STR-HIDARIZUME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    対象患者ファイル削除処理
      *****************************************************************
       800-BG01301-DEL-SEC            SECTION.
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
grpsys*DD  MOVE    ORCBG01301PARA TO  IF-FILENAME
grpsys     MOVE    BG01301PARA     TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           .
       800-BG01301-DEL-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       800-ERRMSG-EDIT-SEC         SECTION.
      *
           MOVE    SPACE           TO  ERR-HEN-ERRMSG
      *
           IF    ( ERR-PTNUM       =   SPACE )
               MOVE    ERR-PTID    TO  WRK-PTID
               PERFORM 900-PTNUM-KEY-SEL-SEC
               IF    ( FLG-PTNUM   =   ZERO )
                   MOVE    PTNUM-PTNUM TO  ERR-PTNUM
               ELSE
                   STRING "?(ID:"      DELIMITED   BY  SIZE
                      ERR-PTID         DELIMITED   BY  SIZE
                      ")"              DELIMITED   BY  SIZE
                   INTO   ERR-PTNUM
                   END-STRING
               END-IF
           END-IF
      *
           MOVE    1               TO  WRK-LEN
           MOVE    SPACE           TO  WRK-STR
           STRING  ERR-PTNUM           DELIMITED   BY  SPACE
           INTO    WRK-STR     WITH    POINTER WRK-LEN
           END-STRING
      *
           COMPUTE WRK-LEN         =   WRK-LEN -   1
      *
           MOVE    ZERO            TO  WRK-SHO
                                       WRK-AMARI
           DIVIDE  2   INTO    WRK-LEN GIVING  WRK-SHO
           REMAINDER WRK-AMARI
           END-DIVIDE
      *
           IF    ( WRK-AMARI   =   ZERO )
               STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                       "［患者番号："      DELIMITED   BY  SIZE
                       ERR-PTNUM           DELIMITED   BY  SPACE
                       "］"                DELIMITED   BY  SIZE
               INTO    ERR-HEN-ERRMSG
               END-STRING
           ELSE
               STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                       "［患者番号:"       DELIMITED   BY  SIZE
                       ERR-PTNUM           DELIMITED   BY  SPACE
                       "］"                DELIMITED   BY  SIZE
               INTO    ERR-HEN-ERRMSG
               END-STRING
           END-IF
      *
           .
       800-ERRMSG-EDIT-EXT.
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
      *    対象患者ファイル読込処理
      *****************************************************************
       900-BG01301-READ-SEC          SECTION.
      *
           READ    BG01301-FILE       NEXT
           AT  END
               MOVE    1               TO  FLG-BG01301
           END-READ
      *
           .
       900-BG01301-READ-EXT.
           EXIT.
      *****************************************************************
      *    対象患者ファイル読込処理
      *****************************************************************
       900-BG01301-READ-DYN-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-BG01301
      *
           READ    BG01301-FILE
           INVALID
               MOVE    1               TO  FLG-BG01301
           END-READ
      *
           .
       900-BG01301-READ-DYN-EXT.
           EXIT.
      *****************************************************************
      *    収納情報検索処理(KEY52)
      *****************************************************************
       900-SYUNOU-KEY52-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           INITIALIZE                  SYUNOU-REC
grpsys*DD  MOVE    SPA-HOSPID      TO  SYU-HOSPID
grpsys     MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
grpsys*DD  MOVE    SPA-NYUGAIKBN   TO  SYU-NYUGAIKBN
grpsys     STRING  SPA-NYUGAIKBN       DELIMITED   BY  SPACE
grpsys             "%"                 DELIMITED   BY  SIZE
grpsys     INTO    SYU-NYUGAIKBN
grpsys     END-STRING
           STRING  WRK-PARA-SRYYM      DELIMITED   BY  SPACE
                   "%"                 DELIMITED   BY  SIZE
           INTO    SYU-SRYYMD
           END-STRING
      *
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key52"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY52-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納情報ＦＥＴＣＨ処理(KEY52)
      *****************************************************************
       900-SYUNOU-KEY52-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key52"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY52-FET-EXT.
           EXIT.
      *****************************************************************
      *    バッチパラメタ検索処理
      *****************************************************************
       900-BTPARA-KEY8-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BTPARA
      *
           INITIALIZE                  BTPARA-REC
           MOVE    SPA-HOSPNUM     TO  BTPARA-HOSPNUM
           MOVE    CONST-BTPARA-SHELLID
                                   TO  BTPARA-SHELLID
           MOVE    CONST-BTPARA-JOBID
                                   TO  BTPARA-JOBID
           MOVE    CONST-BTPARA-DSP-RENNUM
                                   TO  BTPARA-DSP-RENNUM
      *
           MOVE    BTPARA-REC      TO  MCPDATA-REC
           MOVE    "tbl_btpara"    TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BTPARA-REC
           ELSE
               INITIALIZE              BTPARA-REC
               MOVE    1           TO  FLG-BTPARA
           END-IF
      *
           .
       900-BTPARA-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    バッチパラメタ検索処理
      *****************************************************************
       900-BTPARA-KEY8-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BTPARA
      *
           MOVE    "tbl_btpara"    TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BTPARA-REC
           ELSE
               INITIALIZE              BTPARA-REC
               MOVE    1           TO  FLG-BTPARA
           END-IF
      *
           .
       900-BTPARA-KEY8-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC   SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号情報検索処理(KEY)
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
grpsys*DD  MOVE    WRK-PARA-HOSPID TO  PTNUM-HOSPID
grpsys     MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    WRK-PTID        TO  PTNUM-PTID
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNUM-REC
           ELSE
               MOVE    1           TO  FLG-PTNUM
               INITIALIZE              PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号情報検索処理(KEY2)
      *****************************************************************
       900-PTNUM-KEY2-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
grpsys*DD  MOVE    WRK-PARA-HOSPID TO  PTNUM-HOSPID
grpsys     MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           STRING  WRK-PTNUM       DELIMITED   BY  SPACE
                   "%"             DELIMITED   BY  SIZE
           INTO    PTNUM-PTNUM
           END-STRING
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNUM-REC
           ELSE
               MOVE    1           TO  FLG-PTNUM
               INITIALIZE              PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理(KEY)
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
grpsys*DD  MOVE    WRK-PARA-HOSPID TO  PTINF-HOSPID
grpsys     MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
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
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-KEY73-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    WRK-PARA-SRYYM      TO  SYU-SRYYMD
           MOVE     "__"               TO  SYU-SRYYMD (7:2)
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key73"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-SYUNOU-KEY73-FET-SEC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY73-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納マスターFETCH処理
      **************************************************************
       900-SYUNOU-KEY73-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key73"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  SYUNOU-REC
      *
                   IF    ( WRK-PARA-SRYKA  =   SPACE )
                           MOVE    1       TO  FLG-LOOPEND
                   ELSE
                       IF    ( WRK-PARA-SRYKA  =   SYU-SRYKA )
                           MOVE    1       TO  FLG-LOOPEND
                       END-IF
                   END-IF
               ELSE
                   INITIALIZE                  SYUNOU-REC
                   MOVE    1               TO  FLG-SYUNOU
               END-IF
      *
           END-PERFORM
      *
           .
       900-SYUNOU-KEY73-FET-EXT.
           EXIT.
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    WRK-DENPNUM         TO  SYU-DENPNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブル検索
      *****************************************************************
       900-PARA-KEY5-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           INITIALIZE                      PARA-REC
           MOVE    "G01"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PARA-REC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           .
       900-PARA-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブルFETCH
      *****************************************************************
       900-PARA-KEY5-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PARA-REC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           .
       900-PARA-KEY5-FET-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
           IF        ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       911-DBSELECT-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢインサート処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　切断処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
