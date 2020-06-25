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
       PROGRAM-ID.             ORCBG012.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 調整金一覧表
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/12/03    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-森脇    07/06/06  グループ診療対応
      *  04.08.01    NACL-森脇    15/08/24  ＣＳＶ出力対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    調整金情報ソートファイル
           SELECT  BG012SRT-FILE   ASSIGN   BG012SRTPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BG012SRT-KEY
                                   FILE    STATUS  IS  STS-BG012SRT.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *
      *    調整金情報ソートファイル
       FD  BG012SRT-FILE.
       01  BG012SRT-REC.
           02  BG012SRT-KEY.
               03  BG012SRT-KEY-SRYKA          PIC X(02).
               03  BG012SRT-KEY-CHOSEIKBN      PIC X(01).
               03  BG012SRT-KEY-SEQ            PIC 9(08).
               03  BG012SRT-KEY-DUMMY          PIC X(139).
           02  BG012SRT-PTNUM                  PIC X(20).
           02  BG012SRT-NAME                   PIC X(100).
           02  BG012SRT-DAT.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-//
                                           BY //BG012SRT-SYU-//.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    患者番号順
       01  BG012SRT01-REC.
           02  BG012SRT01-KEY.
               03  BG012SRT01-KEY-SRYKA        PIC X(002).
               03  BG012SRT01-KEY-CHOSEIKBN    PIC X(001).
               03  BG012SRT01-KEY-PTNUM        PIC X(020).
               03  BG012SRT01-KEY-SRYYMD       PIC X(008).
               03  BG012SRT01-KEY-DENPNUM      PIC 9(007).
               03  BG012SRT01-KEY-SEQ          PIC 9(008).
               03  BG012SRT01-KEY-DUMMY        PIC X(104).
           02  BG012SRT01-PTNUM                PIC X(20).
           02  BG012SRT01-NAME                 PIC X(100).
           02  BG012SRT01-DAT.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-//
                                           BY //BG012SRT01-SYU-//.
      *
      *    診療年月日
       01  BG012SRT02-REC.
           02  BG012SRT02-KEY.
               03  BG012SRT02-KEY-SRYKA        PIC X(002).
               03  BG012SRT02-KEY-CHOSEIKBN    PIC X(001).
               03  BG012SRT02-KEY-SRYYMD       PIC X(008).
               03  BG012SRT02-KEY-DENPNUM      PIC 9(007).
               03  BG012SRT02-KEY-PTNUM        PIC X(020).
               03  BG012SRT02-KEY-SEQ          PIC 9(008).
               03  BG012SRT02-KEY-DUMMY        PIC X(104).
           02  BG012SRT02-PTNUM                PIC X(20).
           02  BG012SRT02-NAME                 PIC X(100).
           02  BG012SRT02-DAT.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-//
                                           BY //BG012SRT02-SYU-//.
      *
      *    伝票番号
       01  BG012SRT03-REC.
           02  BG012SRT03-KEY.
               03  BG012SRT03-KEY-SRYKA        PIC X(002).
               03  BG012SRT03-KEY-CHOSEIKBN    PIC X(001).
               03  BG012SRT03-KEY-DENPNUM      PIC 9(007).
               03  BG012SRT03-KEY-SEQ          PIC 9(008).
               03  BG012SRT03-KEY-DUMMY        PIC X(132).
           02  BG012SRT03-PTNUM                PIC X(20).
           02  BG012SRT03-NAME                 PIC X(100).
           02  BG012SRT03-DAT.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-//
                                           BY //BG012SRT03-SYU-//.
      *
      *    カナ氏名順
       01  BG012SRT04-REC.
           02  BG012SRT04-KEY.
               03  BG012SRT04-KEY-SRYKA        PIC X(002).
               03  BG012SRT04-KEY-CHOSEIKBN    PIC X(001).
               03  BG012SRT04-KEY-KANANAME     PIC X(100).
               03  BG012SRT04-KEY-PTNUM        PIC X(020).
               03  BG012SRT04-KEY-SRYYMD       PIC X(008).
               03  BG012SRT04-KEY-DENPNUM      PIC 9(007).
               03  BG012SRT04-KEY-SEQ          PIC 9(008).
               03  BG012SRT04-KEY-DUMMY        PIC X(004).
           02  BG012SRT04-PTNUM                PIC X(20).
           02  BG012SRT04-NAME                 PIC X(100).
           02  BG012SRT04-DAT.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-//
                                           BY //BG012SRT04-SYU-//.
      *
      *    調整金情報ソートファイル
           COPY    "CPCOMMONDAT2.INC"
                                           REPLACING   //RECE01//
                                           BY          //BG012SRT//.
      *
           COPY    "CPRECEERR.INC".
      *
      *    印刷用領域
           COPY    "HCM42V01.INC"          REPLACING  //HCM42V01//
                                           BY         //HCM42//.
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-RECEERR                         PIC X(02).
           03  STS-BG012SRTCL                      PIC X(02).
           03  STS-BG012SRT                        PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYUNOU                          PIC 9(01).
           03  FLG-BG012SRT                        PIC 9(01).
           03  FLG-PTNUM                           PIC 9(01).
           03  FLG-PTINF                           PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-HIZUKE-ERR                      PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                            PIC 9(05).
           03  CNT-LINE                            PIC 9(05).
           03  CNT-SEQ                             PIC 9(08).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                                PIC 9(05).
           03  IDXC                                PIC 9(05).
           03  IDX-TTL                             PIC 9(05).
      *----(04.08.01)--ADD-START---
           03  IDW2                                PIC 9(04).
      *----(04.08.01)--ADD-END-----
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID                      PIC 9(07).
           03  WRK-PARA-SHELLID                    PIC X(08).
grpsys*DD  03  WRK-PARA-HOSPID                     PIC X(24).
grpsys     03  WRK-PARA-HOSPNUM                    PIC 9(02).
           03  WRK-PARA-KKNKBN                     PIC X(01).
           03  WRK-PARA-KKNSTYM                    PIC X(06).
           03  WRK-PARA-KKNEDYM                    PIC X(06).
           03  WRK-PARA-SRTKBN                     PIC X(01).
           03  WRK-PARA-KAKBN                      PIC X(01).
           03  WRK-PARA-NYUGAIKBN                  PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR                         PIC X(200).
      *----(04.08.01)--UPD-START---
           03  WRK-TOUKEIERR                       PIC X(200).
           03  WRK-SYMD.
               05  WRK-SYY                         PIC 9(04).
               05  WRK-SMM                         PIC 9(02).
               05  WRK-SDD                         PIC 9(02).
      *----(04.08.01)--UPD-END-----
           03  WRK-EDTYMD1                         PIC X(10).
           03  WRK-EDTYMD3                         PIC X(22).
           03  WRK-TITLE                           PIC X(100).
           03  WRK-PTID                            PIC 9(10).
           03  WRK-SRYKALST-G.
               05  WRK-SRYKALST-MAX                PIC 9(05).
               05  WRK-SRYKALST-OCC                OCCURS 50.
                   07  WRK-SRYKAL                  PIC X(02).
                   07  WRK-SRYKAL-STYUKYMD         PIC X(08).
                   07  WRK-SRYKAL-EDYUKYMD         PIC X(08).
                   07  WRK-SRYKAMEIL               PIC X(80).
                   07  WRK-SRYKAMEI1L              PIC X(20).
           03  WRK-KKN-HEN.
               05  WRK-KKNSTYMD                    PIC X(16).
               05  WRK-KKNMID                      PIC X(02).
               05  WRK-KKNEDYMD                    PIC X(16).
           03  WRK-ZZ9                             PIC ZZ9.
           03  WRK-CHOSEI-G.
               05  WRK-CHOSEI                      PIC ---,---,--9.
               05  WRK-CHOSEIKNS                   PIC ZZ,ZZ9.
               05  WRK-CHOSEI-SGK                  PIC S9(11).
           03  WRK-DENPNUM-G.
               05  WRK-DENPNUM                     PIC 9(07).
           03  WRK-SRYYMDM-G.
               05  WRK-SRYYMDM-ST                  PIC X(09).
               05  WRK-SRYYMDM-MID                 PIC X(01).
               05  WRK-SRYYMDM-ED                  PIC X(05).
           03  WRK-SRT-SRYKA                       PIC X(02).
           03  WRK-SRT-SRYYMD                      PIC X(08).
           03  WRK-SPRT-SRYYM                      PIC X(06).
           03  WRK-KKNYM                           PIC X(100).
           03  WRK-CHOSEIMEI                       PIC X(20).
      *
      *----(04.08.01)--ADD-START---
      *    ＣＳＶデータ数字編集用
           03  WRK-REC             PIC X(20000).
           03  WRK-REC-LENGTH      PIC 9(05).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA        PIC X(20000).
               05  WRK-NAGASA      PIC 9(04).
               05  WRK-RECSTR      PIC 9(02). 
               05  WRK-SYOSUU      PIC 9(01).
               05  WRK-RECEND      PIC 9(01).
      *
      *    ＣＳＶデータ日付編集用
       01  WRK-HEN-YMD-OT.
           03  WRK-HEN-YY-OT       PIC X(04).
           03  FILLER              PIC X(01)   VALUE   "/". 
           03  WRK-HEN-MM-OT       PIC X(02).
           03  FILLER              PIC X(01)   VALUE   "/". 
           03  WRK-HEN-DD-OT       PIC X(02).
      *
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI          PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO          PIC X(01)   VALUE   X"0D".
      *
      *    ＣＳＶデータファイル名
       01  WRK-TO-DATA-G.
           03  WRK-TO-DATA.
               05  WRK-TO-DATA-HOSPNUM PIC 9(02).
               05  FILLER              PIC X(10)   VALUE   "choseikin_".
               05  WRK-TO-DATA-KIKAN   PIC X(24).
           03  WRK-TO-DATA-NYUGAI      PIC X(06).
           03  WRK-TO-DATA1.
               05  FILLER              PIC X(04)   VALUE   ".csv".
           03  WRK-TO-DATA2.
               05  FILLER              PIC X(01)   VALUE   "_".
               05  WRK-TO-DATA-STRYM   PIC X(06).
               05  FILLER              PIC X(01)   VALUE   "-".
               05  WRK-TO-DATA-ENDYM   PIC X(06).
               05  FILLER              PIC X(04)   VALUE   ".csv".
      *
       01  WRK-CSV-AREA.
           03  WRK-CSV-CHOSEIKBN   PIC X(08).
           03  WRK-CSV-PTNUM       PIC X(20).
           03  WRK-CSV-NAME        PIC X(100).
           03  WRK-CSV-DENPNUM     PIC 9(07).
           03  WRK-CSV-SRYKA       PIC X(02).
           03  WRK-CSV-SRYKAMEI    PIC X(20).
           03  WRK-CSV-STYMD       PIC X(10).
           03  WRK-CSV-EDYMD       PIC X(10).
           03  WRK-CSV-CHOSEIP     PIC S9(07).
           03  WRK-CSV-CHOSEIM     PIC S9(07).
           03  WRK-CSV-Z7          PIC -999999.
           03  WRK-CSV-Z11         PIC -9999999999.
      *
      *    ＣＳＶデータヘッダー情報
       01  CSV-HEAD-01.
           03  FILLER              PIC X(06)   VALUE   "並び順".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-HEAD-SRTKBN     PIC X(12)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "対象期間".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-HEAD-KKNYMS     PIC X(08)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-HEAD-KKNYME     PIC X(08)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-HEAD-NYUGAI     PIC X(04)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "作成日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-HEAD-SKYYMD     PIC X(10)   VALUE   SPACE.
      *
       01  CSV-HEAD-02.
           03  FILLER              PIC X(12)   VALUE   "調整金１／２".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "伝票番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "診療科".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "診療科名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "診療年月日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "調整金（＋）".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "調整金（−）".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "調整金計".
      *----(04.08.01)--ADD-END-----
      *
       01  HEAD-AREA.
           03  HEAD-NYUGAI                         PIC X(10).
           03  HEAD-KKNYM                          PIC X(34).
           03  HEAD-SAKYMD                         PIC X(22).
           03  HEAD-SRTKBN                         PIC X(20).
      *
       01  TTL-AREA.
           03  TTL-KNS                             PIC 9(06).
           03  TTL-TITLE                           PIC X(20).
           03  TTL-CHOSEIP                         PIC S9(11).
           03  TTL-CHOSEIM                         PIC S9(11).
           03  TTL-SRYKA-G.
               05  TTL-SRYKA-OCC                   OCCURS  2.
                   07  TTL-SRYKA-KNS               PIC 9(06).
                   07  TTL-SRYKA-CHOSEIP           PIC S9(11).
                   07  TTL-SRYKA-CHOSEIM           PIC S9(11).
           03  TTL-TTL-G.
               05  TTL-TTL-OCC                     OCCURS  2.
                   07  TTL-TTL-KNS                 PIC 9(06).
                   07  TTL-TTL-CHOSEIP             PIC S9(11).
                   07  TTL-TTL-CHOSEIM             PIC S9(11).
      *
       01  ERR-EDIT-AREA.
           03  ERR-PTID                            PIC 9(10).
           03  ERR-PTNUM                           PIC X(20).
           03  ERR-ERRMSG                          PIC X(80).
           03  ERR-HEN-ERRMSG                      PIC X(80).
      *
       01  KEY-AREA.
           03  KEY-SRYKA                           PIC X(02).
           03  KEY-SRYKAMEI                        PIC X(10).
           03  KEY-CHOSEIKBN                       PIC X(01).
      *
       01  CONST-AREA.
           03  CONST-LINE-MAX              PIC 9(05)   VALUE 30.
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    診療科情報
           COPY    "CPSK1005.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
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
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *----(04.08.01)--ADD-START---
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *----(04.08.01)--ADD-END-----
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *    COPY    "ORCA-DBPATH".
      *
           COPY    "COMMON-SPA".
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
           PERFORM UNTIL ( FLG-END NOT =   ZERO )
               PERFORM 200-MAIN-SEC
           END-PERFORM
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
           INITIALIZE                  HEAD-AREA
           INITIALIZE                  TTL-AREA
           INITIALIZE                  KEY-AREA
           INITIALIZE                  ERR-EDIT-AREA
grpsys     INITIALIZE                  SPA-AREA
      *
           INITIALIZE                  RECEERR
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
                                       WRK-PARA-KKNKBN
                                       WRK-PARA-KKNSTYM
                                       WRK-PARA-KKNEDYM
                                       WRK-PARA-SRTKBN
                                       WRK-PARA-KAKBN
                                       WRK-PARA-NYUGAIKBN
grpsys     END-UNSTRING
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    SPACE               TO  WRK-TITLE
      *
           IF    ( WRK-PARA-NYUGAIKBN  =   "1" )
               MOVE    "調整金一覧表−入院"
                                       TO  WRK-TITLE
           ELSE
               MOVE    "2"             TO  WRK-PARA-NYUGAIKBN
               MOVE    "調整金一覧表−外来"
                                       TO  WRK-TITLE
           END-IF
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBG012"          TO  JOB-PGID
           MOVE    WRK-TITLE           TO  JOB-SHELLMSG
      *
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
grpsys                                     SPA-AREA
      *
grpsys     MOVE    "BG01201"          TO  BG012SRTPARA-FILE-ID
grpsys     MOVE    LNK-PRTKANRI-TERMID TO  BG012SRTPARA-TERMID
grpsys     MOVE    SPA-HOSPNUM         TO  BG012SRTPARA-HOSPNUM
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    BG012SRTPARA-BASENAME
                                   TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  BG012SRTPARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  RECEERR
      *
           MOVE    ZERO                TO  FLG-END
      *
      *    パラメータチェック処理
           PERFORM 1001-PARA-CHK-SEC
           IF    ( FLG-END         NOT =   ZERO )
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    診療科ワーク編集処理
           PERFORM 1001-SRYKALST-HEN-SEC
      *
      *    ヘッダー編集処理
           PERFORM 1001-HEADER-HEN-SEC
      *
      *    調整金情報ソートファイル編集処理
           PERFORM 1001-BG012SRT-HEN-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    パラメータチェック処理
      *****************************************************************
       1001-PARA-CHK-SEC               SECTION.
      *
      *    期間指定区分チェック
           PERFORM 1001-KKNKBN-CHK-SEC
           IF    ( FLG-END         NOT =   ZERO )
               GO  TO  1001-PARA-CHK-EXT
           END-IF
      *
      *    期間指定区分＝１のとき
           IF    ( WRK-PARA-KKNKBN     =   "1" )
      *
      *        期間開始年月チェック
               PERFORM 1001-KKNSTYM-CHK-SEC
               IF    ( FLG-END         NOT =   ZERO )
                   GO  TO  1001-PARA-CHK-EXT
               END-IF
      *
      *        期間終了年月チェック
               IF    ( WRK-PARA-KKNEDYM    NOT =   SPACE )
                   PERFORM 1001-KKNEDYM-CHK-SEC
                   IF    ( FLG-END         NOT =   ZERO )
                       GO  TO  1001-PARA-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
      *----(04.08.01)--ADD-START---
      *    ＣＳＶ出力ファイル名設定
           MOVE    SPA-HOSPNUM     TO  WRK-TO-DATA-HOSPNUM
      *
           MOVE    SPACE           TO  WRK-TO-DATA-NYUGAI
           MOVE    SPACE           TO  WRK-TO-DATA-KIKAN
      *
           IF    ( WRK-PARA-NYUGAIKBN  =   "1" )
               MOVE    "nyuin"             TO  WRK-TO-DATA-NYUGAI
           ELSE
               MOVE    "gairai"            TO  WRK-TO-DATA-NYUGAI
           END-IF
      *
           IF    ( WRK-PARA-KKNKBN     =   "1" )
               MOVE    WRK-PARA-KKNSTYM        TO  WRK-TO-DATA-STRYM
               MOVE    WRK-PARA-KKNEDYM        TO  WRK-TO-DATA-ENDYM
               STRING  WRK-TO-DATA-NYUGAI      DELIMITED BY SPACE
                       WRK-TO-DATA2            DELIMITED BY SPACE
                       INTO                    WRK-TO-DATA-KIKAN
               END-STRING
           ELSE
               STRING  WRK-TO-DATA-NYUGAI      DELIMITED BY SPACE
                       WRK-TO-DATA1            DELIMITED BY SPACE
                       INTO                    WRK-TO-DATA-KIKAN
               END-STRING
           END-IF
      *----(04.08.01)--ADD-END-----
           .
       1001-PARA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間指定区分チェック
      *****************************************************************
       1001-KKNKBN-CHK-SEC             SECTION.
      *
      *    期間指定区分＝０，１であること
           IF    ( WRK-PARA-KKNKBN     =   "0" OR  "1" )
               CONTINUE
           ELSE
               MOVE    SPACE           TO  WRK-RECEERR
               STRING  "期間指定区分が誤りです［"
                                                   DELIMITED   BY  SIZE
                       WRK-PARA-KKNKBN             DELIMITED   BY  SIZE
                       "］"                        DELIMITED   BY  SIZE
               INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  1001-KKNKBN-CHK-EXT
           END-IF
      *
           .
       1001-KKNKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間開始年月チェック
      *****************************************************************
       1001-KKNSTYM-CHK-SEC            SECTION.
      *
      *    開始年月が設定されていること
           IF    ( WRK-PARA-KKNSTYM    NOT =   SPACE )
               CONTINUE
           ELSE
               MOVE    "開始年月が未設定です"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  1001-KKNSTYM-CHK-EXT
           END-IF
      *
      *    開始年月が"000000"か日付として妥当であること
           IF    ( WRK-PARA-KKNSTYM    =   "000000" )
               CONTINUE
           ELSE
               MOVE    WRK-PARA-KKNSTYM    TO  WRK-SYMD
               MOVE    "01"            TO  WRK-SYMD (7:2)
               PERFORM 800-SEIWA-HEN-SEC
               IF    ( FLG-HIZUKE-ERR  =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    SPACE           TO  WRK-RECEERR
                   STRING  "開始年月が誤りです［"
                                               DELIMITED   BY  SIZE
                       WRK-PARA-KKNSTYM        DELIMITED   BY  SIZE
                       "］"                    DELIMITED   BY  SIZE
                   INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  1001-KKNSTYM-CHK-EXT
               END-IF
           END-IF
      *
           .
       1001-KKNSTYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間終了年月チェック
      *****************************************************************
       1001-KKNEDYM-CHK-SEC            SECTION.
      *
      *    終了年月が"999999"か日付として妥当であること
           IF    ( WRK-PARA-KKNEDYM    =   "999999" )
               CONTINUE
           ELSE
               MOVE    WRK-PARA-KKNEDYM    TO  WRK-SYMD
               MOVE    "01"            TO  WRK-SYMD (7:2)
               PERFORM 800-SEIWA-HEN-SEC
               IF    ( FLG-HIZUKE-ERR  =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    SPACE           TO  WRK-RECEERR
                   STRING  "終了年月が誤りです［"
                                               DELIMITED   BY  SIZE
                       WRK-PARA-KKNEDYM        DELIMITED   BY  SIZE
                       "］"                    DELIMITED   BY  SIZE
                   INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  1001-KKNEDYM-CHK-EXT
               END-IF
           END-IF
      *
      *    開始年月≦終了年月であること
           IF    ( WRK-PARA-KKNSTYM   <=   WRK-PARA-KKNEDYM )
               CONTINUE
           ELSE
               MOVE    "開始年月＞終了年月となっています"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  1001-KKNEDYM-CHK-EXT
           END-IF
      *
           .
       1001-KKNEDYM-CHK-EXT.
           EXIT.
      *************************************************************
      *    診療科ワーク編集処理
      *************************************************************
       1001-SRYKALST-HEN-SEC           SECTION.
      *
           INITIALIZE  WRK-SRYKALST-G
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           INITIALIZE  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
grpsys*DD  MOVE    PATH-TBL-SYSKANRI-KEY5  TO  MCP-PATH
grpsys     MOVE    "tbl_syskanri"  TO  MCP-TABLE
grpsys     MOVE    "key5"          TO  MCP-PATHNAME

           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYS-1005-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( WRK-SRYKALST-MAX   >=   50   )
      *
               COMPUTE WRK-SRYKALST-MAX
                   =   WRK-SRYKALST-MAX    +   1
      *
               MOVE    WRK-SRYKALST-MAX    TO  IDX1
      *
               MOVE    SYS-1005-KBNCD      TO  WRK-SRYKAL    (IDX1)
               MOVE    SYS-1005-STYUKYMD   TO  WRK-SRYKAL-STYUKYMD
                                                             (IDX1)
               MOVE    SYS-1005-EDYUKYMD   TO  WRK-SRYKAL-EDYUKYMD
                                                             (IDX1)
               MOVE    SYS-1005-SRYKANAME  TO  WRK-SRYKAMEIL (IDX1)
               MOVE    SYS-1005-SRYKANAME1 TO  WRK-SRYKAMEI1L(IDX1)
      *
grpsys         MOVE    "tbl_syskanri"  TO  MCP-TABLE
grpsys         MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  SYS-1005-REC
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
                   INITIALIZE                  SYS-1005-REC
               END-IF
      *
           END-PERFORM
      *
grpsys     MOVE    "tbl_syskanri"  TO  MCP-TABLE
grpsys     MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-SRYKALST-HEN-EXT.
           EXIT.
      *----(04.08.01)--UPD-START---
      *************************************************************
      *    ヘッダー編集処理
      *************************************************************
       1001-HEADER-HEN-SEC             SECTION.
      *
           MOVE    "000000"            TO  WRK-SPRT-SRYYM
      *
      *    タイトル
           IF    ( WRK-PARA-NYUGAIKBN  =   "1" )
               MOVE    "（入院）"      TO  HEAD-NYUGAI
               MOVE    "入院"          TO  CSV-HEAD-NYUGAI
           ELSE
               MOVE    "（外来）"      TO  HEAD-NYUGAI
               MOVE    "外来"          TO  CSV-HEAD-NYUGAI
           END-IF
      *
      *    ソート区分
           EVALUATE    WRK-PARA-SRTKBN
           WHEN    "1"
               MOVE    "診療年月日順"  TO  HEAD-SRTKBN
           WHEN    "2"
               MOVE    "伝票番号順"    TO  HEAD-SRTKBN
           WHEN    "3"
               MOVE    "カナ氏名順"    TO  HEAD-SRTKBN
           WHEN    OTHER
               MOVE    "患者番号順"    TO  HEAD-SRTKBN
           END-EVALUATE
      *
           MOVE    HEAD-SRTKBN         TO  CSV-HEAD-SRTKBN
      *
      *    作成日
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3         TO  HEAD-SAKYMD
      *
           MOVE    WRK-SYY             TO  WRK-HEN-YY-OT
           MOVE    WRK-SMM             TO  WRK-HEN-MM-OT
           MOVE    WRK-SDD             TO  WRK-HEN-DD-OT
           MOVE    WRK-HEN-YMD-OT      TO  CSV-HEAD-SKYYMD
      *
           IF    ( WRK-PARA-KKNKBN     =   "1" )
      *
               IF    ( WRK-PARA-KKNSTYM    =   "000000" )
                AND  ( WRK-PARA-KKNEDYM    =   "999999" )
                   MOVE    "指定なし"              TO  HEAD-KKNYM
                                                       CSV-HEAD-KKNYMS
               ELSE
      *            期間
                   INITIALIZE  WRK-KKN-HEN
      *
                   IF    ( WRK-PARA-KKNSTYM    NOT =   "000000" )
                       MOVE    WRK-PARA-KKNSTYM    TO  WRK-SYMD
                       MOVE    "01"                TO  WRK-SYMD(7:2)
                       PERFORM 800-SEIWA-HEN-SEC
                       MOVE    WRK-EDTYMD3 (1:16)  TO  WRK-KKNSTYMD
                       MOVE    WRK-PARA-KKNSTYM    TO  WRK-SPRT-SRYYM
      *
                       MOVE    WRK-SYY             TO  WRK-HEN-YY-OT
                       MOVE    WRK-SMM             TO  WRK-HEN-MM-OT
                       MOVE    WRK-SDD             TO  WRK-HEN-DD-OT
                       MOVE    WRK-HEN-YMD-OT(1:7) TO  CSV-HEAD-KKNYMS
                   END-IF
      *
                   IF    ( WRK-PARA-KKNEDYM    NOT =   SPACE )
                       MOVE    "〜"                    TO  WRK-KKNMID
                   END-IF
      *
                   IF    ( WRK-PARA-KKNEDYM        =   SPACE
                                                   OR  "999999" )
                       MOVE    SPACE               TO  WRK-KKNEDYMD
                   ELSE
                       MOVE    WRK-PARA-KKNEDYM    TO  WRK-SYMD
                       MOVE    "01"                TO  WRK-SYMD(7:2)
                       PERFORM 800-SEIWA-HEN-SEC
                       MOVE    WRK-EDTYMD3 (1:16)  TO  WRK-KKNEDYMD
      *
                       MOVE    WRK-SYY             TO  WRK-HEN-YY-OT
                       MOVE    WRK-SMM             TO  WRK-HEN-MM-OT
                       MOVE    WRK-SDD             TO  WRK-HEN-DD-OT
                       MOVE    WRK-HEN-YMD-OT(1:7) TO  CSV-HEAD-KKNYME
                   END-IF
      *
                   IF    ( WRK-KKNSTYMD        =   WRK-KKNEDYMD )
                       MOVE    WRK-KKNSTYMD        TO  HEAD-KKNYM
                   ELSE
                       MOVE    WRK-KKN-HEN         TO  HEAD-KKNYM
                   END-IF
               END-IF
      *
           ELSE
               MOVE    "指定なし"                  TO  HEAD-KKNYM
                                                       CSV-HEAD-KKNYMS
           END-IF
      *
           .
       1001-HEADER-HEN-EXT.
           EXIT.
      *****************************************************************
      *    調整金情報ソートファイル編集処理
      *****************************************************************
       1001-BG012SRT-HEN-SEC           SECTION.
      *
           MOVE    ZERO                TO  CNT-SEQ
      *
      *    調整金情報ソートファイル初期化
grpsys*DD  OPEN    OUTPUT                  BG012SRTCL-FILE
grpsys*DD  CLOSE   BG012SRTCL-FILE
grpsys     OPEN    OUTPUT                  BG012SRT-FILE
grpsys     CLOSE   BG012SRT-FILE
      *
           OPEN    OUTPUT                  BG012SRT-FILE
      *
           PERFORM 900-SYUNOU-KEY72-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
               COMPUTE CNT-SEQ =   CNT-SEQ     +   1
      *
               MOVE    SYU-PTID        TO  WRK-PTID
               PERFORM 900-PTINF-KEY-SEL-SEC
      *
               MOVE    SYU-PTID        TO  WRK-PTID
               PERFORM 900-PTNUM-KEY-SEL-SEC
      *
               IF    ( WRK-PARA-KAKBN  =   "1" )
                   MOVE    SYU-SRYKA       TO  WRK-SRT-SRYKA
               ELSE
                   MOVE    "00"            TO  WRK-SRT-SRYKA
               END-IF
      *
               IF    ( SYU-NYUGAIKBN   =   "1" )
                   MOVE    SYU-SKYEDYMD    TO  WRK-SRT-SRYYMD
               ELSE
                   MOVE    SYU-SRYYMD      TO  WRK-SRT-SRYYMD
               END-IF
      *
               EVALUATE    WRK-PARA-SRTKBN
      *        診療年月日
               WHEN    "1"
                   INITIALIZE                  BG012SRT02-REC
                   MOVE    WRK-SRT-SRYKA   TO  BG012SRT02-KEY-SRYKA
                   MOVE    WRK-SRT-SRYYMD  TO  BG012SRT02-KEY-SRYYMD
                   MOVE    PTNUM-PTNUM     TO  BG012SRT02-KEY-PTNUM
                   MOVE    SYU-DENPNUM     TO  BG012SRT02-KEY-DENPNUM
                   MOVE    CNT-SEQ         TO  BG012SRT02-KEY-SEQ
                   MOVE    PTINF-NAME      TO  BG012SRT02-NAME
                   MOVE    PTNUM-PTNUM     TO  BG012SRT02-PTNUM
                   MOVE    SYUNOU-REC      TO  BG012SRT02-DAT
                   IF    ( SYU-CHOSEI1 NOT =   ZERO )
                       MOVE    "1"         TO  BG012SRT02-KEY-CHOSEIKBN
                       MOVE    BG012SRT02-REC  TO  BG012SRT-REC
                       WRITE   BG012SRT-REC
                   END-IF
                   IF    ( SYU-CHOSEI2 NOT =   ZERO )
                       MOVE    "2"         TO  BG012SRT02-KEY-CHOSEIKBN
                       MOVE    BG012SRT02-REC  TO  BG012SRT-REC
                       WRITE   BG012SRT-REC
                   END-IF
      *
      *        伝票番号順
               WHEN    "2"
                   INITIALIZE                  BG012SRT03-REC
                   MOVE    WRK-SRT-SRYKA   TO  BG012SRT03-KEY-SRYKA
                   MOVE    SYU-DENPNUM     TO  BG012SRT03-KEY-DENPNUM
                   MOVE    CNT-SEQ         TO  BG012SRT03-KEY-SEQ
                   MOVE    PTINF-NAME      TO  BG012SRT03-NAME
                   MOVE    PTNUM-PTNUM     TO  BG012SRT03-PTNUM
                   MOVE    SYUNOU-REC      TO  BG012SRT03-DAT
                   IF    ( SYU-CHOSEI1 NOT =   ZERO )
                       MOVE    "1"         TO  BG012SRT03-KEY-CHOSEIKBN
                       MOVE    BG012SRT03-REC  TO  BG012SRT-REC
                       WRITE   BG012SRT-REC
                   END-IF
                   IF    ( SYU-CHOSEI2 NOT =   ZERO )
                       MOVE    "2"         TO  BG012SRT03-KEY-CHOSEIKBN
                       MOVE    BG012SRT03-REC  TO  BG012SRT-REC
                       WRITE   BG012SRT-REC
                   END-IF
      *
      *        カナ氏名順
               WHEN    "3"
                   INITIALIZE                  BG012SRT04-REC
                   MOVE    WRK-SRT-SRYKA   TO  BG012SRT04-KEY-SRYKA
                   MOVE    PTINF-KANANAME  TO  BG012SRT04-KEY-KANANAME
                   MOVE    PTNUM-PTNUM     TO  BG012SRT04-KEY-PTNUM
                   MOVE    WRK-SRT-SRYYMD  TO  BG012SRT04-KEY-SRYYMD
                   MOVE    SYU-DENPNUM     TO  BG012SRT04-KEY-DENPNUM
                   MOVE    CNT-SEQ         TO  BG012SRT04-KEY-SEQ
                   MOVE    PTINF-NAME      TO  BG012SRT04-NAME
                   MOVE    PTNUM-PTNUM     TO  BG012SRT04-PTNUM
                   MOVE    SYUNOU-REC      TO  BG012SRT04-DAT
                   IF    ( SYU-CHOSEI1 NOT =   ZERO )
                       MOVE    "1"         TO  BG012SRT04-KEY-CHOSEIKBN
                       MOVE    BG012SRT04-REC  TO  BG012SRT-REC
                       WRITE   BG012SRT-REC
                   END-IF
                   IF    ( SYU-CHOSEI2 NOT =   ZERO )
                       MOVE    "2"         TO  BG012SRT04-KEY-CHOSEIKBN
                       MOVE    BG012SRT04-REC  TO  BG012SRT-REC
                       WRITE   BG012SRT-REC
                   END-IF
      *
      *        患者番号順
               WHEN    OTHER
                   INITIALIZE                  BG012SRT01-REC
                   MOVE    WRK-SRT-SRYKA   TO  BG012SRT01-KEY-SRYKA
                   MOVE    PTNUM-PTNUM     TO  BG012SRT01-KEY-PTNUM
                   MOVE    WRK-SRT-SRYYMD  TO  BG012SRT01-KEY-SRYYMD
                   MOVE    SYU-DENPNUM     TO  BG012SRT01-KEY-DENPNUM
                   MOVE    CNT-SEQ         TO  BG012SRT01-KEY-SEQ
                   MOVE    PTINF-NAME      TO  BG012SRT01-NAME
                   MOVE    PTNUM-PTNUM     TO  BG012SRT01-PTNUM
                   MOVE    SYUNOU-REC      TO  BG012SRT01-DAT
                   IF    ( SYU-CHOSEI1 NOT =   ZERO )
                       MOVE    "1"         TO  BG012SRT01-KEY-CHOSEIKBN
                       MOVE    BG012SRT01-REC  TO  BG012SRT-REC
                       WRITE   BG012SRT-REC
                   END-IF
                   IF    ( SYU-CHOSEI2 NOT =   ZERO )
                       MOVE    "2"         TO  BG012SRT01-KEY-CHOSEIKBN
                       MOVE    BG012SRT01-REC  TO  BG012SRT-REC
                       WRITE   BG012SRT-REC
                   END-IF
      *
               END-EVALUATE
      *
               PERFORM 900-SYUNOU-KEY72-FET-SEC
      *
           END-PERFORM
      *
      *
grpsys     MOVE    "tbl_syunou"    TO  MCP-TABLE
grpsys     MOVE    "key72"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   BG012SRT-FILE
      *
           .
       1001-BG012SRT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO            TO  CNT-PAGE
           MOVE    ZERO            TO  CNT-LINE
           INITIALIZE                  WRK-CHOSEI-G
           MOVE    ZERO            TO  FLG-BG012SRT
      *
           OPEN    INPUT               BG012SRT-FILE
      *
           PERFORM 900-BG012SRT-READ-SEC
      *
      *----(04.08.01)--ADD-START---
           IF      FLG-BG012SRT    =   ZERO
      *        ＣＳＶ出力処理
               PERFORM 270-CSV-HEAD-SET-SEC
           END-IF
      *----(04.08.01)--ADD-END-----
      *
           PERFORM UNTIL ( FLG-BG012SRT    NOT =  ZERO )
                     OR  ( FLG-END         NOT =  ZERO )
      *
               PERFORM 210-HEAD-SEC
      *
               PERFORM
               UNTIL     ( CNT-LINE         >=  CONST-LINE-MAX )
                     OR  ( KEY-SRYKA     NOT =  BG012SRT-KEY-SRYKA )
                     OR  ( KEY-CHOSEIKBN NOT =  BG012SRT-KEY-CHOSEIKBN )
                     OR  ( FLG-BG012SRT  NOT =  ZERO )
                     OR  ( FLG-END       NOT =  ZERO )
      *
                   PERFORM 220-MEISAI-SEC
      *
               END-PERFORM
      *
               PERFORM 230-TOTAL-SEC
      *
               PERFORM 240-PRINT-SEC
      *
           END-PERFORM
      *
           CLOSE   BG012SRT-FILE
      *
           MOVE    1                   TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ヘッダ編集処理
      *****************************************************************
       210-HEAD-SEC                    SECTION.
      *
           MOVE    BG012SRT-KEY-SRYKA
                                   TO  KEY-SRYKA
      *
           MOVE    BG012SRT-KEY-CHOSEIKBN
                                   TO  KEY-CHOSEIKBN
      *
           MOVE    ZERO            TO  CNT-LINE
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
           INITIALIZE                  HCM42
           MOVE    HEAD-NYUGAI     TO  HCM42-NYUGAI
           MOVE    HEAD-SRTKBN     TO  HCM42-SRTKBN
           MOVE    HEAD-KKNYM      TO  HCM42-KKNYM
           MOVE    HEAD-SAKYMD     TO  HCM42-SAKYMD
           MOVE    CNT-PAGE        TO  WRK-ZZ9
           MOVE    WRK-ZZ9         TO  HCM42-PAGE
      *
           MOVE    "調整金（＋）"  TO  HCM42-LBLCHOSEIP
           MOVE    "調整金（−）"  TO  HCM42-LBLCHOSEIM
      *
           IF    ( WRK-PARA-KAKBN  =   "1" )
      *
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   WRK-SRYKALST-MAX )
      *
                   IF    ( BG012SRT-SYU-SRYKA
                                   =   WRK-SRYKAL (IDX1) )
                    AND  ( BG012SRT-SYU-SRYYMD
                                  >=  WRK-SRYKAL-STYUKYMD (IDX1))
                    AND  ( BG012SRT-SYU-SRYYMD
                                  <=  WRK-SRYKAL-EDYUKYMD (IDX1))
      *
                       MOVE    SPACE       TO  WRK-KKNYM
                       STRING  HCM42-KKNYM     DELIMITED   BY  SPACE
                               "　"            DELIMITED   BY  SIZE
                               WRK-SRYKAMEIL (IDX1)
                                               DELIMITED   BY  SPACE
                       INTO    WRK-KKNYM
                       END-STRING
      *
                       MOVE    WRK-SRYKAMEI1L (IDX1)
                                           TO  KEY-SRYKAMEI
      *
                       MOVE    WRK-KKNYM   TO  HCM42-KKNYM
                       MOVE    WRK-SRYKALST-MAX
                                           TO  IDX1
      *
                   END-IF
      *
               END-PERFORM
           ELSE
               MOVE    SPACE               TO  KEY-SRYKAMEI
           END-IF
      *
           IF    ( KEY-CHOSEIKBN           =   "1" )
               MOVE    "調整金１"          TO  HCM42-LBLCHOSEI
           ELSE
               MOVE    "調整金２"          TO  HCM42-LBLCHOSEI
           END-IF
      *
           .
       210-HEAD-EXT.
           EXIT.
      *----(04.08.01)--UPD-START---
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       220-MEISAI-SEC                  SECTION.
      *
           MOVE    BG012SRT-DAT    TO  SYUNOU-REC
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           MOVE    BG012SRT-PTNUM  TO  HCM42-PTNUM (CNT-LINE)
           MOVE    BG012SRT-NAME   TO  HCM42-NAME  (CNT-LINE)
      *
           MOVE    SYU-DENPNUM     TO  WRK-DENPNUM
           MOVE    WRK-DENPNUM-G   TO  HCM42-DENPNUM (CNT-LINE)
      *
           INITIALIZE                  WRK-CSV-AREA
      *
           MOVE    BG012SRT-PTNUM  TO  WRK-CSV-PTNUM
           MOVE    BG012SRT-NAME   TO  WRK-CSV-NAME
           MOVE    WRK-DENPNUM-G   TO  WRK-CSV-DENPNUM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   WRK-SRYKALST-MAX )
                    OR   ( HCM42-SRYKA (CNT-LINE)  NOT =   SPACE )
      *
               IF    ( SYU-SRYKA   =   WRK-SRYKAL (IDX1) )
                AND  ( SYU-SRYYMD  >=  WRK-SRYKAL-STYUKYMD (IDX1))
                AND  ( SYU-SRYYMD  <=  WRK-SRYKAL-EDYUKYMD (IDX1))
                   MOVE    WRK-SRYKAMEI1L(IDX1)
                                       TO  HCM42-SRYKA (CNT-LINE)
      *
                   MOVE    WRK-SRYKAL(IDX1)
                                       TO  WRK-CSV-SRYKA
                   MOVE    WRK-SRYKAMEI1L(IDX1)
                                       TO  WRK-CSV-SRYKAMEI
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-PARA-NYUGAIKBN  =   "1" )
               INITIALIZE                  WRK-SRYYMDM-G
               MOVE    SYU-SKYSTYMD    TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-EDTYMD1     TO  WRK-SRYYMDM-ST
               MOVE    "-"             TO  WRK-SRYYMDM-MID
               MOVE    SYU-SKYEDYMD    TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-EDTYMD1(5:) TO  WRK-SRYYMDM-ED
      *
               MOVE    WRK-SRYYMDM-G   TO  HCM42-SKYKKN  (CNT-LINE)
      *
               MOVE    SYU-SKYSTYMD    TO  WRK-SYMD
               MOVE    WRK-SYY         TO  WRK-HEN-YY-OT
               MOVE    WRK-SMM         TO  WRK-HEN-MM-OT
               MOVE    WRK-SDD         TO  WRK-HEN-DD-OT
               MOVE    WRK-HEN-YMD-OT  TO  WRK-CSV-STYMD
               MOVE    SYU-SKYEDYMD    TO  WRK-SYMD
               MOVE    WRK-SYY         TO  WRK-HEN-YY-OT
               MOVE    WRK-SMM         TO  WRK-HEN-MM-OT
               MOVE    WRK-SDD         TO  WRK-HEN-DD-OT
               MOVE    WRK-HEN-YMD-OT  TO  WRK-CSV-EDYMD
           ELSE
               MOVE    SYU-SRYYMD      TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-EDTYMD1     TO  HCM42-SRYMD   (CNT-LINE)
      *
               MOVE    SYU-SRYYMD      TO  WRK-SYMD
               MOVE    WRK-SYY         TO  WRK-HEN-YY-OT
               MOVE    WRK-SMM         TO  WRK-HEN-MM-OT
               MOVE    WRK-SDD         TO  WRK-HEN-DD-OT
               MOVE    WRK-HEN-YMD-OT  TO  WRK-CSV-STYMD
           END-IF
      *
           IF    ( KEY-CHOSEIKBN       =   "1" )
               MOVE    1               TO  IDXC
               MOVE    "調整金１"      TO  WRK-CSV-CHOSEIKBN
           ELSE
               MOVE    2               TO  IDXC
               MOVE    "調整金２"      TO  WRK-CSV-CHOSEIKBN
           END-IF
      *
           IF    ( SYU-RCHOSEI (IDXC)  >=  ZERO )
               MOVE    SYU-RCHOSEI (IDXC)
                                       TO  WRK-CHOSEI
               MOVE    WRK-CHOSEI      TO  HCM42-CHOSEIP (CNT-LINE)
               COMPUTE TTL-SRYKA-CHOSEIP (IDXC)
                   =   TTL-SRYKA-CHOSEIP (IDXC)
                   +   SYU-RCHOSEI (IDXC)
               COMPUTE TTL-TTL-CHOSEIP (IDXC)
                   =   TTL-TTL-CHOSEIP (IDXC)
                   +   SYU-RCHOSEI (IDXC)
      *
               MOVE    SYU-RCHOSEI (IDXC)
                                       TO  WRK-CSV-CHOSEIP
           ELSE
               MOVE    SYU-RCHOSEI (IDXC)
                                       TO  WRK-CHOSEI
               MOVE    WRK-CHOSEI      TO  HCM42-CHOSEIM (CNT-LINE)
               COMPUTE TTL-SRYKA-CHOSEIM (IDXC)
                   =   TTL-SRYKA-CHOSEIM (IDXC)
                   +   SYU-RCHOSEI (IDXC)
               COMPUTE TTL-TTL-CHOSEIM (IDXC)
                   =   TTL-TTL-CHOSEIM (IDXC)
                   +   SYU-RCHOSEI (IDXC)
      *
               MOVE    SYU-RCHOSEI (IDXC)
                                       TO  WRK-CSV-CHOSEIM
           END-IF
      *
           COMPUTE TTL-SRYKA-KNS (IDXC)
               =   TTL-SRYKA-KNS (IDXC)    +   1
           COMPUTE TTL-TTL-KNS (IDXC)
               =   TTL-TTL-KNS (IDXC)      +   1
      *
      *    ＣＳＶ出力処理
           PERFORM 270-CSV-BODY-SET-SEC
      *
           PERFORM 900-BG012SRT-READ-SEC
      *
           .
       220-MEISAI-EXT.
           EXIT.
      *----(04.08.01)--UPD-END-----
      *****************************************************************
      *    合計行編集処理
      *****************************************************************
       230-TOTAL-SEC                   SECTION.
      *
           IF    ( KEY-SRYKA       NOT =  BG012SRT-KEY-SRYKA )
             OR  ( FLG-BG012SRT    NOT =  ZERO )
      *
                   MOVE    "件数"              TO  HCM42-LBLTTLKNS
                   MOVE    "調整金（＋）"      TO  HCM42-LBLTTLCHOSEIP
                   MOVE    "調整金（−）"      TO  HCM42-LBLTTLCHOSEIM
                   MOVE    "調整金計"          TO  HCM42-LBLTTLCHOSEI
      *
                   MOVE    SPACE               TO  TTL-TITLE
                   STRING  KEY-SRYKAMEI    DELIMITED   BY  SPACE
                           "調整金１計"    DELIMITED   BY  SIZE
                   INTO    TTL-TITLE
                   END-STRING
      *
                   MOVE    TTL-SRYKA-KNS (1)       TO  TTL-KNS
                   MOVE    TTL-SRYKA-CHOSEIP (1)   TO  TTL-CHOSEIP
                   MOVE    TTL-SRYKA-CHOSEIM (1)   TO  TTL-CHOSEIM
      *
                   MOVE    1                   TO  IDX-TTL
                   PERFORM 2301-TOTAL-SEC
      *
                   MOVE    SPACE               TO  TTL-TITLE
                   STRING  KEY-SRYKAMEI    DELIMITED   BY  SPACE
                           "調整金２計"    DELIMITED   BY  SIZE
                   INTO    TTL-TITLE
                   END-STRING
      *
                   MOVE    TTL-SRYKA-KNS (2)       TO  TTL-KNS
                   MOVE    TTL-SRYKA-CHOSEIP (2)   TO  TTL-CHOSEIP
                   MOVE    TTL-SRYKA-CHOSEIM (2)   TO  TTL-CHOSEIM
      *
                   MOVE    2                   TO  IDX-TTL
                   PERFORM 2301-TOTAL-SEC
      *
                   MOVE    SPACE               TO  TTL-TITLE
                   STRING  KEY-SRYKAMEI    DELIMITED   BY  SPACE
                           "調整金合計"    DELIMITED   BY  SIZE
                   INTO    TTL-TITLE
                   END-STRING
      *
                   COMPUTE TTL-KNS
                       =   TTL-SRYKA-KNS (1)
                       +   TTL-SRYKA-KNS (2)
                   COMPUTE TTL-CHOSEIP
                       =   TTL-SRYKA-CHOSEIP (1)
                       +   TTL-SRYKA-CHOSEIP (2)
                   COMPUTE TTL-CHOSEIM
                       =   TTL-SRYKA-CHOSEIM (1)
                       +   TTL-SRYKA-CHOSEIM (2)
      *
                   MOVE    3                   TO  IDX-TTL
                   PERFORM 2301-TOTAL-SEC
      *
                   INITIALIZE                  TTL-SRYKA-G
      *
               IF    ( WRK-PARA-KAKBN      =   "1" )
      *
                   IF    ( FLG-BG012SRT    NOT =   ZERO )
      *
                       MOVE    "調整金１合計"  TO  TTL-TITLE
                       MOVE    TTL-TTL-KNS (1)     TO  TTL-KNS
                       MOVE    TTL-TTL-CHOSEIP (1) TO  TTL-CHOSEIP
                       MOVE    TTL-TTL-CHOSEIM (1) TO  TTL-CHOSEIM
      *
                       MOVE    4                   TO  IDX-TTL
                       PERFORM 2301-TOTAL-SEC
      *
                       MOVE    "調整金２合計"  TO  TTL-TITLE
                       MOVE    TTL-TTL-KNS (2)     TO  TTL-KNS
                       MOVE    TTL-TTL-CHOSEIP (2) TO  TTL-CHOSEIP
                       MOVE    TTL-TTL-CHOSEIM (2) TO  TTL-CHOSEIM
      *
                       MOVE    5                   TO  IDX-TTL
                       PERFORM 2301-TOTAL-SEC
      *
                       MOVE    "調整金合計"    TO  TTL-TITLE
                       COMPUTE TTL-KNS
                           =   TTL-TTL-KNS (1)
                           +   TTL-TTL-KNS (2)
                       COMPUTE TTL-CHOSEIP
                           =   TTL-TTL-CHOSEIP (1)
                           +   TTL-TTL-CHOSEIP (2)
                       COMPUTE TTL-CHOSEIM
                           =   TTL-TTL-CHOSEIM (1)
                           +   TTL-TTL-CHOSEIM (2)
      *
                       MOVE    6                   TO  IDX-TTL
                       PERFORM 2301-TOTAL-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       230-TOTAL-EXT.
           EXIT.
      *****************************************************************
      *    合計行編集処理
      *****************************************************************
       2301-TOTAL-SEC                      SECTION.
      *
           MOVE    TTL-TITLE            TO  HCM42-TTLTITLE  (IDX-TTL)
      *
           MOVE    TTL-KNS              TO  WRK-CHOSEIKNS
           MOVE    WRK-CHOSEIKNS        TO  HCM42-TTLKNS    (IDX-TTL)
      *
           MOVE    TTL-CHOSEIP          TO  WRK-CHOSEI
           MOVE    WRK-CHOSEI           TO  HCM42-TTLCHOSEIP(IDX-TTL)
      *
           MOVE    TTL-CHOSEIM          TO  WRK-CHOSEI
           MOVE    WRK-CHOSEI           TO  HCM42-TTLCHOSEIM(IDX-TTL)
      *
           COMPUTE WRK-CHOSEI-SGK
               =   TTL-CHOSEIP
               +   TTL-CHOSEIM
           MOVE    WRK-CHOSEI-SGK       TO  WRK-CHOSEI
           MOVE    WRK-CHOSEI           TO  HCM42-TTLCHOSEI (IDX-TTL)
      *
      *----(04.08.01)--ADD-START---
           PERFORM 270-CSV-GOKEI-SET-SEC
      *----(04.08.01)--ADD-END-----
      *
           .
       2301-TOTAL-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       240-PRINT-SEC                   SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    WRK-SPRT-SRYYM      TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE   "HCM42V01.red"       TO  SPRT-PRTID
           MOVE    WRK-TITLE           TO  SPRT-TITLE
           MOVE    HCM42               TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
grpsys                                 SPA-AREA
           IF    ( SPRT-RETURN         =   ZERO )
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       240-PRINT-EXT.
           EXIT.
      *----(04.08.01)--ADD-START---
      *****************************************************************
      *    ＣＳＶ（ＨＥＡＤレコード）編集処理
      *****************************************************************
       270-CSV-HEAD-SET-SEC        SECTION.
      *
           INITIALIZE                  WRK-REC
                                       WRK-REC-LENGTH
      *
           MOVE    CSV-HEAD-01     TO  WRK-DATA
           MOVE    100             TO  WRK-NAGASA
           MOVE    1               TO  WRK-RECEND
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                       DELIMITED  BY  SIZE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                   INTO                WRK-REC
           END-STRING
           PERFORM 500-TOUKEICSV-SEC
      *
           INITIALIZE                  WRK-REC
                                       WRK-REC-LENGTH
      *
           MOVE    CSV-HEAD-02     TO  WRK-DATA
           MOVE    120             TO  WRK-NAGASA
           MOVE    1               TO  WRK-RECEND
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                       DELIMITED  BY  SIZE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                   INTO                WRK-REC
           END-STRING
           PERFORM 500-TOUKEICSV-SEC
      *
           .
       270-CSV-HEAD-SET-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ（データレコード）編集処理
      *****************************************************************
       270-CSV-BODY-SET-SEC        SECTION.
      *
           INITIALIZE                  WRK-REC
                                       WRK-REC-LENGTH
      *
      *    調整金区分
           MOVE    WRK-CSV-CHOSEIKBN   TO  WRK-DATA
           MOVE    8               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    患者番号
           MOVE    WRK-CSV-PTNUM   TO  WRK-DATA
           MOVE    20              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    患者氏名
           MOVE    WRK-CSV-NAME    TO  WRK-DATA
           MOVE    100             TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    伝票番号
           MOVE    WRK-CSV-DENPNUM TO  WRK-DATA
           MOVE    7               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    診療科
           MOVE    WRK-CSV-SRYKA   TO  WRK-DATA
           MOVE    2               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    診療科名
           MOVE    WRK-CSV-SRYKAMEI
                                   TO  WRK-DATA
           MOVE    20              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    診療年月日
           MOVE    WRK-CSV-STYMD   TO  WRK-DATA
           MOVE    10              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    WRK-CSV-EDYMD   TO  WRK-DATA
           MOVE    10              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    調整金（＋）
           MOVE    WRK-CSV-CHOSEIP TO  WRK-DATA
           MOVE    7               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    調整金（−）
           MOVE    WRK-CSV-CHOSEIM TO  WRK-CSV-Z7
           MOVE    WRK-CSV-Z7      TO  WRK-DATA
           MOVE    7               TO  WRK-NAGASA
           MOVE    1               TO  WRK-RECEND
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                       DELIMITED  BY  SIZE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                   INTO                WRK-REC
           END-STRING
           PERFORM 500-TOUKEICSV-SEC
      *
           .
       270-CSV-BODY-SET-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ（合計レコード）編集処理
      *****************************************************************
       270-CSV-GOKEI-SET-SEC       SECTION.
      *
           INITIALIZE                  WRK-REC
                                       WRK-REC-LENGTH
      *
           MOVE    ",,,,,"         TO  WRK-REC(1:5)
           MOVE    5               TO  WRK-REC-LENGTH
      *
           MOVE    TTL-TITLE       TO  WRK-DATA
           MOVE    20              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    TTL-KNS         TO  WRK-DATA
           MOVE    6               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    "件"            TO  WRK-DATA
           MOVE    2               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    TTL-CHOSEIP     TO  WRK-DATA
           MOVE    12              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    TTL-CHOSEIM     TO  WRK-CSV-Z11
           MOVE    WRK-CSV-Z11     TO  WRK-DATA
           MOVE    12              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           IF      WRK-CHOSEI-SGK  <   0
               MOVE    WRK-CHOSEI-SGK  TO  WRK-CSV-Z11
               MOVE    WRK-CSV-Z11     TO  WRK-DATA
           ELSE
               MOVE    WRK-CHOSEI-SGK  TO  WRK-DATA
           END-IF
           MOVE    11              TO  WRK-NAGASA
           MOVE    1               TO  WRK-RECEND
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                       DELIMITED  BY  SIZE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                   INTO                WRK-REC
           END-STRING
           PERFORM 500-TOUKEICSV-SEC
      *
           .
       270-CSV-GOKEI-SET-EXT.
           EXIT.
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       500-TOUKEICSV-SEC           SECTION.
      *
           INITIALIZE                  ORCSTOUKEICSVAREA
           MOVE    "INS"           TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  STOUKEICSV-SHELLID
           MOVE    SPACE           TO  STOUKEICSV-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN
                                   TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO            TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"     TO  STOUKEICSV-TO-FOLDER
           MOVE    WRK-TO-DATA     TO  STOUKEICSV-TO-DATA
           MOVE    "2"             TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-REC         TO  STOUKEICSV-CSVDATA
      *
           MOVE    "調整金一覧表ＣＳＶ作成"
                                   TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"     USING
                                       ORCSTOUKEICSVAREA
                                       SPA-AREA
           IF      STOUKEICSV-RETURN   =   ZERO
               CONTINUE
           ELSE
               MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                      TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
      *
       500-TOUKEICSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       5004-MOJI-HENSYU-SEC        SECTION.
      *
           IF      WRK-DATA            =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDW2    FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW2    <       1
                   IF      WRK-DATA (IDW2:1)   NOT =   SPACE
                       STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                   DELIMITED   BY  SIZE
                               WRK-DATA(1:IDW2)    DELIMITED   BY  SIZE
                               INTO    WRK-REC
                       END-STRING
                       ADD     IDW2        TO  WRK-REC-LENGTH
                       MOVE    1           TO  IDW2
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-RECEND         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
      *
           INITIALIZE                      WRK-HENSYU-AREA
      *
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    異常終了処理
      *****************************************************************
       500-COBABORT-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-RECEERR
           STRING  "ORCBG012 "     DELIMITED  BY  SIZE
                   WRK-TOUKEIERR   DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-RECEERR
           END-STRING
           CALL    "cobabort"      USING   WRK-RECEERR
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *----(04.08.01)--ADD-END-----
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
           PERFORM 800-BG012SRT-DEL-SEC
      *
           IF    ( CNT-PAGE        =   ZERO )
               MOVE    "処理対象のデータがありませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBG012 IN " CNT-PAGE  " ***"
           DISPLAY "*** ORCBG012 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
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
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC           SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
                                       WRK-EDTYMD3
      *
           IF        ( WRK-SYMD        =   SPACE )
               GO  TO  800-SEIWA-HEN-EXT
           END-IF
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
           INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    調整金情報ソートファイル削除処理
      *****************************************************************
       800-BG012SRT-DEL-SEC            SECTION.
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
grpsys*DD  MOVE    ORCBG012SRTPARA TO  IF-FILENAME
grpsys     MOVE    BG012SRTPARA    TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           .
       800-BG012SRT-DEL-EXT.
           EXIT.
      *****************************************************************
      *    調整金情報ソートファイル読込処理
      *****************************************************************
       900-BG012SRT-READ-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-BG012SRT
      *
           READ    BG012SRT-FILE       NEXT
           AT  END
               MOVE    1               TO  FLG-BG012SRT
           NOT AT  END
               MOVE    ZERO            TO  FLG-BG012SRT
               DISPLAY "BG012SRT-KEY[" BG012SRT-KEY (1:20) "]"
           END-READ
      *
           .
       900-BG012SRT-READ-EXT.
           EXIT.
      *****************************************************************
      *    収納情報検索処理
      *****************************************************************
       900-SYUNOU-KEY72-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
grpsys*DD  MOVE    WRK-PARA-HOSPID     TO  SYU-HOSPID
grpsys     MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    WRK-PARA-NYUGAIKBN  TO  SYU-NYUGAIKBN
           IF    ( WRK-PARA-KKNKBN =   "0" )
               MOVE    "00000000"      TO  SYU-SKYSTYMD
               MOVE    "99999999"      TO  SYU-SKYEDYMD
           ELSE
               IF    ( WRK-PARA-KKNSTYM    =   "000000" )
                   MOVE    "00000000"  TO  SYU-SKYSTYMD
               ELSE
                   MOVE    WRK-PARA-KKNSTYM
                                       TO  SYU-SKYSTYMD
                   MOVE    "01"        TO  SYU-SKYSTYMD(7:2)
               END-IF
      *
               IF    ( WRK-PARA-KKNEDYM    =   SPACE )
                       MOVE    SYU-SKYSTYMD
                                       TO  SYU-SKYEDYMD
                       MOVE    "31"    TO  SYU-SKYEDYMD (7:2)
               ELSE
                   IF    ( WRK-PARA-KKNEDYM    =   "999999" )
                       MOVE    "99999999"
                                       TO  SYU-SKYEDYMD
                   ELSE
                       MOVE    WRK-PARA-KKNEDYM
                                       TO  SYU-SKYEDYMD
                       MOVE    "31"    TO  SYU-SKYEDYMD (7:2)
                   END-IF
               END-IF
      *
           END-IF
      *
           MOVE    SYUNOU-REC              TO  MCPDATA-REC
grpsys     MOVE    "tbl_syunou"    TO  MCP-TABLE
grpsys     MOVE    "key72"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               MOVE    1           TO  FLG-SYUNOU
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           .
       900-SYUNOU-KEY72-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納情報ＦＥＴＣＨ処理
      *****************************************************************
       900-SYUNOU-KEY72-FET-SEC        SECTION.
      *
grpsys     MOVE    "tbl_syunou"    TO  MCP-TABLE
grpsys     MOVE    "key72"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           .
       900-SYUNOU-KEY72-FET-EXT.
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
grpsys*DD  MOVE    PATH-TBL-PTNUM-KEY  TO  MCP-PATH
grpsys     MOVE    "tbl_ptnum"     TO  MCP-TABLE
grpsys     MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNUM-REC
           ELSE
               MOVE    1           TO  FLG-PTNUM
               INITIALIZE              PTNUM-REC
           END-IF
      *
grpsys*DD  MOVE    PATH-TBL-PTNUM-KEY  TO  MCP-PATH
grpsys     MOVE    "tbl_ptnum"     TO  MCP-TABLE
grpsys     MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY-SEL-EXT.
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
grpsys*DD  MOVE    PATH-TBL-PTINF-KEY  TO  MCP-PATH
grpsys     MOVE    "tbl_ptinf"     TO  MCP-TABLE
grpsys     MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               MOVE    1           TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
grpsys*DD  MOVE    PATH-TBL-PTINF-KEY  TO  MCP-PATH
grpsys     MOVE    "tbl_ptinf"     TO  MCP-TABLE
grpsys     MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
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
grpsys*DD  CALL    "MCPSUB"            USING
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
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MCPSUB"            USING
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
grpsys*DD  CALL    "MCPSUB"            USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
grpsys*DD  MOVE    LOW-VALUE           TO  MCP-PATH
           MOVE    "DBOPEN"        TO  MCP-FUNC.
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE.
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MCPSUB"            USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
      *
grpsys*DD  MOVE    LOW-VALUE           TO  MCP-PATH
           MOVE    "DBSTART"           TO  MCP-FUNC.
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE.
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MCPSUB"            USING
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
grpsys*DD  MOVE    LOW-VALUE           TO  MCP-PATH
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE.
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MCPSUB"            USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
      *
grpsys*DD  MOVE    LOW-VALUE           TO  MCP-PATH
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE.
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MCPSUB"            USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
