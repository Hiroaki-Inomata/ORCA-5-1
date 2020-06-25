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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCBG016.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 指定診療行為件数調
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/06/21    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者        日付      内容
      *  03.05.01    NACL-森脇     07/06/06  グループ診療対応
      *  04.03.01    NACL-森脇     08/08/29  パラメタ増加
      *  04.05.01    NACL-森脇     08/09/18  病棟別集計対応
      *  04.08.01    NACL-森脇     14/07/22  一時ファイルディレクトリ設定
      *  05.00.00    NACL-森脇     19/03/20  労災自賠の入院時の加算除外
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    診療行為使用患者一覧ファイル
           SELECT  IN01-FILE       ASSIGN  BG0161
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  IN01-KEY
                                   FILE    STATUS  IS  STS-IN01.
      *
      *    診療会計ファイル
           SELECT  IN02-FILE       ASSIGN  BG0162
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  IN02-KEY
                                   FILE    STATUS  IS  STS-IN02.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    診療行為使用患者一覧ファイル
       FD  IN01-FILE.
       01  IN01-REC.
           03  IN01-KEY.
               05  IN01-KEY-CDSEQ      PIC 9(03).
               05  IN01-KEY-SRYCD      PIC X(09).
               05  IN01-KEY-SRYKA      PIC X(02).
               05  IN01-KEY-KANANAME   PIC X(100).
               05  IN01-KEY-PTNUM      PIC X(20).
               05  IN01-KEY-PTID       PIC 9(10).
           03  IN01-DAY                PIC 9(03)   OCCURS  31.
           03  IN01-KAISU              PIC 9(05).
           03  IN01-NAME               PIC X(100).
           03  IN01-TSTPTNUMKBN        PIC X(01).
      *
      *    診療会計ファイル
       FD  IN02-FILE.
       01  IN02-REC.
           COPY    "CPSRYACCT.INC"         REPLACING   //ACCT-//
                                           BY          //IN02-//.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    診療行為使用患者一覧ファイル
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING   //RECE01PARA//
                                   BY          //BG0161//.
      *----(04.08.01)--UPD-START---
      *     03  FILLER              PIC X(04)   VALUE   ".dat".
      *----(04.08.01)--UPD-END-----
      *
      *    診療会計ファイル
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING   //RECE01PARA//
                                   BY          //BG0162//.
      *----(04.08.01)--UPD-START---
      *     03  FILLER              PIC X(04)   VALUE   ".dat".
      *----(04.08.01)--UPD-END-----
      *----(04.08.01)--ADD-START---
           COPY    "CPRECEERR.INC".
      *----(04.08.01)--ADD-END-----
      *
           COPY    "HCG016.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-IN01            PIC X(02).
           03  STS-IN02            PIC X(02).
           03  STS-BG01DAT         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-IN01            PIC 9(01).
           03  FLG-LOOP-END        PIC 9(01).
           03  FLG-BG01DAT         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-TAISYO          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
      *----(05.00.00)--ADD-START---
           03  FLG-COMB            PIC 9(01).
      *----(05.00.00)--ADD-END-----
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(03).
           03  CNT-PAGE            PIC 9(06).
           03  CNT-SEQ             PIC 9(06).
           03  CNT-INPUT-ITEM      PIC 9(03).
           03  CNT-KETA                PIC 9(02).
           03  CNT-KETA2               PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX3                PIC 9(05).
           03  IDX4                PIC 9(05).
           03  IDX5                PIC 9(05).
           03  IDX6                PIC 9(05).
           03  IDX-NACCT           PIC 9(05).
           03  IDX-NSRY            PIC 9(05).
           03  IDX-DAY             PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *----(04.08.01)--UPD-START---
      *     03  RECEERR             PIC X(100).
      *----(04.08.01)--UPD-END-----
           03  WRK-PARA-SRYYM      PIC X(06).
           03  WRK-PARA-NYUGAIKBN-G.
               05  WRK-PARA-NYUGAIKBN
                                   PIC X(01).
               05  WRK-PARA-HOKATSUKBN
                                   PIC X(01).
           03  WRK-PARA-SRYKA      PIC X(02).
           03  WRK-PARA-SYORIKBN   PIC X(01).
           03  WRK-PARA-SRYCD      PIC X(20)
                                   OCCURS  7.
      *
      *    出力先ファイル名
       01  WRK-TO-DATA-G.
           03  WRK-TO-DATA.
               05  WRK-TO-DATA-HOSPNUM PIC 9(02).
               05  FILLER              PIC X(12)   VALUE
                                                   "srycd_kensu_".
               05  WRK-TO-DATA-STRYM   PIC X(06).
               05  FILLER              PIC X(04)   VALUE   ".csv".
      *
      *    ヘッダー情報
       01  CSV-HEAD-01.
           03  FILLER              PIC X(08)   VALUE   "診療年月".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "入外区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "入外区分名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "診療行為コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "診療行為名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-SRYKA           PIC X(12)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-SRYKAMEI        PIC X(10)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "カナ氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "最終受診日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "保険組合せ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(50)   VALUE
               "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20".
           03  FILLER              PIC X(38)   VALUE
               ",21,22,23,24,25,26,27,28,29,30,31,合計".
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-BG01DAT-FILE    PIC X(1024).
           03  WRK-RECEERR         PIC X(200).
           03  WRK-TOUKEIERR       PIC X(200).
           03  WRK-SRYYMD          PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1         PIC X(10).
           03  WRK-EDTYMD3         PIC X(22).
           03  WRK-SRYYM-N         PIC X(22).
           03  WRK-SAKYMD-N        PIC X(22).
           03  WRK-ZZZZ            PIC ZZZZ.
           03  WRK-ZZ9             PIC ZZ9.
           03  WRK-ZZZ9            PIC ZZZ9.
           03  WRK-SRYKA           PIC X(02).
           03  WRK-ITEM            PIC X(20).
           03  WRK-STR             PIC X(20).
           03  WRK-SRYCD           PIC X(09).
           03  WRK-KAISU           PIC 9(03).
           03  WRK-PTID            PIC 9(10).
           03  WRK-KANACHK-G.
               05  WRK-KANACHK-INPUT   PIC X(100).
               05  WRK-KANACHK-MAX     PIC 9(05).
           03  WRK-REC             PIC X(20000).

           03  WRK-HENSURYO            PIC X(08).
           03  WRK-SURYO               PIC ZZZZZZZ9.
      *
       01  WRK-HEN-YMD-OT.
           03  WRK-HEN-YY-OT       PIC X(04).
           03  FILLER              PIC X(01)   VALUE   "/". 
           03  WRK-HEN-MM-OT       PIC X(02).
           03  FILLER              PIC X(01)   VALUE   "/". 
           03  WRK-HEN-DD-OT       PIC X(02).

       01  CSV-AREA.
           03  CSV-DAY             PIC X(03)   OCCURS  32.
           03  CSV-SRYYM           PIC X(07).
           03  CSV-LASTYMD         PIC X(10).
      *
       01  TOTAL-AREA.
           03  TOTAL-PTNUM         PIC 9(08).
           03  TOTAL-SRYKA-G.
               05  TOTAL-SRYKA-DAY PIC 9(08) OCCURS 31.
           03  TOTAL-G.
               05  TOTAL-NINZU     PIC 9(08).
               05  TOTAL-DAY       PIC 9(08) OCCURS 31.
      *
       01  TSRYCD-AREA.
           03  TSRYCD-MAX          PIC 9(03).
           03  TSRYCD-SET-SRYCD    PIC X(09).
           03  TSRYCD-OCC          OCCURS  100.
               05  TSRYCD-SRYCD    PIC X(09).
               05  TSRYCD-NAME     PIC X(200).
               05  TSRYCD-KAISU    PIC 9(03).
      *----(05.00.00)--ADD-START---
               05  TSRYCD-NG       PIC 9(01).
      *----(05.00.00)--ADD-END-----
      *
       01  TPTINF-AREA.
        02 TPTINF-MAX                      PIC 9(05).
        02 TPTINF-CHK-HOSPNUM              PIC 9(02).
        02 TPTINF-CHK-PTID                 PIC 9(10).
        02 TPTINF-FLG                      PIC 9(01).
        02 TPTINF-IDX                      PIC 9(05).
        02 TPTINF-OCC                      OCCURS  1000.
           COPY    "CPPTINF.INC"           REPLACING   //PTINF-//
                                           BY          //TPTINF-//.
           03  TPTINF-PTNUM                PIC X(20).
      *
       01  KEY-AREA.
           03  KEY-SRYKA                   PIC X(02).
           03  KEY-SRYCD                   PIC X(09).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-LINE-MAX              PIC 9(05)   VALUE 50.
           03  CONST-SRYKA-TOTAL-IDX       PIC 9(05)   VALUE 51.
           03  CONST-TOTAL-IDX             PIC 9(05)   VALUE 52.
           03  CONST-TPTINF-MAX            PIC 9(05)   VALUE 1000.
           03  CONST-TSRYCD-MAX            PIC 9(05)   VALUE 100.
           03  CONST-SRYCD-MAX             PIC 9(05)   VALUE 5.
           03  CONST-NSRYACT-MAX           PIC 9(05)   VALUE 20.
           03  CONST-PARA-SRYCD-MAX        PIC 9(05)   VALUE 7.
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
           COPY    "CPSK1001.INC".
           COPY    "CPSK1005.INC".
           COPY    "CPSK5001.INC".
      *
      *    入院診療会計マスター
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号マスタ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *----(05.00.00)--ADD-START---
      *    保険組み合わせ
       01  COMB-REC.
           COPY    "CPHKNCOMBI.INC".
      *----(05.00.00)--ADD-END-----
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    パス指定
           COPY    "CPMKPASS.INC".
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
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    指定診療行為件数調サブ１
           COPY    "CPORCHCG016S01.INC".
      *
      *    カナチェックパラメタ
           COPY    "CPORCSKANACHK.INC".
           COPY    "CPORCSKANACHK2.INC".
      *
      *    指定診療行為件数調サブ２
           COPY    "CPORCSS009.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *----(04.08.01)--ADD-START---
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
      *
      *----(05.00.00)--ADD-START---
      *    労災入院診療コードチェックサブ
           COPY    "CPORCHCM34S03.INC".
      *----(05.00.00)--ADD-END-----
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
grpsys     COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(400).
      *
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
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
            STOP   RUN
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  KEY-AREA
           INITIALIZE                  TSRYCD-AREA
           INITIALIZE                  TPTINF-AREA
           INITIALIZE                  TOTAL-AREA
grpsys     INITIALIZE                  SPA-AREA
      *----(04.08.01)--ADD-START---
           INITIALIZE                  RECEERR
      *----(04.08.01)--ADD-END-----
      *
           PERFORM 100-DBOPEN-SEC
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
grpsys*DD                                      WRK-PARA-HOSPID
grpsys                                         WRK-PARA-HOSPNUM
grpsys                                         RECEERR
                                               WRK-PARA-SRYYM
                                               WRK-PARA-NYUGAIKBN-G
                                               WRK-PARA-SRYKA
                                               WRK-PARA-SYORIKBN
                                               WRK-PARA-SRYCD (1)
                                               WRK-PARA-SRYCD (2)
                                               WRK-PARA-SRYCD (3)
                                               WRK-PARA-SRYCD (4)
                                               WRK-PARA-SRYCD (5)
                                               WRK-PARA-SRYCD (6)
                                               WRK-PARA-SRYCD (7)
grpsys     END-UNSTRING
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           CALL    "ORCSENCODING"      USING
                                       MCPAREA
                                       SPA-AREA
      *      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    "ORCBG016"      TO  JOB-PGID
           MOVE    "指定診療行為件数調"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
grpsys                                 SPA-AREA
      *
grpsys*DD  MOVE    "RECEERR"       TO  RECEERR-FILE-ID
grpsys*DD  MOVE    LNK-PRTKANRI-TERMID
grpsys*DD                          TO  RECEERR-TERMID
      *
           MOVE    "BG01601"       TO  BG0161-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  BG0161-TERMID
grpsys     MOVE    SPA-HOSPNUM     TO  BG0161-HOSPNUM
      *
           MOVE    "BG01602"       TO  BG0162-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  BG0162-TERMID
grpsys     MOVE    SPA-HOSPNUM     TO  BG0162-HOSPNUM
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    BG0161-BASENAME     TO  SGETTEMP-BASENAMES (1)
           MOVE    BG0162-BASENAME     TO  SGETTEMP-BASENAMES (2)
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES (3)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  BG0161-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  BG0162-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECEERR
      *----(04.08.01)--ADD-END-----
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
          MOVE    SPACE                TO  WRK-KANACHK-INPUT
          MOVE    1                    TO  WRK-KANACHK-MAX
          STRING  WRK-PARA-SRYYM       DELIMITED   BY  SIZE
          INTO    WRK-KANACHK-INPUT
          WITH    POINTER WRK-KANACHK-MAX
          END-STRING
          COMPUTE WRK-KANACHK-MAX      =   WRK-KANACHK-MAX -   1
          PERFORM 800-HANKAKU-CHK-SEC
          MOVE    WRK-KANACHK-INPUT    TO  WRK-PARA-SRYYM
      *
          MOVE    SPACE                TO  WRK-KANACHK-INPUT
          MOVE    1                    TO  WRK-KANACHK-MAX
          STRING  WRK-PARA-NYUGAIKBN-G DELIMITED   BY  SIZE
          INTO    WRK-KANACHK-INPUT
          WITH    POINTER WRK-KANACHK-MAX
          END-STRING
          COMPUTE WRK-KANACHK-MAX      =   WRK-KANACHK-MAX -   1
          PERFORM 800-HANKAKU-CHK-SEC
          MOVE    WRK-KANACHK-INPUT    TO  WRK-PARA-NYUGAIKBN-G
      *
          MOVE    SPACE                TO  WRK-KANACHK-INPUT
          MOVE    1                    TO  WRK-KANACHK-MAX
          STRING  WRK-PARA-SRYKA       DELIMITED   BY  SIZE
          INTO    WRK-KANACHK-INPUT
          WITH    POINTER WRK-KANACHK-MAX
          END-STRING
          COMPUTE WRK-KANACHK-MAX      =   WRK-KANACHK-MAX -   1
          PERFORM 800-HANKAKU-CHK-SEC
          MOVE    WRK-KANACHK-INPUT    TO  WRK-PARA-SRYKA
      *
      *
          PERFORM VARYING IDX0   FROM  1   BY  1
                  UNTIL ( IDX0   >     CONST-PARA-SRYCD-MAX )
      *
              MOVE    SPACE                TO  WRK-KANACHK-INPUT
              MOVE    1                    TO  WRK-KANACHK-MAX
              STRING  WRK-PARA-SRYCD (IDX0)    DELIMITED   BY  SIZE
              INTO    WRK-KANACHK-INPUT
              WITH    POINTER WRK-KANACHK-MAX
              END-STRING
              COMPUTE WRK-KANACHK-MAX      =   WRK-KANACHK-MAX -   1
              PERFORM 800-HANKAKU-CHK-SEC
              MOVE    WRK-KANACHK-INPUT    TO  WRK-PARA-SRYCD (IDX0)
      *
          END-PERFORM
      *
           MOVE    WRK-PARA-SRYYM      TO  WRK-SRYYMD
           MOVE    "01"                TO  WRK-SRYYMD(7:)
      *
           IF    ( WRK-PARA-NYUGAIKBN  =   "1" )
               CONTINUE
           ELSE
               MOVE    "2"             TO  WRK-PARA-NYUGAIKBN
           END-IF
      *
           IF    ( WRK-PARA-HOKATSUKBN =   "1" )
               CONTINUE
           ELSE
               MOVE    SPACE           TO  WRK-PARA-HOKATSUKBN
           END-IF
      *
      *    対象年月日編集
           MOVE    WRK-SRYYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3 (1:16)  TO  WRK-SRYYM-N
      *
      *    作成日編集
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3         TO  WRK-SAKYMD-N
      *
           MOVE    SPA-HOSPNUM     TO  WRK-TO-DATA-HOSPNUM
           MOVE    WRK-PARA-SRYYM  TO  WRK-TO-DATA-STRYM
      *
           PERFORM 1101-TSRYCD-HEN-SEC
      *
           IF    ( WRK-PARA-SYORIKBN   =   "1" )
           AND   ( WRK-PARA-NYUGAIKBN  NOT =   "1" )
               MOVE    "外来分は病棟別に集計できません"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
           IF    ( WRK-PARA-SYORIKBN   NOT =   "1" )
               MOVE    "診療科コード"  TO  CSV-SRYKA
               MOVE    "診療科名称"    TO  CSV-SRYKAMEI
               IF   ( WRK-PARA-SRYKA  NOT =   SPACE )
                   INITIALIZE              SYSKANRI-REC
                   MOVE    "1005"          TO  SYS-KANRICD
                   MOVE    WRK-PARA-SRYKA  TO  SYS-KBNCD
                   MOVE    WRK-SRYYMD      TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
                   MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   IF    ( FLG-SYSKANRI    =   ZERO )
                       CONTINUE
                   ELSE
                       MOVE    "診療科を正しく設定してください"
                                           TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF
               END-IF
           ELSE
               MOVE    "病棟番号"      TO  CSV-SRYKA
               MOVE    "病棟名称"      TO  CSV-SRYKAMEI
               IF    ( WRK-PARA-SRYKA  NOT =   SPACE )
                   MOVE    "病棟別の場合は診療科を入力しないでください"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
           IF ( FLG-END                =   ZERO )
               IF ( TSRYCD-MAX             =   ZERO )
                   IF    ( CNT-INPUT-ITEM  =   ZERO )
                       MOVE    "診療行為が未入力です"
                                       TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                   ELSE
                       MOVE    "診療行為の入力内容を確認してください"
                                       TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コードテーブル編集処理
      *****************************************************************
       1101-TSRYCD-HEN-SEC             SECTION.
      *
           MOVE    ZERO            TO   CNT-INPUT-ITEM
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-PARA-SRYCD-MAX )
      *
               IF    ( WRK-PARA-SRYCD (IDX0)   NOT =   SPACE )
      *
      *
                   COMPUTE CNT-INPUT-ITEM  =   CNT-INPUT-ITEM  +   1
      *
                   MOVE    SPACE   TO  WRK-SRYCD
                                       WRK-STR
      *
                   INSPECT WRK-PARA-SRYCD (IDX0)
                           REPLACING   ALL ":" BY  " "
      *
                   MOVE    WRK-PARA-SRYCD (IDX0)   TO  WRK-ITEM
                   UNSTRING    WRK-ITEM    DELIMITED   BY  " "
                   INTO        WRK-SRYCD
                               WRK-STR
                   END-UNSTRING
      *
                   MOVE    SPACE               TO  WRK-KANACHK-INPUT
                   MOVE    1                   TO  WRK-KANACHK-MAX
                   STRING  WRK-SRYCD    DELIMITED   BY  SIZE
                   INTO    WRK-KANACHK-INPUT
                   WITH    POINTER WRK-KANACHK-MAX
                   END-STRING
                   COMPUTE WRK-KANACHK-MAX     =   WRK-KANACHK-MAX -   1
                   PERFORM 800-HANKAKU-CHK-SEC
                   MOVE    WRK-KANACHK-INPUT   TO  WRK-SRYCD
      *
                   MOVE    ZERO                TO  WRK-KAISU
      *
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    WRK-STR         TO  SNUM-INX
                   CALL    "ORCSNUM"       USING   ORCSNUMAREA
                                    
                   IF    ( SNUM-RC         NOT =   ZERO  )
                    OR   ( SNUM-MINKBN         =   "1"   )
                       CONTINUE
                   ELSE
                       MOVE    SNUM-OUTNUM TO  WRK-KAISU
                   END-IF
      *
                   PERFORM   11011-TSRYCD-HEN-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           .
       1101-TSRYCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療コードテーブル編集処理
      *****************************************************************
       11011-TSRYCD-HEN-SEC            SECTION.
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   TSRYCD-MAX )
                    OR   ( TSRYCD-SRYCD (IDX2) =   WRK-SRYCD )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX2    >   TSRYCD-MAX )
               IF    ( CONST-TSRYCD-MAX    >   TSRYCD-MAX )
      *
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU       =   ZERO )
                       COMPUTE TSRYCD-MAX  =   TSRYCD-MAX  +   1
                       MOVE    WRK-SRYCD   TO  TSRYCD-SRYCD (TSRYCD-MAX)
                       MOVE    TNS-NAME    TO  TSRYCD-NAME  (TSRYCD-MAX)
                       MOVE    WRK-KAISU   TO  TSRYCD-KAISU (TSRYCD-MAX)
      *----(05.00.00)--ADD-START---
                       INITIALIZE              HCM34S03-AREA
                       MOVE    WRK-PARA-SRYYM
                                           TO  HCM34S03-SRYYM
                       MOVE    WRK-SRYCD   TO  HCM34S03-SRYCD
                       CALL    "ORCHCM34S03"   USING
                                               HCM34S03-AREA
                                               SPA-AREA
                       IF    ( HCM34S03-IGNORE =   1 )
                           MOVE    1       TO  TSRYCD-NG (TSRYCD-MAX)
                       END-IF
      *----(05.00.00)--ADD-END-----
                   END-IF
               END-IF
           END-IF
      *
           .
       11011-TSRYCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-IN01-EDIT-SEC
      *
           OPEN    INPUT   IN01-FILE
      *
           MOVE    ZERO            TO  FLG-IN01
           PERFORM 900-IN01SEQ-READ-SEC
      *
           IF      FLG-IN01    =   ZERO
      *        ＣＳＶ出力処理
               INITIALIZE                  WRK-REC
               MOVE    CSV-HEAD-01     TO  WRK-REC
               PERFORM 500-TOUKEICSV-SEC
           END-IF
      *
           PERFORM UNTIL ( FLG-IN01 NOT =   ZERO )
                     OR  ( FLG-END  NOT =   ZERO )
      *
      *        ヘッダー編集処理
               PERFORM 2001-HEAD-HEN-SEC
      *
      *        明細行編集処理
               PERFORM 2001-BODY-HEN-SEC
      *
      *        印刷処理
               PERFORM 2001-PRINT-SEC
      *
           END-PERFORM
      *
           CLOSE    IN01-FILE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為使用患者一覧ファイル編集処理
      *****************************************************************
       2001-IN01-EDIT-SEC              SECTION.
      *
           OPEN    OUTPUT  IN01-FILE
           CLOSE           IN01-FILE
      *
           OPEN    OUTPUT  IN02-FILE
           CLOSE           IN02-FILE
      *
           OPEN    I-O     IN01-FILE
           OPEN    I-O     IN02-FILE
      *
           PERFORM 20011-SRYACT-GET-SEC
      *
           IF    ( WRK-PARA-NYUGAIKBN  =   "1" )
               PERFORM 20012-NYUACT-GET-SEC
           END-IF
      *
           CLOSE           IN01-FILE
                           IN02-FILE
      *
           .
       2001-IN01-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為取得処理
      *****************************************************************
       20011-SRYACT-GET-SEC            SECTION.
      *
           PERFORM 900-SRYACT-KEY14-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SRYACT  NOT =   ZERO )
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL ( IDX3    >   CONST-SRYCD-MAX )
                   IF    ( SRY-SRYCD (IDX3)    NOT =   SPACE )
                       PERFORM VARYING IDX4    FROM    1   BY  1
                               UNTIL ( IDX4    >   TSRYCD-MAX )
                           IF    ( TSRYCD-SRYCD (IDX4)
                                               =   SRY-SRYCD (IDX3) )
                               PERFORM 20011-IN01-HEN-SEC
                               MOVE    TSRYCD-MAX  TO  IDX4
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
               PERFORM 900-SRYACT-KEY14-FET-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key14"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-SRYACT-GET-EXT.
           EXIT.
      *****************************************************************
      *    診療行為使用患者一覧ファイル編集処理
      *****************************************************************
       20011-IN01-HEN-SEC              SECTION.
      *
           PERFORM 900-SRYACCT-KEY-SEL-SEC
           IF    ( FLG-SRYACCT     NOT =   ZERO )
               DISPLAY "SRYACCT SEL ERR !! " SRY-PTID ":" SRY-ZAINUM
               GO  TO  20011-IN01-HEN-EXT
           END-IF
           IF    ( ACCT-ZAIKBN     =   1 ) OR
                 ( ACCT-ZAIKBN     =   2 )
               GO  TO  20011-IN01-HEN-EXT
           END-IF
      *
           IF    ( WRK-PARA-HOKATSUKBN   = "1" )
               IF  ( ACCT-JIHOKBN        = "1"
                OR   ACCT-HKNCOMBI       = 9999 )
                   GO  TO  20011-IN01-HEN-EXT
               END-IF
           END-IF
      *
           MOVE    SRY-PTID            TO  WRK-PTID
           PERFORM 900-PTINF-PTNUM-KEY-SEL-SEC
           IF    ( FLG-PTINF       NOT =   ZERO )
               DISPLAY "PTINF   SEL ERR !! " SRY-PTID
               GO  TO  20011-IN01-HEN-EXT
           END-IF
      *
           IF      WRK-PARA-SYORIKBN   NOT =   "1"
               PERFORM 20011-SRYKA-SET-SEC
           ELSE
               PERFORM 20011-NYUINACCT-GET-SEC
           END-IF
      *
           .
       20011-IN01-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計取得処理
      *****************************************************************
       20011-NYUINACCT-GET-SEC     SECTION.
      *
      *     MOVE    ZERO            TO  FLG-NYUINACCT
      *
           PERFORM 900-NYUINACCT-KEY9-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
               IF    ( NACCT-ZAISKBKBN =   "5" )
                   INITIALIZE          SS009-AREA
                   MOVE    "1"         TO      SS009-EXGAIHAKU
                   CALL    "ORCSS009"  USING   SS009-AREA
                                               NYUINACCT-REC
                                               SPA-AREA
                   IF    ( SS009-RC    =   ZERO )
                       PERFORM 20011-BTUNUM-SET-SEC
                   ELSE
                       DISPLAY "ERR SS009-RC:" SS009-RC
                   END-IF
               END-IF
               PERFORM 900-NYUINACCT-KEY9-FET-SEC
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
           MOVE    "key9"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-NYUINACCT-GET-EXT.
           EXIT.
      *****************************************************************
      *    診療科別件数セット処理
      *****************************************************************
       20011-SRYKA-SET-SEC         SECTION.
      *
           INITIALIZE                  IN01-REC
           MOVE    IDX4                TO  IN01-KEY-CDSEQ
           MOVE    SRY-SRYCD (IDX3)    TO  IN01-KEY-SRYCD
           MOVE    PTINF-KANANAME      TO  IN01-KEY-KANANAME
           MOVE    PTNUM-PTNUM         TO  IN01-KEY-PTNUM
           MOVE    PTINF-PTID          TO  IN01-KEY-PTID
           MOVE    PTINF-NAME          TO  IN01-NAME
           MOVE    PTINF-TSTPTNUMKBN   TO  IN01-TSTPTNUMKBN
           MOVE    SRY-SRYKA           TO  IN01-KEY-SRYKA
           PERFORM 900-IN01DYN-READ-SEC
           COMPUTE IN01-KAISU      =   IN01-KAISU
                                   +   ACCT-ZAIKAISU
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
               COMPUTE IN01-DAY (IDX-DAY)
                   =   IN01-DAY (IDX-DAY)
                   +   ACCT-DAY (1 IDX-DAY)
           END-PERFORM
           IF    ( FLG-IN01            =   ZERO )
               REWRITE                 IN01-REC
           ELSE
               WRITE                   IN01-REC
           END-IF
      *
           .
       20011-SRYKA-SET-EXT.
           EXIT.
      *****************************************************************
      *    病棟別件数セット処理
      *****************************************************************
       20011-BTUNUM-SET-SEC        SECTION.
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
               IF    ( SS009-BRMNUM-DAY(IDX-DAY)   NOT =   SPACE )
                   INITIALIZE                  IN01-REC
                   MOVE    IDX4                TO  IN01-KEY-CDSEQ
                   MOVE    SRY-SRYCD (IDX3)    TO  IN01-KEY-SRYCD
                   MOVE    PTINF-KANANAME      TO  IN01-KEY-KANANAME
                   MOVE    PTNUM-PTNUM         TO  IN01-KEY-PTNUM
                   MOVE    PTINF-PTID          TO  IN01-KEY-PTID
                   MOVE    PTINF-NAME          TO  IN01-NAME
                   MOVE    PTINF-TSTPTNUMKBN   TO  IN01-TSTPTNUMKBN
                   MOVE    SS009-BRMNUM-DAY(IDX-DAY)(1:2)
                                               TO  IN01-KEY-SRYKA
                   PERFORM 900-IN01DYN-READ-SEC
                   COMPUTE IN01-DAY (IDX-DAY)
                       =   IN01-DAY (IDX-DAY)
                       +   ACCT-DAY (1 IDX-DAY)
                   COMPUTE IN01-KAISU
                       =   IN01-KAISU
                       +   ACCT-DAY (1 IDX-DAY)
                   IF    ( FLG-IN01            =   ZERO )
                       REWRITE                 IN01-REC
                   ELSE
                       WRITE                   IN01-REC
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       20011-BTUNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為取得処理
      *****************************************************************
       20012-NYUACT-GET-SEC            SECTION.
      *
           PERFORM 900-NYUINACCT-KEY27-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
               IF    ( NACCT-ZAIKAISU  >   ZERO )
                   INITIALIZE          SS009-AREA
                   MOVE    "1"         TO      SS009-EXGAIHAKU
                   CALL    "ORCSS009"  USING   SS009-AREA
                                               NYUINACCT-REC
                                               SPA-AREA
                   IF    ( SS009-RC    =   ZERO )
                       PERFORM  20012-NSRYACT-GET-SEC
                   ELSE
                       DISPLAY "ERR SS009-RC:" SS009-RC
                   END-IF
               END-IF
               PERFORM 900-NYUINACCT-KEY27-FET-SEC
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20012-NYUACT-GET-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為取得処理
      *****************************************************************
       20012-NSRYACT-GET-SEC           SECTION.
      *
           PERFORM VARYING IDX-NACCT   FROM    1   BY  1
                   UNTIL ( IDX-NACCT   >   SS009-HENKYAKU-MAX )
      *----(05.00.00)--ADD-START---
               PERFORM 900-HKNCOMBI-READ-SEC
      *----(05.00.00)--ADD-END-----
               PERFORM VARYING IDX-NSRY    FROM    1   BY  1
                       UNTIL ( IDX-NSRY    >   CONST-NSRYACT-MAX )
                   IF    ( SS009-SRYCD (IDX-NACCT IDX-NSRY)
                                   NOT =   SPACE )
                       PERFORM VARYING IDX4    FROM    1   BY  1
                               UNTIL ( IDX4    >   TSRYCD-MAX )
                           IF    ( TSRYCD-SRYCD (IDX4)
                               =   SS009-SRYCD(IDX-NACCT IDX-NSRY) )
      *----(05.00.00)--UPD-START---
                               IF   (( COMB-HKNNUM =   "971"   OR 
                                       COMB-HKNNUM =   "973" ) AND
                                       TSRYCD-NG (IDX4)    =   1 )
                                   CONTINUE
                               ELSE
                                   PERFORM 20012-IN01-HEN-SEC
                                   MOVE    TSRYCD-MAX  TO  IDX4
                               END-IF
      *----(05.00.00)--UPD-END-----
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       20012-NSRYACT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為使用患者一覧診療会計情報編集処理
      *****************************************************************
       20012-IN01-HEN-SEC          SECTION.
      *
           MOVE    NACCT-PTID          TO  WRK-PTID
           PERFORM 900-PTINF-PTNUM-KEY-SEL-SEC
           IF    ( FLG-PTINF       NOT =   ZERO )
               DISPLAY "PTINF   SEL ERR !! " SRY-PTID
               GO  TO  20012-IN01-HEN-EXT
           END-IF
      *
           IF    ( WRK-PARA-SYORIKBN   NOT =   "1" )
               PERFORM 20012-SRYKA-SET-SEC
           ELSE
               PERFORM 20012-BTUNUM-SET-SEC
           END-IF
      *
           .
       20012-IN01-HEN-EXT.
           EXIT.
      *****************************************************************
      *   入院診療科別セット処理
      *****************************************************************
       20012-SRYKA-SET-SEC         SECTION.
      *
           INITIALIZE                  IN01-REC
           MOVE    IDX4                TO  IN01-KEY-CDSEQ
           MOVE    SS009-SRYCD (IDX-NACCT IDX-NSRY)
                                       TO  IN01-KEY-SRYCD
           MOVE    PTINF-KANANAME      TO  IN01-KEY-KANANAME
           MOVE    PTNUM-PTNUM         TO  IN01-KEY-PTNUM
           MOVE    PTINF-PTID          TO  IN01-KEY-PTID
           MOVE    PTINF-NAME          TO  IN01-NAME
           MOVE    PTINF-TSTPTNUMKBN   TO  IN01-TSTPTNUMKBN
           MOVE    SS009-SRYKA (IDX-NACCT)
                                       TO  IN01-KEY-SRYKA
           PERFORM 900-IN01DYN-READ-SEC
           COMPUTE IN01-KAISU  =   IN01-KAISU
                               +   SS009-ZAIKAISU (IDX-NACCT)
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
               COMPUTE IN01-DAY (IDX-DAY)
                   =   IN01-DAY (IDX-DAY)
                   +   SS009-DAY (IDX-NACCT  IDX-DAY)
           END-PERFORM
           IF    ( FLG-IN01            =   ZERO )
               REWRITE                 IN01-REC
           ELSE
               WRITE                   IN01-REC
           END-IF
      *
           .
       20012-SRYKA-SET-EXT.
           EXIT.
      *****************************************************************
      *   入院病棟別セット処理
      *****************************************************************
       20012-BTUNUM-SET-SEC         SECTION.
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
               IF    ( SS009-BRMNUM-DAY(IDX-DAY)   NOT =   SPACE )
                   INITIALIZE                  IN01-REC
                   MOVE    IDX4                TO  IN01-KEY-CDSEQ
                   MOVE    SS009-SRYCD (IDX-NACCT IDX-NSRY)
                                               TO  IN01-KEY-SRYCD
                   MOVE    PTINF-KANANAME      TO  IN01-KEY-KANANAME
                   MOVE    PTNUM-PTNUM         TO  IN01-KEY-PTNUM
                   MOVE    PTINF-PTID          TO  IN01-KEY-PTID
                   MOVE    PTINF-NAME          TO  IN01-NAME
                   MOVE    PTINF-TSTPTNUMKBN   TO  IN01-TSTPTNUMKBN
                   MOVE    SS009-BRMNUM-DAY(IDX-DAY)(1:2)
                                               TO  IN01-KEY-SRYKA
                   PERFORM 900-IN01DYN-READ-SEC
                   COMPUTE IN01-DAY (IDX-DAY)
                       =   IN01-DAY (IDX-DAY)
                       +   SS009-DAY (IDX-NACCT  IDX-DAY)
                   COMPUTE IN01-KAISU
                       =   IN01-KAISU
                       +   SS009-DAY (IDX-NACCT  IDX-DAY)
                   IF    ( FLG-IN01            =   ZERO )
                       REWRITE                 IN01-REC
                   ELSE
                       WRITE                   IN01-REC
                   END-IF
               END-IF
           END-PERFORM
     *
           .
       20012-BTUNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       2001-HEAD-HEN-SEC               SECTION.
      *
           INITIALIZE                  HCG016
      *
           MOVE    ZERO                TO  CNT-LINE
      *
           MOVE    IN01-KEY-SRYKA      TO  KEY-SRYKA
           MOVE    IN01-KEY-SRYCD      TO  KEY-SRYCD
      *
           IF      WRK-PARA-SYORIKBN   =   "1"
               INITIALIZE                  SYSKANRI-REC
               MOVE    "5001"              TO  SYS-KANRICD
               MOVE    KEY-SRYKA           TO  SYS-KBNCD
               MOVE    WRK-SRYYMD          TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
grpsys         MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC        TO  SYS-5001-REC
               MOVE    SYS-5001-BTU-TANNAME    TO  HCG016-SRYKA
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    "1005"              TO  SYS-KANRICD
               MOVE    KEY-SRYKA           TO  SYS-KBNCD
               MOVE    WRK-SRYYMD          TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
grpsys         MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME  TO  HCG016-SRYKA
           END-IF
      *
           MOVE    TSRYCD-NAME  (IN01-KEY-CDSEQ)
                                       TO  HCG016-SRYNAME
           MOVE    WRK-SRYYM-N         TO  HCG016-SRYYM
           MOVE    WRK-SAKYMD-N        TO  HCG016-SAKYMD
      *
           IF    ( WRK-PARA-NYUGAIKBN  =   "1" )
               MOVE    "（入院）"      TO  HCG016-NYUGAIKBN
           ELSE
               MOVE    "（外来）"      TO  HCG016-NYUGAIKBN
           END-IF
      *
           IF    ( WRK-PARA-HOKATSUKBN =   "1" )
               MOVE    "（包括分を含まない）"
                                       TO  HCG016-HOKATSUKBN
           END-IF
      *
           COMPUTE CNT-PAGE        =   CNT-PAGE        +   1
      *
           MOVE    CNT-PAGE            TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  HCG016-PAGE
      *
           .
      *
       2001-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       2001-BODY-HEN-SEC               SECTION.
      *
           PERFORM UNTIL ( FLG-IN01      =   1 )
                   OR    ( KEY-SRYCD NOT =   IN01-KEY-SRYCD )
                   OR    ( KEY-SRYKA NOT =   IN01-KEY-SRYKA )
                   OR    ( CNT-LINE     >=   CONST-LINE-MAX )
      *
               PERFORM 20011-BODY-NAIYO-HEN-SEC
      *
               PERFORM 900-IN01SEQ-READ-SEC
      *
           END-PERFORM
      *
           IF    ( FLG-IN01        NOT =   ZERO )
             OR  ( KEY-SRYCD       NOT =   IN01-KEY-SRYCD )
             OR  ( KEY-SRYKA       NOT =   IN01-KEY-SRYKA )
      *
               PERFORM 20011-SRYKA-TOTAL-HEN-SEC
      *
               MOVE    ZERO        TO  CNT-SEQ
               INITIALIZE              TOTAL-SRYKA-G
      *
               IF    ( FLG-IN01        NOT =   ZERO )
                 OR  ( KEY-SRYCD       NOT =   IN01-KEY-SRYCD )
                   PERFORM 20011-TOTAL-HEN-SEC
                   INITIALIZE          TOTAL-G
               END-IF
      *
           END-IF
      *
           .
       2001-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       20011-BODY-NAIYO-HEN-SEC        SECTION.
      *
           COMPUTE CNT-SEQ     =   CNT-SEQ     +   1
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           MOVE    CNT-SEQ             TO  WRK-ZZZ9
           MOVE    WRK-ZZZ9            TO  HCG016-RENNUM(CNT-LINE)
      *
           IF    ( IN01-KEY-PTNUM (11:)    =   SPACE )
               MOVE    IN01-KEY-PTNUM  TO  HCG016-PTNUM (CNT-LINE)
           ELSE
               MOVE    IN01-KEY-PTNUM  TO  HCG016-PTNUM2(CNT-LINE)
           END-IF
           MOVE    IN01-NAME           TO  HCG016-NAME  (CNT-LINE)
           MOVE    HCG016-NAME  (CNT-LINE)
                                       TO  WRK-KANACHK-INPUT
           PERFORM 800-ZENKAKU-CHK-SEC
           MOVE    WRK-KANACHK-INPUT   TO  HCG016-NAME  (CNT-LINE)
      *
           INITIALIZE                  HCG016S01-IN-AREA
grpsys*DD  MOVE    WRK-PARA-HOSPID     TO  HCG016S01-HOSPID
grpsys     MOVE    SPA-HOSPNUM         TO  HCG016S01-HOSPNUM
           MOVE    IN01-KEY-PTID       TO  HCG016S01-PTID
           MOVE    LNK-PRTKANRI-SKYYMD TO  HCG016S01-KJNYMD
           CALL    "ORCHCG016S01"      USING   HCG016S01-AREA
grpsys                                         SPA-AREA
           IF    ( HCG016S01-BRMNUM       NOT =   SPACE )
               MOVE    HCG016S01-BRMNUM
                                       TO  HCG016-LASTYMD
                                                    (CNT-LINE)
                                                    (2:)
           ELSE
               MOVE    HCG016S01-LASTYMD  TO  WRK-SYMD
               IF    ( HCG016S01-LASTYMD  NOT =   SPACE )
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-EDTYMD1 TO  HCG016-LASTYMD
                                                    (CNT-LINE)
               END-IF
           END-IF
      *
           MOVE   HCG016S01-HKNCOMBIMEI
                                       TO  HCG016-HKNCOMBI
                                                    (CNT-LINE)
      *
           MOVE    SPACE               TO  CSV-AREA
           MOVE    ZERO                TO  TOTAL-PTNUM
           PERFORM VARYING IDX-DAY     FROM    1   BY  1
                   UNTIL ( IDX-DAY     >   31 )
      *
               MOVE    IN01-DAY (IDX-DAY)
                                       TO  WRK-ZZZZ
               MOVE    WRK-ZZZZ        TO  HCG016-DAY(CNT-LINE IDX-DAY)
               MOVE    IN01-DAY (IDX-DAY)  TO WRK-SURYO
               PERFORM 400-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO    TO  CSV-DAY (IDX-DAY)
      *
               COMPUTE TOTAL-SRYKA-DAY (IDX-DAY)
                   =   TOTAL-SRYKA-DAY (IDX-DAY)
                                       +   IN01-DAY (IDX-DAY) 
      *
               COMPUTE TOTAL-PTNUM
                   =   TOTAL-PTNUM     +   IN01-DAY (IDX-DAY) 
      *
           END-PERFORM
      *
           MOVE    TOTAL-PTNUM         TO  WRK-ZZZZ
           MOVE    WRK-ZZZZ            TO  HCG016-DAY (CNT-LINE 32)
           MOVE    TOTAL-PTNUM     TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO    TO  CSV-DAY (32)
      *
           PERFORM 270-CSV-BODY-SET-SEC
           .
       20011-BODY-NAIYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ（データレコード）編集処理
      *****************************************************************
       270-CSV-BODY-SET-SEC        SECTION.
      *
           MOVE    WRK-PARA-SRYYM  TO  WRK-SYMD(1:6)
           MOVE    WRK-SYY         TO  WRK-HEN-YY-OT
           MOVE    WRK-SMM         TO  WRK-HEN-MM-OT
           MOVE    WRK-HEN-YMD-OT(1:7)
                                   TO  CSV-SRYYM
      *
           MOVE    HCG016S01-LASTYMD
                                   TO  WRK-SYMD
           MOVE    WRK-SYY         TO  WRK-HEN-YY-OT
           MOVE    WRK-SMM         TO  WRK-HEN-MM-OT
           MOVE    WRK-SDD         TO  WRK-HEN-DD-OT
           MOVE    WRK-HEN-YMD-OT  TO  CSV-LASTYMD

           INITIALIZE                  WRK-REC
      *
           STRING  CSV-SRYYM           DELIMITED BY SIZE
                   ","                 DELIMITED BY SIZE
                   WRK-PARA-NYUGAIKBN  DELIMITED BY SIZE
                   ","                 DELIMITED BY SIZE
                   HCG016-NYUGAIKBN(3:2)
                   ","                 DELIMITED BY SIZE
                   IN01-KEY-SRYCD      DELIMITED BY SIZE
                   ","                 DELIMITED BY SIZE
                   HCG016-SRYNAME      DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   KEY-SRYKA           DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCG016-SRYKA        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   IN01-KEY-PTNUM      DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   IN01-NAME           DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   IN01-KEY-KANANAME   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-LASTYMD         DELIMITED BY SIZE
                   ","                 DELIMITED BY SIZE
                   HCG016S01-HKNCOMBIMEI
                                       DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (1)         DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (2)         DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (3)         DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (4)         DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (5)         DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (6)         DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (7)         DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (8)         DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (9)         DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (10)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (11)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (12)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (13)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (14)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (15)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (16)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (17)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (18)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (19)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (20)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (21)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (22)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (23)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (24)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (25)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (26)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (27)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (28)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (29)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (30)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (31)        DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-DAY (32)        DELIMITED BY SPACE
                                 INTO  WRK-REC
           END-STRING
           PERFORM 500-TOUKEICSV-SEC
      *
           .
       270-CSV-BODY-SET-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<診療科合計行>処理
      *****************************************************************
       20011-SRYKA-TOTAL-HEN-SEC       SECTION.
      *
           MOVE    ZERO                TO  TOTAL-PTNUM
      *
           IF    ( WRK-PARA-SYORIKBN   NOT =   "1" )
               MOVE    "　診療科件数計"    TO  HCG016-HKNCOMBI
                                         (CONST-SRYKA-TOTAL-IDX)
           ELSE
               MOVE    "　　病棟件数計"    TO  HCG016-HKNCOMBI
                                         (CONST-SRYKA-TOTAL-IDX)
           END-IF
      *
           COMPUTE TOTAL-NINZU
               =   TOTAL-NINZU     +   CNT-SEQ
      *
           MOVE    CNT-SEQ             TO  WRK-ZZZZ
           MOVE    WRK-ZZZZ            TO  HCG016-RENNUM
                                            (CONST-SRYKA-TOTAL-IDX)
      *
           PERFORM VARYING IDX-DAY    FROM    1   BY  1
                   UNTIL ( IDX-DAY    >   31 )
      *
               MOVE    TOTAL-SRYKA-DAY (IDX-DAY)
                                       TO  WRK-ZZZZ
               MOVE    WRK-ZZZZ        TO  HCG016-DAY
                                         (CONST-SRYKA-TOTAL-IDX IDX-DAY)
      *
               COMPUTE TOTAL-DAY (IDX-DAY)
                   =   TOTAL-DAY (IDX-DAY)
                                       +   TOTAL-SRYKA-DAY (IDX-DAY) 
      *
               COMPUTE TOTAL-PTNUM
                   =   TOTAL-PTNUM     +   TOTAL-SRYKA-DAY (IDX-DAY)
      *
           END-PERFORM
      *
           MOVE    TOTAL-PTNUM         TO  WRK-ZZZZ
           MOVE    WRK-ZZZZ            TO  HCG016-DAY
                                            (CONST-SRYKA-TOTAL-IDX 32)
      *
           .
       20011-SRYKA-TOTAL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<合計行>処理
      *****************************************************************
       20011-TOTAL-HEN-SEC        SECTION.
      *
           MOVE    ZERO                TO  TOTAL-PTNUM
      *
           MOVE    "　診療行為件数計"  TO  HCG016-HKNCOMBI
                                         (CONST-TOTAL-IDX)
      *
           MOVE    TOTAL-NINZU         TO  WRK-ZZZZ
           MOVE    WRK-ZZZZ            TO  HCG016-RENNUM
                                           (CONST-TOTAL-IDX)
      *
           PERFORM VARYING IDX-DAY    FROM    1   BY  1
                   UNTIL ( IDX-DAY    >   31 )
      *
               MOVE    TOTAL-DAY (IDX-DAY)
                                       TO  WRK-ZZZZ
               MOVE    WRK-ZZZZ        TO  HCG016-DAY
                                           (CONST-TOTAL-IDX IDX-DAY)
      *
               COMPUTE TOTAL-PTNUM
                   =   TOTAL-PTNUM     +   TOTAL-DAY (IDX-DAY)
      *
           END-PERFORM
      *
           MOVE    TOTAL-PTNUM         TO  WRK-ZZZZ
           MOVE    WRK-ZZZZ            TO  HCG016-DAY
                                           (CONST-TOTAL-IDX 32)
      *
           .
       20011-TOTAL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2001-PRINT-SEC                  SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"           TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SPRT-TBL-GROUP
           MOVE    WRK-PARA-SRYYM
                                   TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SPRT-PRIORITY
           MOVE    "HCG016.red"    TO  SPRT-PRTID
           MOVE    "指定診療行為件数調"
                                   TO  SPRT-TITLE
           MOVE    HCG016          TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SPRT-PRTNM
           MOVE    "1"             TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
grpsys                                 SPA-AREA
           IF    ( SPRT-RETURN     =   ZERO )
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
      *
       2001-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票Ｚ編集処理
      *****************************************************************
       400-ZHEN-SURYO-SET-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-HENSURYO
           MOVE    ZERO            TO  CNT-KETA
           PERFORM VARYING CNT-KETA2   FROM    1   BY  1
                   UNTIL   CNT-KETA2   >   8
               IF      WRK-SURYO(CNT-KETA2:1)    NOT =   SPACE
                   COMPUTE CNT-KETA    =   CNT-KETA    +   1
                   MOVE    WRK-SURYO(CNT-KETA2:1)
                                           TO  WRK-HENSURYO(CNT-KETA:1)
               END-IF
           END-PERFORM
      *
           .
       400-ZHEN-SURYO-SET-EXT.
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
           MOVE    WRK-PARA-SRYYM  TO  STOUKEICSV-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN
                                   TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO            TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"     TO  STOUKEICSV-TO-FOLDER
           MOVE    WRK-TO-DATA     TO  STOUKEICSV-TO-DATA
           MOVE    "2"             TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-REC         TO  STOUKEICSV-CSVDATA
           MOVE    "指定診療行為件数調ＣＳＶ作成"
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
      *    異常終了処理
      *****************************************************************
       500-COBABORT-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-RECEERR
           STRING  "ORCBG016 "     DELIMITED  BY  SIZE
                   WRK-TOUKEIERR   DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-RECEERR
           END-STRING
           CALL    "cobabort"      USING   WRK-RECEERR
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    BG0161          TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    BG0162          TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
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
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角文字チェック
      *****************************************************************
       800-HANKAKU-CHK-SEC              SECTION.
      *
           IF  ( WRK-KANACHK-INPUT NOT =   SPACE )
               MOVE    SPACE           TO  ORCSKANACHKAREA
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-KANACHK-INPUT
                                       TO  KANACHK-MAE-INPUT
               MOVE    WRK-KANACHK-MAX TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-KANACHK-INPUT
           END-IF
      *
           .
       800-HANKAKU-CHK-EXT.
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
      *    点数マスタ検索処理(KEY)
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-SRYCD       TO  TNS-SRYCD
           MOVE    WRK-SRYYMD      TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
grpsys     MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
      *
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               INITIALIZE              TNS-TENSU-REC
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療会計検索処理(KEY)
      *****************************************************************
       900-SRYACCT-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYACCT
      *
           INITIALIZE                  SRYACCT-REC
grpsys*DD  MOVE    SRY-HOSPID      TO  ACCT-HOSPID
grpsys     MOVE    SPA-HOSPNUM     TO  ACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN   TO  ACCT-NYUGAIKBN
           MOVE    SRY-PTID        TO  ACCT-PTID
           MOVE    SRY-SRYKA       TO  ACCT-SRYKA
           MOVE    SRY-SRYYM       TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN      TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM      TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC     TO  IN02-REC
           READ    IN02-FILE
           INVALID KEY
               MOVE    SRYACCT-REC TO  MCPDATA-REC
               MOVE    "tbl_sryacct"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC      =   ZERO )
                   MOVE    MCPDATA-REC     TO  SRYACCT-REC
                                               IN02-REC
                   WRITE   IN02-REC
               ELSE
                   INITIALIZE          SRYACCT-REC
                   MOVE    1       TO  FLG-SRYACCT
               END-IF
      *
               MOVE    "tbl_sryacct"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           NOT INVALID KEY
               MOVE    IN02-REC    TO  SRYACCT-REC
           END-READ
      *
           .
       900-SRYACCT-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報、番号検索処理(KEY)
      *****************************************************************
       900-PTINF-PTNUM-KEY-SEL-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
grpsys*DD  MOVE    WRK-PARA-HOSPID TO  TPTINF-CHK-HOSPID
grpsys     MOVE    SPA-HOSPNUM     TO  TPTINF-CHK-HOSPNUM
           MOVE    WRK-PTID        TO  TPTINF-CHK-PTID
           PERFORM 9001-TPTINF-CHK-SEC
      *
           IF    ( TPTINF-FLG      =   ZERO )
               INITIALIZE              PTINF-REC
                                       PTNUM-REC
      *
grpsys*DD      MOVE    WRK-PARA-HOSPID
grpsys*DD                          TO  PTINF-HOSPID
grpsys         MOVE    SPA-HOSPNUM TO  PTINF-HOSPNUM
               MOVE    WRK-PTID    TO  PTINF-PTID
               MOVE    PTINF-REC   TO  MCPDATA-REC
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC      =   ZERO )
                   MOVE    MCPDATA-REC TO  PTINF-REC
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
      *
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               IF    ( FLG-PTINF       =   ZERO )
grpsys*DD         MOVE    WRK-PARA-HOSPID
grpsys*DD                              TO  PTNUM-HOSPID
grpsys             MOVE    SPA-HOSPNUM TO  PTNUM-HOSPNUM
                   MOVE    WRK-PTID    TO  PTNUM-PTID
                   MOVE    PTNUM-REC   TO  MCPDATA-REC
                   MOVE    "tbl_ptnum"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC      =   ZERO )
                       MOVE    MCPDATA-REC TO  PTNUM-REC
                   ELSE
                       INITIALIZE              PTNUM-REC
                   END-IF
      *
                   MOVE    "tbl_ptnum"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
      *
                   PERFORM 9001-TPTINF-HEN-SEC
      *
               END-IF
      *
           END-IF
      *
           .
       900-PTINF-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報退避エリアチェック処理
      *****************************************************************
       9001-TPTINF-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  TPTINF-FLG
           INITIALIZE                  PTINF-REC
                                       PTNUM-REC
      *
           PERFORM VARYING TPTINF-IDX  FROM    1   BY  1
                   UNTIL ( TPTINF-IDX  >   TPTINF-MAX )
                    OR   ( TPTINF-FLG  =   1 )
      *
grpsys*DD      IF    ( TPTINF-HOSPID (TPTINF-IDX)
grpsys*DD                              =   TPTINF-CHK-HOSPID )
grpsys         IF    ( TPTINF-HOSPNUM (TPTINF-IDX)
grpsys                                 =   TPTINF-CHK-HOSPNUM )
                 AND ( TPTINF-PTID   (TPTINF-IDX)
                                       =   TPTINF-CHK-PTID   )
                   MOVE    1           TO  TPTINF-FLG
                   MOVE    TPTINF-OCC   (TPTINF-IDX)
                                       TO  PTINF-REC
grpsys*DD          MOVE    TPTINF-HOSPID (TPTINF-IDX)
grpsys*DD                              TO  PTNUM-HOSPID
grpsys             MOVE    TPTINF-HOSPNUM (TPTINF-IDX)
grpsys                                 TO  PTNUM-HOSPNUM
                   MOVE    TPTINF-PTNUM (TPTINF-IDX)
                                       TO  PTNUM-PTNUM
               END-IF
      *
           END-PERFORM
      *
           .
       9001-TPTINF-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者情報退避エリア編集処理
      *****************************************************************
       9001-TPTINF-HEN-SEC             SECTION.
      *
           IF    ( CONST-TPTINF-MAX    >   TPTINF-MAX )
      *
               COMPUTE TPTINF-MAX      =   TPTINF-MAX  +   1
      *
               MOVE    PTINF-REC       TO  TPTINF-OCC   (TPTINF-MAX)
               MOVE    PTNUM-PTNUM     TO  TPTINF-PTNUM (TPTINF-MAX)
      *
           END-IF
      *
           .
       9001-TPTINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為使用患者一覧ファイル検索処理
      *****************************************************************
       900-IN01DYN-READ-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-IN01
      *
           READ    IN01-FILE
           INVALID KEY
               MOVE    1           TO  FLG-IN01
           END-READ
      *
           .
       900-IN01DYN-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療行為使用患者一覧ファイル読込処理
      *****************************************************************
       900-IN01SEQ-READ-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-IN01    NOT =   ZERO )
                   OR  ( FLG-LOOP-END  NOT =   ZERO )
      *
               READ    IN01-FILE   NEXT
               AT  END
                   MOVE    1           TO      FLG-IN01
               NOT AT END
                   MOVE    1           TO      FLG-LOOP-END
      *
                   EVALUATE    TRUE
                   WHEN  ( IN01-TSTPTNUMKBN    =   "1" )
                   WHEN  ( IN01-KAISU  <   1 )
                   WHEN  ( IN01-KAISU  <   TSRYCD-KAISU(IN01-KEY-CDSEQ))
                   WHEN  ( WRK-PARA-SRYKA  NOT =   SPACE  )
                     AND ( WRK-PARA-SRYKA  NOT =   IN01-KEY-SRYKA  )
                       MOVE    ZERO    TO      FLG-LOOP-END
                   END-EVALUATE
      *
               END-READ
      *
           END-PERFORM
      *
           .
       900-IN01SEQ-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療行為検索処理(KEY14)
      *****************************************************************
       900-SRYACT-KEY14-SEL-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYACT
      *
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM     TO  SRY-HOSPNUM
           MOVE    WRK-PARA-SRYYM  TO  SRY-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN
                                   TO  SRY-NYUGAIKBN
           MOVE    SRYACT-REC      TO  MCPDATA-REC
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key14"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-KEY14-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為FETCH処理(KEY14)
      *****************************************************************
       900-SRYACT-KEY14-FET-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYACT
      *
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key14"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-KEY14-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
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
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY9-SEL-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-NYUINACCT
      *
           INITIALIZE                  NYUINACCT-REC
           MOVE    SPA-HOSPNUM     TO  NACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN   TO  NACCT-NYUGAIKBN
           MOVE    SRY-PTID        TO  NACCT-PTID
           MOVE    SRY-SRYYM       TO  NACCT-SRYYM
           MOVE    "5"             TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC   TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
           MOVE    "key9"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY9-FET-SEC  SECTION.
      *
           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
           MOVE    "key9"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY9-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY27-SEL-SEC SECTION.
      *
           MOVE    ZERO            TO  FLG-NYUINACCT
      *
           INITIALIZE                  NYUINACCT-REC
           MOVE    SPA-HOSPNUM     TO  NACCT-HOSPNUM
           MOVE    WRK-PARA-SRYYM  TO  NACCT-SRYYM
           MOVE    "1"             TO  NACCT-NYUGAIKBN
           MOVE    "5"             TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC   TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
           MOVE    "key27"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY27-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY27-FET-SEC  SECTION.
      *
           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
           MOVE    "key27"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY27-FET-EXT.
           EXIT.
      *----(05.00.00)--ADD-START---
      *****************************************************************
      *    検索処理
      *****************************************************************
       900-HKNCOMBI-READ-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-COMB
      *
           INITIALIZE                  COMB-REC
           MOVE    SPA-HOSPNUM     TO  COMB-HOSPNUM
           MOVE    NACCT-PTID      TO  COMB-PTID
           MOVE    SS009-HKNCOMBI(IDX-NACCT)
                                   TO  COMB-HKNCOMBINUM 
           MOVE    COMB-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  COMB-REC
           ELSE
               INITIALIZE                  COMB-REC
               MOVE    1               TO  FLG-COMB
           END-IF
      *
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *----(05.00.00)--ADD-END-----
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       100-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
