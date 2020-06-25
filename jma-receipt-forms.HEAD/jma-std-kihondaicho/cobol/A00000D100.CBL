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
       PROGRAM-ID.             A00000D100.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次帳票
      *  コンポーネント名  : 基本台帳制御処理
      *  管理者            : 
      *  作成日付    作業者       記述
      *  04/02/09    @@@@-半田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    10/12/15  コメント履歴削除
      *                                     open-cobol1.0対応
      *  02.00.01    @@@@-門間    14/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
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
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-PARA            PIC 9(02).
           03  STS-PARA2           PIC 9(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                  PIC 9(01).
           03  FLG-SYSKANRI             PIC 9(01).
           03  FLG-SYSKANRI3            PIC 9(01).
           03  FLG-HKNCOMBI             PIC 9(01).
           03  FLG-PTBYOMEI             PIC 9(01).
           03  FLG-PTINF                PIC 9(01).
           03  FLG-PTNUM                PIC 9(01).
           03  FLG-PTHKNINF             PIC 9(01).
           03  FLG-SRYACCT              PIC 9(01).
           03  FLG-JYURRK               PIC 9(01).
           03  FLG-PTNYUINRRK           PIC 9(01).
           03  FLG-SRYACT               PIC 9(01).
           03  FLG-TENSU                PIC 9(01).
           03  FLG-SRYKA                PIC 9(01).
           03  FLG-SRYKBN               PIC 9(01).
           03  FLG-OK                   PIC 9(01).
           03  FLG-NG                   PIC 9(01).
           03  FLG-ATEND                PIC 9(01).
           03  FLG-GAISW                PIC 9(01).
           03  FLG-NYUSW                PIC 9(01).
           03  FLG-SRYKA-ALL            PIC 9(01).
           03  INP-STS                  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0               PIC 9(04).
           03  IDX1               PIC 9(02).
           03  IDX2               PIC 9(04).
           03  IDX3               PIC 9(04).
           03  IDX4               PIC 9(02).
           03  IDX5               PIC 9(04).
           03  IDX                PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID        PIC 9(07).
           03  WRK-PARA-SHELLID      PIC X(08).
           03  WRK-PARA-HOSPNUM       PIC X(24).
      *
           03  WRK-PARA-PRTYPE       PIC X(01).
           03  WRK-PARA-YMD1         PIC X(08).
           03  WRK-PARA-YMD2         PIC X(08).
           03  WRK-PARA-NYUGAIKBN    PIC X(01).
           03  WRK-PARA-SRYKACD      PIC X(02).
           03  WRK-PARA-DRCD         PIC X(05).
           03  WRK-PARA-ZERO         PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
      *  
           03  WRK-PRDTLCNT          PIC 9(06).
           03  WRK-RECCNT            PIC 9(06).
           03  WRK-HONKZKKBN         PIC X(04).
           03  WRK-CHK-SRYKA         PIC X(02).
      *
           03  WRK-SRYYMWH           PIC X(16).
           03  WRK-SYSYMDWH          PIC X(22).
           03  WRK-RENNUM            PIC 9(03).
           03  WRK-PTNUM             PIC X(20).
           03  WRK-PTNUM6            PIC 9(06). 
           03  WRK-SYUHKNNUM         PIC X(03). 
      *  
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
      *  
      *    郵便番号変換
           03  WRK-POSTMAEG.
               05  WRK-POSTMAE1    PIC X(03).
               05  WRK-POSTMAE2    PIC X(04).
           03  WRK-POSTATOG.
               05  WRK-POSTATO1    PIC X(03).
               05  WRK-POSTNAKA          PIC X(01)   VALUE  "-".
               05  WRK-POSTATO2    PIC X(04).
      *
           03  WRK-PTID2             PIC X(10).
           03  WRK-SRYCD2            PIC X(10).
           03  WRK-NYUGAIKBN         PIC X(01).
           03  WRK-NENREI            PIC 9(03).
           03  WRK-ADRS              PIC X(200).
           03  WRK-PRSRYKA           PIC X(02).
      *
           03  WRK-Z3                PIC ZZ9.
           03  WRK-PAGE              PIC ZZ9.
           03  WRK-Z9                PIC ZZZZZZZZZZZ9.
           03  WRK-ZZ                PIC ZZZZZZZZZZZZ.
           03  WRK-S9                PIC -----------9.
           03  WRK-SZ                PIC -----------Z.
           03  DEBUG-CNT             PIC 9(03).
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".        
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
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
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
           COPY    "A00000D100LNK.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
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
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  WRK-PARA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
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
                                               WRK-PARA-PRTYPE
                                               WRK-PARA-YMD1
                                               WRK-PARA-YMD2
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-SRYKACD
                                               WRK-PARA-DRCD
                                               WRK-PARA-ZERO
           END-UNSTRING
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE  LNK-PRTKANRI-RENNUM    TO
                              D100LNK-PRTKANRI-RENNUM
           MOVE  LNK-PRTKANRI-TBL-KEY   TO
                              D100LNK-PRTKANRI-TBL-KEY
           MOVE  LNK-PRTKANRI-TBL-GROUP TO
                              D100LNK-PRTKANRI-TBL-GROUP
           MOVE  LNK-PRTKANRI-SHORI-RENNUM      TO
                              D100LNK-PRTKANRI-SHORI-RENNUM
           MOVE  LNK-PRTKANRI-SRYYM     TO
                              D100LNK-PRTKANRI-SRYYM
           MOVE  LNK-PRTKANRI-SKYYMD    TO
                              D100LNK-PRTKANRI-SKYYMD
           MOVE  LNK-PRTKANRI-SHELLID   TO
                              D100LNK-PRTKANRI-SHELLID
           MOVE  LNK-PRTKANRI-PRIORITY  TO
                              D100LNK-PRTKANRI-PRIORITY
           MOVE  LNK-PRTKANRI-TERMID    TO
                              D100LNK-PRTKANRI-TERMID
           MOVE  LNK-PRTKANRI-OPID      TO
                              D100LNK-PRTKANRI-OPID
           MOVE  LNK-PRTKANRI-PRTNM     TO
                              D100LNK-PRTKANRI-PRTNM
      *
           MOVE  WRK-PARA-JOBID         TO    D100LNK-JOBID
           MOVE  WRK-PARA-SHELLID       TO    D100LNK-SHELLID
           MOVE  WRK-PARA-HOSPNUM       TO    D100LNK-HOSPNUM
           MOVE  WRK-PARA-YMD1          TO    D100LNK-YMD1
           MOVE  WRK-PARA-YMD2          TO    D100LNK-YMD2
           MOVE  WRK-PARA-NYUGAIKBN     TO    D100LNK-NYUGAIKBN
           MOVE  WRK-PARA-SRYKACD       TO    D100LNK-SRYKACD
           MOVE  WRK-PARA-DRCD          TO    D100LNK-DRCD
           MOVE  WRK-PARA-ZERO          TO    D100LNK-ZERO
      *
      *    帳票別サブプログラムCALL
           EVALUATE  WRK-PARA-PRTYPE
               WHEN  SPACE
      *              (日報)
                     EVALUATE  WRK-PARA-NYUGAIKBN
                         WHEN  "1"
      *                        基本台帳(入院日報)
                          CALL "A00000D122" USING  A00000D100-LNK
                         WHEN  OTHER
      *                        基本台帳(外来日報)
                          CALL "A00000D121" USING  A00000D100-LNK
                     END-EVALUATE
               WHEN  "0"
      *              (月報)
                     EVALUATE  WRK-PARA-NYUGAIKBN
                         WHEN  "1"
      *                        基本台帳(入院月報)
                          CALL "A00000D124" USING  A00000D100-LNK
                         WHEN  OTHER
      *                        基本台帳(外来月報)
                          CALL "A00000D123" USING  A00000D100-LNK
                     END-EVALUATE
               WHEN  "1"
      *              (診療課別)
                     EVALUATE  WRK-PARA-NYUGAIKBN
                         WHEN  "1"
      *                        基本台帳(入院・診療科別)
                          CALL "A00000D126" USING  A00000D100-LNK
                         WHEN  OTHER
      *                        基本台帳(外来・診療科別)
                          CALL "A00000D125" USING  A00000D100-LNK
                     END-EVALUATE
               WHEN  "2"
      *              (医師別)
                     EVALUATE  WRK-PARA-NYUGAIKBN
                         WHEN  "1"
      *                        基本台帳(入院・医師別)
                          CALL "A00000D128" USING  A00000D100-LNK
                         WHEN  OTHER
      *                        基本台帳(外来・医師別)
                          CALL "A00000D127" USING  A00000D100-LNK
                     END-EVALUATE
               WHEN  "3"
      *              (年齢別)
                     EVALUATE  WRK-PARA-NYUGAIKBN
                         WHEN  "1"
      *                        基本台帳(入院・年齢別)
                          CALL "A00000D130" USING  A00000D100-LNK
                         WHEN  OTHER
      *                        基本台帳(外来・年齢別)
                          CALL "A00000D129" USING  A00000D100-LNK
                     END-EVALUATE
               WHEN  "4"
      *              (地区別)
                     EVALUATE  WRK-PARA-NYUGAIKBN
                         WHEN  "1"
      *                        基本台帳(入院・地区別)
                          CALL "A00000D132" USING  A00000D100-LNK
                         WHEN  OTHER
      *                        基本台帳(外来・地区別)
                          CALL "A00000D131" USING  A00000D100-LNK
                     END-EVALUATE
               WHEN  "5"
      *              (保険別)
                     EVALUATE  WRK-PARA-NYUGAIKBN
                         WHEN  "1"
      *                        基本台帳(入院・保険別)
                          CALL "A00000D134" USING  A00000D100-LNK
                         WHEN  OTHER
      *                        基本台帳(外来・保険別)
                          CALL "A00000D133" USING  A00000D100-LNK
                     END-EVALUATE
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
