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
       PROGRAM-ID.             ORCBD001.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次帳票
      *  コンポーネント名  : 収納一覧表（ＨＣ０７）
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
      *  01.01.01    NACL-藤原    02/10/10  サイト区分追加
      *  03.05.01    NACL-森脇    07/06/08  グループ診療対応
      *  04.08.01    NACL-森脇    14/07/28  一時ファイルディレクトリ設定
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
      *    シェル用領域
      *     COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域 
grpsys*DD  COPY    "CPCOMMONDAT2.INC"
grpsys*DD                          REPLACING  //RECE01PARA//
grpsys*DD                          BY         //RECEERR//.
grpsys*DD  03  FILLER              PIC X(04)   VALUE   ".dat".
      *     
      *----(04.08.01)--ADD-START---
           COPY    "CPRECEERR.INC".
      *----(04.08.01)--ADD-END-----
      *
           COPY    "HC07.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-INPUT           PIC 9(01).
           03  FLG-BD001           PIC 9(01).  
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
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
grpsys*DD  03  WRK-PARA-HOSPID     PIC X(24).
grpsys     03  WRK-PARA-HOSPNUM    PIC 9(02).
      *----(04.08.01)--UPD-START---
grpsys*     03  RECEERR             PIC X(100).
      *----(04.08.01)--UPD-END-----
           03  WRK-PARA-DENPPRTYMD PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYKANAME         PIC X(20).
           03  WRK-PARA-DENPPRTYMDWH PIC X(22).
      *
      *
           03  WRK-RECEERR           PIC X(200).
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
           03  WRK-MISYU             PIC S9(08). 
      *
      *****03  WRK-IN                PIC X(01).
      *
           03  WRK-PAGE              PIC ZZ9.
           03  WRK-Z9                PIC Z,ZZZ,ZZ9.
           03  WRK-ZZ9               PIC ZZ,ZZZ,ZZ9.
      *
       01  SUM-AREA                         VALUE   ZERO.
           03  SUM-OCC               OCCURS  3.
               05  SUM-SEIKYU        PIC 9(08).
               05  SUM-RYOSYU        PIC 9(08).
      *
       01  KEY-AREA                         VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-SRYKA       PIC X(02).                       
           03  KEY-OLD.
               05  KEY-O-SRYKA       PIC X(02).                       
      *
           COPY    "CPSHELLTBL.INC".
      *     
      *    COPY    "ORCA-DBPATH".         
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *     
      *    収納マスタ
       01  BD001-REC.
           COPY    "CPBD001.INC".
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
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".          
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
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
           COPY    "CPORCMCP.INC".
      *
grpsys     COPY    "COMMON-SPA".
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
                   UNTIL       FLG-END     =   1
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SUM-AREA 
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
                                               WRK-PARA-DENPPRTYMD
grpsys     END-UNSTRING
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    "ORCBD001"      TO  JOB-PGID
           MOVE    "収納一覧表"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
grpsys                             SPA-AREA
      *
      *
grpsys*DD  MOVE    "RECEERR"       TO  RECEERR-FILE-ID
grpsys*DD  MOVE    LNK-PRTKANRI-TERMID
grpsys*DD                          TO  RECEERR-TERMID
      *
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *----(04.08.01)--ADD-END-----
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC                   SECTION.
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYSYMD
      * 
      *    対象年月日編集
           MOVE    WRK-PARA-DENPPRTYMD TO    WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-PARA-DENPPRTYMDWH 
      * 
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
grpsys     MOVE    WRK-SYSYMD      TO  SYS-1001-STYUKYMD
grpsys                                 SYS-1001-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
grpsys*DD      MOVE    SYS-1001-HOSPID     TO  WRK-PARA-HOSPID
           ELSE    
               MOVE    "医療機関ＩＤが取得できませんでした。"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF                                     
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    帳票印刷開始処理
           PERFORM 900-DBSELECT-SEC
      *
           PERFORM                     UNTIL   FLG-END     =   1
      *        診療科情報
               MOVE    BD001-SRYKA         TO  KEY-N-SRYKA    
               INITIALIZE                      SUM-OCC (2)
      *
               INITIALIZE                      SYS-1005-REC
               MOVE    "1005"              TO  SYS-1005-KANRICD
               MOVE    KEY-N-SRYKA         TO  SYS-1005-KBNCD
grpsys         MOVE    WRK-SYSYMD      TO  SYS-1005-STYUKYMD
grpsys                                     SYS-1005-EDYUKYMD
grpsys         MOVE    SPA-HOSPNUM     TO  SYS-1005-HOSPNUM
               MOVE    SYS-1005-REC        TO  MCPDATA-REC
               PERFORM 800-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1005-REC 
                   MOVE    SYS-1005-SRYKANAME  TO  WRK-SRYKANAME
               END-IF
      *
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM              UNTIL  FLG-END     =   1
                                   OR      KEY-NEW NOT =   KEY-OLD
      *            帳票編集<見出し>処理
                   PERFORM 310-HEAD-HEN-SEC
      *            帳票編集<明細>処理
                   PERFORM 320-BODY-HEN-SEC
      *            帳票編集<合計>処理
                   PERFORM 330-TERM-HEN-SEC
      *            帳票印刷処理
                   PERFORM 390-PRINT-OUT-SEC
               END-PERFORM
           END-PERFORM    
      *
           PERFORM 900-DBCLOSE-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
           ADD     1                   TO  CNT-PAGE
      *
           MOVE    SPACE               TO  HC07 
      *
           MOVE    WRK-PARA-DENPPRTYMDWH    TO  HC07-SRYYMD
      *
           MOVE    WRK-SRYKANAME       TO  HC07-SRYKA
      *
           MOVE    CNT-PAGE            TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC07-PAGE
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
           INITIALIZE                      SUM-OCC (1)
      *
           PERFORM             UNTIL   FLG-END     =   1
                               OR      KEY-NEW NOT =   KEY-OLD
                               OR      CNT-LINE    =   28
      *   
               ADD     1                   TO  CNT-LINE  
      *    患者番号
               MOVE    BD001-PTNUM         TO  HC07-PTNUM (CNT-LINE)
      *    氏名
               MOVE    BD001-NAME          TO  HC07-NAME  (CNT-LINE)
      *    性別
               EVALUATE    BD001-SEX
                   WHEN    "1"
                       MOVE    "男"            TO  HC07-SEX (CNT-LINE)
                   WHEN    "2"
                       MOVE    "女"            TO  HC07-SEX (CNT-LINE)
               END-EVALUATE
      *    生年月日
               MOVE    BD001-BIRTHDAY      TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  HC07-BIRTHDAY (CNT-LINE)
      *    請求金額
               MOVE    BD001-SKYMONEY      TO  WRK-Z9
               MOVE    WRK-Z9              TO  HC07-SEIKYU   (CNT-LINE)
      *    領収金額
               MOVE    BD001-NYUKIN-TOTAL  TO  WRK-Z9
               MOVE    WRK-Z9              TO  HC07-RYOSYU   (CNT-LINE)
      *    未収金額
               COMPUTE WRK-MISYU   =       BD001-SKYMONEY     -
                                           BD001-NYUKIN-TOTAL
               IF      WRK-MISYU           <   ZERO
                   MOVE    ZERO                TO  WRK-MISYU
               END-IF  
               MOVE    WRK-MISYU           TO  WRK-Z9
               MOVE    WRK-Z9              TO  HC07-MISYU   (CNT-LINE)
      *
               ADD     BD001-SKYMONEY      TO  SUM-SEIKYU (1)
                                               SUM-SEIKYU (2)
                                               SUM-SEIKYU (3)
               ADD     BD001-NYUKIN-TOTAL  TO  SUM-RYOSYU (1)
                                               SUM-RYOSYU (2)
                                               SUM-RYOSYU (3)       
      *
               PERFORM 900-DBFETCH-SEC
           END-PERFORM     
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *          *    帳票編集<合計>処理
      *****************************************************************
      *    
       330-TERM-HEN-SEC         SECTION.
      *
           MOVE    1                  TO  IDX
           PERFORM 331-TOTAL-HENSYU-SEC
      *
           IF      KEY-NEW        NOT =   KEY-OLD
               MOVE    2                  TO  IDX
               PERFORM 331-TOTAL-HENSYU-SEC
           END-IF    
      *
           IF      FLG-END            =   1
               MOVE    3                  TO  IDX
               PERFORM 331-TOTAL-HENSYU-SEC
           END-IF    
           .
       330-TERM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
      *    
       331-TOTAL-HENSYU-SEC       SECTION.
      *         
           EVALUATE    IDX
               WHEN    1
                   MOVE    "頁内合計"         TO  HC07-TTLMSG    (1)
               WHEN    2
                   MOVE    "科合計"           TO  HC07-TTLMSG    (2)
               WHEN    3
                   MOVE    "総合計"           TO  HC07-TTLMSG    (3)
           END-EVALUATE  
           MOVE    SUM-SEIKYU (IDX)    TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  HC07-SEIKYUTTL (IDX)
           MOVE    SUM-RYOSYU (IDX)    TO  WRK-ZZ9 
           MOVE    WRK-ZZ9             TO  HC07-RYOSYUTTL (IDX) 
           COMPUTE WRK-MISYU   =       SUM-SEIKYU (IDX)    -
                                       SUM-RYOSYU (IDX) 
           IF      WRK-MISYU           <   ZERO
               MOVE    ZERO                TO  WRK-MISYU
           END-IF  
           MOVE    WRK-MISYU           TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  HC07-MISYUTTL (IDX)
      *
           .
       331-TOTAL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC                SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
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
           MOVE   "HC07"               TO  SPRT-PRTID
           MOVE    "収納一覧表"        TO  SPRT-TITLE
           MOVE    HC07                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
grpsys                                 SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC                           
           END-IF                                                              
      *
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
057400*
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
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
grpsys         MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
grpsys                                 SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END     
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
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-PAGE        TO  JOB-UPDCNT                        
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
grpsys                             SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       900-DBSELECT-SEC                 SECTION.
      *
           INITIALIZE                  BD001-REC
grpsys*DD  MOVE    WRK-PARA-HOSPID     TO  BD001-HOSPID  
grpsys     MOVE    SPA-HOSPNUM         TO  BD001-HOSPNUM 
           MOVE    WRK-PARA-DENPPRTYMD TO  BD001-DENPPRTYMD
           MOVE    BD001-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     MOVE    "view_bd001"    TO  MCP-TABLE
grpsys     MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  MOVE    "BD001-KEY"         TO  ORC-DBPATH
grpsys*DD  CALL    "ORCMCPSUB"          USING
grpsys*DD                               MCPAREA
grpsys*DD                               ORCMCPAREA
grpsys*DD                               MCPDATA-REC
           IF      MCP-RC              =   ZERO
               PERFORM 900-DBFETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-BD001
                                               FLG-END
           END-IF        
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納を検索処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     MOVE    "view_bd001"    TO  MCP-TABLE
grpsys     MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  MOVE    "BD001-KEY"         TO  ORC-DBPATH
grpsys*DD  CALL    "ORCMCPSUB"          USING
grpsys*DD                               MCPAREA
grpsys*DD                               ORCMCPAREA
grpsys*DD                               MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-BD001
               MOVE    MCPDATA-REC         TO  BD001-REC
           ELSE
               MOVE    1                   TO  FLG-BD001
           END-IF        
      *
           IF      FLG-BD001           =   ZERO
               MOVE    BD001-SRYKA         TO  KEY-N-SRYKA
           ELSE
               MOVE    1                   TO  FLG-END
               MOVE    HIGH-VALUE              TO  KEY-NEW
           END-IF  
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     MOVE    "view_bd001"    TO  MCP-TABLE
grpsys     MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  MOVE    "BD001-KEY"         TO  ORC-DBPATH
grpsys*DD  CALL    "ORCMCPSUB"          USING
grpsys*DD                               MCPAREA
grpsys*DD                               ORCMCPAREA
grpsys*DD                               MCPDATA-REC
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
grpsys*DD  MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     MOVE    "tbl_syskanri"  TO  MCP-TABLE
grpsys     MOVE    "key10"         TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
grpsys*DD  CALL    "ORCMCPSUB"          USING
grpsys*DD                               MCPAREA
grpsys*DD                               ORCMCPAREA
grpsys*DD                               MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys         MOVE    "tbl_syskanri"  TO  MCP-TABLE
grpsys         MOVE    "key10"         TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                         MCPDATA-REC
grpsys                                         SPA-AREA
grpsys*DD      MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
grpsys*DD      CALL    "ORCMCPSUB"          USING
grpsys*DD                                   MCPAREA
grpsys*DD                                   ORCMCPAREA
grpsys*DD                                   MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys*DD  MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
grpsys     MOVE    "tbl_syskanri"  TO  MCP-TABLE
grpsys     MOVE    "key10"         TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "ORCMCPSUB"          USING
grpsys*DD                               MCPAREA
grpsys*DD                               ORCMCPAREA
grpsys*DD                                MCPDATA-REC
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA.
grpsys*DD  CALL    "ORCMCPSUB"          USING
grpsys*DD                               MCPAREA
grpsys*DD                               ORCMCPAREA
grpsys*DD                               MCPDATA-REC.
      *
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA.
grpsys*DD  CALL    "ORCMCPSUB"          USING
grpsys*DD                               MCPAREA
grpsys*DD                               ORCMCPAREA
grpsys*DD                               MCPDATA-REC.
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA.
grpsys*DD  CALL    "ORCMCPSUB"          USING
grpsys*DD                               MCPAREA
grpsys*DD                               ORCMCPAREA
grpsys*DD                               MCPDATA-REC.
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "ORCMCPSUB"          USING
grpsys*DD                               MCPAREA
grpsys*DD                               ORCMCPAREA
grpsys*DD                               MCPDATA-REC.
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
