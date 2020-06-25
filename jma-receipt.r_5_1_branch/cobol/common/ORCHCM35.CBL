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
       PROGRAM-ID.             ORCHCM35.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : レセプト件数枚数印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/08/20    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  00.00.01    NACL-森脇    06/01/20  編綴分対応
      *  03.04.01    NACL-森脇    06/11/29  パラメタ変更
      *  03.05.00    NACL-森脇    07/11/05  グループ診療対応
      *  04.02.01    NACL-森脇    08/04/16  平成２０年４月改正
      *  04.07.01    NACL-藤原    15/06/19  診療科表示件数の変更
      *  04.08.01    NACL-森脇    14/08/20  一時ファイルディレクトリ設定
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
      *----(04.08.01)--UPD-START---
      *     SELECT  PRT-FILE            ASSIGN  HC01PARA
           SELECT  PRT-FILE            ASSIGN  ASGNPARA
                                       FILE    STATUS  IS  STS-PARA.
      *----(04.08.01)--UPD-END-----
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
           COPY    "HCMC001.INC".
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *----(04.08.01)--ADD-START---
           COPY    "CPASGNPARA.INC".
      *----(04.08.01)--ADD-END-----
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA                PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                PIC 9(02).
           03  CNT-PAGE                PIC 9(03).
           03  CNT-TPAGE               PIC 9(10).
           03  CNT-TKENSU              PIC 9(10).
           03  CNT-GPAGE               PIC 9(10).
           03  CNT-GKENSU              PIC 9(10).
      *----(04.02.01)--ADD-START---
           03  CNT-SYAHO               PIC 9(02).
           03  CNT-KOKUHO              PIC 9(02).
           03  CNT-KOUIKI              PIC 9(02).
      *----(04.02.01)--ADD-END-----
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME                PIC 9(08).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
      *----(00.00.01)--ADD-START---
           03  IDY                     PIC 9(04).
      *----(00.00.01)--ADD-END-----
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
      *
           03  WRK-ZZ9                 PIC ZZ9.
           03  WRK-Z10                 PIC ZZZZZZZZZ9.
      *----(04.02.01)--ADD-START---
           03  WRK-NAME                PIC X(10).
      *----(04.02.01)--ADD-END-----
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *----(04.08.01)--ADD-START---
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       COPY    "COMMON-SPA".
       01  PARA-SYAPRTNM               PIC X(16).
      *----(03.04.01)--UPD-START---
       COPY    "R02SCR-SPA".
       01  SPA-SCR-REC.
           COPY    "CPR02SPASCR.INC".
      * 01  PARA-FILEKBN                PIC X(40).
      * 01  PARA-TBL1.
      *----(00.00.01)--UPD-START---
      *     03  PARA-TBL-OCC            OCCURS   21.
      *----(00.00.01)--UPD-END-----
      *         05  PARA-TBL-G          OCCURS   36.
      *             07  PARA-TPAGE      PIC 9(04).
      *             07  PARA-TKENSU     PIC 9(04).
      *             07  PARA-TSYURUINAME
      *                                 PIC X(40).
      *----(03.04.01)--UPD-END-----
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                       USING
                                       SPA-AREA
                                       PARA-SYAPRTNM
      *----(03.04.01)--UPD-START---
                                       SPA-R02
                                       SPA-SCR-REC.
      *                                 PARA-FILEKBN
      *                                 PARA-TBL1.
      *----(03.04.01)--UPD-END-----
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
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  SYS-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *----(03.04.01)--UPD-START---
      *----(04.02.01)--UPD-START---
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    0                   TO  CNT-GPAGE
           MOVE    0                   TO  CNT-GKENSU
           MOVE    0                   TO  CNT-TPAGE
           MOVE    0                   TO  CNT-TKENSU
           MOVE    0                   TO  CNT-SYAHO
           MOVE    0                   TO  CNT-KOKUHO
           MOVE    0                   TO  CNT-KOUIKI
      *
      *    帳票見出編集処理
           PERFORM 220-PRT-PAGE-SEC
      *
      *    社保
           MOVE    1                   TO  IDY
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   24
                   OR      SPA-GMN-TSYURUINAME(IDY IDX) =  SPACE
               ADD     1           TO  CNT-SYAHO
               PERFORM 210-LINE-SET-SEC
           END-PERFORM
      *
      *    社保合計
           PERFORM 210-GOKEI-SYAHO-SET-SEC
      *
      *    国保、広域
           PERFORM VARYING IDY     FROM    2   BY  1
                   UNTIL ( IDY     >   19 )
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL ( IDX     >   36 )
                       OR    ( SPA-GMN-TSYURUINAME(IDY IDX) =  SPACE )
                   IF      SPA-NAI-TEISYUTUSAKI(IDY IDX)  = "6"
      *                国保合計
                       PERFORM 210-GOKEI-KOKUHO-SET-SEC
                       MOVE    0           TO  CNT-KOKUHO
                       ADD     1           TO  CNT-KOUIKI
                   ELSE
                       ADD     1           TO  CNT-KOKUHO
                   END-IF
                   PERFORM 210-LINE-SET-SEC
               END-PERFORM
           END-PERFORM
      *
      *    国保合計
           PERFORM 210-GOKEI-KOKUHO-SET-SEC
      *
      *    広域合計
           PERFORM 210-GOKEI-KOUIKI-SET-SEC
      *
      *    特別療養費（広域）
           MOVE    1               TO  IDY
           MOVE    33              TO  IDX
           PERFORM 210-LINE-SET-SEC
      *
      *    特別療養費（国保）
           MOVE    1               TO  IDY
           MOVE    34              TO  IDX
           PERFORM 210-LINE-SET-SEC
      *
      *    自費
           MOVE    1               TO  IDY
           MOVE    35              TO  IDX
           PERFORM 210-LINE-SET-SEC
      *
      *    不明
           MOVE    1               TO  IDY
           MOVE    36              TO  IDX
           PERFORM 210-LINE-SET-SEC
      *
      *    合計
           PERFORM 210-GOKEI-GOKEI-SET-SEC
      *
      *----(00.00.01)--UPD-END-----
      *    印刷処理
           PERFORM 400-PRINT-OUT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *----(04.02.01)--UPD-END-----
      *----(03.04.01)--UPD-END-----
      *----(04.02.01)--ADD-START---
      *****************************************************************
      *    帳票明細セット処理
      *****************************************************************
       210-LINE-SET-SEC            SECTION.
      *
           IF      SPA-GMN-TSYURUINAME(IDY IDX) =  SPACE
               GO  TO  210-LINE-SET-EXT
           END-IF
      *
           IF      CNT-LINE    >=  35
      *        印刷処理
               PERFORM 400-PRINT-OUT-SEC
      *        帳票見出編集処理
               PERFORM 220-PRT-PAGE-SEC
           END-IF
      *
           ADD     1               TO  CNT-LINE
           MOVE    SPA-GMN-TSYURUINAME(IDY IDX)
                                   TO  HCMC001-NAME(CNT-LINE)
           MOVE    SPA-GMN-TKENSU(IDY IDX)
                                   TO  WRK-Z10
           MOVE    WRK-Z10         TO  HCMC001-NAIYO(CNT-LINE 1)
           MOVE    SPA-GMN-TPAGE(IDY IDX)
                                   TO  WRK-Z10
           MOVE    WRK-Z10         TO  HCMC001-NAIYO(CNT-LINE 2)
           ADD     SPA-GMN-TPAGE(IDY IDX)
                                   TO  CNT-TPAGE
           ADD     SPA-GMN-TKENSU(IDY IDX)
                                   TO  CNT-TKENSU
      *
           .
       210-LINE-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票合計セット処理
      *****************************************************************
       210-GOKEI-SET-SEC           SECTION.
      *
           ADD     1               TO  CNT-LINE
      *
           IF      CNT-LINE    >=  35
      *        印刷処理
               PERFORM 400-PRINT-OUT-SEC
      *        帳票見出編集処理
               PERFORM 220-PRT-PAGE-SEC
           END-IF
      *
           ADD     1               TO  CNT-LINE
           MOVE    WRK-NAME        TO  HCMC001-NAME(CNT-LINE)
           MOVE    CNT-TKENSU      TO  WRK-Z10
           MOVE    WRK-Z10         TO  HCMC001-NAIYO(CNT-LINE 1)
           MOVE    CNT-TPAGE       TO  WRK-Z10
           MOVE    WRK-Z10         TO  HCMC001-NAIYO(CNT-LINE 2)
      *
           .
       210-GOKEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票合計セット処理
      *****************************************************************
       210-GOKEI-GOKEI-SET-SEC     SECTION.
      *
           ADD     CNT-TPAGE           TO  CNT-GPAGE
           ADD     CNT-TKENSU          TO  CNT-GKENSU
      *
           ADD     1               TO  CNT-LINE
      *
           IF      CNT-LINE    >=  35
      *        印刷処理
               PERFORM 400-PRINT-OUT-SEC
      *        帳票見出編集処理
               PERFORM 220-PRT-PAGE-SEC
           END-IF
      *
           ADD     1               TO  CNT-LINE
      *
           MOVE    "合　　　計"    TO  HCMC001-NAME(CNT-LINE)
           MOVE    CNT-GKENSU      TO  WRK-Z10
           MOVE    WRK-Z10         TO  HCMC001-NAIYO(CNT-LINE 1)
           MOVE    CNT-GPAGE       TO  WRK-Z10
           MOVE    WRK-Z10         TO  HCMC001-NAIYO(CNT-LINE 2)
      *
           .
       210-GOKEI-GOKEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票社保合計セット処理
      *****************************************************************
       210-GOKEI-SYAHO-SET-SEC     SECTION.
      *
           IF      CNT-SYAHO       =   0
               GO  TO  210-GOKEI-SYAHO-SET-EXT
           END-IF
      *
           IF      SPA-GMN-PRTJYUN     =   "01" OR "02" OR "09" OR "10"
               MOVE    "社保　小計"    TO  WRK-NAME
               PERFORM 210-GOKEI-SET-SEC
           END-IF
      *
           ADD     CNT-TPAGE           TO  CNT-GPAGE
           ADD     CNT-TKENSU          TO  CNT-GKENSU
           MOVE    0                   TO  CNT-TPAGE
           MOVE    0                   TO  CNT-TKENSU
           ADD     1                   TO  CNT-LINE
      *
           .
       210-GOKEI-SYAHO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票国保合計セット処理
      *****************************************************************
       210-GOKEI-KOKUHO-SET-SEC    SECTION.
      *
           IF      CNT-KOKUHO      =   0
               GO  TO  210-GOKEI-KOKUHO-SET-EXT
           END-IF
      *
           IF      SPA-GMN-PRTJYUN1    =   "01" OR "02" OR "09" OR "10"
               MOVE    "国保　小計"    TO  WRK-NAME
               PERFORM 210-GOKEI-SET-SEC
           END-IF
      *
           ADD     CNT-TPAGE           TO  CNT-GPAGE
           ADD     CNT-TKENSU          TO  CNT-GKENSU
           MOVE    0                   TO  CNT-TPAGE
           MOVE    0                   TO  CNT-TKENSU
           ADD     1                   TO  CNT-LINE
      *
           .
       210-GOKEI-KOKUHO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票広域合計セット処理
      *****************************************************************
       210-GOKEI-KOUIKI-SET-SEC    SECTION.
      *
           IF      CNT-KOUIKI      =   0
               GO  TO  210-GOKEI-KOUIKI-SET-EXT
           END-IF
      *
           IF      SPA-GMN-PRTJYUN2    =   "01" OR "02" OR "09" OR "10"
               MOVE    "広域　小計"    TO  WRK-NAME
               PERFORM 210-GOKEI-SET-SEC
           END-IF
      *
           ADD     CNT-TPAGE           TO  CNT-GPAGE
           ADD     CNT-TKENSU          TO  CNT-GKENSU
           MOVE    0                   TO  CNT-TPAGE
           MOVE    0                   TO  CNT-TKENSU
           ADD     1                   TO  CNT-LINE
      *
           .
       210-GOKEI-KOUIKI-SET-EXT.
           EXIT.
      *
      *----(04.02.01)--ADD-END-----
      *****************************************************************
      *    帳票<見出>編集処理
      *****************************************************************
       220-PRT-PAGE-SEC            SECTION.
      *
           INITIALIZE                  HCMC001
      *
           ADD     1                   TO  CNT-PAGE
           MOVE    ZERO                TO  CNT-LINE
      *
      *    タイトル
      *----(03.04.01)--UPD-START---
           MOVE    SPA-GMN-FILEKBN     TO  HCMC001-DAI
      *     MOVE    PARA-FILEKBN        TO  HCMC001-DAI
      *
      *    印刷日付
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HCMC001-YMD
      *
      *    ページ数
           MOVE    CNT-PAGE            TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  HCMC001-PAGE
      *
      *    見出し
           MOVE    "レセプト種別"      TO  HCMC001-TAI-NAME
           MOVE    "患者数"            TO  HCMC001-TAI-NAIYO(1)
           MOVE    "レセプト枚数"      TO  HCMC001-TAI-NAIYO(2)
      *
           .
       220-PRT-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-OUT-SEC           SECTION.
      *
      *    印刷用ファイル作成
      *----(03.05.00)--ADD-START---
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
      *----(03.05.00)--ADD-END-----
           MOVE    "HC01"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
           MOVE    CNT-PAGE            TO  HC01PARA-CNT
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *----(04.08.01)--ADD-END-----
           OPEN    OUTPUT              PRT-FILE
           WRITE   HCMC001
           CLOSE                       PRT-FILE
      *
           INITIALIZE                  ORCSMKPRTAREA
           MOVE    "HC01.sh"           TO  MKPRT-ID
           MOVE    "HCMC001.red"       TO  MKPRT-DIA
           MOVE    SPACE               TO  MKPRT-DEF
      *----(04.08.01)--UPD-START---
      *     MOVE    HC01PARA            TO  MKPRT-DAT
           MOVE    ASGNPARA            TO  MKPRT-DAT
      *----(04.08.01)--UPD-END-----
           MOVE    PARA-SYAPRTNM       TO  MKPRT-PRTNM
           CALL    "ORCSMKPRT"         USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       400-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "*** ORCHCM35 END ***"
           .
       300-END-EXT.
           EXIT.
      *
