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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBM020.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 決定通知書（社保）（ＨＣ２６）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-藤原      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     02/04/10  医療機関コードの編集修正 
      *  01.00.02    MCC-藤原     02/05/22  テスト患者は処理しない     
      *
      *  01.01.01    NACL-藤原    02.09.17  印刷サブの追加
      *  01.01.02    NACL-藤原    03.07.01  入院・入院外による集計
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.02.01    NACL-藤原    08/11/05  協会けんぽ対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PARA.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.

      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
           COPY    "CPPRTALL.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys COPY    "COMMON-SPA".
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
           COPY    "CPASGNPARA.INC".
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SKYYM  PIC X(06).
               05  WRK-PARA-TERMID PIC X(16).
               05  WRK-PARA-SYSYMD PIC X(08).
               05  WRK-PARA-TERMID64
                                   PIC X(64).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *
           03  WRK-SRYYMWH         PIC X(16).
           03  WRK-SRYYMD          PIC X(08).
      *
           03  WRK-RECEERR         PIC X(200). 
      *     
           03  WRK-HC01PARA-CNT    PIC 9(06).
      *
           03  WRK-Z9              PIC ZZZZZ.
           03  WRK-ZZ9             PIC ZZZ,ZZZ,ZZZ.
      *
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
           03  WRK-ENCODING        PIC X(01).
           03  WRK-ENCODING9       REDEFINES WRK-ENCODING
                                   PIC 9(01).
           03  WRK-UUID            PIC X(36).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-RECE10          PIC 9(06).
      *
      *    集計領域
       01  SUM-AREA.
           03  SUM-TBL-G                   OCCURS  3.
               05  SUM-TBL                 OCCURS  2.
                   07  SUM-KENSU           PIC 9(06).
                   07  SUM-TOTALTEN        PIC 9(10).
                   07  SUM-FTNMONEY        PIC 9(10).
                   07  SUM-YKZFTN          PIC 9(10).
                   07  SUM-SHOKUJIKENSU    PIC 9(06).
                   07  SUM-SHOKUJIRYOYOHI  PIC 9(10).
                   07  SUM-SHOKUJIFTN      PIC 9(10).
           03  SUM-KOH-TBL-G               OCCURS  100.
               05  SUM-KOH-KOHNUM          PIC X(03).  
               05  SUM-KOH-KENSU           PIC 9(06).
               05  SUM-KOH-TOTALTEN        PIC 9(10).
               05  SUM-KOH-FTNMONEY        PIC 9(10).
               05  SUM-KOH-YKZFTN          PIC 9(10).
      *
       01  CONST-AREA.
           03  CONST-FORMID        PIC X(20)   VALUE  "HC26.red".
      *
           COPY    "HC26.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    職員情報
           COPY  "CPSK1010.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
           COPY    "CPSK1044.INC".
      *
           COPY    "CPRCF010.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *   ＵＩＤ取得用エリア
       01 UIDIO-AREA.
           03  UIDIO-RET   PIC X(2).
           03  UIDIO-UID   PIC X(36).
       01  UIDIO-ST        PIC 9(2).
      *
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
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
                                       SPA-AREA
      *
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
           MOVE    MCP-USER            TO  SPA-OPID
      *
           INITIALIZE              UIDIO-AREA.
           CALL    "orcuidset"     USING
                                   UIDIO-ST
                                   UIDIO-AREA
      *
           MOVE    UIDIO-UID           TO  WRK-UUID
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
      *
           CALL    "ORCSENCODING"          USING
                                           MCPAREA
                                           SPA-AREA
           MOVE    SPA-ENCODING    TO      WRK-ENCODING9
      *     
           MOVE    WRK-PARA-SKYYM      TO  WRK-SRYYMD (1:6)
           MOVE    "01"                TO  WRK-SRYYMD (7:2)
           MOVE    WRK-PARA-SKYYM      TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-SRYYMWH 
      *
      *    医療機関ＩＤ編集
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  SYS-1001-HOSPNUM
           MOVE    WRK-PARA-SYSYMD     TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE
               MOVE    "医療機関情報が取得できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF 
      *     
      *    出力先プリンタ名割り当て情報編集
           MOVE    SPACE               TO  SYS-1031-REC
           INITIALIZE                  SYS-1031-REC
           MOVE    "1031"              TO  SYS-1031-KANRICD
           MOVE    "*"                 TO  SYS-1031-KBNCD
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  SYS-1031-HOSPNUM
           MOVE    WRK-PARA-SYSYMD     TO  SYS-1031-STYUKYMD
                                           SYS-1031-EDYUKYMD
           MOVE    SYS-1031-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1031-REC
           ELSE    
               INITIALIZE                  SYS-1031-REC
           END-IF                                     
      *
           PERFORM 800-OFFSET-GET-SEC
      *
           MOVE    "HC26"              TO  HC01PARA-FORM-ID
           MOVE    WRK-PARA-HOSPNUM    TO  HC01PARA-HOSPNUM
           MOVE    WRK-PARA-TERMID64   TO  HC01PARA-TERMID
           MOVE    SMCNDATE-HMS        TO  HC01PARA-SYOHMS
           MOVE    1                   TO  HC01PARA-CNT
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    HC01PARA-BASENAME
                                   TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"   USING
                                   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  ASGNPARA
           MOVE    SGETTEMP-TEMPDIR
                                   TO  MCP-TEMPDIR
           MOVE    SGETTEMP-ST     TO  ASGNPARA-ST
      *  
           PERFORM 900-SEIKYU-SELECT-SEC 
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO                    TO  IDX-AREA
                                               FLG-UPD
      *
      *    入外区分
           EVALUATE    RECE10-NYUGAIKBN
               WHEN    1
                   MOVE    1                   TO  IDX3
               WHEN    2
                   MOVE    2                   TO  IDX3
           END-EVALUATE
      *
      *    老人保健欄
           IF      RECE10-RJNHKNNUM    NOT =   SPACE
               MOVE    1                   TO  IDX 
               PERFORM 210-HKN-SYUKEI-SEC
           END-IF
      *     
      *    医療保険欄
           IF      RECE10-RJNHKNNUM        =   SPACE
           AND     RECE10-HKNNUM       NOT =   SPACE
               IF      RECE10-HONKZKKBN        =   "1"
                   IF      RECE10-KOHNUM (1)   NOT =   SPACE
                       MOVE    2                   TO  IDX 
                   ELSE
                       EVALUATE    RECE10-HKNNUM
                           WHEN    "009"
                           WHEN    "001"
                           WHEN    "002"
                           WHEN    "003"
                           WHEN    "004"
                           WHEN    "031"  THRU  "034"
                           WHEN    "006"
                           WHEN    "007"               
                           WHEN    "063"
                           WHEN    "072"  THRU  "075"
                               MOVE    2           TO  IDX 
                       END-EVALUATE
                   END-IF
               ELSE
                   IF      RECE10-KOHNUM (1)   NOT =   SPACE
                       MOVE    3                   TO  IDX 
                   ELSE
                       EVALUATE    RECE10-HKNNUM
                           WHEN    "009"
                           WHEN    "001"
                           WHEN    "002"
                           WHEN    "003"
                           WHEN    "004"
                           WHEN    "031"  THRU  "034"
                           WHEN    "006"
                           WHEN    "063"
                           WHEN    "072"  THRU  "075"
                               MOVE    3            TO  IDX
                       END-EVALUATE
                   END-IF
               END-IF
               PERFORM 210-HKN-SYUKEI-SEC
           END-IF    
      *
      *    公費負担分     
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       4
                   OR  RECE10-KOHNUM (IDZ) =   SPACE
               PERFORM 220-KOH-SYUKEI-SEC
           END-PERFORM                
      *
      *    集計の対象にならなかったとき                
           IF      FLG-UPD             =   1
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "集計対象外　患者番号＝"     DELIMITED  BY  SIZE
                       RECE10-PTNUM                DELIMITED  BY  SPACE
                       " 診療年月＝"               DELIMITED  BY  SIZE
                       RECE10-SRYYM                DELIMITED  BY  SIZE
                       " レセ種別＝"               DELIMITED  BY  SIZE
                       RECE10-RECESYUBETU          DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING                                
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF 
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・老人保健分集計処理
      *****************************************************************
       210-HKN-SYUKEI-SEC          SECTION.
      *
           IF      IDX             =   ZERO
           OR      IDX3            =   ZERO
               GO  TO  210-HKN-SYUKEI-EXT
           END-IF
      *
      *    医療保険・老人保健分
           MOVE    1                         TO  IDX1
           MOVE    IDX                       TO  IDX2
           PERFORM 2101-HKN-SYUKEI-01-SEC
      *
           .
       210-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・老人保健分集計処理
      *****************************************************************
       2101-HKN-SYUKEI-01-SEC      SECTION.
      *    
      *    件数
           ADD     1                   TO  SUM-KENSU (IDX2 IDX3)
           IF      RECE10-SHOKUJINISSU(IDX1)   >   ZERO
               ADD     1               TO  SUM-SHOKUJIKENSU(IDX2 IDX3)
           END-IF
      *
      *    診療実日数〜一部負担金 
           ADD     RECE10-TOTALTEN       (IDX1)
                                     TO  SUM-TOTALTEN      (IDX2 IDX3)
           ADD     RECE10-YKZFTN         (IDX1)
                                     TO  SUM-YKZFTN        (IDX2 IDX3)
           ADD     RECE10-FTNMONEY       (IDX1)
                                     TO  SUM-FTNMONEY      (IDX2 IDX3)
           ADD     RECE10-SHOKUJIRYOYOHI (IDX1)
                                     TO  SUM-SHOKUJIRYOYOHI(IDX2 IDX3)
           ADD     RECE10-SHOKUJIFTN     (IDX1)
                                     TO  SUM-SHOKUJIFTN    (IDX2 IDX3)
      *
           MOVE    1               TO  FLG-UPD 
           .
       2101-HKN-SYUKEI-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担分集計処理
      *****************************************************************
       220-KOH-SYUKEI-SEC        SECTION.
      *
           PERFORM VARYING IDW     FROM    1   BY  1
                   UNTIL   IDW     >       100
               IF      SUM-KOH-KOHNUM(IDW)     =   SPACE
                   MOVE    RECE10-KOHNUM (IDZ)
                                       TO  SUM-KOH-KOHNUM (IDW)
               END-IF
               IF      RECE10-KOHNUM (IDZ) =   SUM-KOH-KOHNUM (IDW)             
                   ADD     1           TO  SUM-KOH-KENSU         (IDW)
                   COMPUTE IDY     =   IDZ     +   1
                   ADD     RECE10-TOTALTEN (IDY)
                                       TO  SUM-KOH-TOTALTEN      (IDW)
                   ADD     RECE10-YKZFTN   (IDY)
                                       TO  SUM-KOH-YKZFTN        (IDW)      
                   ADD     RECE10-FTNMONEY (IDY)
                                       TO  SUM-KOH-FTNMONEY      (IDW) 
                   MOVE    100         TO  IDW                        
               END-IF
           END-PERFORM                                
      *
           MOVE    1               TO  FLG-UPD 
      *
           .
       220-KOH-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
           END-IF
      *
           CLOSE   RECEERR-FILE
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
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      CNT-RECE10          >   ZERO 
               PERFORM 310-PRINT-SEC
           END-IF    
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORCBM020 IN  "  CNT-RECE10
           DISPLAY "*** ORCBM020 END ***"
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       310-PRINT-SEC                SECTION.
      *
           OPEN    OUTPUT              PRT-FILE
      *
      *    帳票編集<見出し>処理
           PERFORM 310-HEAD-HEN-SEC
      *     
      *    帳票編集<明細>処理
           PERFORM 320-BODY-HEN-SEC
      *
      *    帳票印刷処理
           PERFORM 390-PRINT-OUT-SEC
      *
           CLOSE   PRT-FILE
      *
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "HCALL_prv.sh"  TO  SHELLTBL-NAME
           MOVE    ASGNPARA (ASGNPARA-ST:)
                                   TO  SHELLTBL-ARG1
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG2
           MOVE    WRK-ENCODING    TO  SHELLTBL-ARG3
           IF    ( SPA-CLIENT-PRT-FLG  =   2 )
              MOVE    "決定通知書"
                                   TO  SHELLTBL-ARG5
              MOVE    SPA-CLIENT-SHOWDIALOG
                                   TO  SHELLTBL-ARG6
           END-IF
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "daily"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       310-PRINT-EXT.
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
           MOVE    SPACE               TO  HC26
      *
           MOVE    WRK-SRYYMWH         TO  HC26-PRTYM
           MOVE    SYS-1001-HOSPCDN    TO  HC26-HOSPCD
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC          SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       3
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       2
                   MOVE    SUM-KENSU       (IDX IDY)
                                           TO  WRK-Z9
                   MOVE    WRK-Z9          TO  HC26-KENSU    (IDX IDY)
                   MOVE    SUM-TOTALTEN    (IDX IDY)
                                           TO  WRK-ZZ9
                   MOVE    WRK-ZZ9         TO  HC26-TENSU    (IDX IDY)
                   MOVE    SUM-YKZFTN      (IDX IDY)
                                           TO  WRK-ZZ9
                   MOVE    WRK-ZZ9         TO  HC26-YKZFTN   (IDX IDY)
                   MOVE    SUM-FTNMONEY    (IDX IDY)
                                           TO  WRK-ZZ9
                   MOVE    WRK-ZZ9         TO  HC26-ITBFTN   (IDX IDY)
                   IF      IDY             =   1
                       MOVE    SUM-SHOKUJIKENSU(IDX IDY)
                                           TO  WRK-Z9
                       MOVE    WRK-Z9      TO  HC26-SYOKENSU (IDX)
                       MOVE    SUM-SHOKUJIRYOYOHI(IDX IDY)
                                           TO  WRK-ZZ9
                       MOVE    WRK-ZZ9     TO  HC26-SYOKINGAK(IDX)
                       MOVE    SUM-SHOKUJIFTN   (IDX IDY)
                                           TO  WRK-ZZ9
                       MOVE    WRK-ZZ9     TO  HC26-SYOFTNGAK(IDX)
                   END-IF
               END-PERFORM    
           END-PERFORM
      *     
      *    帳票編集<公費分>処理
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       100
                   OR      SUM-KOH-KOHNUM (IDX)    =   SPACE
               PERFORM 330-BODY-KOH-HEN-SEC
           END-PERFORM    
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       330-BODY-KOH-HEN-SEC      SECTION.
      *
           IF      CNT-LINE        >=  27
      *        帳票印刷処理
               PERFORM 390-PRINT-OUT-SEC
      *        帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
           END-IF                    
      *
           ADD     1               TO  CNT-LINE
      *     
           PERFORM 3202-HKNNUM-HENSYU-SEC     
      *
           MOVE    SUM-KOH-KENSU       (IDX)
                               TO  WRK-Z9
           MOVE    WRK-Z9      TO  HC26-KOHKENSU    (CNT-LINE)
           MOVE    SUM-KOH-TOTALTEN    (IDX)
                               TO  WRK-ZZ9
           MOVE    WRK-ZZ9     TO  HC26-KOHTENSU    (CNT-LINE)
           MOVE    SUM-KOH-YKZFTN      (IDX)
                               TO  WRK-ZZ9
           MOVE    WRK-ZZ9     TO  HC26-KOHYKZFTN   (CNT-LINE)
           MOVE    SUM-KOH-FTNMONEY    (IDX)
                               TO  WRK-ZZ9
           MOVE    WRK-ZZ9     TO  HC26-KOHFTNGAK   (CNT-LINE)
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費保険区分編集処理
      *****************************************************************
       3202-HKNNUM-HENSYU-SEC         SECTION.
      *
           MOVE    SUM-KOH-KOHNUM  (IDX) (2:2)
                                       TO  HC26-KOHNUM (CNT-LINE)
      *        保険番号読込み  
           INITIALIZE                      HKNNUM-REC
grpsys     MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE   SUM-KOH-KOHNUM  (IDX)
                                       TO  HKN-HKNNUM
           MOVE    WRK-SRYYMD          TO  HKN-TEKSTYMD
                                           HKN-TEKEDYMD                               
           MOVE   HKNNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-HKNNUM-READ-SEC
           ELSE
               MOVE    1                  TO  FLG-HKNNUM
           END-IF
      *
           PERFORM UNTIL       FLG-HKNNUM      =   1
               IF     SUM-KOH-KOHNUM  (IDX)    =   "027"
                   IF      SYS-1001-HOSPSBT    =   HKN-PAYKBN (1:1)
                   AND     SYS-1001-ROUPAYKBN  =   HKN-PAYKBN (2:1)
                       MOVE    HKN-TANSEIDONAME  
                                           TO  HC26-KOHKBN (CNT-LINE)
                       MOVE    1           TO  FLG-HKNNUM   
                   END-IF
               ELSE
                   MOVE    HKN-TANSEIDONAME    
                                           TO  HC26-KOHKBN (CNT-LINE)
                   MOVE    1               TO  FLG-HKNNUM   
               END-IF     
      *
               IF      FLG-HKNNUM           =   ZERO
                   MOVE    "tbl_hknnum"        TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 900-HKNNUM-READ-SEC
               END-IF 
           END-PERFORM
      * 
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       3202-HKNNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC                SECTION.
      *
      *　　印刷用データ出力
           MOVE    SPACE               TO  PRT-REC 
           INITIALIZE                      PRT-REC 
           MOVE    CONST-FORMID        TO  PRT-PRTID
           MOVE    SYS-1031-PRTNM(5)   TO  PRT-PRTNM
           MOVE    SYS-1044-OFFSET-X   TO  PRT-OFFSET-X
           MOVE    SYS-1044-OFFSET-Y   TO  PRT-OFFSET-Y
           MOVE    "1"                 TO  PRT-SITEKBN
           IF    ( SPA-CLIENT-PRT-FLG  =   2 )
               MOVE    "5"             TO  PRT-PRTKBN
           ELSE
               MOVE    "1"             TO  PRT-PRTKBN
           END-IF
           MOVE    MCP-TEMPDIR         TO  PRT-OUT
           MOVE    "HC26"              TO  PRT-TBL-KEY
           MOVE    1                   TO  PRT-RENNUM
           MOVE    SMCNDATE-YMD        TO  PRT-TBL-GROUP (1:8)
           MOVE    SMCNDATE-HMS        TO  PRT-TBL-GROUP (9:6)
           MOVE    1                   TO  PRT-SHORI-RENNUM
           MOVE    CNT-PAGE            TO  PRT-PAGE  
           MOVE    WRK-UUID            TO  PRT-UUID
           MOVE    HC26                TO  PRT-PRTDATA
           WRITE   PRT-REC
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
      *
      *****************************************************************
      *    オフセット取得処理
      *****************************************************************
       800-OFFSET-GET-SEC              SECTION.
      *
           INITIALIZE                      SYS-1044-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1044-HOSPNUM
           MOVE    "1044"              TO  SYS-1044-KANRICD
           MOVE    CONST-FORMID        TO  SYS-1044-FORMID
           STRING  SYS-1031-PRTNM(5)   DELIMITED  BY  SPACE
                   "%"                 DELIMITED  BY  SIZE
           INTO    SYS-1044-TBL(21:)
           END-STRING
           MOVE    "99999999"          TO  SYS-1044-STYUKYMD
           MOVE    "99999999"          TO  SYS-1044-EDYUKYMD
           MOVE    SYS-1044-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE  MCPDATA-REC       TO  SYS-1044-REC
               ELSE
                   INITIALIZE              SYS-1044-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1044-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF   (  SYS-1044-FORMID  =   CONST-FORMID  )
            AND (  SYS-1044-PRTNM   =   SYS-1031-PRTNM(5))
               CONTINUE
           ELSE
               INITIALIZE  SYS-1044-REC
           END-IF
      *
           .
       800-OFFSET-GET-EXT.
            EXIT.
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           MOVE    SPACE               TO  RECE10-REC
           INITIALIZE                  RECE10-REC
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE10-HOSPNUM
           MOVE    WRK-PARA-SKYYM      TO  RECE10-SKYYM
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-END
               MOVE    MCPDATA-REC         TO  RECE10-REC
      *
      *        テスト患者のときは読み飛ばす
               MOVE    SPACE               TO  PTINF-REC
               INITIALIZE                      PTINF-REC
grpsys         MOVE    RECE10-HOSPNUM      TO  PTINF-HOSPNUM
               MOVE    RECE10-PTID         TO  PTINF-PTID
               MOVE    PTINF-REC           TO  MCPDATA-REC
               PERFORM 800-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
               AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *         
               DISPLAY "PTNUM=" RECE10-PTNUM 
      *         
               ADD     1                   TO  CNT-RECE10
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF          MCP-RC      =   ZERO
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
           PERFORM 900-CLOSE-SEC
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
