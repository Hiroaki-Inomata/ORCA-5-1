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
       PROGRAM-ID.             ORCBM021.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 決定通知書（国保）（ＨＣ２７）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-藤原      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     02/04/08  短縮名１から正式名称に変更 
      *  01.00.02    MCC-藤原     02/04/10  医療機関コードの編集修正 
      *  01.00.03    MCC-藤原     02/05/22  テスト患者は処理しない     
      *
      *  01.01.01    NACL-藤原    02.09.17  印刷サブの追加
      *  01.01.02    NACL-藤原    03.04.11  入院・入院外を分けない
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
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
                                   ORGANIZATION    IS  LINE SEQUENTIAL
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
           03  FLG-RECE10          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
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
                                   PIC  X(64).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *
           03  WRK-SRYYMWH         PIC X(16).
           03  WRK-SRYYMD          PIC X(08).
      *
           03  WRK-RECEERR         PIC X(200). 
      *     
           03  WRK-HC01PARA-CNT    PIC 9(06).
      *
           03  WRK-Z9              PIC ZZZZ9.
      *
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
           03  WRK-ENCODING        PIC X(01).
           03  WRK-ENCODING9       REDEFINES WRK-ENCODING
                                   PIC 9(01).
           03  WRK-UUID            PIC X(36).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE              PIC 9(02).
           03  CNT-LINE1             PIC 9(02).
           03  CNT-PAGE              PIC 9(03).
           03  CNT-RECE10            PIC 9(06).
      *
      *    集計領域
       01  SUM-AREA.
           03  SUM-HKN-AREA.
               05  SUM-HKN-TBL         OCCURS  3.
                   07  SUM-HKN-TBL-G   OCCURS  2.
                       09  SUM-KENSU   PIC 9(05).
           03  SUM-GOK-AREA.
               05  SUM-GOK-TBL         OCCURS  3.
                   07  SUM-GOK-TBL-G   OCCURS  2.
                       09  SUM-GOK-KENSU
                                       PIC 9(05).
           03  SUM-KOH-AREA.
               05  SUM-KOH-TBL         OCCURS  50.
                   07  SUM-KOH-HKNNUM  PIC X(03).
                   07  SUM-KOH-TBL-G   OCCURS  2.
                       09  SUM-KOH-KENSU
                                       PIC 9(05).
      *
       01  WRK-KOH-LINE-MAX                 PIC 9(03)   VALUE   6.
       01  WRK-HKN-LINE-MAX                 PIC 9(03)   VALUE   84.
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-HKNJANUM      PIC X(08).
           03  KEY-OLD.
               05  KEY-O-HKNJANUM      PIC X(08). 
      *
       01  CONST-AREA.
           03  CONST-FORMID        PIC X(20)   VALUE  "HC27.red".
      *
           COPY    "HC27.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
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
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
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
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
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
      *
           PERFORM 800-OFFSET-GET-SEC
      *
           MOVE    "HC27"              TO  HC01PARA-FORM-ID
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
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    OUTPUT              PRT-FILE
      *
      *    県内分
      *
           MOVE    ZERO            TO  SUM-GOK-AREA     
      *     
      *    県内保険者分
           MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
           MOVE    "key6"              TO  WRK-MCP-PATHNAME
           PERFORM 900-SEIKYU-SELECT-SEC 
      *
           PERFORM             UNTIL   FLG-RECE10  =   1     
               INITIALIZE                  SUM-HKN-AREA
               MOVE    KEY-NEW             TO  KEY-OLD      
               PERFORM             UNTIL   FLG-RECE10  =   1
                                   OR      KEY-NEW NOT =   KEY-OLD
                   PERFORM 2001-SYUKEI-SEC
               END-PERFORM
      *         
               PERFORM 2002-PRINT-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *     
      *    県内扱い保険者分
           MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
           MOVE    "key20"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SEIKYU-SELECT-SEC 
      *
           PERFORM             UNTIL   FLG-RECE10  =   1     
               INITIALIZE                  SUM-HKN-AREA
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM             UNTIL   FLG-RECE10  =   1
                                   OR      KEY-NEW NOT =   KEY-OLD
                   PERFORM 2001-SYUKEI-SEC
               END-PERFORM
      *         
               PERFORM 2002-PRINT-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      CNT-RECE10          >   ZERO
               PERFORM 2003-GOKEI-PRINT-SEC
           END-IF
      *
      *    県外分
      *
           MOVE    ZERO            TO  SUM-GOK-AREA     
      *     
           MOVE    "tbl_seikyu"    TO  WRK-MCP-TABLE
           MOVE    "key7"          TO  WRK-MCP-PATHNAME
           PERFORM 900-SEIKYU-SELECT-SEC
           IF      FLG-RECE10          =   ZERO
               IF      CNT-LINE            >   ZERO
                   ADD     1                   TO  CNT-LINE
               END-IF
           END-IF         
      *
           PERFORM             UNTIL   FLG-RECE10  =   1     
               INITIALIZE                  SUM-HKN-AREA
               MOVE    KEY-NEW             TO  KEY-OLD      
               PERFORM             UNTIL   FLG-RECE10  =   1
                                   OR      KEY-NEW NOT =   KEY-OLD
                   PERFORM 2001-SYUKEI-SEC
               END-PERFORM
      *
               PERFORM 2002-PRINT-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      CNT-RECE10          >   ZERO
               PERFORM 2003-GOKEI-PRINT-SEC
           END-IF                                     
      *
      *    県単医療分処理
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >   50
                   OR      SUM-KOH-HKNNUM (IDX)    =   SPACE
               PERFORM 2004-BODY-KOH-HEN-SEC 
           END-PERFORM
      *            
      *    帳票印刷処理
           IF      CNT-RECE10          >   ZERO
               PERFORM 2005-PRINT-OUT-SEC
           END-IF    
      *
           CLOSE  PRT-FILE
      *
           IF    ( WRK-HC01PARA-CNT    >   ZERO )
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
           END-IF
      *
           MOVE    1                   TO  FLG-END                                 
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号別集計処理
      *****************************************************************
       2001-SYUKEI-SEC                SECTION.
      * 
           MOVE    ZERO                    TO  IDX-AREA
                                               FLG-UPD 
      *
      *****EVALUATE    RECE10-NYUGAIKBN
      *         WHEN    "1"
      *             MOVE    1                 TO  IDX3
      *         WHEN    "2"
      *             MOVE    2                 TO  IDX3
      *****END-EVALUATE           
           MOVE    1                  TO  IDX3
      *
      *        老人保健
           IF      RECE10-RJNHKNNUM    NOT =   SPACE
               MOVE    3                       TO  IDX
           ELSE
      *        国民健康保険
               EVALUATE    RECE10-HKNNUM-S
                       WHEN    "060"
                           MOVE    1               TO  IDX
                       WHEN    "067"
                           MOVE    2               TO  IDX
               END-EVALUATE
           END-IF
           PERFORM 210-HKN-SYUKEI-SEC
      *
      *    公費負担分     
           IF      RECE10-KOHNUM (1)   NOT =   SPACE
               PERFORM 220-KOH-SYUKEI-SEC
           END-IF
      *
      *    集計の対象にならなかったとき                
           IF      FLG-UPD             =   1
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
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
               MOVE    1                   TO  FLG-RECE10
           END-IF 
           .
       2001-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・老人保健分集計処理
      *****************************************************************
       210-HKN-SYUKEI-SEC          SECTION.
      *
           IF      IDX             >   ZERO
      *        件数
               ADD     1               TO  SUM-KENSU     (IDX IDX3)
                                           SUM-GOK-KENSU (IDX IDX3)
      *
               MOVE    1               TO  FLG-UPD 
           END-IF
           .
 
       210-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担分集計処理
      *****************************************************************
       220-KOH-SYUKEI-SEC        SECTION.
      *
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       4
                   OR  RECE10-KOHNUM (IDZ) =   SPACE
               IF      RECE10-KOHNUM (IDZ) (1:1)   >=  "1" AND <=  "8"
                   PERFORM VARYING IDW FROM    1   BY  1
                           UNTIL   IDW >       WRK-KOH-LINE-MAX
                       IF      SUM-KOH-HKNNUM(IDW) =   SPACE
                           MOVE    RECE10-KOHNUM (IDZ)
                                               TO  SUM-KOH-HKNNUM (IDW)
                       END-IF                             
                       IF      RECE10-KOHNUM (IDZ)
                                               =   SUM-KOH-HKNNUM (IDW)
                           ADD     1       TO  SUM-KOH-KENSU (IDW IDX3)
                           MOVE    WRK-KOH-LINE-MAX     TO  IDW
                       END-IF
                   END-PERFORM
               END-IF            
           END-PERFORM        
      *
           .
       220-KOH-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           IF      CNT-LINE          =   ZERO      
      *        帳票編集<見出し>処理
               PERFORM 20021-HEAD-HEN-SEC
           END-IF    
      *     
      *    帳票編集<明細>処理
           PERFORM 20022-BODY-HEN-SEC
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       20021-HEAD-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
                                           CNT-LINE1
           ADD     1                   TO  CNT-PAGE
      *
           MOVE    SPACE               TO  HC27
      *
           MOVE    WRK-SRYYMWH         TO  HC27-PRTYM
           MOVE    SYS-1001-HOSPCDN    TO  HC27-HOSPCD
           MOVE    SYS-1001-HOSPNAME   TO  HC27-HOSPNAME
           .
       20021-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       20022-BODY-HEN-SEC          SECTION.
      *     
           IF      CNT-LINE            >=  WRK-HKN-LINE-MAX
      *        帳票印刷処理
               PERFORM 2005-PRINT-OUT-SEC
      *        帳票編集<見出し>処理
               PERFORM 20021-HEAD-HEN-SEC
           END-IF    
      *
           ADD     1                   TO  CNT-LINE
      *     
           INITIALIZE                      HKNJAINF-REC
grpsys     MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
           MOVE   KEY-O-HKNJANUM       TO  HKNJA-HKNJANUM
           MOVE   HKNJAINF-REC         TO  MCPDATA-REC
           PERFORM 900-HKNJAINF-READ-SEC
           IF      FLG-HKNJAINF        =   ZERO
               MOVE    HKNJA-HKNJANAME     TO  HC27-NAME (CNT-LINE)
           ELSE
               MOVE    KEY-O-HKNJANUM(1:6) TO  HC27-NAME (CNT-LINE)                            
           END-IF                            
      *  
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       3
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       1
                   MOVE    SUM-KENSU   (IDX IDY)
                                       TO  WRK-Z9
                   MOVE    WRK-Z9      TO  HC27-KENSU (IDX CNT-LINE)
               END-PERFORM    
           END-PERFORM
           .
       20022-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<合計行>処理
      *****************************************************************
       2003-GOKEI-PRINT-SEC          SECTION.
      *     
           IF      CNT-LINE            >=  WRK-HKN-LINE-MAX
      *        帳票印刷処理
               PERFORM 2005-PRINT-OUT-SEC
      *        帳票編集<見出し>処理
               PERFORM 20021-HEAD-HEN-SEC
           END-IF    
      *
           ADD     1                   TO  CNT-LINE
      *
           IF      WRK-MCP-PATHNAME    =   "key20"
               MOVE    "県合計"            TO  HC27-NAME (CNT-LINE)
           ELSE    
               MOVE    "県外計"            TO  HC27-NAME (CNT-LINE)
           END-IF    
      *  
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       3
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       1
                   MOVE    SUM-GOK-KENSU   (IDX IDY)
                                       TO  WRK-Z9
                   MOVE    WRK-Z9      TO  HC27-KENSU (IDX CNT-LINE)
               END-PERFORM    
           END-PERFORM
           .
       2003-GOKEI-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<県単医療分>処理
      *****************************************************************
       2004-BODY-KOH-HEN-SEC      SECTION.
      *     
           IF      CNT-LINE1           >=  WRK-KOH-LINE-MAX
      *        帳票印刷処理
               PERFORM 2005-PRINT-OUT-SEC
      *        帳票編集<見出し>処理
               PERFORM 20021-HEAD-HEN-SEC
           END-IF    
      *
           ADD     1                   TO  CNT-LINE1
      *
      *    保険名称  
           INITIALIZE                      HKNNUM-REC
grpsys     MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE   SUM-KOH-HKNNUM  (IDX)
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
           IF      FLG-HKNNUM      =   ZERO
               MOVE    HKN-TANSEIDONAME
                                       TO  HC27-KOHNAME (CNT-LINE1)
115900     END-IF
      * 
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    SUM-KOH-KENSU       (IDX 1)
                               TO  WRK-Z9
           MOVE    WRK-Z9      TO  HC27-KOHKENSU    (CNT-LINE1)
           .
       2004-BODY-KOH-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2005-PRINT-OUT-SEC                SECTION.
      *
      *　　印刷用データ出力
           ADD     1                   TO  WRK-HC01PARA-CNT 
      *
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
           MOVE    "HC27"              TO  PRT-TBL-KEY
           MOVE    1                   TO  PRT-RENNUM
           MOVE    SMCNDATE-YMD        TO  PRT-TBL-GROUP (1:8)
           MOVE    SMCNDATE-HMS        TO  PRT-TBL-GROUP (9:6)
           MOVE    1                   TO  PRT-SHORI-RENNUM
           MOVE    WRK-HC01PARA-CNT    TO  PRT-PAGE  
           MOVE    WRK-UUID            TO  PRT-UUID
           MOVE    HC27                TO  PRT-PRTDATA
           WRITE   PRT-REC
      *
           .
       2005-PRINT-OUT-EXT.
           EXIT.
057400*
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
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORCBM021 IN  "  CNT-RECE10
           DISPLAY "*** ORCBM021 END ***"
      ***     ACCEPT  WRK-IN
           .
       300-END-EXT.
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
           MOVE    SYS-1001-PREFNUM    TO  RECE10-XXPREFNUM
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE10
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
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RECE10
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
               MOVE    RECE10-HKNJANUM-S   TO  KEY-N-HKNJANUM
               ADD     1                   TO  CNT-RECE10
      *         
               DISPLAY "PTNUM=" RECE10-PTNUM 
      *         
           ELSE
               MOVE    HIGH-VALUE          TO  KEY-NEW 
               MOVE    1                   TO  FLG-RECE10
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
      *    保険者番号マスター読込
      *****************************************************************
       900-HKNJAINF-READ-SEC         SECTION.
      * 
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-HKNJAINF-READ-EXT.
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
