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
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBM006.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 請求帳票
      *  コンポーネント名  : 主科未設定リスト
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/02/20    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.02.01    NACL-藤原    06/07/31  国保主科対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      * 
      *  04.02.01    NACL-藤原    08/03/04  平成２０年４月改正対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
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
grpsys*
grpsys COPY    "COMMON-SPA".
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *
           COPY    "HCM05V02.INC"  REPLACING  //HCM05V02//
                                   BY         //HC05//.
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-SYORI           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-IN              PIC 9(05).
           03  CNT-OUT             PIC 9(05).
           03  CNT-RENNUM          PIC 9(05). 
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR         PIC X(200).        
      *
           03  WRK-SYSYMDWH        PIC X(22).
      *
           03  WRK-SRYYM           PIC X(16).     
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-PAGE            PIC ZZ9.
           03  WRK-RENNUM          PIC ZZZZ9.
           03  WRK-TEN             PIC ZZZ,ZZZ,ZZ9.
           03  WRK-JNISSU          PIC ZZZZZZZ9.
      *
      *    キー領域
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-NYUGAIKBN PIC 9(01).
           03  KEY-OLD.
               05  KEY-O-NYUGAIKBN PIC 9(01).
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
           COPY    "CPRCF010.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK2009.INC".
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
      ******************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".          
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
                                       CNT-AREA
                                       SPA-AREA
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
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SPACE           TO  RECEERR
           MOVE    SGETTEMP-FULLNAME
                                   TO  RECEERR
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBM006"      TO  JOB-PGID
           MOVE    "主科未設定リスト"
                                   TO  JOB-SHELLMSG
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    システム日付の編集
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-SYSYMDWH   
      *
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-SRYYM
      *         
      *    レセプト編集区分情報
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    "2009"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
grpsys     MOVE    WRK-PARA-HOSPNUM
                                   TO  SYS-HOSPNUM
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-2009-REC
           ELSE    
               INITIALIZE                  SYS-2009-REC
           END-IF
           IF      SYS-2009-SYUKA-KBN
                                   =   SPACE
               MOVE    "0"             TO  SYS-2009-SYUKA-KBN
           END-IF
           IF    ( SYS-2009-SYUKA-KBN
                                   =   "1"   )
           AND   ( SYS-2009-SYUKA-SYAHO
                                   =   SPACE )
               MOVE    "1"             TO  SYS-2009-SYUKA-SYAHO
           END-IF      
           IF    ( SYS-2009-SYUKA-KBN
                                   =   "1"   )
           AND   ( SYS-2009-SYUKA-KOKUHO
                                   =   SPACE )
               MOVE    "0"             TO  SYS-2009-SYUKA-KOKUHO
           END-IF
           IF    ( SYS-2009-SYUKA-KBN
                                   =   "1"   )
           AND   ( SYS-2009-SYUKA-KOUIKI
                                   =   SPACE )
               MOVE    "0"             TO  SYS-2009-SYUKA-KOUIKI
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    社保分
           IF      SYS-2009-SYUKA-SYAHO  =   "1"
               MOVE    ZERO            TO  FLG-END
               MOVE    1               TO  FLG-SYORI
      *
               PERFORM 900-SEIKYU-SELECT-SEC
               PERFORM 2001-HENSYU-SEC
                       UNTIL       FLG-END     =   1
      *
               PERFORM 900-SEIKYU-CLOSE-SEC
           END-IF
      *
      *    国保分
           IF      SYS-2009-SYUKA-KOKUHO =   "1"
               MOVE    ZERO            TO  FLG-END
               MOVE    2               TO  FLG-SYORI
      *
               PERFORM 900-SEIKYU-SELECT-SEC
               PERFORM 2001-HENSYU-SEC
                       UNTIL       FLG-END     =   1
      *
               PERFORM 900-SEIKYU-CLOSE-SEC
           END-IF    
      *
      *    広域連合分
           IF      SYS-2009-SYUKA-KOUIKI =   "1"
               MOVE    ZERO            TO  FLG-END
               MOVE    3               TO  FLG-SYORI
      *
               PERFORM 900-SEIKYU-SELECT-SEC
               PERFORM 2001-HENSYU-SEC
                       UNTIL       FLG-END     =   1
      *
               PERFORM 900-SEIKYU-CLOSE-SEC
           END-IF    
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票作成処理
      *****************************************************************
       2001-HENSYU-SEC                SECTION.
      *
           MOVE    KEY-NEW         TO  KEY-OLD
           MOVE    ZERO            TO  CNT-RENNUM
      *     
      *    帳票編集<見出し>処理
           PERFORM 310-HEAD-HEN-SEC
      *     
      *    帳票編集<明細>処理
           PERFORM             UNTIL   FLG-END     =   1
                               OR      KEY-NEW NOT =   KEY-OLD
               PERFORM 320-BODY-HEN-SEC
           END-PERFORM    
      *
      *    帳票編集<合計>処理
              PERFORM 330-SUM-HEN-SEC
      *
      *    帳票印刷処理
           PERFORM 390-PRINT-OUT-SEC
           .
       2001-HENSYU-EXT.
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
           MOVE    SPACE               TO  HC05
      * 
           MOVE    "＊＊＊　主科未設定リスト　＊＊＊"
                                       TO  HC05-TITLE
      *
           MOVE    WRK-SRYYM           TO  HC05-HSRYYM
           MOVE    WRK-SYSYMDWH        TO  HC05-SYSYMD
           MOVE    CNT-PAGE            TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC05-PAGE
      *
           EVALUATE    KEY-N-NYUGAIKBN     ALSO    FLG-SYORI
               WHEN    1                   ALSO    1
                   MOVE    "社保　入院"        TO  HC05-TEISYUTUNAME
               WHEN    2                   ALSO    1
                   MOVE    "社保　入院外"      TO  HC05-TEISYUTUNAME
               WHEN    1                   ALSO    2
                   MOVE    "国保　入院"        TO  HC05-TEISYUTUNAME
               WHEN    2                   ALSO    2
                   MOVE    "国保　入院外"      TO  HC05-TEISYUTUNAME
               WHEN    1                   ALSO    3
                   MOVE    "広域　入院"        TO  HC05-TEISYUTUNAME
               WHEN    2                   ALSO    3
                   MOVE    "広域　入院外"      TO  HC05-TEISYUTUNAME
           END-EVALUATE
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
           IF      CNT-LINE        >=  22
      *        帳票印刷処理
               PERFORM 390-PRINT-OUT-SEC
      *        帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
           END-IF                    
      *
           ADD     1                   TO  CNT-LINE
           ADD     1                   TO  CNT-RENNUM
      *
           MOVE    CNT-RENNUM          TO  WRK-RENNUM
           MOVE    WRK-RENNUM          TO  HC05-RENNUM (CNT-LINE)
      *
           MOVE    RECE10-PTNUM        TO  HC05-PTNUM  (CNT-LINE)
      *
           MOVE    PTINF-NAME          TO  HC05-NAME   (CNT-LINE)
      *
           IF      RECE10-HKNJANUM     =   "00000000"
               CONTINUE
           ELSE
               MOVE    RECE10-HKNJANUM     TO  HC05-HKNJANUM(CNT-LINE)
           END-IF
      *
           MOVE    RECE10-SRYYM        TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  HC05-SRYYM(CNT-LINE)
      *
           MOVE    RECE10-HENREIYM     TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HC05-HENYM(CNT-LINE)
      *
           MOVE    RECE10-SKYYM        TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  HC05-SEIYM(CNT-LINE)
      *
           MOVE    RECE10-TOTALTEN(1)  TO  WRK-TEN
           MOVE    WRK-TEN             TO  HC05-HKNTEN(CNT-LINE)   
           MOVE    RECE10-TOTALTEN(2)  TO  WRK-TEN
           MOVE    WRK-TEN             TO  HC05-MONEY1(CNT-LINE)
           MOVE    RECE10-TOTALTEN(3)  TO  WRK-TEN
           MOVE    WRK-TEN             TO  HC05-MONEY2(CNT-LINE)
           MOVE    RECE10-JNISSU  (1)  TO  WRK-JNISSU
           MOVE    WRK-JNISSU          TO  HC05-JNISSU(CNT-LINE)
           MOVE    RECE10-JNISSU  (2)  TO  WRK-JNISSU
           MOVE    WRK-JNISSU          TO  HC05-JNISSU1(CNT-LINE)
           MOVE    RECE10-JNISSU  (3)  TO  WRK-JNISSU
           MOVE    WRK-JNISSU          TO  HC05-JNISSU2(CNT-LINE)
      *
           EVALUATE    RECE10-SKYKBN
               WHEN    "0"
                   MOVE    "請求しない"    TO  HC05-SKYKBN (CNT-LINE) 
               WHEN    "1"
                   MOVE    "未請求"        TO  HC05-SKYKBN (CNT-LINE) 
               WHEN    "2"
                   MOVE    "請求済み"      TO  HC05-SKYKBN (CNT-LINE) 
           END-EVALUATE            
      *
           IF      RECE10-SKYKBN       =   "1"
               MOVE    "未"                TO  HC05-MSG (CNT-LINE)
           END-IF
           IF      RECE10-SRYYM    NOT =   LNK-PRTKANRI-SRYYM
               MOVE    "他"                TO  HC05-MSG (CNT-LINE) (3:)
           END-IF
      *
           IF    ( RECE10-SKYKBN   NOT =   ZERO                        )
               IF    ( RECE10-SRYYM    =   LNK-PRTKANRI-SRYYM          )
               AND   ( RECE10-SKYYM    =   LNK-PRTKANRI-SRYYM  OR  ZERO)
                   MOVE    "※"                TO  HC05-TAISYO(CNT-LINE)
               ELSE
                   IF    ( RECE10-SKYKBN   NOT =   ZERO               )
                   AND   ( RECE10-SKYYM        =   LNK-PRTKANRI-SRYYM )
                       MOVE    "※"                TO 
                                                   HC05-TAISYO(CNT-LINE)
                   END-IF
               END-IF
           END-IF        
      *
           ADD     1                   TO  CNT-OUT
      *
           PERFORM 900-SEIKYU-FETCH-SEC         
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<合計部>処理
      *****************************************************************
       330-SUM-HEN-SEC          SECTION.
      *
           MOVE    CNT-RENNUM        TO  WRK-RENNUM
           MOVE    WRK-RENNUM        TO  HC05-GKENSU
      *
           .
       330-SUM-HEN-EXT.
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
           MOVE    "HCM05V02.red"      TO  SPRT-PRTID
           MOVE    "主科未設定リスト"  TO  SPRT-TITLE
           MOVE    HC05                TO  SPRT-PRTDATA
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
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
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
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
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
           MOVE    CNT-PAGE        TO  JOB-UPDCNT                        
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORCBM006 IN  "  CNT-IN
           DISPLAY "***          OUT "  CNT-OUT
           DISPLAY "*** ORCBM006 END ***"
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           IF      WRK-SYMD            =   ZERO   OR  SPACE
                MOVE    SPACE              TO  WRK-HENYMDG
           ELSE    
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
               INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           END-IF    
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
      *    請求管理マスタ読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC                 SECTION.
      *
           MOVE    SPACE               TO  RECE10-REC  
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE10-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE10-SKYYM
           MOVE    "00"                TO  RECE10-RECEKA
           EVALUATE    FLG-SYORI
               WHEN    "1"
                   MOVE    1               TO  RECE10-TEISYUTUSAKI
               WHEN    "2"
                   MOVE    2               TO  RECE10-TEISYUTUSAKI
               WHEN    "3"
                   MOVE    6               TO  RECE10-TEISYUTUSAKI
           END-EVALUATE
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
           MOVE    "key68"             TO  WRK-MCP-PATHNAME
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-SEIKYU-FETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-END
               MOVE    HIGH-VALUE          TO  KEY-NEW 
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスタ 検索処理
      *****************************************************************
       900-SEIKYU-FETCH-SEC                SECTION.
      *           
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
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
               IF    ( FLG-PTINF           =   ZERO )
               AND   ( PTINF-TSTPTNUMKBN   =   "1"  )
                   GO  TO  900-SEIKYU-FETCH-SEC
               END-IF
      *
               MOVE    RECE10-NYUGAIKBN    TO  KEY-N-NYUGAIKBN
               ADD     1                   TO  CNT-IN
      *
               DISPLAY "PTNUM=" RECE10-PTNUM 
                       " TEISYUTUSAKI=" RECE10-TEISYUTUSAKI
           ELSE
               MOVE    1                   TO  FLG-END 
               MOVE    HIGH-VALUE          TO  KEY-NEW
           END-IF  
           .
       900-SEIKYU-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスタ クローズ処理
      *****************************************************************
       900-SEIKYU-CLOSE-SEC                 SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
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
           PERFORM 900-CLOSE-SEC
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
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
