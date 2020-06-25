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
       PROGRAM-ID.             ORCR0890.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト入院（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災自賠責明細書ファイル作成
      *                      傷病名編集（労災・自賠責）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/06/01    NACL−藤原　  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  中止処理追加
      *  01.00.01    NACL-藤原    04/10/13  中止処理の追加
      *  01.00.02    NACL-藤原    04/11/24  レセプト記載順の修正
      *  01.00.03    NACL-門脇    05.09.06  自賠責従来様式対応
      *
      *  02.07.01    NACL-藤原    06/01/25  疑いフラグからの病名の編集方
      *                                     法の変更
      *                                     MONFUNC対応
      *
      *  03.04.01    NACL-藤原    07/05/15  半角空白がある場合の変換
      *
      *  03.05.01    NACL-門脇    07/06/01  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/20  realtime preview 処理追加
      *
      *  04.06.01    NACL-藤原    10/11/04  補足コメント対応
      *  04.06.02    NACL-藤原    12/07/04  表示期間によるレセプト記載対応
      *
      *  04.07.01    NACL-藤原    13/01/21  労災レセ電対応
      *  04.07.02    NACL-藤原    13/09/20  第三者行為対応
      *  04.07.03    NACL-藤原    14/03/18  連番号を３桁に変更
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    レセプト明細書  
           SELECT  RECE82-FILE     ASSIGN  RECE82PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE82-KEY
                                   FILE    STATUS  IS  STS-RECE82.
      *
           SELECT  RECE83-FILE     ASSIGN  RECE83PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE83-KEY
                                   FILE    STATUS  IS  STS-RECE83.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    レセプト明細書  
       FD  RECE82-FILE.
       01  RECE82-REC. 
           COPY    "CPRCF082.INC".
      *
       FD  RECE83-FILE.
       01  RECE83-REC. 
           COPY    "CPRCF083.INC". 
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE82//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE83//.
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE82          PIC X(02).
           03  STS-RECE83          PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-RECE83          PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE82          PIC 9(06).
           03  CNT-RECE83          PIC 9(06).
           03  CNT-IN-CANCEL       PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                 PIC 9(03). 
           03  IDY                 PIC 9(03). 
           03  IDZ                 PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM    PIC X(06).
               05  WRK-PARA-TERMID   PIC X(16).
               05  WRK-PARA-SYSYMD   PIC X(08).
               05  WRK-PARA-RECEKBN  PIC X(01).
               05  WRK-PARA-JOBID    PIC 9(07).
               05  WRK-PARA-SHELLID  PIC X(08).
           03  WRK-PARA-JIBAIPRTKBN  PIC X(01).
           03  WRK-PARA-HOSPNUM      PIC 9(02).
      *
           03  WRK-YM              PIC 9(06).
           03  WRK-RENNUM          PIC 9(03).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-SRYYMD    PIC 9(08).
           03  KEY-OLD.
               05  KEY-O-SRYYMD    PIC 9(08).
      *
       01  WRK-RECE83-REC. 
           COPY    "CPRCF083.INC" REPLACING    //RECE83//
                                   BY          //WRK-RECE83//.
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-MCP-RC              PIC 9(9).
           03  WRK-ERR-FILE-STS        PIC X(02).
           03  WRK-ERR-FILE-MSG        PIC X(100).
           03  WRK-ERR-FILE-MSG1       PIC X(17).
           03  WRK-ERR-FLG             PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPRECEDAT1.INC"    REPLACING  //RECEDAT1PARA//
                                       BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME内のBASENAME開始位置
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
      *    ワーク患者病名
       01  WRK-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING  //PTBYO-//
                                   BY         //WRK-PTBYO-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *     
           COPY    "CPSK2005.INC".
           COPY    "CPSK200501.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    病名組立サブ
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    拡張漢字チェックサブ
           COPY    "CPORCSKANACHK2.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
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
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
      *
           PERFORM 300-TERM-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       SPA-AREA
                                       RECEERR
      *
           MOVE    ZERO                TO  FLG-END
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE82PARA
           INITIALIZE                      RECE82PARA
           MOVE    "RECE820"           TO  RECE82PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE82PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE82PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE83PARA
           INITIALIZE                      RECE83PARA
           MOVE    "RECE830"           TO  RECE83PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE83PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE83PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE82PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE83PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE82PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE83PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    "ORCR0890"          TO  JOB-PGID
               MOVE    "傷病名編集（労災・自賠責）" 
                                           TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF 
      *
           OPEN    OUTPUT              RECE83-FILE
           CLOSE                       RECE83-FILE
           OPEN    I-O                 RECE83-FILE
      *
           OPEN    INPUT               RECE82-FILE    
           IF      STS-RECE82          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE82PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE82          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *     
      *    レセプト編集区分情報
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "*"              TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-2005-REC
           ELSE
               MOVE    SPACE            TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-BYOMEIHENKBN1   =   SPACE
               MOVE    ZERO             TO  SYS-2005-BYOMEIHENKBN1
           END-IF
      *         
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "01"             TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-200501-REC
           ELSE    
               INITIALIZE                   SYS-200501-REC
           END-IF
           IF      SYS-200501-TENKIBYOMEI-KBN
                                    =   SPACE
               MOVE    ZERO             TO  SYS-200501-TENKIBYOMEI-KBN
           END-IF
      *
           DISPLAY "SYS-2005-BYOMEIHENKBN1    ="
                                    SYS-2005-BYOMEIHENKBN1
           DISPLAY "SYS-200501-TENKIBYOMEI-KBN="
                                    SYS-200501-TENKIBYOMEI-KBN
      *
      *    レセプト明細読み込み
           PERFORM 900-RECE82-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *    
      *    患者病名読み込み（患者ＩＤ、診療年月日、診療科順）
           INITIALIZE                  PTBYOMEI-REC
           MOVE    RECE82-HOSPNUM      TO  PTBYO-HOSPNUM
           MOVE    RECE82-PTID         TO  PTBYO-PTID
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM 2001-PTBYO-HENSYU-SEC
                           UNTIL       FLG-PTBYOMEI     =   1
      *    カーソルクロース
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           PERFORM 900-RECE82-READ-SEC      
      *
      *    処理中止設定処理
           IF      CNT-IN-CANCEL   =   50
               MOVE    ZERO            TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT. 
      * 
      *****************************************************************
      *    患者病名診療年月日別処理
      *****************************************************************
       2001-PTBYO-HENSYU-SEC                SECTION.
      *
           MOVE    PTBYO-SRYYMD            TO  KEY-N-SRYYMD
           MOVE    ZERO                    TO  WRK-RENNUM
      *
           MOVE    KEY-NEW                 TO  KEY-OLD   
           PERFORM             UNTIL       FLG-PTBYOMEI    =   1
                               OR          KEY-NEW     NOT =   KEY-OLD
               PERFORM 2001-PTBYO-CHK-SEC
      * 
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM    
           .
       2001-PTBYO-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    患者病名チェック処理
      *****************************************************************
       2001-PTBYO-CHK-SEC         SECTION.
      *
      *    拡張漢字チェック
           INITIALIZE                      ORCSKANACHK2AREA
           MOVE    "1"                 TO  KANACHK2-SYORI
           MOVE    PTBYO-BYOMEI        TO  KANACHK2-MAE-INPUT
           CALL    "ORCSKANACHK2"      USING   ORCSKANACHK2AREA
           IF      KANACHK2-RC4        =   ZERO
               CONTINUE
           ELSE
               CALL    "coblog" USING   "ptbyomei kanachk2 err"
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "病名名称文字コード不正　患者番号="
                                               DELIMITED  BY  SIZE
                       RECE82-PTNUM            DELIMITED  BY  SPACE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
      *    診療科
           MOVE    ZERO                TO  FLG-CHK 
           IF      SYS-2005-BYOMEIHENKBN1      =   ZERO     
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       20
                       OR      RECE82-SRYKA(IDX)   =   SPACE
                   IF      PTBYO-SRYKA          =   RECE82-SRYKA (IDX)
                       MOVE    1                    TO  FLG-CHK
                       MOVE    20                   TO  IDX
                   END-IF
               END-PERFORM
               IF      FLG-CHK              =   ZERO
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
           END-IF        
      *
      *    入外区分
           IF      PTBYO-NYUGAIKBN      =   SPACE
               CONTINUE
           ELSE
               IF      PTBYO-NYUGAIKBN  NOT =   RECE82-NYUGAIKBN
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
      *
           IF      WRK-PARA-RECEKBN     =   "8"
      *        公費ＩＤ（第三者行為）
               IF      PTBYO-KOHID          =   RECE82-HKNID 
                   CONTINUE
               ELSE
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
           ELSE
      *        保険組合せ
               IF      PTBYO-HKNCOMBI       =   RECE82-HKNCOMBI 
                   CONTINUE
               ELSE
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
      *    レセプト表示月
           IF      PTBYO-REZEFLG          =   "1"
              GO  TO  2001-PTBYO-CHK-EXT
           END-IF
      *
      *    診療開始日
           IF      PTBYO-SRYYMD(1:6)    =   RECE82-SRYYM
               CONTINUE
           ELSE
               IF      PTBYO-SRYYMD(1:6)    >   RECE82-SRYYM 
                   GO  TO  2001-PTBYO-CHK-EXT
               ELSE
      *            転帰日
                   IF      PTBYO-TENKIKBN       =   SPACE
                       CONTINUE
                   ELSE
                       IF    ( SYS-200501-TENKIBYOMEI-KBN
                                                   =   "0"  )
                       OR    ( PTBYO-REZEMM        =   ZERO )
                           IF      PTBYO-TENKIYMD(1:6) >=  RECE82-SRYYM
                               CONTINUE
                           ELSE
                               GO  TO  2001-PTBYO-CHK-EXT
                           END-IF
                       END-IF       
                   END-IF       
               END-IF
      *
      *        レセプト表示期間
               MOVE    ZERO                   TO  WRK-YM
               IF      PTBYO-REZEMM           >   ZERO
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    PTBYO-SRYYMD        TO  LNK-DAY6-KIJUN 
                   MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
                   COMPUTE LNK-DAY6-ZOGEN  =   PTBYO-REZEMM    -   1    
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN(1:6)
                                               TO  WRK-YM
                   IF    ( SYS-200501-TENKIBYOMEI-KBN
                                               =   "0"   )
                   OR    ( PTBYO-TENKIKBN      =   SPACE )
                       IF      WRK-YM              >=  RECE82-SRYYM
                           CONTINUE
                       ELSE
                           GO  TO  2001-PTBYO-CHK-EXT
                       END-IF
                   END-IF
               END-IF                    
      *
      *        レセプト表示期間と転帰日の優先設定による判定
               IF    ( PTBYO-TENKIKBN      =   SPACE )
               OR    ( PTBYO-REZEMM        =   ZERO  )
                   CONTINUE
               ELSE
                   IF      SYS-200501-TENKIBYOMEI-KBN
                                               =   "1"
                       IF      WRK-YM              >=  RECE82-SRYYM
                           CONTINUE
                       ELSE
                           GO  TO  2001-PTBYO-CHK-EXT
                       END-IF
                   END-IF    
               END-IF
           END-IF
      *                               
           PERFORM 20011-RECE83-WRITE-SEC
           .
       2001-PTBYO-CHK-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    レセプト明細０３編集処理
      *****************************************************************
       20011-RECE83-WRITE-SEC              SECTION.
      *
           IF      PTBYO-BYOMEIHENFLG  =   SPACE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       21
                   IF      PTBYO-BYOMEICD (IDX) (1:4)  =   "ZZZC"
                                                       OR  "ZZZ("
                       MOVE    "1"         TO  PTBYO-BYOMEIHENFLG
                       MOVE    21          TO  IDX
                   END-IF
               END-PERFORM
           END-IF   
      *
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI 
           MOVE    PTBYO-HOSOKU-COMMENT
                                       TO  KANACHK-MAE-INPUT
           MOVE    40                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  PTBYO-HOSOKU-COMMENT
      *
           IF      PTBYO-BYOMEIHENFLG  =   "1"
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    PTBYO-BYOMEI        TO  KANACHK-MAE-INPUT
               MOVE    80                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT   TO  PTBYO-BYOMEI
               COMPUTE PTBYO-BYOMEIMOJI    =   KANACHK-MAX  /   2
           END-IF
      *
           MOVE    PTBYO-BYOMEI        TO  WRK-PTBYO-BYOMEI
           MOVE    PTBYO-BYOMEIMOJI    TO  WRK-PTBYO-BYOMEIMOJI
      *
      *    疑いフラグ、補足コメントより病名を編集
      *   （病名のみ編集を行う）
           IF    ( PTBYO-UTAGAIFLG NOT =   SPACE )
           OR    ( PTBYO-HOSOKU-COMMENT
                                   NOT =   SPACE )
               INITIALIZE                      ORCSMKBYOMEIAREA
               MOVE    PTBYO-UTAGAIFLG     TO  SMKBYO-UTAGAIFLG
               MOVE    PTBYO-BYOMEI        TO  SMKBYO-BYOMEI
               MOVE    PTBYO-HOSOKU-COMMENT
                                           TO  SMKBYO-HOSOKU-COMMENT
               MOVE    PTBYO-BYOMEIHENFLG  TO  SMKBYO-BYOMEIHENFLG
               MOVE    PTBYO-BYOMEICD-G    TO  SMKBYO-BYOMEICD-G
               CALL    "ORCSMKBYOMEI"      USING   ORCSMKBYOMEIAREA
      *
               COMPUTE PTBYO-BYOMEIMOJI    =   SMKBYO-LENGTH   /   2
               MOVE    SMKBYO-BYOMEI       TO  PTBYO-BYOMEI
               EVALUATE    SMKBYO-STS-UTAGAI   ALSO    SMKBYO-STS-KYUSEI
                   WHEN    1                   ALSO    1
                       MOVE    "3"             TO  PTBYO-UTAGAIFLG
                   WHEN    1                   ALSO    0
                       MOVE    "1"             TO  PTBYO-UTAGAIFLG
                   WHEN    0                   ALSO    1
                       MOVE    "2"             TO  PTBYO-UTAGAIFLG
                   WHEN    OTHER
                       MOVE    SPACE           TO  PTBYO-UTAGAIFLG
               END-EVALUATE     
           END-IF
      *
           ADD     1                   TO  WRK-RENNUM
      *
           INITIALIZE                      RECE83-REC
           MOVE    RECE82-KEY          TO  RECE83-KEY1
           MOVE    PTBYO-SRYYMD        TO  RECE83-SRYYMD
           MOVE    WRK-RENNUM          TO  RECE83-RENNUM
           MOVE    RECE82-PTNUM        TO  RECE83-PTNUM
           MOVE    PTBYO-SRYKA         TO  RECE83-SRYKA
           MOVE    PTBYO-KHNBYOMEICD   TO  RECE83-KHNBYOMEICD   
           MOVE    PTBYO-BYOMEICD-G    TO  RECE83-BYOMEICD-G
           MOVE    PTBYO-UTAGAIFLG     TO  RECE83-UTAGAIFLG
           IF      PTBYO-SYUBYOFLG     =   "1"
               MOVE    PTBYO-SYUBYOFLG     TO  RECE83-SYUBYOFLG    
           ELSE
               MOVE    "9"                 TO  RECE83-SYUBYOFLG 
           END-IF
           MOVE    PTBYO-MANSEIKBN     TO  RECE83-MANSEIKBN
           IF      RECE82-SRYYM        <   PTBYO-TENKIYMD(1:6)
               CONTINUE
           ELSE    
               MOVE    PTBYO-TENKIKBN      TO  RECE83-TENKIKBN
               MOVE    PTBYO-TENKIYMD      TO  RECE83-TENKIYMD
           END-IF
           MOVE    PTBYO-BYOMEI        TO  RECE83-BYOMEI
           MOVE    PTBYO-BYOMEIMOJI    TO  RECE83-BYOMEIMOJI
           MOVE    PTBYO-BYOMEIHENFLG  TO  RECE83-BYOMEIHENFLG
      *
           MOVE    WRK-PTBYO-BYOMEI    TO  RECE83-IN-BYOMEI
           MOVE    WRK-PTBYO-BYOMEIMOJI
                                       TO  RECE83-IN-BYOMEIMOJI
      *
           MOVE    PTBYO-HOSOKU-COMMENT
                                       TO  RECE83-HOSOKU-COMMENT
      *
           MOVE    RECE82-RECESYUBETU  TO  RECE83-RECESYUBETU   
      *
           WRITE   RECE83-REC
           IF      STS-RECE83          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE           TO  WRK-ERR-AREA
               INITIALIZE                  WRK-ERR-AREA
               MOVE    3               TO  WRK-ERR-FLG
               MOVE    RECE83PARA      TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE83      TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-RECE83
           .
       20011-RECE83-WRITE-EXT.
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
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            ジョブ管理終了処理
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルエラー処理
      *****************************************************************
       500-FILE-ERR-ABORT-SEC           SECTION.
      *
           EVALUATE    WRK-ERR-FLG
               WHEN    1
                   MOVE    "ファイルＯＰＥＮエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file open err" TO  WRK-ERR-FILE-MSG1
               WHEN    2
                   MOVE    "ファイル読み込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file read err" TO  WRK-ERR-FILE-MSG1
               WHEN    3
                   MOVE    "ファイル書き込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file write err"
                                           TO  WRK-ERR-FILE-MSG1
               WHEN    4
                   MOVE    "ファイル更新エラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file rewrite err"
                                           TO  WRK-ERR-FILE-MSG1
           END-EVALUATE
      *
           STRING  WRK-ERR-FILE-MSG        DELIMITED   BY  SPACE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   " STS="                 DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
           END-STRING
           PERFORM 500-ERR-HENSYU-SEC
      *
           STRING  "ORCR0890 "             DELIMITED   BY  SIZE
                   WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
                   " FILE="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   "  STS="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                   LOW-VALUE               DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
      *
           .
       500-FILE-ERR-ABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "処理が中止されました"
                                           TO  WRK-RECEERR
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
      *                ジョブ管理終了処理
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE82-FILE
           CLOSE   RECE83-FILE
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE82  IN  " CNT-RECE82 
           DISPLAY "RECE83  CNT " CNT-RECE83  
           DISPLAY "*** ORCR0890 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタ　ＲＥＡＤ
      *****************************************************************
       900-PTBYO-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTBYOMEI-REC
               MOVE    ZERO                TO  FLG-PTBYOMEI
           ELSE
               INITIALIZE                      PTBYOMEI-REC
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           .
       900-PTBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書読込
      *****************************************************************
       900-RECE82-READ-SEC             SECTION.
      *
           READ    RECE82-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
               NOT AT  END
                   IF      STS-RECE82          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE82PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE82      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   ADD     1           TO  CNT-RECE82
                                           CNT-IN-CANCEL
      *
                   DISPLAY "RECE82  PTNUM=" RECE82-PTNUM
      *
           END-READ
           .
       900-RECE82-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    WRK-PARA-SRYYM  TO  SYS-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
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
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "ＤＢ読み込みエラー　TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCR0890 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-COBABORT-SEC
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
      *    クローズ処理
      *****************************************************************
       900-CLOSE-SEC                  SECTION.
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
