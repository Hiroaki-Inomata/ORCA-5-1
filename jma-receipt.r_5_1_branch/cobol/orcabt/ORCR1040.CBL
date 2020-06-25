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
       PROGRAM-ID.             ORCR1040.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 公害レセプト（入院外）
      *                      傷病名編集
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/11/16    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.06.01    NACL-藤原    10/11/04  補足コメント対応
      *  04.06.02    NACL-藤原    12/07/04  表示期間によるレセプト記載対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE100-FILE    ASSIGN  RECE100PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE100-KEY
                                   FILE    STATUS  IS  STS-RECE100.
           SELECT  RECE110-FILE    ASSIGN  RECE110PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE110-KEY
                                   FILE    STATUS  IS  STS-RECE110.
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       FD  RECE100-FILE.
       01  RECE100-REC. 
           COPY    "CPRCF100.INC".
      *
       FD  RECE110-FILE.
       01  RECE110-REC. 
           COPY    "CPRCF110.INC".
      *
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE100//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE110//.
      *
      *    エラーファイル名称領域
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    ファイル名称領域
           COPY    "CPRECEDAT1.INC".
           COPY    "CPTEMPFL.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE100         PIC X(02).
           03  STS-RECE110         PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-RECE110         PIC 9(01).
      *
           03  FLG-BYOMEI-UMU      PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN              PIC 9(06).
           03  CNT-IN-CANCEL       PIC 9(06).
           03  CNT-OUT             PIC 9(06).
      *
           03  CNT-SYU-BYOMEI      PIC 9(01).
      *
       01  INDEX-AREA.
           03  IDX                 PIC 9(03). 
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM  PIC X(06).
               05  WRK-PARA-TERMID PIC X(16).
               05  WRK-PARA-SYSYMD PIC X(08).
               05  WRK-PARA-JOBID  PIC 9(07).
               05  WRK-PARA-SHELLID
                                   PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *
           03  WRK-YM              PIC 9(06).
           03  WRK-RENNUM          PIC 9(02).
      *
           03  WRK-RECEERR         PIC X(200). 
      *
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-N-SRYYMD    PIC 9(08).
           03  KEY-OLD.
               05  KEY-O-SRYYMD    PIC 9(08).
      *
       01  WRK-CONS-AREA.
      *    大気系４指定疾病
           03  WRK-CONS-BYOMEI1    PIC X(12)   VALUE
                                                   "慢性気管支炎".  
           03  WRK-CONS-BYOMEI2    PIC X(12)   VALUE
                                                   "気管支ぜん息".  
           03  WRK-CONS-BYOMEI3    PIC X(10)   VALUE
                                                   "気管支喘息".  
           03  WRK-CONS-BYOMEI4    PIC X(16)   VALUE
                                                   "ぜん息性気管支炎".  
           03  WRK-CONS-BYOMEI5    PIC X(14)   VALUE
                                                   "喘息性気管支炎".  
           03  WRK-CONS-BYOMEI6    PIC X(06)   VALUE
                                                   "肺気腫".  
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *     
           COPY    "CPSK200501.INC".
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
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
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
                                       FLG-AREA
                                       STS-AREA
           MOVE    LOW-VALUE           TO  KEY-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *
           MOVE    "RECE900"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE100PARA
      *
           MOVE    "RECE910"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE110PARA
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ＤＢオープン処理
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR1040"      TO  JOB-PGID
               MOVE    "傷病名編集"    TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF    
      *
           OPEN    INPUT               RECE100-FILE   
      * 
           OPEN    OUTPUT              RECE110-FILE
           CLOSE                       RECE110-FILE
           OPEN    I-O                 RECE110-FILE
      *         
      *    レセプト編集区分情報
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
           DISPLAY "SYS-200501-TENKIBYOMEI-KBN="
                                    SYS-200501-TENKIBYOMEI-KBN
      *
           PERFORM 900-RECE100-READ-SEC
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
           INITIALIZE                      PTBYOMEI-REC
           MOVE    RECE100-HOSPNUM     TO  PTBYO-HOSPNUM
           MOVE    RECE100-PTID        TO  PTBYO-PTID
           MOVE    RECE100-HKNCOMBI    TO  PTBYO-HKNCOMBI
           MOVE    RECE100-SRYYM       TO  PTBYO-SRYYMD (1:6)
           MOVE    "31"                TO  PTBYO-SRYYMD (7:2)
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key32"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key32"             TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
               ADD     1               TO  CNT-IN-CANCEL
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM 2001-PTBYO-HENSYU-SEC
                           UNTIL       FLG-PTBYOMEI     =   1
                           OR          FLG-END          =   1
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key32"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-END             =   ZERO
               PERFORM 900-RECE100-READ-SEC      
      *
      *        処理中止設定処理
               IF      CNT-IN-CANCEL   =   50
                   MOVE    ZERO            TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF
           .
       200-MAIN-EXT.
           EXIT. 
      *            
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
           PERFORM             UNTIL       FLG-END         =   1
                               OR          FLG-PTBYOMEI    =   1
                               OR          KEY-NEW     NOT =   KEY-OLD
               PERFORM 2001-PTBYO-CHK-SEC
      * 
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key32"             TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
               ADD     1               TO  CNT-IN-CANCEL
      *
      *        処理中止設定処理
               IF      CNT-IN-CANCEL   =   50
                   MOVE    ZERO            TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
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
      *    入外区分
           IF      PTBYO-NYUGAIKBN      =   SPACE
               CONTINUE
           ELSE
               IF      PTBYO-NYUGAIKBN  NOT =   RECE100-NYUGAIKBN
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
      *    診療開始日
           IF      PTBYO-SRYYMD(1:6)    =   RECE100-SRYYM
               CONTINUE
           ELSE
               IF      PTBYO-SRYYMD(1:6)    >   RECE100-SRYYM 
                   GO  TO  2001-PTBYO-CHK-EXT
               ELSE
      *            転帰日
                   IF      PTBYO-TENKIKBN       =   SPACE
                       CONTINUE
                   ELSE
                       IF    ( SYS-200501-TENKIBYOMEI-KBN
                                                   =   "0"  )
                       OR    ( PTBYO-REZEMM        =   ZERO )
                           IF      PTBYO-TENKIYMD(1:6) >=  RECE100-SRYYM
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
                       IF      WRK-YM              >=  RECE100-SRYYM
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
                       IF      WRK-YM              >=  RECE100-SRYYM
                           CONTINUE
                       ELSE
                           GO  TO  2001-PTBYO-CHK-EXT
                       END-IF
                   END-IF    
               END-IF
           END-IF
      *                               
      *    編集処理
           PERFORM 20011-RECE110-WRITE-SEC
           .
       2001-PTBYO-CHK-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    編集処理
      *****************************************************************
       20011-RECE110-WRITE-SEC              SECTION.
      *
      *    主病名フラグの設定
           IF      PTBYO-SYUBYOFLG     =   "1"
               CONTINUE
           ELSE
      *        大気系４指定疾病のときは主病とする
               MOVE    ZERO                TO  CNT-SYU-BYOMEI
               INSPECT PTBYO-BYOMEI    TALLYING  
                       CNT-SYU-BYOMEI  FOR ALL WRK-CONS-BYOMEI1,
                                           ALL WRK-CONS-BYOMEI2,
                                           ALL WRK-CONS-BYOMEI3,
                                           ALL WRK-CONS-BYOMEI4,
                                           ALL WRK-CONS-BYOMEI5,
                                           ALL WRK-CONS-BYOMEI6
               IF      CNT-SYU-BYOMEI      >   ZERO      
                   MOVE    "1"                 TO  PTBYO-SYUBYOFLG 
               ELSE
                   MOVE    "9"                 TO  PTBYO-SYUBYOFLG
               END-IF 
           END-IF
      *
      *    転記日と診療月による転記の設定
           IF      RECE100-SRYYM       <   PTBYO-TENKIYMD(1:6)
               MOVE    SPACE               TO  PTBYO-TENKIKBN
                                               PTBYO-TENKIYMD
           END-IF    
      *
           IF      PTBYO-BYOMEIHENFLG  =   "1"
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    PTBYO-BYOMEI        TO  KANACHK-MAE-INPUT
               MOVE    80                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT   TO  PTBYO-BYOMEI
               COMPUTE PTBYO-BYOMEIMOJI    =   KANACHK-MAX  /   2
           END-IF        
      *
      *    疑いフラグ、補足コメントより病名を編集
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
               MOVE    SMKBYO-BYOMEIHENFLG TO  PTBYO-BYOMEIHENFLG
               MOVE    SMKBYO-BYOMEICD-G   TO  PTBYO-BYOMEICD-G
           END-IF
      *
           MOVE    ZERO                TO  FLG-BYOMEI-UMU
      *
           INITIALIZE                      RECE110-REC
           MOVE    RECE100-KEY         TO  RECE110-KEY1
           MOVE    PTBYO-SYUBYOFLG     TO  RECE110-SYUBYOFLG    
           MOVE    PTBYO-SRYYMD        TO  RECE110-SRYYMD
           MOVE    PTBYO-TENKIKBN      TO  RECE110-TENKIKBN
           MOVE    PTBYO-TENKIYMD      TO  RECE110-TENKIYMD
           START   RECE110-FILE
               KEY IS    >=   RECE110-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE110
               NOT INVALID
                   PERFORM 900-RECE110-READ-SEC
           END-START
      *
           PERFORM UNTIL  ( FLG-END        =   1                 )
                   OR     ( FLG-RECE110    =   1                 )
                   OR     ( FLG-BYOMEI-UMU =   1                 )
                   OR     ( RECE100-KEY
                                       NOT =   RECE110-KEY1      )
                   OR     ( PTBYO-SYUBYOFLG
                                       NOT =   RECE110-SYUBYOFLG )
                   OR     ( PTBYO-SRYYMD
                                       NOT =   RECE110-SRYYMD    )
                   OR     ( PTBYO-TENKIKBN
                                       NOT =   RECE110-TENKIKBN  )
                   OR     ( PTBYO-TENKIYMD
                                       NOT =   RECE110-TENKIYMD  )
               IF      PTBYO-BYOMEI    =   RECE110-BYOMEI
                   MOVE    1               TO  FLG-BYOMEI-UMU
               ELSE
                   PERFORM 900-RECE110-READ-SEC
               END-IF
           END-PERFORM
      *
           IF      FLG-BYOMEI-UMU      =   ZERO
               ADD     1                   TO  WRK-RENNUM
      *
               INITIALIZE                      RECE110-REC
               MOVE    RECE100-KEY         TO  RECE110-KEY1
               MOVE    PTBYO-SYUBYOFLG     TO  RECE110-SYUBYOFLG
               MOVE    PTBYO-SRYYMD        TO  RECE110-SRYYMD
               MOVE    PTBYO-TENKIKBN      TO  RECE110-TENKIKBN
               MOVE    PTBYO-TENKIYMD      TO  RECE110-TENKIYMD
               MOVE    WRK-RENNUM          TO  RECE110-RENNUM
               MOVE    PTBYO-BYOMEI        TO  RECE110-BYOMEI
               MOVE    PTBYO-BYOMEIMOJI    TO  RECE110-BYOMEIMOJI
      *
               WRITE   RECE110-REC
               ADD     1                   TO  CNT-OUT
           END-IF
           .
       20011-RECE110-WRITE-EXT.
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
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
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
           CLOSE   RECE100-FILE
           CLOSE   RECE110-FILE
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
           DISPLAY "RECE100  CNT-IN  :" CNT-IN
           DISPLAY "RECE110  CNT-OUT :" CNT-OUT  
           DISPLAY "*** ORCR1040 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル名取得処理
      *****************************************************************
       800-FILENM-SET-SEC        SECTION.
      *
           MOVE    WRK-PARA-TERMID     TO  RECEDAT1PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEDAT1PARA-HOSPNUM
           MOVE    SGETTEMP-TEMPDIR    TO  TEMPFLPARA
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  TEMPFLPARA (SGETTEMP-ST:)
           .
       800-FILENM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタ　ＲＥＡＤ
      *****************************************************************
       900-PTBYO-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTBYOMEI
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC 
           ELSE
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
           .
       900-PTBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本情報ファイル読込処理
      *****************************************************************
       900-RECE100-READ-SEC             SECTION.
      *
           READ    RECE100-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
               NOT AT  END
                   ADD     1           TO  CNT-IN
                                           CNT-IN-CANCEL
      *
                   DISPLAY "RECE100  PTNUM=" RECE100-PTNUM
           END-READ
           .
       900-RECE100-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名情報ファイル読込処理
      *****************************************************************
       900-RECE110-READ-SEC           SECTION.
      *
           READ    RECE110-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-RECE110
               NOT AT  END
                   MOVE    ZERO        TO  FLG-RECE110
           END-READ
           .
       900-RECE110-READ-EXT.
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
