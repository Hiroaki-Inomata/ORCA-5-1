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
       PROGRAM-ID.             SEIKYU03BYOMEI1.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 山形・複写式レセプト用患者病名ファイル作成ＰＧ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/09/17    有村     　   新規作成(2.4.0のORCR0050を元に作成)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ***********************     ORCR0050修正履歴     ****************
      *  01.00.01    MCC-藤原     01.06.29  保険者番号・
      *                                     適用開始年月日の追加
      *  01.00.02    MCC-藤原     01.10.26  保険組合せの判定の修正
      *  01.00.03    MCC-藤原     01.12.13  レセプト表示月の判定の修正
      *  01.00.04    MCC-藤原     02.02.08  転帰日が未来日のとき転帰の
      *                                     表示をやめる
      *  01.00.06    MCC-藤原     02.02.13  保険組合せを３から１０に変更
      *  01.00.07    MCC-藤原     02.05.13  病名の疑いフラグからの編集
      *  01.00.08    NACL-藤原    02.11.11  転帰日が未来日のとき病名の表示
      *                                     をするように修正
      *  01.00.09    NACL-藤原    03.03.12  補助区分の追加
      *                                     診療科数１０から２０へ変更
      *  01.00.10    NACL-藤原    03/07/02  主病名編集フラグによる編集の追加
      *  01.00.11    NACL-藤原    03/09/03  疑いフラグからのコードの編集
      *  01.00.12    NACL-藤原    04/02/01  病名コード２１に変更
      ****************************
      *  01.00.13    内田         07/08/21  日レセver4.0対応
      *  01.00.14    門間         08/10/09  SPA-AREAの初期化
      *  01.00.15    吉川         11/06/16  レセプト病名記載順対応
      *                                     補足コメント対応
      *  01.00.16    吉川         14/10/03  ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.17    萩野         16/06/08  対象公費はPGオプションから取得する
      *  01.00.18    萩野         18/05/08  表示期間によるレセプト記載対応
      *  01.00.19    井上         18/10/10  修正履歴コメント削除
      *  01.00.20    門間         19/06/05  入院で診療行為がない場合、
      *                                     病名が印字されるよう修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *
           SELECT  RECE03-FILE     ASSIGN  RECE03PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE03-KEY
                                   FILE    STATUS  IS  STS-RECE03.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    レセプト明細書  
      *
       FD  RECE03-FILE.
       01  RECE03-REC. 
           COPY    "CPRCF003.INC".
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC".
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE03//.
      *
      *    エラーファイル 名称領域 
           COPY  "CPRECEERR.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE03          PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PRGOPTION  PIC 9(01).
           03  FLG-OK         PIC 9(01).
           03  FLG-TAISYO     PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN              PIC 9(06).
           03  CNT-OUT             PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                 PIC 9(03). 
           03  IDY                 PIC 9(03). 
           03  IDZ                 PIC 9(03).
           03  IDXX                PIC 9(04).
           03  IDX1                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM   PIC X(06).
               05  WRK-PARA-TERMID  PIC X(16).
               05  WRK-PARA-SYSYMD  PIC X(08).
               05  WRK-PARA-JOBID   PIC 9(07).
               05  WRK-PARA-SHELLID PIC X(08).
               05  WRK-PARA-HOSPNUM PIC 9(02).
           03  WRK-PATH             PIC X(64). 
           03  WRK-YM               PIC 9(06).
           03  WRK-RENNUM           PIC 9(02).
      *
           03  WRK-RECEERR         PIC X(200). 
      *    傷病名編集
           03  WRK-BYOMEI          PIC X(240).
           03  WRK-BYOMEIW         PIC X(240).
      *    傷病名編集フラグ（疑い）
           03  WRK-HENFLG1         PIC X(01).
      *    傷病名編集フラグ（急性）
           03  WRK-HENFLG2         PIC X(01). 
      *    病名文字数         
           03  WRK-MOJISU          PIC 9(03).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
           03  WRK-OPTION-TBL.
               05  WRK-OPTION        PIC X(100)  OCCURS 100.
           03  WRK-PRGID             PIC X(25).
           03  WRK-KBNCD             PIC X(16).
           03  WRK-CHK-KOHNUM-TBL.
               05  WRK-CHK-KOHNUM    PIC X(03)  OCCURS 4.
      *    対象公費
           03  WRK-TAISYO-KOHNUM     PIC X(03)  OCCURS 100.
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-N-SRYYMD    PIC 9(08).
           03  KEY-OLD.
               05  KEY-O-SRYYMD    PIC 9(08).
      *
      *    疑い編集
       01  WRK-UTAGAI-AREA.
      *    疑い
           03  WRK-CONS-UTAGAI     PIC X(06)   VALUE   "の疑い".
           03  WRK-CONS-UTAGAI-MOJISU
                                   PIC 9(02)   VALUE   6.       
      *    急性
           03  WRK-CONS-KYUSEI     PIC X(04)   VALUE   "急性".      
           03  WRK-CONS-KYUSEI-MOJISU
                                   PIC 9(02)   VALUE   4. 
      *
       01  WRK-CONS-AREA.
      *    疑い
           03  WRK-CONS-BYOMEICD-UTAGAI
                                   PIC X(07)   VALUE   "ZZZ8002".   
      *    急性
           03  WRK-CONS-BYOMEICD-KYUSEI
                                   PIC X(07)   VALUE   "ZZZ4012".   
      *    DB参照用
           COPY    "COMMON-SPA".
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
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    プログラムオプション
       01  PRGOPTION-REC.
           COPY    "CPPRGOPTION.INC".
      *
      *    入院履歴マスタ
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
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
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  WRK-PARA-LINK.
           03  WRK-PARA-LINK-SRYYM    PIC  X(06).
           03  WRK-PARA-LINK-TERMID   PIC  X(16).
           03  WRK-PARA-LINK-SYSYMD   PIC  X(08).
           03  WRK-PARA-LINK-JOBID    PIC  9(07).
           03  WRK-PARA-LINK-SHELLID  PIC  X(08).
           03  WRK-PARA-LINK-HOSPNUM  PIC  9(02).
      ****************************************************************
       PROCEDURE           DIVISION
               USING    WRK-PARA-LINK.
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
           EXIT    PROGRAM
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
                                       RECEERR
           MOVE    LOW-VALUE           TO  KEY-AREA
           MOVE    WRK-PARA-LINK       TO  WRK-PARA
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "SEIKYU06BYOMEI1"   TO  JOB-PGID
           MOVE    "傷病名編集1"       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "RECE030"           TO  RECE03PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE03PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  RECE03PARA-HOSPNUM
      *
           MOVE  RECE03PARA-BASENAME     TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (2)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  RECE03PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEERR
      *
           OPEN    OUTPUT              RECE03-FILE
           CLOSE                       RECE03-FILE
           OPEN    I-O                 RECE03-FILE
      *     
      *    レセプト編集区分情報
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "*"              TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM(1:6)
                                    TO  SYS-STYUKYMD(1:6)
                                        SYS-EDYUKYMD(1:6)
           MOVE    "01"             TO  SYS-STYUKYMD(7:2)
                                        SYS-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-2005-REC
                
           ELSE    
               MOVE    SPACE            TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-BYOMEIHENKBN1   =   SPACE
               MOVE    ZERO             TO  SYS-2005-BYOMEIHENKBN1
           END-IF
           IF      SYS-2005-SYUBYOHENFLG    =   SPACE
               MOVE    ZERO             TO  SYS-2005-SYUBYOHENFLG
           END-IF
      *
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "01"             TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM(1:6)
                                    TO  SYS-STYUKYMD(1:6)
                                        SYS-EDYUKYMD(1:6)
           MOVE    "01"             TO  SYS-STYUKYMD(7:2)
                                        SYS-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM        TO  SYS-HOSPNUM
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-200501-REC
           ELSE    
               INITIALIZE                   SYS-200501-REC
           END-IF
           IF      SYS-200501-CHKKBN
                                    =   SPACE
               MOVE    ZERO             TO  SYS-200501-CHKKBN
           END-IF
           IF      SYS-200501-BYOMEI-PRTJYUN
                                    =   SPACE
               MOVE    ZERO             TO  SYS-200501-BYOMEI-PRTJYUN
           END-IF
      *
           IF      SYS-200501-TENKIBYOMEI-KBN
                                    =   SPACE
               MOVE    ZERO             TO  SYS-200501-TENKIBYOMEI-KBN
           END-IF
      *
           DISPLAY "SYS-2005-BYOMEIHENKBN1    ="
                                    SYS-2005-BYOMEIHENKBN1
           DISPLAY "SYS-2005-SYUBYOHENFLG     ="
                                    SYS-2005-SYUBYOHENFLG
           DISPLAY "SYS-200501-CHKKBN         ="
                                    SYS-200501-CHKKBN
           DISPLAY "SYS-200501-BYOMEI-PRTJYUN ="
                                    SYS-200501-BYOMEI-PRTJYUN
           DISPLAY "SYS-200501-TENKIBYOMEI-KBN="
                                    SYS-200501-TENKIBYOMEI-KBN
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SEIKYU0307"              TO  WRK-PRGID
      *
      *    対象公費番号
           MOVE    "TAISYOKOHI"              TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  100
                    OR       WRK-OPTION(IDX)  =  SPACE
             MOVE    WRK-OPTION(IDX)(1:3)    TO  WRK-TAISYO-KOHNUM(IDX)
             DISPLAY "対象公費番号="  WRK-TAISYO-KOHNUM(IDX)
           END-PERFORM
      *
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションパラメタセット処理
      *****************************************************************
       1001-OPTION-SET-SEC              SECTION.
      *
           INITIALIZE                            WRK-OPTION-TBL
                                                 FLG-OK
      *    プログラムオプション情報読込
           PERFORM  900-PRGOPTION-READ-SEC
      *
      *    オプション数の初期設定
           MOVE    1                         TO  IDX1
           IF  FLG-PRGOPTION  =  ZERO
             PERFORM  VARYING  IDX  FROM  1  BY  1
                      UNTIL   (IDX     >   44000 )
                         OR   (IDX1  >  100)
                         OR   (FLG-OK  NOT =   ZERO)
      *
      *        オプションの1桁目が空白であれば処理終了
               IF   PRGOPTION-OPTION(IDX:1)  =  SPACE
               AND  IDY                      =  ZERO
                   MOVE    1                 TO  FLG-OK
               END-IF
      *
               IF  FLG-OK  =  ZERO
      *          改行地点か
                 IF  PRGOPTION-OPTION(IDX:1)  =   X"0A"
                 OR  IDY  >=  100
                   ADD    1                  TO  IDX1
                   MOVE    ZERO              TO  IDY
                 ELSE
                   ADD    1                  TO  IDY
      *            PGオプション内容を１文字ずつセット
                   MOVE    PRGOPTION-OPTION(IDX:1)
                                         TO  WRK-OPTION(IDX1)(IDY:1)
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
           .
       1001-OPTION-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *    
           PERFORM 900-KOHSKY-START-SEC
      *
           PERFORM  UNTIL  FLG-END   =   1
      *    患者病名読み込み（患者ＩＤ、診療年月日、診療科順）
             INITIALIZE                  PTBYOMEI-REC
             MOVE    SPA-HOSPNUM                TO  PTBYO-HOSPNUM
             MOVE    RECE02-PTID                TO  PTBYO-PTID
             MOVE    PTBYOMEI-REC               TO  MCPDATA-REC
             MOVE    "DBSELECT"                 TO  MCP-FUNC
             MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
             IF      SYS-200501-BYOMEI-PRTJYUN   =   "2"
                 MOVE    "key39"         TO  WRK-MCP-PATHNAME
             ELSE
                 MOVE    "key3"          TO  WRK-MCP-PATHNAME
             END-IF
             MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
                 MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                 PERFORM 900-PTBYO-READ-SEC
             ELSE
                 INITIALIZE                  PTBYOMEI-REC
                 MOVE    1                   TO  FLG-PTBYOMEI
             END-IF
      *
             PERFORM 2001-PTBYO-HENSYU-SEC
                             UNTIL       FLG-PTBYOMEI     =   1
      *    カーソルクロース
             INITIALIZE                  MCPAREA
             MOVE    "DBCLOSECURSOR"           TO  MCP-FUNC
             MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
             MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
             PERFORM  900-KOHSKY-READ-SEC
           END-PERFORM
      *
           CLOSE   RECE03-FILE
      *
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
           DISPLAY "患者病名フラグ=" FLG-PTBYOMEI
                   "患者番号=" RECE02-PTNUM
           PERFORM             UNTIL       FLG-PTBYOMEI    =   1
                               OR          KEY-NEW     NOT =   KEY-OLD
               DISPLAY "病名チェック開始"
               PERFORM 2001-PTBYO-CHK-SEC
      *
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM    
           .
       2001-PTBYO-HENSYU-EXT.
           EXIT. 
      *            
070100*****************************************************************
070200*    患者病名チェック処理
070300*****************************************************************
070400 2001-PTBYO-CHK-SEC         SECTION.
      *
      *    診療科
           MOVE    ZERO                TO  FLG-CHK 
           IF      SYS-2005-BYOMEIHENKBN1      =   ZERO     
             IF  RECE02-NYUGAIKBN  =  "1"
      *            入院履歴読み込み
                   PERFORM  900-PTNYUINRRK-READ-SEC
                   IF  PTBYO-SRYKA  =  PTNYUINRRK-NYUINKA
                       MOVE    1                    TO  FLG-CHK
                   END-IF
                   IF      FLG-CHK              =   ZERO
                       GO  TO  2001-PTBYO-CHK-EXT
                   END-IF
             ELSE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       20
                       OR      RECE02-SRYKA(IDX)   =   SPACE
                   IF      PTBYO-SRYKA          =   RECE02-SRYKA (IDX)
                       MOVE    1                    TO  FLG-CHK
                       MOVE    20                   TO  IDX
                   END-IF
               END-PERFORM
               IF      FLG-CHK              =   ZERO
                    DISPLAY "診療科 GO TO" 
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
             END-IF
           END-IF
      *
      *    入外区分
           IF      PTBYO-NYUGAIKBN      =   SPACE
               CONTINUE
           ELSE
               IF      PTBYO-NYUGAIKBN  NOT =   RECE02-NYUGAIKBN
                   DISPLAY "入外区分 GO TO" 
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
      *    保険組合せ
           IF      PTBYO-HKNCOMBI     =   ZERO
               CONTINUE
           ELSE 
               MOVE    ZERO            TO  IDZ  
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       10
                       OR      RECE02-HKNCOMBI (IDX)   =   ZERO 
                   IF      PTBYO-HKNCOMBI   =
                                            RECE02-HKNCOMBI (IDX)
                       MOVE    IDX             TO  IDZ
                       MOVE    10              TO  IDX
                   END-IF
               END-PERFORM
               IF      IDZ            >   ZERO          
                   CONTINUE
               ELSE
                   DISPLAY "保険組合せ GO TO" 
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
      *    レセプト表示月
           DISPLAY "レセプト表示月=" PTBYO-REZEFLG
           IF      PTBYO-REZEFLG          =   "1"
              GO  TO  2001-PTBYO-CHK-EXT
           END-IF
      *
      *    診療開始日
           IF      PTBYO-SRYYMD(1:6)    =   RECE02-SRYYM
               CONTINUE
           ELSE
               IF      PTBYO-SRYYMD(1:6)    >   RECE02-SRYYM
                   GO  TO  2001-PTBYO-CHK-EXT
               ELSE
      *            転帰日
                   IF      PTBYO-TENKIKBN       =   SPACE
                       CONTINUE
                   ELSE
                       IF    ( SYS-200501-TENKIBYOMEI-KBN
                                                   =   "0"  )
                       OR    ( PTBYO-REZEMM        =   ZERO )
                           IF      PTBYO-TENKIYMD(1:6) >=  RECE02-SRYYM
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
                       IF      WRK-YM              >=  RECE02-SRYYM
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
                       IF      WRK-YM              >=  RECE02-SRYYM
                           CONTINUE
                       ELSE
                           GO  TO  2001-PTBYO-CHK-EXT
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           PERFORM 20011-RECE03-WRITE-SEC
           .
       2001-PTBYO-CHK-EXT.
           EXIT.
      *
070100*****************************************************************
070200*    ;レセプト明細０３編集処理
070300*****************************************************************
070400 20011-RECE03-WRITE-SEC              SECTION.
      *
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI 
           MOVE    PTBYO-HOSOKU-COMMENT
                                       TO  KANACHK-MAE-INPUT
           MOVE    40                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  PTBYO-HOSOKU-COMMENT
      *
      *    疑いフラグより病名を編集
           MOVE    SPACE               TO  WRK-HENFLG1
                                           WRK-HENFLG2
      *       
           IF      PTBYO-UTAGAIFLG NOT =   SPACE
               COMPUTE WRK-MOJISU   =   PTBYO-BYOMEIMOJI   *   2
               IF      PTBYO-UTAGAIFLG  =   "1" OR  "3"
                   IF      WRK-MOJISU       <   8
                       MOVE    1                TO  IDZ
                   ELSE    
                       COMPUTE IDZ     =   WRK-MOJISU    -   7
                   END-IF    
                   PERFORM VARYING IDX FROM    IDZ BY  2
                           UNTIL   IDX >       WRK-MOJISU
                       IF      PTBYO-BYOMEI (IDX:2)      =   "疑"
                           MOVE    "1"              TO  WRK-HENFLG1
                           MOVE    WRK-MOJISU       TO  IDX
                       END-IF
                   END-PERFORM
               END-IF                
               IF      PTBYO-UTAGAIFLG =   "2" OR  "3"
                   PERFORM VARYING IDX FROM    1   BY  2
                           UNTIL   IDX >       WRK-MOJISU
                       IF      PTBYO-BYOMEI (IDX:4)      =   "急性"
                           MOVE    "1"              TO  WRK-HENFLG2
                           MOVE    WRK-MOJISU       TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *     
      *    病名文字数を計算
           COMPUTE WRK-MOJISU          =   PTBYO-BYOMEIMOJI   *   2
           MOVE    PTBYO-BYOMEI      TO  WRK-BYOMEI
      *
      *    疑いフラグがセットされているとき
           IF    ( PTBYO-UTAGAIFLG   =   "1" OR  "3" )
           AND   ( WRK-HENFLG1         =   SPACE     )                         
               MOVE    SPACE               TO  WRK-BYOMEI
               STRING  PTBYO-BYOMEI        DELIMITED BY  SPACE
                       WRK-CONS-UTAGAI     DELIMITED BY  SIZE
                       INTO                WRK-BYOMEI
               END-STRING
               COMPUTE WRK-MOJISU          =      
                                WRK-MOJISU +   WRK-CONS-UTAGAI-MOJISU
               IF      PTBYO-BYOMEICD (21)  NOT =   SPACE             
                   MOVE    "1"                 TO  PTBYO-BYOMEIHENFLG
               ELSE
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       21
                       IF      PTBYO-BYOMEICD (IDX)    =   SPACE
                           MOVE    WRK-CONS-BYOMEICD-UTAGAI
                                               TO  PTBYO-BYOMEICD (IDX)
                           MOVE    21          TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-IF    
           IF    ( PTBYO-UTAGAIFLG   =   "2" OR  "3" )
           AND   ( WRK-HENFLG2         =   SPACE     )                         
               MOVE    SPACE               TO  WRK-BYOMEIW
               STRING  WRK-CONS-KYUSEI     DELIMITED BY  SIZE
                       WRK-BYOMEI          DELIMITED BY  SPACE
                       INTO                WRK-BYOMEIW
               END-STRING
               COMPUTE WRK-MOJISU          =      
                                WRK-MOJISU +   WRK-CONS-KYUSEI-MOJISU
               MOVE    WRK-BYOMEIW         TO  WRK-BYOMEI  
               IF      PTBYO-BYOMEICD (21) NOT =   SPACE             
                   MOVE    "1"                 TO  PTBYO-BYOMEIHENFLG
               ELSE
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       21
                       IF      PTBYO-BYOMEICD (IDX)    =   SPACE
                           MOVE    WRK-CONS-BYOMEICD-KYUSEI
                                               TO  PTBYO-BYOMEICD (IDX)
                           MOVE    21          TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           IF    PTBYO-HOSOKU-COMMENT  NOT =   SPACE
               MOVE    SPACE               TO  WRK-BYOMEIW
               STRING  WRK-BYOMEI          DELIMITED BY SPACE
                       "（"                DELIMITED BY SIZE
                       PTBYO-HOSOKU-COMMENT
                                           DELIMITED BY SPACE
                       "）"                DELIMITED BY SIZE
               INTO    WRK-BYOMEIW
               END-STRING
               MOVE    WRK-BYOMEIW         TO  WRK-BYOMEI
           END-IF
      *
           ADD     1                   TO  WRK-RENNUM
           
           INITIALIZE                      RECE03-REC
           MOVE    SPA-HOSPNUM         TO  RECE03-HOSPNUM
           MOVE    RECE02-SRYYM        TO  RECE03-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE03-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE03-PTID
           MOVE    RECE02-RECEKA       TO  RECE03-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE03-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE03-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE03-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE03-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE03-TEKSTYMD
           MOVE    PTBYO-SRYYMD        TO  RECE03-SRYYMD
           MOVE    WRK-RENNUM          TO  RECE03-RENNUM
           MOVE    RECE02-PTNUM        TO  RECE03-PTNUM
           MOVE    PTBYO-SRYKA         TO  RECE03-SRYKA
           MOVE    PTBYO-KHNBYOMEICD   TO  RECE03-KHNBYOMEICD   
           MOVE    PTBYO-BYOMEICD-G    TO  RECE03-BYOMEICD-G
           MOVE    PTBYO-UTAGAIFLG     TO  RECE03-UTAGAIFLG
           IF      SYS-2005-SYUBYOHENFLG    =   ZERO
               MOVE    "9"                 TO  RECE03-SYUBYOFLG 
           ELSE
               IF      PTBYO-SYUBYOFLG     =   "1"
                   MOVE    PTBYO-SYUBYOFLG     TO  RECE03-SYUBYOFLG    
               ELSE
                   MOVE    "9"                 TO  RECE03-SYUBYOFLG 
               END-IF
           END-IF
           MOVE    PTBYO-MANSEIKBN     TO  RECE03-MANSEIKBN
           IF      RECE02-SRYYM        <   PTBYO-TENKIYMD(1:6)
               CONTINUE
           ELSE    
               MOVE    PTBYO-TENKIKBN      TO  RECE03-TENKIKBN
               MOVE    PTBYO-TENKIYMD      TO  RECE03-TENKIYMD   
           END-IF    
           MOVE    WRK-BYOMEI          TO  RECE03-BYOMEI
           COMPUTE RECE03-BYOMEIMOJI    =   WRK-MOJISU    /   2
           MOVE    PTBYO-BYOMEIHENFLG  TO  RECE03-BYOMEIHENFLG   
           MOVE    PTBYO-HOSOKU-COMMENT
                                       TO  RECE03-HOSOKU-COMMENT
      *
           DISPLAY "患者病名ファイルWRITE 患者番号=" RECE02-PTNUM
           WRITE   RECE03-REC
           ADD     1                   TO  CNT-OUT   
076800     .
076900 20011-RECE03-WRITE-EXT.
077000     EXIT.
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
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
057400*
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象公費番号チェック処理
      *****************************************************************
       800-TAISYOKOHI-CHK-SEC                SECTION.
      *
           INITIALIZE     FLG-TAISYO
      *
           PERFORM  VARYING  IDXX  FROM  1  BY  1
                    UNTIL    IDXX  >  100
                    OR       WRK-TAISYO-KOHNUM(IDXX)  =  SPACE
               IF  WRK-CHK-KOHNUM(1)  =  WRK-TAISYO-KOHNUM(IDXX)
               OR  WRK-CHK-KOHNUM(2)  =  WRK-TAISYO-KOHNUM(IDXX)
               OR  WRK-CHK-KOHNUM(3)  =  WRK-TAISYO-KOHNUM(IDXX)
               OR  WRK-CHK-KOHNUM(4)  =  WRK-TAISYO-KOHNUM(IDXX)
                   MOVE     1                 TO  FLG-TAISYO
                   MOVE     100               TO  IDXX
               END-IF
           END-PERFORM
      *
           .
       800-TAISYOKOHI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴マスタ読込
      *****************************************************************
       900-PTNYUINRRK-READ-SEC         SECTION.
      *
           INITIALIZE                     PTNYUINRRK-REC
      *
           MOVE    RECE02-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    RECE02-PTID            TO  PTNYUINRRK-PTID
           MOVE    RECE02-SRYYM(1:6)      TO  PTNYUINRRK-NYUINYMD(1:6)
                                              PTNYUINRRK-TAIINYMD(1:6)
           MOVE    "99"                   TO  PTNYUINRRK-NYUINYMD(7:2)
           MOVE    "01"                   TO  PTNYUINRRK-TAIINYMD(7:2)
           MOVE    PTNYUINRRK-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key11"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           IF   MCP-RC       =   ZERO
               MOVE    "DBFETCH"          TO  MCP-FUNC
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key11"            TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF  MCP-RC     =   ZERO
                   MOVE    ZERO           TO  FLG-PTNYUINRRK
                   MOVE    MCPDATA-REC    TO  PTNYUINRRK-REC
               ELSE
                   MOVE    1              TO  FLG-PTNYUINRRK
               END-IF
           ELSE
              MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "DBCLOSECURSOR"        TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key11"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           .
       900-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタ　ＲＥＡＤ
      *****************************************************************
       900-PTBYO-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
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
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      * 
           MOVE    SYSKANRI-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"               TO  MCP-FUNC
           MOVE    "tbl_syskanri"           TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"            TO  MCP-FUNC
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"              TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    公費請求書マスタ読込
      *****************************************************************
       900-KOHSKY-START-SEC            SECTION.
      *
           INITIALIZE                  RECE02-REC
           MOVE    SPA-HOSPNUM         TO  RECE02-HOSPNUM
           MOVE    WRK-PARA-SRYYM      TO  RECE02-SKYYM
           MOVE    RECE02-REC          TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"               TO  MCP-FUNC
           MOVE    "tbl_kohsky"         TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 900-KOHSKY-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
               PERFORM  900-KOHSKY-CLOSE-SEC
           END-IF
           .
       900-KOHSKY-START-EXT.
           EXIT.
      *      
      *****************************************************************
      *    公費請求書マスタＲＥＡＤ  複写式レセの対象となる公費を設定する
      *****************************************************************
       900-KOHSKY-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"                TO  MCP-FUNC
           MOVE    "tbl_kohsky"         TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE02-REC
               INITIALIZE                          WRK-CHK-KOHNUM-TBL
               MOVE    RECE02-KOHNUM(1)        TO  WRK-CHK-KOHNUM(1)
               MOVE    RECE02-KOHNUM(2)        TO  WRK-CHK-KOHNUM(2)
               MOVE    RECE02-KOHNUM(3)        TO  WRK-CHK-KOHNUM(3)
               MOVE    RECE02-KOHNUM(4)        TO  WRK-CHK-KOHNUM(4)
      *        対象公費番号チェック処理
               PERFORM  800-TAISYOKOHI-CHK-SEC
               IF  FLG-TAISYO  =  1
      *    テスト患者のときは読み飛ばす
                   MOVE    SPACE               TO  PTINF-REC
                   INITIALIZE                      PTINF-REC
                   MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                   MOVE    RECE02-PTID         TO  PTINF-PTID
                   MOVE    PTINF-REC           TO  MCPDATA-REC
                   PERFORM 800-PTINF-READ-SEC
                   IF      FLG-PTINF           =   ZERO
                   AND     PTINF-TSTPTNUMKBN   =   "1"
                       GO  TO  900-KOHSKY-READ-SEC
                   END-IF
      *
               ELSE
                 GO  TO  900-KOHSKY-READ-SEC
               END-IF
           ELSE
               MOVE    1               TO  FLG-END
               PERFORM  900-KOHSKY-CLOSE-SEC
           END-IF
           .
       900-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求マスタCLOSE
      *****************************************************************
       900-KOHSKY-CLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_kohsky"         TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-KOHSKY-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_ptinf"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                TO  MCP-FUNC
               MOVE    "tbl_ptinf"          TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
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
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_ptinf"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション情報読込
      *****************************************************************
       900-PRGOPTION-READ-SEC  SECTION.
      *
           INITIALIZE                      PRGOPTION-REC
           MOVE    SPA-HOSPNUM         TO  PRGOPTION-HOSPNUM
           MOVE    WRK-PRGID           TO  PRGOPTION-PRGID
           MOVE    WRK-KBNCD           TO  PRGOPTION-KBNCD
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PRGOPTION-REC
                   MOVE    ZERO                TO  FLG-PRGOPTION
               ELSE
                   INITIALIZE                      PRGOPTION-REC
                   MOVE    1                   TO  FLG-PRGOPTION
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PRGOPTION
           END-IF
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PRGOPTION-READ-EXT.
           EXIT.
      *