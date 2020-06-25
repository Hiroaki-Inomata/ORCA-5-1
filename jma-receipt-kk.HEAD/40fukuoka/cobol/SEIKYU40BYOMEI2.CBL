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
000200 IDENTIFICATION          DIVISION.
000300 PROGRAM-ID.             SEIKYU40BYOMEI2.
000400*****************************************************************
000500*  システム名        : ＯＲＣＡ
000600*  サブシステム名    : レセプト
000700*  コンポーネント名  :
000800*  管理者            :
      *  作成日付    作業者        記述
      *  04/09/00    有村     　   新規作成(2.3.0のORCR0055を元に作成)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ***********************     ORCR0055修正履歴     ****************
      *  01.00.01    MCC-藤原     01.06.29  保険者番号・
      *                                     適用開始年月日の追加
      *  01.00.02    MCC-藤原     02.05.14  同一開始日・病名・転帰区分のとき
      *                                     ひとつだけ出力する
      *  01.00.03    MCC-藤原     02.06.11  転帰日を表示するとき同一開始日・
      *                                     病名・転帰区分・転帰日のとき
      *                                     ひとつだけ出力する
      *  01.00.04    NACL-藤原    03.03.12  補助区分の追加
      ************************    SEIKYU40BYOMEI2修正履歴    **********
      *  01.00.01    有村         05/05/06  DB参照方法修正(MCPSUB使用)
      *  01.00.02    内田         07/10/10  日レセver4.0対応
      *  01.00.03    吉川         11/05/18  レセプト病名記載順対応
      *                                     補足コメント対応
      *  01.00.04    吉川         14/10/27  ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.05    門間         19/01/10  修正履歴コメント削除
      *****************************************************************
001200*
001300 ENVIRONMENT             DIVISION.
001400 CONFIGURATION           SECTION.
001500 INPUT-OUTPUT            SECTION.
001600 FILE-CONTROL.
      *
      *    レセプト明細書
006900*
           SELECT  RECE03-FILE     ASSIGN  RECE03PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE03-KEY
                                   FILE    STATUS  IS  STS-RECE03.
006900*
           SELECT  RECE031-FILE    ASSIGN  RECE031PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE031-KEY
                                   FILE    STATUS  IS  STS-RECE031.
006900*    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
006900*
007000 DATA                    DIVISION.
007100 FILE                    SECTION.
      *
      *    レセプト明細書
       FD  RECE03-FILE.
       01  RECE03-REC.
           COPY    "CPRCF003.INC".
      *
       FD  RECE031-FILE.
       01  RECE031-REC.
           COPY    "CPRCF0031.INC".
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
010200 WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE03//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE031//.
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
018000*    フラグ領域
       01  STS-AREA.
           03  STS-RECE031                             PIC X(02).
           03  STS-RECE03                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
018100 01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
020700*
020800*    カウント領域
020900 01  CNT-AREA.
021000     03  CNT-IN                                  PIC 9(06).
           03  CNT-OUT                                 PIC 9(06).
024000*
024100*    一時領域
024200 01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC  X(06).
               05  WRK-PARA-TERMID                     PIC  X(16).
               05  WRK-PARA-SYSYMD                     PIC  X(08).
               05  WRK-PARA-JOBID                      PIC  9(07).
               05  WRK-PARA-SHELLID                    PIC  X(08).
               05  WRK-PARA-HOSPNUM                    PIC 9(02).
      *
           03  WRK-RECEERR             PIC X(200).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-HOSPNUM       PIC 9(02).
               05  KEY-N-SRYYM         PIC 9(06).
               05  KEY-N-NYUGAIKBN     PIC X(01).
               05  KEY-N-PTID          PIC 9(10).
               05  KEY-N-RECEKA        PIC X(02).
               05  KEY-N-TEISYUTUSAKI  PIC 9(01).
               05  KEY-N-RECESYUBETU   PIC 9(04).
               05  KEY-N-HKNJANUM      PIC X(08).
               05  KEY-N-HOJOKBN       PIC X(01).
               05  KEY-N-TEKSTYMD      PIC X(08).
               05  KEY-N-SRYYMD        PIC X(08).
               05  KEY-N-TENKIKBN      PIC X(01).
               05  KEY-N-TENKIYMD      PIC X(08).
               05  KEY-N-BYOMEI        PIC X(160).
           03  KEY-OLD.
               05  KEY-O-HOSPNUM       PIC 9(02).
               05  KEY-O-SRYYM         PIC 9(06).
               05  KEY-O-NYUGAIKBN     PIC X(01).
               05  KEY-O-PTID          PIC 9(10).
               05  KEY-O-RECEKA        PIC X(02).
               05  KEY-O-TEISYUTUSAKI  PIC 9(01).
               05  KEY-O-RECESYUBETU   PIC 9(04).
               05  KEY-O-HKNJANUM      PIC X(08).
               05  KEY-O-HOJOKBN       PIC X(01).
               05  KEY-O-TEKSTYMD      PIC X(08).
               05  KEY-O-SRYYMD        PIC X(08).
               05  KEY-O-TENKIKBN      PIC X(01).
               05  KEY-O-TENKIYMD      PIC X(08).
               05  KEY-O-BYOMEI        PIC X(160).
      *
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
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
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
           INITIALIZE                  RECEERR
      *
           MOVE    ZERO                TO  FLG-END
      *
           MOVE    COMMAND-PARAM       TO  WRK-PARA
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "SEIKYU40BYOMEI2"   TO  JOB-PGID
           MOVE    "傷病名編集2"       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           MOVE    "RECE030"           TO  RECE03PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE03PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE03PARA-HOSPNUM
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "RECE031"           TO  RECE031PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE031PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE031PARA-HOSPNUM
      *
           MOVE  RECE03PARA-BASENAME     TO  SGETTEMP-BASENAMES (1)
           MOVE  RECE031PARA-BASENAME    TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  RECE03PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECE031PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECEERR
      *
           OPEN    INPUT               RECE03-FILE
      *
           OPEN    OUTPUT              RECE031-FILE
           CLOSE                       RECE031-FILE
           OPEN    I-O                 RECE031-FILE
      *
      *    レセプト編集区分情報
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "*"              TO  SYS-KBNCD
      *
           MOVE    WRK-PARA-SRYYM(1:6)
                                    TO  SYS-STYUKYMD(1:6)
                                        SYS-EDYUKYMD(1:6)
           MOVE    "01"             TO  SYS-STYUKYMD(7:2)
                                        SYS-EDYUKYMD(7:2)
      *
           MOVE  SPA-HOSPNUM        TO  SYS-HOSPNUM
           PERFORM 960-KANRIMST-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-2005-REC
           ELSE
               MOVE    SPACE            TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-TENKIHENKBN    =   SPACE
               MOVE    ZERO             TO  SYS-2005-TENKIHENKBN
           END-IF
      *
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    "2005"          TO  SYS-KANRICD
           MOVE    "01"            TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM(1:6)
                                   TO  SYS-STYUKYMD(1:6)
                                       SYS-EDYUKYMD(1:6)
           MOVE    "01"            TO  SYS-STYUKYMD(7:2)
                                       SYS-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           PERFORM 960-KANRIMST-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-200501-REC
           ELSE
               INITIALIZE                   SYS-200501-REC
           END-IF
           IF      SYS-200501-BYOMEI-PRTJYUN
                                    =   SPACE
               MOVE    ZERO             TO  SYS-200501-BYOMEI-PRTJYUN
           END-IF
      *
      *    レセプト明細０３読み込み
           PERFORM 900-RECE03-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                      RECE031-REC
           MOVE    RECE03-HOSPNUM      TO  RECE031-HOSPNUM
           MOVE    RECE03-SRYYM        TO  RECE031-SRYYM
           MOVE    RECE03-NYUGAIKBN    TO  RECE031-NYUGAIKBN
           MOVE    RECE03-PTID         TO  RECE031-PTID
           MOVE    RECE03-RECEKA       TO  RECE031-RECEKA
           MOVE    RECE03-TEISYUTUSAKI TO  RECE031-TEISYUTUSAKI
           MOVE    RECE03-RECESYUBETU  TO  RECE031-RECESYUBETU
           MOVE    RECE03-HKNJANUM     TO  RECE031-HKNJANUM
           MOVE    RECE03-HOJOKBN      TO  RECE031-HOJOKBN
           MOVE    RECE03-TEKSTYMD     TO  RECE031-TEKSTYMD
           MOVE    RECE03-SRYYMD       TO  RECE031-SRYYMD
           MOVE    RECE03-SYUBYOFLG    TO  RECE031-SYUBYOFLG
           MOVE    RECE03-TENKIKBN     TO  RECE031-TENKIKBN
           MOVE    RECE03-RENNUM       TO  RECE031-RENNUM
           MOVE    RECE03-PTNUM        TO  RECE031-PTNUM
           MOVE    RECE03-SRYKA        TO  RECE031-SRYKA
           MOVE    RECE03-KHNBYOMEICD  TO  RECE031-KHNBYOMEICD
           MOVE    RECE03-BYOMEICD-G   TO  RECE031-BYOMEICD-G
           MOVE    RECE03-UTAGAIFLG    TO  RECE031-UTAGAIFLG
           MOVE    RECE03-MANSEIKBN    TO  RECE031-MANSEIKBN
           IF      SYS-2005-TENKIHENKBN    =   ZERO
               MOVE    SPACE               TO  RECE031-TENKIYMD
           ELSE
               MOVE    RECE03-TENKIYMD     TO  RECE031-TENKIYMD
           END-IF
           MOVE    RECE03-BYOMEI       TO  RECE031-BYOMEI
           MOVE    RECE03-BYOMEIMOJI   TO  RECE031-BYOMEIMOJI
           MOVE    RECE03-BYOMEIHENFLG TO  RECE031-BYOMEIHENFLG
           MOVE    RECE03-HOSOKU-COMMENT
                                       TO  RECE031-HOSOKU-COMMENT
      *
      *    病名記載順対応
           IF      SYS-200501-BYOMEI-PRTJYUN
                                       =   "0"  OR  "1"
               IF    ( SYS-200501-BYOMEI-PRTJYUN
                                           =   "1"   )
               AND   ( RECE031-TENKIKBN    =   SPACE )
                   MOVE    "9"                 TO  RECE031-TENKIKBN-X
               ELSE
                   MOVE    RECE031-TENKIKBN    TO  RECE031-TENKIKBN-X
               END-IF
               MOVE    RECE031-TENKIYMD    TO  RECE031-TENKIYMD-X
           END-IF
      *
           WRITE   RECE031-REC
           ADD     1                   TO  CNT-OUT
      *
           MOVE    KEY-NEW             TO  KEY-OLD
           PERFORM             UNTIL   FLG-END       =   1
                               OR      KEY-NEW   NOT =   KEY-OLD
               PERFORM 900-RECE03-READ-SEC
           END-PERFORM
           .
       200-MAIN-EXT.
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
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE03-FILE
           CLOSE   RECE031-FILE
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           DISPLAY "RECE03  CNT-IN  :" CNT-IN
           DISPLAY "RECE031 CNT-OUT :" CNT-OUT
           DISPLAY "*** ORCR0055 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書０２読込
      *****************************************************************
       900-RECE03-READ-SEC             SECTION.
      *
           READ    RECE03-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
                   MOVE    HIGH-VALUE  TO  KEY-NEW
               NOT AT  END
                   ADD     1           TO  CNT-IN
                   MOVE    ZERO        TO  FLG-END
                   MOVE    RECE03-HOSPNUM      TO  KEY-N-HOSPNUM
                   MOVE    RECE03-SRYYM        TO  KEY-N-SRYYM
                   MOVE    RECE03-NYUGAIKBN    TO  KEY-N-NYUGAIKBN
                   MOVE    RECE03-PTID         TO  KEY-N-PTID
                   MOVE    RECE03-RECEKA       TO  KEY-N-RECEKA
                   MOVE    RECE03-TEISYUTUSAKI TO  KEY-N-TEISYUTUSAKI
                   MOVE    RECE03-RECESYUBETU  TO  KEY-N-RECESYUBETU
                   MOVE    RECE03-HKNJANUM     TO  KEY-N-HKNJANUM
                   MOVE    RECE03-HOJOKBN      TO  KEY-N-HOJOKBN
                   MOVE    RECE03-TEKSTYMD     TO  KEY-N-TEKSTYMD
                   MOVE    RECE03-SRYYMD       TO  KEY-N-SRYYMD
                   MOVE    RECE03-TENKIKBN     TO  KEY-N-TENKIKBN
                   MOVE    RECE03-BYOMEI       TO  KEY-N-BYOMEI
                   IF      SYS-2005-TENKIHENKBN    =   ZERO
                       MOVE    SPACE               TO  KEY-N-TENKIYMD
                   ELSE
                       MOVE    RECE03-TENKIYMD     TO  KEY-N-TENKIYMD
                   END-IF
      *
                   DISPLAY "RECE03  PTID=" RECE03-PTID
           END-READ
           .
       900-RECE03-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
               MOVE  "tbl_syskanri"          TO  MCP-TABLE
               MOVE  "key10"                 TO  MCP-PATHNAME
               CALL  "ORCDBMAIN"             USING
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
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       960-KANRIMST-READ-EXT.
           EXIT.
