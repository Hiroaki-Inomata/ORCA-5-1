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
       PROGRAM-ID.             ORCR0250.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（入院）
      *  コンポーネント名  : レセプト電算ファイル作成
      *                      レセプト明細書の並び替え処理 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/04/28    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    05/01/07  差し込みコメントを対象とする
      *
      *  02.07.01    NACL-藤原    05/08/20  特別療養費対応
      *  02.07.02    NACL-藤原    05/12/02  診療区分９９は０１に変更
      *                                     MONFUNC対応
      *
      *  ２００６年４月改正対応
      *  02.09.01    NACL-藤原    06/04/20  検査等実施判断グループの追加
      *
      *  03.03.01    NACL-藤原    06/09/29  検査の判断料の記載方法の変更
      *                                     検査等実施判断の追加
      *
      *  03.04.01    NACL-藤原    06/12/21  入院料加算のコメントを記録する
      *  03.05.01    NACL-小豆沢  07/06/06  グループ診療対応
      *
      *  04.02.01    NACL-藤原    08/03/25  平成２０年４月改正対応
      *                                     広域連合対応
      *                                     診療区分６４を６０に変更
      *  04.02.02    NACL-藤原    08/04/10  レセ電用入院料加算フラグ追加
      *
      *  04.03.01    NACL-藤原    08/07/09  仮レセ作成（院外処方含む）対応
      *
      *  04.05.01    NACL-藤原    09/06/10  摘要欄下部へのコメント記載対応
      *                                     診療種別区分の追加
      *  04.05.02    NACL-藤原    09/11/27  レセ電コメント拡張対応
      *                                     関係コメント存在フラグ追加
      *  04.05.03    NACL-藤原    10/01/13  治験対応
      *                                     剤区分の追加
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.02    NACL-藤原    11/01/05  機能追加（注加算、同時併施等）
      *  04.06.03    NACL-藤原    11/01/28  キーに剤連番追加
      *  04.06.04    NACL-門脇    12/10/15  診療行為入力したコメントのみの剤の
      *                                     レセプト記載対応
      *
      *  04.07.01    NACL-藤原    12/12/17  複数枚レセプト分割対応
      *  04.07.02    NACL-藤原    15/02/18  医療情報連携対応
      *                                     自費レセプトを対象とする
      *  04.07.03    NACL-門脇    15/08/10  日別保険組合せ番号編集対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-門脇    16/08/23  持参薬対応
      *  04.08.03    NACL-門脇    16/09/20  １５円以下薬剤の提出用レセプト記載
      *                                     （記録）対応
      *
      *  05.00.01    NACL-藤原    18/06/11  Warning 対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE64-FILE     ASSIGN  RECE64PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE64-KEY
                                   FILE    STATUS  IS  STS-RECE64.
      *
           SELECT  RECE641-FILE    ASSIGN  RECE641PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE641-KEY
                                   FILE    STATUS  IS  STS-RECE641.
      *
           SELECT  RECE65-FILE     ASSIGN  RECE65PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE65-KEY
                                   FILE    STATUS  IS  STS-RECE65.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    レセプト明細書  
       FD  RECE64-FILE.
       01  RECE64-REC. 
           COPY    "CPRCF064.INC".
      *
       FD  RECE641-FILE.
       01  RECE641-REC. 
           COPY    "CPRCF0641.INC".
      *
      *    レセプト６５  
       FD  RECE65-FILE.
       01  RECE65-REC. 
           COPY    "CPRCF065.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
      *     レセプト明細ファイル 名称領域 
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE64PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE641PARA//.
      *
      *     レセプト６５ファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE65PARA//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECEERR//.
      *
      *    ファイル名取得
           COPY    "CPRECEDAT1.INC".
           COPY    "CPTEMPFL.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE641                             PIC X(02).
           03  STS-RECE64                              PIC X(02).
           03  STS-RECE65                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-READ                                PIC 9(01).
           03  FLG-RECE65                              PIC 9(01).
      *
           03  FLG-SYSKANRI            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-OUT                                 PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(06).
           03  IDY                     PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA1.
                   07  WRK-PARA-SRYYM  PIC X(06).
                   07  WRK-PARA-TERMID PIC X(16).
                   07  WRK-PARA-SYSYMD PIC X(08).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
               05  WRK-PARA-TEISYUTUSAKI
                                       PIC X(01).
               05  WRK-PARA-SYARECEKBN PIC X(01).
               05  WRK-PARA-KKHRECEKBN PIC X(01).
               05  WRK-PARA-KOUIKIRECEKBN
                                       PIC X(01).
               05  WRK-PARA-PRTKBN     PIC X(01).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
      *
           03  WRK-KOHBUNRI            PIC 9(01).
           03  WRK-HKNCOMBI            PIC 9(04).
           03  OLD-KEY.
               05  OLD-RECE64-HOSPNUM     PIC  9(02).
               05  OLD-RECE64-SRYYM       PIC  9(06).
               05  OLD-RECE64-NYUGAIKBN   PIC  X(01).
               05  OLD-RECE64-PTID        PIC  9(10).
               05  OLD-RECE64-RECEKA      PIC  X(02).
               05  OLD-RECE64-TEISYUTUSAKI
                                          PIC  9(01).
               05  OLD-RECE64-RECESYUBETU PIC  9(04).
               05  OLD-RECE64-HKNJANUM    PIC  X(08).
               05  OLD-RECE64-HOJOKBN     PIC  X(01).
               05  OLD-RECE64-KOHID       PIC  9(10).
               05  OLD-RECE64-TEKSTYMD    PIC  X(08). 
               05  OLD-RECE64-RECEIPT-KBN PIC  X(10).
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-ERRMSG                          PIC X(300).
           03  WRK-MCP-RC                          PIC 9(9).
           03  WRK-ERR-FILE-STS                    PIC X(02).
           03  WRK-ERR-FILE-MSG                    PIC X(100).
           03  WRK-ERR-FILE-MSG1                   PIC X(17).
           03  WRK-ERR-FLG                         PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME内のBASENAME開始位置
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
           COPY    "CPSK200501.INC"
                                   REPLACING  //SYS-200501//
                                   BY         //SYS-25//.
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
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
       PROCEDURE                   DIVISION
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
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       SPA-AREA
      *
           MOVE    ZERO                TO  FLG-END
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA1
                                               WRK-PARA-TEISYUTUSAKI
                                               WRK-PARA-SYARECEKBN
                                               WRK-PARA-KKHRECEKBN
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING                                  
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           DISPLAY "WRK-PARA-PRTKBN=" WRK-PARA-PRTKBN
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "レセ電用データ並び変え"
                                   TO  JOB-SHELLMSG
           MOVE    "ORCR0250"      TO  JOB-PGID
           PERFORM   900-CALL-ORCSJOB-SEC
      *
      *    レセプト編集区分情報
           MOVE    SPACE               TO  SYS-25-REC
           INITIALIZE                      SYS-25-REC
           MOVE    SPA-HOSPNUM         TO  SYS-25-HOSPNUM
           MOVE    "2005"              TO  SYS-25-KANRICD
           MOVE    "01"                TO  SYS-25-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-25-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-25-STYUKYMD (7:2)
           MOVE    SYS-25-STYUKYMD     TO  SYS-25-EDYUKYMD
           MOVE    SYS-25-REC          TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-25-REC
           ELSE    
               MOVE    SPACE           TO  SYS-25-REC
               INITIALIZE                  SYS-25-REC
           END-IF
           IF      SYS-25-KOUIKIRECEKBN
                                       =   SPACE
               MOVE    ZERO        TO  SYS-25-KOUIKIRECEKBN
           END-IF
           MOVE    SYS-25-KOUIKIRECEKBN
                                   TO  WRK-PARA-KOUIKIRECEKBN 
           DISPLAY "KOUIKIRECEKBN=" WRK-PARA-KOUIKIRECEKBN
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    "RECE641"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           MOVE    "RECE649"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (3)
           MOVE    "RECE651"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (4)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE64PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE641PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE65PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           OPEN    OUTPUT              RECE641-FILE
           CLOSE                       RECE641-FILE
      *
           OPEN    INPUT               RECE64-FILE
           IF      STS-RECE64          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE64PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE64          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
           OPEN    INPUT               RECE65-FILE
           IF      STS-RECE64          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE64PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE64          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE641-FILE
      *
           PERFORM 900-RECE64-READ-SEC
           MOVE    RECE64-KEY1         TO  OLD-KEY
           MOVE  9                     TO  WRK-KOHBUNRI
           MOVE  ZERO                  TO  WRK-HKNCOMBI
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                      RECE641-REC
           MOVE    RECE64-KEY1         TO  RECE641-KEY1
           IF      RECE64-SRYKBN       =   "33"
               PERFORM   210-KBNNUM-SET-SEC
           END-IF
           MOVE    RECE64-ZAINUM       TO  RECE641-ZAINUM
           MOVE    RECE64-ZAIREN       TO  RECE641-ZAIREN
           MOVE    RECE64-KOHBUNRI     TO  RECE641-KOHBUNRI
           IF      RECE64-KEY1     NOT =   OLD-KEY
                MOVE  9                TO  WRK-KOHBUNRI
                MOVE  ZERO             TO  WRK-HKNCOMBI
                MOVE  RECE64-KEY1      TO  OLD-KEY
           END-IF
           IF      RECE64-KOHBUNRI     <   WRK-KOHBUNRI
                MOVE   RECE64-KOHBUNRI TO  WRK-KOHBUNRI
                MOVE   RECE64-HKNCOMBI TO  WRK-HKNCOMBI
           END-IF
           IF      RECE64-SRYKBN   =   "99"
               IF      RECE64-SRYSYUKBN    =   "991"
                   MOVE    "99"            TO  RECE641-SRYKBN
               ELSE
                   MOVE    "01"            TO  RECE641-SRYKBN
               END-IF
               IF  RECE64-HKNCOMBI     =   ZERO
                   MOVE    WRK-HKNCOMBI    TO  RECE64-HKNCOMBI
               END-IF
           ELSE
               IF      RECE64-SRYKBN   =   "64"
                   MOVE    "60"            TO  RECE641-SRYKBN
               ELSE
                   MOVE    RECE64-SRYKBN       TO  RECE641-SRYKBN
                   IF      RECE64-SRYKBN   >=  "90"
                       IF  RECE64-HKNCOMBI     =   ZERO
                           MOVE    WRK-HKNCOMBI    TO  RECE64-HKNCOMBI
                       END-IF
                   END-IF
               END-IF
           END-IF
           MOVE    RECE64-PTNUM        TO  RECE641-PTNUM
           MOVE    RECE64-SRYKA        TO  RECE641-SRYKA
           MOVE    RECE64-HKNCOMBI     TO  RECE641-HKNCOMBI
           MOVE    RECE64-JIHOKBN      TO  RECE641-JIHOKBN
           MOVE    RECE64-ZAITEN       TO  RECE641-ZAITEN
           MOVE    RECE64-SRYCDTOTAL   TO  RECE641-SRYCDTOTAL
           MOVE    RECE64-SURYOUTOTAL  TO  RECE641-SURYOUTOTAL
           MOVE    RECE64-MEISAISU     TO  RECE641-MEISAISU
           MOVE    RECE64-ZAIKAISU     TO  RECE641-ZAIKAISU
           MOVE    RECE64-SYUTEN1      TO  RECE641-SYUTEN1
           MOVE    RECE64-SYUTEN2      TO  RECE641-SYUTEN2
           MOVE    RECE64-YKZTEN       TO  RECE641-YKZTEN
           MOVE    RECE64-KIZAITEN     TO  RECE641-KIZAITEN
           MOVE    RECE64-DRCD         TO  RECE641-DRCD
           MOVE    RECE64-ZAIREQFLG    TO  RECE641-ZAIREQFLG
           MOVE    RECE64-TEIGENRATE   TO  RECE641-TEIGENRATE
           MOVE    RECE64-KNSJISGRPKBN TO  RECE641-KNSJISGRPKBN
           MOVE    RECE64-KNSJISKBN    TO  RECE641-KNSJISKBN
           MOVE    RECE64-DAY-AREA     TO  RECE641-DAY-AREA
           MOVE    RECE64-NYUKSNFLG    TO  RECE641-NYUKSNFLG
           MOVE    RECE64-TOYAKUFLG    TO  RECE641-TOYAKUFLG
           MOVE    RECE64-SRYSYUKBN    TO  RECE641-SRYSYUKBN
           MOVE    RECE64-SETCOMFLG    TO  RECE641-SETCOMFLG
           MOVE    RECE64-ZAIKBN       TO  RECE641-ZAIKBN
           MOVE    RECE64-ACCTP-SYUGI-TBL-G
                                       TO  RECE641-ACCTP-SYUGI-TBL-G
           MOVE    RECE64-HKNCOMBI-DAY-AREA
                                       TO  RECE641-HKNCOMBI-DAY-AREA
           MOVE    RECE64-NYUSAIFLG    TO  RECE641-NYUSAIFLG
           MOVE    RECE64-YKZKISAIFLG  TO  RECE641-YKZKISAIFLG
           MOVE    RECE64-JISANYAKKBN  TO  RECE641-JISANYAKKBN
      *
           WRITE   RECE641-REC
      *
           IF      STS-RECE641         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE641PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE641         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-OUT
      *
      *
           PERFORM 900-RECE64-READ-SEC      
           .
       200-MAIN-EXT.
           EXIT. 
      *
      *****************************************************************
      *    表用番号−区分番号のセット
      *****************************************************************
       210-KBNNUM-SET-SEC                SECTION.
      *
           INITIALIZE                      RECE65-REC
           MOVE    RECE64-KEY1         TO  RECE65-KEY1
           MOVE    RECE64-ZAINUM       TO  RECE65-ZAINUM
           START   RECE65-FILE
               KEY IS    >=   RECE65-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE65
               NOT INVALID
                   READ    RECE65-FILE       NEXT
                       AT  END
                           MOVE    1         TO  FLG-RECE65
                       NOT AT  END
                           MOVE    ZERO      TO  FLG-RECE65
                  END-READ
           END-START
           IF  ( FLG-RECE65            =   ZERO )            AND
               ( RECE64-KEY1           =   RECE65-KEY1   )   AND
               ( RECE64-ZAINUM         =   RECE65-ZAINUM       )
      *
      *        表用−区分番号
      *        ４:点滴
      *        ５：中心静脈
      *        ６：中心静脈栄養
      *        ７以上　その他注射
               IF     RECE65-CDKBN-KBNNUM(1)   NOT =    ZERO
                  IF     RECE65-CDKBN-KBNNUM(1)  <=    6
                      EVALUATE    RECE65-CDKBN-KBNNUM(1)
                         WHEN     5
                             MOVE   6     TO   RECE641-CDKBN-KBNNUM
                         WHEN     6
                             MOVE   8     TO   RECE641-CDKBN-KBNNUM
                         WHEN  OTHER
                             MOVE   RECE65-CDKBN-KBNNUM(1)
                                          TO   RECE641-CDKBN-KBNNUM
                      END-EVALUATE
                  ELSE
                      MOVE   RECE65-CDKBN-KBNNUM(1)
                                       TO  RECE641-CDKBN-KBNNUM
                      ADD    10        TO  RECE641-CDKBN-KBNNUM
                  END-IF
      *           点滴注射（血漿成分製剤文書）加算
                  IF     RECE65-SRYCD(1)    =   "130010570"
                      MOVE    5        TO  RECE641-CDKBN-KBNNUM
                  END-IF
      *           中心静脈注射（血漿成分製剤文書）加算
                  IF     RECE65-SRYCD(1)    =   "130010670"
                      MOVE    7        TO  RECE641-CDKBN-KBNNUM
                  END-IF
               ELSE
      *            薬剤
                   IF     RECE64-YKZTEN   NOT =   ZERO
                       MOVE   9        TO  RECE641-CDKBN-KBNNUM
                   END-IF
      *            器材
                   IF     RECE64-KIZAITEN   NOT =   ZERO
                       MOVE  10        TO  RECE641-CDKBN-KBNNUM
                   END-IF
      *            コメント
                   IF    (RECE65-SRYCD(1)(1:1)    =   "8")   OR
                         (RECE65-SRYCD(1)(1:3)    =   "008")
                       MOVE   999      TO  RECE641-CDKBN-KBNNUM
                   END-IF
               END-IF
           END-IF
      *
           .
       210-KBNNUM-SET-EXT.
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
               PERFORM   900-CALL-ORCSJOB-SEC
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
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
           STRING  "ORCR0250 "             DELIMITED   BY  SIZE
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
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           CLOSE   RECE64-FILE
           CLOSE   RECE641-FILE
           CLOSE   RECE65-FILE
      *
           DISPLAY "RECE64   IN  " CNT-IN
           DISPLAY "RECE641  CNT " CNT-OUT
           DISPLAY "*** ORCR0250 END ***"
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
           .
       800-FILENM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書６２読込
      *****************************************************************
       900-RECE64-READ-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-READ
      *
           PERFORM                 UNTIL   FLG-END     =   1
                                   OR      FLG-READ    =   1            
               READ    RECE64-FILE         NEXT
                   AT  END
                       MOVE    1           TO  FLG-END
                   NOT AT  END
                       IF      STS-RECE64          =   "00"
                           CONTINUE
                       ELSE
                           MOVE    SPACE           TO  WRK-ERR-AREA
                           INITIALIZE                  WRK-ERR-AREA
                           MOVE    2               TO  WRK-ERR-FLG
                           MOVE    RECE64PARA      TO  WRK-ERR-FILE-NM
                           MOVE    STS-RECE64      TO  WRK-ERR-FILE-STS
                           PERFORM 500-FILE-ERR-ABORT-SEC
                       END-IF
      *
                       IF  ( ( WRK-PARA-SYARECEKBN =   ZERO
                                                   OR  "1" )   AND
                             ( RECE64-TEISYUTUSAKI
                                                   =   "1"  )      )
                       OR  ( ( WRK-PARA-KKHRECEKBN =   ZERO
                                                   OR  "1"  )  AND
                             ( RECE64-TEISYUTUSAKI
                                                   =   "2"  )      )
                       OR  ( ( WRK-PARA-KOUIKIRECEKBN
                                                   =   ZERO
                                                   OR  "1"  )  AND
                             ( RECE64-TEISYUTUSAKI
                                                   =   "6"  )      )
                       OR  (   RECE64-RECESYUBETU  =   ZERO   OR 
      *************************************************8888   OR 
                                                       6666        )
                       OR  ( RECE64-SRYKBN         =  "95"    OR
                                                      "96"    OR
                                                      "98"         )
                               CONTINUE
                       ELSE
                           IF      RECE64-RECESYUBETU  =   8888
                               IF      WRK-PARA-PRTKBN =   "9"
                                   MOVE    1               TO  FLG-READ
                               END-IF
                           ELSE
                               IF      WRK-PARA-PRTKBN =   "9"
                                   IF    ( RECE64-JIHOKBN
                                                       NOT =   SPACE )
                                   AND   ( RECE64-ZAIKBN   =   3   OR
                                                               4     )
                                       CONTINUE
                                   ELSE
                                       MOVE    1           TO  FLG-READ
                                   END-IF
                               ELSE 
                                   MOVE    1           TO  FLG-READ
                               END-IF
                           END-IF
                       END-IF      
      *
                       IF      FLG-READ            =   1         
                           ADD     1                   TO  CNT-IN
                           MOVE    ZERO                TO  FLG-END
      *
                           DISPLAY "RECE64  PTNUM=" RECE64-PTNUM
                                    " RECESYUBETU=" RECE64-RECESYUBETU
                                    " " RECE64-JIHOKBN
                                    " " RECE64-ZAIKBN
                       END-IF
               END-READ
           END-PERFORM
           .
       900-RECE64-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SYSKANRI-INV-EXT.
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
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
               STRING  "ORCR0250 select err TABLE="
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
