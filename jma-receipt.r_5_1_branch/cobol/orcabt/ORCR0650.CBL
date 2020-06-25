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
       PROGRAM-ID.             ORCR0650.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（入院）
      *  コンポーネント名  : 公費分点数判定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/12/13    NaCl−門脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    04.03.17  ＜愛媛県地方公費対応＞
      *                                     合計点数情報（ぶん点・公費分離
      *                                     区分編集用）を使用するよう修正
      *  01.00.02    NACL-門脇    04.08.04  同一保険番号対応
      *  中止処理追加
      *  01.01.01    NACL-藤原    04/10/07  中止処理の追加
      *
      *  02.06.01    NACL-門脇    05/06/02  公費分離区分編集修正
      *  02.06.02    NACL-竹田    05/05/30  realtime preview 処理追加
      *  03.05.01    NACL-小豆沢  07/06/04  グループ診療対応
      *  04.02.01    NACL-門脇    07/05/12  第３公費以降の負担区分編集
      *  04.04.01    NACL-門脇    09/09/10  月途中に医保と生保の併用から
      *                                     生保単独になったときの対応
      *  04.04.02    NACL-門脇    10/01/04  月途中に医保と更生等の併用から
      *                                     更生等単独になったときの対応
      *
      *  04.05.01    NACL-門脇    09/12/04  食事（特別食）の他保険対応
      *  04.05.02    NACL-門脇    11/06/20  公費分離区分編集修正
      *  04.05.03    NACL-門脇    12/06/28  生保まとめ分点欄摘要欄記載対応
      *
      *  04.06.01    NACL-門脇    10/10/14  編集条件判定修正
      *
      *  04.07.01    NACL-門脇    13/09/04  自賠責（第三者行為）対応
      *  04.07.02    NACL-門脇    14/01/08  請求点数がない保険組み合わせが有る
      *                                     場合の公費分離区分編集対応
      *  04.07.03    NACL-門脇    15/03/17  請求点数０点公費レセ記載対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-門脇    16/02/08  RECE62の予備項目使用分について、
      *                                     個々に項目を設ける事に伴う修正
      *
      *  05.00.01    NACL-藤原    18/06/11  ファイルのエラー処理追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE62-FILE     ASSIGN  RECE62PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE62-KEY
                                   FILE    STATUS  IS  STS-RECE62.
           SELECT  RECE64-FILE     ASSIGN  RECE64PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE64-KEY
                                   FILE    STATUS  IS  STS-RECE64.
           SELECT  RECE65-FILE     ASSIGN  RECE65PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE65-KEY
                                   FILE    STATUS  IS  STS-RECE65.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE62-FILE.
       01  RECE62-REC. 
           COPY    "CPRCF062.INC".
      *
       FD  RECE64-FILE.
       01  RECE64-REC. 
           COPY    "CPRCF064.INC".
      *
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
      *     レセプト明細ファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE62PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE64PARA//.
      *
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
           03  STS-RECE62                              PIC X(02).
           03  STS-RECE64                              PIC X(02).
           03  STS-RECE65                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-RECE64                              PIC 9(01).
           03  FLG-RECE65                              PIC 9(01).
           03  FLG-KOHBUNRI-OKIKAE                     PIC 9(01).
           03  FLG-KOHBUNRI-OKIKAE2                    PIC 9(01).
           03  FLG-FTNKBN-ERR                          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-OUT1                                PIC 9(06).
           03  CNT-OUT2                                PIC 9(06).
           03  CNT-OUT3                                PIC 9(06).
           03  CNT-FTNKBNTBL                           PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  HC-IDX                                  PIC 9(03).
           03  KH-IDX                                  PIC 9(03).
           03  BUN-IDX                                 PIC 9(03).
           03  IDY                                     PIC 9(01).
           03  IDY2                                    PIC 9(01).
           03  IDY3                                    PIC 9(01).
           03  IDZ                                     PIC 9(03).
      *
      *    剤番号の退避
       01  WK-ZAINUM                                   PIC 9(08).
      *
      *    剤連番の退避
       01  WK-ZAIREN                                   PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-IN                                  PIC X(01).
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC X(06).
               05  WRK-PARA-TERMID                     PIC X(16).
               05  WRK-PARA-SYSYMD                     PIC X(08).
               05  WRK-PARA-JOBID                      PIC 9(07).
               05  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-HOSPNUM                        PIC 9(02).
      *
           03  WRK-HIKAKU-TABLE.
               05  WRK-HIKAKU-TBL      OCCURS   4.
                 07  WRK-HIKAKU-TOTALTEN               PIC 9(08).
                 07  WRK-HIKAKU-KOJYUN                 PIC 9(01).
      *
           03  WRK-HIKAKU-DATA                         PIC X(09).
      *
           03  WRK-3SYU-KBN.
               05  WRK-3SYU                            PIC X(03).
      *
           03  WRK-FTNKBN-AREA.
               05  WRK-FTNKBN                          PIC X(01).
               05  WRK-HKNNUM                          PIC X(03).
               05  WRK-FTNKBN-AREA-OCC                 OCCURS  4.
                 07  WRK-KOHNUM                        PIC 9(01).
           03  WRK-FTNKBN-AREA2.
               05  WRK-FTNKBN-AREA2-OCC                OCCURS 31.
                 07  WRK-FTNKBNTBL                     PIC X(01).
                 07  WRK-FTNKBNTBL-NUM                 PIC 9(02).
           03  WRK-FTNKBN-AREA3.
               05  WRK-FTNKBN-AREA3-OCC                OCCURS 10.
                 07  WRK-HKNCOMBI-FTNKBN               PIC X(01).
      *
           03  WRK-KOHBUNRI                            PIC 9(01).
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
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *     
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
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
                                       FLG-AREA
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    "RECE620"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           MOVE    "RECE640"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (3)
           MOVE    "RECE650"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (4)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE62PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE64PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE65PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ＤＢオープン処理
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0650"      TO  JOB-PGID
               MOVE    "公費分点数判定"
                                       TO  JOB-SHELLMSG
               PERFORM   900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           OPEN    I-O                 RECE62-FILE
           IF      STS-RECE62          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE62PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE62          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE64-FILE
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
           OPEN    I-O                 RECE65-FILE
           IF      STS-RECE65          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE65PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE65          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           PERFORM    900-RECE62-READ-SEC
      *
          .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM     210-HENSYU-SEC
      *
           PERFORM     900-RECE62-READ-SEC
      *
      *    処理中止設定処理
           IF      CNT-IN-CANCEL       =   50
               MOVE    ZERO                TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
           MOVE       ZERO      TO     FLG-KOHBUNRI-OKIKAE
      *
           INITIALIZE                  WRK-HIKAKU-TABLE
           MOVE       SPACE     TO     WRK-3SYU-KBN
      *
           IF   (RECE62-KOHNUM(1)    =     SPACE)
           AND  (RECE62-KOHNUM(2)    =     SPACE)
           AND  (RECE62-KOHNUM(3)    =     SPACE)
           AND  (RECE62-KOHNUM(4)    =     SPACE)
               CONTINUE
           ELSE
             IF     RECE62-HKNNUM         NOT = SPACE
               IF ((RECE62-KOHNUM(1)  NOT = SPACE                ) AND
                   (RECE62-HENTOTALTEN(1)
                                      NOT = RECE62-HENTOTALTEN(2)))
               OR ((RECE62-KOHNUM(1)  NOT = SPACE                ) AND
                   (RECE62-HENTOTALTEN(1) = RECE62-HENTOTALTEN(2)) AND
                   (RECE62-SKYTENFLG      =  1                   ))
               OR ((RECE62-KOHNUM(1)  NOT = SPACE                ) AND
                   (RECE62-HENSHOKUJIRYOYOHI(1)
                                NOT = RECE62-HENSHOKUJIRYOYOHI(2)))
               OR ((RECE62-KOHNUM(1)  NOT = SPACE                ) AND
                   (RECE62-HENSHOKUJIRYOYOHI(1)
                                    = RECE62-HENSHOKUJIRYOYOHI(2)) AND
                   (RECE62-SKYTENFLG      =  1                   ))
                   MOVE      1          TO     BUN-IDX
                   PERFORM   RECE64-HENSYU-SEC
               END-IF
               IF ((RECE62-KOHNUM(2)  NOT = SPACE                ) AND
                   (RECE62-HENTOTALTEN(1)
                                      NOT = RECE62-HENTOTALTEN(3)))
               OR ((RECE62-KOHNUM(2)  NOT = SPACE                ) AND
                   (RECE62-HENTOTALTEN(1) = RECE62-HENTOTALTEN(3)) AND
                   (RECE62-SKYTENFLG      =  1                   ))
               OR ((RECE62-KOHNUM(2)  NOT = SPACE                ) AND
                   (RECE62-HENSHOKUJIRYOYOHI(1)
                                NOT = RECE62-HENSHOKUJIRYOYOHI(3)))
               OR ((RECE62-KOHNUM(2)  NOT = SPACE                ) AND
                   (RECE62-HENSHOKUJIRYOYOHI(1)
                                    = RECE62-HENSHOKUJIRYOYOHI(3)) AND
                   (RECE62-SKYTENFLG      =  1                   ))
                   MOVE      2          TO     BUN-IDX
                   PERFORM   RECE64-HENSYU-SEC
               END-IF
               IF ((RECE62-KOHNUM(3)  NOT = SPACE                ) AND
                   (RECE62-HENTOTALTEN(1)
                                      NOT = RECE62-HENTOTALTEN(4)))
               OR ((RECE62-KOHNUM(3)  NOT = SPACE                ) AND
                   (RECE62-HENTOTALTEN(1) = RECE62-HENTOTALTEN(4)) AND
                   (RECE62-SKYTENFLG      =  1                   ))
               OR ((RECE62-KOHNUM(3)  NOT = SPACE                ) AND
                   (RECE62-HENSHOKUJIRYOYOHI(1)
                                NOT = RECE62-HENSHOKUJIRYOYOHI(4)))
               OR ((RECE62-KOHNUM(3)  NOT = SPACE                ) AND
                   (RECE62-HENSHOKUJIRYOYOHI(1)
                                    = RECE62-HENSHOKUJIRYOYOHI(4)) AND
                   (RECE62-SKYTENFLG      =  1                   ))
                   MOVE      3          TO     BUN-IDX
                   PERFORM   RECE64-HENSYU-SEC
               END-IF
               IF ((RECE62-KOHNUM(4)  NOT = SPACE                ) AND
                   (RECE62-HENTOTALTEN(1)
                                      NOT = RECE62-HENTOTALTEN(5)))
               OR ((RECE62-KOHNUM(4)  NOT = SPACE                ) AND
                   (RECE62-HENTOTALTEN(1) = RECE62-HENTOTALTEN(5)) AND
                   (RECE62-SKYTENFLG      =  1                   ))
               OR ((RECE62-KOHNUM(4)  NOT = SPACE                ) AND
                   (RECE62-HENSHOKUJIRYOYOHI(1)
                                NOT = RECE62-HENSHOKUJIRYOYOHI(5)))
               OR ((RECE62-KOHNUM(4)  NOT = SPACE                ) AND
                   (RECE62-HENSHOKUJIRYOYOHI(1)
                                    = RECE62-HENSHOKUJIRYOYOHI(5)) AND
                   (RECE62-SKYTENFLG      =  1                   ))
                   MOVE      4          TO     BUN-IDX
                   PERFORM   RECE64-HENSYU-SEC
               END-IF
             ELSE
                   PERFORM   2101-TENSU-HIKAU-SEC
             END-IF
           END-IF
      *
           IF     (FLG-KOHBUNRI-OKIKAE   =     1    )
           AND    (RECE62-SRYYM         >=  "200804")
             IF    RECE62-HKNNUM     NOT =   SPACE
                DISPLAY   "< KOHBUNRI OKIKAE >"
                PERFORM   2103-KOHBUNRI-OKIKAE-SEC
             END-IF
           END-IF
      *
           PERFORM        20014-RIHA-KOHBUNRI-HENSYU-SEC
      *
           REWRITE RECE62-REC
      *
           IF      STS-RECE62          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    4                   TO  WRK-ERR-FLG
               MOVE    RECE62PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE62          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求点数比較
      *****************************************************************
       2101-TENSU-HIKAU-SEC                SECTION.
      *
           PERFORM    VARYING    IDY    FROM   1   BY   1
                       UNTIL   ( IDY      >    4                )
                         OR    ( RECE62-KOHNUM(IDY)  =  SPACE   )
                MOVE  RECE62-HENTOTALTEN(IDY + 1)
                                    TO WRK-HIKAKU-TOTALTEN(IDY)
                MOVE  IDY           TO WRK-HIKAKU-KOJYUN(IDY)
      *
      *         生保
                IF    RECE62-KOHNUM(IDY)  =   "012"
                      MOVE   "1"    TO WRK-3SYU(1:1)
                END-IF
      *         結核
                IF    RECE62-KOHNUM(IDY)  =   "010"  OR  "011"
                      MOVE   "1"    TO WRK-3SYU(2:1)
                END-IF
                IF    RECE62-SRYYM   <   "200604"
      *           精神
                  IF  RECE62-KOHNUM(IDY)  =   "020"  OR  "021"
                      MOVE   "1"    TO WRK-3SYU(3:1)
                  END-IF
                ELSE
      *           自立支援法
                  IF  RECE62-KOHNUM(IDY)  =   "021"  OR  "015"
                                          OR  "016"  OR  "024"
                      MOVE   "1"    TO WRK-3SYU(3:1)
                  END-IF
                END-IF
           END-PERFORM
      *
           PERFORM    VARYING    IDY    FROM   1   BY   1
                       UNTIL   ( IDY      >    4                )
                         OR    ( WRK-HIKAKU-KOJYUN(IDY) = ZERO  )
      *
              COMPUTE    IDY2  =  IDY  +  1
      *
              PERFORM VARYING    IDY3   FROM  IDY2 BY   1
                        UNTIL  ( IDY3     >    4                )
                          OR   ( WRK-HIKAKU-KOJYUN(IDY3) = ZERO )
      *
                IF       WRK-HIKAKU-TBL(IDY)   <   WRK-HIKAKU-TBL(IDY3)
                   MOVE  WRK-HIKAKU-TBL(IDY)   TO  WRK-HIKAKU-DATA
                   MOVE  WRK-HIKAKU-TBL(IDY3)  TO  WRK-HIKAKU-TBL(IDY)
                   MOVE  WRK-HIKAKU-DATA       TO  WRK-HIKAKU-TBL(IDY3)
                END-IF
      *
              END-PERFORM
      *
           END-PERFORM
      *
           PERFORM    VARYING    IDY    FROM   1   BY   1
                       UNTIL   ( IDY      >    4                )
                         OR    ( WRK-HIKAKU-KOJYUN(IDY)  = ZERO )
      *
              MOVE  WRK-HIKAKU-KOJYUN(IDY)  TO   BUN-IDX
              IF   (WRK-3SYU-KBN            =   ALL "1")
              AND  (RECE62-KOHNUM(BUN-IDX)  =   "012"  )
                   CONTINUE
              ELSE
                   PERFORM   RECE64-HENSYU-SEC
              END-IF
      *
           END-PERFORM
      *
           .
       2101-TENSU-HIKAU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの作成（Ｆ００４）
      *****************************************************************
       RECE64-HENSYU-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE64-REC
           INITIALIZE                           RECE64-REC
           MOVE    RECE62-KEY           TO      RECE64-KEY1
           START   RECE64-FILE   KEY  IS  >=    RECE64-KEY
                   INVALID
                       MOVE   1         TO      FLG-RECE64
                   NOT  INVALID
                       MOVE   ZERO      TO      FLG-RECE64
                       PERFORM   900-RECE64-NEXT-SEC
           END-START
           PERFORM   UNTIL
                    (RECE62-KEY       NOT = RECE64-KEY1 )     OR
                    (FLG-RECE64       =    1)
                     MOVE   RECE64-ZAINUM    TO   WK-ZAINUM
                     MOVE   RECE64-ZAIREN    TO   WK-ZAIREN
                     PERFORM      KOHBUNRI-SET-SEC
                     PERFORM      RECE64-REWRITE-SEC
                     PERFORM      RECE65-HENSYU-SEC
                     PERFORM      900-RECE64-NEXT-SEC
           END-PERFORM
      *
           .
       RECE64-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費分離区分の算出
      *****************************************************************
       KOHBUNRI-SET-SEC               SECTION.
      *
           MOVE    ZERO                TO  HC-IDX
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
                   OR      RECE62-HKNCOMBI (IDX)   =   ZERO 
               IF      RECE64-HKNCOMBI     =   RECE62-HKNCOMBI (IDX)
                     MOVE     IDX              TO  HC-IDX
                     MOVE    10                TO  IDX
               END-IF
           END-PERFORM          
      *
           MOVE       ZERO              TO   KH-IDX
           IF ((RECE62-KOHNUMHC(HC-IDX 1) = RECE62-KOHNUM(BUN-IDX)) AND
               (RECE62-KOHIDHC(HC-IDX 1)  = RECE62-KOHID(BUN-IDX) ) AND
               (RECE62-KOHNUM(BUN-IDX)    NOT = SPACE             ))
           OR ((RECE62-KOHNUMHC(HC-IDX 2) = RECE62-KOHNUM(BUN-IDX)) AND
               (RECE62-KOHIDHC(HC-IDX 2)  = RECE62-KOHID(BUN-IDX) ) AND
               (RECE62-KOHNUM(BUN-IDX)    NOT = SPACE             ))
           OR ((RECE62-KOHNUMHC(HC-IDX 3) = RECE62-KOHNUM(BUN-IDX)) AND
               (RECE62-KOHIDHC(HC-IDX 3)  = RECE62-KOHID(BUN-IDX) ) AND
               (RECE62-KOHNUM(BUN-IDX)    NOT = SPACE             ))
           OR ((RECE62-KOHNUMHC(HC-IDX 4) = RECE62-KOHNUM(BUN-IDX)) AND
               (RECE62-KOHIDHC(HC-IDX 4)  = RECE62-KOHID(BUN-IDX) ) AND
               (RECE62-KOHNUM(BUN-IDX)    NOT = SPACE             ))
                MOVE    BUN-IDX     TO        KH-IDX
           END-IF
      *
           IF    KH-IDX      NOT =   ZERO
                IF  ( RECE64-KOHBUNRI  NOT =  ZERO )
                OR  ( RECE62-SEIHOFLG      =  "1"  )
                   MOVE    1        TO     FLG-KOHBUNRI-OKIKAE
                END-IF
                MOVE    KH-IDX      TO     RECE64-KOHBUNRI
           END-IF
      *
           .
       KOHBUNRI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの作成（Ｆ００５）
      *****************************************************************
       RECE65-HENSYU-SEC               SECTION.
      *
           MOVE    SPACE                TO      RECE65-REC
           INITIALIZE                           RECE65-REC
           MOVE    RECE62-KEY           TO      RECE65-KEY1
           MOVE    WK-ZAINUM            TO      RECE65-ZAINUM
           MOVE    WK-ZAIREN            TO      RECE65-ZAIREN
           START   RECE65-FILE   KEY  IS  >=    RECE65-KEY
                   INVALID
                       MOVE    1        TO      FLG-RECE65
                   NOT  INVALID
                       MOVE    ZERO     TO      FLG-RECE65
                       PERFORM   900-RECE65-NEXT-SEC
           END-START
           PERFORM   UNTIL
                  (RECE62-KEY           NOT =  RECE65-KEY1)   OR
                  (WK-ZAINUM            NOT =  RECE65-ZAINUM)    OR
                  (WK-ZAIREN            NOT =  RECE65-ZAIREN)    OR
                  (FLG-RECE65           =      1)
      *
                   IF    KH-IDX      NOT =   ZERO
                      MOVE   KH-IDX        TO     RECE65-KOHBUNRI
                      PERFORM      RECE65-REWRITE-SEC
                   END-IF
      *
                   PERFORM      900-RECE65-NEXT-SEC
           END-PERFORM
      *
           .
       RECE65-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理（Ｆ００４）
      *****************************************************************
       RECE64-REWRITE-SEC                SECTION.
      *
           REWRITE   RECE64-REC
      *
           IF      STS-RECE64          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    4                   TO  WRK-ERR-FLG
               MOVE    RECE64PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE64          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-OUT1
      *
           .
       RECE64-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理（Ｆ００５）
      *****************************************************************
       RECE65-REWRITE-SEC                SECTION.
      *
           REWRITE   RECE65-REC
      *
           IF      STS-RECE65          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    4                   TO  WRK-ERR-FLG
               MOVE    RECE65PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE65          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
      *
           ADD     1                   TO  CNT-OUT2
      *
           .
       RECE65-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費分離区分の置き換え
      *****************************************************************
       2103-KOHBUNRI-OKIKAE-SEC                SECTION.
      *
           INITIALIZE                   WRK-FTNKBN-AREA2
           INITIALIZE                   WRK-FTNKBN-AREA3
      *
           MOVE    ZERO                 TO      FLG-FTNKBN-ERR
      *
           MOVE    SPACE                TO      RECE64-REC
           INITIALIZE                           RECE64-REC
           MOVE    RECE62-KEY           TO      RECE64-KEY1
           START   RECE64-FILE   KEY  IS  >=    RECE64-KEY
                   INVALID
                       MOVE   1         TO      FLG-RECE64
                   NOT  INVALID
                       MOVE   ZERO      TO      FLG-RECE64
                       PERFORM    900-RECE64-NEXT-SEC
           END-START
           PERFORM   UNTIL
                    (RECE62-KEY      NOT =    RECE64-KEY1 )    OR
                    (FLG-RECE64          =    1           )
                     PERFORM      21031-FTNKBN-SET-SEC
                     PERFORM      900-RECE64-NEXT-SEC
           END-PERFORM
      *
           MOVE     ZERO     TO   CNT-FTNKBNTBL
           PERFORM  VARYING  IDX  FROM   1   BY   1
                     UNTIL   IDX   >    31
              IF   WRK-FTNKBNTBL(IDX)  NOT =  SPACE
                 ADD    1               TO   CNT-FTNKBNTBL
                 MOVE   CNT-FTNKBNTBL   TO   WRK-FTNKBNTBL-NUM(IDX)
              END-IF
           END-PERFORM
      *
           IF    FLG-FTNKBN-ERR   =    ZERO
             IF    CNT-FTNKBNTBL   >    5
                DISPLAY  "## CNT OVER ##  OKIKAE NG"
             ELSE
                PERFORM   21032-RECE64-HENSYU2-SEC
             END-IF
           END-IF
      *
           .
       2103-KOHBUNRI-OKIKAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担区分編集
      *****************************************************************
       21031-FTNKBN-SET-SEC               SECTION.
      *
           INITIALIZE                   WRK-FTNKBN-AREA
      *
           MOVE       ZERO        TO        IDZ
      *
           PERFORM  VARYING  IDX  FROM   1   BY   1
                     UNTIL   IDX   >    10
                      OR     RECE62-HKNCOMBI(IDX)  =  ZERO
               IF   RECE64-HKNCOMBI   =   RECE62-HKNCOMBI(IDX)
                  MOVE    RECE62-HKNNUMHC(IDX)  TO   WRK-HKNNUM
                  MOVE    IDX                   TO   IDZ
                  PERFORM  VARYING  IDY  FROM   1   BY   1
                            UNTIL   IDY   >     4
                     IF  (RECE62-KOHNUMHC (IDX IDY)  NOT =  SPACE)
      ***            AND (RECE62-FTNKBNFLG(IDX IDY)      =  ZERO )
                        PERFORM  VARYING  IDY2  FROM   1   BY   1
                                  UNTIL   IDY2   >     4
                                   OR     RECE62-KOHNUM(IDY2) = SPACE
                          IF  (RECE62-KOHNUMHC(IDX IDY) =
                                          RECE62-KOHNUM(IDY2))
                          AND (RECE62-KOHIDHC (IDX IDY) =
                                          RECE62-KOHID (IDY2))
                              MOVE    1     TO   WRK-KOHNUM(IDY2)
                              MOVE    4     TO   IDY2
                          END-IF
                        END-PERFORM
                     END-IF
                  END-PERFORM
                  MOVE    10                TO   IDX
               END-IF
           END-PERFORM
      *
           IF     IDZ           NOT =    ZERO
             IF   WRK-HKNNUM    NOT =    SPACE
               EVALUATE    WRK-KOHNUM(1)
                               ALSO    WRK-KOHNUM(2)
                                           ALSO    WRK-KOHNUM(3)
                                                   ALSO    WRK-KOHNUM(4)
      *            １者 
                   WHEN    0   ALSO    0   ALSO    0   ALSO    0
                       MOVE    "1"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(1)
      *            ２者 
                   WHEN    1   ALSO    0   ALSO    0   ALSO    0
                       MOVE    "2"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(2)
                   WHEN    0   ALSO    1   ALSO    0   ALSO    0
                       MOVE    "3"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(3)
                   WHEN    0   ALSO    0   ALSO    1   ALSO    0
                       MOVE    "E"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(4)
                   WHEN    0   ALSO    0   ALSO    0   ALSO    1
                       MOVE    "G"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(5)
      *            ３者 
                   WHEN    1   ALSO    1   ALSO    0   ALSO    0
                       MOVE    "4"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(6)
                   WHEN    1   ALSO    0   ALSO    1   ALSO    0
                       MOVE    "M"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(7)
                   WHEN    1   ALSO    0   ALSO    0   ALSO    1
                       MOVE    "N"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(8)
                   WHEN    0   ALSO    1   ALSO    1   ALSO    0
                       MOVE    "O"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(9)
                   WHEN    0   ALSO    1   ALSO    0   ALSO    1
                       MOVE    "P"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(10)
                   WHEN    0   ALSO    0   ALSO    1   ALSO    1
                       MOVE    "Q"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(11)
      *            ４者 
                   WHEN    1   ALSO    1   ALSO    1   ALSO    0
                       MOVE    "V"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(12)
                   WHEN    1   ALSO    1   ALSO    0   ALSO    1
                       MOVE    "W"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(13)
                   WHEN    1   ALSO    0   ALSO    1   ALSO    1
                       MOVE    "X"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(14)
                   WHEN    0   ALSO    1   ALSO    1   ALSO    1
                       MOVE    "Y"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(15)
      *            ５者 
                   WHEN    1   ALSO    1   ALSO    1   ALSO    1
                       MOVE    "9"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(16)
               END-EVALUATE
             ELSE
               EVALUATE    WRK-KOHNUM(1)
                               ALSO    WRK-KOHNUM(2)
                                           ALSO    WRK-KOHNUM(3)
                                                   ALSO    WRK-KOHNUM(4)
      *            １者 
                   WHEN    1   ALSO    0   ALSO    0   ALSO    0
                       MOVE    "5"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(17)
                   WHEN    0   ALSO    1   ALSO    0   ALSO    0
                       MOVE    "6"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(18)
                   WHEN    0   ALSO    0   ALSO    1   ALSO    0
                       MOVE    "B"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(19)
                   WHEN    0   ALSO    0   ALSO    0   ALSO    1
                       MOVE    "C"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(20)
      *            ２者 
                   WHEN    1   ALSO    1   ALSO    0   ALSO    0
                       MOVE    "7"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(21)
                   WHEN    1   ALSO    0   ALSO    1   ALSO    0
                       MOVE    "H"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(22)
                   WHEN    1   ALSO    0   ALSO    0   ALSO    1
                       MOVE    "I"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(23)
                   WHEN    0   ALSO    1   ALSO    1   ALSO    0
                       MOVE    "J"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(24)
                   WHEN    0   ALSO    1   ALSO    0   ALSO    1
                       MOVE    "K"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(25)
                   WHEN    0   ALSO    0   ALSO    1   ALSO    1
                       MOVE    "L"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(26)
      *            ３者 
                   WHEN    1   ALSO    1   ALSO    1   ALSO    0
                       MOVE    "R"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(27)
                   WHEN    1   ALSO    1   ALSO    0   ALSO    1
                       MOVE    "S"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(28)
                   WHEN    1   ALSO    0   ALSO    1   ALSO    1
                       MOVE    "T"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(29)
                   WHEN    0   ALSO    1   ALSO    1   ALSO    1
                       MOVE    "U"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(30)
      *            ４者 
                   WHEN    1   ALSO    1   ALSO    1   ALSO    1
                       MOVE    "Z"            TO  WRK-FTNKBN
                                                  WRK-FTNKBNTBL(31)
               END-EVALUATE
             END-IF
             MOVE      WRK-FTNKBN     TO    WRK-HKNCOMBI-FTNKBN(IDZ)
           ELSE
             DISPLAY  "## HKNCOMBI ERR ##  OKIKAE NG"
             MOVE         1           TO    FLG-FTNKBN-ERR
           END-IF
      *
           .
       21031-FTNKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの作成（Ｆ００４）
      *****************************************************************
       21032-RECE64-HENSYU2-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE64-REC
           INITIALIZE                           RECE64-REC
           MOVE    RECE62-KEY           TO      RECE64-KEY1
           START   RECE64-FILE   KEY  IS  >=    RECE64-KEY
                   INVALID
                       MOVE   1         TO      FLG-RECE64
                   NOT  INVALID
                       MOVE   ZERO      TO      FLG-RECE64
                       PERFORM    900-RECE64-NEXT-SEC
           END-START
           PERFORM   UNTIL
                    (RECE62-KEY      NOT =    RECE64-KEY1 )    OR
                    (FLG-RECE64          =    1           )
                     MOVE   RECE64-ZAINUM    TO   WK-ZAINUM
                     MOVE   RECE64-ZAIREN    TO   WK-ZAIREN
                     PERFORM      210321-KOHBUNRI-SET2-SEC
                     PERFORM      RECE64-REWRITE-SEC
                     PERFORM      210322-RECE65-HENSYU2-SEC
                     PERFORM      900-RECE64-NEXT-SEC
           END-PERFORM
      *
           .
       21032-RECE64-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費分離区分の算出
      *****************************************************************
       210321-KOHBUNRI-SET2-SEC               SECTION.
      *
           MOVE       ZERO        TO   FLG-KOHBUNRI-OKIKAE2
      *
           MOVE       ZERO        TO        IDZ
           PERFORM  VARYING  IDX  FROM   1   BY   1
                     UNTIL   IDX   >    10
                      OR     RECE62-HKNCOMBI(IDX)  =  ZERO
               IF   RECE64-HKNCOMBI   =   RECE62-HKNCOMBI(IDX)
                  MOVE    IDX               TO   IDZ
                  MOVE    10                TO   IDX
               END-IF
           END-PERFORM
           IF     IDZ    NOT =    ZERO
               PERFORM  VARYING  IDX  FROM   1   BY   1
                         UNTIL   IDX   >    31
                   IF   WRK-HKNCOMBI-FTNKBN(IDZ)  =  WRK-FTNKBNTBL(IDX)
                     COMPUTE  WRK-KOHBUNRI  =  WRK-FTNKBNTBL-NUM(IDX)
                                            -  1
                     MOVE     WRK-KOHBUNRI  TO   RECE64-KOHBUNRI
                     MOVE     1             TO   FLG-KOHBUNRI-OKIKAE2
                     MOVE     1             TO   RECE62-KOHBUNRICHIKAN
                     MOVE     31            TO   IDX
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       210321-KOHBUNRI-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの作成（Ｆ００５）
      *****************************************************************
       210322-RECE65-HENSYU2-SEC               SECTION.
      *
           MOVE    SPACE                TO      RECE65-REC
           INITIALIZE                           RECE65-REC
           MOVE    RECE62-KEY           TO      RECE65-KEY1
           MOVE    WK-ZAINUM            TO      RECE65-ZAINUM
           MOVE    WK-ZAIREN            TO      RECE65-ZAIREN
           START   RECE65-FILE   KEY  IS  >=    RECE65-KEY
                   INVALID
                       MOVE    1        TO      FLG-RECE65
                   NOT  INVALID
                       MOVE    ZERO     TO      FLG-RECE65
                       PERFORM    900-RECE65-NEXT-SEC
           END-START
           PERFORM   UNTIL
                  (RECE62-KEY           NOT =  RECE65-KEY1)      OR
                  (WK-ZAINUM            NOT =  RECE65-ZAINUM)    OR
                  (WK-ZAIREN            NOT =  RECE65-ZAIREN)    OR
                  (FLG-RECE65           =      1)
                   IF    FLG-KOHBUNRI-OKIKAE2  =   1
                      MOVE     WRK-KOHBUNRI   TO   RECE65-KOHBUNRI
                      PERFORM      RECE65-REWRITE-SEC
                   END-IF
                   PERFORM       900-RECE65-NEXT-SEC
           END-PERFORM
      *
           .
       210322-RECE65-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ開始日編集用公費分離区分編集
      *****************************************************************
       20014-RIHA-KOHBUNRI-HENSYU-SEC                SECTION.
      *
           MOVE    SPACE          TO        RECE64-REC
           INITIALIZE                       RECE64-REC
           MOVE    RECE62-KEY     TO        RECE64-KEY1
           START   RECE64-FILE  KEY IS  >=  RECE64-KEY
             INVALID
               MOVE     1         TO        FLG-RECE64
             NOT  INVALID
               MOVE     ZERO      TO        FLG-RECE64
               PERFORM    900-RECE64-NEXT-SEC
           END-START
           PERFORM   UNTIL
               (RECE62-KEY     NOT =   RECE64-KEY1 ) OR
               (FLG-RECE64         =   1           )
             PERFORM  VARYING  IDX  FROM   1   BY   1
                       UNTIL  (IDX                   >   10 )
                        OR    (RECE62-HKNCOMBI(IDX)  =  ZERO)
               IF   RECE64-HKNCOMBI  =  RECE62-HKNCOMBI(IDX)
                  MOVE  RECE64-KOHBUNRI  TO  RECE62-RIHA-KOHBUNRI(IDX)
                  MOVE    10             TO  IDX
               END-IF
             END-PERFORM
             PERFORM      900-RECE64-NEXT-SEC
           END-PERFORM
      *
           .
       20014-RIHA-KOHBUNRI-HENSYU-EXT.
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
                   PERFORM   900-CALL-ORCSJOB-SEC
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
           STRING  "ORCR0650 "             DELIMITED   BY  SIZE
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
               PERFORM   900-CALL-ORCSJOB-SEC
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
                       PERFORM   900-CALL-ORCSJOB-SEC
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
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               PERFORM   900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           CLOSE   RECE62-FILE
                   RECE64-FILE
                   RECE65-FILE
      *
           DISPLAY "RECE62-IN  :" CNT-IN   
           DISPLAY "RECE64-MOD :" CNT-OUT1  
           DISPLAY "RECE65-MOD :" CNT-OUT2 
           DISPLAY "*** ORCR0650 END ***"
      *
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
      *    中間ファイルＲＥＡＤ処理（Ｆ００２）
      *****************************************************************
       900-RECE62-READ-SEC            SECTION.
      *
           READ    RECE62-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   ADD     1     TO    CNT-IN
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE62  PTNUM=" RECE62-PTNUM
      *
           END-READ
           .
       900-RECE62-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE64-NEXT-SEC            SECTION.
      *
           MOVE    ZERO          TO    FLG-RECE64
           READ    RECE64-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-RECE64
           END-READ
           .
       900-RECE64-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００５）
      *****************************************************************
       900-RECE65-NEXT-SEC            SECTION.
      *
           MOVE    ZERO          TO    FLG-RECE65
           READ    RECE65-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-RECE65
           END-READ
           .
       900-RECE65-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
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
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
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
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
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
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
