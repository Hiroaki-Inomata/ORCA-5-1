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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCXSMST1.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : マスタ更新処理
      *  コンポーネント名  : ＤＢ構造更新ファイル作成処理
      *                    : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/02/22    MCC-多々納    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-伊藤     02.05.20  クリーンインストールでオリ
      *                                     ジナルダンプが複数ある場合
      *                                     ０件となってしまう
      *  01.00.01    NACL-多々納  02/09/18  バージョン判定変更
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    入力：ＤＢ構造ファイル
           SELECT  IN-FILE         ASSIGN
                                   ASS-IN
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-IN.
      *    出力：更新ファイル
           SELECT  OUT-FILE        ASSIGN
                                   ASS-OUT
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-OUT.
      *    出力：テーブルファイル
           SELECT  TBL-FILE        ASSIGN
                                   ASS-TBL
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-TBL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    入力ファイル
       FD  IN-FILE.
       01  IN-REC.
           03  IN-DBSVERSION           PIC X(21).
           03  IN-DBRNAME              PIC X(60).
           03  IN-DBSVERSION2          PIC X(21).
           03  IN-DATASYU              PIC X(06).
           03  IN-UPFILE               PIC X(100).
           03  IN-TBLNAME              PIC X(20).
      *
      *    更新ファイル
       FD  OUT-FILE.
       01  OUT-REC.
           03  OUT-DBSVERSION          PIC X(21).
           03  OUT-DATASYU             PIC X(06).
           03  OUT-SYORIFLG            PIC X(07).
           03  OUT-UPFILE              PIC X(100).
      *
      *    出力：テーブルファイル
       FD  TBL-FILE.
       01  TBL-REC.
           03  TBL-TBLNAME             PIC X(20).
      *
       WORKING-STORAGE             SECTION.
      *
      *    ファイル指定領域
       01  ASS-AREA.
           03  ASS-IN                  PIC X(256).
           03  ASS-OUT                 PIC X(256).
           03  ASS-TBL                 PIC X(256).
           03  ASS-PLF                 PIC X(256).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-IN                  PIC X(02).
           03  STS-OUT                 PIC X(02).
           03  STS-TBL                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-IN                  PIC 9(01).
           03  FLG-DBKANRI             PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-INS                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PARA                PIC 9(06).
           03  CNT-IN                  PIC 9(06).
           03  CNT-OUT                 PIC 9(06).
           03  CNT-TBL                 PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDX1                    PIC 9(04).
           03  IDX2                    PIC 9(04).
           03  IDY                     PIC 9(04).
      *
           03  IDT                     PIC 9(04).
      *
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYMD2.
               05  WRK-SYY2            PIC 9(02).
               05  WRK-SMM2            PIC 9(02).
               05  WRK-SDD2            PIC 9(02).
      *
           03  WRK-DBFILE              PIC X(20).
           03  WRK-IN                  PIC X(64).
           03  WRK-SYORI               PIC 9(04).
           03  WRK-ST-IDX              PIC 9(04).
      *
           03  WRK-DBSVERSION          PIC X(21).
           03  WRK-DBSVERSION2         PIC X(21).
           03  WRK-MAX                 PIC 9(04). 
      *
           03  WRK-INACT               PIC X(01). 
      *
           03  WRK-DBKANRI-VERSION     PIC X(64).
      *
      *    引数
       01  ACCT-AREA.
           03  ACCT-REC                PIC X(200).
      *    入力ファイルテーブル
       01  WRK-IN-REC.
           03  WRK-IN-DBSVERSION           PIC X(21).
           03  WRK-IN-DBRNAME              PIC X(60).
           03  WRK-IN-DBSVERSION2          PIC X(21).
           03  WRK-IN-DATASYU              PIC X(06).
           03  WRK-IN-UPFILE               PIC X(100).
           03  WRK-IN-TBLNAME              PIC X(20).
      *
      *    テーブル名
       01  WRK-TBL-AREA.
           03  WRK-TBL-REC                 OCCURS  100.
               05  WRK-TBL-TBLNAME             PIC X(20).
      *
      *
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    ＤＢ管理情報
       01  DBKANRI-REC.
           COPY    "CPDBKANRI.INC".
      *
      *
       PROCEDURE                       DIVISION.
      ****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-IN      =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
      *
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    ZERO            TO  FLG-AREA
           MOVE    ZERO            TO  CNT-AREA
           MOVE    ZERO            TO  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ACCT-AREA
      *
      *    パラメタ取り込み
           ACCEPT  ACCT-AREA           FROM    COMMAND-LINE
           PERFORM 1001-ACCT-HEN-SEC
      *
      *    ファイル情報
           PERFORM 101-ASSIN-GET-SEC
      *
      *    入力　初期処理
           OPEN    INPUT           IN-FILE
           IF      STS-IN          NOT =   ZERO
               MOVE    1           TO  FLG-IN
               DISPLAY "*(ORCXSMST1)* IN-FILE OPN STS["
                                       STS-IN "]"
           END-IF
      *
      *    出力ファイル　初期処理
           OPEN    OUTPUT          OUT-FILE
           IF      STS-OUT         NOT =   ZERO
               DISPLAY "*(ORCXSMST1)* OUT-FILE OPN STS["
                                       STS-OUT "]"
           END-IF
      *
      *    出力ファイル　初期処理
           OPEN    OUTPUT          TBL-FILE
           IF      STS-TBL         NOT =   ZERO
               DISPLAY "*(ORCXSMST1)* TBL-FILE OPN STS["
                                       STS-TBL "]"
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       1001-ACCT-HEN-SEC                SECTION.
      *
      *
           INITIALIZE                  DBKANRI-REC
           MOVE    SPACE               TO  WRK-DBFILE
           MOVE    SPACE               TO  WRK-IN
           MOVE    1                   TO  WRK-SYORI
           MOVE    1                   TO  WRK-ST-IDX
      *    OpenCOBOL version check
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   200     )
               IF      ACCT-REC(IDX:9)     =   "ORCXSMST1"
      *            for OpenCOBOL 0.8.9
                   COMPUTE WRK-ST-IDX          =   IDX  +  10
                   MOVE    200                 TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  IDX1
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    WRK-ST-IDX  BY  1
                   UNTIL      (IDX     >   200     )  OR
                              (IDY     >   7       )
              IF      WRK-SYORI            =   1
                   MOVE    ACCT-REC(IDX:13)    TO  WRK-DBFILE
                   ADD     1                   TO  WRK-SYORI
                   ADD     12                  TO  IDX
               ELSE
                 IF      WRK-SYORI            >   1
                   IF      ACCT-REC(IDX:1)     =   "|"
                       ADD     1               TO  IDY
                       PERFORM 10011-DBKANRI-SET-SEC
                       MOVE    ZERO            TO  IDX2
                       MOVE    SPACE           TO  WRK-IN
                   ELSE
                       ADD     1               TO  IDX2
                       MOVE    ACCT-REC(IDX:1) TO  WRK-IN(IDX2:1)
                   END-IF
                 END-IF
               END-IF
           END-PERFORM
      *
           .
       1001-ACCT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル情報取得
      *****************************************************************
       10011-DBKANRI-SET-SEC                SECTION.
      *
           EVALUATE    IDY
      *管理コード
               WHEN    1
                   MOVE    WRK-IN          TO   DBKANRI-KANRICD
      *バージョン
               WHEN    2
                   MOVE    WRK-IN          TO   DBKANRI-VERSION
      *ＤＢ構造バージョン１
               WHEN    3
                   MOVE    WRK-IN          TO   DBKANRI-DBSVERSION1
      *ＤＢ構造バージョン２
               WHEN    4
                   MOVE    WRK-IN          TO   DBKANRI-DBSVERSION2
      *端末ＩＤ
               WHEN    7
                   MOVE    WRK-IN          TO   DBKANRI-TERMID
      *オペーレータＩＤ
               WHEN    8
                   MOVE    WRK-IN          TO   DBKANRI-OPID
      *作成年月日
               WHEN    9
                   MOVE    WRK-IN          TO   DBKANRI-CREYMD
      *更新年月日
               WHEN    10
                   MOVE    WRK-IN          TO   DBKANRI-UPYMD
      *更新時間
               WHEN    11
                   MOVE    WRK-IN          TO   DBKANRI-UPHMS
           END-EVALUATE
           .
       10011-DBKANRI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル情報取得
      *****************************************************************
       101-ASSIN-GET-SEC                SECTION.
      *
      *    入力ファイル
           MOVE    SPACE               TO  ASS-IN
           STRING  "/var/tmp/"         DELIMITED  BY  SIZE
                   WRK-DBFILE          DELIMITED  BY  SPACE
                   "/ORCADBS.DAT"      DELIMITED  BY  SIZE
                                       INTO    ASS-IN
           END-STRING
      *
      *    出力ーファイル
           MOVE    SPACE               TO  ASS-OUT
           STRING  "/var/tmp/"         DELIMITED  BY  SIZE
                   WRK-DBFILE          DELIMITED  BY  SPACE
                   "/ORCADBS.OUT"      DELIMITED  BY  SIZE
                                       INTO    ASS-OUT
           END-STRING
      *
      *    出力ーファイル
           MOVE    SPACE               TO  ASS-TBL
           STRING  "/var/tmp/"         DELIMITED  BY  SIZE
                   WRK-DBFILE          DELIMITED  BY  SPACE
                   "/ORCADBS.TBL"      DELIMITED  BY  SIZE
                                       INTO    ASS-TBL
           END-STRING
      *
           .
       101-ASSIN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DBKANRI-VERSION
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   64
               IF      (DBKANRI-VERSION(IDX:1)   NOT =   SPACE)
                   ADD     1               TO  IDY
                   MOVE    DBKANRI-VERSION(IDX:1)
                                       TO  WRK-DBKANRI-VERSION(IDY:1)
               END-IF
          END-PERFORM
      *
           MOVE    ZERO                TO  FLG-SYORI
      *    クリーンインストール
           IF      DBKANRI-DBSVERSION1 =   "S-000000-0-00000000-0"
               MOVE    1                   TO  FLG-INS
           ELSE
               MOVE    ZERO                TO  FLG-INS
           END-IF
           MOVE    SPACE               TO  WRK-DBSVERSION
           MOVE    SPACE               TO  WRK-DBSVERSION2
           MOVE    SPACE               TO  WRK-IN-REC
           MOVE    SPACE               TO  WRK-TBL-AREA
           MOVE    ZERO                TO  IDT
      *    レコード管理ファイル読込み
           PERFORM 900-IN-READ-SEC
           PERFORM
               UNTIL    FLG-IN         =   1
      *        入力件数
               ADD     1               TO  CNT-IN
      *
               IF      FLG-INS         =   ZERO
      *        アップグレード処理
                   IF     (FLG-SYORI           =   1   )  OR
                          (IN-DBSVERSION       >   DBKANRI-DBSVERSION1)
                       MOVE    1               TO  FLG-SYORI
                       IF      IN-DBSVERSION(3:IDY)
                                               >   WRK-DBKANRI-VERSION
                                                       (1:IDY)
                           MOVE    1               TO  FLG-IN
                       ELSE
                           PERFORM 210-OUTFILE-SEC
                       END-IF
                   ELSE
                       IF      IN-DBSVERSION   =   DBKANRI-DBSVERSION1
                           MOVE    1               TO  FLG-SYORI
                       END-IF
                   END-IF
               ELSE
      *        クリーンインストール
                   IF      IN-DBSVERSION(3:IDY)
                                               >   WRK-DBKANRI-VERSION
                                                       (1:IDY)
                       MOVE    1               TO  FLG-IN
                   ELSE
                       MOVE    IN-DBSVERSION2  TO  WRK-DBSVERSION2
                       MOVE    IN-REC          TO  WRK-IN-REC
                   END-IF
               END-IF
      *
               IF      FLG-IN              =   ZERO
      *            レコード管理ファイル読込み
                   PERFORM 900-IN-READ-SEC
               END-IF
           END-PERFORM
      *
      *     アップグレード処理終了
           IF      FLG-INS             =   ZERO
               GO      TO      200-MAIN-EXT
           END-IF
      *
           IF      WRK-IN-REC          =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
      *    クリーンインストールの時、最後の対象データのバージョンが空白
           IF      WRK-DBSVERSION2     =   SPACE
               MOVE    WRK-IN-REC          TO  IN-REC
               PERFORM 210-OUTFILE-SEC
               GO      TO      200-MAIN-EXT
           END-IF
      *
           CLOSE                       IN-FILE
           MOVE    ZERO                TO  FLG-IN
           OPEN    INPUT               IN-FILE
      *
      *    レコード管理ファイル読込み
           PERFORM 900-IN-READ-SEC
           PERFORM
               UNTIL    FLG-IN         =   1
      *
               IF      IN-DBSVERSION(3:IDY)    >   WRK-DBKANRI-VERSION
                                                       (1:IDY)
                   MOVE    1               TO  FLG-IN
               ELSE
      *
                   IF      WRK-DBSVERSION2     <=  IN-DBSVERSION
                       PERFORM 210-OUTFILE-SEC
                   END-IF
      *            レコード管理ファイル読込み
                   PERFORM 900-IN-READ-SEC
               END-IF
           END-PERFORM
      *
      *    テーブルファイル出力
           PERFORM 220-TBLFILE-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新ファイル出力  処理
      *****************************************************************
       210-OUTFILE-SEC               SECTION.
      *
           INITIALIZE                  OUT-REC
           MOVE    IN-DBSVERSION       TO  OUT-DBSVERSION
           MOVE    IN-DATASYU          TO  OUT-DATASYU
           MOVE    "upgrade"           TO  OUT-SYORIFLG
           MOVE    IN-UPFILE           TO  OUT-UPFILE
           IF      FLG-INS             =   ZERO
               IF      IN-DBSVERSION2      =   SPACE
                   MOVE    "dbvonly"           TO  OUT-SYORIFLG
                   MOVE    ALL "X"             TO  OUT-UPFILE
               END-IF
           END-IF
      *
           WRITE                       OUT-REC
           ADD     1                   TO  CNT-OUT
      *
      *    バックアップテーブル名保存
           IF      IN-TBLNAME      NOT =   SPACE
      *      オリジナルバージョンの時、オリジナルダンプのみ
             IF     (FLG-INS             =   1      )  AND
                    (IN-DBSVERSION2  NOT =   SPACE  )
                 CONTINUE
             ELSE
               PERFORM VARYING     IDT     FROM    1   BY  1
                       UNTIL       IDT     >   100
                   IF      WRK-TBL-TBLNAME(IDT)    =   SPACE
                       MOVE    IN-TBLNAME      TO  WRK-TBL-TBLNAME(IDT)
                       MOVE    100             TO  IDT
                   ELSE
                       IF      WRK-TBL-TBLNAME(IDT)    =   IN-TBLNAME
                           MOVE    100             TO  IDT
                       END-IF
                   END-IF
               END-PERFORM
             END-IF
           END-IF
      *
           .
       210-OUTFILE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルファイル出力  処理
      *****************************************************************
       220-TBLFILE-SEC               SECTION.
      *
           PERFORM VARYING     IDT     FROM    1   BY  1
                   UNTIL      (IDT     >   100   )  OR
                              (WRK-TBL-REC (IDT)   =   SPACE)
               INITIALIZE                  TBL-REC
               MOVE    WRK-TBL-TBLNAME (IDT)   TO  TBL-TBLNAME
      *
               WRITE                       TBL-REC
               ADD     1                   TO  CNT-TBL
           END-PERFORM
      *
           .
       220-TBLFILE-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *    出力ファイル　終了処理
           CLOSE                       IN-FILE
           CLOSE                       OUT-FILE
           CLOSE                       TBL-FILE
      *
           DISPLAY "*(ORCXSMST1)* INPUT  /I   CNT[" CNT-IN  "]"
           DISPLAY "*(ORCXSMST1)* OUTPUT /O   CNT[" CNT-OUT "]"
           DISPLAY "*(ORCXSMST1)*        /TBL CNT[" CNT-TBL "]"
      *****ACCEPT  WRK-INACT
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力ファイル読込み処理
      *****************************************************************
       900-IN-READ-SEC          SECTION.
      *
          READ    IN-FILE
              AT  END
                  MOVE    1               TO  FLG-IN
              NOT AT  END
                  MOVE    ZERO            TO  FLG-IN
          END-READ
      *
          .
       900-IN-READ-EXT.
          EXIT.
      *
