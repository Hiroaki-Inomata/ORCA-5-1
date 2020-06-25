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
       PROGRAM-ID.             ORCBN21.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 薬情マスタ一覧印刷
      *  コンポーネント名  : 薬情マスタ一覧印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/05/15    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  00.00.01    NACL-森脇    06/09/28  条件なしの処理
      *  03.05.01    NACL-森脇    07/05/30  グループ診療対応
      *  04.01.01    NACL-森脇    08/01/16  文字数増
      *  04.08.01    NACL-森脇    14/07/30  一時ファイルディレクトリ設定
      *  04.08.02    NACL-森脇    14/08/08  画像イメージ作成機能追加
      *  05.00.00    NACL-森脇    17/02/22  自費薬剤対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
006900*    エラーファイル
           SELECT  YKJMERR-FILE        ASSIGN  YKJMERR
                                       FILE    STATUS  IS  STS-YKJMERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  YKJMERR-FILE.
       01  YKJMERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
           COPY    "CPSHELLTBL.INC".
      *    COPY    "ORCA-DBPATH".
      *
      *----(04.08.01)--ADD-START---
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //YKJMERR//.
      *----(04.08.01)--ADD-END-----
      *
           COPY    "HCM61.INC".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-YKJMERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-OK                  PIC 9(01).
           03  FLG-END                 PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-YAKUJYO             PIC 9(01).
      *
           03  FLG-KEIJYO              PIC 9(01).
           03  FLG-KONO                PIC 9(01).
           03  FLG-CYUI                PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(04).
           03  CNT-NUM                 PIC 9(04).
           03  CNT-LINE                PIC 9(01).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX-GYO                 PIC 9(03).
           03  IDX-GYO1                PIC 9(03).
      *
           03  IDX-STR                 PIC 9(04).
           03  IDX-STR1                PIC 9(04).
           03  IDX-KET                 PIC 9(04).
      *
           03  IDX-KEIJYO              PIC 9(04).
           03  IDX-KONO                PIC 9(04).
           03  IDX-CYUI                PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-JYO            PIC X(01).
           03  WRK-PARA-KBN            PIC X(01).
           03  WRK-PARA-IYK            PIC X(01).
           03  WRK-PARA-SYU            PIC X(01).
           03  WRK-PARA-NAME           PIC X(10).
           03  WRK-PARA-SRYCD          PIC X(09).
           03  WRK-PARA-INPUTCD        PIC X(10).
      *----(04.08.01)--UPD-START---
      *     03  YKJMERR                 PIC X(100).
      *----(04.08.01)--UPD-END-----
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-YKJMERR             PIC X(200).
           03  WRK-TABLE               PIC X(20).
           03  WRK-PATHNAME            PIC X(20).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
      *
           03  WRK-ZZZ9                PIC ZZZ9.
      *
      *    全角を行毎に編集
       01  WRK-HENSYU-AREA.
           03  WRK-HENSYU-OCC          OCCURS   64.
               05  WRK-HENSYU          PIC X(44).
           03  WRK-NAIYOU              PIC X(4000).
           03  GYO-MAX                 PIC 9(04).
           03  KOU-MAX                 PIC 9(04).
      *
      *    ＊を％に編集
       01  WRK-KEY-AREA.
           03  WRK-MAE-KEY             PIC X(30).
           03  WRK-ATO-KEY             PIC X(30).
           03  WRK-MAX                 PIC 9(02).
           03  WRK-KET                 PIC 9(02).
           03  WRK-STR                 PIC 9(02).
           03  WRK-STR1                PIC 9(02).
      *
       01  WRK-YKJ-AREA.
           03  WRK-YKJ-TBL.
               05  WRK-YKJ-NUM         PIC X(04).
               05  WRK-YKJ-GAZO        PIC X(1024).
               05  WRK-YKJ-SRYCD       PIC X(11).
               05  WRK-YKJ-YKZNAME-G.
                   07  WRK-YKJ-YKZNAME     PIC X(32)   OCCURS  3.
               05  WRK-YKJ-KONOKOKA-G.
                   07  WRK-YKJ-KONOKOKA    PIC X(34)   OCCURS  64.
               05  WRK-YKJ-KEIJYO-G.
                   07  WRK-YKJ-KEIJYO      PIC X(20)   OCCURS  64.
               05  WRK-YKJ-CYUJKO-G.
                   07  WRK-YKJ-CYUJKO      PIC X(44)   OCCURS  64.
      *
       01  FILE-NAME                   PIC X(512).
      *
      *    空白の画像
       01  KUHAKU-NAME.
           03  FILLER                  PIC X(05)   VALUE   "data/".
           03  FILLER                  PIC X(15)   VALUE
                                                   "transparent.jpg".
      *
      *----(04.08.02)--ADD-START---
       01  FILE-IMAGE              PIC X(512).
      *
       01  WRK-CONS-AREA.
      *    画像ファイルフォルダ名
           03  WRK-CONS-MEDIMGDIR  PIC X(08)   VALUE   "medimage".
      *
      *    画像イメージ作成パラメタ
           COPY    "CPORCSMIAREA.INC".
      *----(04.08.02)--ADD-END-----
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    薬情マスタ
           COPY    "CPYAKUJYO.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ファイル存在チェック
           COPY    "CPORCSFILECHK.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    全角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *
      *----(04.08.01)--ADD-START---
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(1000).
      *
      ******************************************************************
       PROCEDURE                   DIVISION
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
                   UNTIL   FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  SYS-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
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
                                       WRK-PARA-JOBID
                                       WRK-PARA-SHELLID
                                       WRK-PARA-HOSPNUM
                                       WRK-PARA-JYO
                                       WRK-PARA-KBN
                                       WRK-PARA-IYK
                                       WRK-PARA-SYU
                                       WRK-PARA-NAME
                                       WRK-PARA-SRYCD
                                       WRK-PARA-INPUTCD
                                       YKJMERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           MOVE    "ORCBN21"           TO  JOB-PGID
           MOVE    "薬剤情報マスタ一覧"
                                       TO  JOB-SHELLMSG
      *
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
      *    ファイルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    KUHAKU-NAME     TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING
                                       ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  FILE-NAME
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    YKJMERR-BASENAME    TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  YKJMERR
      *----(04.08.01)--ADD-END-----
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
           MOVE    ZERO            TO  CNT-PAGE
           MOVE    ZERO            TO  CNT-LINE
           MOVE    ZERO            TO  CNT-NUM
      *
      *    薬剤情報読み込み
           PERFORM 900-YAKUJYO-SELECT-SEC
      *
           PERFORM UNTIL   FLG-YAKUJYO     =   1
               PERFORM 220-PRT-HEAD-SEC
               PERFORM UNTIL   FLG-YAKUJYO     =   1
                   PERFORM 210-PRT-HEN-SEC
                   PERFORM 220-PRT-BODY-SEC
      *            薬剤情報読み込み
                   PERFORM 900-YAKUJYO-FETCH-SEC
               END-PERFORM
               PERFORM 230-PRT-PRINT-SEC
           END-PERFORM
      *
      *    薬剤情報読み込み
           PERFORM 900-YAKUJYO-CLOSE-SEC
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票明細処理
      *****************************************************************
       210-PRT-HEN-SEC             SECTION.
      *
           INITIALIZE                  WRK-YKJ-AREA
      *
      *    空白の画像
           MOVE    FILE-NAME       TO  WRK-YKJ-GAZO
      *
      *    番号セット
           COMPUTE CNT-NUM     =   CNT-NUM     +   1
           MOVE    CNT-NUM         TO  WRK-ZZZ9
           MOVE    WRK-ZZZ9        TO  WRK-YKJ-NUM
      *
      *    診療行為コードセット
           STRING  "("             DELIMITED  BY  SIZE
                   YAKUJYO-SRYCD   DELIMITED  BY  SPACE
                   ")"             DELIMITED  BY  SIZE
                   INTO     WRK-YKJ-SRYCD
           END-STRING
      *
      *    効能効果セット
           PERFORM 211-HENEFFECT-SET-SEC
      *
      *    注意事項
           PERFORM 212-HENCAUTION-SET-SEC
      *
      *    薬剤名セット
           PERFORM 213-HENYKZNAME-SET-SEC
      *
      *    画像編集
           IF      YAKUJYO-PHOTO   NOT =   SPACE
      *----(04.08.02)--ADD-START---
      *        データからイメージファイル作成
               INITIALIZE                  ORCSMIAREA
               MOVE    YAKUJYO-SRYCD   TO  ORCSMI-SRYCD
               MOVE    SGETTEMP-TEMPDIR
                                       TO  ORCSMI-TEMPDIR
               MOVE    YAKUJYO-PHOTO   TO  ORCSMI-FILENAME
               CALL    "ORCSMILOAD"        USING
                                           ORCSMIAREA
                                           SPA-AREA
      *
               MOVE    SPACE           TO  FILE-IMAGE
               STRING  SGETTEMP-TEMPDIR    DELIMITED BY SPACE
                       WRK-CONS-MEDIMGDIR  DELIMITED BY SPACE
                       "/"                 DELIMITED BY SIZE
                       YAKUJYO-PHOTO       DELIMITED BY SPACE
                                   INTO    FILE-IMAGE
               END-STRING
      *----(04.08.02)--ADD-END-----
      *----(04.08.02)--UPD-START---
      *        ファイル存在チェック
               INITIALIZE                  ORCSFILECHKAREA
               MOVE    FILE-IMAGE      TO  FILECHK-NAME
               CALL    "ORCSFILECHK"       USING
                                           ORCSFILECHKAREA
               IF      FILECHK-RC      =   ZERO
                   MOVE    FILE-IMAGE      TO  WRK-YKJ-GAZO
               END-IF
      *----(04.08.02)--UPD-END-----
           END-IF
      *
      *    形状編集
           PERFORM 214-KEIJYO-SET-SEC
      *
           .
       220-HEN-BODY-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票効能効果データ編集処理
      *****************************************************************
       211-HENEFFECT-SET-SEC       SECTION.
      *
           INITIALIZE                  WRK-HENSYU-AREA
      *
           MOVE    YAKUJYO-EFFECT  TO  WRK-NAIYOU
           MOVE    1               TO  IDX-GYO
      *    行
           MOVE    61              TO  GYO-MAX
      *    文字数
           MOVE    34              TO  KOU-MAX
           PERFORM 216-HENSYU-SYORI-SEC
      *
           MOVE    ZERO            TO  IDX-GYO1
           PERFORM VARYING IDX-GYO     FROM    1   BY  1
                   UNTIL   IDX-GYO     >   GYO-MAX
               COMPUTE IDX-GYO1    =   IDX-GYO     +   3
               MOVE    WRK-HENSYU(IDX-GYO)
                                   TO  WRK-YKJ-KONOKOKA(IDX-GYO1)
           END-PERFORM
      *
           .
       211-HENEFFECT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票注意事項データ編集処理
      *****************************************************************
       212-HENCAUTION-SET-SEC      SECTION.
      *
           INITIALIZE                  WRK-HENSYU-AREA
      *
           MOVE    YAKUJYO-CAUTION TO  WRK-NAIYOU
           MOVE    1               TO  IDX-GYO
      *    行
           MOVE    64              TO  GYO-MAX
      *    文字数
           MOVE    44              TO  KOU-MAX
           PERFORM 216-HENSYU-SYORI-SEC
      *
           PERFORM VARYING IDX-GYO     FROM    1   BY  1
                   UNTIL   IDX-GYO     >   GYO-MAX
               MOVE    WRK-HENSYU(IDX-GYO)
                                   TO  WRK-YKJ-CYUJKO(IDX-GYO)
           END-PERFORM
      *
           .
       212-HENCAUTION-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票薬剤名データ編集処理
      *****************************************************************
       213-HENYKZNAME-SET-SEC      SECTION.
      *
           INITIALIZE                  WRK-HENSYU-AREA
      *
           MOVE    YAKUJYO-NAME    TO  WRK-NAIYOU
           MOVE    1               TO  IDX-GYO
      *    行
           MOVE    3               TO  GYO-MAX
      *    文字数
           MOVE    32              TO  KOU-MAX
           PERFORM 216-HENSYU-SYORI-SEC
      *
           PERFORM VARYING IDX-GYO     FROM    1   BY  1
                   UNTIL   IDX-GYO     >   GYO-MAX
               MOVE    WRK-HENSYU(IDX-GYO)
                                   TO  WRK-YKJ-YKZNAME(IDX-GYO)
           END-PERFORM
      *
           .
       213-HENYKZNAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票形状データセット処理
      *****************************************************************
       214-KEIJYO-SET-SEC          SECTION.
      *
           INITIALIZE                  WRK-HENSYU-AREA
      *
           MOVE    1               TO  IDX-GYO
      *    行
           MOVE    64              TO  GYO-MAX
      *    文字数
           MOVE    20              TO  KOU-MAX
      *
      *    色編集
           MOVE    YAKUJYO-COLOR   TO  WRK-NAIYOU
           PERFORM 216-HENSYU-SYORI-SEC
      *
      *    形編集
           MOVE    YAKUJYO-SHAPE   TO  WRK-NAIYOU
           PERFORM 216-HENSYU-SYORI-SEC
      *
      *    記号編集
           IF      YAKUJYO-MARK    NOT =   SPACE
      *        記号チェック
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    YAKUJYO-MARK    TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
      
               IF      KANACHK-SYUBETU =   1
      *            文字数
                  MOVE    21              TO  KOU-MAX
               END-IF
      *
               MOVE    YAKUJYO-MARK    TO  WRK-NAIYOU
               PERFORM 216-HENSYU-SYORI-SEC
           END-IF
      *
           PERFORM VARYING IDX-GYO     FROM    1   BY  1
                   UNTIL   IDX-GYO     >   GYO-MAX
               MOVE    WRK-HENSYU(IDX-GYO)
                                   TO  WRK-YKJ-KEIJYO(IDX-GYO)
           END-PERFORM
      *
           .
       214-KEIJYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票全角編集処理
      *****************************************************************
       216-HENSYU-SYORI-SEC        SECTION.
      *
           MOVE    1               TO  IDX-STR
           MOVE    1               TO  IDX-KET
      *
           PERFORM UNTIL  (IDX-STR     >   4000 )
                   OR     (IDX-GYO     >   GYO-MAX )
                   OR     (WRK-NAIYOU(IDX-STR:)  =   SPACE)
      *        改行なら最大字セット
               IF      WRK-NAIYOU(IDX-STR:1)   =   X"0A"
                   MOVE    KOU-MAX         TO  IDX-KET
                   ADD     1               TO  IDX-STR
               ELSE
      *            全角セット
                   IF      WRK-NAIYOU(IDX-STR:1)    >=  X"A1"
                       MOVE    WRK-NAIYOU(IDX-STR:2)
                                   TO  WRK-HENSYU(IDX-GYO)(IDX-KET:2)
                       ADD     2               TO  IDX-STR
                       ADD     2               TO  IDX-KET
      *            半角セット
                   ELSE
                       MOVE    WRK-NAIYOU(IDX-STR:1)
                                   TO  WRK-HENSYU(IDX-GYO)(IDX-KET:1)
                       ADD     1               TO  IDX-STR
                       ADD     1               TO  IDX-KET
                   END-IF
               END-IF
      *        最大字で改行
               IF         (IDX-KET         >=  KOU-MAX)
                   OR    ((IDX-STR         <=  4000)
                   AND    (WRK-NAIYOU(IDX-STR:) =   SPACE))
                   ADD     1               TO  IDX-GYO
                   MOVE    1               TO  IDX-KET
      *            改行なら次へ
                   IF      WRK-NAIYOU(IDX-STR:1)   =   X"0A"
                       COMPUTE IDX-STR =   IDX-STR     +   1
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       216-HENSYU-SYORI-EXT.
           EXIT.
      *
     *****************************************************************
      *   ＊編集処理
      *****************************************************************
       217-ASUTA-SYORI-SEC         SECTION.
      *
           MOVE    1               TO  WRK-STR
           MOVE    1               TO  WRK-STR1
           MOVE    ZERO            TO  WRK-KET
           MOVE    SPACE           TO  WRK-ATO-KEY
      *
           IF      WRK-PARA-KBN    =   "1"
               MOVE    "%"             TO  WRK-ATO-KEY(WRK-STR1:1)
               ADD     1               TO  WRK-STR1
               ADD     1               TO  WRK-KET
           END-IF
      *
           PERFORM UNTIL ( WRK-STR     >   WRK-MAX )
                   OR    ( WRK-MAE-KEY(WRK-STR:)   =   SPACE )
               IF      WRK-MAE-KEY(WRK-STR:2)  =   "＊"
                   MOVE    "%"             TO  WRK-ATO-KEY(WRK-STR1:1)
                   ADD     2               TO  WRK-STR
                   ADD     1               TO  WRK-STR1
                   ADD     1               TO  WRK-KET
               ELSE
                   IF      WRK-MAE-KEY(WRK-STR:1)  =   "*"
                       MOVE    "%"         TO  WRK-ATO-KEY(WRK-STR1:1)
                       ADD     1           TO  WRK-STR
                       ADD     1           TO  WRK-STR1
                       ADD     1           TO  WRK-KET
                   ELSE
                       MOVE    WRK-MAE-KEY(WRK-STR:1)
                                           TO  WRK-ATO-KEY(WRK-STR1:1)
                       ADD     1           TO  WRK-STR
                       ADD     1           TO  WRK-STR1
                       ADD     1           TO  WRK-KET
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-PARA-KBN    =   "1"
               MOVE    "%"             TO  WRK-ATO-KEY(WRK-STR1:1)
               ADD     1               TO  WRK-KET
           END-IF
      *
           .
       217-ASUTA-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       220-PRT-HEAD-SEC            SECTION.
      *
           INITIALIZE                  HCM61
      *
      *    ページセット
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
           MOVE    CNT-PAGE        TO  WRK-ZZZ9
           MOVE    WRK-ZZZ9        TO  HCM61-PAGE
      *
      *    日付セット
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  HCM61-SYORIBI
      *
           MOVE    "薬剤情報マスタ一覧"
                                   TO  HCM61-TITLE
           MOVE    SPACE           TO  HCM61-KIKAN
      *
           MOVE    FILE-NAME       TO  HCM61-GAZO(1)
                                       HCM61-GAZO(2)
                                       HCM61-GAZO(3)
                                       HCM61-GAZO(4)
                                       HCM61-GAZO(5)
                                       HCM61-GAZO(6)
                                       HCM61-GAZO(7)
                                       HCM61-GAZO(8)
                                       HCM61-GAZO(9)
      *
           MOVE    ZERO            TO  CNT-LINE
      *
           .
       220-PRT-HEAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       220-PRT-BODY-SEC            SECTION.
      *
           IF      CNT-LINE       =   9
               PERFORM 230-PRT-PRINT-SEC
               PERFORM 220-PRT-HEAD-SEC
           END-IF
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           MOVE    WRK-YKJ-NUM     TO  HCM61-NUM(CNT-LINE)
      *
           MOVE    WRK-YKJ-SRYCD   TO  HCM61-SRYCD(CNT-LINE)
           MOVE    WRK-YKJ-GAZO    TO  HCM61-GAZO(CNT-LINE)
      *
           MOVE    WRK-YKJ-YKZNAME-G
                                   TO  HCM61-YKZNAME-G(CNT-LINE)
           MOVE    WRK-YKJ-KEIJYO-G(1:160)
                                   TO  HCM61-KEIJYO-G(CNT-LINE)
           MOVE    WRK-YKJ-KONOKOKA-G(1:272)
                                   TO  HCM61-KONOKOKA-G(CNT-LINE)
           MOVE    WRK-YKJ-CYUJKO-G(1:352)
                                   TO  HCM61-CYUJKO-G(CNT-LINE)
      *
           MOVE    ZERO            TO  FLG-KEIJYO
           MOVE    ZERO            TO  FLG-KONO
           MOVE    ZERO            TO  FLG-CYUI
           MOVE    161             TO  IDX-KEIJYO
           MOVE    273             TO  IDX-KONO
           MOVE    353             TO  IDX-CYUI
      *
      *    文字数がオーバーしていたら次行にセット
           PERFORM 220-CHK-BODY-NEXT-SEC
           PERFORM UNTIL ( FLG-KEIJYO  =   1 )
                   AND   ( FLG-KONO    =   1 )
                   AND   ( FLG-CYUI    =   1 )
               PERFORM 220-PRT-BODY-NEXT-SEC
               PERFORM 220-CHK-BODY-NEXT-SEC
           END-PERFORM
      *
           .
       220-PRT-BODY-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＢＯＤＹ次行チェック処理
      *****************************************************************
       220-CHK-BODY-NEXT-SEC       SECTION.
      *
      *    薬剤形状チェック
           IF    ( IDX-KEIJYO      >=  1280 )  OR
                 ( WRK-YKJ-KEIJYO-G(IDX-KEIJYO:)   =   SPACE )
               MOVE    1           TO  FLG-KEIJYO
           END-IF
      *    効能効果チェック
           IF    ( IDX-KONO        >=  2176 )  OR
                 ( WRK-YKJ-KONOKOKA-G(IDX-KONO:)   =   SPACE )
               MOVE    1           TO  FLG-KONO
           END-IF
      *    注意事項チェック
           IF    ( IDX-CYUI        >=  2816 )  OR
                 ( WRK-YKJ-CYUJKO-G(IDX-CYUI:)     =   SPACE )
               MOVE    1           TO  FLG-CYUI
           END-IF
      *
           .
       220-CHK-BODY-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       220-PRT-BODY-NEXT-SEC       SECTION.
      *
           IF      CNT-LINE    =   9
               PERFORM 230-PRT-PRINT-SEC
               PERFORM 220-PRT-HEAD-SEC
           END-IF
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           IF      FLG-KEIJYO      NOT =   1
               MOVE    WRK-YKJ-KEIJYO-G(IDX-KEIJYO:160)
                                       TO  HCM61-KEIJYO-G(CNT-LINE)
               COMPUTE IDX-KEIJYO  =   IDX-KEIJYO  +   160
           END-IF
      *
           IF      FLG-KONO        NOT =   1
               MOVE    WRK-YKJ-KONOKOKA-G(IDX-KONO:272)
                                       TO  HCM61-KONOKOKA-G(CNT-LINE)
               COMPUTE IDX-KONO    =   IDX-KONO  +   272
           END-IF
      *
           IF      FLG-CYUI        NOT =   1
               MOVE    WRK-YKJ-CYUJKO-G(IDX-CYUI:352)
                                       TO  HCM61-CYUJKO-G(CNT-LINE)
               COMPUTE IDX-CYUI    =   IDX-CYUI  +   352
           END-IF
      *
           .
       220-PRT-BODY-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       230-PRT-PRINT-SEC           SECTION.
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
           MOVE    "HCM61.red"         TO  SPRT-PRTID
           MOVE    "薬剤情報マスタ一覧"
                                       TO  SPRT-TITLE
           MOVE    HCM61               TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-YKJMERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       230-PRT-PRINT-EXT.
           EXIT.
      *
     *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT           YKJMERR-FILE
           IF        ( STS-YKJMERR =   ZERO )
               CONTINUE
           ELSE
               OPEN    OUTPUT      YKJMERR-FILE
      *
               MOVE    WRK-YKJMERR     TO  YKJMERR-REC
               WRITE   YKJMERR-REC
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               MOVE    WRK-YKJMERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           END-IF
      *
           CLOSE   YKJMERR-FILE
           MOVE    1               TO  FLG-END
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
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG     REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読み込み
      *****************************************************************
       900-TENSU-READ-SEC          SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    YAKUJYO-SRYCD   TO  TNS-SRYCD
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys         CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
grpsys                                         SPA-AREA
               IF  MCP-RC  =  ZERO
                   MOVE    ZERO            TO  FLG-TENSU
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               ELSE
                   MOVE    1               TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           .
       900-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報読み込み処理
      *****************************************************************
       900-YAKUJYO-SELECT-SEC      SECTION.
      *
           INITIALIZE                  YAKUJYO-REC
           INITIALIZE                  WRK-KEY-AREA
      *
      *----(05.00.00)--UPD-START---
           MOVE    "tbl_yakujyo"   TO  WRK-TABLE
           EVALUATE    WRK-PARA-JYO
               WHEN    "1"
                   EVALUATE    WRK-PARA-SYU
                       WHEN    "1"
      *                     MOVE    "key5s"         TO  WRK-PATHNAME
                           MOVE    "key5js"        TO  WRK-PATHNAME
                       WHEN    "2"
      *                     MOVE    "key5k"         TO  WRK-PATHNAME
                           MOVE    "key5jk"        TO  WRK-PATHNAME
                   END-EVALUATE
               WHEN    "2"
                   EVALUATE    WRK-PARA-KBN
                       WHEN    "1"
                           MOVE    30                  TO  WRK-MAX
                           MOVE    WRK-PARA-NAME       TO  WRK-MAE-KEY
                           PERFORM 217-ASUTA-SYORI-SEC
                           MOVE    WRK-ATO-KEY(1:WRK-KET) 
                                                       TO  YAKUJYO-NAME
                           EVALUATE    WRK-PARA-SYU
                               WHEN    "1"
      *                             MOVE    "key2s"    TO  WRK-PATHNAME
                                   MOVE    "key2js"    TO  WRK-PATHNAME
                               WHEN    "2"
      *                             MOVE    "key2k"    TO  WRK-PATHNAME
                                   MOVE    "key2jk"    TO  WRK-PATHNAME
                           END-EVALUATE
                       WHEN    "2"
                           MOVE    9                   TO  WRK-MAX
                           MOVE    WRK-PARA-SRYCD      TO  WRK-MAE-KEY
                           PERFORM 217-ASUTA-SYORI-SEC
                           MOVE    WRK-ATO-KEY(1:WRK-KET)
                                                       TO  YAKUJYO-SRYCD
                           EVALUATE    WRK-PARA-SYU
                               WHEN    "1"
      *                             MOVE    "key3s"    TO  WRK-PATHNAME
                                   MOVE    "key3js"    TO  WRK-PATHNAME
                               WHEN    "2"
      *                             MOVE    "key3k"    TO  WRK-PATHNAME
                                   MOVE    "key3jk"    TO  WRK-PATHNAME
                           END-EVALUATE
                       WHEN    "3"
                           MOVE    10                  TO  WRK-MAX
                           MOVE    WRK-PARA-INPUTCD    TO  WRK-MAE-KEY
                           PERFORM 217-ASUTA-SYORI-SEC
                           MOVE    WRK-ATO-KEY(1:WRK-KET)
                                                       TO  YAKUJYO-SRYCD
                           EVALUATE    WRK-PARA-SYU
                               WHEN    "1"
      *                             MOVE    "key4s"    TO  WRK-PATHNAME
                                   MOVE    "key4js"    TO  WRK-PATHNAME
                               WHEN    "2"
      *                             MOVE    "key4k"    TO  WRK-PATHNAME
                                   MOVE    "key4jk"    TO  WRK-PATHNAME
                           END-EVALUATE
                       WHEN    "4"
                           EVALUATE    WRK-PARA-SYU
                               WHEN    "1"
      *                             MOVE    "key5s"    TO  WRK-PATHNAME
                                   MOVE    "key5js"    TO  WRK-PATHNAME
                               WHEN    "2"
      *                             MOVE    "key5k"    TO  WRK-PATHNAME
                                   MOVE    "key5jk"    TO  WRK-PATHNAME
                           END-EVALUATE
                   END-EVALUATE
           END-EVALUATE
      *----(05.00.00)--UPD-END-----
      *
           MOVE    WRK-PARA-HOSPNUM    TO  YAKUJYO-HOSPNUM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  YAKUJYO-CREYMD
           MOVE    YAKUJYO-REC     TO  MCPDATA-REC
      *
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           IF      MCP-RC          =   ZERO
               PERFORM 900-YAKUJYO-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-YAKUJYO
           END-IF
      *
           .
       900-YAKUJYO-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報読み込み処理
      *****************************************************************
       900-YAKUJYO-FETCH-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM UNTIL   FLG-OK  =   1
               MOVE    WRK-TABLE       TO  MCP-TABLE
               MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
               MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys         CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
grpsys                                         SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-YAKUJYO
                   MOVE    MCPDATA-REC     TO  YAKUJYO-REC
      *            薬剤名をセット
                   PERFORM 900-TENSU-READ-SEC
                   IF  YAKUJYO-NAME        =   SPACE
                       MOVE    TNS-NAME        TO  YAKUJYO-NAME
                   END-IF
      *            薬剤区分をチェック
                   IF        ( WRK-PARA-JYO        =   "1" )
                       OR    ( WRK-PARA-KBN    NOT =   "4" )
                       MOVE    1               TO  FLG-OK
                   ELSE
                       EVALUATE    WRK-PARA-IYK    ALSO    TNS-YKZKBN
                           WHEN    "0"     ALSO    "1"
                           WHEN    "0"     ALSO    "6"
                           WHEN    "0"     ALSO    "4"
                           WHEN    "1"     ALSO    "1"
                           WHEN    "1"     ALSO    "6"
                           WHEN    "2"     ALSO    "1"
                           WHEN    "2"     ALSO    "4"
                           WHEN    "3"     ALSO    "1"
                           WHEN    "4"     ALSO    "4"
                           WHEN    "4"     ALSO    "6"
                           WHEN    "5"     ALSO    "6"
                           WHEN    "6"     ALSO    "4"
                               MOVE    1           TO  FLG-OK
                       END-EVALUATE
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-YAKUJYO
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           .
      *
       900-YAKUJYO-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報クロース処理
      *****************************************************************
       900-YAKUJYO-CLOSE-SEC       SECTION.
      *
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
      *
           .
       900-YAKUJYO-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
grpsys                                     SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
