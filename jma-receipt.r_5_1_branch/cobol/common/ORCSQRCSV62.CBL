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
       PROGRAM-ID.             ORCSQRCSV62.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : お薬手帳　ＱＲデータ出力
      *  管理者            : 
      *  作成日付    作業者        記述
      *  12/09/03    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  13/07/XX  電子版お薬手帳アプリ連携対応他
      *  04.08.00    NACL-多々納  14/07/30  一時ディレクトリ対応
      *  04.08.00    NACL-多々納  16/11/XX  ＱＲコードver.2.1対応
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ＱＲデータ
           SELECT  QRCSV-FILE      ASSIGN  FILE-NAME1
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-FILE1.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    ＱＲデータ
       FD  QRCSV-FILE.
       01  QRCSV-REC           PIC X(1000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    ファイル指定領域
       01  FILE-NAME1              PIC X(1024).
       01  FILE-NAME2              PIC X(1024).
      *
      *    ファイル指定領域
       01  FILE-NAME1-PARA.
      **** 03  FILLER              PIC X(05)   VALUE   "/tmp/".
           03  FILE-HOSPNUM1       PIC 9(02)   VALUE   ZERO.
           03  FILE-FORM-ID1       PIC X(05)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ".".
           03  FILE-TERMID1        PIC X(32)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ".".
           03  FILE-SYOHMS1        PIC 9(08)   VALUE   ZERO.
           03  FILLER              PIC X(07)   VALUE   ".qr.dat".
       01  FILE-NAME2-PARA.
      **** 03  FILLER              PIC X(05)   VALUE   "/tmp/".
           03  FILE-HOSPNUM2       PIC 9(02)   VALUE   ZERO.
           03  FILE-FORM-ID2       PIC X(05)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ".".
           03  FILE-TERMID2        PIC X(32)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ".".
           03  FILE-SYOHMS2        PIC 9(08)   VALUE   ZERO.
           03  FILLER              PIC X(07)   VALUE   ".qr.png".
      *    フラグ領域
       01  STS-AREA.
           03  STS-FILE1           PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
      *
           03  FLG-NAI-TEN         PIC 9(01).
           03  FLG-YAKUZAI         PIC 9(01).
           03  FLG-KIZAI           PIC 9(01).
           03  FLG-ETC             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDX-D               PIC 9(04).
           03  IDX-H               PIC 9(02).
           03  IDX-B               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDXM                PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  TBL-MAX             PIC 9(04).
           03  IDX-B2              PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-POINT-AREA.
               05  WRK-PO-ST           PIC 9(05).
               05  WRK-PO-ED           PIC 9(05).
               05  WRK-CNT             PIC 9(05).
           03  WRK-DATA                PIC X(140).
      *
           03  WRK-RP2                 PIC 9(03).
           03  WRK-RP28                PIC 9(03).
           03  WRK-RP-11               PIC 9(02).
      *
           03  WRK-H-RP1               PIC X(03).
           03  WRK-H-RP2               PIC X(03).
      **** 03  WRK-H-RP2-1             PIC X(03).
           03  WRK-H-RP-11             PIC X(03).
           03  WRK-H-DATA              PIC X(03).
           03  WRK-ZZZ                 PIC ZZZ.
      *
           03  WRK-SURYO-K          PIC 9(05)V9(05).
           03  WRK-KANSAN           PIC 9(05)V9(05).
           03  WRK-SURYO            PIC 9(05)V9(05).
           03  WRK-SURYO-R          REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1     PIC 9(05).
               05  WRK-SURYO-S2     PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1     PIC ZZZZ9.
               05  WRK-SURYO-Z2     PIC X(01).
               05  WRK-SURYO-Z3     PIC ZZZZZ.
           03  WRK-SURYO-H          PIC X(12).
      *
      *    薬品名称
           03  WRK-NAME.
               05  WRK-NAME-1          PIC X(80).
               05  WRK-NAME-2          PIC X(20).
      *
      *    ＱＲテーブル
       01  TBL-QRCSV-AREA.
           03  TBL-QRCSV-REC.
               05  TBL-QRCSV-DATA        PIC X(140)  OCCURS  11.
      *
      *    ＱＲ作成用
           COPY    "CPORCSQRENCODE.INC".
      *    ＣＳＶファイル作成パラメタ
      ***  COPY    "CPORCSFILEENCODE.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    お薬手帳データ制御サブパラメタ
           COPY    "CPORCSMNOTE.INC".
      *
      *v4.8
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    ＱＲパラメタ
           COPY    "CPORCSQRCSV62.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSQRCSV62AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM1
           MOVE    ORCSQRCSV62-FORM-ID TO  FILE-FORM-ID1
           MOVE    SPA-TERMID          TO  FILE-TERMID1
           MOVE    ORCSQRCSV62-SYSTIME TO  FILE-SYOHMS1
      *ver.4.8
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    FILE-NAME1-PARA     TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES(1)
                                       TO  FILE-NAME1
      *
           OPEN    OUTPUT              QRCSV-FILE
           IF      STS-FILE1       NOT =   ZERO
               MOVE    9               TO  ORCSQRCSV62-ERRCD
           END-IF
      *
      *    ＤＢ検索処理
           PERFORM 1001-DBKENSAKU-SEC
      *H28.11
      *    バージョン判定
           IF      ORCSQRCSV62-CSV-KBN =   "2"
      *    ver.2.1
      *        共通情報出力
               IF      ORCSQRCSV62-ERRCD   =   ZERO
                   PERFORM 100-HEAD21-SYORI-SEC
               END-IF
           ELSE
      *    ver.1.1
      *        共通情報出力
               IF      ORCSQRCSV62-ERRCD   =   ZERO
                   PERFORM 100-HEAD-SYORI-SEC
               END-IF
           END-IF
      *    薬剤情報出力
           IF      ORCSQRCSV62-ERRCD   =   ZERO
               PERFORM 200-MAIN-SYORI-SEC
           END-IF
      *
           CLOSE                       QRCSV-FILE
      *
      *    ＱＲイメージ作成
           IF     (ORCSQRCSV62-ERRCD   =   ZERO )
             AND  (ORCSQRCSV62-QR-KBN  =   "1"  )
               PERFORM 400-QRIMAGE-SEC
           END-IF
      *
      *    CSVデータ
           IF     (ORCSQRCSV62-ERRCD   =   ZERO   )    AND
                  (ORCSQRCSV62-CSV-FILENAME
                                   NOT =   SPACE  )
               INITIALIZE                  ORCSMNOTEAREA
               MOVE    "INS"               TO  SMNOTE-MODE
               MOVE    SPA-PTID            TO  SMNOTE-PTID
               MOVE    "2"                 TO  SMNOTE-NYUGAIKBN
               MOVE    "/var/tmp"          TO  SMNOTE-TO-FOLDER
               MOVE    ORCSQRCSV62-CSV-FILENAME
                                           TO  SMNOTE-TO-DATA
               MOVE    "2"                 TO  SMNOTE-CODE-TYPE
               MOVE    "お薬手帳"          TO  SMNOTE-TITLE
               MOVE    "MNOTE"             TO  SMNOTE-TBL-KEY
      *v4.8
      ******** MOVE    FILE-NAME1          TO  SMNOTE-CSVFILE
               MOVE    FILE-NAME1-PARA     TO  SMNOTE-CSVFILE
               MOVE    ORCSQRCSV62-CNT     TO  SMNOTE-RENNUM
               MOVE    ORCSQRCSV62-DRCD    TO  SMNOTE-DRCD
      *        データバージョン
               MOVE    ORCSQRCSV62-CSV-KBN TO  SMNOTE-DATA-VERSION
      *        出力場所コード
               EVALUATE    ORCSQRCSV62-PGKBN
      *            請求確認
                   WHEN    "1"
                       MOVE    "01"        TO  SMNOTE-LOCATION-CD
      *            中途終了
                   WHEN    "2"
                       MOVE    "02"        TO  SMNOTE-LOCATION-CD
      *            前回処方
                   WHEN    "3"
                       MOVE    "03"        TO  SMNOTE-LOCATION-CD
               END-EVALUATE
               CALL    "ORCSMNOTE"         USING
                                           ORCSMNOTEAREA
                                           SPA-AREA
           END-IF
      *
      *    CSVデータ（JIS）の作成
      *    CSVファイル名が設定されている場合に作成する
      *    IF      ORCSQRCSV62-CSV-FILENAME
      *                            NOT =   SPACE
      *        INITIALIZE                      ORCSFILEENCODEAREA
      *        MOVE    FILE-NAME1          TO  FLEN-INFILENAME
      *        MOVE    ORCSQRCSV62-CSV-FILENAME
      *                                    TO  FLEN-OUTFILENAME
      *        MOVE    "3"                 TO  FLEN-OPTION
      *        CALL    "orcfileencode"     USING
      *                                    ORCSFILEENCODEAREA
      *        IF      FLEN-RET        NOT =   ZERO
      *            MOVE    9               TO  ORCSQRCSV62-ERRCD
      *        END-IF
      *    END-IF
      *
      *    ＱＲデータ削除（ＣＳＶなし）
           IF      SPA-API-FLG         =   ZERO
               IF      ORCSQRCSV62-CSV-FILENAME
                                       =   SPACE
                   MOVE    ZERO                TO  IF-RESULT
                   MOVE    FILE-NAME1          TO  IF-FILENAME
                   CALL    "orcfiledel"        USING
                                               ORCSFDELAREA
               END-IF
           END-IF
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    共通情報出力処理
      *****************************************************************
       100-HEAD-SYORI-SEC             SECTION.
      *
      *    バージョン情報
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "JAHISTC01"         TO  TBL-QRCSV-DATA (1)
           MOVE    1                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *
      *    患者情報レコード
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "1"                 TO  TBL-QRCSV-DATA (1)
      *    患者氏名　拡張漢字変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    ORCSQRCSV62-NAME    TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-OUT-INPUT(1:40)
                                       =   ORCSQRCSV62-NAME
      *        拡張漢字なし
               MOVE    ORCSQRCSV62-NAME    TO  TBL-QRCSV-DATA (2)
           ELSE
      *        拡張漢字あり　カナ氏名
               MOVE    ORCSQRCSV62-KANANAME
                                           TO  TBL-QRCSV-DATA (2)
           END-IF
      *    性別
           EVALUATE    ORCSQRCSV62-SEX
               WHEN    "男"
                   MOVE    "1"                 TO  TBL-QRCSV-DATA (3)
               WHEN    "女"
                   MOVE    "2"                 TO  TBL-QRCSV-DATA (3)
           END-EVALUATE
      *    患者生年月日
           MOVE    ORCSQRCSV62-BIRTHDAY    TO  TBL-QRCSV-DATA (4)
           MOVE    4                       TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *
      *    調剤年月日レコード
           MOVE    SPACE                   TO  TBL-QRCSV-AREA
           MOVE    "5"                     TO  TBL-QRCSV-DATA (1)
           MOVE    ORCSQRCSV62-SRYYMD      TO  TBL-QRCSV-DATA (2)
           MOVE    2                       TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *
      *    医療機関コード
           MOVE    SPACE                   TO  TBL-QRCSV-AREA
           MOVE    "11"                    TO  TBL-QRCSV-DATA (1)
           MOVE    ORCSQRCSV62-HOSPNAME    TO  TBL-QRCSV-DATA (2)
           MOVE    ORCSQRCSV62-PREFNUM     TO  TBL-QRCSV-DATA (3)
           MOVE    "1"                     TO  TBL-QRCSV-DATA (4)
           MOVE    ORCSQRCSV62-HOSPCD      TO  TBL-QRCSV-DATA (5)
           MOVE    5                       TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *
      *    医師レコード
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "15"                TO  TBL-QRCSV-DATA (1)
      *    拡張漢字変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    ORCSQRCSV62-DR-NAME TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  TBL-QRCSV-DATA (2)
           MOVE    2                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *
           .
       100-HEAD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       1001-DBKENSAKU-SEC             SECTION.
      *
      *
           .
       1001-DBKENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       400-DATAOUT-SEC             SECTION.
      *
           INITIALIZE                  WRK-POINT-AREA
           MOVE    SPACE           TO  QRCSV-REC
           INITIALIZE                  QRCSV-REC
      *
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL      (IDK     >   TBL-MAX )
                       OR     (WRK-PO-ED  >=  1000 )
      *
               COMPUTE WRK-PO-ST      =   WRK-PO-ED   +   1
      *
               MOVE    TBL-QRCSV-DATA (IDK)    TO  WRK-DATA
      *        項目別編集処理
               PERFORM 4001-KOUMOKU-HEN-SEC
               MOVE    WRK-PO-ST       TO  WRK-PO-ED
           END-PERFORM
      *
      *    ＱＲデータ出力
           WRITE   QRCSV-REC
           .
       400-DATAOUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       400-QRIMAGE-SEC             SECTION.
      *
           MOVE    SPACE               TO  ORCSQRENCODEAREA
      *
           MOVE    FILE-NAME1          TO  QREN-FILENAME
           MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
           MOVE    ORCSQRCSV62-FORM-ID TO  FILE-FORM-ID2
           MOVE    SPA-TERMID          TO  FILE-TERMID2
           MOVE    ORCSQRCSV62-SYSTIME TO  FILE-SYOHMS2
      *ver.4.8
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    FILE-NAME2-PARA     TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES(1)
                                       TO  FILE-NAME2
      *
           MOVE    FILE-NAME2          TO  QREN-QRFILENAME
      *!!  MOVE    "L"                 TO  QREN-SL-OPTION
           MOVE    "M"                 TO  QREN-SL-OPTION
           MOVE    "1"                 TO  QREN-SS-OPTION
           MOVE    "2"                 TO  QREN-SM-OPTION
      *サイズ
           MOVE    "12"                TO  QREN-SV-OPTION
      **** MOVE    "14"                TO  QREN-SV-OPTION
      **** MOVE    "1"                 TO  QREN-SK-OPTION
      **** MOVE    "3"                 TO  QREN-SK-OPTION
           MOVE    "2"                 TO  QREN-SK-OPTION
           MOVE    "1"                 TO  QREN-LC-OPTION
      *
           CALL    "orcqrencode"       USING   ORCSQRENCODEAREA
      *H25.2
      *    サイズエラーの時は、再度作成
           IF     (QREN-RET            =   "1"  )
               OR (QREN-RET-COUNT      >   12   )
               MOVE    "L"                 TO  QREN-SL-OPTION
               INITIALIZE                      QREN-RET-VERSION
               INITIALIZE                      QREN-RET-COUNT
               INITIALIZE                      QREN-RET
               CALL    "orcqrencode"       USING   ORCSQRENCODEAREA
           END-IF
      *
           MOVE    QREN-RET-VERSION    TO  ORCSQRCSV62-RET-VERSION
           MOVE    QREN-RET-COUNT      TO  ORCSQRCSV62-RET-COUNT
           MOVE    QREN-RET            TO  ORCSQRCSV62-RET
           .
       400-QRIMAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目別編集処理
      *****************************************************************
       4001-KOUMOKU-HEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-CNT
           PERFORM VARYING IDX-D       FROM    140   BY  -1
                   UNTIL  (IDX-D       <   1     )
      *        半角空白以外
               IF      WRK-DATA (IDX-D:1)  NOT =   SPACE
                   MOVE    IDX-D           TO  WRK-CNT
                   MOVE    1               TO  IDX-D
               END-IF
           END-PERFORM
      *
           IF          WRK-CNT         >   0
               MOVE    WRK-DATA (1:WRK-CNT)
                                           TO  QRCSV-REC
                                               (WRK-PO-ST:WRK-CNT)
               COMPUTE WRK-PO-ST   =   WRK-PO-ST   +   WRK-CNT
           END-IF
      *
      *    最終項目以外に , 追加
           IF          IDK             <   TBL-MAX
               MOVE    ","             TO  QRCSV-REC(WRK-PO-ST:1)
           END-IF
           .
       4001-KOUMOKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報出力処理
      *****************************************************************
       200-MAIN-SYORI-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-RP2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   50  )
                         OR   (ORCSQRCSV62-SRYKBN (IDX) =   SPACE )
               MOVE    ZERO                TO  WRK-RP28
               PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL  (IDX2    >   100  )
      *            薬剤レコード
                   IF     (ORCSQRCSV62-SRYCD(IDX IDX2)(1:1) =  "6"
                                                            OR "7" )
                       OR (ORCSQRCSV62-SRYCD(IDX IDX2)(1:3) =  "059")
      *                （サ）は対象
                       OR (ORCSQRCSV62-SRYCD(IDX IDX2) =   "820000046")
      *??
                       PERFORM 2003-RP201-SEC
      *                種類チェック
                       PERFORM 2004-YKZCHEK-SEC
                   END-IF
      *
      *            薬剤補足レコード
                   IF     (ORCSQRCSV62-SRYCD(IDX IDX2)(1:1) =  "8" )
                     OR   (ORCSQRCSV62-SRYCD(IDX IDX2)(1:3) =  "008")
                     OR  ((ORCSQRCSV62-SRYCD(IDX IDX2)(1:3) =  "001")
                      AND (ORCSQRCSV62-YCOMMNTKBN(IDX IDX2) =   "2" ))
      *                （サ）は対象外
                       IF      ORCSQRCSV62-SRYCD(IDX IDX2)
                                                       =   "820000046"
                           CONTINUE
                       ELSE
                           PERFORM 20030-RP281-SYORI-SEC
                       END-IF
                   END-IF
               END-PERFORM
      *        用法レコード
               IF      WRK-RP2             >   ZERO
                   PERFORM 2001-RP301-SYORI-SEC
               END-IF
           END-PERFORM
      *
      *    備考レコード
           PERFORM VARYING     IDX-B   FROM    1   BY  1
                   UNTIL      (IDX-B   >   10  )
                           OR (ORCSQRCSV62-BIKOU (IDX-B) =   SPACE)
               MOVE    SPACE               TO  TBL-QRCSV-AREA
               MOVE    "501"               TO  TBL-QRCSV-DATA (1)
               MOVE    ORCSQRCSV62-BIKOU (IDX-B)
                                           TO  TBL-QRCSV-DATA (2)
      *H28.11
               IF      ORCSQRCSV62-CSV-KBN =   "2"
      *            v.2.1
                   MOVE    "1"                 TO  TBL-QRCSV-DATA (3)
                   MOVE    3                   TO  TBL-MAX
               ELSE
      *           v.1.0
                   MOVE    2                   TO  TBL-MAX
               END-IF
               PERFORM 400-DATAOUT-SEC
           END-PERFORM
      *
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＲ番号数値編集処理
      *****************************************************************
       20011-SUJIHEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-H-DATA
           PERFORM VARYING    IDZ      FROM    1  BY  1
                   UNTIL      IDZ      >   3
               IF      WRK-ZZZ(IDZ:1)  NOT =   SPACE
                   MOVE    WRK-ZZZ(IDZ:)   TO  WRK-H-DATA
                   MOVE    3               TO  IDZ
               END-IF
           END-PERFORM
           .
       20011-SUJIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤種類チェック処理
      *****************************************************************
       2004-YKZCHEK-SEC            SECTION.
      *
           EVALUATE    ORCSQRCSV62-SRYCD(IDX IDX2)(1:1)
               WHEN    "6"
                   IF      ORCSQRCSV62-NAIHENKBN (IDX IDX2)  =   "1"
                       MOVE    1               TO  FLG-NAI-TEN
                   ELSE
                       MOVE    1               TO  FLG-YAKUZAI
                   END-IF
               WHEN    "7"
                   MOVE    1               TO  FLG-KIZAI
               WHEN    OTHER
                   MOVE    1               TO  FLG-ETC
           END-EVALUATE
           .
       2004-YKZCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    用法レコード処理
      *****************************************************************
       2001-RP301-SYORI-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-RP-11
      *    用法レコードは最後のコードとする
           PERFORM VARYING     IDX2    FROM    100  BY  -1
                   UNTIL      (IDX2    <   1    )
                         OR   (WRK-RP-11   >   ZERO  )
      *        用法レコード
               IF     (ORCSQRCSV62-SRYCD(IDX IDX2)(1:3) =   "001")
               AND    (ORCSQRCSV62-YCOMMNTKBN(IDX IDX2) =   "1"
                                                        OR  "0" )
                   MOVE    ORCSQRCSV62-TNS-NAME (IDX IDX2)
                                               TO  WRK-NAME
                   PERFORM 20021-RP301-SEC
                   MOVE    SPACE       TO  ORCSQRCSV62-SRYCD(IDX IDX2)
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   100   )
      *        用法補助レコード
               IF     (ORCSQRCSV62-SRYCD(IDX IDX2)(1:3) =  "001")
               AND    (ORCSQRCSV62-YCOMMNTKBN(IDX IDX2) =   "1"
                                                        OR  "0" )
                   PERFORM 20022-RP311-SEC
               END-IF
           END-PERFORM
      *    用法がない時、用法名称なしとする
           IF      WRK-RP-11           =   ZERO
               MOVE    SPACE               TO  WRK-NAME
               PERFORM 20021-RP301-SEC
           END-IF
           .
       2002-RP111-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    用法レコード処理
      *****************************************************************
       20021-RP301-SEC            SECTION.
      *
           ADD     1                   TO  WRK-RP-11
      *
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "301"               TO  TBL-QRCSV-DATA (1)
           MOVE    WRK-H-RP2           TO  TBL-QRCSV-DATA (2)
      *    用法名称
           MOVE    WRK-NAME            TO  TBL-QRCSV-DATA (3)
      *    日数（数量）
           MOVE    ORCSQRCSV62-KAISU (IDX)   TO  WRK-ZZZ
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  TBL-QRCSV-DATA (4)
      *    調剤単位
           EVALUATE    ORCSQRCSV62-SRYKBN (IDX)
               WHEN    "21"
      *            内服
                   MOVE    "日分"          TO  TBL-QRCSV-DATA (5)
                   MOVE    "1"             TO  TBL-QRCSV-DATA (6)
      *            内服点滴のみ
                   IF     (FLG-NAI-TEN         =   1    )  AND
                          (FLG-YAKUZAI         =   ZERO )
                       MOVE    "調剤"          TO  TBL-QRCSV-DATA (5)
                       MOVE    "2"             TO  TBL-QRCSV-DATA (6)
      *                日数＝１
                       MOVE    "1"             TO  TBL-QRCSV-DATA (4)
                   END-IF
               WHEN    "22"
      *            頓服
                   MOVE    "回分"          TO  TBL-QRCSV-DATA (5)
                   MOVE    "3"             TO  TBL-QRCSV-DATA (6)
               WHEN    "23"
      *            外用
                   MOVE    "調剤"          TO  TBL-QRCSV-DATA (5)
                   MOVE    "5"             TO  TBL-QRCSV-DATA (6)
               WHEN    "14"
      *            在宅
                   MOVE    "調剤"          TO  TBL-QRCSV-DATA (5)
                   MOVE    "10"            TO  TBL-QRCSV-DATA (6)
                   IF     (FLG-YAKUZAI     =   1   ) AND
                          (FLG-KIZAI       =   0   )
      *                薬剤のみ在宅注射
                       MOVE    "4"             TO  TBL-QRCSV-DATA (6)
                   END-IF
                   IF     (FLG-YAKUZAI     =   0   )  AND
                          (FLG-KIZAI       =   1   )
      *                器材のみ
                       MOVE    "9"         TO  TBL-QRCSV-DATA (6)
                   END-IF
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
      *            注射（入院）
                   MOVE    "調剤"          TO  TBL-QRCSV-DATA (5)
                   MOVE    "4"             TO  TBL-QRCSV-DATA (6)
               WHEN    OTHER
                   MOVE    "調剤"          TO  TBL-QRCSV-DATA (5)
                   MOVE    "10"            TO  TBL-QRCSV-DATA (6)
           END-EVALUATE
      *
           MOVE    "1"                 TO  TBL-QRCSV-DATA (7)
           MOVE    SPACE               TO  TBL-QRCSV-DATA (8)
      *H28.11
           IF      ORCSQRCSV62-CSV-KBN =   "2"
      *        v.2.1
               MOVE    "1"                 TO  TBL-QRCSV-DATA (9)
               MOVE    9                   TO  TBL-MAX
           ELSE
      *        v.1.0
               MOVE    8                   TO  TBL-MAX
           END-IF
           PERFORM 400-DATAOUT-SEC
           .
       20021-RP301-EXT.
           EXIT.
      *
      *****************************************************************
      *    用法補助レコード処理
      *****************************************************************
       20022-RP311-SEC            SECTION.
      *
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "311"               TO  TBL-QRCSV-DATA (1)
           MOVE    WRK-H-RP2           TO  TBL-QRCSV-DATA (2)
           MOVE    ORCSQRCSV62-TNS-NAME (IDX IDX2)
                                       TO  TBL-QRCSV-DATA (3)
      *H28.11
           IF      ORCSQRCSV62-CSV-KBN =   "2"
      *        v.2.1
               MOVE    "1"                 TO  TBL-QRCSV-DATA (4)
               MOVE    4                   TO  TBL-MAX
           ELSE
      *        v.1.0
               MOVE    3                   TO  TBL-MAX
           END-IF
           PERFORM 400-DATAOUT-SEC
           .
       20022-RP311-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤レコード処理
      *****************************************************************
       2003-RP201-SEC            SECTION.
      *
           IF      WRK-RP28            =   ZERO
               ADD     1                   TO  WRK-RP2
               MOVE    WRK-RP2             TO  WRK-ZZZ
               PERFORM 20011-SUJIHEN-SEC
               MOVE    WRK-H-DATA          TO  WRK-H-RP2
           END-IF
      *
           ADD     1                   TO  WRK-RP28
      *
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "201"               TO  TBL-QRCSV-DATA (1)
           MOVE    WRK-H-RP2           TO  TBL-QRCSV-DATA (2)
      *    薬品の名称は４０文字とする
           MOVE    ORCSQRCSV62-TNS-NAME (IDX IDX2)
                                       TO  WRK-NAME
           MOVE    WRK-NAME-1          TO  TBL-QRCSV-DATA (3)
      *    用量（数量）
           MOVE    ORCSQRCSV62-SURYOU(IDX IDX2)  TO  WRK-SURYO
           PERFORM 20031-SURYO-HEN-SEC
           MOVE    WRK-SURYO-H         TO  TBL-QRCSV-DATA (4)
      *    単位名
           MOVE    ORCSQRCSV62-TANINAME(IDX IDX2)
                                       TO  TBL-QRCSV-DATA (5)
      *    薬品コード種別
           IF     (ORCSQRCSV62-SRYCD(IDX IDX2)(1:1)  =   "6" )
               MOVE    "2"                 TO  TBL-QRCSV-DATA (6)
               MOVE    ORCSQRCSV62-SRYCD(IDX IDX2)
                                           TO  TBL-QRCSV-DATA (7)
           ELSE
               MOVE    "1"                 TO  TBL-QRCSV-DATA (6)
           END-IF
      *H28.11
           IF      ORCSQRCSV62-CSV-KBN =   "2"
      *        v.2.1
               MOVE    "1"                 TO  TBL-QRCSV-DATA (8)
               MOVE    8                   TO  TBL-MAX
           ELSE
      *        v.1.0
               MOVE    7                   TO  TBL-MAX
           END-IF
           PERFORM 400-DATAOUT-SEC
      *
           .
       2003-RP201-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬品補助レコード作成処理
      *****************************************************************
       20030-RP281-SYORI-SEC         SECTION.
      *
           IF     (WRK-RP28            =   ZERO  )
               CONTINUE
           ELSE
               MOVE    IDX2            TO  IDX3
               PERFORM 20032-RP281-SEC
           END-IF
           .
       20030-RP281-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬品補助レコード処理
      *****************************************************************
       20032-RP281-SEC         SECTION.
      *
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "281"               TO  TBL-QRCSV-DATA (1)
           MOVE    WRK-H-RP2           TO  TBL-QRCSV-DATA (2)
      *
           MOVE    ORCSQRCSV62-TNS-NAME (IDX IDX3)
                                       TO  TBL-QRCSV-DATA (3)
      *H28.11
           IF      ORCSQRCSV62-CSV-KBN =   "2"
      *        v.2.1
               MOVE    "1"                 TO  TBL-QRCSV-DATA (4)
               MOVE    4                   TO  TBL-MAX
           ELSE
      *        v.1.0
               MOVE    3                   TO  TBL-MAX
           END-IF
           PERFORM 400-DATAOUT-SEC
           .
       20032-RP281-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量編集処理
      *****************************************************************
       20031-SURYO-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDXM    FROM    5  BY  -1
                   UNTIL       IDXM    <   1
               IF      WRK-SURYO-S2(IDXM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDXM:1)  TO  WRK-SURYO-Z3
                                                             (IDXM:1)
               END-IF
           END-PERFORM
           IF      WRK-SURYO-Z3    NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF
      *
           MOVE    SPACE               TO  WRK-SURYO-H
           PERFORM VARYING     IDXM    FROM    1  BY  1
                   UNTIL       IDXM    >   12
               IF      WRK-SURYO-X(IDXM:1) NOT =   SPACE
                   MOVE    WRK-SURYO-X (IDXM:) TO  WRK-SURYO-H
                   MOVE    12                  TO  IDXM
               END-IF
           END-PERFORM
           .
       20031-SURYO-HEN-EXT.
           EXIT.
      *
      *H28.11
      *****************************************************************
      *    共通情報出力処理(ver.2.1)
      *****************************************************************
       100-HEAD21-SYORI-SEC             SECTION.
      *
      *    患者情報
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               INITIALIZE                  PTINF-REC
           END-IF
      *
      *    バージョン情報
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "JAHISTC04"         TO  TBL-QRCSV-DATA (1)
           MOVE    "1"                 TO  TBL-QRCSV-DATA (2)
           MOVE    2                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *
      *    患者情報レコード
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "1"                 TO  TBL-QRCSV-DATA (1)
      *    患者氏名　拡張漢字変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    ORCSQRCSV62-NAME    TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-OUT-INPUT(1:40)
                                       =   ORCSQRCSV62-NAME
      *        拡張漢字なし
               MOVE    ORCSQRCSV62-NAME    TO  TBL-QRCSV-DATA (2)
           ELSE
      *        拡張漢字あり　カナ氏名
               MOVE    ORCSQRCSV62-KANANAME
                                           TO  TBL-QRCSV-DATA (2)
           END-IF
      *    性別
           EVALUATE    ORCSQRCSV62-SEX
               WHEN    "男"
                   MOVE    "1"                 TO  TBL-QRCSV-DATA (3)
               WHEN    "女"
                   MOVE    "2"                 TO  TBL-QRCSV-DATA (3)
           END-EVALUATE
      *    患者生年月日
           MOVE    ORCSQRCSV62-BIRTHDAY    TO  TBL-QRCSV-DATA (4)
      *    自宅−郵便番号
           MOVE    PTINF-HOME-POST     TO  TBL-QRCSV-DATA (5)
      *    自宅−住所
           STRING  PTINF-HOME-ADRS
                   PTINF-HOME-BANTI    DELIMITED  BY  SPACE
                                       INTO    TBL-QRCSV-DATA (6)
           END-STRING
      *    自宅−昼電話番号
           IF      PTINF-HOME-TEL1     =   SPACE
               MOVE    PTINF-HOME-TEL2     TO  TBL-QRCSV-DATA (7)
           ELSE
               MOVE    PTINF-HOME-TEL1     TO  TBL-QRCSV-DATA (7)
           END-IF
      *    緊急連絡先（連絡先−名称、昼電話番号）
           STRING  PTINF-RENRAKU-NAME
                   PTINF-RENRAKU-TEL1  DELIMITED  BY  SPACE
                                       INTO    TBL-QRCSV-DATA (8)
           END-STRING
      *    カナ氏名
           MOVE    ORCSQRCSV62-KANANAME    TO  TBL-QRCSV-DATA (11)
           MOVE    11                      TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    患者特記事項
           IF     (PTINF-ALLERGY-1     =   SPACE )
             AND  (PTINF-ALLERGY-2     =   SPACE )
               CONTINUE
           ELSE
      *        アレルギーレコード
               MOVE    SPACE                   TO  TBL-QRCSV-AREA
               MOVE    "2"                     TO  TBL-QRCSV-DATA (1)
               MOVE    "1"                     TO  TBL-QRCSV-DATA (2)
               STRING  PTINF-ALLERGY-1
                       PTINF-ALLERGY-2     DELIMITED  BY  SPACE
                                           INTO    TBL-QRCSV-DATA (3)
               END-STRING
               MOVE    "1"                     TO  TBL-QRCSV-DATA (4)
               MOVE    4                       TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *
           IF     (PTINF-TABOO-1       =   SPACE )
             AND  (PTINF-TABOO-2       =   SPACE )
               CONTINUE
           ELSE
      *        禁忌レコード
               MOVE    SPACE                   TO  TBL-QRCSV-AREA
               MOVE    "2"                     TO  TBL-QRCSV-DATA (1)
               MOVE    "9"                     TO  TBL-QRCSV-DATA (2)
               STRING  PTINF-TABOO-1
                       PTINF-TABOO-2     DELIMITED  BY  SPACE
                                           INTO    TBL-QRCSV-DATA (3)
               END-STRING
               MOVE    "1"                     TO  TBL-QRCSV-DATA (4)
               MOVE    4                       TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *
      *    調剤年月日レコード
           MOVE    SPACE                   TO  TBL-QRCSV-AREA
           MOVE    "5"                     TO  TBL-QRCSV-DATA (1)
           MOVE    ORCSQRCSV62-SRYYMD      TO  TBL-QRCSV-DATA (2)
           MOVE    "1"                     TO  TBL-QRCSV-DATA (3)
           MOVE    3                       TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *
      *    医療機関コード
           MOVE    SPACE                   TO  TBL-QRCSV-AREA
           MOVE    "11"                    TO  TBL-QRCSV-DATA (1)
           MOVE    ORCSQRCSV62-HOSPNAME    TO  TBL-QRCSV-DATA (2)
           MOVE    ORCSQRCSV62-PREFNUM     TO  TBL-QRCSV-DATA (3)
           MOVE    "1"                     TO  TBL-QRCSV-DATA (4)
           MOVE    ORCSQRCSV62-HOSPCD      TO  TBL-QRCSV-DATA (5)
           MOVE    ORCSQRCSV62-HOSPPOST    TO  TBL-QRCSV-DATA (6)
           MOVE    ORCSQRCSV62-HOSPADRS    TO  TBL-QRCSV-DATA (7)
           MOVE    ORCSQRCSV62-HOSPTEL     TO  TBL-QRCSV-DATA (8)
           MOVE    "1"                     TO  TBL-QRCSV-DATA (9)
           MOVE    9                       TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *
      *    医師レコード
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "15"                TO  TBL-QRCSV-DATA (1)
      *    拡張漢字変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    ORCSQRCSV62-DR-NAME TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  TBL-QRCSV-DATA (2)
           MOVE    ORCSQRCSV62-HOSPTEL TO  TBL-QRCSV-DATA (3)
           MOVE    "1"                 TO  TBL-QRCSV-DATA (4)
           MOVE    4                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *
           .
       100-HEAD21-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
