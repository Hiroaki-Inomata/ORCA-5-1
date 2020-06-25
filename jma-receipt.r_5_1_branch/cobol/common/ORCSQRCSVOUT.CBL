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
       PROGRAM-ID.             ORCSQRCSVOUT.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 処方箋　ＱＲデータＣＳＶ出力
      *                      （ORCSQRCSV.CBLを複写）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  14/12/XX    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  16/03/XX  最低薬価テーブル対応
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
      *    ファイル指定領域
      *
       01  FILE-NAME1-PARA.
      **** 03  FILLER              PIC X(05)   VALUE   "/tmp/".
           03  FILE-HOSPNUM1       PIC 9(02)   VALUE   ZERO.
           03  FILE-FORM-ID1       PIC X(04)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ".".
           03  FILE-TERMID1        PIC X(32)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ".".
           03  FILE-SYOHMS1        PIC 9(08)   VALUE   ZERO.
           03  FILLER              PIC X(07)   VALUE   ".dat".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-FILE1           PIC X(02).
           03  STS-FILE2           PIC X(02).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTPUBLIC        PIC 9(01).
           03  FLG-GENERICPRICE    PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
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
           03  IDXA                PIC 9(04).
      *
           03  TBL-MAX             PIC 9(04).
           03  IDX-B2              PIC 9(03).
      *
           03  IDXF                PIC 9(04).
           03  IDXF2               PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDS                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-POINT-AREA.
               05  WRK-PO-ST           PIC 9(05).
               05  WRK-PO-ED           PIC 9(05).
               05  WRK-CNT             PIC 9(05).
           03  WRK-DATA                PIC X(140).
      *
           03  WRK-RP1                 PIC 9(03).
           03  WRK-RP2-1               PIC 9(02).
           03  WRK-RP2                 PIC 9(02).
           03  WRK-RP-11               PIC 9(02).
      *
           03  WRK-H-RP1               PIC X(03).
           03  WRK-H-RP2               PIC X(03).
           03  WRK-H-RP2-1             PIC X(03).
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
           03  WRK-QR-KOHDATA.
               05  WRK-RJ-FTNJA        PIC X(08).
               05  WRK-RJ-JKYSNUM      PIC X(07).
      *
               05  WRK-QR-KOU-G        OCCURS   4.
                   07  WRK-QR-FTNJA        PIC X(08).
                   07  WRK-QR-JKYSNUM      PIC X(07).
               05  WRK-QR-FTNJA-N      PIC X(40).
               05  WRK-QR-JKYSNUM-N    PIC X(40).
      *    薬品名称
           03  WRK-NAME.
               05  WRK-NAME-1          PIC X(80).
               05  WRK-NAME-2          PIC X(20).
      *H24.8
           03  WRK-YAKKAKJNCD          PIC X(12).
           03  WRK-GENERIC-STYMD       PIC X(08).
           03  WRK-GENERIC-EDYMD       PIC X(08).
      *
           03  WRK-KEI-SURYO           PIC 9(05)V9(05).
      *
      *    CSVデータ名称
           03  WRK-CSV-FILENAME        PIC X(1024).
      *
           03  WRK-DAYCNT              PIC 9(02).
      *
           03  WRK-PTNUM               PIC X(15).
           03  WRK-BIKO-FILE           PIC X(100).
      *
      *    ＱＲテーブル
       01  TBL-QRCSV-AREA.
           03  TBL-QRCSV-REC.
               05  TBL-QRCSV-DATA        PIC X(140)  OCCURS  11.
      *
      *    一般名例外コードテーブル
           COPY    "CMGENERICTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    地域連携情報
           COPY  "CPSK9101.INC".
      *
      *    患者地域連携テーブル
       01  PTPUBLIC-REC.
           COPY    "CPPTNUM-PUBLIC.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *H28.4
      *    最低薬価マスタ
       01  GENERICPRICE-REC.
           COPY    "CPGENERICPRICE.INC".
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
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＣＳＶファイル作成パラメタ
           COPY    "CPORCSFILEENCODE.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
           COPY    "ENUM-VALUE".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    処方箋共通パラメタ
           COPY    "CPORCSQRCSV.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSQRCSVAREA
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
           MOVE    ZERO                TO  ORCSQRCSV-ERRCD
           MOVE    SPACE               TO  ORCSQRCSV-CSVFILE
      *v4.8
      *    ginbee対応
           IF      SPA-MW              =   SPA-GINBEE
               MOVE    9               TO  ORCSQRCSV-ERRCD
               GO      TO      000-PROC-EXT
           END-IF
      *
      *    CSVデータファイル名取得
           PERFORM 100-CVSFILE-HEN-SEC
           IF      ORCSQRCSV-ERRCD NOT =   ZERO
               GO      TO      000-PROC-EXT
           END-IF
      *
      *    一時ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM1
           MOVE    "HC2Q"              TO  FILE-FORM-ID1
           MOVE    SPA-TERMID          TO  FILE-TERMID1
           MOVE    ORCSQRCSV-SYSTIME   TO  FILE-SYOHMS1
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
               MOVE    9               TO  ORCSQRCSV-ERRCD
           END-IF
      *
      *    ＤＢ検索処理
           PERFORM 1001-DBKENSAKU-SEC
      *    共通情報出力
           IF      ORCSQRCSV-ERRCD     =   ZERO
               PERFORM 100-HEAD-SYORI-SEC
           END-IF
      *    薬剤情報出力
           IF      ORCSQRCSV-ERRCD     =   ZERO
               PERFORM 200-MAIN-SYORI-SEC
           END-IF
      *
           CLOSE                       QRCSV-FILE
      *
           IF      ORCSQRCSV-ERRCD     =   ZERO
      *        CSVデータ（JIS）の作成
               PERFORM 290-CSVOUT-SYORI-SEC
           END-IF
      *
           IF      ORCSQRCSV-ERRCD     =   ZERO
      *        CSVファイル名編集
               MOVE    WRK-BIKO-FILE       TO  ORCSQRCSV-CSVFILE
           END-IF
      *
      *    データ削除
           MOVE    ZERO                TO  IF-RESULT
           MOVE    FILE-NAME1          TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    CSVデータファイル名取得処理
      *****************************************************************
       100-CVSFILE-HEN-SEC             SECTION.
      *
      *    ＣＶＳデータ保存場所
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    "9103"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-9101-REC
           ELSE
               MOVE    SPACE               TO  SYS-9101-REC
               INITIALIZE                      SYS-9101-REC
           END-IF
           IF      SYS-9101-COOPBASE   =   "1"
                                       OR  "2"
               MOVE    SYS-9101-FILE-DIR   TO  WRK-CSV-FILENAME
           ELSE
               MOVE    9                   TO  ORCSQRCSV-ERRCD
               GO      TO      100-CVSFILE-HEN-EXT
           END-IF
      *
      *    地域連携データ検索
           INITIALIZE                  PTPUBLIC-REC
           MOVE    SPA-HOSPNUM         TO  PTPUBLIC-HOSPNUM
           MOVE    SPA-PTID            TO  PTPUBLIC-PTID
      *
           MOVE    PTPUBLIC-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-PTPUBLIC-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTPUBLIC
               INITIALIZE                  PTPUBLIC-REC
           END-IF
           MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    情報提供同意有無
           IF     (PTPUBLIC-AGREEMENT  =   "0"  )
             AND  (SYS-9101-COOPBASE   =   "1"  )
      *        同意しない　終了
               MOVE    9                   TO  ORCSQRCSV-ERRCD
               GO      TO      100-CVSFILE-HEN-EXT
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    患者番号変換（６桁以下は６桁とする）
           PERFORM VARYING    IDX      FROM    15  BY  -1
                   UNTIL     (IDX      <   1    )
                          OR (IDX-MAX  >   ZERO )
               IF      ORCSQRCSV-PTNUM(IDX:1)  NOT =   SPACE
                   MOVE    IDX             TO  IDX-MAX
               END-IF
           END-PERFORM
           IF      IDX-MAX             <   6
               MOVE    ZERO                TO  WRK-PTNUM
               COMPUTE IDS                 =   6    -  IDX-MAX  +  1
               MOVE    ORCSQRCSV-PTNUM (1:IDX-MAX)
                                           TO  WRK-PTNUM(IDS:)
           ELSE
               MOVE    ORCSQRCSV-PTNUM     TO  WRK-PTNUM
           END-IF
      *
           MOVE    SPACE               TO  FILE-NAME2
           STRING  WRK-CSV-FILENAME
                   "/"
                   ORCSQRCSV-HOSPCD
                   "-"
                   WRK-PTNUM
                   "-"
                   SMCNDATE-YMD
                   SYS-TIME
                   "-qr.dat"
                                       DELIMITED  BY  SPACE
                                       INTO   FILE-NAME2
           END-STRING
      *    備考レコード用
           MOVE    SPACE               TO  WRK-BIKO-FILE
           STRING  ORCSQRCSV-HOSPCD
                   "-"
                   WRK-PTNUM
                   "-"
                   SMCNDATE-YMD
                   SYS-TIME
                   "-qr"
                                       DELIMITED  BY  SPACE
                                       INTO   WRK-BIKO-FILE
           END-STRING
           .
       100-CVSFILE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSVデータ（JIS）作成処理
      *****************************************************************
       290-CSVOUT-SYORI-SEC             SECTION.
      *
      *    CSVデータ（JIS）の作成
           INITIALIZE                      ORCSFILEENCODEAREA
           MOVE    FILE-NAME1          TO  FLEN-INFILENAME
           MOVE    FILE-NAME2          TO  FLEN-OUTFILENAME
           MOVE    "1"                 TO  FLEN-OPTION
           CALL    "orcfileencode"     USING
                                           ORCSFILEENCODEAREA
           IF      FLEN-RET        NOT =   ZERO
               MOVE    9               TO  ORCSQRCSV-ERRCD
           END-IF
      *
           .
       290-CSVOUT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    共通情報出力処理
      *****************************************************************
       100-HEAD-SYORI-SEC             SECTION.
      *
      *    バージョン情報
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "JAHIS2"            TO  TBL-QRCSV-DATA (1)
           IF     (SPA-SRYYMD          >=  "20160401" )
      *    残量確認指示
            AND   (ORCSQRCSV-ZANSIJI   =   "1"
                                       OR  "2"   )
               MOVE    "JAHIS3"            TO  TBL-QRCSV-DATA (1)
           END-IF
      *H30.4
           IF     (SPA-SRYYMD          >=  "20180401" )
               MOVE    "JAHIS5"            TO  TBL-QRCSV-DATA (1)
           END-IF
      *
           MOVE    1                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    医療機関コード
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "1"                 TO  TBL-QRCSV-DATA (1)
           MOVE    "1"                 TO  TBL-QRCSV-DATA (2)
           MOVE    ORCSQRCSV-HOSPCD    TO  TBL-QRCSV-DATA (3)
           MOVE    ORCSQRCSV-PREFNUM   TO  TBL-QRCSV-DATA (4)
           MOVE    ORCSQRCSV-HOSPNAME  TO  TBL-QRCSV-DATA (5)
           MOVE    5                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    医療機関所在地レコード
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "2"                 TO  TBL-QRCSV-DATA (1)
           MOVE    ORCSQRCSV-HOSPPOST  TO  TBL-QRCSV-DATA (2)
           MOVE    ORCSQRCSV-HOSPADRS  TO  TBL-QRCSV-DATA (3)
           MOVE    3                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    医療機関電話レコード
           IF      ORCSQRCSV-HOSPTEL   NOT =   SPACE
               MOVE    SPACE               TO  TBL-QRCSV-AREA
               MOVE    "3"                 TO  TBL-QRCSV-DATA (1)
               MOVE    ORCSQRCSV-HOSPTEL   TO  TBL-QRCSV-DATA (2)
               MOVE    2                   TO  TBL-MAX
      *H30.4
      *        FAX
               IF      SPA-SRYYMD          >=  "20180401"
                   MOVE    ORCSQRCSV-HOSPFAX    TO  TBL-QRCSV-DATA (3)
                   MOVE    3                    TO  TBL-MAX
               END-IF
               PERFORM 400-DATAOUT-SEC
           END-IF
      *    診療科
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "4"                 TO  TBL-QRCSV-DATA (1)
           IF      ORCSQRCSV-RECESRYKA =   SPACE  OR  ZERO
               MOVE    "1"                 TO  TBL-QRCSV-DATA (2)
           ELSE
               MOVE    "2"                 TO  TBL-QRCSV-DATA (2)
               MOVE    ORCSQRCSV-RECESRYKA TO  TBL-QRCSV-DATA (3)
           END-IF
           MOVE    ORCSQRCSV-SRYKANAME TO  TBL-QRCSV-DATA (4)
           MOVE    4                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    医師レコード
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "5"                 TO  TBL-QRCSV-DATA (1)
      *    医師コード
           MOVE    ORCSQRCSV-DR-CD     TO  TBL-QRCSV-DATA (2)
      **** MOVE    ORCSQRCSV-DR-KANANAME
      ****                             TO  TBL-QRCSV-DATA (3)
      *    拡張漢字変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    ORCSQRCSV-DR-NAME   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  TBL-QRCSV-DATA (4)
           MOVE    4                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *
      *    患者氏名レコード
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "11"                TO  TBL-QRCSV-DATA (1)
      *    患者番号
           MOVE    WRK-PTNUM           TO  TBL-QRCSV-DATA (2)
      **   MOVE    ORCSQRCSV-NAME(1:40)
      **                               TO  TBL-QRCSV-DATA (3)
      *    拡張漢字変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    ORCSQRCSV-NAME      TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT(1:40)
                                       TO  TBL-QRCSV-DATA (3)
      *
           MOVE    ORCSQRCSV-KANANAME  TO  TBL-QRCSV-DATA (4)
           MOVE    4                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    患者性別レコード
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "12"                TO  TBL-QRCSV-DATA (1)
           MOVE    ORCSQRCSV-SEX       TO  TBL-QRCSV-DATA (2)
           MOVE    2                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    患者生年月日レコード
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "13"                TO  TBL-QRCSV-DATA (1)
           MOVE    ORCSQRCSV-BIRTHDAY  TO  TBL-QRCSV-DATA (2)
           MOVE    2                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    一部負担区分レコード
           IF      ORCSQRCSV-BIKO-FTN  NOT =   SPACE
               MOVE    SPACE               TO  TBL-QRCSV-AREA
               MOVE    ORCSQRCSV-BIKO-FTN  TO  TBL-QRCSV-DATA (2)
               IF      TBL-QRCSV-DATA (2)  NOT =   SPACE
                   MOVE    "14"                TO  TBL-QRCSV-DATA (1)
                   MOVE    2                   TO  TBL-MAX
                   PERFORM 400-DATAOUT-SEC
               END-IF
           END-IF
      *    保険種別レコード
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "21"                TO  TBL-QRCSV-DATA (1)
           EVALUATE    COMB-HKNNUM
      *       国保
              WHEN     "060"
              WHEN     "067"
              WHEN     "068"
              WHEN     "069"
                  MOVE    "2"          TO  TBL-QRCSV-DATA (2)
      *       労災
              WHEN     "971"
                  MOVE    "3"          TO  TBL-QRCSV-DATA (2)
      *       自賠責
              WHEN     "973"
                  MOVE    "4"          TO  TBL-QRCSV-DATA (2)
      *       公害
              WHEN     "975"
                  MOVE    "5"          TO  TBL-QRCSV-DATA (2)
      *       自費
              WHEN     "980"  THRU  "989"
                  MOVE    "6"          TO  TBL-QRCSV-DATA (2)
      *       後期高齢者
              WHEN     "039"
              WHEN     "040"
                  MOVE    "7"          TO  TBL-QRCSV-DATA (2)
      *       医保、公費
              WHEN     OTHER
                  MOVE    "1"          TO  TBL-QRCSV-DATA (2)
           END-EVALUATE
           MOVE    2                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    労災・自賠責・自費は保険者・記号番号クリア
           IF     (COMB-HKNNUM         =   "971"
                                       OR  "973" )
               OR (COMB-HKNNUM (1:2)   =   "98"  )
               MOVE    SPACE               TO  PTHKN-HKNJANUM
               MOVE    SPACE               TO  PTHKN-KIGO
               MOVE    SPACE               TO  PTHKN-NUM
               MOVE    "1"                 TO  PTHKN-HONKZKKBN
           END-IF
      *    公費単独は本人とする
           IF     (COMB-HKNNUM         =   SPACE )
             AND  (COMB-KOHHKNNUM (1)  NOT =   SPACE )
               MOVE    "1"                 TO  PTHKN-HONKZKKBN
           END-IF
      *    後期高齢者は記号クリア
           IF      COMB-HKNNUM         =   "039"
               MOVE    SPACE               TO  PTHKN-KIGO
           END-IF
      *    保険者番号レコード
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "22"                TO  TBL-QRCSV-DATA (1)
           MOVE    PTHKN-HKNJANUM      TO  TBL-QRCSV-DATA (2)
           MOVE    2                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    記号・番号レコード
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "23"                TO  TBL-QRCSV-DATA (1)
           MOVE    PTHKN-KIGO          TO  TBL-QRCSV-DATA (2)
           MOVE    PTHKN-NUM           TO  TBL-QRCSV-DATA (3)
           MOVE    PTHKN-HONKZKKBN     TO  TBL-QRCSV-DATA (4)
           MOVE    4                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    職務上の事由レコード
      *        船員,共済
           IF      PTHKN-HKNNUM        =   "002"
                                       OR  "031"
                                       OR  "032"
                                       OR  "033"
                                       OR  "034"
               MOVE    SPACE               TO  TBL-QRCSV-AREA
               EVALUATE    PTHKN-HOJOKBN
                   WHEN    "1"
                   WHEN    "A"
                   WHEN    "G"
      *R01.6
                   WHEN    "D"
      *                職務
                       MOVE    "1"         TO  TBL-QRCSV-DATA (2)
                   WHEN    "2"
                   WHEN    "B"
                   WHEN    "H"
      *R01.6
                   WHEN    "E"
      *                下船
                       MOVE    "2"         TO  TBL-QRCSV-DATA (2)
                   WHEN    "3"
                   WHEN    "C"
                   WHEN    "I"
      *R01.6
                   WHEN    "F"
      *                通勤
                       MOVE    "3"         TO  TBL-QRCSV-DATA (2)
               END-EVALUATE
               IF      TBL-QRCSV-DATA (2)  NOT =   SPACE
                   MOVE    "25"                TO  TBL-QRCSV-DATA (1)
                   MOVE    2                   TO  TBL-MAX
                   PERFORM 400-DATAOUT-SEC
               END-IF
           END-IF
      *
      *    老人保健レコード
           IF      WRK-RJ-FTNJA    NOT =   SPACE
               MOVE    SPACE               TO  TBL-QRCSV-AREA
               MOVE    "26"                TO  TBL-QRCSV-DATA (1)
               MOVE    WRK-RJ-FTNJA        TO  TBL-QRCSV-DATA (2)
               MOVE    WRK-RJ-JKYSNUM      TO  TBL-QRCSV-DATA (3)
               MOVE    3                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *    公費１レコード
           IF      WRK-QR-FTNJA (1)    NOT =   SPACE
               MOVE    SPACE               TO  TBL-QRCSV-AREA
               MOVE    "27"                TO  TBL-QRCSV-DATA (1)
               MOVE    WRK-QR-FTNJA (1)    TO  TBL-QRCSV-DATA (2)
               MOVE    WRK-QR-JKYSNUM(1)   TO  TBL-QRCSV-DATA (3)
               MOVE    3                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *    公費２レコード
           IF      WRK-QR-FTNJA (2)    NOT =   SPACE
               MOVE    SPACE               TO  TBL-QRCSV-AREA
               MOVE    "28"                TO  TBL-QRCSV-DATA (1)
               MOVE    WRK-QR-FTNJA (2)    TO  TBL-QRCSV-DATA (2)
               MOVE    WRK-QR-JKYSNUM(2)   TO  TBL-QRCSV-DATA (3)
               MOVE    3                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *    公費３レコード
           IF      WRK-QR-FTNJA (3)    NOT =   SPACE
               MOVE    SPACE               TO  TBL-QRCSV-AREA
               MOVE    "29"                TO  TBL-QRCSV-DATA (1)
               MOVE    WRK-QR-FTNJA (3)    TO  TBL-QRCSV-DATA (2)
               MOVE    WRK-QR-JKYSNUM(3)   TO  TBL-QRCSV-DATA (3)
               MOVE    3                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *    特殊公費レコード
           IF      WRK-QR-FTNJA-N      NOT =   SPACE
               MOVE    SPACE               TO  TBL-QRCSV-AREA
               MOVE    "30"                TO  TBL-QRCSV-DATA (1)
               MOVE    WRK-QR-FTNJA-N      TO  TBL-QRCSV-DATA (2)
               MOVE    WRK-QR-JKYSNUM-N    TO  TBL-QRCSV-DATA (3)
               MOVE    3                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *    交付日レコード
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "51"                TO  TBL-QRCSV-DATA (1)
           MOVE    SPA-SRYYMD          TO  TBL-QRCSV-DATA (2)
           MOVE    2                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *
      *    麻薬施用レコード
           IF      ORCSQRCSV-MAYAKU    =   1
               MOVE    SPACE               TO  TBL-QRCSV-AREA
               MOVE    "61"                TO  TBL-QRCSV-DATA (1)
      *        麻薬施用コード
               MOVE    ORCSQRCSV-MAYAKULICENSE
                                           TO  TBL-QRCSV-DATA (2)
               MOVE    ORCSQRCSV-ADRS      TO  TBL-QRCSV-DATA (3)
               MOVE    ORCSQRCSV-TEL       TO  TBL-QRCSV-DATA (4)
               MOVE    4                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *H28.4
      *    残量確認指示
           IF      ORCSQRCSV-ZANSIJI   =   "1"
                                       OR  "2"
               MOVE    SPACE               TO  TBL-QRCSV-AREA
               MOVE    "62"                TO  TBL-QRCSV-DATA (1)
      *        残量確認指示
               MOVE    ORCSQRCSV-ZANSIJI   TO  TBL-QRCSV-DATA (2)
               MOVE    2                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *H30.4
      *    分割指示レコード
           IF     (SPA-SRYYMD              >=  "20180401" )
             AND  (ORCSQRCSV-BUNKATU-MAX   >   1         )
               MOVE    SPACE               TO  TBL-QRCSV-AREA
               MOVE    "63"                TO  TBL-QRCSV-DATA (1)
      *        分割回数
               MOVE    ORCSQRCSV-BUNKATU-MAX
                                           TO  TBL-QRCSV-DATA (2)
      *        分割回
               MOVE    ORCSQRCSV-BUNKATU-CNT
                                           TO  TBL-QRCSV-DATA (3)
               MOVE    3                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *
      *    備考レコード
           MOVE    ZERO                TO  IDX-B2
           PERFORM VARYING     IDX-B   FROM    1   BY  1
                   UNTIL      (IDX-B   >   20  )
                           OR (ORCSQRCSV-BIKOU (IDX-B) =   SPACE)
               ADD     1                   TO  IDX-B2
               MOVE    SPACE               TO  TBL-QRCSV-AREA
               MOVE    "81"                TO  TBL-QRCSV-DATA (1)
               MOVE    IDX-B2              TO  WRK-ZZZ
               PERFORM 20011-SUJIHEN-SEC
               MOVE    WRK-H-DATA          TO  TBL-QRCSV-DATA (2)
      *        種別
               IF      ORCSQRCSV-BIKOU-KBN (IDX-B)  NOT =   SPACE
                   MOVE    ORCSQRCSV-BIKOU-KBN (IDX-B)
                                           TO  TBL-QRCSV-DATA (3)
               END-IF
               MOVE    ORCSQRCSV-BIKOU (IDX-B)
                                           TO  TBL-QRCSV-DATA (4)
               MOVE    4                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-PERFORM
      *!!
      *    連携用コード
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "81"                TO  TBL-QRCSV-DATA (1)
           MOVE    "999"               TO  TBL-QRCSV-DATA (2)
           MOVE    "51"                TO  TBL-QRCSV-DATA (3)
           MOVE    WRK-BIKO-FILE       TO  TBL-QRCSV-DATA (4)
           MOVE    4                   TO  TBL-MAX
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
      *    保険組合せマスター検索
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    ORCSQRCSV-HKNCOMBINUM
                                       TO  COMB-HKNCOMBINUM
           PERFORM 910-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   1
                INITIALIZE                      HKNCOMBI-REC
           END-IF
      *    患者保険情報検索
           IF      COMB-HKNID          =   ZERO
               INITIALIZE                  PTHKNINF-REC
           ELSE
               INITIALIZE                      PTHKNINF-REC
               MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
               MOVE    SPA-PTID            TO  PTHKN-PTID
      *        主保険−ＩＤ
               MOVE    COMB-HKNID          TO  PTHKN-HKNID
               PERFORM 920-PTHKNINF-READ-SEC
               IF      FLG-PTHKNINF    NOT =   ZERO
                   INITIALIZE              PTHKNINF-REC
               END-IF
           END-IF
      *    公費保健情報編集処理
           PERFORM 1001-KOHHKN-HEN-SEC
      *
           .
       1001-DBKENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費保健情報編集処理
      *****************************************************************
       1001-KOHHKN-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-QR-KOHDATA
           MOVE    ZERO                TO  IDX-H
      *
           PERFORM VARYING    IDX-K    FROM    1   BY  1
                   UNTIL     (IDX-K    >   4   )
                         OR  (ORCSQRCSV-KOHID (IDX-K)  =   ZERO )
                         OR  (ORCSQRCSV-ERRCD  NOT =   ZERO     )
               INITIALIZE                  PTKOHINF-REC
               MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
               MOVE    SPA-PTID            TO  PTKOH-PTID
               MOVE    ORCSQRCSV-KOHID (IDX-K)
                                           TO  PTKOH-KOHID
               PERFORM 940-PTKOHINF-READ-SEC
      *        負担者番号がない時、エラーとする
               IF      PTKOH-FTNJANUM      =   SPACE
                   IF      PTKOH-JKYSNUM       =   SPACE
      *                受給者番号もないとき、対象外
                       CONTINUE
                   ELSE
                       MOVE    1                   TO  ORCSQRCSV-ERRCD
                   END-IF
               ELSE
                  IF      PTKOH-KOHNUM            =   "027"
      *            老人保健
                       MOVE    PTKOH-FTNJANUM      TO  WRK-RJ-FTNJA
                       MOVE    PTKOH-JKYSNUM       TO  WRK-RJ-JKYSNUM
                   ELSE
      *            公費
                       PERFORM 1001-KOHCHEK-SEC
                   END-IF
               END-IF
           END-PERFORM
           .
       1001-KOHHKN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費区分チェック処理
      *****************************************************************
       1001-KOHCHEK-SEC             SECTION.
      *
      *    負担者番号
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    PTKOH-FTNJANUM      TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-MAX         >   8    )  OR
                  (KANACHK-SYUBETU     =   2    )
      *        ８桁以上か、全角
               MOVE    PTKOH-FTNJANUM      TO  WRK-QR-FTNJA-N
               MOVE    PTKOH-JKYSNUM       TO  WRK-QR-JKYSNUM-N
           ELSE
      *    受給者番号
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    PTKOH-JKYSNUM       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-MAX         >   7    )  OR
                      (KANACHK-SYUBETU     =   2    )
      *            ８桁以上か、全角
                   MOVE    PTKOH-FTNJANUM      TO  WRK-QR-FTNJA-N
                   MOVE    PTKOH-JKYSNUM       TO  WRK-QR-JKYSNUM-N
               ELSE
                   ADD     1                   TO  IDX-H
                   IF      IDX-H               <=  3
                       MOVE    PTKOH-FTNJANUM      TO  WRK-QR-FTNJA
                                                               (IDX-H)
                       MOVE    PTKOH-JKYSNUM       TO  WRK-QR-JKYSNUM
                                                                (IDX-H)
                   END-IF
               END-IF
           END-IF
           .
       1001-KOHCHEK-EXT.
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
      *    項目別編集処理
      *****************************************************************
       4001-KOUMOKU-HEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-CNT
           PERFORM VARYING IDX-D       FROM    1   BY  1
                   UNTIL  (IDX-D       >   140     )
               IF      WRK-DATA (IDX-D:1)  =   SPACE
                   MOVE    140             TO  IDX-D
               ELSE
                   ADD     1               TO  WRK-CNT
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
           MOVE    ZERO                TO  WRK-RP1
           MOVE    ZERO                TO  WRK-RP-11
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   50  )
                         OR   (ORCSQRCSV-ZAIKATA(IDX) =   SPACE )
      *        薬剤レコード判定
               PERFORM 2000-SRYCD-CHK-SEC
               IF      FLG-SYORI           =   1
      *            剤型レコード
                   PERFORM 2001-RP101-SEC
      *H30.4
      *            分割指示数量レコード
                   IF     (SPA-SRYYMD          >=  "20180401" )
                     AND  (ORCSQRCSV-GOKKAISU(IDX) >   ZERO   )
                       PERFORM 2001-RP102-SEC
                   END-IF
      *            用法レコード
                   PERFORM 2002-RP111-SYORI-SEC
      *
                   MOVE    ZERO                TO  WRK-RP2
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL      (IDX2    >   50  )
      *                薬剤レコード
                       IF     (ORCSQRCSV-SRYCD(IDX IDX2)(1:1) =  "6"
                                                              OR "7" )
                           OR (ORCSQRCSV-SRYCD(IDX IDX2)(1:3) =  "059")
                           PERFORM 2003-RP201-SEC
                       END-IF
      *                薬剤補足レコード
                       IF     (ORCSQRCSV-SRYCD(IDX IDX2)(1:1) =  "8" )
                           OR (ORCSQRCSV-SRYCD(IDX IDX2)(1:3) =  "008"
                                                              OR "099")
                       OR    ((ORCSQRCSV-SRYCD(IDX IDX2)(1:3) =  "001")
                          AND (ORCSQRCSV-YCOMMNTKBN(IDX IDX2) =   "2" ))
                           PERFORM 20030-RP281-SYORI-SEC
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤型レコード処理
      *****************************************************************
       2000-SRYCD-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-SYORI
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   50  )
                          OR  (FLG-SYORI   =   1   )
      *        薬剤レコード
               IF     (ORCSQRCSV-SRYCD(IDX IDX2)(1:1) =  "6"
                                                      OR "7" )
                   OR (ORCSQRCSV-SRYCD(IDX IDX2)(1:3) =  "059")
                   MOVE    1               TO  FLG-SYORI
               END-IF
           END-PERFORM
           .
       2000-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤型レコード処理
      *****************************************************************
       2001-RP101-SEC            SECTION.
      *
           ADD     1                   TO  WRK-RP1
           MOVE    WRK-RP1             TO  WRK-ZZZ
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  WRK-H-RP1
      *
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "101"               TO  TBL-QRCSV-DATA (1)
           MOVE    WRK-H-RP1           TO  TBL-QRCSV-DATA (2)
           MOVE    ORCSQRCSV-ZAIKATA (IDX)
                                       TO  TBL-QRCSV-DATA (3)
           MOVE    ORCSQRCSV-KAISU (IDX)   TO  WRK-ZZZ
      *H30.4
      *    分割指示の時は、総量
           IF     (SPA-SRYYMD          >=  "20180401" )
             AND  (ORCSQRCSV-GOKKAISU(IDX) >   ZERO   )
               MOVE    ORCSQRCSV-GOKKAISU (IDX)    TO  WRK-ZZZ
           END-IF
      *
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  TBL-QRCSV-DATA (5)
           MOVE    5                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
           .
       2001-RP101-EXT.
           EXIT.
      *
      *H30.4
      *****************************************************************
      *    分割指示数量レコード処理
      *****************************************************************
       2001-RP102-SEC            SECTION.
      *
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "102"               TO  TBL-QRCSV-DATA (1)
           MOVE    WRK-H-RP1           TO  TBL-QRCSV-DATA (2)
           MOVE    ORCSQRCSV-KAISU (IDX)   TO  WRK-ZZZ
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  TBL-QRCSV-DATA (3)
           MOVE    ORCSQRCSV-GOKKAISU(IDX) TO  WRK-ZZZ
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  TBL-QRCSV-DATA (4)
           MOVE    4                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
           .
       2001-RP102-EXT.
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
      *    用法レコード処理
      *****************************************************************
       2002-RP111-SYORI-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-RP-11
      *    用法レコードは最後のコードとする
           PERFORM VARYING     IDX2    FROM    50  BY  -1
                   UNTIL      (IDX2    <   1    )
                         OR   (WRK-RP-11   >   ZERO  )
      *        用法レコード
               IF     (ORCSQRCSV-SRYCD(IDX IDX2)(1:3) =   "001")
               AND    (ORCSQRCSV-YCOMMNTKBN(IDX IDX2) =   "1"
                                                      OR  "0" )
                   PERFORM 20021-RP111-SEC
                   MOVE    SPACE       TO  ORCSQRCSV-SRYCD(IDX IDX2)
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   50   )
      *        用法補助レコード
               IF     (ORCSQRCSV-SRYCD(IDX IDX2)(1:3) =  "001")
               AND    (ORCSQRCSV-YCOMMNTKBN(IDX IDX2) =   "1"
                                                      OR  "0" )
                   PERFORM 20022-RP181-SEC
               END-IF
           END-PERFORM
      *    用法がない時、最後のコメントを用法とする
           IF      WRK-RP-11           =   ZERO
               PERFORM VARYING     IDX2    FROM    50  BY  -1
                       UNTIL      (IDX2    <   1   )
                             OR   (WRK-RP-11   >   ZERO  )
      *            用法レコード
                   IF     (ORCSQRCSV-SRYCD(IDX IDX2)(1:1) =  "8") OR
                          (ORCSQRCSV-SRYCD(IDX IDX2)(1:3) =  "008")
                   OR    ((ORCSQRCSV-SRYCD(IDX IDX2)(1:3) =  "001")
                      AND (ORCSQRCSV-YCOMMNTKBN(IDX IDX2) =   "2" ))
                       PERFORM 20021-RP111-SEC
                       MOVE    SPACE       TO  ORCSQRCSV-SRYCD(IDX IDX2)
                   END-IF
               END-PERFORM
           END-IF
      *?
      **** 用法がなければ空白
           IF      WRK-RP-11           =   ZERO
               MOVE    SPACE               TO  TBL-QRCSV-AREA
               MOVE    "111"               TO  TBL-QRCSV-DATA (1)
               MOVE    WRK-H-RP1           TO  TBL-QRCSV-DATA (2)
               MOVE    "1"                 TO  TBL-QRCSV-DATA (3)
               MOVE    6                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
           .
       2002-RP111-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    用法レコード処理
      *****************************************************************
       20021-RP111-SEC            SECTION.
      *
      *    内服の時、用法コードから１日回数を取得する
           MOVE    ZERO                TO  WRK-DAYCNT
           IF     (ORCSQRCSV-SRYCD(IDX IDX2)(1:3) =   "001")
           AND    (ORCSQRCSV-YCOMMNTKBN(IDX IDX2) =   "1"
                                                  OR  "0" )
           AND    (ORCSQRCSV-ZAIKATA (IDX)        =   "1" )
               MOVE    ORCSQRCSV-SRYCD(IDX IDX2)
                                          TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                   IF     (TNS-KENTAICOMMENT   =   0
                                               OR  1 )
                       PERFORM VARYING    IDXA FROM    1   BY  1
                               UNTIL      IDXA >   5
                           IF      TNS-SSTKIJUNCD (IDXA)   =   1
                               ADD     1           TO  WRK-DAYCNT
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
           END-IF
           IF      WRK-DAYCNT          =   ZERO
               MOVE    1                   TO  WRK-DAYCNT
           END-IF
      *
      *
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "111"               TO  TBL-QRCSV-DATA (1)
           MOVE    WRK-H-RP1           TO  TBL-QRCSV-DATA (2)
           MOVE    "1"                 TO  TBL-QRCSV-DATA (3)
           MOVE    ORCSQRCSV-TNS-NAME (IDX IDX2)
                                       TO  TBL-QRCSV-DATA (5)
      *    １日回数
           MOVE    WRK-DAYCNT          TO  WRK-ZZZ
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  TBL-QRCSV-DATA (6)
           MOVE    6                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
           ADD     1                   TO  WRK-RP-11
      *
           MOVE    WRK-RP-11           TO  WRK-ZZZ
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  WRK-H-RP-11
           .
       20021-RP111-EXT.
           EXIT.
      *****************************************************************
      *    用法補助レコード処理
      *****************************************************************
       20022-RP181-SEC            SECTION.
      *
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "181"               TO  TBL-QRCSV-DATA (1)
           MOVE    WRK-H-RP1           TO  TBL-QRCSV-DATA (2)
           MOVE    WRK-H-RP-11         TO  TBL-QRCSV-DATA (3)
           MOVE    ORCSQRCSV-TNS-NAME (IDX IDX2)
                                       TO  TBL-QRCSV-DATA (5)
           MOVE    5                   TO  TBL-MAX
      *H30.4
           IF      SPA-SRYYMD          >=  "20180401"
               MOVE    7                   TO  TBL-MAX
           END-IF
           PERFORM 400-DATAOUT-SEC
           ADD     1                   TO  WRK-RP-11
      *
           MOVE    WRK-RP-11           TO  WRK-ZZZ
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  WRK-H-RP-11
           .
       20022-RP181-EXT.
           EXIT.
      *****************************************************************
      *    薬剤レコード処理
      *****************************************************************
       2003-RP201-SEC            SECTION.
      *
           ADD     1                   TO  WRK-RP2
           MOVE    WRK-RP2             TO  WRK-ZZZ
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  WRK-H-RP2
      *
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "201"               TO  TBL-QRCSV-DATA (1)
           MOVE    WRK-H-RP1           TO  TBL-QRCSV-DATA (2)
           MOVE    WRK-H-RP2           TO  TBL-QRCSV-DATA (3)
           EVALUATE    ORCSQRCSV-SRYCD(IDX IDX2)(1:1)
               WHEN    "6"
                   MOVE    "1"             TO  TBL-QRCSV-DATA (4)
               WHEN    "7"
                   MOVE    "2"             TO  TBL-QRCSV-DATA (4)
               WHEN    OTHER
                   MOVE    SPACE           TO  TBL-QRCSV-DATA (4)
           END-EVALUATE
           IF     (ORCSQRCSV-SRYCD(IDX IDX2)(1:1)  =   "6"
                                                   OR  "7" )
             AND  (ORCSQRCSV-KISAIKBN (IDX IDX2)   =   ZERO )
               MOVE    "2"                 TO  TBL-QRCSV-DATA (5)
               MOVE    ORCSQRCSV-SRYCD(IDX IDX2)
                                           TO  TBL-QRCSV-DATA (6)
           ELSE
               MOVE    "1"                 TO  TBL-QRCSV-DATA (5)
           END-IF
      *
      *H24.4 から
      *    一般名記載
           IF     (SPA-SRYYMD          >=  "20120401" )  AND
                  (ORCSQRCSV-SRYCD(IDX IDX2)(1:1)  =   "6" )
             AND  (ORCSQRCSV-KISAIKBN (IDX IDX2)   =   1   )
               MOVE    "7"                 TO  TBL-QRCSV-DATA (5)
      *H24.8
      **       MOVE    ORCSQRCSV-YAKKAKJNCD(IDX IDX2)
      **                                   TO  TBL-QRCSV-DATA (6)
      *H28.4
               IF      SPA-SRYYMD      <   "20160401"
      *        一般名例外コード変換
                   PERFORM 20039-GENERICCD-HEN-SEC
               ELSE
      *            最低薬価テーブルより
                   PERFORM 20039-GENERICPRICE-HEN-SEC
               END-IF
               MOVE    WRK-YAKKAKJNCD      TO  TBL-QRCSV-DATA (6)
           END-IF
      *
      *H24.4 訂正
      **** MOVE    ORCSQRCSV-TNS-NAME (IDX IDX2)
      ****                             TO  TBL-QRCSV-DATA (7)
      *    薬品の名称は４０文字とする
           MOVE    ORCSQRCSV-TNS-NAME (IDX IDX2)
                                       TO  WRK-NAME
      *H26.12
           IF      WRK-NAME            =   SPACE
               MOVE    ORCSQRCSV-CSV-NAME (IDX IDX2)
                                           TO  WRK-NAME
           END-IF
      *
           MOVE    WRK-NAME-1          TO  TBL-QRCSV-DATA (7)
      *
           MOVE    ORCSQRCSV-SURYOU(IDX IDX2)  TO  WRK-SURYO
           PERFORM 20031-SURYO-HEN-SEC
           MOVE    WRK-SURYO-H         TO  TBL-QRCSV-DATA (8)
           MOVE    "1"                 TO  TBL-QRCSV-DATA (9)
           MOVE    ORCSQRCSV-TANINAME(IDX IDX2)
                                       TO  TBL-QRCSV-DATA (10)
           MOVE    10                  TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    単位変換レコード
           IF      ORCSQRCSV-KANZANCHI (IDX IDX2)  =   ZERO
                                                   OR  1
               CONTINUE
           ELSE
               MOVE    SPACE               TO  TBL-QRCSV-AREA
               MOVE    "211"               TO  TBL-QRCSV-DATA (1)
               MOVE    WRK-H-RP1           TO  TBL-QRCSV-DATA (2)
               MOVE    WRK-H-RP2           TO  TBL-QRCSV-DATA (3)
               COMPUTE WRK-SURYO-K         =   ORCSQRCSV-SURYOU
                                                          (IDX IDX2)
                                           /   ORCSQRCSV-KANZANCHI
                                                          (IDX IDX2)
               COMPUTE WRK-KANSAN          =   WRK-SURYO-K
                                           /   ORCSQRCSV-SURYOU
                                                          (IDX IDX2)
               MOVE    WRK-KANSAN         TO  WRK-SURYO
               PERFORM 20031-SURYO-HEN-SEC
               MOVE    WRK-SURYO-H         TO  TBL-QRCSV-DATA (4)
               MOVE    4                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *
      *H26.5
      *    １回服用量レコード
           IF     (ORCSQRCSV-SURYOU-SG (IDX IDX2)  NOT =   ZERO)
              AND (ORCSQRCSV-KAISU-SG  (IDX IDX2)  NOT =   ZERO)
               PERFORM 20032-RP241-SYORI-SEC
           END-IF
      *
      *
           MOVE    ZERO                TO   WRK-RP2-1
      *    前に破砕がある時
           COMPUTE IDX3                =   IDX2  -  1
           IF     (IDX2                >   1  )  AND
                  (ORCSQRCSV-SRYCD(IDX IDX3) =    "099209901" )
               PERFORM 20032-RP281-SEC
           END-IF
           .
       2003-RP201-EXT.
           EXIT.
      *
      *H28.4
      *****************************************************************
      *    一般名例外コード変換処理
      *****************************************************************
       20039-GENERICPRICE-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-YAKKAKJNCD
      *
           MOVE    SPACE               TO  GENERICPRICE-REC
           INITIALIZE                      GENERICPRICE-REC
           MOVE    ORCSQRCSV-YAKKAKJNCD(IDX IDX2)
                                       TO  GENERICPRICE-YAKKAKJNCD
           MOVE    SPA-SRYYMD          TO  GENERICPRICE-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  GENERICPRICE-YUKOEDYMD
           MOVE    GENERICPRICE-REC    TO  MCPDATA-REC
           MOVE    "tbl_generic_price" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_generic_price" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  GENERICPRICE-REC
      *            一般名コード
                   MOVE    GENERICPRICE-GECODE TO  WRK-YAKKAKJNCD
               ELSE
                   INITIALIZE                  GENERICPRICE-REC
                   MOVE    1                   TO  FLG-GENERICPRICE
               END-IF
           ELSE
               INITIALIZE                  GENERICPRICE-REC
               MOVE    1                   TO  FLG-GENERICPRICE
           END-IF
           MOVE    "tbl_generic_price" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-YAKKAKJNCD      =   SPACE
               STRING  ORCSQRCSV-YAKKAKJNCD(IDX IDX2)(1:9)
                       "ZZZ"               DELIMITED  BY  SIZE
                                           INTO    WRK-YAKKAKJNCD
               END-STRING
           END-IF
           .
       20039-GENERICPRICE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    一般名例外コード変換処理
      *****************************************************************
       20039-GENERICCD-HEN-SEC         SECTION.
      *
           SET     TBL-GEN             TO  1
           SEARCH      TBL-GENERIC-OCC     VARYING   TBL-GEN
                   AT  END
                       MOVE    SPACE            TO  WRK-YAKKAKJNCD
                       MOVE    SPACE            TO  WRK-GENERIC-STYMD
                       MOVE    SPACE            TO  WRK-GENERIC-EDYMD
                   WHEN    ORCSQRCSV-YAKKAKJNCD(IDX IDX2)
                                               =   TBL-YAKKAKJNCD
                                                           (TBL-GEN)
                       MOVE    TBL-GENERIC-CODE (TBL-GEN)
                                                TO  WRK-YAKKAKJNCD
                       MOVE    TBL-GENERIC-STYMD (TBL-GEN)
                                                TO  WRK-GENERIC-STYMD
                       MOVE    TBL-GENERIC-EDYMD (TBL-GEN)
                                                TO  WRK-GENERIC-EDYMD
           END-SEARCH
      *H25.4
           IF      WRK-YAKKAKJNCD  NOT =   SPACE
      *        有効期間判定
               IF    (SPA-SRYYMD           >=  WRK-GENERIC-STYMD)
                 AND (SPA-SRYYMD           <=  WRK-GENERIC-EDYMD)
                   CONTINUE
               ELSE
                   MOVE    SPACE           TO  WRK-YAKKAKJNCD
               END-IF
           END-IF
      *
           IF      WRK-YAKKAKJNCD      =   SPACE
               STRING  ORCSQRCSV-YAKKAKJNCD(IDX IDX2)(1:9)
                       "ZZZ"               DELIMITED  BY  SIZE
                                           INTO    WRK-YAKKAKJNCD
               END-STRING
           END-IF
           .
       20039-GENERICCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    １回服用量レコード作成処理
      *****************************************************************
       20032-RP241-SYORI-SEC         SECTION.
      *
           COMPUTE WRK-KEI-SURYO  =   ORCSQRCSV-SURYOU-SG (IDX IDX2)
                                  *   ORCSQRCSV-KAISU-SG  (IDX IDX2)
           IF      WRK-KEI-SURYO      =    ORCSQRCSV-SURYOU(IDX IDX2)
      *    服用量×服用回数＝数量
               MOVE    SPACE               TO  TBL-QRCSV-AREA
               MOVE    "241"               TO  TBL-QRCSV-DATA (1)
               MOVE    WRK-H-RP1           TO  TBL-QRCSV-DATA (2)
               MOVE    WRK-H-RP2           TO  TBL-QRCSV-DATA (3)
      *        １回服用量
               MOVE    ORCSQRCSV-SURYOU-SG (IDX IDX2)
                                           TO  WRK-SURYO
               PERFORM 20031-SURYO-HEN-SEC
               MOVE    WRK-SURYO-H         TO  TBL-QRCSV-DATA (4)
      *        １日服用回数
               MOVE    ORCSQRCSV-KAISU-SG (IDX IDX2)
                                           TO  WRK-SURYO
               PERFORM 20031-SURYO-HEN-SEC
               MOVE    WRK-SURYO-H         TO  TBL-QRCSV-DATA (5)
               MOVE    5                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           ELSE
      *        不均等レコード
               MOVE    SPACE               TO  TBL-QRCSV-AREA
               MOVE    "221"               TO  TBL-QRCSV-DATA (1)
               MOVE    WRK-H-RP1           TO  TBL-QRCSV-DATA (2)
               MOVE    WRK-H-RP2           TO  TBL-QRCSV-DATA (3)
      *        １回服用量
               MOVE    ORCSQRCSV-SURYOU-SG (IDX IDX2)
                                           TO  WRK-SURYO
               PERFORM 20031-SURYO-HEN-SEC
               MOVE    3                   TO  IDXF
               PERFORM VARYING    IDXF2    FROM    1   BY  1
                       UNTIL     (IDXF2    >   ORCSQRCSV-KAISU-SG
                                                         (IDX IDX2))
                              OR (IDXF2    >   5    )
                   ADD     1               TO  IDXF
                   PERFORM 20031-SURYO-HEN-SEC
                   MOVE    WRK-SURYO-H         TO  TBL-QRCSV-DATA(IDXF)
               END-PERFORM
               MOVE    IDXF                TO  TBL-MAX
      *H30.4
               IF      SPA-SRYYMD          >=  "20180401"
                   MOVE    13                  TO  TBL-MAX
               END-IF
               PERFORM 400-DATAOUT-SEC
           END-IF
           .
       20032-RP241-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬品補助レコード作成処理
      *****************************************************************
       20030-RP281-SYORI-SEC         SECTION.
      *
           IF     (ORCSQRCSV-SRYCD(IDX IDX2)   =   "099209901" )
               OR (WRK-RP2             =   ZERO  )
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
           ADD     1                   TO  WRK-RP2-1
           MOVE    WRK-RP2-1           TO  WRK-ZZZ
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  WRK-H-RP2-1
      *
           MOVE    SPACE               TO  TBL-QRCSV-AREA
           MOVE    "281"               TO  TBL-QRCSV-DATA (1)
           MOVE    WRK-H-RP1           TO  TBL-QRCSV-DATA (2)
           MOVE    WRK-H-RP2           TO  TBL-QRCSV-DATA (3)
           MOVE    WRK-H-RP2-1         TO  TBL-QRCSV-DATA (4)
      *H24.4
           IF      SPA-SRYYMD          >=  "20120401"
               EVALUATE    ORCSQRCSV-SRYCD(IDX IDX3)
               WHEN    "099209901"
      *            粉砕
                   MOVE    "2"         TO  TBL-QRCSV-DATA (5)
               WHEN    "099209903"
      *            後発医薬品へ変更不可
                   MOVE    "3"         TO  TBL-QRCSV-DATA (5)
               WHEN    "099209906"
      *            剤型変更不可
                   MOVE    "4"         TO  TBL-QRCSV-DATA (5)
               WHEN    "099209905"
      *            含量規格変更不可
                   MOVE    "5"         TO  TBL-QRCSV-DATA (5)
               END-EVALUATE
           END-IF
      *
           MOVE    ORCSQRCSV-TNS-NAME (IDX IDX3)
                                       TO  TBL-QRCSV-DATA (6)
           MOVE    6                   TO  TBL-MAX
      *H30.4
           IF      SPA-SRYYMD          >=  "20180401"
               MOVE    7                   TO  TBL-MAX
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
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
     *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       920-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTHKNINF-REC
                   MOVE    ZERO            TO  FLG-PTHKNINF
               ELSE
                   MOVE    1               TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       920-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    公費マスター読み込み
      *****************************************************************
       940-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTKOHINF-REC
                   MOVE    ZERO            TO  FLG-PTKOHINF
               ELSE
                   MOVE    1               TO  FLG-PTKOHINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       940-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ主キー読み込み処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-KEY10-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者地域連携データ読込処理
      *****************************************************************
       920-PTPUBLIC-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTPUBLIC
               MOVE    MCPDATA-REC         TO  PTPUBLIC-REC
           ELSE
               MOVE    1                   TO  FLG-PTPUBLIC
               INITIALIZE                  PTPUBLIC-REC
           END-IF
      *
           .
       920-PTPUBLIC-READ-EXT.
           EXIT.
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
