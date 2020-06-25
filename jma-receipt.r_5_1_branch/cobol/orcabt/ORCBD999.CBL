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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBD999.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 統計ＣＳＶサンプル
      *  コンポーネント名  : 日計表（診療年月日）
      *                      平成２０年４月以降のみＣＳＶ作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/09/18    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    収納ファイル
           SELECT  BD00801-FILE    ASSIGN   BD00801PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BD00801-ISAM-KEY
                                   FILE    STATUS  IS  STS-BD00801.
      *    エラーファイル
           SELECT  TOUKEIERR-FILE    ASSIGN  TOUKEIERR
                                   FILE    STATUS  IS  STS-TOUKEIERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    収納ファイル
       FD  BD00801-FILE.
       01  BD00801-REC.
           02  BD00801-ISAM-KEY.
               03  BD00801-ISAM-KEY-VAL        PIC X(100).
               03  BD00801-ISAM-KEY1 REDEFINES BD00801-ISAM-KEY-VAL.
                   05  BD00801-ISAM-KEY1-PTNUM      PIC X(20).
                   05  BD00801-ISAM-KEY1-PTID       PIC 9(10).
                   05  BD00801-ISAM-KEY1-DENPNUM    PIC 9(07).
                   05  BD00801-ISAM-KEY1-NYUGAIKBN  PIC X(01).
                   05  BD00801-ISAM-KEY1-FIL        PIC X(62).
               03  BD00801-ISAM-KEY2 REDEFINES BD00801-ISAM-KEY-VAL.
                   05  BD00801-ISAM-KEY2-NYUGAIKBN  PIC X(01).
                   05  BD00801-ISAM-KEY2-DENPNUM    PIC 9(07).
                   05  BD00801-ISAM-KEY2-FIL        PIC X(92).
           02  BD00801-PTNUM                        PIC X(20).
           02  BD00801-SYU-DAT.
           COPY    "CPSYUNOU.INC"  REPLACING  //SYU-//
                                   BY         //BD00801-SYU-//.
      *
      *    エラーファイル
       FD  TOUKEIERR-FILE.
       01  TOUKEIERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    収納ファイル
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //BD00801//.
      *
           COPY    "CPRECEERR.INC"
                                   REPLACING  //RECEERR//
                                   BY         //TOUKEIERR//.
      *
           COPY    "HCM41V02.INC"  REPLACING  //HCM41V02//
                                   BY         //HCM41//.
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-TOUKEIERR         PIC X(02).
           03  STS-BD00801         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  IDW2                PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-TOUKEIERR    PIC X(100).
           03  WRK-PARA-SYORIYMD   PIC X(08).
           03  WRK-PARA-PRTKBN     PIC X(01).
           03  WRK-PARA-SORTKBN    PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TOUKEIERR         PIC X(200).
           03  WRK-ERRMSG          PIC X(300).
      *
           03  WRK-SYORIYMDWH      PIC X(22).
           03  WRK-SYSYMDWH        PIC X(22).
           03  WRK-RENNUM          PIC 9(03).
           03  WRK-PTNUM           PIC X(20).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-DISCOUNT-HASU-KBN2   PIC X(01).
      *
           03  WRK-MONEY           PIC S9(08).
           03  WRK-JIHI            PIC S9(08).
      *
           03  WRK-RENNUM-X        PIC ZZ9.
           03  WRK-PAGE            PIC ZZ9.
      *
           03  WRK-Z8              PIC ZZZZZZZ9.
           03  WRK-Z9              PIC ZZZZZZZZ9.
           03  WRK-Z99             PIC --------9.
      *
      *    ＣＳＶデータ編集用
           03  WRK-REC             PIC X(20000).
           03  WRK-REC-LENGTH      PIC 9(05).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA        PIC X(20000).
               05  WRK-NUM         PIC -------------9.99999. 
               05  WRK-NAGASA      PIC 9(04).
               05  WRK-ST          PIC 9(02). 
               05  WRK-SYOSUU      PIC 9(01).
               05  WRK-END         PIC 9(01).
      *
           03  WRK-STOUKEICSV-PTID PIC 9(10).
           03  WRK-STOUKEICSV-NYUGAIKBN
                                   PIC X(01).
      *    出力先ファイル名
       01  WRK-TO-DATA.
           03  WRK-TO-DATA-HOSPNUM PIC 9(02).
           03  FILLER              PIC X(07)   VALUE   "nikkei_".
           03  WRK-TO-DATA-SRYYMD  PIC X(08).
           03  FILLER              PIC X(04)   VALUE   ".csv".
      *
       01  WRK-HEN-YMD-IN.
           03  WRK-HEN-YY-IN       PIC X(04).
           03  WRK-HEN-MM-IN       PIC X(02).
           03  WRK-HEN-DD-IN       PIC X(02).
       01  WRK-HEN-YMD-OT.
           03  WRK-HEN-YY-OT       PIC X(04).
           03  FILLER              PIC X(01)   VALUE   "/". 
           03  WRK-HEN-MM-OT       PIC X(02).
           03  FILLER              PIC X(01)   VALUE   "/". 
           03  WRK-HEN-DD-OT       PIC X(02).
      *
      *    ヘッダー情報
       01  CSV-HEAD-01.
           03  FILLER              PIC X(10)   VALUE   "作成年月日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "診療年月日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "伝票番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "カナ氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "性別".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "生年月日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "郵便番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "電話番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "保険点数".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "社保本人".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(16)   VALUE
              "社保本人（点数）".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "社保家族".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(16)   VALUE
               "社保家族（点数）".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "国保".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "国保（点数）".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "後期高齢".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(16)   VALUE
               "後期高齢（点数）".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "公費単独".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(16)   VALUE
               "公費単独（点数）".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "自費".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "自費（点数）".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "労災".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "自賠責".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "公害".
      *
       01  SUM-AREA                            VALUE   ZERO.
           03  SUM-OCC             OCCURS  11.
               05  SUM-KENSU       PIC 9(05).
               05  SUM-HKNTEN      PIC 9(08).
               05  SUM-MONEY       PIC S9(08).
      *
       01  WRK-CONS-AREA.
      *    自費保険番号
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980".
      *    労災保険番号
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971".
      *    自賠責保険番号
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".
      *    公害保険番号
           03  WRK-CONS-KOUGAI-HKNNUM   PIC X(03)   VALUE  "975".
      *    コンマ
           03  WRK-KUGIRI              PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO              PIC X(01)   VALUE   X"0D".
      *
      *----(03.04.01)--ADD-START---
       01  CONST-AREA.
           03  CONST-H190401           PIC X(08)   VALUE   "20070401".
      *----(03.04.01)--ADD-END-----
      *----(04.02.01)--ADD-START---
           03  CONST-H200401           PIC X(08)   VALUE   "20080401".
      *----(04.02.01)--ADD-END-----
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    収納マスタ
       01  SYU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    患者保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号マスタ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
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
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      ****************************************************************
       PROCEDURE               DIVISION
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
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SUM-AREA
           INITIALIZE                  SPA-AREA
                                       WRK-PARA
      *
           INITIALIZE                  TOUKEIERR
      *
           INITIALIZE                  WRK-HENSYU-AREA
           MOVE    ZERO            TO  WRK-NUM    
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                               INTO    LNK-PRTKANRI-RENNUM
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
                                       TOUKEIERR
                                       WRK-PARA-SYORIYMD
                                       WRK-PARA-PRTKBN
                                       WRK-PARA-SORTKBN
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM TO SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-HOSPNUM 
                                   TO JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
      *
           MOVE    "ORCBD999"      TO  JOB-PGID
           MOVE    "日計ＣＳＶ作成（診療年月日）"
                                   TO  JOB-SHELLMSG
      *
           PERFORM 800-ORCSJOB-SEC
      *
           MOVE    WRK-PARA-HOSPNUM TO BD00801PARA-HOSPNUM
           MOVE    "BD00801"       TO  BD00801PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  BD00801PARA-TERMID
      *
           MOVE    WRK-PARA-TOUKEIERR TO TOUKEIERR
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    BD00801PARA-BASENAME
                                   TO  SGETTEMP-BASENAMES (1)
           MOVE    TOUKEIERR       TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  BD00801PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  TOUKEIERR
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
      *    収納ファイル編集処理
           PERFORM 120-BD00801-HEN-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  WRK-SYSYMDWH
      *
      *    対象年月日編集
           MOVE    WRK-PARA-SYORIYMD
                                   TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  WRK-SYORIYMDWH
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納ファイル編集処理
      *****************************************************************
       120-BD00801-HEN-SEC             SECTION.
      *
           OPEN    OUTPUT  BD00801-FILE
      *
           PERFORM 900-SYUNOU-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
               INITIALIZE                  PTNUM-REC
               MOVE    SYU-HOSPNUM    TO  PTNUM-HOSPNUM
               MOVE    SYU-PTID      TO  PTNUM-PTID
               PERFORM 900-PTNUM-READ-SEC
      *
               MOVE    PTNUM-PTNUM   TO  BD00801-PTNUM
      *
               IF    ( WRK-PARA-SORTKBN  =     "1" )
                   MOVE    SYU-NYUGAIKBN TO  BD00801-ISAM-KEY2-NYUGAIKBN
                   MOVE    SYU-DENPNUM   TO  BD00801-ISAM-KEY2-DENPNUM
               ELSE
                   MOVE    PTNUM-PTNUM   TO  BD00801-ISAM-KEY1-PTNUM
                   MOVE    SYU-PTID      TO  BD00801-ISAM-KEY1-PTID
                   MOVE    SYU-DENPNUM   TO  BD00801-ISAM-KEY1-DENPNUM
                   MOVE    SYU-NYUGAIKBN TO  BD00801-ISAM-KEY1-NYUGAIKBN
               END-IF
      *
               MOVE    SYU-REC       TO  BD00801-SYU-DAT
      *
               WRITE   BD00801-REC
      *
               PERFORM 900-SYUNOU-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key44"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   BD00801-FILE
      *
           .
       120-BD00801-HEN-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    INPUT  BD00801-FILE
      *
           PERFORM 900-BD00801-READ-SEC
      *
      *    ＣＳＶ（ＨＥＡＤレコード）編集処理
           IF   ( FLG-END       =   ZERO          )
           AND  ( WRK-PARA-SYORIYMD
                                >=  CONST-H200401 )
               PERFORM 2001-CSV-HEAD-RECORD-HENSYU-SEC
           END-IF
      *
           PERFORM UNTIL   FLG-END     =   1
      *        帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
      *        帳票編集<明細>処理
               PERFORM 320-BODY-HEN-SEC
      *        帳票編集<合計>処理
               IF     FLG-END       =   1
                   PERFORM 330-TERM-HEN-SEC
               END-IF
      *        帳票印刷処理
               PERFORM 390-PRINT-OUT-SEC
           END-PERFORM
      *
           CLOSE   BD00801-FILE
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC            SECTION.
      *
           MOVE    ZERO            TO  CNT-LINE
           ADD     1               TO  CNT-PAGE
      *
           INITIALIZE                  HCM41
      *
           MOVE    "診療年月日"    TO  HCM41-MIDASI
      *
           MOVE    WRK-SYORIYMDWH  TO  HCM41-SRYYMD
           MOVE    WRK-SYSYMDWH    TO  HCM41-SYSYMD
      *
           MOVE    CNT-PAGE        TO  WRK-PAGE
           MOVE    WRK-PAGE        TO  HCM41-PAGE
      *
      *----(04.02.01)--ADD-START---
           IF      WRK-PARA-SYORIYMD   <   CONST-H200401
               MOVE    "社保本人"      TO  HCM41-HKNNAME(1)
               MOVE    "社保家族"      TO  HCM41-HKNNAME(2)
               MOVE    "社保老人"      TO  HCM41-HKNNAME(3)
               MOVE    "国保一般"      TO  HCM41-HKNNAME(4)
               MOVE    "国保老人"      TO  HCM41-HKNNAME(5)
               MOVE    "公費単独"      TO  HCM41-HKNNAME(6)
               MOVE    "自費"          TO  HCM41-HKNNAME(7)
               MOVE    "労災"          TO  HCM41-HKNNAME(8)
               MOVE    "自賠責"        TO  HCM41-HKNNAME(9)
               MOVE    "公害"          TO  HCM41-HKNNAME(10)
           ELSE
               MOVE    "社保本人"      TO  HCM41-HKNNAME(1)
               MOVE    "社保家族"      TO  HCM41-HKNNAME(2)
               MOVE    "国保"          TO  HCM41-HKNNAME(3)
               MOVE    "後期高齢"      TO  HCM41-HKNNAME(4)
               MOVE    "公費単独"      TO  HCM41-HKNNAME(5)
               MOVE    "自費"          TO  HCM41-HKNNAME(6)
               MOVE    "労災"          TO  HCM41-HKNNAME(7)
               MOVE    "自賠責"        TO  HCM41-HKNNAME(8)
               MOVE    "公害"          TO  HCM41-HKNNAME(9)
           END-IF
      *----(04.02.01)--ADD-END-----
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC            SECTION.
      *
           MOVE    ZERO            TO  CNT-LINE
      *
           PERFORM UNTIL   FLG-END     =   1
                   OR      CNT-LINE    =   30
      *    患者情報読み込み
      *         INITIALIZE                  PTINF-REC
      *         MOVE    SYU-HOSPNUM    TO  PTINF-HOSPNUM
      *         MOVE    SYU-PTID      TO  PTINF-PTID
      *         PERFORM 900-PTINF-READ-SEC
      *         IF    ( FLG-PTINF               =   ZERO )
      *          AND  ( PTINF-TSTPTNUMKBN   NOT =   "1" )
                   PERFORM 3201-BODY-HEN-SEC
      *         END-IF
      *
               PERFORM 900-BD00801-READ-SEC
      *
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3201-BODY-HEN-SEC            SECTION.
      *
           ADD     1               TO  CNT-LINE
      *    連番
           ADD     1               TO  WRK-RENNUM
           MOVE    WRK-RENNUM      TO  WRK-RENNUM-X
           MOVE    WRK-RENNUM-X    TO  HCM41-RENNUM(CNT-LINE)
      *    患者番号
           IF    ( BD00801-PTNUM (11:) =   SPACE )
               MOVE    BD00801-PTNUM   TO  HCM41-PTNUM(CNT-LINE)
           ELSE
               MOVE    BD00801-PTNUM   TO  HCM41-PTNUM2(CNT-LINE)
           END-IF
      *    氏名
           MOVE    PTINF-NAME      TO  HCM41-NAME(CNT-LINE)
      *    保険点数
           EVALUATE    SYU-SYUHKNNUM
               WHEN    WRK-CONS-JIBAI-HKNNUM
               WHEN    WRK-CONS-ROUSAI-HKNNUM
               WHEN    WRK-CONS-KOUGAI-HKNNUM
                   MOVE    ZERO        TO  SYU-TOTAL-HKNTEN
               WHEN    OTHER
                   MOVE    SYU-TOTAL-HKNTEN
                                       TO  WRK-Z8
                   MOVE    WRK-Z8      TO  HCM41-HKNTEN (CNT-LINE)
           END-EVALUATE
      *
      *----(04.02.01)--ADD-START---
               IF      WRK-PARA-SYORIYMD   <   CONST-H200401
                   PERFORM 3201-BODY-H200401-MAE-SEC
               ELSE
                   PERFORM 3201-BODY-H200401-ATO-SEC
      *            ＣＳＶ（データレコード）編集処理
                   PERFORM 2001-CSV-DATA-RECORD-HENSYU-SEC
               END-IF
      *----(04.02.01)--ADD-END-----
      *
           .
       3201-BODY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ表示処理
      *****************************************************************
       3201-PTHKNINF-HENSYU-SEC    SECTION.
      *
      *   保険組合せ読み出し
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SYU-HOSPNUM    TO  COMB-HOSPNUM
           MOVE    SYU-PTID      TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM
                                   TO  COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   ZERO
      *    患者保険情報読み込み
               INITIALIZE                  PTHKNINF-REC
               MOVE    SYU-HOSPNUM    TO  PTHKN-HOSPNUM
               MOVE    SYU-PTID      TO  PTHKN-PTID
               MOVE    COMB-HKNID      TO  PTHKN-HKNID
               PERFORM 900-PTHKNINF-READ-SEC
           END-IF
           .
       3201-PTHKNINF-HENSYU-EXT.
           EXIT.
      *
      *----(04.02.01)--ADD-START---
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3201-BODY-H200401-MAE-SEC   SECTION.
      *
      *    保険別金額（社保本人〜自賠責）
           MOVE    ZERO                TO  IDX
           EVALUATE    SYU-SYUHKNNUM
      *        公費単独
               WHEN    SPACE
                   MOVE    6                   TO  IDX
      *        公害
               WHEN    WRK-CONS-KOUGAI-HKNNUM
                   MOVE    10                  TO  IDX
      *        自賠責
               WHEN    WRK-CONS-JIBAI-HKNNUM
                   MOVE    9                   TO  IDX
      *        労災
               WHEN    WRK-CONS-ROUSAI-HKNNUM
                   MOVE    8                   TO  IDX
      ***      WHEN    WRK-CONS-JIHI-HKNNUM    
      ***          MOVE    7                   TO  IDX
      *        国保
               WHEN    "060"
               WHEN    "067"
               WHEN    "068"
               WHEN    "069"
      *            国保老人
                   IF      SYU-KOH1HKNNUM      =   "027"
                       MOVE    5                   TO  IDX
                   ELSE
      *                国保一般
                       MOVE    4                   TO  IDX
                   END-IF
               WHEN    OTHER
      *            自費
                   IF      SYU-SYUHKNNUM(1:2) = "98"
                       MOVE    7                   TO  IDX
                   ELSE
      *                社保老人
                       IF      SYU-KOH1HKNNUM      =   "027"
                           MOVE    3                   TO  IDX
                       ELSE
                           PERFORM 3201-PTHKNINF-HENSYU-SEC
      *                    社保本人・家族
                           EVALUATE    PTHKN-HONKZKKBN
                               WHEN    "1"
                                   MOVE    1                   TO  IDX
                               WHEN    "2"
                                   MOVE    2                   TO  IDX
                           END-EVALUATE
                       END-IF
                   END-IF
           END-EVALUATE
      *
           IF      IDX                 >   ZERO
      *        自費以外のとき自費分だけ集計
               IF      IDX             =   7
                   MOVE    ZERO            TO  WRK-JIHI
               ELSE
      *----(03.04.01)--UPD-START---
                   COMPUTE WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                       +   SYU-TOTAL-TGMONEY-TAX
                                       +   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
                                       +   SYU-RMSAGAKU
                                       +   SYU-SKYMONEY-SKJ-JIHI-KEI
                                       +   SYU-SKYMONEY-LIFE-JIHI-KEI
                   IF      SYU-SRYYMD  <   CONST-H190401
                       COMPUTE WRK-JIHI    =   WRK-JIHI 
                                           +   SYU-JIHI-TAX
                   END-IF
      *----(03.04.01)--UPD-START---
               END-IF
               IF    IDX  =  8  OR  9 OR 10
                   COMPUTE WRK-MONEY   =   SYU-RSIJIBAI-SKYMONEY
                                       -   WRK-JIHI
               ELSE
                   COMPUTE WRK-MONEY   =   SYU-SKYMONEY
                                       -   WRK-JIHI
                   IF   WRK-JIHI  NOT =  ZERO
                       IF  ((SYU-DISCOUNT-TEIRITU  NOT =  SPACE) AND
                            (SYU-DISCOUNT-RATE     NOT =  ZERO ))
                       OR  ( SYU-CHOSEI            NOT =  ZERO  )
                           CONTINUE
                       ELSE
                           COMPUTE WRK-MONEY   =   SYU-HKNTEKMONEY
                                               +   SYU-RJN-FTN
                                               +   SYU-KOH-FTN
                                               +   SYU-YKZ-FTN
                                               +   SYU-SKYMONEY-SKJ-KEI
                                               +   SYU-SKYMONEY-LIFE-KEI
                           COMPUTE WRK-JIHI    =   SYU-SKYMONEY
                                               -   WRK-MONEY
                       END-IF
                   END-IF
               END-IF
      *
               IF      WRK-MONEY       <   0
                   MOVE    WRK-MONEY   TO  WRK-Z99
                   MOVE    WRK-Z99     TO  HCM41-MONEY(IDX CNT-LINE)
               ELSE
                   MOVE    WRK-MONEY   TO  WRK-Z9
                   MOVE    WRK-Z9      TO  HCM41-MONEY(IDX CNT-LINE)
               END-IF
               IF      IDX             =   7
                   CONTINUE
               ELSE
                   IF      WRK-JIHI    >   ZERO
                       MOVE    WRK-JIHI    TO  WRK-Z9
                       MOVE    "*"         TO  WRK-Z9 (2:1)
                       MOVE    WRK-Z9      TO  HCM41-MONEY (7 CNT-LINE)
                   END-IF
               END-IF
           END-IF
      *
           ADD     SYU-TOTAL-HKNTEN  TO  SUM-HKNTEN (1)
           ADD     SYU-SKYMONEY      TO  SUM-MONEY  (1)
           ADD     1                   TO  SUM-KENSU  (1)
           IF      IDX                 >   ZERO
               COMPUTE IDY     =   IDX     +   1
               IF      IDX     <   8
                   ADD     SYU-TOTAL-HKNTEN
                                       TO  SUM-HKNTEN (IDY)
               END-IF
               ADD     WRK-MONEY       TO  SUM-MONEY(IDY)
               ADD     1               TO  SUM-KENSU(IDY)
               ADD     WRK-JIHI        TO  SUM-MONEY(8)
           END-IF
      *
           .
       3201-BODY-H200401-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3201-BODY-H200401-ATO-SEC   SECTION.
      *
      *    保険別金額（社保本人〜自賠責）
           MOVE    ZERO                TO  IDX
           EVALUATE    SYU-SYUHKNNUM
      *        公費単独
               WHEN    SPACE
                   MOVE    5                   TO  IDX
      *        公害
               WHEN    WRK-CONS-KOUGAI-HKNNUM
                   MOVE    9                   TO  IDX
      *        自賠責
               WHEN    WRK-CONS-JIBAI-HKNNUM
                   MOVE    8                   TO  IDX
      *        労災
               WHEN    WRK-CONS-ROUSAI-HKNNUM
                   MOVE    7                   TO  IDX
      *        国保
               WHEN    "060"
               WHEN    "067"
               WHEN    "068"
               WHEN    "069"
      *            国保老人
                   IF      SYU-KOH1HKNNUM      =   "027"
                       MOVE    4                   TO  IDX
                   ELSE
                       MOVE    3                   TO  IDX
                   END-IF
      *        後期高齢
               WHEN    "039"
               WHEN    "040"
                   MOVE    4                   TO  IDX
      *        その他
               WHEN    OTHER
      *            自費
                   IF      SYU-SYUHKNNUM(1:2) = "98"
                       MOVE    6                   TO  IDX
                   ELSE
      *                社保老人
                       IF      SYU-KOH1HKNNUM      =   "027"
                           MOVE    4                   TO  IDX
                       ELSE
      *                    社保
                           PERFORM 3201-PTHKNINF-HENSYU-SEC
                           EVALUATE    PTHKN-HONKZKKBN
                               WHEN    "1"
                                   MOVE    1                   TO  IDX
                               WHEN    "2"
                                   MOVE    2                   TO  IDX
                           END-EVALUATE
                       END-IF
                   END-IF
           END-EVALUATE
      *
           IF      IDX                 >   ZERO
      *        自費以外のとき自費分だけ集計
               IF      IDX             =   6
                   MOVE    ZERO            TO  WRK-JIHI
               ELSE
                   COMPUTE WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                       +   SYU-TOTAL-TGMONEY-TAX
                                       +   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
                                       +   SYU-RMSAGAKU
                                       +   SYU-SKYMONEY-SKJ-JIHI-KEI
                                       +   SYU-SKYMONEY-LIFE-JIHI-KEI
                   IF      SYU-SRYYMD  <   CONST-H190401
                       COMPUTE WRK-JIHI    =   WRK-JIHI 
                                           +   SYU-JIHI-TAX
                   END-IF
               END-IF
      *
               IF      IDX  =  7  OR  8 OR  9
                   COMPUTE WRK-MONEY   =   SYU-RSIJIBAI-SKYMONEY
                                       -   WRK-JIHI
               ELSE
                   COMPUTE WRK-MONEY   =   SYU-SKYMONEY
                                       -   WRK-JIHI
                   IF      WRK-JIHI    NOT =  ZERO
                       IF  ((SYU-DISCOUNT-TEIRITU  NOT =  SPACE) AND
                            (SYU-DISCOUNT-RATE     NOT =  ZERO ))
                       OR  ( SYU-CHOSEI            NOT =  ZERO  )
                            CONTINUE
                       ELSE
                           COMPUTE WRK-MONEY   =   SYU-HKNTEKMONEY
                                               +   SYU-RJN-FTN
                                               +   SYU-KOH-FTN
                                               +   SYU-YKZ-FTN
                                               +   SYU-SKYMONEY-SKJ-KEI
                                               +   SYU-SKYMONEY-LIFE-KEI
                           COMPUTE WRK-JIHI    =   SYU-SKYMONEY
                                               -   WRK-MONEY
                       END-IF
                   END-IF
               END-IF
      *
               IF      WRK-MONEY       <   0
                   MOVE    WRK-MONEY   TO  WRK-Z99
                   MOVE    WRK-Z99     TO  HCM41-MONEY(IDX CNT-LINE)
               ELSE
                   MOVE    WRK-MONEY   TO  WRK-Z9
                   MOVE    WRK-Z9      TO  HCM41-MONEY(IDX CNT-LINE)
               END-IF
               IF      IDX             =   6
                   CONTINUE
               ELSE
                   IF      WRK-JIHI    >   ZERO
                       MOVE    WRK-JIHI    TO  WRK-Z9
                       MOVE    "*"         TO  WRK-Z9 (2:1)
                       MOVE    WRK-Z9      TO  HCM41-MONEY (6 CNT-LINE)
                   END-IF
               END-IF
           END-IF
      *
           ADD     SYU-TOTAL-HKNTEN    TO  SUM-HKNTEN (1)
           ADD     SYU-SKYMONEY        TO  SUM-MONEY  (1)
           ADD     1                   TO  SUM-KENSU  (1)
           IF      IDX                 >   ZERO
               COMPUTE IDY     =   IDX     +   1
               IF      IDX     <   7
                   ADD     SYU-TOTAL-HKNTEN    TO  SUM-HKNTEN (IDY)
               END-IF
               ADD     WRK-MONEY       TO  SUM-MONEY(IDY)
               ADD     1               TO  SUM-KENSU(IDY)
               ADD     WRK-JIHI        TO  SUM-MONEY(7)
           END-IF
      *
           .
       3201-BODY-H200401-ATO-EXT.
           EXIT.
      *
      *----(04.02.01)--ADD-END-----
      *****************************************************************
      *          *    帳票編集<合計>処理
      *****************************************************************
       330-TERM-HEN-SEC            SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       11
      *----(04.02.01)--UPD-START---
               IF      WRK-PARA-SYORIYMD   <   CONST-H200401
                   IF      IDX                 <   9
                       MOVE    SUM-HKNTEN(IDX) TO  WRK-Z8
                       MOVE    WRK-Z8          TO  HCM41-GHKNTEN(IDX)
                   END-IF
               ELSE
                   IF      IDX                 <   8
                       MOVE    SUM-HKNTEN(IDX) TO  WRK-Z8
                       MOVE    WRK-Z8          TO  HCM41-GHKNTEN(IDX)
                   END-IF
               END-IF
      *----(04.02.01)--UPD-END-----
               IF      SUM-MONEY (IDX)     <   0
                   MOVE    SUM-MONEY(IDX)  TO  WRK-Z99
                   MOVE    WRK-Z99         TO  HCM41-GMONEY(IDX)
               ELSE
                   MOVE    SUM-MONEY(IDX)  TO  WRK-Z9 
                   MOVE    WRK-Z9          TO  HCM41-GMONEY(IDX)
               END-IF
               MOVE    SUM-KENSU(IDX)  TO  WRK-Z8
               MOVE    WRK-Z8          TO  HCM41-GKENSU(IDX)
           END-PERFORM
      *
           .
       331-TOTAL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC           SECTION.
      *
      *    「１：印刷する」以外は処理しない
           IF      WRK-PARA-PRTKBN =   "1"
               CONTINUE
           ELSE
               GO  TO   390-PRINT-OUT-EXT
           END-IF
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"           TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                   TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SPRT-PRIORITY
      *----(04.02.01)--UPD-START---
      *     MOVE    "HCM41V01.red"  TO  SPRT-PRTID
           MOVE    "HCM41V02.red"  TO  SPRT-PRTID
           MOVE    "日計ＣＳＶ作成（診療年月日）"
                                       TO  SPRT-TITLE
           MOVE    HCM41            TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                    TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                    TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                    TO  SPRT-PRTNM
           MOVE    "1"              TO  SPRT-SITEKBN
           CALL    "ORCSPRT"            USING
                                        ORCSPRTAREA
                                        SPA-AREA
           IF      SPRT-RETURN          =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                        TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       390-PRINT-OUT-EXT.
           EXIT.
057400*
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   TOUKEIERR-FILE
           IF      STS-TOUKEIERR     =   ZERO
               CLOSE   TOUKEIERR-FILE
           ELSE
               OPEN    OUTPUT              TOUKEIERR-FILE
      *
               MOVE    WRK-TOUKEIERR     TO  TOUKEIERR-REC
               WRITE   TOUKEIERR-REC
               CLOSE   TOUKEIERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-HOSPNUM TO JOB-HOSPNUM
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-TOUKEIERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
      *
               PERFORM 800-ORCSJOB-SEC
      *
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    異常終了処理
      *****************************************************************
       500-COBABORT-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-ERRMSG
           STRING  "ORCBD999 "
                                   DELIMITED  BY  SIZE
                   WRK-TOUKEIERR   DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"      USING   WRK-ERRMSG
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    BD00801PARA     TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-HOSPNUM TO JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           IF      WRK-PARA-PRTKBN =   "1"
               MOVE    CNT-PAGE        TO  JOB-UPDCNT
           END-IF
      *
           PERFORM 800-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
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
           MOVE    LNK-DAY2-EDTYMD3
                                   TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ（ＨＥＡＤレコード）編集処理
      *****************************************************************
       2001-CSV-HEAD-RECORD-HENSYU-SEC                SECTION.
      *
           INITIALIZE                  WRK-REC
                                       WRK-REC-LENGTH
      *
      *    改行コード編集
           STRING  CSV-HEAD-01         DELIMITED  BY  SIZE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                   INTO                WRK-REC
           END-STRING
      *
           MOVE    ZERO            TO  WRK-STOUKEICSV-PTID
           MOVE    "2"             TO  WRK-STOUKEICSV-NYUGAIKBN
           PERFORM 500-TOUKEICSV-SEC
           .
       2001-CSV-HEAD-RECORD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ（データレコード）編集処理
      *****************************************************************
       2001-CSV-DATA-RECORD-HENSYU-SEC SECTION.
      *
           INITIALIZE                  WRK-REC
                                       WRK-REC-LENGTH
      *
      *    作成年月日          
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  WRK-HEN-YMD-IN
           PERFORM 5006-YMD-HENSYU-SEC
           MOVE    WRK-HEN-YMD-OT  TO  WRK-DATA
           MOVE    10              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療年月日
           MOVE    WRK-PARA-SYORIYMD
                                   TO  WRK-HEN-YMD-IN
           PERFORM 5006-YMD-HENSYU-SEC
           MOVE    WRK-HEN-YMD-OT  TO  WRK-DATA
           MOVE    10              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    伝票番号
           MOVE    SYU-DENPNUM     TO  WRK-NUM
           MOVE    7               TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    患者番号
           MOVE    BD00801-PTNUM   TO  WRK-DATA
           MOVE    20              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    氏名
           MOVE    PTINF-NAME      TO  WRK-DATA
           MOVE    100             TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    カナ氏名
           MOVE    PTINF-KANANAME  TO  WRK-DATA
           MOVE    50              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    性別
           MOVE    PTINF-SEX       TO  WRK-DATA
           MOVE    1               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    生年月日
           MOVE    PTINF-BIRTHDAY  TO  WRK-HEN-YMD-IN
           PERFORM 5006-YMD-HENSYU-SEC
           MOVE    WRK-HEN-YMD-OT  TO  WRK-DATA
           MOVE    10              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    自宅−郵便番号
           MOVE    PTINF-HOME-POST TO  WRK-DATA
           MOVE    7               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    自宅−昼電話番号
           MOVE    PTINF-HOME-TEL1 TO  WRK-DATA
           MOVE    15              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    保険点数
           EVALUATE    SYU-SYUHKNNUM
               WHEN    WRK-CONS-JIBAI-HKNNUM
               WHEN    WRK-CONS-ROUSAI-HKNNUM
               WHEN    WRK-CONS-KOUGAI-HKNNUM
                   CONTINUE
               WHEN    OTHER
                   MOVE    SYU-TOTAL-HKNTEN
                                           TO  WRK-NUM
           END-EVALUATE
           MOVE    12              TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    社保本人
           IF      IDX                 =   1
               MOVE    WRK-MONEY           TO  WRK-NUM
           END-IF
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    社保本人（点数）
           IF      IDX                 =   1
               MOVE    SYU-TOTAL-HKNTEN    TO  WRK-NUM
           END-IF
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    社保家族
           IF      IDX                 =   2
               MOVE    WRK-MONEY           TO  WRK-NUM
           END-IF
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    社保家族（点数）
           IF      IDX                 =   2
               MOVE    SYU-TOTAL-HKNTEN    TO  WRK-NUM
           END-IF
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    国保
           IF      IDX                 =   3
               MOVE    WRK-MONEY           TO  WRK-NUM
           END-IF
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    国保（点数）
           IF      IDX                 =   3
               MOVE    SYU-TOTAL-HKNTEN    TO  WRK-NUM
           END-IF
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    後期高齢
           IF      IDX                 =   4
               MOVE    WRK-MONEY           TO  WRK-NUM
           END-IF
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    後期高齢（点数）
           IF      IDX                 =   4
               MOVE    SYU-TOTAL-HKNTEN    TO  WRK-NUM
           END-IF
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    公費単独
           IF      IDX                 =   5
               MOVE    WRK-MONEY           TO  WRK-NUM
           END-IF
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    公費単独（点数）
           IF      IDX                 =   5
               MOVE    SYU-TOTAL-HKNTEN    TO  WRK-NUM
           END-IF
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    自費
           IF      IDX                 =   6
               MOVE    WRK-MONEY           TO  WRK-NUM
           ELSE
               MOVE    WRK-JIHI            TO  WRK-NUM
           END-IF
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    自費（点数）
           IF      IDX                 =   6
               MOVE    SYU-TOTAL-HKNTEN    TO  WRK-NUM
           END-IF
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    労災
           IF      IDX                 =   7
               MOVE    WRK-MONEY           TO  WRK-NUM
           END-IF
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    自賠責
           IF      IDX                 =   8
               MOVE    WRK-MONEY           TO  WRK-NUM
           END-IF
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    公害
           IF      IDX                 =   9
               MOVE    WRK-MONEY           TO  WRK-NUM
           END-IF
           MOVE    9                   TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                       DELIMITED  BY  SIZE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                   INTO                WRK-REC
           END-STRING
      *
           MOVE    SYU-PTID        TO  WRK-STOUKEICSV-PTID
           MOVE    "2"             TO  WRK-STOUKEICSV-NYUGAIKBN
           PERFORM 500-TOUKEICSV-SEC
           .
       2001-CSV-DATA-RECORD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       500-TOUKEICSV-SEC           SECTION.
      *
           INITIALIZE                  ORCSTOUKEICSVAREA
           MOVE    "INS"           TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  STOUKEICSV-SHELLID
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  STOUKEICSV-SRYYM
           MOVE    WRK-PARA-SYORIYMD
                                   TO  STOUKEICSV-SRYYMD
           MOVE    WRK-STOUKEICSV-NYUGAIKBN
                                   TO  STOUKEICSV-NYUGAIKBN
           MOVE    WRK-STOUKEICSV-PTID
                                   TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"     TO  STOUKEICSV-TO-FOLDER
           MOVE    WRK-PARA-SYORIYMD
                                   TO  WRK-TO-DATA-SRYYMD
           MOVE    SPA-HOSPNUM     TO  WRK-TO-DATA-HOSPNUM
           MOVE    WRK-TO-DATA     TO  STOUKEICSV-TO-DATA
           MOVE    "2"             TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-REC         TO  STOUKEICSV-CSVDATA
           MOVE    "日計ＣＳＶ作成（診療年月日）"
                                   TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"     USING
                                       ORCSTOUKEICSVAREA
                                       SPA-AREA
           IF      STOUKEICSV-RETURN   =   ZERO
               CONTINUE
           ELSE
               MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                      TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
      *
       500-TOUKEICSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       5004-MOJI-HENSYU-SEC          SECTION.
      *
           IF      WRK-DATA        =   SPACE
               CONTINUE
           ELSE                   
               PERFORM VARYING IDW2    FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW2    <       1
                   IF      WRK-DATA (IDW2:1)   NOT =   SPACE
                       STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                   DELIMITED   BY  SIZE
                               WRK-DATA(1:IDW2)    DELIMITED   BY  SIZE
                               INTO    WRK-REC
                       END-STRING
                       ADD     IDW2        TO  WRK-REC-LENGTH        
                       MOVE    1           TO  IDW2
                   END-IF
               END-PERFORM
           END-IF
      *     
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING        
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF    
      *
           INITIALIZE                      WRK-HENSYU-AREA
           MOVE    ZERO                TO  WRK-NUM    
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    出力レコード処理（数字）------------9.99999の編集
      *****************************************************************
       5005-NUM-HENSYU-SEC          SECTION.
      *
      *    WRK-NUM     セットする値
      *    WRK-NAGASA  全体の長さ（最大１９桁、小数点を含む）
      *                編集しない場合はゼロをセット
      *
      *    WRK-SYOSUU  小数部の長さ（最大５桁）
      *                整数の場合はゼロをセット
      *
           IF      WRK-NAGASA          =   ZERO
               CONTINUE
           ELSE                   
      *        開始位置
               IF      WRK-NAGASA          >   13
                   MOVE    1                   TO  WRK-ST
               ELSE
                   COMPUTE WRK-ST  =   15      -   WRK-NAGASA
                   IF      WRK-SYOSUU          >   ZERO
                       COMPUTE WRK-ST  =   WRK-ST  +   WRK-SYOSUU
                                                   +   1
                   END-IF
               END-IF                                
      *     
               PERFORM VARYING IDW2    FROM    WRK-ST  BY  1
                       UNTIL   IDW2    >       14
                   IF      WRK-NUM (IDW2:1)    NOT =   SPACE
                        COMPUTE IDZ    =   15  +   WRK-SYOSUU
                                               -   IDW2       
                        IF      WRK-SYOSUU     >   ZERO
                            ADD     1              TO  IDZ
                        END-IF     
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                   DELIMITED  BY  SIZE
                               WRK-NUM(IDW2:IDZ)   DELIMITED  BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       COMPUTE WRK-REC-LENGTH  =   WRK-REC-LENGTH   +
                                                   IDZ                  
                       MOVE    14              TO  IDW2
                   END-IF
               END-PERFORM
           END-IF    
      *     
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH)   DELIMITED  BY  SIZE
                       WRK-KUGIRI                  DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING        
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF    
      *
           INITIALIZE                      WRK-HENSYU-AREA
           MOVE    ZERO                TO  WRK-NUM    
           .
       5005-NUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       5006-YMD-HENSYU-SEC                 SECTION.
      *
           MOVE    WRK-HEN-YY-IN   TO  WRK-HEN-YY-OT
           MOVE    WRK-HEN-MM-IN   TO  WRK-HEN-MM-OT
           MOVE    WRK-HEN-DD-IN   TO  WRK-HEN-DD-OT
      *
           MOVE    SPACE           TO  WRK-HEN-YMD-IN
           .
       5006-YMD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
       800-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    収納マスタ　伝票発行日で検索
      *****************************************************************
       900-SYUNOU-SEL-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           INITIALIZE                  SYU-REC
           MOVE    WRK-PARA-HOSPNUM
                                   TO  SYU-HOSPNUM
           MOVE    "2"             TO  SYU-NYUGAIKBN
           MOVE    WRK-PARA-SYORIYMD
                                   TO  SYU-SRYYMD
      *
           MOVE    SYU-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key44"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC TO  SYU-REC
           ELSE
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納を検索処理
      *****************************************************************
       900-SYUNOU-FET-SEC              SECTION.
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key44"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
      *
               MOVE    ZERO            TO  FLG-SYUNOU
               MOVE    MCPDATA-REC     TO  SYU-REC
      *
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納ファイル読込処理
      *****************************************************************
       900-BD00801-READ-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM UNTIL   FLG-OK      =   1
               READ    BD00801-FILE        NEXT
               AT  END
                   MOVE    1               TO  FLG-END
                   MOVE    1               TO  FLG-OK
               NOT AT  END
                   MOVE    ZERO            TO  FLG-END
                   MOVE    BD00801-SYU-DAT TO  SYU-REC
      *            患者情報読み込み
                   INITIALIZE                  PTINF-REC
                   MOVE    SYU-HOSPNUM    TO  PTINF-HOSPNUM
                   MOVE    SYU-PTID      TO  PTINF-PTID
                   PERFORM 900-PTINF-READ-SEC
                   IF    ( FLG-PTINF               =   ZERO )
                    AND  ( PTINF-TSTPTNUMKBN   NOT =   "1" )
                       MOVE    1               TO  FLG-OK
                   ELSE
                       MOVE    ZERO            TO  FLG-OK
                   END-IF
               END-READ
           END-PERFORM
      *
           .
       900-BD00801-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者番号マスター読込処理
      *****************************************************************
       900-PTNUM-READ-SEC          SECTION.
      *
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  PTNUM-REC
               MOVE    ZERO            TO  FLG-PTNUM
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    患者保険マスター読込処理
      *****************************************************************
       900-PTHKNINF-READ-SEC       SECTION.
      *
           MOVE    PTHKNINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
           END-IF
      *
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込処理
      *****************************************************************
       900-HKNCOMBI-READ-SEC       SECTION.
      *
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                  SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
