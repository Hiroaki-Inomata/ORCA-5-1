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
       PROGRAM-ID.             ORCBG002.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 定期請求患者リスト
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/04/28    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    04/03/29  請求金額を収納より取得する
      *                                     よう変更
      *  03.05.01    NACL-森脇    07/06/07  グループ診療対応
      *  04.05.00    NACL-森脇    09/12/15  ＣＳＶ出力対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    定期請求情報ファイル（初期化／読込用）
grpsys*DD  SELECT  BG00201-FILE   ASSIGN   ORCBG002PARA
grpsys     SELECT  BG00201-FILE   ASSIGN   BG00201PARA
                                  ORGANIZATION    IS  INDEXED
                                  ACCESS  MODE    IS  DYNAMIC
                                  RECORD  KEY     IS  BG00201-ISAM-KEY
                                  FILE    STATUS  IS  STS-BG00201.
      * 
006900*    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       FD  BG00201-FILE.
       01  BG00201-REC.
      *----(02.00.01)--UPD-START---
           02  BG00201-ISAM-KEY                PIC X(48).
      *----(02.00.01)--UPD-END-----
           02  BG00201-SYU-DATA-AREA.
           COPY    "CPSYUNOU.INC"  REPLACING   //SYU-//
                                   BY          //BG00201-SYU-//.
           02  BG00201-PTTEIKIRRK-DATA-AREA.
           COPY    "CPPTTEIKIRRK.INC"  REPLACING
                                               //PTTEIKIRRK-//
                                   BY          //BG00201-PTTEIKIRRK-//.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    定期請求情報ファイル
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING   //RECE01//
                                   BY          //BG00201//.
      *
           COPY    "CPRECEERR.INC".
      *
           COPY    "HCMG002.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-BG00201                         PIC X(02).
           03  STS-RECEERR                         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-PTTEIKIRRK                      PIC 9(01).
           03  FLG-SYUNOU                          PIC 9(01).
           03  FLG-HIT                             PIC 9(01).
           03  FLG-DBRC                            PIC 9(01).
           03  FLG-BG00201                         PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                            PIC 9(02).
           03  CNT-PAGE                            PIC 9(03).
           03  CNT-BG00201                         PIC 9(05).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY                          PIC 9(02).
               05  SYS-MM                          PIC 9(02).
               05  SYS-DD                          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
      *
           03  IDW1                    PIC 9(04).
           03  IDW2                    PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID                      PIC 9(07).
           03  WRK-PARA-SHELLID                    PIC X(08).
grpsys*DD  03  WRK-PARA-HOSPID                     PIC X(24).
grpsys     03  WRK-PARA-HOSPNUM                    PIC 9(02).
           03  WRK-PARA-SRYYM                      PIC X(06).
           03  WRK-PARA-SYORIKBN                   PIC X(01).
      *----(02.00.01)--ADD-START---
           03  WRK-PARA-JUNKBN                     PIC X(01).
      *
       01  ISAM-AREA.
           03  ISAM-KEY.
               05  ISAM-KEY-BTUNUM      PIC X(02).
               05  ISAM-KEY-BRMNUM      PIC X(08).
               05  ISAM-KEY-SRYKA       PIC X(02).
               05  ISAM-KEY-PTNUM       PIC X(20).
               05  ISAM-KEY-SAKKBN      PIC X(01).
               05  ISAM-KEY-SRYYMD      PIC X(08).
               05  ISAM-KEY-DENPNUM     PIC X(07).
           03  ISAM-KEY2.
               05  ISAM-KEY2-PTNUM      PIC X(20).
               05  ISAM-KEY2-BTUNUM     PIC X(02).
               05  ISAM-KEY2-BRMNUM     PIC X(08).
               05  ISAM-KEY2-SRYKA      PIC X(02).
               05  ISAM-KEY2-SAKKBN     PIC X(01).
               05  ISAM-KEY2-SRYYMD     PIC X(08).
               05  ISAM-KEY2-DENPNUM    PIC X(07).
      *----(02.00.01)--ADD-END-----
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-DBPATH                          PIC  X(100).
grpsys     03  WRK-DBTABLE                         PIC  X(100).
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-TOUKEIERR           PIC X(200).
           03  WRK-TAISYO.
               05  WRK-TAISYO-YM                   PIC X(16).
               05  WRK-TAISYO-SYORIMEI             PIC X(44).
           03  WRK-SYMD                            PIC X(08).
           03  WRK-EDTYMD1                         PIC X(10).
           03  WRK-EDTYMD3                         PIC X(22).
           03  WRK-SYORIYMD                        PIC X(22).
           03  WRK-SPTID-PTNUM                     PIC X(20).
           03  WRK-SKYMONEY-PAGE                   PIC S9(10).
           03  WRK-SKYMONEY-TTL                    PIC S9(10).
           03  WRK-PAGE                            PIC ZZ9.
           03  WRK-HCMG002.
               05  WRK-HCMG002-BTUNAME             PIC X(10).
               05  WRK-HCMG002-BRMNUM              PIC X(6).
               05  WRK-HCMG002-SRYKA               PIC X(8).
               05  WRK-HCMG002-PTNUM               PIC X(20).
               05  WRK-HCMG002-NAME                PIC X(30).
               05  WRK-HCMG002-SEX                 PIC X(2).
               05  WRK-HCMG002-BIRTHDAY            PIC X(22).
           03  WRK-BRM-NUM-IN                      PIC X(06).
           03  WRK-BRM-NUM-EDT                     PIC X(06).
           03  WRK-STR1                            PIC X(20).
           03  WRK-STR2                            PIC X(20).
           03  WRK-IDX1                            PIC 9(05).
           03  WRK-IDX2                            PIC 9(05).
           03  WRK-Z9                              PIC ----,---,--9.
           03  WRK-JYOTAI                          PIC X(16).
           03  WRK-SPTID-PTID                      PIC 9(10).
           03  WRK-STR                 PIC X(200).
           03  WRK-LEN                 PIC 9(05).
           03  WRK-SHO                 PIC 9(05).
           03  WRK-AMARI               PIC 9(05).
      *
      *    ＣＳＶデータ編集用
           03  WRK-REC             PIC X(20000).
           03  WRK-REC-LENGTH      PIC 9(05).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA        PIC X(20000).
               05  WRK-NUM         PIC -------------9.99999. 
               05  WRK-NAGASA      PIC 9(04).
               05  WRK-RECSTR      PIC 9(02). 
               05  WRK-SYOSUU      PIC 9(01).
               05  WRK-RECEND      PIC 9(01).
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
       01  KEY-AREA.
           03  KEY-BTUNUM          PIC X(02).
           03  KEY-BRMNUM          PIC X(08).
           03  KEY-SRYKA           PIC X(02).
           03  KEY-PTID            PIC 9(10).
      *
       01  ERR-EDIT-AREA.
           03  ERR-PTID                            PIC 9(10).
           03  ERR-PTNUM                           PIC X(20).
           03  ERR-ERRMSG                          PIC X(80).
           03  ERR-HEN-ERRMSG                      PIC X(80).
      *
       01  CONST-AREA.
           03  CONST-LINE-MAX      PIC 9(05)   VALUE 30.
      *    コンマ
           03  WRK-KUGIRI          PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO          PIC X(01)   VALUE   X"0D".
      *
      *    出力先ファイル名
       01  WRK-TO-DATA-G.
           03  WRK-TO-DATA.
               05  WRK-TO-DATA-HOSPNUM PIC 9(02).
               05  FILLER              PIC X(19)   VALUE
                                               "teikiseikyu_kanjya_".
               05  WRK-TO-DATA-STRYM   PIC X(06).
               05  FILLER              PIC X(04)   VALUE   ".csv".
      *
      *    ヘッダー情報
       01  CSV-HEAD-01.
           03  FILLER              PIC X(08)   VALUE   "診療年月".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "病棟区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "病室".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "診療科".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "性別".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "生年月日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "伝票番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "請求金額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "処理区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "名称".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    COPY    "ORCA-DBPATH".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者定期請求履歴
       01  PTTEIKIRRK-REC.
           COPY    "CPPTTEIKIRRK.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *
      *    患者番号構成情報
           COPY    "CPSK1009.INC".
      *
      *    病棟情報
           COPY    "CPSK5001.INC".
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
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
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
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
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
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
      *
      ******************************************************************
      *
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
           PERFORM UNTIL ( FLG-END     =   1 )
               PERFORM 200-MAIN-SEC
           END-PERFORM
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
grpsys     INITIALIZE                  SPA-AREA
      *
           INITIALIZE                  RECEERR
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
grpsys*DD                              WRK-PARA-HOSPID
grpsys                                 WRK-PARA-HOSPNUM
grpsys                                 RECEERR
                                       WRK-PARA-SRYYM
                                       WRK-PARA-SYORIKBN
      *----(02.00.01)--ADD-START---
                                       WRK-PARA-JUNKBN
      *----(02.00.01)--ADD-END-----
grpsys     END-UNSTRING
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBG002"          TO  JOB-PGID
           MOVE    "定期請求患者一覧表"
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
grpsys                                     SPA-AREA
      *
grpsys*DD  MOVE    "RECEERR"           TO  RECEERR-FILE-ID
grpsys*DD  MOVE    LNK-PRTKANRI-TERMID
grpsys*DD                              TO  RECEERR-TERMID
      *
      *    収納処理ファイル名
grpsys*DD  MOVE    "BG00201"           TO  ORCBG002PARA-FILE-ID
grpsys*DD  MOVE    LNK-PRTKANRI-TERMID TO  ORCBG002PARA-TERMID
grpsys     MOVE    "BG00201"           TO  BG00201PARA-FILE-ID
grpsys     MOVE    LNK-PRTKANRI-TERMID TO  BG00201PARA-TERMID
grpsys     MOVE    SPA-HOSPNUM         TO  BG00201PARA-HOSPNUM
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    BG00201PARA-BASENAME
                                   TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  BG00201PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  RECEERR
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
           IF    ( FLG-END         NOT =   ZERO )
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    医療機関ＩＤ編集
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD (1:6)
           MOVE    "01"                TO  ORC-DBYMD (7:2)
grpsys     MOVE    ORC-DBYMD           TO  SYS-1001-STYUKYMD
grpsys                                     SYS-1001-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
grpsys*DD  MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
grpsys     MOVE    "tbl_syskanri"  TO  WRK-DBTABLE
grpsys     MOVE    "key10"         TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC            =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           ELSE
               MOVE    "医療機関情報が取得できませんでした。"
                                       TO  WRK-RECEERR 
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  100-INIT-EXT
           END-IF
      *
grpsys*DD  MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
grpsys     MOVE    "tbl_syskanri"  TO  WRK-DBTABLE
grpsys     MOVE    "key10"         TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    患者番号構成管理情報編集
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD (1:6)
           MOVE    "01"                TO  ORC-DBYMD (7:2)
grpsys     MOVE    ORC-DBYMD           TO  SYS-1009-STYUKYMD
grpsys                                     SYS-1009-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
grpsys*DD  MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
grpsys     MOVE    "tbl_syskanri"  TO  WRK-DBTABLE
grpsys     MOVE    "key10"         TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                       TO  SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-HJNKSIKBN  TO  SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN 
                                           TO  SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                           TO  SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                           TO  SPA-1009-HJNKSIRENNUMKETA
           ELSE
               MOVE    "患者番号構成管理情報が取得できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  100-INIT-EXT
           END-IF
      *
grpsys*DD  MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
grpsys     MOVE    "tbl_syskanri"  TO  WRK-DBTABLE
grpsys     MOVE    "key10"         TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    対象定期請求情報ファイル出力処理
           PERFORM 110-TEIKI-OUT-SEC
           IF    ( FLG-END         NOT =   ZERO )
               GO  TO  100-INIT-EXT
           END-IF
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
      *    処理対象編集
           PERFORM 1101-TAISYO-EDIT-SEC
           IF    ( FLG-END         NOT =   ZERO )
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
      *    作成日
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3         TO  WRK-SYORIYMD
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理対象編集処理
      *****************************************************************
       1101-TAISYO-EDIT-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-TAISYO
      *
           MOVE    WRK-PARA-SRYYM      TO  WRK-SYMD
           MOVE    "01"                TO  WRK-SYMD (7 : )
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3 (1 : 16)
                                       TO  WRK-TAISYO-YM
      *
           EVALUATE    WRK-PARA-SYORIKBN
           WHEN    "0"
               MOVE    "（全件）"      TO  WRK-TAISYO-SYORIMEI
           WHEN    "1"
               MOVE    "（１期）"      TO  WRK-TAISYO-SYORIMEI
           WHEN    "2"
               MOVE    "（２期）"      TO  WRK-TAISYO-SYORIMEI
           WHEN    "3"
               MOVE    "（３期）"      TO  WRK-TAISYO-SYORIMEI
           WHEN    "9"
               MOVE    "（月末一括請求者のみ）"
                                       TO  WRK-TAISYO-SYORIMEI
      *
      *    その他はエラー
           WHEN    OTHER
               MOVE    SPACE           TO  WRK-RECEERR
               STRING  "処理区分が誤りです：["     DELIMITED   BY  SIZE
                       WRK-PARA-SYORIKBN           DELIMITED   BY  SIZE
                       "]"                         DELIMITED   BY  SIZE
               INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  1101-TAISYO-EDIT-EXT
           END-EVALUATE
      *
           .
       1101-TAISYO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ファイル出力処理
      *****************************************************************
       110-TEIKI-OUT-SEC           SECTION.
      *
      *    定期請求情報ファイル初期化
           OPEN    OUTPUT              BG00201-FILE
           CLOSE   BG00201-FILE
      *
           OPEN    OUTPUT              BG00201-FILE
      *
      *    患者定期請求履歴検索
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
           INITIALIZE  PTTEIKIRRK-REC
grpsys*DD  MOVE    SYS-1001-HOSPID     TO  PTTEIKIRRK-HOSPID
grpsys     MOVE    SPA-HOSPNUM         TO  PTTEIKIRRK-HOSPNUM
           MOVE    WRK-PARA-SRYYM      TO  PTTEIKIRRK-SRYYM
           IF    ( WRK-PARA-SYORIKBN   =   "0" )
               MOVE    "_"             TO  PTTEIKIRRK-SAKKBN
           ELSE
               MOVE    WRK-PARA-SYORIKBN
                                       TO  PTTEIKIRRK-SAKKBN
           END-IF
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
grpsys*DD  MOVE    "PTTEIKIRRK-KEY5"   TO  WRK-DBPATH
grpsys     MOVE    "tbl_ptteikirrk"    TO  WRK-DBTABLE
grpsys     MOVE    "key5"          TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    NOT =   ZERO )
               MOVE    1           TO  FLG-PTTEIKIRRK
           END-IF
      *
           PERFORM UNTIL ( FLG-PTTEIKIRRK  =   1 )
      *
               MOVE    MCPDATA-REC TO  PTTEIKIRRK-REC
      *
      *        定期請求情報ファイル編集
               PERFORM 1101-TEIKI-EDIT-SEC
               IF    ( FLG-END     NOT =   ZERO )
                   GO  TO  110-TEIKI-OUT-EXT
               END-IF
      *
grpsys*DD      MOVE    "PTTEIKIRRK-KEY5"
grpsys*DD                              TO  WRK-DBPATH
grpsys         MOVE    "tbl_ptteikirrk"    TO  WRK-DBTABLE
grpsys         MOVE    "key5"          TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
               IF    ( FLG-DBRC    NOT =   ZERO )
                   MOVE    1           TO  FLG-PTTEIKIRRK
               END-IF
      *
           END-PERFORM
      *
grpsys*DD  MOVE    "PTTEIKIRRK-KEY5"   TO  WRK-DBPATH
grpsys     MOVE    "tbl_ptteikirrk"    TO  WRK-DBTABLE
grpsys     MOVE    "key5"          TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           CLOSE   BG00201-FILE
      *
           .
       110-TEIKI-OUT-EXT.
           EXIT.
      *****************************************************************
      *    定期請求情報ファイル編集処理
      *****************************************************************
       1101-TEIKI-EDIT-SEC             SECTION.
      *
      *    収納マスタ検索
           MOVE    ZERO            TO  FLG-SYUNOU
           INITIALIZE  SYUNOU-REC
grpsys*DD  MOVE    SYS-1001-HOSPID TO  SYU-HOSPID
grpsys     MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    "1"             TO  SYU-NYUGAIKBN
           MOVE    PTTEIKIRRK-PTID TO  SYU-PTID
           MOVE    PTTEIKIRRK-DENPNUM
                                   TO  SYU-DENPNUM
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
grpsys*DD  MOVE    "SYUNOU-KEY"    TO  WRK-DBPATH
grpsys     MOVE    "tbl_syunou"    TO  WRK-DBTABLE
grpsys     MOVE    "key"           TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               MOVE    1           TO  FLG-SYUNOU
               INITIALIZE              SYUNOU-REC
           END-IF
      *
grpsys*DD  MOVE    "SYUNOU-KEY"    TO  WRK-DBPATH
grpsys     MOVE    "tbl_syunou"    TO  WRK-DBTABLE
grpsys     MOVE    "key"           TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF    ( FLG-SYUNOU      NOT =   ZERO )
               MOVE    PTTEIKIRRK-PTID TO  ERR-PTID
               MOVE    "収納データの取得に失敗しました"
                                       TO  ERR-ERRMSG
               PERFORM 800-ERRMSG-EDIT-SEC
               MOVE    ERR-HEN-ERRMSG  TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  1101-TEIKI-EDIT-EXT
           END-IF
      *
           INITIALIZE  BG00201-REC
      *----(02.00.01)--ADD-START---
           INITIALIZE  ISAM-AREA
      *----(02.00.01)--ADD-END-----
      *
      *----(02.00.01)--UPD-START---
           MOVE    SYU-BYOTONUM        TO  ISAM-KEY-BTUNUM
           MOVE    SYU-ROOMNUM         TO  ISAM-KEY-BRMNUM
           MOVE    SYU-SRYKA           TO  ISAM-KEY-SRYKA
           MOVE    SYU-PTID            TO  WRK-SPTID-PTID
           PERFORM 1101-PTNUM-GET-SEC
           IF    ( WRK-SPTID-PTNUM     =   SPACE )
               MOVE    SYU-PTID        TO  ERR-PTID
               MOVE    "患者番号の取得に失敗しました"
                                       TO  ERR-ERRMSG
               PERFORM 800-ERRMSG-EDIT-SEC
               MOVE    ERR-HEN-ERRMSG  TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  1101-TEIKI-EDIT-EXT
           END-IF
      *
           MOVE    WRK-SPTID-PTNUM     TO  ISAM-KEY-PTNUM
           MOVE    PTTEIKIRRK-SAKKBN   TO  ISAM-KEY-SAKKBN
           MOVE    SYU-SRYYMD          TO  ISAM-KEY-SRYYMD
           MOVE    SYU-DENPNUM         TO  ISAM-KEY-DENPNUM
      *----(02.00.01)--UPD-END-----
      *
      *----(02.00.01)--ADD-START---
           IF      WRK-PARA-JUNKBN     =   "1"
               MOVE    ISAM-KEY-BTUNUM     TO  ISAM-KEY2-BTUNUM
               MOVE    ISAM-KEY-BRMNUM     TO  ISAM-KEY2-BRMNUM
               MOVE    ISAM-KEY-SRYKA      TO  ISAM-KEY2-SRYKA
               MOVE    ISAM-KEY-PTNUM      TO  ISAM-KEY2-PTNUM
               MOVE    ISAM-KEY-SAKKBN     TO  ISAM-KEY2-SAKKBN
               MOVE    ISAM-KEY-SRYYMD     TO  ISAM-KEY2-SRYYMD
               MOVE    ISAM-KEY-DENPNUM    TO  ISAM-KEY2-DENPNUM
               MOVE    ISAM-KEY2           TO  BG00201-ISAM-KEY
           ELSE
               MOVE    ISAM-KEY            TO  BG00201-ISAM-KEY
           END-IF
      *----(02.00.01)--ADD-END-----
      *
           MOVE    SYUNOU-REC          TO  BG00201-SYU-DATA-AREA
           MOVE    PTTEIKIRRK-REC      TO  BG00201-PTTEIKIRRK-DATA-AREA
      *
           WRITE   BG00201-REC
      *
           .
       1101-TEIKI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    患者番号取得処理
      *****************************************************************
       1101-PTNUM-GET-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-SPTID-PTNUM
      *
      *    患者番号を取得
           INITIALIZE                  ORCSPTIDAREA
grpsys*DD  MOVE    WRK-PARA-HOSPID TO  SPTID-HOSPID
grpsys     MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
           MOVE    WRK-SPTID-PTID  TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
grpsys*DD                                  SPA-1009-TBL
grpsys                                     SPA-AREA
           IF    ( SPTID-RC            =   ZERO )
               MOVE    SPTID-PTNUM TO  WRK-SPTID-PTNUM
           END-IF
      *
           .
       1101-PTNUM-GET-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    LOW-VALUE       TO  KEY-BTUNUM
                                       KEY-BRMNUM
                                       KEY-SRYKA
                                       KEY-PTID
      *
           MOVE    ZERO            TO  CNT-PAGE
           MOVE    ZERO            TO  WRK-SKYMONEY-TTL
           MOVE    ZERO            TO  FLG-BG00201
      *
           MOVE    ZERO            TO  CNT-BG00201
           OPEN    INPUT   BG00201-FILE
           PERFORM 900-BG00201-READ-SEC
      *
           IF      FLG-BG00201 =   ZERO
      *    ＣＳＶ出力処理
           PERFORM 270-CSV-HEAD-SET-SEC
           PERFORM UNTIL ( FLG-BG00201 =   1 )
      *
               MOVE    SPACE       TO  HCMG002
      *
      *        ヘッダ編集処理
               PERFORM 210-HEAD-EDIT-SEC
      *
               MOVE    ZERO        TO  CNT-LINE
               MOVE    ZERO        TO  WRK-SKYMONEY-PAGE
               PERFORM UNTIL ( FLG-BG00201 =   1 )
                       OR    ( CNT-LINE   >=   CONST-LINE-MAX )
      *
      *            明細編集処理
                   PERFORM 210-BODY-EDIT-SEC
      *
               END-PERFORM
      *
      *        合計行編集処理
               PERFORM 210-TOTAL-EDIT-SEC
      *
      *        出力処理
               PERFORM 400-PRINT-OUT-SEC
      *
           END-PERFORM
           END-IF
      *
           CLOSE   BG00201-FILE
      *
      *    ファイル削除
           MOVE    ZERO        TO  IF-RESULT
grpsys*DD  MOVE    ORCBG002PARA
grpsys     MOVE    BG00201PARA
	                       TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       210-HEAD-EDIT-SEC           SECTION.
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
           MOVE    WRK-TAISYO      TO  HCMG002-TAISYO
      *
           MOVE    WRK-SYORIYMD    TO  HCMG002-SYORIYMD
      *
           MOVE    CNT-PAGE        TO  WRK-PAGE
           MOVE    WRK-PAGE        TO  HCMG002-PAGE
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<明細部>処理
      *****************************************************************
       210-BODY-EDIT-SEC           SECTION.
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           MOVE    SPACE           TO      WRK-HCMG002-BTUNAME
                                           WRK-HCMG002-BRMNUM
                                           WRK-HCMG002-SRYKA
                                           WRK-HCMG002-PTNUM
                                           WRK-HCMG002-NAME
                                           WRK-HCMG002-SEX
                                           WRK-HCMG002-BIRTHDAY
      *
      *----(02.00.01)--UPD-START---
           IF      WRK-PARA-JUNKBN     =   "1"
               PERFORM 2101-BODY-BTUNAME-EDIT-SEC
               PERFORM 2101-BODY-BRMNUM-EDIT-SEC
               PERFORM 2101-BODY-SRYKA-EDIT-SEC
               PERFORM 2101-BODY-PTINF-EDIT-SEC
           ELSE
               IF    ( CNT-LINE        =   1 )
                OR   ( KEY-BTUNUM  NOT =   ISAM-KEY-BTUNUM )
                   PERFORM 2101-BODY-BTUNAME-EDIT-SEC
               END-IF
      *
               IF    ( CNT-LINE        =   1 )
                OR   ( KEY-BTUNUM  NOT =   ISAM-KEY-BTUNUM )
                OR   ( KEY-BRMNUM  NOT =   ISAM-KEY-BRMNUM )
                   PERFORM 2101-BODY-BRMNUM-EDIT-SEC
               END-IF
      *
               IF    ( CNT-LINE        =   1 )
                OR   ( KEY-BTUNUM  NOT =   ISAM-KEY-BTUNUM )
                OR   ( KEY-BRMNUM  NOT =   ISAM-KEY-BRMNUM )
                OR   ( KEY-SRYKA   NOT =   ISAM-KEY-SRYKA )
                   PERFORM 2101-BODY-SRYKA-EDIT-SEC
               END-IF
      *
               IF    ( CNT-LINE        =   1 )
                OR   ( KEY-BTUNUM  NOT =   ISAM-KEY-BTUNUM )
                OR   ( KEY-BRMNUM  NOT =   ISAM-KEY-BRMNUM )
                OR   ( KEY-SRYKA   NOT =   ISAM-KEY-SRYKA )
                OR   ( KEY-PTID    NOT =   SYU-PTID )
                   PERFORM 2101-BODY-PTINF-EDIT-SEC
               END-IF
      *
      *        キーを設定
               MOVE    ISAM-KEY-BTUNUM TO  KEY-BTUNUM
               MOVE    ISAM-KEY-BRMNUM TO  KEY-BRMNUM
               MOVE    ISAM-KEY-SRYKA  TO  KEY-SRYKA
               MOVE    SYU-PTID        TO  KEY-PTID
           END-IF
      *----(02.00.01)--UPD-END-----
      *
           MOVE    WRK-HCMG002-BTUNAME     TO  HCMG002-BTUNAME
                                                           (CNT-LINE)
           MOVE    WRK-HCMG002-BRMNUM      TO  HCMG002-BRMNUM
                                                           (CNT-LINE)
           MOVE    WRK-HCMG002-SRYKA       TO  HCMG002-SRYKA
                                                           (CNT-LINE)
      *
           IF    ( WRK-HCMG002-PTNUM (11 : 1)    =   SPACE )
               MOVE    WRK-HCMG002-PTNUM   TO  HCMG002-PTNUM
                                                           (CNT-LINE)
           ELSE
               MOVE    WRK-HCMG002-PTNUM   TO  HCMG002-PTNUM2
                                                           (CNT-LINE)
           END-IF
      *
           MOVE    WRK-HCMG002-NAME        TO  HCMG002-NAME(CNT-LINE)
           MOVE    WRK-HCMG002-SEX         TO  HCMG002-SEX (CNT-LINE)
           MOVE    WRK-HCMG002-BIRTHDAY    TO  HCMG002-BIRTHDAY
                                                           (CNT-LINE)
           MOVE    SYU-DENPNUM             TO  HCMG002-DENPNUM
                                                           (CNT-LINE)
      *
           MOVE    SYU-SKYMONEY            TO  WRK-Z9
           MOVE    WRK-Z9                  TO  HCMG002-SEIKYU
                                                           (CNT-LINE)
      *
           MOVE    SPACE                   TO  WRK-JYOTAI
           EVALUATE    PTTEIKIRRK-SAKKBN
           WHEN    "1"
               MOVE    "１期"              TO  WRK-JYOTAI
           WHEN    "2"
               MOVE    "２期"              TO  WRK-JYOTAI
           WHEN    "3"
               MOVE    "３期"              TO  WRK-JYOTAI
           WHEN    "9"
               MOVE    "月末一括請求"      TO  WRK-JYOTAI
           END-EVALUATE
           MOVE    WRK-JYOTAI              TO  HCMG002-JYOTAI
                                                           (CNT-LINE)
      *
           COMPUTE WRK-SKYMONEY-PAGE
               =   WRK-SKYMONEY-PAGE   +   SYU-SKYMONEY
      *
           PERFORM 270-CSV-BODY-SET-SEC
      *
           PERFORM 900-BG00201-READ-SEC
      *
           .
       210-BODY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟名称編集処理
      *****************************************************************
       2101-BODY-BTUNAME-EDIT-SEC      SECTION.
      *
      *    病棟情報
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
      *----(02.00.01)--UPD-START---
           MOVE    ISAM-KEY-BTUNUM     TO  SYS-5001-KBNCD
      *----(02.00.01)--UPD-END-----
           MOVE    SYU-SKYSTYMD        TO  ORC-DBYMD
grpsys     MOVE    ORC-DBYMD           TO  SYS-5001-STYUKYMD
grpsys                                     SYS-5001-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
grpsys*DD  MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
grpsys     MOVE    "tbl_syskanri"      TO  WRK-DBTABLE
grpsys     MOVE    "key10"             TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               INITIALIZE                  SYS-5001-REC
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-5001-REC
           END-IF
      *
grpsys*DD  MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
grpsys     MOVE    "tbl_syskanri"      TO  WRK-DBTABLE
grpsys     MOVE    "key10"             TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           MOVE    SYS-5001-BTU-TANNAME
                                       TO  WRK-HCMG002-BTUNAME
      *
           .
       2101-BODY-BTUNAME-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病室名称編集処理
      *****************************************************************
       2101-BODY-BRMNUM-EDIT-SEC        SECTION.
      *
      *----(02.00.01)--UPD-START---
           MOVE   ISAM-KEY-BRMNUM ( 3 : )
                                       TO  WRK-BRM-NUM-IN
      *----(02.00.01)--UPD-END-----
           IF    ( WRK-BRM-NUM-IN (1 : 1)  =   SPACE )
               MOVE    SPACE           TO  WRK-STR1
               MOVE    1               TO  WRK-IDX1
               UNSTRING    WRK-BRM-NUM-IN
                   DELIMITED   BY  ALL SPACE
                   INTO    WRK-STR1
                   WITH    POINTER WRK-IDX1
               END-UNSTRING
               MOVE    WRK-BRM-NUM-IN (WRK-IDX1 : )
                                       TO  WRK-BRM-NUM-EDT
           ELSE
               MOVE    WRK-BRM-NUM-IN  TO  WRK-BRM-NUM-EDT
           END-IF
      *
           PERFORM VARYING WRK-IDX1    FROM    6   BY  -1
               UNTIL     ( WRK-IDX1    <   1 )
                 OR      ( WRK-BRM-NUM-EDT (WRK-IDX1 : 1)
                                   NOT =   SPACE )
               CONTINUE
           END-PERFORM
      *
           IF    ( WRK-IDX1    <   1 )
               MOVE    WRK-BRM-NUM-EDT TO  WRK-HCMG002-BRMNUM
               GO  TO  2101-BODY-BRMNUM-EDIT-EXT
           END-IF
      *
      *    病室番号が数字の場合前ゼロをとる
           IF    ( WRK-BRM-NUM-EDT ( 1 : WRK-IDX1 )
                                       IS  NUMERIC )
               IF    ( WRK-BRM-NUM-EDT ( 1 : 1 )
                                       =   ZERO )
                   MOVE    1               TO  WRK-IDX2
                   MOVE    SPACE           TO  WRK-STR1
                   UNSTRING    WRK-BRM-NUM-EDT
                       DELIMITED BY  ALL ZERO
                   INTO    WRK-STR1
                   WITH    POINTER WRK-IDX2
                   END-UNSTRING
                   MOVE    WRK-BRM-NUM-EDT (WRK-IDX2 : )
                                           TO  WRK-HCMG002-BRMNUM
               ELSE
                   MOVE    WRK-BRM-NUM-EDT TO  WRK-HCMG002-BRMNUM
               END-IF
           ELSE
                   MOVE    WRK-BRM-NUM-EDT TO  WRK-HCMG002-BRMNUM
           END-IF
      *
           .
       2101-BODY-BRMNUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       2101-BODY-SRYKA-EDIT-SEC        SECTION.
      *
      *    診療科情報
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
      *----(02.00.01)--UPD-START---
           MOVE    ISAM-KEY-SRYKA      TO  SYS-1005-KBNCD
      *----(02.00.01)--UPD-END-----
           MOVE    SYU-SKYSTYMD        TO  ORC-DBYMD
grpsys     MOVE    ORC-DBYMD           TO  SYS-1005-STYUKYMD
grpsys                                     SYS-1005-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
grpsys*DD  MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
grpsys     MOVE    "tbl_syskanri"      TO  WRK-DBTABLE
grpsys     MOVE    "key10"             TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               INITIALIZE                  SYS-1005-REC
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
           END-IF
      *
grpsys*DD  MOVE    "SYSKANRI-KEY"      TO  WRK-DBPATH
grpsys     MOVE    "tbl_syskanri"      TO  WRK-DBTABLE
grpsys     MOVE    "key10"             TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           MOVE    SYS-1005-SRYKANAME2 TO  WRK-HCMG002-SRYKA
      *
           .
       2101-BODY-SRYKA-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       2101-BODY-PTINF-EDIT-SEC        SECTION.
      *
      *    患者情報読み込み
           INITIALIZE                      PTINF-REC
grpsys*DD  MOVE    SYU-HOSPID          TO  PTINF-HOSPID
grpsys     MOVE    SYU-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SYU-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
grpsys*DD  MOVE    "PTINF-KEY"         TO  WRK-DBPATH
grpsys     MOVE    "tbl_ptinf"         TO  WRK-DBTABLE
grpsys     MOVE    "key"               TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               INITIALIZE                  PTINF-REC
           ELSE
               MOVE    MCPDATA-REC     TO  PTINF-REC
           END-IF
      *
grpsys     MOVE    "key"               TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
      *    患者番号
      *----(02.00.01)--UPD-START---
           MOVE    ISAM-KEY-PTNUM      TO  WRK-HCMG002-PTNUM
      *----(02.00.01)--UPD-END-----
      *
      *    氏名
           MOVE    PTINF-NAME          TO  WRK-HCMG002-NAME
      *
      *    性別
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"        TO  WRK-HCMG002-SEX
               WHEN    "2"
                   MOVE    "女"        TO  WRK-HCMG002-SEX
           END-EVALUATE
      *
      *    生年月日
           MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3     TO  WRK-HCMG002-BIRTHDAY
      *
           .
       2101-BODY-PTINF-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    合計行編集処理
      *****************************************************************
       210-TOTAL-EDIT-SEC              SECTION.
      *
           MOVE    "頁内合計"          TO  HCMG002-TTLMSG(1)
           MOVE    WRK-SKYMONEY-PAGE   TO  WRK-Z9
           MOVE    WRK-Z9              TO  HCMG002-SEIKYUTTL(1)
      *
           COMPUTE WRK-SKYMONEY-TTL
               =   WRK-SKYMONEY-TTL    +   WRK-SKYMONEY-PAGE
      *
           IF    ( FLG-BG00201         =   1 )
               MOVE    "総合計"        TO  HCMG002-TTLMSG(2)
               MOVE    WRK-SKYMONEY-TTL
                                       TO  WRK-Z9
               MOVE    WRK-Z9          TO  HCMG002-SEIKYUTTL(2)
           END-IF
      *
             .
       210-TOTAL-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-OUT-SEC                SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
      *     MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    WRK-PARA-SRYYM      TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE   "HCMG002.red"        TO  SPRT-PRTID
           MOVE    "定期請求患者一覧表"
                                       TO  SPRT-TITLE
           MOVE    HCMG002             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
grpsys                                 SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       400-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ（ＨＥＡＤレコード）編集処理
      *****************************************************************
       270-CSV-HEAD-SET-SEC        SECTION.
      *
           INITIALIZE                  WRK-REC
                                       WRK-REC-LENGTH
      *
      *    改行コード編集
           STRING  CSV-HEAD-01         DELIMITED  BY  SIZE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                   INTO                WRK-REC
           END-STRING
           PERFORM 500-TOUKEICSV-SEC
      *
           .
       270-CSV-HEAD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ（データレコード）編集処理
      *****************************************************************
       270-CSV-BODY-SET-SEC        SECTION.
      *
           PERFORM 2101-BODY-BTUNAME-EDIT-SEC
           PERFORM 2101-BODY-BRMNUM-EDIT-SEC
           PERFORM 2101-BODY-SRYKA-EDIT-SEC
           PERFORM 2101-BODY-PTINF-EDIT-SEC
      *
           INITIALIZE                  WRK-REC
                                       WRK-REC-LENGTH
      *    診療年月
           MOVE    WRK-PARA-SRYYM  TO  WRK-HEN-YMD-IN
           MOVE    WRK-HEN-YY-IN   TO  WRK-HEN-YY-OT
           MOVE    WRK-HEN-MM-IN   TO  WRK-HEN-MM-OT
           MOVE    SPACE           TO  WRK-HEN-YMD-IN
           MOVE    WRK-HEN-YMD-OT(1:7)
                                   TO  WRK-DATA
           MOVE    7               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    病棟
           MOVE    ISAM-KEY-BTUNUM TO  WRK-DATA
           MOVE    2               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           MOVE    WRK-HCMG002-BTUNAME
                                   TO  WRK-DATA
           MOVE    10              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    病室
           MOVE    WRK-HCMG002-BRMNUM
                                   TO  WRK-DATA
           MOVE    6               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    診療科
           MOVE    ISAM-KEY-SRYKA  TO  WRK-DATA
           MOVE    2               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           MOVE    WRK-HCMG002-SRYKA
                                   TO  WRK-DATA
           MOVE    8               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    患者番号
           MOVE    WRK-HCMG002-PTNUM
                                   TO  WRK-DATA
           MOVE    20              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    患者氏名
           MOVE    WRK-HCMG002-NAME
                                   TO  WRK-DATA
           MOVE    30              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    性別
           MOVE    PTINF-SEX       TO  WRK-DATA
           MOVE    1               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           MOVE    WRK-HCMG002-SEX
                                   TO  WRK-DATA
           MOVE    2               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    生年月日
           MOVE    PTINF-BIRTHDAY  TO  WRK-HEN-YMD-IN
           MOVE    WRK-HEN-YY-IN   TO  WRK-HEN-YY-OT
           MOVE    WRK-HEN-MM-IN   TO  WRK-HEN-MM-OT
           MOVE    WRK-HEN-DD-IN   TO  WRK-HEN-DD-OT
           MOVE    SPACE           TO  WRK-HEN-YMD-IN
           MOVE    WRK-HEN-YMD-OT  TO  WRK-DATA
           MOVE    10              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    伝票番号
           MOVE    SYU-DENPNUM     TO  WRK-DATA
           MOVE    7               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    請求金額
           MOVE    SYU-SKYMONEY    TO  WRK-NUM
           MOVE    13              TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    処理区分
           MOVE    PTTEIKIRRK-SAKKBN   TO  WRK-DATA
           MOVE    1               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           MOVE    SPACE                   TO  WRK-JYOTAI
           EVALUATE    PTTEIKIRRK-SAKKBN
           WHEN    "1"
               MOVE    "１期"              TO  WRK-JYOTAI
           WHEN    "2"
               MOVE    "２期"              TO  WRK-JYOTAI
           WHEN    "3"
               MOVE    "３期"              TO  WRK-JYOTAI
           WHEN    "9"
               MOVE    "月末一括請求"      TO  WRK-JYOTAI
           END-EVALUATE
           MOVE    WRK-JYOTAI      TO  WRK-DATA
           MOVE    16              TO  WRK-NAGASA
           MOVE    1               TO  WRK-RECEND
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                       DELIMITED  BY  SIZE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                   INTO                WRK-REC
           END-STRING
      *
           PERFORM 500-TOUKEICSV-SEC
      *
           .
       2270-CSV-BODY-SET-EXT.
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
           MOVE    WRK-PARA-SRYYM  TO  STOUKEICSV-SRYYM
           MOVE    "1"             TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO            TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"     TO  STOUKEICSV-TO-FOLDER
           MOVE    SPA-HOSPNUM     TO  WRK-TO-DATA-HOSPNUM
           MOVE    WRK-PARA-SRYYM  TO  WRK-TO-DATA-STRYM
           MOVE    WRK-TO-DATA     TO  STOUKEICSV-TO-DATA
           MOVE    "2"             TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-REC         TO  STOUKEICSV-CSVDATA
           MOVE    "定期請求患者一覧ＣＳＶ作成"
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
       5004-MOJI-HENSYU-SEC        SECTION.
      *
           IF      WRK-DATA            =   SPACE
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
           IF      WRK-RECEND         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING        
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF    
      *
           INITIALIZE                      WRK-HENSYU-AREA
           MOVE    ZERO                TO  WRK-NUM    
      *
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（数字）------------9.99999の編集
      *****************************************************************
       5005-NUM-HENSYU-SEC          SECTION.
      *
           IF      WRK-NAGASA          =   ZERO
               CONTINUE
           ELSE                   
      *        開始位置
               IF      WRK-NAGASA          >   13
                   MOVE    1                   TO  WRK-RECSTR
               ELSE
                   COMPUTE WRK-RECSTR  =   15      -   WRK-NAGASA
                   IF      WRK-SYOSUU          >   ZERO
                       COMPUTE WRK-RECSTR  =   WRK-RECSTR
                                           +   WRK-SYOSUU
                                                   +   1
                   END-IF
               END-IF                                
      *     
               PERFORM VARYING IDW2    FROM    WRK-RECSTR  BY  1
                       UNTIL   IDW2    >       14
                   IF      WRK-NUM (IDW2:1)    NOT =   SPACE
                        COMPUTE IDW1    =   15  +   WRK-SYOSUU
                                               -   IDW2       
                        IF      WRK-SYOSUU     >   ZERO
                            ADD     1              TO  IDW1
                        END-IF     
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                   DELIMITED  BY  SIZE
                               WRK-NUM(IDW2:IDW1)   DELIMITED  BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       COMPUTE WRK-REC-LENGTH  =   WRK-REC-LENGTH   +
                                                   IDW1                  
                       MOVE    14              TO  IDW2
                   END-IF
               END-PERFORM
           END-IF    
      *     
           IF      WRK-RECEND         =   ZERO
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
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF        ( STS-RECEERR     =   ZERO )
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
grpsys         MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
grpsys                                     SPA-AREA
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
           MOVE    SPACE           TO  WRK-RECEERR
           STRING  "ORCBG002 "     DELIMITED  BY  SIZE
                   WRK-TOUKEIERR   DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-RECEERR
           END-STRING
           CALL    "cobabort"      USING   WRK-RECEERR
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF    ( CNT-BG00201         =   ZERO )
               MOVE    "処理対象のデータがありませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBG002 IN " CNT-BG00201  " ***"
           DISPLAY "*** ORCBG002 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
grpsys                                 SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       800-ERRMSG-EDIT-SEC             SECTION.
      *
           MOVE    SPACE               TO  ERR-HEN-ERRMSG
	                                       ERR-PTNUM
      *
      *    患者番号を取得
           INITIALIZE                      ORCSPTIDAREA
grpsys*DD  MOVE    SYS-1001-HOSPID     TO  SPTID-HOSPID
grpsys     MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    ERR-PTID            TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
grpsys*DD                                  SPA-1009-TBL
grpsys                                     SPA-AREA
           IF    ( SPTID-RC            =   ZERO )
               MOVE    SPTID-PTNUM     TO  ERR-PTNUM
           ELSE
               STRING "?(ID:"          DELIMITED   BY  SIZE
                      ERR-PTID         DELIMITED   BY  SIZE
                      ")"              DELIMITED   BY  SIZE
               INTO   ERR-PTNUM
               END-STRING
           END-IF
      *
           MOVE    1                   TO  WRK-LEN
           MOVE    SPACE               TO  WRK-STR
           STRING  ERR-PTNUM           DELIMITED   BY  SPACE
           INTO    WRK-STR
           WITH  POINTER WRK-LEN
           END-STRING
      *
           COMPUTE WRK-LEN =   WRK-LEN -   1
      *
           MOVE    ZERO                TO  WRK-SHO
                                           WRK-AMARI
           DIVIDE  2   INTO    WRK-LEN GIVING  WRK-SHO
           REMAINDER WRK-AMARI
           END-DIVIDE
      *
           IF    ( WRK-AMARI   =   ZERO )
               STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                       "［患者番号："      DELIMITED   BY  SIZE
                       ERR-PTNUM           DELIMITED   BY  SPACE
                       "］"                DELIMITED   BY  SIZE
               INTO    ERR-HEN-ERRMSG
               END-STRING
           ELSE
               STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                       "［患者番号:"       DELIMITED   BY  SIZE
                       ERR-PTNUM           DELIMITED   BY  SPACE
                       "］"                DELIMITED   BY  SIZE
               INTO    ERR-HEN-ERRMSG
               END-STRING
           END-IF
      *
           .
       800-ERRMSG-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD1
                                           WRK-EDTYMD3
      *
           IF    ( WRK-SYMD            =   SPACE )
               GO  TO  800-SEIWA-HEN-EXT
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
           INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    定期請求情報ファイル読込処理
      *****************************************************************
       900-BG00201-READ-SEC            SECTION.
      *
           READ    BG00201-FILE NEXT
           AT END
               MOVE    1               TO FLG-BG00201
           NOT AT  END
      *
               COMPUTE CNT-BG00201     =  CNT-BG00201      +   1
      *
               MOVE    BG00201-SYU-DATA-AREA
                                       TO SYUNOU-REC
               MOVE    BG00201-PTTEIKIRRK-DATA-AREA
                                       TO PTTEIKIRRK-REC
      *----(02.00.01)--ADD-START---
               INITIALIZE                 ISAM-AREA
      *
               IF      WRK-PARA-JUNKBN    =   "1"
                   MOVE    BG00201-ISAM-KEY   TO  ISAM-KEY2
                   MOVE    ISAM-KEY2-PTNUM    TO  ISAM-KEY-PTNUM
                   MOVE    ISAM-KEY2-BTUNUM   TO  ISAM-KEY-BTUNUM
                   MOVE    ISAM-KEY2-BRMNUM   TO  ISAM-KEY-BRMNUM
                   MOVE    ISAM-KEY2-SRYKA    TO  ISAM-KEY-SRYKA
                   MOVE    ISAM-KEY2-SAKKBN   TO  ISAM-KEY-SAKKBN
                   MOVE    ISAM-KEY2-SRYYMD   TO  ISAM-KEY-SRYYMD
                   MOVE    ISAM-KEY2-DENPNUM  TO  ISAM-KEY-DENPNUM
               ELSE
                   MOVE    BG00201-ISAM-KEY   TO  ISAM-KEY
               END-IF
      *----(02.00.01)--ADD-END-----
           END-READ
           .
      *
       900-BG00201-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       900-TBL-SELECT-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
grpsys*DD  MOVE    WRK-DBPATH      TO  ORC-DBPATH
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     MOVE    WRK-DBTABLE     TO  MCP-TABLE
grpsys     MOVE    WRK-DBPATH      TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "ORCMCPSUB"         USING
grpsys*DD                              MCPAREA
grpsys*DD                              ORCMCPAREA
grpsys*DD                              MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
grpsys*DD      MOVE    WRK-DBPATH  TO  ORC-DBPATH
               MOVE    "DBFETCH"   TO  MCP-FUNC
grpsys         MOVE    WRK-DBTABLE     TO  MCP-TABLE
grpsys         MOVE    WRK-DBPATH      TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                         MCPDATA-REC
grpsys                                         SPA-AREA
grpsys*DD      CALL    "ORCMCPSUB"     USING
grpsys*DD                              MCPAREA
grpsys*DD                              ORCMCPAREA
grpsys*DD                              MCPDATA-REC
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    1       TO  FLG-DBRC
               END-IF
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       900-TBL-READ-SEC                SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
grpsys*DD  MOVE    WRK-DBPATH      TO  ORC-DBPATH
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     MOVE    WRK-DBTABLE     TO  MCP-TABLE
grpsys     MOVE    WRK-DBPATH      TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "ORCMCPSUB"         USING
grpsys*DD                              MCPAREA
grpsys*DD                              ORCMCPAREA
grpsys*DD                              MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-TBL-CLOSE-SEC               SECTION.
      *
grpsys*DD  MOVE    WRK-DBPATH      TO  ORC-DBPATH
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     MOVE    WRK-DBTABLE     TO  MCP-TABLE
grpsys     MOVE    WRK-DBPATH      TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "ORCMCPSUB"         USING
grpsys*DD                              MCPAREA
grpsys*DD                              ORCMCPAREA
grpsys*DD                              MCPDATA-REC
      *
           .
       900-TBL-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE.
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "ORCMCPSUB"          USING
grpsys*DD                               MCPAREA
grpsys*DD                               ORCMCPAREA
grpsys*DD                               MCPDATA-REC.
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE.
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "ORCMCPSUB"          USING
grpsys*DD                               MCPAREA
grpsys*DD                               ORCMCPAREA
grpsys*DD                               MCPDATA-REC.
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE.
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "ORCMCPSUB"          USING
grpsys*DD                               MCPAREA
grpsys*DD                               ORCMCPAREA
grpsys*DD                               MCPDATA-REC.
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE.
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "ORCMCPSUB"          USING
grpsys*DD                               MCPAREA
grpsys*DD                               ORCMCPAREA
grpsys*DD                               MCPDATA-REC.
           .
       900-DBDISCONNECT-EXT.
           EXIT.
