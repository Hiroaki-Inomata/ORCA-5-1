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
       PROGRAM-ID.             ORCBD005V02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次帳票
      *  コンポーネント名  : 収納一覧表（ＨＣＭ０８）入院 期間指定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/03/06    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  00.00.01    NACL-太田    03/04/25  病棟・病室名の編集を追加
      *  03.05.01    NACL-森脇    07/06/11  グループ診療対応
      *  04.08.01    NACL-森脇    14/07/29  一時ファイルディレクトリ設定
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      * 
      *    収納明細処理ファイル（初期化／読込用）
           SELECT  BD00501-FILE   ASSIGN   ORCBD005PARA
                                  ORGANIZATION    IS  INDEXED
                                  ACCESS  MODE    IS  DYNAMIC
                                  RECORD  KEY     IS  BD00501-ISAM-KEY
                                  FILE    STATUS  IS  STS-BD00501.
      * 
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                            SECTION.
      *
       FD  BD00501-FILE.
       01  BD00501-REC.
           02  BD00501-ISAM-KEY.
               03  BD00501-ISAM-KEY-VAL            PIC X(100).
               03  BD00501-ISAM-KEY1   REDEFINES   BD00501-ISAM-KEY-VAL.
                   05  BD00501-ISAM-KEY1-BTUNUM    PIC X(02).
                   05  BD00501-ISAM-KEY1-BRMNUM    PIC X(08).
                   05  BD00501-ISAM-KEY1-SRYKA     PIC X(02).
                   05  BD00501-ISAM-KEY1-PTNUM     PIC X(20).
                   05  BD00501-ISAM-KEY1-DENPNUM   PIC 9(07).
                   05  BD00501-ISAM-KEY1-DENPEDANUM PIC 9(02).
                   05  BD00501-ISAM-KEY1-FIL       PIC X(59).
               03  BD00501-ISAM-KEY2-G   REDEFINES BD00501-ISAM-KEY-VAL.
                   05  BD00501-ISAM-KEY2-DENPNUM   PIC 9(07).
                   05  BD00501-ISAM-KEY2-DENPEDANUM PIC 9(02).
                   05  BD00501-ISAM-KEY2-FIL       PIC X(91).
           02  BD00501-SYU-DATA-AREA.
               03  BD00501-SYU-BTUNUM          PIC X(02).
               03  BD00501-SYU-BRMNUM          PIC X(08).
               03  BD00501-SYU-SRYKA           PIC X(02).
               03  BD00501-SYU-PTNUM           PIC X(20).
               03  BD00501-SYU-DENPNUM         PIC 9(07).
               03  BD00501-SYU-DENPEDANUM      PIC 9(02).
               03  BD00501-SYU-CREATEKBN       PIC X(01).
           02  BD00501-SMEI-DATA-AREA.
           COPY    "CPSYUMEI.INC"  REPLACING   //SMEI//
                                   BY          //BD00501-SMEI//.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    収納明細処理ファイル
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING   //RECE01//
                                   BY          //ORCBD005//.
      *----(04.08.01)--UPD-START---
      *     03  FILLER              PIC X(04)   VALUE   ".dat".
      *----(04.08.01)--UPD-END-----
      *
      *    エラーファイル 名称領域 
grpsys*DD  COPY    "CPCOMMONDAT2.INC"
grpsys*DD                          REPLACING   //RECE01PARA//
grpsys*DD                          BY          //RECEERR//.
grpsys*DD  03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *----(04.08.01)--ADD-START---
           COPY    "CPRECEERR.INC".
      *----(04.08.01)--ADD-END-----
      *
           COPY    "HCM08V02.INC"  REPLACING   //HCM08V02//
                                   BY          //HCM08//.
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-BD00501         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYU             PIC 9(01).
           03  FLG-SMEI            PIC 9(01).
           03  FLG-SMEI2           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-NEXTPAGE        PIC 9(01).
           03  FLG-MAINEXT         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
           03  CNT-DENP            PIC 9(06).
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
           03  IDX1                PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
grpsys*DD  03  WRK-PARA-HOSPID     PIC X(24).
grpsys     03  WRK-PARA-HOSPNUM    PIC 9(02).
      *----(04.08.01)--UPD-START---
grpsys*     03  RECEERR             PIC X(100).
      *----(04.08.01)--UPD-END-----
           03  WRK-PARA-SYORI-KBN  PIC X(01).
           03  WRK-PARA-SORTKBN    PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR         PIC X(200).
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
           03  WRK-HYYMMDD         PIC X(09).
           03  WRK-MAE-INPUT       PIC X(300).
           03  WRK-OUT-INPUT       PIC X(300).
           03  WRK-OUT-MAX         PIC 9(05).
      *
           03  WRK-Z9              PIC ----,---,--9.
      *
           03  WRK-SYORIYMD        PIC X(24).
           03  WRK-PAGE            PIC ZZ9.
           03  WRK-HJYOTAI         PIC X(16).
           03  WRK-MAX             PIC 9(02).
      *
           03  WRK-EDT-AREA.
grpsys*DD      05  WRK-EDT-HOSPID      PIC X(24).
grpsys         05  WRK-EDT-HOSPNUM     PIC 9(02).
               05  WRK-EDT-PTID        PIC 9(10).
               05  WRK-EDT-PTNUM       PIC X(20).
               05  WRK-EDT-BTUNUM      PIC X(02).
               05  WRK-EDT-BRMNUM      PIC X(08).
               05  WRK-EDT-SRYKA       PIC X(02).
           03  WRK-OUT-AREA.
               05  WRK-OUT-BTUNAME     PIC X(10).
               05  WRK-OUT-BRMNUM      PIC X(06).
               05  WRK-OUT-SRYKA       PIC X(08).
               05  WRK-OUT-PTNUM       PIC X(20).
               05  WRK-OUT-NAME        PIC X(36).
               05  WRK-OUT-SEX         PIC X(2).
               05  WRK-OUT-BIRTHDAY    PIC X(22).
           03  WRK-NAI-AREA.
grpsys*DD      05  WRK-NAI-HOSPID      PIC X(24).
grpsys         05  WRK-NAI-HOSPNUM     PIC 9(02).
               05  WRK-NAI-PTID        PIC 9(10).
               05  WRK-NAI-PTNUM       PIC X(20).
               05  WRK-NAI-BTUNUM      PIC X(02).
               05  WRK-NAI-BRMNUM      PIC X(08).
               05  WRK-NAI-SRYKA       PIC X(02).
               05  WRK-NAI-DENPNUM     PIC 9(07).
               05  WRK-NAI-DENPNUM-X   REDEFINES WRK-NAI-DENPNUM
                                       PIC X(07).
               05  WRK-NAI-TBL.
                   07  WRK-NAI-G       OCCURS  30.
                       09  WRK-SEIKYU  PIC S9(07).
                       09  WRK-RYOSYU  PIC S9(07).
                       09  WRK-MISYU   PIC S9(07).
                       09  WRK-JYOTAI  PIC X(16).
      *
           03  WRK-SUM-TBL         OCCURS  4.
               05  WRK-GSEIKYU     PIC S9(08).
               05  WRK-GRYOSYU     PIC S9(08).
               05  WRK-GMISYU      PIC S9(08).
           03  WRK-SPTID-PTNUM     PIC X(20).
           03  WRK-SPTID-PTID      PIC 9(10).
           03  WRK-BRM-NUM-IN                      PIC X(06).
           03  WRK-BRM-NUM-EDT                     PIC X(06).
           03  WRK-STR                             PIC X(300).
           03  WRK-STR1                            PIC X(20).
           03  WRK-STR2                            PIC X(20).
           03  WRK-IDX1                            PIC 9(05).
           03  WRK-IDX2                            PIC 9(05).
           03  WRK-STDAILYKEY.
               05  WRK-STDAILYKEY-YMD              PIC X(08).
               05  WRK-STDAILYKEY-TIME             PIC X(04).
           03  WRK-EDDAILYKEY.
               05  WRK-EDDAILYKEY-YMD              PIC X(08).
               05  WRK-EDDAILYKEY-TIME             PIC X(04).
           03  WRK-KIKAN-G.
               05  WRK-KIKANSTYMD                  PIC X(22).
               05  WRK-KIKANSTTIME.
                   07  WRK-KIKANSTTIME-HH          PIC X(04).
                   07  WRK-KIKANSTTIME-MM          PIC X(04).
               05  WRK-KIKANEDYMD                  PIC X(22).
               05  WRK-KIKANEDTIME.
                   07  WRK-KIKANEDTIME-HH          PIC X(04).
                   07  WRK-KIKANEDTIME-MM          PIC X(04).
           03  WRK-CREYMD                          PIC X(30).
      *
       01  KEY-AREA.
           03  KEY-BTUNUM          PIC X(02).
           03  KEY-BRMNUM          PIC X(08).
           03  KEY-SRYKA           PIC X(02).
           03  KEY-PTID            PIC 9(10).
           03  KEY-DENPNUM         PIC 9(07).
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    患者番号構成情報
           COPY    "CPSK1009.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    病棟情報
           COPY    "CPSK5001.INC".
      *
      *    締め時刻情報
           COPY    "CPSK3005.INC".
      *
      *    収納マスタ
       01  SYU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細マスタ
       01  SMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    収納明細履歴マスタ
       01  SMEI2-REC.
           COPY    "CPSYUMEI.INC"  REPLACING   //SMEI//
                                   BY          //SMEI2//.
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号変換マスタ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
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
      *    文字列変換
           COPY    "CPORCXKANACONV.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *----(04.08.01)--ADD-START---
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      *
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
                   UNTIL   FLG-END     =   1
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  KEY-AREA
           INITIALIZE                  SPA-AREA
      *----(04.08.01)--ADD-START---
           INITIALIZE                  RECEERR
      *----(04.08.01)--ADD-END-----
      *
           PERFORM 100-DBOPEN-SEC
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
grpsys*DD                                      WRK-PARA-HOSPID
grpsys                                         WRK-PARA-HOSPNUM
grpsys                                         RECEERR
                                               WRK-PARA-SYORI-KBN
                                               WRK-PARA-SORTKBN
grpsys     END-UNSTRING
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    "ORCBD005V02"   TO  JOB-PGID
           MOVE    "収納一覧表（入院）（期間指定）"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
grpsys                                 SPA-AREA
      *
grpsys*DD  MOVE    "RECEERR"       TO  RECEERR-FILE-ID
grpsys*DD  MOVE    LNK-PRTKANRI-TERMID
grpsys*DD                          TO  RECEERR-TERMID
      *
      *    収納明細処理ファイル名
           MOVE    "BD00501"       TO  ORCBD005PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  ORCBD005PARA-TERMID
grpsys     MOVE    SPA-HOSPNUM     TO  ORCBD005PARA-HOSPNUM
      *
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    ORCBD005PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ORCBD005PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECEERR
      *----(04.08.01)--ADD-END-----
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
           IF    ( FLG-END     NOT =   ZERO )
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    対象収納明細ファイル出力処理
           PERFORM 110-SMEI-OUT-SEC
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
                                   TO  WRK-SYSYMD
      *
      *    締め時刻情報
           INITIALIZE                  SYS-3005-REC
           MOVE    "3005"          TO  SYS-3005-KANRICD
           MOVE    "*"             TO  SYS-3005-KBNCD
           MOVE    WRK-SYSYMD      TO  SYS-3005-STYUKYMD
                                       SYS-3005-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-3005-HOSPNUM
           MOVE    SYS-3005-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-3005-REC
      *
               MOVE    SYS-3005-STSRYYMD   TO  WRK-STDAILYKEY-YMD
               MOVE    SYS-3005-STTIME     TO  WRK-STDAILYKEY-TIME
               MOVE    SYS-3005-EDSRYYMD   TO  WRK-EDDAILYKEY-YMD
               MOVE    SYS-3005-EDTIME     TO  WRK-EDDAILYKEY-TIME
      *
               MOVE    SYS-3005-STSRYYMD   TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  WRK-KIKANSTYMD
      *
               MOVE    SYS-3005-STTIME     TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  WRK-KIKANSTTIME
      *
               MOVE    SYS-3005-EDSRYYMD   TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               IF    ( SYS-3005-STSRYYMD (1:4)
                                           =   SYS-3005-EDSRYYMD (1:4))
                   MOVE    WRK-HENYMDG (11:)   TO  WRK-KIKANEDYMD
               ELSE
                   MOVE    WRK-HENYMDG (5:)    TO  WRK-KIKANEDYMD
               END-IF
      *
               MOVE    SYS-3005-EDTIME     TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  WRK-KIKANEDTIME
      *
           ELSE
               MOVE    "締め処理にて対象期間を登録してください"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    患者番号構成管理情報編集
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    WRK-SYSYMD          TO  SYS-1009-STYUKYMD
                                           SYS-1009-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-KEY10-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
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
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
           EVALUATE WRK-PARA-SYORI-KBN
           WHEN    SPACE
               MOVE    "0"             TO  WRK-PARA-SYORI-KBN
           WHEN    "0"
           WHEN    "1"
           WHEN    "2"
               CONTINUE
           WHEN    OTHER
               MOVE    SPACE           TO  WRK-RECEERR
               STRING  "処理区分が誤りです：["     DELIMITED   BY  SIZE
                       WRK-PARA-SYORI-KBN          DELIMITED   BY  SIZE
                       "]"                         DELIMITED   BY  SIZE
               INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-EVALUATE
      *
           IF    ( WRK-PARA-SYORI-KBN   =   "2" )
               MOVE    SPACE           TO  WRK-PARA-SORTKBN
           END-IF
      *
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           STRING  "作成日："              DELIMITED   BY  SIZE
                   WRK-HENYMDG             DELIMITED   BY  SIZE
           INTO    WRK-CREYMD
           END-STRING
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル出力処理
      *****************************************************************
       110-SMEI-OUT-SEC            SECTION.
      *
           OPEN    OUTPUT              BD00501-FILE
      *
      *    収納明細をソートする為、一度ファイルに退避する
           MOVE    ZERO            TO  FLG-SMEI
           PERFORM 900-DBSELECT-SEC
      *
           PERFORM UNTIL ( FLG-SMEI    =   1 )
      *
      *        収納データ取得
               PERFORM 1101-SYUNOU-GET-SEC
               MOVE    SYU-BYOTONUM    TO  BD00501-SYU-BTUNUM
               MOVE    SYU-ROOMNUM     TO  BD00501-SYU-BRMNUM
               MOVE    SYU-SRYKA       TO  BD00501-SYU-SRYKA
      *
               MOVE    SMEI-PTID       TO  WRK-SPTID-PTID
               PERFORM 1101-PTNUM-GET-SEC
               MOVE    WRK-SPTID-PTNUM TO  BD00501-SYU-PTNUM
      *
               MOVE    SMEI-DENPNUM    TO  BD00501-SYU-DENPNUM
               MOVE    SMEI-DENPEDANUM TO  BD00501-SYU-DENPEDANUM
      *
               MOVE    SYU-CREATEKBN   TO  BD00501-SYU-CREATEKBN
               MOVE    SMEI-REC        TO  BD00501-SMEI-DATA-AREA
      *
               IF    ( WRK-PARA-SORTKBN =   "1" )
                   MOVE    BD00501-SYU-DENPNUM
                                       TO  BD00501-ISAM-KEY2-DENPNUM
                   MOVE    BD00501-SYU-DENPEDANUM
                                       TO  BD00501-ISAM-KEY2-DENPEDANUM
               ELSE
                   MOVE    BD00501-SYU-BTUNUM
                                       TO  BD00501-ISAM-KEY1-BTUNUM
                   MOVE    BD00501-SYU-BRMNUM
                                       TO  BD00501-ISAM-KEY1-BRMNUM
                   MOVE    BD00501-SYU-SRYKA
                                       TO  BD00501-ISAM-KEY1-SRYKA
                   MOVE    BD00501-SYU-PTNUM
                                       TO  BD00501-ISAM-KEY1-PTNUM
                   MOVE    BD00501-SYU-DENPNUM
                                       TO  BD00501-ISAM-KEY1-DENPNUM
                   MOVE    BD00501-SYU-DENPEDANUM
                                       TO  BD00501-ISAM-KEY1-DENPEDANUM
               END-IF
      *
               WRITE   BD00501-REC
      *
               PERFORM 900-DBFETCH-SEC
      *
           END-PERFORM
      *
           PERFORM 900-DBCLOSE-SEC
      *
           CLOSE   BD00501-FILE
      *
           .
       110-SMEI-OUT-EXT.
           EXIT.
      *****************************************************************
      *    収納データ取得処理
      *****************************************************************
       1101-SYUNOU-GET-SEC             SECTION.
      *
      *    収納読み込み
           INITIALIZE                  SYU-REC
grpsys*DD  MOVE    SMEI-HOSPID     TO  SYU-HOSPID
grpsys     MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    SMEI-NYUGAIKBN  TO  SYU-NYUGAIKBN
           MOVE    SMEI-PTID       TO  SYU-PTID
           MOVE    SMEI-DENPNUM    TO  SYU-DENPNUM
           PERFORM 900-SYU-READ-SEC
      *
           .
       1101-SYUNOU-GET-EXT.
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
                                       KEY-DENPNUM
      *
           MOVE    ZERO            TO  FLG-NEXTPAGE
      *
           OPEN    INPUT           BD00501-FILE
      *
           MOVE    ZERO            TO  FLG-SMEI
           PERFORM 900-BD00501-READ-SEC
           IF    ( FLG-SMEI    NOT =   ZERO )
               MOVE    1           TO  FLG-END
           END-IF
      *
           PERFORM UNTIL   FLG-END     =   1
      *        帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
      *        帳票編集<明細>処理
               PERFORM 320-BODY-HEN-SEC
      *        帳票編集<合計>処理
               PERFORM 330-TERM-HEN-SEC
      *        帳票印刷処理
               PERFORM 390-PRINT-OUT-SEC
           END-PERFORM
      *
           CLOSE   BD00501-FILE
      *
      
      *    ファイル削除
           MOVE    ZERO        TO  IF-RESULT
           MOVE    ORCBD005PARA
	                       TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           .
      *
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC            SECTION.
      *
           INITIALIZE                  WRK-SUM-TBL(1)
      *
           MOVE    ZERO            TO  CNT-LINE
           ADD     1               TO  CNT-PAGE
      *
           MOVE    SPACE           TO  HCM08
      *
           IF    ( SYS-3005-STSRYYMD   =   SYS-3005-EDSRYYMD )
               STRING "期間："             DELIMITED   BY  SIZE
                      WRK-KIKANSTYMD       DELIMITED   BY  SIZE
                      WRK-KIKANSTTIME-HH   DELIMITED   BY  SIZE
                      "："                 DELIMITED   BY  SIZE
                      WRK-KIKANSTTIME-MM   DELIMITED   BY  SIZE
                      "〜"                 DELIMITED   BY  SIZE
                      WRK-KIKANEDTIME-HH   DELIMITED   BY  SIZE
                      "："                 DELIMITED   BY  SIZE
                      WRK-KIKANEDTIME-MM   DELIMITED   BY  SIZE
               INTO   HCM08-KIKAN
               END-STRING
           ELSE
               STRING "期間："             DELIMITED   BY  SIZE
                      WRK-KIKANSTYMD       DELIMITED   BY  SIZE
                      WRK-KIKANSTTIME-HH   DELIMITED   BY  SIZE
                      "："                 DELIMITED   BY  SIZE
                      WRK-KIKANSTTIME-MM   DELIMITED   BY  SIZE
                      "〜"                 DELIMITED   BY  SIZE
                      WRK-KIKANEDYMD       DELIMITED   BY  SPACE
                      WRK-KIKANEDTIME-HH   DELIMITED   BY  SIZE
                      "："                 DELIMITED   BY  SIZE
                      WRK-KIKANEDTIME-MM   DELIMITED   BY  SIZE
               INTO   HCM08-KIKAN
               END-STRING
           END-IF
      *
           MOVE    CNT-PAGE        TO  WRK-PAGE
           MOVE    WRK-PAGE        TO  HCM08-PAGE
      *
           EVALUATE    WRK-PARA-SYORI-KBN
           WHEN    "0"
               MOVE    "（収納明細別）"
                                   TO  HCM08-SYORIKBN
           WHEN    "1"
               MOVE    "（伝票別）"
                                   TO  HCM08-SYORIKBN
           WHEN    "2"
               MOVE    "（患者別）"
                                   TO  HCM08-SYORIKBN
           END-EVALUATE
      *
           MOVE    WRK-CREYMD      TO  HCM08-CREYMD
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
           PERFORM UNTIL ( FLG-END         =   1 )
                   OR    ( CNT-LINE       >=   30 )
      *
               IF    ( FLG-NEXTPAGE        =   1 )
                   PERFORM 3202-NEXT-PAGE-HEN-SEC
               END-IF
      *
               IF    ( FLG-END         =   ZERO )
                   PERFORM 3202-BODY-MAIN-HEN-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行　次ページ>処理
      *****************************************************************
       3202-NEXT-PAGE-HEN-SEC          SECTION.
      *
           INITIALIZE                  WRK-OUT-AREA
      *
           MOVE    WRK-NAI-BTUNUM  TO  WRK-EDT-BTUNUM
           PERFORM 3200-BODY-BTUNAME-HEN-SEC
      *
           MOVE    WRK-NAI-BRMNUM  TO  WRK-EDT-BRMNUM
           PERFORM 3200-BODY-BRMNUM-HEN-SEC
      *
           MOVE    WRK-NAI-SRYKA   TO  WRK-EDT-SRYKA
           PERFORM 3200-BODY-SRYKA-HEN-SEC
      *
grpsys*DD  MOVE    WRK-NAI-HOSPID  TO  WRK-EDT-HOSPID
grpsys     MOVE    WRK-NAI-HOSPNUM TO  WRK-EDT-HOSPNUM
           MOVE    WRK-NAI-PTID    TO  WRK-EDT-PTID
           MOVE    WRK-NAI-PTNUM   TO  WRK-EDT-PTNUM
           PERFORM 3201-BODY-PTID-HEN-SEC
      *
           PERFORM 3203-BODY-NAIYO-SET-SEC
      *
           MOVE    ZERO            TO  FLG-NEXTPAGE
      *
           .
       3202-NEXT-PAGE-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3202-BODY-MAIN-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-DENP
           INITIALIZE                      WRK-NAI-AREA
      *
           PERFORM 3202-BODY-MIDASI-HEN-SEC
      *
grpsys*DD  MOVE    BD00501-SMEI-HOSPID     TO  WRK-NAI-HOSPID
grpsys     MOVE    SPA-HOSPNUM             TO  WRK-NAI-HOSPNUM
           MOVE    BD00501-SMEI-PTID       TO  WRK-NAI-PTID
           MOVE    BD00501-SYU-PTNUM  TO  WRK-NAI-PTNUM
           MOVE    BD00501-SYU-BTUNUM TO  WRK-NAI-BTUNUM
           MOVE    BD00501-SYU-BRMNUM TO  WRK-NAI-BRMNUM
           MOVE    BD00501-SYU-SRYKA  TO  WRK-NAI-SRYKA
      *
           INITIALIZE                      WRK-SUM-TBL(4)
      *
      *    伝票履歴チェック
           PERFORM 3202-BODY-DENPRRK-CHK-SEC
      *
      *    キーを設定
           MOVE    BD00501-SYU-BTUNUM
                                       TO  KEY-BTUNUM
           MOVE    BD00501-SYU-BRMNUM
                                       TO  KEY-BRMNUM
           MOVE    BD00501-SYU-SRYKA
                                       TO  KEY-SRYKA
           MOVE    SMEI-PTID           TO  KEY-PTID
           MOVE    BD00501-SYU-DENPNUM
                                       TO  KEY-DENPNUM
           MOVE    ZERO                TO  FLG-MAINEXT
      *
           PERFORM
               UNTIL ( FLG-SMEI        =   1 )
               OR    ( KEY-BTUNUM  NOT =   BD00501-SYU-BTUNUM )
               OR    ( KEY-BRMNUM  NOT =   BD00501-SYU-BRMNUM )
               OR    ( KEY-SRYKA   NOT =   BD00501-SYU-SRYKA )
               OR    ( KEY-PTID    NOT =   SMEI-PTID )
      *        OR    ( KEY-DENPNUM NOT =   BD00501-SYU-DENPNUM )
               OR    ( FLG-MAINEXT NOT =   ZERO )
               OR    ( CNT-DENP       >=   30 )
      *
      *
      *        処理区分が0（収納明細単位）か1（伝票番号単位）
      *        の場合、伝票番号が変わったらループを抜ける
               IF    ( KEY-DENPNUM NOT =   BD00501-SYU-DENPNUM )
                   IF    ( WRK-PARA-SYORI-KBN  =   "0" OR  "1" )
                       MOVE    1       TO  FLG-MAINEXT
                   ELSE
      *
      *                伝票履歴チェック
                       PERFORM 3202-BODY-DENPRRK-CHK-SEC
                       MOVE    BD00501-SYU-DENPNUM
                                       TO  KEY-DENPNUM
                   END-IF
               END-IF
      *
      *
               IF    ( FLG-MAINEXT     =   ZERO )
      *
                   PERFORM 3202-BODY-MAIN-NAIYO-HEN-SEC
                   PERFORM 900-BD00501-READ-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           COMPUTE IDY         =   30  -   CNT-LINE
           IF        ( CNT-DENP    >  IDY )
               MOVE    30          TO  CNT-LINE
               MOVE    1           TO  FLG-NEXTPAGE
           ELSE
               PERFORM 3203-BODY-NAIYO-SET-SEC
           END-IF
      *
           .
       3202-BODY-MAIN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    見出し項目編集処理
      *****************************************************************
       3202-BODY-MIDASI-HEN-SEC        SECTION.
      *
           INITIALIZE                  WRK-OUT-AREA
      *
           IF    ( CNT-LINE        =   ZERO )
            OR   ( WRK-PARA-SORTKBN =  "1" )
            OR   ( KEY-BTUNUM  NOT =   BD00501-SYU-BTUNUM )
               MOVE    BD00501-SYU-BTUNUM
                                       TO  WRK-EDT-BTUNUM
               PERFORM 3200-BODY-BTUNAME-HEN-SEC
           END-IF
      *
           IF    ( CNT-LINE        =   ZERO )
            OR   ( WRK-PARA-SORTKBN =  "1" )
            OR   ( KEY-BTUNUM  NOT =   BD00501-SYU-BTUNUM )
            OR   ( KEY-BRMNUM  NOT =   BD00501-SYU-BRMNUM )
               MOVE    BD00501-SYU-BRMNUM
                                       TO  WRK-EDT-BRMNUM
               PERFORM 3200-BODY-BRMNUM-HEN-SEC
           END-IF
      *
           IF    ( CNT-LINE        =   ZERO )
            OR   ( WRK-PARA-SORTKBN =  "1" )
            OR   ( KEY-BTUNUM  NOT =   BD00501-SYU-BTUNUM )
            OR   ( KEY-BRMNUM  NOT =   BD00501-SYU-BRMNUM )
            OR   ( KEY-SRYKA   NOT =   BD00501-SYU-SRYKA )
               MOVE    BD00501-SYU-SRYKA
                                       TO  WRK-EDT-SRYKA
               PERFORM 3200-BODY-SRYKA-HEN-SEC
           END-IF
      *
           IF    ( CNT-LINE        =   ZERO )
            OR   ( WRK-PARA-SORTKBN =  "1" )
            OR   ( KEY-BTUNUM  NOT =   BD00501-SYU-BTUNUM )
            OR   ( KEY-BRMNUM  NOT =   BD00501-SYU-BRMNUM )
            OR   ( KEY-SRYKA   NOT =   BD00501-SYU-SRYKA )
            OR   ( KEY-PTID    NOT =   SMEI-PTID )
grpsys*DD      MOVE    SMEI-HOSPID     TO  WRK-EDT-HOSPID
grpsys         MOVE    SMEI-HOSPNUM    TO  WRK-EDT-HOSPNUM
               MOVE    SMEI-PTID       TO  WRK-EDT-PTID
               MOVE    BD00501-SYU-PTNUM
                                       TO  WRK-EDT-PTNUM
               PERFORM 3201-BODY-PTID-HEN-SEC
           END-IF
      *
           .
       3202-BODY-MIDASI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<明細行−内容>処理
      *****************************************************************
       3202-BODY-MAIN-NAIYO-HEN-SEC    SECTION.
      *
      *    処理区分が0（収納明細単位）の場合のみ、カウントアップ
      *    処理区分が0以外は1固定
           IF    ( WRK-PARA-SYORI-KBN  =   "0" )
               ADD     1               TO  CNT-DENP
           ELSE
               MOVE    1               TO  CNT-DENP
           END-IF
      *
      *    伝票番号
           IF    ( WRK-NAI-DENPNUM-X   =   ZERO OR SPACE)
               MOVE    SMEI-DENPNUM    TO  WRK-NAI-DENPNUM
           ELSE
               IF    ( WRK-NAI-DENPNUM-X   =   ALL "*" )
                OR   ( SMEI-DENPNUM    NOT =   WRK-NAI-DENPNUM )
                   MOVE    ALL "*"     TO  WRK-NAI-DENPNUM-X
               END-IF
           END-IF
      *
           IF    ( WRK-PARA-SYORI-KBN  =   "0" )
      *        処理区分
               EVALUATE    SMEI-JOUTAIKBN
               WHEN    SPACE
                   MOVE    "請求なし"  TO  WRK-HJYOTAI
                   MOVE    9           TO  WRK-MAX
               WHEN    "1"
                   MOVE    "請求・入金"
                                       TO  WRK-HJYOTAI
                   MOVE    11          TO  WRK-MAX
               WHEN    "2"
                   MOVE    "入金"      TO  WRK-HJYOTAI
                   MOVE    5           TO  WRK-MAX
               WHEN    "3"
                   MOVE    "再計算"    TO  WRK-HJYOTAI
                   MOVE    7           TO  WRK-MAX
               WHEN    "4"
                   MOVE    "返金"      TO  WRK-HJYOTAI
                   MOVE    5           TO  WRK-MAX
               WHEN    "5"
                   MOVE    "請求取消"  TO  WRK-HJYOTAI
                   MOVE    9           TO  WRK-MAX
               WHEN    "6"
                   MOVE    "入金取消"  TO  WRK-HJYOTAI
                   MOVE    9           TO  WRK-MAX
               WHEN    "8"
                   MOVE    "診療訂正"  TO  WRK-HJYOTAI
                   MOVE    9           TO  WRK-MAX
               WHEN    "9"
                   MOVE    "請求"      TO  WRK-HJYOTAI
                   MOVE    5           TO  WRK-MAX
               WHEN    OTHER
                   MOVE    SPACE       TO  WRK-HJYOTAI
                   MOVE    1           TO  WRK-MAX
               END-EVALUATE
      *
               EVALUATE    BD00501-SYU-CREATEKBN
               WHEN    "1"
                   MOVE    "（退）"    TO  WRK-HJYOTAI(WRK-MAX:6)
               WHEN    "2"
                   MOVE    "（定）"    TO  WRK-HJYOTAI(WRK-MAX:6)
               END-EVALUATE
      *
               MOVE    WRK-HJYOTAI     TO  WRK-JYOTAI(CNT-DENP)
           END-IF
      *
           COMPUTE WRK-GSEIKYU(4)
               =   WRK-GSEIKYU(4)      +   SMEI-SKYMONEY
      *
           COMPUTE WRK-GRYOSYU(4)
               =   WRK-GRYOSYU(4)      +   SMEI-NYUHEN-MONEY
      *
           COMPUTE WRK-GMISYU(4)
               =   WRK-GSEIKYU(4)      -   WRK-GRYOSYU(4)
      *
      *    請求額
           COMPUTE WRK-SEIKYU(CNT-DENP)
               =   WRK-SEIKYU(CNT-DENP)    +   SMEI-SKYMONEY
      *
      *    領収金額
           COMPUTE WRK-RYOSYU(CNT-DENP)
               =   WRK-RYOSYU(CNT-DENP)    +   SMEI-NYUHEN-MONEY
      *
      *    未収金額
           MOVE    WRK-GMISYU(4)       TO  WRK-MISYU(CNT-DENP)
      *
           .
       3202-BODY-MAIN-NAIYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    伝票履歴チェック処理
      *****************************************************************
       3202-BODY-DENPRRK-CHK-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-SMEI2
           PERFORM 900-SMEI2-SELECT-SEC
           PERFORM UNTIL    FLG-SMEI2  =   1
               COMPUTE WRK-GSEIKYU(4)  =   WRK-GSEIKYU(4)
                                       +   SMEI2-SKYMONEY
               COMPUTE WRK-GRYOSYU(4)  =   WRK-GRYOSYU(4)
                                       +   SMEI2-NYUHEN-MONEY
               PERFORM 900-SMEI2-FETCH-SEC
           END-PERFORM
           PERFORM 900-SMEI2-CLOSE-SEC
      *
           .
       3202-BODY-DENPRRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟名称編集処理
      *****************************************************************
       3200-BODY-BTUNAME-HEN-SEC      SECTION.
      *
      *    病棟情報
           INITIALIZE                  SYS-5001-REC
           MOVE    "5001"          TO  SYS-5001-KANRICD
           MOVE    WRK-EDT-BTUNUM
                                   TO  SYS-5001-KBNCD
           MOVE    WRK-SYSYMD      TO  SYS-5001-STYUKYMD
                                       SYS-5001-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-KEY10-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-5001-REC
               MOVE    SYS-5001-BTU-TANNAME
                                       TO  WRK-OUT-BTUNAME
           END-IF
      *
           .
       3200-BODY-BTUNAME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    病室名称編集処理
      *****************************************************************
       3200-BODY-BRMNUM-HEN-SEC        SECTION.
      *
           MOVE   WRK-EDT-BRMNUM ( 3 : )
                                       TO  WRK-BRM-NUM-IN
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
               MOVE    WRK-BRM-NUM-EDT TO  WRK-OUT-BRMNUM
               GO  TO  3200-BODY-BRMNUM-HEN-EXT
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
                                           TO  WRK-OUT-BRMNUM
               ELSE
                   MOVE    WRK-BRM-NUM-EDT TO  WRK-OUT-BRMNUM
               END-IF
           ELSE
                   MOVE    WRK-BRM-NUM-EDT TO  WRK-OUT-BRMNUM
           END-IF
      *
           .
       3200-BODY-BRMNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       3200-BODY-SRYKA-HEN-SEC      SECTION.
      *
      *    診療科情報
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"          TO  SYS-1005-KANRICD
           MOVE    WRK-EDT-SRYKA
                                   TO  SYS-1005-KBNCD
           MOVE    WRK-SYSYMD      TO  SYS-1005-STYUKYMD
                                       SYS-1005-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-KEY10-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME
                                       TO  WRK-OUT-SRYKA
           END-IF
      *
           .
       3200-BODY-SRYKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       3201-BODY-PTID-HEN-SEC      SECTION.
      *
      *    患者情報読み込み
           INITIALIZE                  PTINF-REC
grpsys*DD  MOVE    WRK-EDT-HOSPID  TO  PTINF-HOSPID
grpsys     MOVE    WRK-EDT-HOSPNUM TO  PTINF-HOSPNUM
           MOVE    WRK-EDT-PTID
                                   TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
      *    患者番号
           MOVE    WRK-EDT-PTNUM   TO  WRK-OUT-PTNUM
      *
      *    氏名
           MOVE    PTINF-NAME      TO  WRK-OUT-NAME
      *
      *    性別
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"    TO  WRK-OUT-SEX
               WHEN    "2"
                   MOVE    "女"    TO  WRK-OUT-SEX
           END-EVALUATE
      *
      *    生年月日
           MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HYYMMDD     TO  WRK-OUT-BIRTHDAY
      *
           .
       3201-BODY-PTID-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細>処理
      *****************************************************************
       3203-BODY-NAIYO-SET-SEC     SECTION.
      *
           MOVE    ZERO            TO  IDZ
      *
           PERFORM UNTIL   IDZ     =   CNT-DENP
               ADD     1                   TO  IDZ
               ADD     1                   TO  CNT-LINE
      *
               IF    ( IDZ         =   1 )
                   MOVE    WRK-OUT-BTUNAME TO  HCM08-BTUNAME (CNT-LINE)
                   MOVE    WRK-OUT-BRMNUM  TO  HCM08-BRMNUM  (CNT-LINE)
                   MOVE    WRK-OUT-SRYKA   TO  HCM08-SRYKA(CNT-LINE)
                   IF    ( WRK-OUT-PTNUM  ( 11 : ) =   SPACE )
                       MOVE    WRK-OUT-PTNUM
                                           TO  HCM08-PTNUM (CNT-LINE)
                   ELSE
                       MOVE    WRK-OUT-PTNUM
                                           TO  HCM08-PTNUM2(CNT-LINE)
                   END-IF
                   MOVE    WRK-OUT-NAME    TO  HCM08-NAME(CNT-LINE)
                   MOVE    WRK-OUT-SEX     TO  HCM08-SEX(CNT-LINE)
                   MOVE    WRK-OUT-BIRTHDAY
                                           TO  HCM08-BIRTHDAY(CNT-LINE)
               END-IF
      *
               MOVE    WRK-NAI-DENPNUM-X   TO  HCM08-DENPNUM(CNT-LINE)
               MOVE    WRK-SEIKYU(IDZ)     TO  WRK-Z9
               MOVE    WRK-Z9              TO  HCM08-SEIKYU(CNT-LINE)
               MOVE    WRK-RYOSYU(IDZ)     TO  WRK-Z9
               MOVE    WRK-Z9              TO  HCM08-RYOSYU(CNT-LINE)
               MOVE    WRK-MISYU(IDZ)      TO  WRK-Z9
               MOVE    WRK-Z9              TO  HCM08-MISYU(CNT-LINE)
               MOVE    WRK-JYOTAI(IDZ)     TO  HCM08-JYOTAI(CNT-LINE)
      *
               COMPUTE WRK-GSEIKYU (1)
                   =   WRK-GSEIKYU (1)     +   WRK-SEIKYU(IDZ)
               COMPUTE WRK-GSEIKYU (3)
                   =   WRK-GSEIKYU (3)     +   WRK-SEIKYU(IDZ)
               COMPUTE WRK-GRYOSYU (1)
                   =   WRK-GRYOSYU (1)     +   WRK-RYOSYU(IDZ)
               COMPUTE WRK-GRYOSYU (3)
                   =   WRK-GRYOSYU (3)     +   WRK-RYOSYU(IDZ)
           END-PERFORM
      *
           COMPUTE WRK-GMISYU (1)
               =   WRK-GMISYU (1)      +   WRK-GMISYU(4)
           COMPUTE WRK-GMISYU (3)
               =   WRK-GMISYU (3)      +   WRK-GMISYU(4)
      *
      *    MOVE    ZERO            TO  CNT-DENP
      *    INITIALIZE                  WRK-NAI-AREA
      *
           IF    ( FLG-SMEI        NOT =   ZERO )
               MOVE    1           TO  FLG-END
           END-IF
      *
           .
       3203-BODY-NAIYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<合計>処理
      *****************************************************************
       330-TERM-HEN-SEC            SECTION.
      *
           MOVE    1               TO  IDX
           PERFORM 331-TOTAL-HENSYU-SEC
      *
           IF      FLG-END             =   1
               MOVE    3               TO  IDX
               PERFORM 331-TOTAL-HENSYU-SEC
           END-IF
           .
       330-TERM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       331-TOTAL-HENSYU-SEC        SECTION.
      *
           EVALUATE    IDX
               WHEN    1
                   MOVE    "頁内合計"      TO  HCM08-TTLMSG(1)
               WHEN    3
                   MOVE    "総合計"        TO  HCM08-TTLMSG(3)
           END-EVALUATE
      *
           MOVE    WRK-GSEIKYU(IDX)    TO  WRK-Z9
           MOVE    WRK-Z9              TO  HCM08-SEIKYUTTL(IDX)
           MOVE    WRK-GRYOSYU(IDX)    TO  WRK-Z9
           MOVE    WRK-Z9              TO  HCM08-RYOSYUTTL(IDX)
           MOVE    WRK-GMISYU(IDX)     TO  WRK-Z9
           MOVE    WRK-Z9              TO  HCM08-MISYUTTL(IDX)
      *
      *    IF    ( IDX    =    3 )
      *        PERFORM 3311-TOTAL-MES-HEN-SEC
      *    END-IF
      *
           .
       331-TOTAL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    領収金額計編集処理
      *****************************************************************
       3311-TOTAL-MES-HEN-SEC      SECTION.
      *
           MOVE    WRK-GRYOSYU(IDX)    TO  WRK-Z9
           MOVE    SPACE               TO  WRK-STR
           MOVE    1                   TO  IDX1
           UNSTRING    WRK-Z9
                   DELIMITED   BY  ALL SPACE
                   INTO    WRK-STR
                   WITH    POINTER IDX1
           END-UNSTRING
           MOVE    WRK-Z9 (IDX1 : )    TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
      *
      *    ステップ途中処理
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           STRING  "領収金額合計　"    DELIMITED   BY  SIZE
                   WRK-OUT-INPUT       DELIMITED   BY  SPACE
                   "円"                DELIMITED   BY  SIZE
           INTO    JOB-YOBI
           END-STRING
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
      *
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
grpsys                                     SPA-AREA
      *
           .
       3311-TOTAL-MES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC           SECTION.
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
           MOVE   "HCM08V02.red"   TO  SPRT-PRTID
           MOVE    "収納一覧表（入院）（期間指定）"
                                   TO  SPRT-TITLE
           MOVE    HCM08           TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SPRT-PRTNM
           MOVE    "1"             TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
grpsys                                 SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       390-PRINT-OUT-EXT.
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
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-HYYMMDD
           MOVE    LNK-DAY2-EDTYMD3
                                   TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG     REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
057400*
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
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
           MOVE    1               TO  FLG-END
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
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
grpsys                                 SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-HEN-SEC         SECTION.
      *
           MOVE    SPACE           TO  WRK-OUT-INPUT
           MOVE    ZERO            TO  WRK-OUT-MAX
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE 300                TO  KANACONV-LEN
           MOVE 1000               TO  KANACONV-STLEN
           MOVE 1                  TO  KANACONV-CONV-FLAG
           MOVE 15                 TO  KANACONV-CHAR-TYPE
           MOVE WRK-MAE-INPUT      TO  KANACONV-INDATA
           CALL "kanaconv"             USING ORCXKANACONVAREA
           IF    ( KANACONV-RETURN =   ZERO )
               MOVE    KANACONV-OUTDATA
                                       TO  WRK-OUT-INPUT
               MOVE    KANACONV-MAXLEN TO  WRK-OUT-MAX
           END-IF
      *
           .
       800-ZENKAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    収納明細検索処理
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
      *
           INITIALIZE                          SMEI-REC
grpsys*DD  MOVE    WRK-PARA-HOSPID         TO  SMEI-HOSPID
grpsys     MOVE    SPA-HOSPNUM             TO  SMEI-HOSPNUM
           MOVE    "1"                     TO  SMEI-NYUGAIKBN
           MOVE    WRK-STDAILYKEY          TO  SMEI-STDAILYKEY
           MOVE    WRK-EDDAILYKEY          TO  SMEI-EDDAILYKEY
           MOVE    SMEI-REC                TO  MCPDATA-REC
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           MOVE    "DBSELECT"              TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
           IF      MCP-RC              =   ZERO
               PERFORM 900-DBFETCH-SEC
           ELSE
               MOVE    1               TO  FLG-SMEI
           END-IF
      *
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細読込処理
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SMEI
               MOVE    MCPDATA-REC     TO  SMEI-REC
           ELSE
               MOVE    1               TO  FLG-SMEI
               INITIALIZE                  SMEI-REC
           END-IF
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細クローズ処理
      *****************************************************************
       900-DBCLOSE-SEC             SECTION.
      *
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細処理ファイル読込処理
      *****************************************************************
       900-BD00501-READ-SEC            SECTION.
      *
           READ    BD00501-FILE NEXT
           AT END
               MOVE    1               TO FLG-SMEI
           NOT AT  END
               IF    ( WRK-PARA-SORTKBN   =    "1" )
                   MOVE    BD00501-SYU-BTUNUM  TO  KEY-BTUNUM
                   MOVE    BD00501-SYU-BRMNUM  TO  KEY-BRMNUM
                   MOVE    BD00501-SYU-SRYKA   TO  KEY-SRYKA
               END-IF
               MOVE    BD00501-SMEI-DATA-AREA
                                       TO SMEI-REC
           END-READ
           .
      *
       900-BD00501-READ-EXT.
           EXIT.
      *****************************************************************
      *    収納明細マスター読込処理
      *****************************************************************
       900-SMEI2-SELECT-SEC        SECTION.
      *
           INITIALIZE                  SMEI2-REC
grpsys*DD  MOVE    WRK-PARA-HOSPID TO  SMEI2-HOSPID
grpsys     MOVE    SPA-HOSPNUM     TO  SMEI2-HOSPNUM
           MOVE    SMEI-PTID       TO  SMEI2-PTID
           MOVE    SMEI-DENPNUM    TO  SMEI2-DENPNUM
           MOVE    SMEI-DENPEDANUM TO  SMEI2-DENPEDANUM
           MOVE    SMEI-NYUGAIKBN  TO  SMEI2-NYUGAIKBN
           MOVE    SMEI2-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key11"         TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
           IF      MCP-RC              =   ZERO
               PERFORM 900-SMEI2-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-SMEI2
           END-IF
           .
       900-SMEI2-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納明細を検索処理
      *****************************************************************
       900-SMEI2-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key11"         TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SMEI2
               MOVE    MCPDATA-REC     TO  SMEI2-REC
           ELSE
               MOVE    1               TO  FLG-SMEI2
           END-IF
           .
       900-SMEI2-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-SMEI2-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key11"         TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
           .
       900-SMEI2-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    収納マスター読込処理
      *****************************************************************
       900-SYU-READ-SEC            SECTION.
      *
           MOVE    SYU-REC         TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYU-REC
               MOVE    ZERO            TO  FLG-SYU
           ELSE
               MOVE    1               TO  FLG-SYU
               INITIALIZE                  SYU-REC
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
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
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-KEY10-SEL-SEC       SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       800-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"   USING   MCPAREA
grpsys*DD                              MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"   USING   MCPAREA
grpsys*DD                              MCPDATA-REC
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"   USING   MCPAREA
grpsys*DD                              MCPDATA-REC
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"   USING   MCPAREA
grpsys*DD                              MCPDATA-REC
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC.
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　切断処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC.
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
