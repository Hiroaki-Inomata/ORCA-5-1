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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCL0031N.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＣＬＡＩＭ
      *  コンポーネント名  : 入院ＣＬＡＩＭサブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/11/09    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/06/05  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-KARTE-KEY           PIC 9(01).
           03  FLG-END                 PIC 9(01).
           03  FLG-ORDER               PIC 9(01).
           03  FLG-ODRITEM             PIC 9(01).
           03  FLG-ENTRYCHK            PIC 9(01).
           03  FLG-CHRERR              PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-ORCSNUM             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ORDER               PIC 9(05).
           03  CNT-IDOKBN              PIC 9(05).
           03  CNT-ERR                 PIC 9(05).
           03  CNT-TBLID               PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDX3                    PIC 9(05).
           03  IDX4                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXB                    PIC 9(05).
           03  IDX-IJI                 PIC 9(05).
           03  IDX-CMB                 PIC 9(05).
           03  IDX-ODR                 PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-IDOKBN              PIC X(03).
           03  WRK-ERRCD               PIC X(04).
           03  WRK-KARTE-KEY           PIC X(36).
           03  WRK-ORDERNUM            PIC 9(05).
           03  WRK-ITEMNUM             PIC 9(05).
           03  WRK-ITEMJMA             PIC X(05).
           03  WRK-KANACHK-INPUT       PIC X(500).
           03  WRK-PGNAME              PIC X(20).
           03  WRK-TBLID               PIC X(20).
           03  WRK-ITEM-G.
               05  WRK-ITEM            PIC X(10).
               05  WRK-MEMO            PIC X(80).
           03  WRK-STATUS-ITEM         PIC 9(05).
           03  WRK-UKEYMD              PIC X(08).
           03  WRK-UKEHMS              PIC X(08).
           03  WRK-ORDER-CLAIMID-G.
               05  WRK-ORDER-CLAIMID1  PIC X(02).
               05  WRK-ORDER-CLAIMID2  PIC X(03).
           03  WRK-CMB-CD              PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM         PIC X(200).
           03  WRK-ORDERID             PIC X(04).
           03  WRK-YMD                 PIC X(10).
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-KEISANYMD           PIC X(08).
           03  WRK-SAGAKU              PIC 9(08).
           03  WRK-SNUM-INX            PIC X(26).
           03  WRK-SNUM-OUTNUM         PIC 9(12).
      *
       01  ERRCD-AREA.
           03  ERRCD-KHNERR            PIC X(04).
           03  ERRCD-ERRCD             PIC X(04)
                                       OCCURS  2.
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-H280401           PIC X(08)   VALUE "20160401".
           03  CONST-SRYTBLP-MAX       PIC 9(05)   VALUE   20.
           03  CONST-SRYTBLC-MAX       PIC 9(05)   VALUE   20.
      *
      *    項目IDコピー句
           COPY    "CPNCLAIMID.INC".
      *
      *    ORDERIDコピー句
           COPY    "CPNORDERID.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1010.INC".
           COPY    "CPSK5013.INC".
           COPY    "CPSK5113.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    オーダーテーブル
       01  ORDER-REC.
           COPY    "CPORDER.INC".
      *
      *    オーダ項目
       01  ODRITEM-REC.
           COPY    "CPODRITEM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    カナチェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    入院異動チェックサブ
           COPY    "CPORCSODR.INC".
      *
       01  LNK-ODRITEM-REC.
           02  LNK-ODRITEM-MAX         PIC 9(03).
           02  LNK-ODRITEM-OCC         OCCURS   30.
           COPY    "CPODRITEM.INC"     REPLACING  //ODRITEM-//
                                       BY         //LNK-ODRITEM-//.
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET                   PIC X(2).
           03  C-UID                   PIC X(36).
       01  ST                          PIC 9(2).
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                         SECTION.
      *
           COPY    "CPORCL0031N.INC".
      *
       01  LF04-REC. 
           COPY    "CPRLF004.INC".
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING
                                               SCL31N-LNK
                                               LF04-REC.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END     =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SPA-AREA
      *
           MOVE    SCL31N-HOSPNUM  TO  SPA-HOSPNUM
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
           MOVE    SMCNDATE-YMD        TO  WRK-UKEYMD
           MOVE    SMCNDATE-TIME       TO  WRK-UKEHMS
      *
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
      *
           MOVE    MCP-USER        TO  SPA-OPID
      *
           MOVE    ZERO            TO  FLG-END
           MOVE    ZERO            TO  WRK-ORDERNUM
      *
           PERFORM 1001-KARTE-KEY-CHK-SEC
           IF    ( FLG-KARTE-KEY   =   9 )
               GO  TO  100-INIT-EXT
           END-IF
      *
           PERFORM 1001-KIHON-CHK-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    カルテキーチェック処理（重複チェック）
      *****************************************************************
       1001-KARTE-KEY-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-KARTE-KEY
           MOVE    LF04-UID8           TO  WRK-KARTE-KEY
      *
           IF    ( WRK-KARTE-KEY       =   SPACE )
               PERFORM 800-KARTE-KEY-GET-SEC
               INITIALIZE                  ORDER-REC
               MOVE    SCL31N-HOSPNUM   TO  ORDER-HOSPNUM
               MOVE    C-UID           TO  ORDER-KARTE-KEY
               PERFORM 900-ORDER-KEY2-SEL-SEC
               IF    ( FLG-ORDER       =   ZERO )
                   MOVE    9           TO  FLG-KARTE-KEY
                   DISPLAY "ORCL0031N ERROR 001:KARTE-KEY 未設定"
                           " PTID="        SCL31N-PTID
                           " KARTE-KEY="   WRK-KARTE-KEY
                   MOVE    1           TO  FLG-END
               ELSE
                   MOVE    C-UID       TO  WRK-KARTE-KEY
                   MOVE    1           TO  FLG-KARTE-KEY
               END-IF
           ELSE
               INITIALIZE                  ORDER-REC
               MOVE    SCL31N-HOSPNUM       TO  ORDER-HOSPNUM
               MOVE    LF04-UID8           TO  ORDER-KARTE-KEY
               PERFORM 900-ORDER-KEY2-SEL-SEC
               IF    ( FLG-ORDER       =   ZERO )
                   PERFORM 800-KARTE-KEY-GET-SEC
                   INITIALIZE                  ORDER-REC
                   MOVE    SCL31N-HOSPNUM   TO  ORDER-HOSPNUM
                   MOVE    C-UID           TO  ORDER-KARTE-KEY
                   PERFORM 900-ORDER-KEY2-SEL-SEC
                   IF    ( FLG-ORDER       =   ZERO )
                       MOVE    9           TO  FLG-KARTE-KEY
                       DISPLAY "ORCL0031N ERROR 002:KARTE-KEY ２重登録"
                               " PTID="        SCL31N-PTID
                               " KARTE-KEY="   WRK-KARTE-KEY
                       MOVE    1           TO  FLG-END
                   ELSE
                       MOVE    C-UID       TO  WRK-KARTE-KEY
                       MOVE    1           TO  FLG-KARTE-KEY
                   END-IF
               END-IF
           END-IF
      *
           .
       1001-KARTE-KEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本項目チェック
      *****************************************************************
       1001-KIHON-CHK-SEC              SECTION.
      *
           INITIALIZE                  ERRCD-AREA
           MOVE    ZERO            TO  CNT-ERR
      *
           IF    ( SCL31N-HOSPNUM   =   ZERO )
               MOVE    "0001"      TO  ERRCD-KHNERR
               PERFORM 1001-KIHON-ERRSET-SEC
           END-IF
      *
      *    IF    ( SCL31N-SRYKA    =   SPACE )
      *        MOVE    "0002"      TO  ERRCD-KHNERR
      *        PERFORM 1001-KIHON-ERRSET-SEC
      *    END-IF
      *
           IF    ( SCL31N-PTID     =   ZERO )
               MOVE    "0003"      TO  ERRCD-KHNERR
               PERFORM 1001-KIHON-ERRSET-SEC
           END-IF
      *
           IF    ( SCL31N-PTNUM    =   SPACE )
            OR   ( SCL31N-PTNUM    =   ZERO )
               MOVE    "0004"      TO  ERRCD-KHNERR
               PERFORM 1001-KIHON-ERRSET-SEC
           END-IF
      *
      *    IF    ( SCL31N-HKNCOMBI =   ZERO )
      *        MOVE    "0005"      TO  ERRCD-KHNERR
      *        PERFORM 1001-KIHON-ERRSET-SEC
      *    END-IF
      *
           IF    ( SCL31N-NYUGAIKBN    NOT =   "1" )
               MOVE    "0006"      TO  ERRCD-KHNERR
               PERFORM 1001-KIHON-ERRSET-SEC
           END-IF
      *
           IF    ( SCL31N-SRYYMD   =   SPACE )
            OR   ( SCL31N-SRYYMD   =   ZERO )
               MOVE    "0007"      TO  ERRCD-KHNERR
               PERFORM 1001-KIHON-ERRSET-SEC
           ELSE
               MOVE    WRK-UKEYMD          TO  WRK-YMD
               MOVE    "2"                 TO  WRK-ZOGENPTN
               MOVE    -2                  TO  WRK-ZOGEN
               PERFORM 800-CALENDAR-SEC
               IF    ( SCL31N-SRYYMD (1:6) <   WRK-KEISANYMD(1:6))
                   MOVE    "0134"          TO  ERRCD-KHNERR
                   PERFORM 1001-KIHON-ERRSET-SEC
               END-IF
           END-IF
      *
           .
       1001-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    エラーコードセット
      *****************************************************************
       1001-KIHON-ERRSET-SEC              SECTION.
      *
           IF    ( CNT-ERR         <   2 )
               COMPUTE CNT-ERR =   CNT-ERR +   1
               MOVE    ERRCD-KHNERR    TO  ERRCD-ERRCD (CNT-ERR)
           END-IF
      *
           .
       1001-KIHON-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   CONST-SRYTBLP-MAX )
                    OR   ( FLG-END =   1 )
      *
               IF    ( LF04-SRYCDTBL (IDXA) 
                                   =   NCLAIMID-CLASS )
      *
                   COMPUTE WRK-ORDERNUM
                       =   WRK-ORDERNUM    +   1
      *
                   EVALUATE    LF04-SRYCD (IDXA)
                   WHEN    NORDERID-ORDER-NYUIN
                   WHEN    NORDERID-ORDER-KYUSYOKU
                   WHEN    NORDERID-ORDER-DELNYUIN
                   WHEN    NORDERID-ORDER-ALLDELNYUIN
                           PERFORM 2001-NYUINIDO-SEC
                   WHEN    OTHER
                       MOVE    "0008"  TO  WRK-ERRCD
                       MOVE    "000"   TO  WRK-IDOKBN
                       PERFORM 20011-ZAIERR-HEN-SEC
                   END-EVALUATE
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院異動処理
      *****************************************************************
       2001-NYUINIDO-SEC                   SECTION.
      *
           PERFORM 20011-NYUINIDOKBN-CHK-SEC
      *
           IF    ( ERRCD-ERRCD (1) =   SPACE )
               PERFORM 20011-NYUINIDO-SYORI-SEC
      *
           ELSE
               PERFORM 20011-ZAIERR-HEN-SEC
           END-IF
      *
           .
       2001-NYUINIDO-EXT.
           EXIT.
      *****************************************************************
      *    入院異動区分チェック処理
      *****************************************************************
       20011-NYUINIDOKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO            TO  CNT-ORDER
                                       CNT-IDOKBN
      *
           MOVE    SPACE           TO  WRK-IDOKBN
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB        >   CONST-SRYTBLC-MAX )
      *
               IF    ( LF04-TBLID (IDXA IDXB)
                                   =   NCLAIMID-ORDER )
      *
                       COMPUTE CNT-ORDER   =   CNT-ORDER   +   1
      *
                   EVALUATE    LF04-SRYCD (IDXA) 
                   WHEN    NORDERID-ORDER-NYUIN
                       IF    (  LF04-IJICD (IDXA IDXB)
                                               =   NORDERID-NYUINCL2
                                               OR  NORDERID-TAIINCL2
                                               OR  NORDERID-GAIHKCL2
                                               OR  NORDERID-GBACKCL2
                                               OR  NORDERID-IDODRCL2
                                               OR  NORDERID-IDOKACL2
                                               OR  NORDERID-IDOBTUCL2
                                               OR  NORDERID-IDOBRMCL2
                                                                     )
                           COMPUTE CNT-IDOKBN  =   CNT-IDOKBN  +   1
                           MOVE    LF04-IJICD (IDXA IDXB)
                                       TO  WRK-IDOKBN
                       END-IF
                   WHEN    NORDERID-ORDER-KYUSYOKU
                       IF    (  LF04-IJICD (IDXA IDXB)
                                               =   NORDERID-FNORLCL2
                                               OR  NORDERID-FSPCLCL2
                                               OR  NORDERID-FSELECL2
                                               OR  NORDERID-FSTOPCL2
                                               OR  NORDERID-FSPSLCL2
                                               OR  NORDERID-FLIQUIDCL2
                                                                     )
                           COMPUTE CNT-IDOKBN  =   CNT-IDOKBN  +   1
                           MOVE    LF04-IJICD (IDXA IDXB)
                                       TO  WRK-IDOKBN
                       END-IF
                   WHEN    NORDERID-ORDER-DELNYUIN
                       IF    (  LF04-IJICD (IDXA IDXB)
                                           =   NORDERID-DELNYUINCL2
                                                                     )
                           COMPUTE CNT-IDOKBN  =   CNT-IDOKBN  +   1
                           MOVE    LF04-IJICD (IDXA IDXB)
                                       TO  WRK-IDOKBN
                       END-IF
                   WHEN    NORDERID-ORDER-ALLDELNYUIN
                       IF    (  LF04-IJICD (IDXA IDXB)
                                           =   NORDERID-ALLDELNYUINCL2
                                                                     )
                           COMPUTE CNT-IDOKBN  =   CNT-IDOKBN  +   1
                           MOVE    LF04-IJICD (IDXA IDXB)
                                       TO  WRK-IDOKBN
                       END-IF
                   END-EVALUATE
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( CNT-ORDER           <=  ZERO )
               MOVE    "0009"          TO  ERRCD-KHNERR
               PERFORM 1001-KIHON-ERRSET-SEC
           END-IF
      *
           IF    ( ERRCD-KHNERR         =  SPACE )
               IF    ( CNT-IDOKBN          <=  ZERO )
                   MOVE    "0008"          TO  ERRCD-KHNERR
                   PERFORM 1001-KIHON-ERRSET-SEC
               END-IF
           END-IF
      *
           IF    ( ERRCD-KHNERR         =  SPACE )
               IF    ( CNT-ORDER           >   1 )
                   MOVE    "0011"          TO  ERRCD-KHNERR
                   PERFORM 1001-KIHON-ERRSET-SEC
               END-IF
           END-IF
      *
           IF    ( ERRCD-KHNERR         =  SPACE )
               IF    ( CNT-IDOKBN          >   1 )
                   MOVE    "0012"          TO  ERRCD-KHNERR
                   PERFORM 1001-KIHON-ERRSET-SEC
               END-IF
           END-IF
      *
           IF    ( ERRCD-KHNERR         =  SPACE )
               IF    ( WRK-IDOKBN          =   NORDERID-FSELECL2
                                           OR  NORDERID-FSPSLCL2
                                                             )
                   MOVE    "0135"          TO  ERRCD-KHNERR
                   PERFORM 1001-KIHON-ERRSET-SEC
               END-IF
           END-IF
      *
           IF    ( ERRCD-KHNERR         =  SPACE )
               IF    ( SCL31N-SRYYMD      >=   CONST-H280401 )
                   CONTINUE
               ELSE
                   IF    ( WRK-IDOKBN      =   NORDERID-FLIQUIDCL2 )
                       MOVE    "0193"      TO  ERRCD-KHNERR
                       PERFORM 1001-KIHON-ERRSET-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       20011-NYUINIDOKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤エラー編集処理
      *****************************************************************
       20011-ZAIERR-HEN-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-ITEMNUM
      *
           IF    ( FLG-END     =   ZERO )
               PERFORM 20011-ORDER-INSERT-SEC
           END-IF
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   CONST-SRYTBLC-MAX )
                    OR   ( FLG-END =   1 )
      *
               PERFORM VARYING IDX-IJI FROM    1   BY  1
                       UNTIL ( IDX-IJI >   NCLAIMID-ITEM-MAX )
                        OR   ( FLG-END =   1 )
                   IF    ( LF04-TBLID (IDXA IDXB)
                                   =   NCLAIMID-ITEM (IDX-IJI) )
      *
                       MOVE    NCLAIMID-ITEMJMA (IDX-IJI)
                                       TO  WRK-ITEMJMA
      *
                       IF    ( FLG-END     =   ZERO )
                           PERFORM 20011-ODRITEM-INSERT-SEC
                       END-IF
      *
                   END-IF
               END-PERFORM
      *
           END-PERFORM
      *
           .
       20011-ZAIERR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院異動処理
      *****************************************************************
       20011-NYUINIDO-SYORI-SEC        SECTION.
      *
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
      *    引渡しオーダー情報設定処理
           PERFORM 20011-LNK-ODRITEM-SET-SEC
      *
           MOVE   SCL31N-HOSPNUM        TO  SODR-HOSPNUM
           MOVE   WRK-KARTE-KEY        TO  SODR-KARTE-KEY
           MOVE   WRK-ORDERNUM         TO  SODR-ORDERNUM
           MOVE   WRK-ORDERID          TO  SODR-ORDERID
           MOVE   SCL31N-NYUGAIKBN     TO  SODR-NYUGAIKBN
           MOVE   SCL31N-PTNUM         TO  SODR-PTNUM
           MOVE   SCL31N-PTID          TO  SODR-PTID
           MOVE   SCL31N-SRYKA         TO  SODR-SRYKA
           MOVE   SCL31N-SRYYMD        TO  SODR-SRYYMD
           MOVE   SCL31N-HKNCOMBI      TO  SODR-HKNCOMBI
           MOVE   SCL31N-DRCD          TO  SODR-DRCD
           MOVE   WRK-UKEYMD           TO  SODR-UKEYMD
           MOVE   WRK-UKEHMS           TO  SODR-UKEHMS
      *
           MOVE    "0"                 TO  SODR-PROCKBN
      *
           MOVE   "CLAIM"              TO  SODR-OPID
           MOVE   WRK-UKEYMD           TO  SODR-UPYMD
           MOVE   WRK-UKEHMS           TO  SODR-UPHMS
      *
           IF   ( FLG-KARTE-KEY        =   ZERO )
               CONTINUE
           ELSE
               MOVE    1               TO  SODR-KEYGENERATE
           END-IF
      *
           MOVE        SPACE           TO  WRK-PGNAME
      *
           INITIALIZE  WRK-ORDER-CLAIMID-G
           MOVE    LF04-SRYCD (IDXA)   TO  WRK-ORDER-CLAIMID1
           MOVE    WRK-IDOKBN          TO  WRK-ORDER-CLAIMID2
           MOVE    WRK-ORDER-CLAIMID-G TO  WRK-CMB-CD
           PERFORM 2300-ORDERID-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SODR-ORDERID
      *
           EVALUATE    SODR-ORDERID
           WHEN   NORDERID-NYUINID
      *
               PERFORM 20011-ORCSODR01-ITM-CHK-SEC
      *
               MOVE    "ORCSODR01"     TO  WRK-PGNAME
      *
           WHEN   NORDERID-TAIINID
      *
               PERFORM 20011-ORCSODR02-ITM-CHK-SEC
      *
               MOVE    "ORCSODR02"     TO  WRK-PGNAME
      *
           WHEN   NORDERID-GAIHKID
           WHEN   NORDERID-GBACKID
      *
               PERFORM 20011-ORCSODR03-ITM-CHK-SEC
      *
               MOVE    "ORCSODR03"     TO  WRK-PGNAME
      *
           WHEN   NORDERID-FNORLID
           WHEN   NORDERID-FSPCLID
           WHEN   NORDERID-FSELEID
           WHEN   NORDERID-FSTOPID
           WHEN   NORDERID-FSPSLID
           WHEN   NORDERID-FLIQUIDID
      *
               PERFORM 20011-ORCSODR04-ITM-CHK-SEC
      *
               MOVE    "ORCSODR04"     TO  WRK-PGNAME
      *
           WHEN   NORDERID-IDODRID
      *
               PERFORM 20011-ORCSODR05-ITM-CHK-SEC
      *
               MOVE    "ORCSODR05"     TO  WRK-PGNAME
      *
           WHEN   NORDERID-IDOKAID
           WHEN   NORDERID-IDOBTUID
           WHEN   NORDERID-IDOBRMID
      *
               PERFORM 20011-ORCSODR06-ITM-CHK-SEC
      *
               MOVE    "ORCSODR06"     TO  WRK-PGNAME
      *
           WHEN   NORDERID-DELNYUINID
           WHEN   NORDERID-ALLDELNYUINID
      *
               PERFORM 20011-ORCSODR07-ITM-CHK-SEC
      *
               MOVE    "ORCSODR07"     TO  WRK-PGNAME
      *
           END-EVALUATE
      *
           IF    ( WRK-PGNAME      NOT =   SPACE )
               CALL    WRK-PGNAME      USING   SODR-AREA
                                               LNK-ODRITEM-REC
                                               MCPAREA
                                               SPA-AREA
               IF    ( SODR-RC     NOT =   ZERO )
                   MOVE    1           TO  FLG-END
               END-IF
           END-IF
      *
           .
       20011-NYUINIDO-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    引渡しオーダー情報設定処理
      *****************************************************************
       20011-LNK-ODRITEM-SET-SEC         SECTION.
      *
           MOVE    ZERO                TO  LNK-ODRITEM-MAX
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   CONST-SRYTBLC-MAX )
      *
               MOVE    ALL "9"     TO  WRK-ITEMJMA
      *
               MOVE    SPACE       TO  WRK-ITEM-G
               MOVE    ZERO        TO  WRK-STATUS-ITEM
      *
               IF    ( LF04-TBLID (IDXA IDXB)  NOT = SPACE )
                AND  ( LF04-TBLID (IDXA IDXB)  NOT = NCLAIMID-ORDER )
      *
                   PERFORM VARYING IDX-IJI FROM    1   BY  1
                           UNTIL ( IDX-IJI     >   NCLAIMID-ITEM-MAX )
                            OR   ( WRK-ITEMJMA  NOT =   ALL "9" )
      *
                       IF    ( LF04-TBLID (IDXA IDXB)
                                   =   NCLAIMID-ITEM (IDX-IJI) )
      *
                           MOVE    NCLAIMID-ITEMJMA (IDX-IJI)
                                       TO  WRK-ITEMJMA
                       END-IF
      *
                   END-PERFORM
      *
                   MOVE    LF04-IJICD (IDXA IDXB)
                                       TO  WRK-ITEM
      *
                   EVALUATE    WRK-ITEMJMA
                   WHEN    NCLAIMID-TAIINCDJMA
                       MOVE    LF04-ALSNAME (IDXA IDXB)
                                       TO  WRK-MEMO
                   WHEN    NCLAIMID-SHOKAIJMA
                       MOVE    LF04-SRYMEMO (IDXA IDXB)
                                       TO  WRK-MEMO
                   END-EVALUATE
      *
                   MOVE    ZERO        TO  WRK-STATUS-ITEM
                   PERFORM 20011-LNK-ODRITEM-HEN-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           .
       20011-LNK-ODRITEM-SET-EXT.
           EXIT.
      *****************************************************************
      *    引渡しオーダー情報設定処理
      *****************************************************************
       20011-LNK-ODRITEM-HEN-SEC       SECTION.
      *
           IF    ( LNK-ODRITEM-MAX      >=  CONST-SRYTBLC-MAX )
               GO  TO  20011-LNK-ODRITEM-HEN-EXT
           END-IF
      *
           COMPUTE LNK-ODRITEM-MAX =   LNK-ODRITEM-MAX   +   1
      *
           MOVE    LNK-ODRITEM-MAX     TO  IDX-ODR
      *
           MOVE    SCL31N-HOSPNUM     TO  LNK-ODRITEM-HOSPNUM  (IDX-ODR)
           MOVE    WRK-KARTE-KEY      TO  LNK-ODRITEM-KARTE-KEY(IDX-ODR)
           MOVE    WRK-ORDERNUM        TO  LNK-ODRITEM-ORDERNUM(IDX-ODR)
           MOVE    IDX-ODR             TO  LNK-ODRITEM-ITEMNUM (IDX-ODR)
      *
           MOVE    WRK-ITEMJMA         TO  LNK-ODRITEM-ITEMID  (IDX-ODR)
      *
           MOVE    WRK-ITEM            TO  WRK-KANACHK-INPUT
           PERFORM 800-HANKAKU-CHK-SEC
           MOVE    WRK-KANACHK-INPUT   TO  WRK-ITEM
           MOVE    WRK-ITEM            TO  LNK-ODRITEM-ITEM    (IDX-ODR)
      *
           EVALUATE    WRK-ITEMJMA
           WHEN    NCLAIMID-DRJMA
               IF    ( LNK-ODRITEM-ITEM(IDX-ODR)   NOT =   SPACE )
                AND  ( LNK-ODRITEM-ITEM(IDX-ODR) (5:)  =   SPACE )
                   MOVE    "1"         TO  WRK-ITEM
                   MOVE    LNK-ODRITEM-ITEM(IDX-ODR)
                                       TO  WRK-ITEM (2:)
                   MOVE    WRK-ITEM    TO  LNK-ODRITEM-ITEM    (IDX-ODR)
               END-IF
           WHEN    NCLAIMID-SAGAKUJMA
               PERFORM 200111-SAGAKU-SET-SEC
               MOVE    WRK-ITEM        TO  LNK-ODRITEM-ITEM    (IDX-ODR)
               MOVE    WRK-MEMO        TO  LNK-ODRITEM-MEMO    (IDX-ODR)
           WHEN    NCLAIMID-TAIINCDJMA
               PERFORM 200111-TAIINCD-SET-SEC
               MOVE    WRK-ITEM        TO  LNK-ODRITEM-ITEM    (IDX-ODR)
               MOVE    WRK-MEMO        TO  LNK-ODRITEM-MEMO    (IDX-ODR)
           WHEN    NCLAIMID-SHOKAIJMA
               MOVE    WRK-MEMO TO  WRK-KANACHK-INPUT
               PERFORM 800-HANKAKU-CHK-SEC
               IF    ( FLG-ENTRYCHK        =   ZERO )
                   MOVE    WRK-MEMO
                                   TO  LNK-ODRITEM-MEMO        (IDX-ODR)
               ELSE
                   MOVE    SPACE   TO  LNK-ODRITEM-MEMO        (IDX-ODR)
               END-IF
           WHEN    NCLAIMID-NISSUJMA
               MOVE    SPACE       TO  LNK-ODRITEM-ITEM    (IDX-ODR)
               MOVE    WRK-MEMO    TO  WRK-KANACHK-INPUT
               PERFORM 800-HANKAKU-CHK-SEC
               IF    ( FLG-ENTRYCHK        =   ZERO )
                   MOVE    WRK-MEMO
                                   TO  LNK-ODRITEM-MEMO        (IDX-ODR)
               ELSE
                   MOVE    SPACE   TO  LNK-ODRITEM-MEMO        (IDX-ODR)
               END-IF
           END-EVALUATE
      *
           MOVE    WRK-STATUS-ITEM     TO  LNK-ODRITEM-STATUS  (IDX-ODR)
      *
           .
       20011-LNK-ODRITEM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    室料差額項目設定処理
      *****************************************************************
       200111-SAGAKU-SET-SEC           SECTION.
      *
           MOVE    WRK-ITEM        TO  WRK-MEMO
           MOVE    SPACE           TO  WRK-ITEM
      *
           MOVE    WRK-MEMO        TO  WRK-SNUM-INX
           PERFORM 800-ORCSNUM-SEC
           IF    ( FLG-ORCSNUM     =   ZERO )
               MOVE    WRK-SNUM-OUTNUM TO  WRK-SAGAKU
               INITIALIZE                  SYSKANRI-REC
               MOVE    "5113"          TO  SYS-KANRICD
               MOVE    SCL31N-SRYYMD   TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
      *
               PERFORM 900-SYSKANRI-KEY2-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5113-REC
      *
               PERFORM UNTIL ( FLG-SYSKANRI        =   1 )
                        OR   ( WRK-ITEM        NOT =   SPACE )
      *
                   IF    ( WRK-SAGAKU  =   SYS-5113-BRM-SAGAKU-NM )
                       MOVE    SYS-5113-KBNCD
                                       TO  WRK-ITEM
                   END-IF
      *
                   PERFORM 900-SYSKANRI-KEY2-FET-SEC
                   MOVE    SYSKANRI-REC TO SYS-5113-REC
      *
               END-PERFORM
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
      *
           .
       200111-SAGAKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    退院事由項目設定処理
      *****************************************************************
       200111-TAIINCD-SET-SEC          SECTION.
      *
           MOVE    WRK-MEMO        TO  WRK-KANACHK-INPUT
           PERFORM 800-ZENKAKU-CHK-SEC
           MOVE    WRK-KANACHK-INPUT
                                   TO  WRK-MEMO
      *
           MOVE    SPACE           TO  WRK-ITEM
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5013"          TO  SYS-KANRICD
           MOVE    SCL31N-SRYYMD   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
      *
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5013-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI        =   1 )
                    OR   ( WRK-ITEM        NOT =   SPACE )
      *
               IF    ( WRK-MEMO    =   SYS-5013-NAME )
                   MOVE    SYS-5013-KBNCD
                                   TO  WRK-ITEM
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC TO SYS-5013-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200111-TAIINCD-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院処理項目チェック（必須項目抜け）
      *****************************************************************
       20011-ORCSODR01-ITM-CHK-SEC     SECTION.
      *
      *    入院区分チェック
           PERFORM 20011-NYUKBN-CHK-SEC
      *
      *    病棟コードチェック
           PERFORM 20011-BTUNUM-CHK-SEC
      *
      *    病室コードチェック
           PERFORM 20011-BRMNUM-CHK-SEC
      *
      *    入院科チェック
           PERFORM 20011-NYUINKA-CHK-SEC
      *
      *    保険組合せチェック
           PERFORM 20011-HKNCOMBI-CHK-SEC
      *
           .
       20011-ORCSODR01-ITM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院処理項目チェック（必須項目抜け）
      *****************************************************************
       20011-ORCSODR02-ITM-CHK-SEC     SECTION.
      *
      *    入院科チェック
           PERFORM 20011-NYUINKA-CHK-SEC
      *
      *    保険組合せチェック
           PERFORM 20011-HKNCOMBI-CHK-SEC
      *
           .
       20011-ORCSODR02-ITM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    外泊処理項目チェック（必須項目抜け）
      *****************************************************************
       20011-ORCSODR03-ITM-CHK-SEC     SECTION.
      *
      *    入院科チェック
           PERFORM 20011-NYUINKA-CHK-SEC
      *
      *    保険組合せチェック
           PERFORM 20011-HKNCOMBI-CHK-SEC
      *
           .
       20011-ORCSODR03-ITM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事処理項目チェック（必須項目抜け）
      *****************************************************************
       20011-ORCSODR04-ITM-CHK-SEC     SECTION.
      *
      *    入院科チェック
           PERFORM 20011-NYUINKA-CHK-SEC
      *
      *    保険組合せチェック
           PERFORM 20011-HKNCOMBI-CHK-SEC
      *
      *    食事時間チェック
           PERFORM 20011-SKJTIME-CHK-SEC
      *
      *    日数設定処理
           PERFORM 20011-NISSU-SET-SEC
      *
           .
       20011-ORCSODR04-ITM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    担当医処理項目チェック（必須項目抜け）
      *****************************************************************
       20011-ORCSODR05-ITM-CHK-SEC     SECTION.
      *
      *    入院科チェック
           PERFORM 20011-NYUINKA-CHK-SEC
      *
      *    保険組合せチェック
           PERFORM 20011-HKNCOMBI-CHK-SEC
      *
           .
       20011-ORCSODR05-ITM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    転科・転棟・転室処理項目チェック（必須項目抜け）
      *****************************************************************
       20011-ORCSODR06-ITM-CHK-SEC     SECTION.
      *
      *    入院区分チェック
           PERFORM 20011-NYUKBN-CHK-SEC
      *
      *    病棟コードチェック
           PERFORM 20011-BTUNUM-CHK-SEC
      *
      *    病室コードチェック
           PERFORM 20011-BRMNUM-CHK-SEC
      *
      *    入院科チェック
           PERFORM 20011-NYUINKA-CHK-SEC
      *
      *    保険組合せチェック
           PERFORM 20011-HKNCOMBI-CHK-SEC
      *
           .
       20011-ORCSODR06-ITM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院取消処理項目チェック（必須項目抜け）
      *****************************************************************
       20011-ORCSODR07-ITM-CHK-SEC     SECTION.
      *
      *    入院科チェック
           PERFORM 20011-NYUINKA-CHK-SEC
      *
      *    保険組合せチェック
           PERFORM 20011-HKNCOMBI-CHK-SEC
      *
           .
       20011-ORCSODR07-ITM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院区分チェック
      *****************************************************************
       20011-NYUKBN-CHK-SEC            SECTION.
      *
      *    入院区分チェック
           MOVE    NCLAIMID-NYUKBN     TO  WRK-TBLID
           PERFORM 20011-ITEM-CNT-CHK-SEC
           IF        ( CNT-TBLID   =   ZERO )
               MOVE    NCLAIMID-NYUKBNJMA
                                       TO  WRK-ITEMJMA
               MOVE    SPACE           TO  WRK-ITEM-G
               MOVE    4               TO  WRK-STATUS-ITEM
               PERFORM 20011-LNK-ODRITEM-HEN-SEC
           END-IF
      *
           .
       20011-NYUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟コードチェック
      *****************************************************************
       20011-BTUNUM-CHK-SEC            SECTION.
      *
      *    病棟コードチェック
           MOVE    NCLAIMID-BTUNUM     TO  WRK-TBLID
           PERFORM 20011-ITEM-CNT-CHK-SEC
           IF        ( CNT-TBLID   =   ZERO )
               MOVE    NCLAIMID-BTUNUMJMA
                                       TO  WRK-ITEMJMA
               MOVE    SPACE           TO  WRK-ITEM-G
               MOVE    4               TO  WRK-STATUS-ITEM
               PERFORM 20011-LNK-ODRITEM-HEN-SEC
           END-IF
      *
           .
       20011-BTUNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病室コードチェック
      *****************************************************************
       20011-BRMNUM-CHK-SEC            SECTION.
      *
      *    病室コードチェック
           MOVE    NCLAIMID-BRMNUM     TO  WRK-TBLID
           PERFORM 20011-ITEM-CNT-CHK-SEC
           IF        ( CNT-TBLID   =   ZERO )
               MOVE    NCLAIMID-BRMNUMJMA
                                       TO  WRK-ITEMJMA
               MOVE    SPACE           TO  WRK-ITEM-G
               MOVE    4               TO  WRK-STATUS-ITEM
               PERFORM 20011-LNK-ODRITEM-HEN-SEC
           END-IF
      *
           .
       20011-BRMNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院科チェック処理
      *****************************************************************
       20011-NYUINKA-CHK-SEC           SECTION.
      *
      *    入院科チェック
           MOVE    NCLAIMID-NYUINKA    TO  WRK-TBLID
           PERFORM 20011-ITEM-CNT-CHK-SEC
           IF        ( CNT-TBLID   =   ZERO )
               MOVE    NCLAIMID-NYUINKAJMA
                                       TO  WRK-ITEMJMA
               MOVE    SPACE           TO  WRK-ITEM-G
               MOVE    SCL31N-SRYKA    TO  WRK-ITEM
               MOVE    4               TO  WRK-STATUS-ITEM
               PERFORM 20011-LNK-ODRITEM-HEN-SEC
           END-IF
      *
           .
       20011-NYUINKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       20011-HKNCOMBI-CHK-SEC          SECTION.
      *
      *    保険組合せチェック
           MOVE    NCLAIMID-HKNCOMBI   TO  WRK-TBLID
           PERFORM 20011-ITEM-CNT-CHK-SEC
           IF        ( CNT-TBLID   =   ZERO )
               MOVE    NCLAIMID-HKNCOMBIJMA
                                       TO  WRK-ITEMJMA
               MOVE    SPACE           TO  WRK-ITEM-G
               MOVE    SCL31N-HKNCOMBI TO  WRK-ITEM
               MOVE    4               TO  WRK-STATUS-ITEM
               PERFORM 20011-LNK-ODRITEM-HEN-SEC
           END-IF
      *
           .
       20011-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事時間チェック
      *****************************************************************
       20011-SKJTIME-CHK-SEC           SECTION.
      *
      *    食事時間チェック
           MOVE    NCLAIMID-SKJTIME    TO  WRK-TBLID
           PERFORM 20011-ITEM-CNT-CHK-SEC
           IF        ( CNT-TBLID   =   ZERO )
               MOVE    NCLAIMID-SKJTIMEJMA
                                       TO  WRK-ITEMJMA
               MOVE    SPACE           TO  WRK-ITEM-G
               MOVE    4               TO  WRK-STATUS-ITEM
               PERFORM 20011-LNK-ODRITEM-HEN-SEC
           END-IF
      *
           .
       20011-SKJTIME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日数項目設定処理
      *****************************************************************
       20011-NISSU-SET-SEC             SECTION.
      *
           MOVE    NCLAIMID-NISSUJMA
                                   TO  WRK-ITEMJMA
           MOVE    SPACE           TO  WRK-ITEM-G
           MOVE    LF04-KAISU (IDXA)
                                   TO  WRK-MEMO
           MOVE    ZERO            TO  WRK-STATUS-ITEM
           PERFORM 20011-LNK-ODRITEM-HEN-SEC
      *
           .
       20011-NISSU-SET-EXT.
           EXIT.
      *****************************************************************
      *    項目出現回数チェック処理
      *****************************************************************
       20011-ITEM-CNT-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-TBLID
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   CONST-SRYTBLC-MAX )
      *
               IF    ( LF04-TBLID (IDXA IDXB)
                                   =   WRK-TBLID )
                   COMPUTE CNT-TBLID
                       =   CNT-TBLID   +   1
               END-IF
      *
           END-PERFORM
      *
           .
       20011-ITEM-CNT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    オーダー追加処理
      *****************************************************************
       20011-ORDER-INSERT-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-CHRERR
      *
           INITIALIZE                      ORDER-REC
           MOVE    SCL31N-HOSPNUM       TO  ORDER-HOSPNUM
           MOVE    WRK-KARTE-KEY       TO  ORDER-KARTE-KEY
           MOVE    WRK-ORDERNUM        TO  ORDER-ORDERNUM
           MOVE    SCL31N-NYUGAIKBN    TO  ORDER-NYUGAIKBN
           MOVE    SCL31N-PTNUM        TO  ORDER-PTNUM
           MOVE    SCL31N-PTID         TO  ORDER-PTID
           MOVE    SCL31N-SRYKA        TO  ORDER-SRYKA
           MOVE    SCL31N-SRYYMD       TO  ORDER-SRYYMD
           MOVE    SCL31N-HKNCOMBI     TO  ORDER-HKNCOMBI
           MOVE    WRK-UKEYMD          TO  ORDER-UKEYMD
           MOVE    WRK-UKEHMS          TO  ORDER-UKEHMS
      *
           INITIALIZE                      WRK-ORDERID
      *
           MOVE    LF04-SRYCD (IDXA)   TO  WRK-KANACHK-INPUT
           PERFORM 800-HANKAKU-CHK-SEC
           MOVE    WRK-KANACHK-INPUT   TO  WRK-ORDERID
           IF    ( FLG-ENTRYCHK        =   ZERO )
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-CHRERR
           END-IF
      *
           INITIALIZE                      WRK-ORDER-CLAIMID-G
           MOVE    LF04-SRYCD (IDXA)   TO  WRK-ORDER-CLAIMID1
           MOVE    WRK-IDOKBN          TO  WRK-ORDER-CLAIMID2
           MOVE    WRK-ORDER-CLAIMID-G TO  WRK-CMB-CD
           PERFORM 2300-ORDERID-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    LF04-SRYCD (IDXA)
                                       TO  ORDER-ORDERID
           ELSE
               MOVE    WRK-CMB-ITM     TO  ORDER-ORDERID
           END-IF
      *
           MOVE    ERRCD-ERRCD (1)     TO  ORDER-ERRCD1
      *    MOVE    ERRCD-ERRCD (2)     TO  ORDER-ERRCD2
      *
           MOVE    1                   TO  ORDER-STATUS
      *
           IF    ( FLG-KARTE-KEY       =   ZERO )
               CONTINUE
           ELSE
               MOVE    1               TO  ORDER-KEYGENERATE
           END-IF
           MOVE    SPACE               TO  ORDER-TERMID
           MOVE    "CLAIM"             TO  ORDER-OPID
           MOVE    WRK-UKEYMD          TO  ORDER-CREYMD
                                           ORDER-UPYMD
           MOVE    WRK-UKEHMS          TO  ORDER-UPHMS
      *
           PERFORM 900-ORDER-KEY-SEL-SEC
           IF    ( FLG-ORDER           =   ZERO )
               DISPLAY "ORCL0031N ERROR 003:ORDER INSERT CHK ERR"
                       " PTID="        SCL31N-PTID
                       " KARTE-KEY="   WRK-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  20011-ORDER-INSERT-EXT
           END-IF
      *
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "tbl_order"         TO MCP-TABLE
           MOVE    "key"               TO MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               DISPLAY "ORCL0031N ERROR 004:ORDER INSERT ERR"
                       " PTID="        SCL31N-PTID
                       " KARTE-KEY="   WRK-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       20011-ORDER-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    オーダー項目追加処理
      *****************************************************************
       20011-ODRITEM-INSERT-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-CHRERR
      *
           COMPUTE WRK-ITEMNUM =   WRK-ITEMNUM +   1
      *
           INITIALIZE                      ODRITEM-REC
           MOVE    SCL31N-HOSPNUM       TO  ODRITEM-HOSPNUM
           MOVE    WRK-KARTE-KEY       TO  ODRITEM-KARTE-KEY
           MOVE    WRK-ORDERNUM        TO  ODRITEM-ORDERNUM
           MOVE    WRK-ITEMNUM         TO  ODRITEM-ITEMNUM
           MOVE    WRK-ITEMJMA         TO  ODRITEM-ITEMID
      *
           MOVE    LF04-IJICD (IDXA IDXB)
                                       TO  WRK-KANACHK-INPUT
           PERFORM 800-HANKAKU-CHK-SEC
           MOVE    WRK-KANACHK-INPUT   TO  ODRITEM-ITEM
           IF    ( FLG-ENTRYCHK        =   ZERO )
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-CHRERR
           END-IF
      *    IF    ( FLG-CHRERR          =   ZERO )
      *        項目チェック無し
               MOVE    SPACE           TO  ODRITEM-ERRCD
      *    ELSE
      *        文字化け
      *        MOVE    "900?"          TO  ODRITEM-ERRCD
      *    END-IF
      *
           MOVE    ZERO                TO  ODRITEM-STATUS
           MOVE    SPACE               TO  ODRITEM-TERMID
           MOVE    "CLAIM"             TO  ODRITEM-OPID
           MOVE    WRK-UKEYMD          TO  ODRITEM-CREYMD
                                           ODRITEM-UPYMD
           MOVE    WRK-UKEHMS          TO  ODRITEM-UPHMS
      *
           PERFORM 900-ODRITEM-KEY-SEL-SEC
           IF    ( FLG-ODRITEM         =   ZERO )
               DISPLAY "ORCL0031N ERROR 003:ODRITEM INSERT CHK ERR"
                       " PTID="        SCL31N-PTID
                       " KARTE-KEY="   WRK-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  20011-ORDER-INSERT-EXT
           END-IF
      *
           MOVE    ODRITEM-REC           TO  MCPDATA-REC
           MOVE    "tbl_odritem"         TO MCP-TABLE
           MOVE    "key"               TO MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               DISPLAY "ORCL0031N ERROR 004:ODRITEM INSERT ERR"
                       " PTID="        SCL31N-PTID
                       " KARTE-KEY="   WRK-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       20011-ODRITEM-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    オーダーＩＤチェック処理
      *****************************************************************
       2300-ORDERID-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   NORDERID-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( NORDERID-CLAIMID (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    NORDERID-ORCAID  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       2300-ORDERID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           .
           EXIT.
      *****************************************************************
      *    カルテキーチェック処理
      *****************************************************************
       800-KARTE-KEY-GET-SEC           SECTION.
      *
           INITIALIZE  UIDIO-AREA.
           CALL    "orcuidset" USING   ST
                                       UIDIO-AREA.
      *
           .
       800-KARTE-KEY-GET-EXT.
           EXIT.
      *****************************************************************
      *    半角文字チェック
      *****************************************************************
       800-HANKAKU-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-INPUT NOT =   SPACE )
               MOVE    SPACE           TO  ORCSKANACHKAREA
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-KANACHK-INPUT
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-KANACHK-INPUT
               IF      ( KANACHK-RC    =   ZERO )
                   IF      ( KANACHK-SYUBETU   NOT =   1 )
                       DISPLAY "HANKAKU CHK ERR 004: " KANACHK-MAE-INPUT
                       MOVE    1       TO  FLG-ENTRYCHK
                   END-IF
               ELSE
                       DISPLAY "HANKAKU CHK ERR 005: " KANACHK-MAE-INPUT
                       MOVE    1       TO  FLG-ENTRYCHK
               END-IF
           END-IF
      *
           IF    ( FLG-ENTRYCHK    NOT =   ZERO )
               MOVE    SPACE           TO  WRK-KANACHK-INPUT
           END-IF
      *
           .
       800-HANKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全角文字チェック
      *****************************************************************
       800-ZENKAKU-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-INPUT     NOT =   SPACE )
               MOVE    SPACE           TO  ORCSKANACHKAREA
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI
               MOVE    WRK-KANACHK-INPUT
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-KANACHK-INPUT
               IF      ( KANACHK-RC    =   ZERO )
                   IF      ( KANACHK-SYUBETU   NOT =   2 )
                       DISPLAY "ZENKAKU CHK ERR 006: " KANACHK-MAE-INPUT
                       MOVE    1       TO  FLG-ENTRYCHK
                   END-IF
               ELSE
                       DISPLAY "ZENKAKU CHK ERR 007: " KANACHK-MAE-INPUT
                       MOVE    1       TO  FLG-ENTRYCHK
               END-IF
           END-IF
      *
           IF    ( FLG-ENTRYCHK    NOT =   ZERO )
               MOVE    SPACE           TO  WRK-KANACHK-INPUT
           END-IF
      *
           .
       800-ZENKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
      *    増減パターン 1:日／2:月／3:年
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    数値変換処理
      *****************************************************************
       800-ORCSNUM-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-ORCSNUM
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-SNUM-INX        TO  SNUM-INX
           CALL    "ORCSNUM"           USING   ORCSNUMAREA
           IF    ( SNUM-RC         NOT =   ZERO  )
            OR   ( SNUM-MINKBN     NOT =   SPACE )
            OR   ( SNUM-SYOKBN     NOT =   SPACE )
               MOVE    1               TO  FLG-ORCSNUM
           ELSE
               MOVE    SNUM-OUTNUM     TO  WRK-SNUM-OUTNUM
           END-IF
      *
           .
      *
       800-ORCSNUM-EXT.
           EXIT.
      *****************************************************************
      *    オーダー検索処理(KEY)
      *****************************************************************
       900-ORDER-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ORDER
      *
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "tbl_order"         TO MCP-TABLE
           MOVE    "key"               TO MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-ORDER
           END-IF
      *
           MOVE    "tbl_order"         TO MCP-TABLE
           MOVE    "key"               TO MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-ORDER-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    オーダ項目検索処理(KEY)
      *****************************************************************
       900-ODRITEM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ODRITEM
      *
           MOVE    ODRITEM-REC         TO  MCPDATA-REC
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-ODRITEM
           END-IF
      *
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key"               TO MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-ODRITEM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    オーダー検索処理(KEY2)
      *****************************************************************
       900-ORDER-KEY2-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-ORDER
      *
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "tbl_order"         TO MCP-TABLE
           MOVE    "key2"              TO MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-ORDER
           END-IF
      *
           MOVE    "tbl_order"         TO MCP-TABLE
           MOVE    "key2"              TO MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-ORDER-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SCL31N-HOSPNUM      TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢインサート処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
