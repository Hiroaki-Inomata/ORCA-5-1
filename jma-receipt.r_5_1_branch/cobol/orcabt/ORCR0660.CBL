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
       PROGRAM-ID.             ORCR0660.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（入院）
      *  コンポーネント名  : レセプトコメント編集
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/12/13    NaCl-門脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.01    NACL-藤原    18/06/11  ファイルのエラー処理追加
      *  05.00.02    NACL-門脇    19/04/03  自動記載コメント改元対応
      *  05.00.03    NACL-門脇    19/05/16  退院時共同指導料２コメント自動
      *                                     記載対応
      *  05.00.04    NACL-門脇    19/06/24  リハビリ発症日レセプト記載対応
      *                                     （機能追加）
      *  05.00.05    NACL-門脇    19/08/01  リハビリ発症日レセプト記載対応
      *                                     （機能追加）修正
      *  05.00.06    NACL-門脇    20/03/30  令和２年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
           SELECT  RECE62-FILE     ASSIGN  RECE62PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE62-KEY
                                   FILE    STATUS  IS  STS-RECE62.
      *
           SELECT  RECE64-FILE     ASSIGN  RECE64PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE64-KEY
                                   FILE    STATUS  IS  STS-RECE64.
      *
           SELECT  RECE65-FILE     ASSIGN  RECE65PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE65-KEY
                                   FILE    STATUS  IS  STS-RECE65.
      *
           SELECT  RECE641-FILE    ASSIGN  RECE641PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE641-KEY
                                   FILE    STATUS  IS  STS-RECE641.
      *
           SELECT  RECE651-FILE    ASSIGN  RECE651PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE651-KEY
                                   FILE    STATUS  IS  STS-RECE651.
      *ワーク
           SELECT  RECE649-FILE    ASSIGN  RECE649PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE649-KEY
                                   FILE    STATUS  IS  STS-RECE649.
      *
           SELECT  RECE659-FILE    ASSIGN  RECE659PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE659-KEY
                                   FILE    STATUS  IS  STS-RECE659.
      *
           SELECT  RECE655-FILE    ASSIGN  RECE655PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE655-KEY
                                   FILE    STATUS  IS  STS-RECE655.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    レセプト０２データ
       FD  RECE62-FILE.
       01  RECE62-REC.
           COPY    "CPRCF062.INC".
      *
      *    レセプト０４データ
       FD  RECE64-FILE.
       01  RECE64-REC.
           COPY    "CPRCF064.INC".
      *
      *    レセプト０５データ
       FD  RECE65-FILE.
       01  RECE65-REC.
           COPY    "CPRCF065.INC".
      *
       FD  RECE641-FILE.
       01  RECE641-REC. 
           COPY    "CPRCF064.INC"  REPLACING   //RECE64//
                                   BY          //RECE641//.
      *
       FD  RECE651-FILE.
       01  RECE651-REC. 
           COPY    "CPRCF065.INC"  REPLACING   //RECE65//
                                   BY          //RECE651//.
      *ワーク
       FD  RECE649-FILE.
       01  RECE649-REC. 
           COPY    "CPRCF064.INC"  REPLACING   //RECE64//
                                   BY          //RECE649//.
      *
       FD  RECE659-FILE.
       01  RECE659-REC. 
           COPY    "CPRCF065.INC"  REPLACING   //RECE65//
                                   BY          //RECE659//.
      *
      *
       FD  RECE655-FILE.
       01  RECE655-REC. 
           COPY    "CPRCF0655.INC".
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
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE641PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE651PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE649PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE659PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE655PARA//.
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
           03  STS-RECE62          PIC X(02).
           03  STS-RECE64          PIC X(02).
           03  STS-RECE641         PIC X(02).
           03  STS-RECE65          PIC X(02).
           03  STS-RECE651         PIC X(02).
           03  STS-RECE649         PIC X(02).
           03  STS-RECE659         PIC X(02).
           03  STS-RECE655         PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE62          PIC 9(01).
           03  FLG-RECE64          PIC 9(01).
           03  FLG-RECE641         PIC 9(01).
           03  FLG-RECE649         PIC 9(01).
           03  FLG-RECE65          PIC 9(01).
           03  FLG-RECE659         PIC 9(01).
           03  FLG-RECE651         PIC 9(01).
           03  FLG-YAKUKAN-COMMENT PIC 9(01).
           03  FLG-HINKAI-COMMENT  PIC 9(01).
           03  FLG-KYUSEI-COMMENT  PIC 9(01).
           03  FLG-KYUSEI-COMMENT2 PIC 9(01).
           03  FLG-COMMENT         PIC 9(02).
           03  FLG-WRKRECE65       PIC 9(01).
           03  FLG-FIRST           PIC 9(01).
           03  FLG-NYU-RENZOKU     PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-KYUSEI-OK       PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-RIHA            PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-RYOCOM          PIC 9(01).
           03  FLG-RYOCOM2         PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-COMOKIKAE       PIC 9(01).
           03  FLG-COMOKIKAE2      PIC 9(01).
           03  FLG-HENSYUKIN       PIC 9(01).
           03  FLG-HANTEI          PIC 9(01).
           03  FLG-COMARI          PIC 9(01).
           03  FLG-COMARI2         PIC 9(01).
           03  FLG-SANTEIPLUS      PIC 9(01).
           03  FLG-SETCOM          PIC 9(01).
           03  FLG-MEISKY-OKIKAE   PIC 9(01).
           03  FLG-KAISIEN         PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-NG              PIC 9(01).
           03  FLG-SANTEIPLUS-HANTEI
                                   PIC 9(01).
           03  FLG-KAISYOKU-COMMENT
                                   PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-DAISAN1         PIC 9(01).
           03  FLG-DAISAN2         PIC 9(01).
           03  FLG-SINPESUR-COMMENT
                                   PIC 9(01).
           03  FLG-ZAIKYO-COMMENT  PIC 9(01).
           03  FLG-BOKOHO-COMMENT  PIC 9(01).
           03  FLG-CHECK-MAE       PIC 9(01).
           03  FLG-CHECK-ATO       PIC 9(01).
           03  FLG-RETRY           PIC 9(01).
           03  FLG-ZAIKYO-OK       PIC 9(01).
           03  FLG-ZAIKYOCOMARI    PIC 9(01).
           03  FLG-CYOJKSN-OK      PIC 9(01).
           03  FLG-CYOJKSNCOMARI   PIC 9(01).
           03  FLG-CYOJKSN         PIC 9(01).
           03  FLG-GET             PIC 9(01).
           03  FLG-RKISAI          PIC 9(01).
           03  FLG-ETC             PIC 9(01).
           03  FLG-SENTAKU-OKIKAE  PIC 9(01).
           03  FLG-CHECK-OK        PIC 9(01).
           03  FLG-CHECK-OK2       PIC 9(01).
           03  FLG-JINNAI-COMMENT  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE62          PIC 9(06).
           03  CNT-RECE64          PIC 9(06).
           03  CNT-RECE65          PIC 9(06).
           03  CNT-RECE649         PIC 9(06).
           03  CNT-RECE659         PIC 9(06).
           03  CNT-RECE655         PIC 9(06).
           03  CNT-RECE64-RE       PIC 9(06).
           03  CNT-RECE65-RE       PIC 9(06).
           03  CNT-RIHA            PIC 9(06).
           03  CNT-IN-COMMIT       PIC 9(06).
           03  CNT-IN-CANCEL       PIC 9(06).
           03  CNT-KENSA           PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(02).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDX6                PIC 9(04).
           03  IDX12               PIC 9(04).
           03  IDX13               PIC 9(04).
           03  IDX14               PIC 9(04).
           03  IDX15               PIC 9(01).
           03  IDX16               PIC 9(02).
           03  IDX17               PIC 9(02).
           03  IDX18               PIC 9(02).
           03  IDX19               PIC 9(02).
           03  IDX20               PIC 9(04).
           03  IDX21               PIC 9(01).
           03  IDX22               PIC 9(04).
           03  IDX26               PIC 9(02).
           03  IDX27               PIC 9(02).
           03  IDX28               PIC 9(02).
           03  IDX29               PIC 9(02).
           03  IDX30               PIC 9(02).
           03  IDX31               PIC 9(02).
           03  IDX32               PIC 9(02).
           03  IDX33               PIC 9(02).
           03  IDX34               PIC 9(02).
           03  IDX35               PIC 9(02).
           03  IDX41               PIC 9(02).
           03  IDX42               PIC 9(02).
           03  IDX43               PIC 9(02).
           03  IDX44               PIC 9(02).
           03  IDX45               PIC 9(03).
           03  IDX46               PIC 9(03).
           03  IDX47               PIC 9(03).
           03  IDX48               PIC 9(02).
           03  IDX49               PIC 9(02).
           03  DAY-IDX             PIC 9(04).
           03  DAY-IDX2            PIC 9(02).
           03  IDY                 PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDY11               PIC 9(03).
           03  IDY12               PIC 9(03).
           03  IDY13               PIC 9(03).
           03  IDZ                 PIC 9(04).
           03  IDX-RECE65          PIC 9(04).
           03  IDV                 PIC 9(04).
           03  IDW                 PIC 9(04).
           03  RYO-IDX             PIC 9(03).
           03  RYO-IDX2            PIC 9(03).
           03  RYO-IDX3            PIC 9(03).
           03  IDA                 PIC 9(04).
           03  IDB                 PIC 9(04).
           03  IDC1                PIC 9(02).
           03  IDC2                PIC 9(02).
           03  IDC3                PIC 9(02).
           03  IDC4                PIC 9(02).
           03  RIHA-IDX            PIC 9(02).
           03  RIHA-IDX2           PIC 9(02).
           03  RIHA-ST             PIC 9(02).
           03  RIHA-ST2            PIC 9(02).
           03  RIHA-ED             PIC 9(02).
           03  RIHA-ED2            PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM              PIC  X(06).
               05  WRK-PARA-TERMID             PIC  X(16).
               05  WRK-PARA-SYSYMD             PIC  X(08).
               05  WRK-PARA-JOBID              PIC  9(07).
               05  WRK-PARA-SHELLID            PIC  X(08).
           03  WRK-PARA-HOSPNUM                PIC  9(02).
      *
           03  WRK-JYUNBAN         PIC 9(04).
           03  WRK-GYOSUU          PIC 9(04).
           03  WRK-GYOSUU2         PIC 9(04).
           03  WRK-RENNUM          PIC 9(02).
           03  WRK-CNT-MEISAI      PIC 9(04).
           03  WRK-KETA            PIC 9(04).
           03  WRK-KETA-TAIHI      PIC 9(04).
           03  WRK-KETA2           PIC 9(04).
           03  WRK-KOHBUNRI        PIC 9(01).
           03  WRK-KOHBUNRI-JIDO   PIC 9(01).
           03  WRK-HKNCOMBI-JIDO   PIC 9(04).
      *
           03  WRK-FSANTEIYMD      PIC 9(08).
           03  WRK-AFTER2          PIC 9(08).
           03  WRK-AFTER180        PIC 9(08).
           03  WRK-TOSEKIYMD       PIC 9(08).
           03  WRK-KYUSEIYMD       PIC 9(08).
           03  WRK-HINKAIYMD       PIC 9(08).
           03  WRK-DAY             PIC 9(02).
      *
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYCD           PIC X(09).
           03  WRK-SRYCD2          PIC X(09).
           03  WRK-ZAINUM          PIC 9(08).
      *
           03  WRK-HENSYU-KAISU    PIC X(02).
           03  WRK-HENSYU-KAISU2   PIC X(04).
      *
           03  TBL-SANTEI-DAY.
               05  TB-SANTEI-DAY   PIC 9(02)   OCCURS  31.
      *
      *    該当診療月分
           03  TBL-RIHASANTEI-DAY.
               05  TB-RIHASANTEI-DAY PIC 9(01) OCCURS  31.
      *
           03  TBL-RIHASANTEI-DAY-END.
               05  TB-RIHASANTEI-DAY-END
                                     PIC 9(01) OCCURS  31.
           03  TBL-RIHASANTEI-DAY-END2.
               05  TB-RIHASANTEI-DAY-END2
                                     PIC 9(01) OCCURS  31.
           03  TBL-RIHASANTEI-DAY-END3.
               05  TB-RIHASANTEI-DAY-END3
                                     PIC 9(01) OCCURS  31.
      *
      *    直近分
           03  WRK-RIHASANTEI-YMD    PIC X(08).
      *
      *    直近分退避用
           03  WRK-RIHASANTEI-YMD2   PIC X(08).
           03  WRK-RIHASANTEI-YMD3   PIC X(08).
      *
      *    前月以前の直近終了日
           03  WRK-RIHASANTEI-END    PIC X(08).
      *
      *    ２００６年４月１日に算定日があるか
           03  WRK-RIHASANTEI41-FLG  PIC 9(01).
      *
      *    リハビリ開始日指定チェック用
           03  WRK-CHECK-RIHASANTEI-AREA.
               05  WRK-CHECK-RIHASANTEI-YMD     PIC X(08).
               05  WRK-CHECK-RIHASANTEI-YMD3    PIC X(08).
               05  WRK-CHECK-RIHASANTEI-DAY-OCC.
                 07  WRK-CHECK-RIHASANTEI-DAY   PIC 9(01)  OCCURS  31.
      *
           03  TBL-NYU-DAY.
               05  TBL-NYU-DAY-OCC             OCCURS  31.
                 07  TBL-NYU-DAY-ST  PIC 9(02).
                 07  TBL-NYU-DAY-ED  PIC 9(02).
                 07  TBL-NYU-DAY-CD  PIC X(09).
      *
           03  WRK-ZENYYMM           PIC 9(06).
      *
           03  WRK-TUKI-YAKUKAN      PIC 9(02).
           03  WRK-DAY-YAKUKAN       PIC 9(02).
      *
           03  WRK-KEISAN            PIC 9(09).
           03  WRK-KEISAN2           PIC 9(03).
           03  WRK-KEISAN3           PIC 9(09).
           03  WRK-KEISAN4           PIC 9(09).
           03  WRK-KEISAN-TAIHI      PIC X(09).
      *
           03  WRK-KYUSEI-NYUGAIKBN  PIC X(01).
           03  WRK-KYUSEI-SRYKA      PIC X(02).
           03  WRK-KYUSEI-ZAINUM     PIC 9(08).
      *
           03  WRK-ZAIKYO-NYUGAIKBN  PIC X(01).
           03  WRK-ZAIKYO-SRYKA      PIC X(02).
           03  WRK-ZAIKYO-ZAINUM     PIC 9(08).
      *
           03  WRK-CYOJKSN-INPUTCOMENT
                                     PIC X(140).
           03  WRK-CYOJKSN-INPUTCOMENT2
                                     PIC X(140).
           03  WRK-CYOJKSN-INPUTCOMENT3
                                     PIC X(140).
      *
           03  WRK-HENYMD            PIC X(08).
           03  WRK-TAIHI-XX          PIC X(02).
      *
           03  WRK-HENSYU-KINGAKU    PIC X(09).
           03  WRK-HENSYU-KINGAKU2   PIC X(18).
           03  WRK-HENSYU-KINGAKU3   PIC X(18).
      *
           03  WRK-RIHAIGAKU-SRYCD           PIC X(09).
           03  WRK-RIHAIGAKU-SRYCD2          PIC X(09).
           03  WRK-RIHAIGAKU-FSANTEIYMD      PIC X(08).
           03  WRK-RIHAIGAKU-SANTEI-AREA.
               05  WRK-RIHAIGAKU-SANTEI-FLG  PIC X(01).
               05  WRK-RIHAIGAKU-SANTEI-DAY  PIC 9(03)
                                             OCCURS 31.
      *
           03  WRK-YAKUKAN-SRYCD     PIC X(09).
      *
           03  WRK-HEN-RENNUM                PIC 9(02).
           03  WRK-HEN-SHORI-RENNUM          PIC 9(01).
           03  WRK-HEN-SEQNO                 PIC 9(02).
      *
           03  WRK-HEN-RENNUM-X              PIC 9(02).
           03  WRK-HEN-SHORI-RENNUM-X        PIC 9(01).
           03  WRK-HEN-SEQNO-X               PIC 9(02).
      *
           03  WRK-HEN-RENNUM-J              PIC 9(02).
           03  WRK-HEN-SHORI-RENNUM-J        PIC 9(01).
           03  WRK-HEN-SEQNO-J               PIC 9(02).
      *
           03  WRK-HEN-RENNUM-J2             PIC 9(02).
           03  WRK-HEN-SHORI-RENNUM-J2       PIC 9(01).
           03  WRK-HEN-SEQNO-J2              PIC 9(02).
      *
           03  WRK-HEN-RENNUM-J3             PIC 9(02).
           03  WRK-HEN-SHORI-RENNUM-J3       PIC 9(01).
           03  WRK-HEN-SEQNO-J3              PIC 9(02).
      *
           03  WRK-KAISIEN-FSANTEIYMD        PIC X(08).
           03  WRK-KAISIEN-YMD               PIC X(08).
           03  WRK-KAISIEN-CHECKYMD          PIC X(08).
           03  WRK-KAISIEN-RRKNUM            PIC 9(03).
           03  WRK-KAISIEN-RRKNUM-X          PIC 9(03).
           03  WRK-KAISIEN-RRKNUM-Y          PIC 9(03).
      *
           03  WRK-BUNKAKUKETA               PIC 9(02).
           03  WRK-BUNKAKU-N                 PIC X(10).
           03  WRK-SRYKAISU-X                PIC ZZ9.
           03  WRK-CHAR-HENKAN-AREA.
               05  WRK-CHAR-X                PIC X(03).
               05  WRK-CHAR-N                PIC X(06).
               05  WRK-CHAR-N-CNT            PIC 9(01).
               05  WRK-CHAR-IDX              PIC 9(01).
      *
           03  WRK-KAISYOKU-YMD              PIC X(08).
      *
           03  WRK-RIHA-HKNCOMBI             PIC 9(04).
      *
           03  WRK-SINPESUR-YMD              PIC X(08).
           03  WRK-ZAIKYO-YMD                PIC X(08).
           03  WRK-ZAIKYO-YMD2               PIC X(08).
           03  WRK-BOKOHO-YMD                PIC X(08).
           03  WRK-CYOJKSN-YMD               PIC X(08).
           03  WRK-HAIJRT-YMD                PIC X(08).
      *
           03  WRK-GANSIDO-YMD-AREA.
               05  WRK-GANSIDO-YMD           PIC X(08)
                                             OCCURS 10.
      *
           03  WRK-BOKOHO-YMD9               PIC X(08).
           03  WRK-BOKOHO-YMD2-AREA.
               05  WRK-BOKOHO-YMD2           PIC X(08)
                                             OCCURS 30.
      *
           03  WRK-ZAIKYO-YMD9               PIC X(08).
           03  WRK-ZAIKYO-KAISU              PIC 9(02).
           03  WRK-HAIJRT-KAISU              PIC 9(02).
      *
           03  WRK-CYOJKSN-TAIHIYM           PIC X(06).
           03  WRK-CYOJKSN-TAIHIDD           PIC X(02).
      *
           03  WRK-CYOJKSN-YMD-XXX              PIC X(08).
           03  WRK-CYOJKSN-COM-XXX              PIC X(140).
           03  WRK-CYOJKSN-YMD-YYY              PIC X(08).
           03  WRK-CYOJKSN-COM-YYY              PIC X(140).
           03  WRK-CYOJKSN-KAISHIDAY-XXX-AREA.
             04  WRK-CYOJKSN-KAISHIDAY-XXX-OCC  OCCURS  2.
               05  WRK-CYOJKSN-KAISHIDAY-XXX    PIC X(08).
               05  WRK-CYOJKSN-INPUTCOMENT-XXX  PIC X(140).
           03  WRK-CYOJKSN-KAISHIDAY-YYY-AREA.
             04  WRK-CYOJKSN-KAISHIDAY-YYY-OCC  OCCURS  2.
               05  WRK-CYOJKSN-KAISHIDAY-YYY    PIC X(08).
               05  WRK-CYOJKSN-INPUTCOMENT-YYY  PIC X(140).
      *
           03  WRK-SRYACCT-PATHNAME             PIC X(64).
      *
           03  WRK-SEIKENTEN                    PIC ZZZZZZ9.
      *
           03  WRK-GENBAKU-TOTALKIN             PIC 9(07).
           03  WRK-GENBAKU-FTNKIN               PIC 9(07)V9(01).
           03  WRK-GENBAKU-FTNRATE.
               05  WRK-GENBAKU-FTNRATE1         PIC 9(02).
               05  WRK-GENBAKU-FTNRATE2         PIC 9(01).
      *
           03  WRK-ZAIHANKO-FSANTEIYMD          PIC X(08).
      *
           03  WRK-CHECK-SRYCD                  PIC X(09).
           03  WRK-TENSU-SRYCD                  PIC X(09).
           03  WRK-TENSU-SRYYM                  PIC X(06).
      *
           03  WRK-SJIYMD                       PIC X(08).
      *
           03  WRK-MCP-TABLE                    PIC X(64).
           03  WRK-MCP-PATHNAME                 PIC X(64).
      *
           03  WRK-JINNAI-YMD                   PIC X(08).
      *
           03  WRK-HAIJSK-FSANTEIYM             PIC X(08).
           03  WRK-HAIJSK-KAISU                 PIC 9(02).
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
       01  WRK-RECE64-REC. 
           COPY    "CPRCF064.INC"  REPLACING   //RECE64//
                                   BY          //WRK-RECE64//.
      *
       01  WRK-RECE65-REC. 
           COPY    "CPRCF065.INC"  REPLACING   //RECE65//
                                   BY          //WRK-RECE65//.
      *
       01  WRK-RECE65XX-REC.
         02 WRK-RECE65XX-TBL       OCCURS 100.
           COPY    "CPRCF065.INC"  REPLACING   //RECE65//
                                   BY          //WRK-RECE65XX//.
      *
       01  WRK-RECE65XX2-REC.
         02 WRK-RECE65XX2-TBL      OCCURS 300.
           COPY    "CPRCF065.INC"  REPLACING   //RECE65//
                                   BY          //WRK-RECE65XX2//.
      *
       01  WRK-RECE65ZZ-REC.
         02 WRK-RECE65ZZ-TBL       OCCURS 100.
           COPY    "CPRCF065.INC"  REPLACING   //RECE65//
                                   BY          //WRK-RECE65ZZ//.
      *
       01  WRK-FREE-AREA.
           03  WRK-FREEINF                     OCCURS 49.         
               05  WK-FREE-YUKOKETA          PIC  9(03).
               05  WK-FREE-NAME              PIC  X(300).
      *
       01  WRK-FREE-AREA2.                                              
           03  WRK-FREEINF2                    OCCURS 50.               
               05  WK-FREE-YUKOKETA2         PIC  9(03).                
               05  WK-FREE-NAME2             PIC  X(300).               
               05  WK-FREE-MEISKYFLG2        PIC  X(01).
               05  WK-FREE-SETCOMATFLG2      PIC  9(03).
      *
       01  WRK-FREE-AREA3.                                              
           03  WRK-FREEINF3.
               05  WK-FREE-NAME3             PIC  X(1000).              
               05  WK-FREE-NAME3-TAIHI       PIC  X(1000).              
      *
       01  WRK-JIDO-SETCOM-AREA.                                              
           03  WRK-JIDO-SETCOMATFLG          PIC  9(03).
           03  WRK-JIDO-SETCOMFLG            PIC  X(01).
           03  WRK-JIDO-JYOUGEFLG            PIC  9(01).
      *
      *    日付全角変換領域
       01  TBL-DAY-AREA.
           03  FILLER              PIC X(04)  VALUE "　１".
           03  FILLER              PIC X(04)  VALUE "　２".
           03  FILLER              PIC X(04)  VALUE "　３".
           03  FILLER              PIC X(04)  VALUE "　４".
           03  FILLER              PIC X(04)  VALUE "　５".
           03  FILLER              PIC X(04)  VALUE "　６".
           03  FILLER              PIC X(04)  VALUE "　７".
           03  FILLER              PIC X(04)  VALUE "　８".
           03  FILLER              PIC X(04)  VALUE "　９".
           03  FILLER              PIC X(04)  VALUE "１０".
           03  FILLER              PIC X(04)  VALUE "１１".
           03  FILLER              PIC X(04)  VALUE "１２".
           03  FILLER              PIC X(04)  VALUE "１３".
           03  FILLER              PIC X(04)  VALUE "１４".
           03  FILLER              PIC X(04)  VALUE "１５".
           03  FILLER              PIC X(04)  VALUE "１６".
           03  FILLER              PIC X(04)  VALUE "１７".
           03  FILLER              PIC X(04)  VALUE "１８".
           03  FILLER              PIC X(04)  VALUE "１９".
           03  FILLER              PIC X(04)  VALUE "２０".
           03  FILLER              PIC X(04)  VALUE "２１".
           03  FILLER              PIC X(04)  VALUE "２２".
           03  FILLER              PIC X(04)  VALUE "２３".
           03  FILLER              PIC X(04)  VALUE "２４".
           03  FILLER              PIC X(04)  VALUE "２５".
           03  FILLER              PIC X(04)  VALUE "２６".
           03  FILLER              PIC X(04)  VALUE "２７".
           03  FILLER              PIC X(04)  VALUE "２８".
           03  FILLER              PIC X(04)  VALUE "２９".
           03  FILLER              PIC X(04)  VALUE "３０".
           03  FILLER              PIC X(04)  VALUE "３１".
       01  TBL-DAY-AREA-R          REDEFINES   TBL-DAY-AREA.
           03  TBL-DAY             PIC X(04)   OCCURS   31.
      *
       01  MAX-MEISAISU            PIC 9(07).
      *
      *    固定値
       01  CONST-AREA.
      *    薬剤管理指導料
           03  CONS-YAKUKAN-COMMENT PIC X(09)  VALUE "120001610".
      *    算定日コメント
           03  CONS-SANTEI-COMMENT1  PIC X(09) VALUE "840000058".
           03  CONS-SANTEI-COMMENT2  PIC X(09) VALUE "840000065".
      *    在宅血液透析指導管理料
           03  CONS-TOSEKI          PIC X(09)  VALUE "114009310".
      *    脳血管疾患急性発症日
           03  CONS-KYUSEI          PIC X(09)  VALUE "099800101".
      *    脳血管疾患急性発症終了日
           03  CONS-KYUSEI-END      PIC X(09)  VALUE "099800102".
      *    心大リハ開始日
           03  CONS-SINDAI          PIC X(09) VALUE "099800111".
      *    呼吸器リハ開始日
           03  CONS-KOKYUKI         PIC X(09) VALUE "099800141".
      *    脳血管疾患等リハ開始日
           03  CONS-NOUKE           PIC X(09) VALUE "099800121".
      *    運動器リハ開始日
           03  CONS-UNDOKI          PIC X(09) VALUE "099800131".
      *    難病患者リハ開始日
           03  CONS-NANBYO          PIC X(09) VALUE "099800161".
      *    障害児（者）リハ開始日
           03  CONS-SYOGAI          PIC X(09) VALUE "099800171".
      *    摂食療法開始日
           03  CONS-SESSYOKU        PIC X(09) VALUE "099800151".
      *    廃用症候群リハ開始日
           03  CONS-HAIYO           PIC X(09) VALUE "099800191".
      *
      *    心大リハ終了日
           03  CONS-SINDAI-E        PIC X(09) VALUE "099800112".
      *    呼吸器リハ終了日
           03  CONS-KOKYUKI-E       PIC X(09) VALUE "099800142".
      *    脳血管疾患等リハ終了日
           03  CONS-NOUKE-E         PIC X(09) VALUE "099800122".
      *    運動器リハ終了日
           03  CONS-UNDOKI-E        PIC X(09) VALUE "099800132".
      *    難病患者リハ終了日
           03  CONS-NANBYO-E        PIC X(09) VALUE "099800162".
      *    障害児（者）リハ終了日
           03  CONS-SYOGAI-E        PIC X(09) VALUE "099800172".
      *    摂食療法終了日
           03  CONS-SESSYOKU-E      PIC X(09) VALUE "099800152".
      *    廃用症候群リハ開始日
           03  CONS-HAIYO-E         PIC X(09) VALUE "099800192".
      *
      *    心大リハ加算開始日
           03  CONS-SINDAI-K        PIC X(09) VALUE "099800211".
      *    呼吸器リハ加算開始日
           03  CONS-KOKYUKI-K       PIC X(09) VALUE "099800241".
      *    廃用症候群リハ加算開始日
           03  CONS-HAIYO-K         PIC X(09) VALUE "099800291".
      *
      *    心大血管疾患リハビリテーション医学管理料（１）
           03  CONS-SINDAI-IGAKU1   PIC X(09) VALUE "180029910".
      *    心大血管疾患リハビリテーション医学管理料（２）
           03  CONS-SINDAI-IGAKU2   PIC X(09) VALUE "180030010".
      *    呼吸器リハビリテーション医学管理料（１）
           03  CONS-KOKYUKI-IGAKU1  PIC X(09) VALUE "180030510".
      *    呼吸器リハビリテーション医学管理料（２）
           03  CONS-KOKYUKI-IGAKU2  PIC X(09) VALUE "180030610".
      *    脳血管疾患等リハビリテーション医学管理料（１）
           03  CONS-NOUKE-IGAKU1    PIC X(09) VALUE "180030110".
      *    脳血管疾患等リハビリテーション医学管理料（２）
           03  CONS-NOUKE-IGAKU2    PIC X(09) VALUE "180030210".
      *    運動器リハビリテーション医学管理料（１）
           03  CONS-UNDOKI-IGAKU1   PIC X(09) VALUE "180030310".
      *    運動器リハビリテーション医学管理料（２）
           03  CONS-UNDOKI-IGAKU2   PIC X(09) VALUE "180030410".
      *
      *    心大血管疾患リハ実施
           03  CONS-SINDAI-DUMMY    PIC X(09) VALUE "099800311".
      *    呼吸器リハ実施
           03  CONS-KOKYUKI-DUMMY   PIC X(09) VALUE "099800341".
      *    脳血管疾患リハ実施
           03  CONS-NOUKE-DUMMY     PIC X(09) VALUE "099800321".
      *    運動器リハ実施
           03  CONS-UNDOKI-DUMMY    PIC X(09) VALUE "099800331".
      *
      *    介護支援連携指導料
           03  CONS-KAISIEN         PIC X(09) VALUE "113011710".
      *
      *    介護職員等喀痰吸引等指示料
           03  CONS-KAISYOKU        PIC X(09) VALUE "114020910".
      *
      *    心臓ペースメーカー指導管理料（着用型自動除細動器）
           03  CONS-SINPESUR1       PIC X(09) VALUE "113015010".
      *    心臓ペースメーカー指導管理料（ロ・植込型除細動器移行期加算含む）
           03  CONS-SINPESUR2       PIC X(09) VALUE "113015130".
      *
      *    がん患者指導管理料（医師・看護師が心理的不安軽減のため面接）
           03  CONS-GANSIDO1        PIC X(09) VALUE "113015310".
      *    がん患者指導管理料（医師等が坑悪性腫瘍剤の必要性等を文書説明）
           03  CONS-GANSIDO2        PIC X(09) VALUE "113015410".
      *
      *    在宅患者共同診療料開始日
           03  CONS-ZAIKYO          PIC X(09) VALUE "099140004".
      *
      *    磁気による膀胱等刺激法開始日
           03  CONS-BOKOHO1         PIC X(09) VALUE "099400001".
      *    磁気による膀胱等刺激法
           03  CONS-BOKOHO2         PIC X(09) VALUE "140055110".
      *
      *    排尿自立指導料
           03  CONS-HAIJRT          PIC X(09) VALUE "113022810".
      *
      *    在宅酸素療法指導管理料（その他）
           03  CONS-ZAISAN          PIC X(09) VALUE "114003710".
      *    在宅持続陽圧呼吸療法指導管理料２
           03  CONS-YOUATU2         PIC X(09) VALUE "114040810".
      *
      *    在宅半固形栄養経管栄養法指導管理料
           03  CONS-ZAIHANKO        PIC X(09) VALUE "114045510".
      *
      *    高気圧酸素治療（減圧症又は空気塞栓）
           03  CONS-KIATU           PIC X(09) VALUE "140057510".
      *    高気圧酸素治療（その他のもの）
           03  CONS-KIATU2          PIC X(09) VALUE "140057610".
      *    高気圧酸素治療開始日
           03  CONS-KIATU3          PIC X(09) VALUE "099400002".
      *
      *    精神科ＳＣ疾患別等専門プログラム加算開始日
           03  CONS-SCPRO           PIC X(09) VALUE "099830104".
      *
      *    経頭蓋磁気刺激療法開始日
           03  CONS-JIKSGK          PIC X(09) VALUE "099830106".
      *    経頭蓋磁気刺激療法終了日
           03  CONS-JIKSGK-E        PIC X(09) VALUE "099830107".
      *
      *    心不全に対する遠赤外線温熱療法開始日
           03  CONS-ONNETU          PIC X(09) VALUE "099400003".
      *
      *    人工内耳機器調整加算（６歳未満）
           03  CONS-JINNAI1         PIC X(09) VALUE "113030670".
      *    人工内耳機器調整加算（６歳以上）
           03  CONS-JINNAI2         PIC X(09) VALUE "113030770".
      *
      *    在宅患者共同診療料テーブル
       01  TBL-ZAIKYO-CHECK-AREA.
           03  FILLER          PIC X(09)   VALUE    "114027610".
           03  FILLER          PIC X(09)   VALUE    "114027710".
           03  FILLER          PIC X(09)   VALUE    "114027810".
           03  FILLER          PIC X(09)   VALUE    "114027910".
      *
       01  TBL-ZAIKYO-CHECK-R  REDEFINES   TBL-ZAIKYO-CHECK-AREA.
           03  TBL-ZAIKYO-CHECK     PIC X(09)   OCCURS    4.
      *
       01  TBL-ZAIKYO-CHECK-MAX     PIC 9(02)   VALUE     4.
      *
      *    超重症児（者）入院診療加算テーブル
       01  TBL-CYOJKSN-CHECK-XXX-AREA.
           03  FILLER          PIC X(09)   VALUE    "190127510".
           03  FILLER          PIC X(09)   VALUE    "190076570".
      *
       01  TBL-CYOJKSN-CHECK-XXX-R
                               REDEFINES  TBL-CYOJKSN-CHECK-XXX-AREA.
           03  TBL-CYOJKSN-CHECK-XXX  PIC X(09)   OCCURS    2.
      *
       01  TBL-CYOJKSN-CHECK-XXX-MAX  PIC 9(02)   VALUE     2.
      *
      *    準超重症児（者）入院診療加算テーブル
       01  TBL-CYOJKSN-CHECK-YYY-AREA.
           03  FILLER          PIC X(09)   VALUE    "190127610".
           03  FILLER          PIC X(09)   VALUE    "190076670".
      *
       01  TBL-CYOJKSN-CHECK-YYY-R
                               REDEFINES  TBL-CYOJKSN-CHECK-YYY-AREA.
           03  TBL-CYOJKSN-CHECK-YYY  PIC X(09)   OCCURS    2.
      *
       01  TBL-CYOJKSN-CHECK-YYY-MAX  PIC 9(02)   VALUE     2.
      *
      *    薬剤管理指導料テーブル（２００８年４月診療分〜）
       01  TBL-YAKUKAN-AREA.
           03  FILLER               PIC X(09) VALUE "113010710".
           03  FILLER               PIC X(09) VALUE "113010810".
           03  FILLER               PIC X(09) VALUE "120001610".
      *
       01  TBL-YAKUKAN-R       REDEFINES   TBL-YAKUKAN-AREA.
            03  TBL-YAKUKAN    PIC X(09)   OCCURS   3.
      *
       01  TBL-YAKUKAN-MAX     PIC 9(02)   VALUE    3.
      *
      *
       01  TBL-HASSYO-INF.
           03  TBL-HASSYO-KAITATU-FLG           PIC 9(01).
           03  TBL-HASSYO-SINDAI-FLG            PIC 9(01).
           03  TBL-HASSYO-KOKYUKI-FLG           PIC 9(01).
           03  TBL-HASSYO-NOUKE-FLG             PIC 9(01).
           03  TBL-HASSYO-UNDOKI-FLG            PIC 9(01).
           03  TBL-HASSYO-NANBYO-FLG            PIC 9(01).
           03  TBL-HASSYO-SYOGAI-FLG            PIC 9(01).
           03  TBL-HASSYO-SESSYOKU-FLG          PIC 9(01).
           03  TBL-HASSYO-HAIYO-FLG             PIC 9(01).
           03  TBL-HASSYO-INF2                  OCCURS  5.
               05  TBL-HASSYO-KAITATU-COMMENT   PIC 9(01).
               05  TBL-HASSYO-SINDAI-COMMENT    PIC 9(01).
               05  TBL-HASSYO-KOKYUKI-COMMENT   PIC 9(01).
               05  TBL-HASSYO-NOUKE-COMMENT     PIC 9(01).
               05  TBL-HASSYO-UNDOKI-COMMENT    PIC 9(01).
               05  TBL-HASSYO-NANBYO-COMMENT    PIC 9(01).
               05  TBL-HASSYO-SYOGAI-COMMENT    PIC 9(01).
               05  TBL-HASSYO-SESSYOKU-COMMENT  PIC 9(01).
               05  TBL-HASSYO-HAIYO-COMMENT     PIC 9(01).
               05  TBL-HASSYO-HKNCOMBI          PIC 9(04).
           03  TBL-HASSYO-INF3                  OCCURS 10.
               05  TBL-HASSYO-SINDAI2-COMMENT   PIC 9(01).
               05  TBL-HASSYO-KOKYUKI2-COMMENT  PIC 9(01).
               05  TBL-HASSYO-NOUKE2-COMMENT    PIC 9(01).
               05  TBL-HASSYO-UNDOKI2-COMMENT   PIC 9(01).
               05  TBL-HASSYO-NANBYO2-COMMENT   PIC 9(01).
               05  TBL-HASSYO-SYOGAI2-COMMENT   PIC 9(01).
               05  TBL-HASSYO-SESSYOKU2-COMMENT PIC 9(01).
               05  TBL-HASSYO-HAIYO2-COMMENT    PIC 9(01).
               05  TBL-HASSYO-HKNCOMBI2         PIC 9(04).
      *
           03  TBL-HASSYO-SINDAI-DAY-T.
             04  TBL-HASSYO-SINDAI-DAY-T2         OCCURS   5.
               05  TBL-HASSYO-SINDAI-DAY-OCC      OCCURS  31.
                   07  TBL-HASSYO-SINDAI-SANTEI   PIC 9(01).
           03  TBL-HASSYO-KOKYUKI-DAY-T.
             04  TBL-HASSYO-KOKYUKI-DAY-T2        OCCURS   5.
               05  TBL-HASSYO-KOKYUKI-DAY-OCC     OCCURS  31.
                   07  TBL-HASSYO-KOKYUKI-SANTEI  PIC 9(01).
           03  TBL-HASSYO-NOUKE-DAY-T.
             04  TBL-HASSYO-NOUKE-DAY-T2          OCCURS   5.
               05  TBL-HASSYO-NOUKE-DAY-OCC       OCCURS  31.
                   07  TBL-HASSYO-NOUKE-SANTEI    PIC 9(01).
           03  TBL-HASSYO-UNDOKI-DAY-T.
             04  TBL-HASSYO-UNDOKI-DAY-T2         OCCURS   5.
               05  TBL-HASSYO-UNDOKI-DAY-OCC      OCCURS  31.
                   07  TBL-HASSYO-UNDOKI-SANTEI   PIC 9(01).
           03  TBL-HASSYO-NANBYO-DAY-T.
             04  TBL-HASSYO-NANBYO-DAY-T2         OCCURS   5.
               05  TBL-HASSYO-NANBYO-DAY-OCC      OCCURS  31.
                   07  TBL-HASSYO-NANBYO-SANTEI   PIC 9(01).
           03  TBL-HASSYO-SYOGAI-DAY-T.
             04  TBL-HASSYO-SYOGAI-DAY-T2         OCCURS   5.
               05  TBL-HASSYO-SYOGAI-DAY-OCC      OCCURS  31.
                   07  TBL-HASSYO-SYOGAI-SANTEI   PIC 9(01).
           03  TBL-HASSYO-SESSYOKU-DAY-T.
             04  TBL-HASSYO-SESSYOKU-DAY-T2       OCCURS   5.
               05  TBL-HASSYO-SESSYOKU-DAY-OCC    OCCURS  31.
                   07  TBL-HASSYO-SESSYOKU-SANTEI PIC 9(01).
           03  TBL-HASSYO-HAIYO-DAY-T.
             04  TBL-HASSYO-HAIYO-DAY-T2          OCCURS   5.
               05  TBL-HASSYO-HAIYO-DAY-OCC       OCCURS  31.
                   07  TBL-HASSYO-HAIYO-SANTEI    PIC 9(01).
           03  TBL-HASSYO-TAIHI-DAY-T.
               05  TBL-HASSYO-TAIHI-DAY-OCC       OCCURS  31.
                   07  TBL-HASSYO-TAIHI-SANTEI    PIC 9(01).
      *****
           03  TBL-HASSYO-SINYMD1-DAY-T.
             04  TBL-HASSYO-SINYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO-SINYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO-SINYMD1-DAY-OCC1.
                   07  TBL-HASSYO-SINYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO-SINYMD1-DAY-OCC2.
                   07  TBL-HASSYO-SINYMD12        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-KOKYMD1-DAY-T.
             04  TBL-HASSYO-KOKYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO-KOKYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO-KOKYMD1-DAY-OCC1.
                   07  TBL-HASSYO-KOKYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO-KOKYMD1-DAY-OCC2.
                   07  TBL-HASSYO-KOKYMD12        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-NOUYMD1-DAY-T.
             04  TBL-HASSYO-NOUYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO-NOUYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO-NOUYMD1-DAY-OCC1.
                   07  TBL-HASSYO-NOUYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO-NOUYMD1-DAY-OCC2.
                   07  TBL-HASSYO-NOUYMD12        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-UNDYMD1-DAY-T.
             04  TBL-HASSYO-UNDYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO-UNDYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO-UNDYMD1-DAY-OCC1.
                   07  TBL-HASSYO-UNDYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO-UNDYMD1-DAY-OCC2.
                   07  TBL-HASSYO-UNDYMD12        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-NANYMD1-DAY-T.
             04  TBL-HASSYO-NANYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO-NANYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO-NANYMD1-DAY-OCC1.
                   07  TBL-HASSYO-NANYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO-NANYMD1-DAY-OCC2.
                   07  TBL-HASSYO-NANYMD12        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-SYOYMD1-DAY-T.
             04  TBL-HASSYO-SYOYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO-SYOYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO-SYOYMD1-DAY-OCC1.
                   07  TBL-HASSYO-SYOYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO-SYOYMD1-DAY-OCC2.
                   07  TBL-HASSYO-SYOYMD12        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-SESYMD1-DAY-T.
             04  TBL-HASSYO-SESYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO-SESYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO-SESYMD1-DAY-OCC1.
                   07  TBL-HASSYO-SESYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO-SESYMD1-DAY-OCC2.
                   07  TBL-HASSYO-SESYMD12        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-HAIYMD1-DAY-T.
             04  TBL-HASSYO-HAIYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO-HAIYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO-HAIYMD1-DAY-OCC1.
                   07  TBL-HASSYO-HAIYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO-HAIYMD1-DAY-OCC2.
                   07  TBL-HASSYO-HAIYMD12        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-TAIHIYMD1-DAY-T.
               05  TBL-HASSYO-TAIHIYMD1-FLG       PIC 9(01).
               05  TBL-HASSYO-TAIHIYMD1-DAY-OCC1.
                   07  TBL-HASSYO-TAIHIYMD11      PIC X(08) OCCURS  2.
               05  TBL-HASSYO-TAIHIYMD1-DAY-OCC2.
                   07  TBL-HASSYO-TAIHIYMD12      PIC 9(01) OCCURS 31.
      *
           03  TBL-HASSYO-SINDAI2-DAY-T.
             04  TBL-HASSYO-SINDAI2-DAY-OCC         OCCURS  10.
               05  TBL-HASSYO-SINDAI2-DAY-OCC2      OCCURS  31.
                   07  TBL-HASSYO-SINDAI2-SANTEI    PIC 9(01).
           03  TBL-HASSYO-KOKYUKI2-DAY-T.
             04  TBL-HASSYO-KOKYUKI2-DAY-OCC        OCCURS  10.
               05  TBL-HASSYO-KOKYUKI2-DAY-OCC2     OCCURS  31.
                   07  TBL-HASSYO-KOKYUKI2-SANTEI   PIC 9(01).
           03  TBL-HASSYO-NOUKE2-DAY-T.
             04  TBL-HASSYO-NOUKE2-DAY-OCC          OCCURS  10.
               05  TBL-HASSYO-NOUKE2-DAY-OCC2       OCCURS  31.
                   07  TBL-HASSYO-NOUKE2-SANTEI     PIC 9(01).
           03  TBL-HASSYO-UNDOKI2-DAY-T.
             04  TBL-HASSYO-UNDOKI2-DAY-OCC         OCCURS  10.
               05  TBL-HASSYO-UNDOKI2-DAY-OCC2      OCCURS  31.
                   07  TBL-HASSYO-UNDOKI2-SANTEI    PIC 9(01).
           03  TBL-HASSYO-NANBYO2-DAY-T.
             04  TBL-HASSYO-NANBYO2-DAY-OCC         OCCURS  10. 
               05  TBL-HASSYO-NANBYO2-DAY-OCC2      OCCURS  31.
                   07  TBL-HASSYO-NANBYO2-SANTEI    PIC 9(01).
           03  TBL-HASSYO-SYOGAI2-DAY-T.
             04  TBL-HASSYO-SYOGAI2-DAY-OCC         OCCURS  10.
               05  TBL-HASSYO-SYOGAI2-DAY-OCC2      OCCURS  31.
                   07  TBL-HASSYO-SYOGAI2-SANTEI    PIC 9(01).
           03  TBL-HASSYO-SESSYOKU2-DAY-T.
             04  TBL-HASSYO-SESSYOKU2-DAY-OCC       OCCURS  10.
               05  TBL-HASSYO-SESSYOKU2-DAY-OCC2    OCCURS  31.
                   07  TBL-HASSYO-SESSYOKU2-SANTEI  PIC 9(01).
           03  TBL-HASSYO-HAIYO2-DAY-T.
             04  TBL-HASSYO-HAIYO2-DAY-OCC          OCCURS  10.
               05  TBL-HASSYO-HAIYO2-DAY-OCC2       OCCURS  31.
                   07  TBL-HASSYO-HAIYO2-SANTEI     PIC 9(01).
      *****
           03  TBL-HASSYO-SINYMD2-DAY-T.
             04  TBL-HASSYO-SINYMD2-DAY-T2          OCCURS  10.
               05  TBL-HASSYO-SINYMD2-FLG           PIC 9(01).
               05  TBL-HASSYO-SINYMD2-DAY-OCC1.
                   07  TBL-HASSYO-SINYMD21          PIC X(08) OCCURS  2.
               05  TBL-HASSYO-SINYMD2-DAY-OCC2.
                   07  TBL-HASSYO-SINYMD22          PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-KOKYMD2-DAY-T.
             04  TBL-HASSYO-KOKYMD2-DAY-T2          OCCURS  10.
               05  TBL-HASSYO-KOKYMD2-FLG           PIC 9(01).
               05  TBL-HASSYO-KOKYMD2-DAY-OCC1.
                   07  TBL-HASSYO-KOKYMD21          PIC X(08) OCCURS  2.
               05  TBL-HASSYO-KOKYMD2-DAY-OCC2.
                   07  TBL-HASSYO-KOKYMD22          PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-NOUYMD2-DAY-T.
             04  TBL-HASSYO-NOUYMD2-DAY-T2          OCCURS  10.
               05  TBL-HASSYO-NOUYMD2-FLG           PIC 9(01).
               05  TBL-HASSYO-NOUYMD2-DAY-OCC1.
                   07  TBL-HASSYO-NOUYMD21          PIC X(08) OCCURS  2.
               05  TBL-HASSYO-NOUYMD2-DAY-OCC2.
                   07  TBL-HASSYO-NOUYMD22          PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-UNDYMD2-DAY-T.
             04  TBL-HASSYO-UNDYMD2-DAY-T2          OCCURS  10.
               05  TBL-HASSYO-UNDYMD2-FLG           PIC 9(01).
               05  TBL-HASSYO-UNDYMD2-DAY-OCC1.
                   07  TBL-HASSYO-UNDYMD21          PIC X(08) OCCURS  2.
               05  TBL-HASSYO-UNDYMD2-DAY-OCC2.
                   07  TBL-HASSYO-UNDYMD22          PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-NANYMD2-DAY-T.
             04  TBL-HASSYO-NANYMD2-DAY-T2          OCCURS  10.
               05  TBL-HASSYO-NANYMD2-FLG           PIC 9(01).
               05  TBL-HASSYO-NANYMD2-DAY-OCC1.
                   07  TBL-HASSYO-NANYMD21          PIC X(08) OCCURS  2.
               05  TBL-HASSYO-NANYMD2-DAY-OCC2.
                   07  TBL-HASSYO-NANYMD22          PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-SYOYMD2-DAY-T.
             04  TBL-HASSYO-SYOYMD2-DAY-T2          OCCURS  10.
               05  TBL-HASSYO-SYOYMD2-FLG           PIC 9(01).
               05  TBL-HASSYO-SYOYMD2-DAY-OCC1.
                   07  TBL-HASSYO-SYOYMD21          PIC X(08) OCCURS  2.
               05  TBL-HASSYO-SYOYMD2-DAY-OCC2.
                   07  TBL-HASSYO-SYOYMD22          PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-SESYMD2-DAY-T.
             04  TBL-HASSYO-SESYMD2-DAY-T2          OCCURS  10.
               05  TBL-HASSYO-SESYMD2-FLG           PIC 9(01).
               05  TBL-HASSYO-SESYMD2-DAY-OCC1.
                   07  TBL-HASSYO-SESYMD21          PIC X(08) OCCURS  2.
               05  TBL-HASSYO-SESYMD2-DAY-OCC2.
                   07  TBL-HASSYO-SESYMD22          PIC 9(01) OCCURS 31.
           03  TBL-HASSYO-HAIYMD2-DAY-T.
             04  TBL-HASSYO-HAIYMD2-DAY-T2          OCCURS  10.
               05  TBL-HASSYO-HAIYMD2-FLG           PIC 9(01).
               05  TBL-HASSYO-HAIYMD2-DAY-OCC1.
                   07  TBL-HASSYO-HAIYMD21          PIC X(08) OCCURS  2.
               05  TBL-HASSYO-HAIYMD2-DAY-OCC2.
                   07  TBL-HASSYO-HAIYMD22          PIC 9(01) OCCURS 31.
      *
           03  TBL-HASSYO-KYUSEIYMD-T.
             04  TBL-HASSYO-KYUSEIYMD-OCC           OCCURS  50.
               05  TBL-HASSYO-SINDAI-KYUSEIYMD      PIC X(08).
               05  TBL-HASSYO-KOKYUKI-KYUSEIYMD     PIC X(08).
               05  TBL-HASSYO-NOUKE-KYUSEIYMD       PIC X(08).
               05  TBL-HASSYO-UNDOKI-KYUSEIYMD      PIC X(08).
               05  TBL-HASSYO-NANBYO-KYUSEIYMD      PIC X(08).
               05  TBL-HASSYO-SYOGAI-KYUSEIYMD      PIC X(08).
               05  TBL-HASSYO-SESSYOKU-KYUSEIYMD    PIC X(08).
               05  TBL-HASSYO-HAIYO-KYUSEIYMD       PIC X(08).
      *
      *****（リハ加算用）
       01  TBL-HASSYO9-INF.
           03  TBL-HASSYO9-SINDAI-FLG            PIC 9(01).
           03  TBL-HASSYO9-KOKYUKI-FLG           PIC 9(01).
           03  TBL-HASSYO9-HAIYO-FLG             PIC 9(01).
           03  TBL-HASSYO9-INF2                  OCCURS  5.
               05  TBL-HASSYO9-SINDAI-COMMENT    PIC 9(01).
               05  TBL-HASSYO9-KOKYUKI-COMMENT   PIC 9(01).
               05  TBL-HASSYO9-HAIYO-COMMENT     PIC 9(01).
               05  TBL-HASSYO9-HKNCOMBI          PIC 9(04).
           03  TBL-HASSYO9-INF3                  OCCURS 10.
               05  TBL-HASSYO9-SINDAI2-COMMENT   PIC 9(01).
               05  TBL-HASSYO9-KOKYUKI2-COMMENT  PIC 9(01).
               05  TBL-HASSYO9-HAIYO2-COMMENT    PIC 9(01).
               05  TBL-HASSYO9-HKNCOMBI2         PIC 9(04).
      *
           03  TBL-HASSYO9-SINDAI-DAY-T.
             04  TBL-HASSYO9-SINDAI-DAY-T2         OCCURS   5.
               05  TBL-HASSYO9-SINDAI-DAY-OCC      OCCURS  31.
                   07  TBL-HASSYO9-SINDAI-SANTEI   PIC 9(01).
           03  TBL-HASSYO9-KOKYUKI-DAY-T.
             04  TBL-HASSYO9-KOKYUKI-DAY-T2        OCCURS   5.
               05  TBL-HASSYO9-KOKYUKI-DAY-OCC     OCCURS  31.
                   07  TBL-HASSYO9-KOKYUKI-SANTEI  PIC 9(01).
           03  TBL-HASSYO9-HAIYO-DAY-T.
             04  TBL-HASSYO9-HAIYO-DAY-T2          OCCURS   5.
               05  TBL-HASSYO9-HAIYO-DAY-OCC       OCCURS  31.
                   07  TBL-HASSYO9-HAIYO-SANTEI    PIC 9(01).
      *****
           03  TBL-HASSYO9-SINYMD1-DAY-T.
             04  TBL-HASSYO9-SINYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO9-SINYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO9-SINYMD1-DAY-OCC1.
                   07  TBL-HASSYO9-SINYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO9-SINYMD1-DAY-OCC2.
                   07  TBL-HASSYO9-SINYMD12        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO9-KOKYMD1-DAY-T.
             04  TBL-HASSYO9-KOKYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO9-KOKYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO9-KOKYMD1-DAY-OCC1.
                   07  TBL-HASSYO9-KOKYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO9-KOKYMD1-DAY-OCC2.
                   07  TBL-HASSYO9-KOKYMD12        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO9-HAIYMD1-DAY-T.
             04  TBL-HASSYO9-HAIYMD1-DAY-T2        OCCURS   5.
               05  TBL-HASSYO9-HAIYMD1-FLG         PIC 9(01).
               05  TBL-HASSYO9-HAIYMD1-DAY-OCC1.
                   07  TBL-HASSYO9-HAIYMD11        PIC X(08) OCCURS  2.
               05  TBL-HASSYO9-HAIYMD1-DAY-OCC2.
                   07  TBL-HASSYO9-HAIYMD12        PIC 9(01) OCCURS 31.
      *
           03  TBL-HASSYO9-SINDAI2-DAY-T.
             04  TBL-HASSYO9-SINDAI2-DAY-OCC         OCCURS  10.
               05  TBL-HASSYO9-SINDAI2-DAY-OCC2      OCCURS  31.
                   07  TBL-HASSYO9-SINDAI2-SANTEI    PIC 9(01).
           03  TBL-HASSYO9-KOKYUKI2-DAY-T.
             04  TBL-HASSYO9-KOKYUKI2-DAY-OCC        OCCURS  10.
               05  TBL-HASSYO9-KOKYUKI2-DAY-OCC2     OCCURS  31.
                   07  TBL-HASSYO9-KOKYUKI2-SANTEI   PIC 9(01).
           03  TBL-HASSYO9-HAIYO2-DAY-T.
             04  TBL-HASSYO9-HAIYO2-DAY-OCC          OCCURS  10.
               05  TBL-HASSYO9-HAIYO2-DAY-OCC2       OCCURS  31.
                   07  TBL-HASSYO9-HAIYO2-SANTEI     PIC 9(01).
      *****
           03  TBL-HASSYO9-SINYMD2-DAY-T.
             04  TBL-HASSYO9-SINYMD2-DAY-T2        OCCURS  10.
               05  TBL-HASSYO9-SINYMD2-FLG         PIC 9(01).
               05  TBL-HASSYO9-SINYMD2-DAY-OCC1.
                   07  TBL-HASSYO9-SINYMD21        PIC X(08) OCCURS  2.
               05  TBL-HASSYO9-SINYMD2-DAY-OCC2.
                   07  TBL-HASSYO9-SINYMD22        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO9-KOKYMD2-DAY-T.
             04  TBL-HASSYO9-KOKYMD2-DAY-T2        OCCURS  10.
               05  TBL-HASSYO9-KOKYMD2-FLG         PIC 9(01).
               05  TBL-HASSYO9-KOKYMD2-DAY-OCC1.
                   07  TBL-HASSYO9-KOKYMD21        PIC X(08) OCCURS  2.
               05  TBL-HASSYO9-KOKYMD2-DAY-OCC2.
                   07  TBL-HASSYO9-KOKYMD22        PIC 9(01) OCCURS 31.
           03  TBL-HASSYO9-HAIYMD2-DAY-T.
             04  TBL-HASSYO9-HAIYMD2-DAY-T2        OCCURS  10.
               05  TBL-HASSYO9-HAIYMD2-FLG         PIC 9(01).
               05  TBL-HASSYO9-HAIYMD2-DAY-OCC1.
                   07  TBL-HASSYO9-HAIYMD21        PIC X(08) OCCURS  2.
               05  TBL-HASSYO9-HAIYMD2-DAY-OCC2.
                   07  TBL-HASSYO9-HAIYMD22        PIC 9(01) OCCURS 31.
      *
           03  TBL-HASSYO9-KYUSEIYMD-T.
             04  TBL-HASSYO9-KYUSEIYMD-OCC           OCCURS  50.
               05  TBL-HASSYO9-SINDAI-KYUSEIYMD      PIC X(08).
               05  TBL-HASSYO9-KOKYUKI-KYUSEIYMD     PIC X(08).
               05  TBL-HASSYO9-HAIYO-KYUSEIYMD       PIC X(08).
      *****
      *
       01  TBL-ZAISIEN-INF.
           03  TBL-ZAISIEN-INF2                 OCCURS  5.
               05  TBL-ZAISIEN-COMMENT          PIC 9(01).
               05  TBL-ZAISIEN-HKNCOMBI         PIC 9(04).
      *
       01  TBL-SEISIN-SKJ-INF.
           03  TBL-SEISIN-SKJ-INF2              OCCURS  5.
               05  TBL-SEISIN-SKJ-COMMENT       PIC 9(01).
               05  TBL-SEISIN-SKJ-HKNCOMBI      PIC 9(04).
      *
       01  TBL-GENBAKU-INF.
           03  TBL-GENBAKU-INF2                 OCCURS  5.
               05  TBL-GENBAKU-COMMENT          PIC 9(01).
               05  TBL-GENBAKU-HKNCOMBI         PIC 9(04).
               05  TBL-GENBAKU-FTNRATE          PIC 9(03).
      *
      *    酸素（金額・数量）情報
       01  TBL-SANSO-INF.
           03  TBL-SANSO-INF2       OCCURS    10.
               05  TBL-SANSO-TEN           PIC 9(09)V9(02).
               05  TBL-SANSO-SRYSURYO      PIC 9(05)V9(05).
           03  TBL-SANSO-INF3.
               05  TBL-KASAN-SRYSURYO      PIC 9(05)V9(05).
      *
      *    療養レセコメント情報
       01  TBL-RYOCOM-INF.
           03  TBL-RYOCOM-INF2                 OCCURS  31.
               05  TBL-RYOCOM-NYUINKBN         PIC  X(01).
               05  TBL-RYOCOM-AREA.
                 06  TBL-RYOCOM-MEISAISU       PIC  9(03).
                 06  TBL-RYOCOM-SRYCDTOTAL     PIC  9(14).
                 06  TBL-RYOCOM-INF3           OCCURS  30.
                   07  TBL-RYOCOM-IRYO-SRYCD   PIC  X(09).
                 06  TBL-RYOCOM-ADL-SRYCD      PIC  X(09).
                 06  TBL-RYOCOM-ADL-TEN        PIC  9(02).
                 06  TBL-RYOCOM-ADL-TEN-MOJI   PIC  X(04).
                 06  TBL-RYOCOM-INF4           OCCURS   4.
                   07  TBL-RYOCOM-ADL-SRYCD-H22      PIC  X(09).
                   07  TBL-RYOCOM-ADL-TEN-H22        PIC  9(02).
                   07  TBL-RYOCOM-ADL-TEN-MOJI-H22   PIC  X(04).
      *
      *    療養レセコメント情報（該当分のまとめ）
       01  TBL-RYOCOM2-INF.
           03  TBL-RYOCOM2-INF2                OCCURS  31.
               05  TBL-RYOCOM2-AREA.
                 06  TBL-RYOCOM2-MEISAISU      PIC  9(03).
                 06  TBL-RYOCOM2-SRYCDTOTAL    PIC  9(14).
                 06  TBL-RYOCOM2-INF3          OCCURS  30.
                   07  TBL-RYOCOM2-IRYO-SRYCD  PIC  X(09).
                 06  TBL-RYOCOM2-ADL-SRYCD     PIC  X(09).
                 06  TBL-RYOCOM2-ADL-TEN       PIC  9(02).
                 06  TBL-RYOCOM2-ADL-TEN-MOJI  PIC  X(04).
                 06  TBL-RYOCOM2-INF4          OCCURS   4.
                   07  TBL-RYOCOM2-ADL-SRYCD-H22     PIC  X(09).
                   07  TBL-RYOCOM2-ADL-TEN-H22       PIC  9(02).
                   07  TBL-RYOCOM2-ADL-TEN-MOJI-H22  PIC  X(04).
               05  TBL-RYOCOM2-INF5            OCCURS  31.
                 06  TBL-RYOCOM2-DAY           PIC  9(01).
      *
       01  TBL-CYOJKSN-INF.
           03  TBL-CYOJKSN-INF2                OCCURS  5.
               05  TBL-CYOJKSN-X               PIC  9(01).
               05  TBL-CYOJKSN-Y               PIC  9(01).
       01  TBL-CYOJKSN2-INF.
           03  TBL-CYOJKSN2-INF2               OCCURS  5.
             04  TBL-CYOJKSN2-INF3             OCCURS 10.
               05  TBL-CYOJKSN2-X-TAIHIYMD     PIC  X(140).
               05  TBL-CYOJKSN2-Y-TAIHIYMD     PIC  X(140).
      *
      *    パス名領域
       01  WRK-PATH                        PIC X(64).
      *
      *    酸素 
           COPY    "CMSANSOTBL.INC".
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    算定履歴付加
       01  SANTEIPLUS-REC.
           COPY    "CPSANTEIPLUS.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    レセプト記載
       01  RKISAI-REC.
           COPY    "CPRECEKISAI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *     
      *    数値変換サブ
           COPY    "CPORCSNUM.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    療養レセコメント取得サブ
           COPY    "CPORCSRYOCOM.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    オンライン診療料等に関わる算定履歴チェックサブ
           COPY    "CPORCSRECESRCHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL  FLG-END  =   1
      *
           PERFORM 300-END-SEC
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ＤＢオープン処理
               PERFORM 120-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0660"      TO  JOB-PGID
               MOVE    "コメント編集"  TO  JOB-SHELLMSG
               PERFORM   900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
      *    ファイルＯＰＥＮ処理
           PERFORM 110-FILE-OPEN-SEC
      *
           PERFORM 990-RECE62-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルＯＰＥＮ処理
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    "RECE620"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           MOVE    "RECE64W"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (3)
           MOVE    "RECE650"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (4)
           MOVE    "RECE641"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (5)
           MOVE    "RECE651"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (6)
           MOVE    "RECE64Y"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (7)
           MOVE    "RECE65Y"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (8)
           MOVE    "RECE655"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (9)
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
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  RECE641PARA
           MOVE    SGETTEMP-FULLNAMES (6)
                                       TO  RECE651PARA
           MOVE    SGETTEMP-FULLNAMES (7)
                                       TO  RECE649PARA
           MOVE    SGETTEMP-FULLNAMES (8)
                                       TO  RECE659PARA
           MOVE    SGETTEMP-FULLNAMES (9)
                                       TO  RECE655PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           OPEN    INPUT               RECE62-FILE
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
      *
           OPEN    INPUT               RECE65-FILE
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
           OPEN    OUTPUT              RECE641-FILE
           CLOSE                       RECE641-FILE
      *
           OPEN    OUTPUT              RECE651-FILE
           CLOSE                       RECE651-FILE
      *
           OPEN    OUTPUT              RECE649-FILE
           CLOSE                       RECE649-FILE
      *
           OPEN    OUTPUT              RECE659-FILE
           CLOSE                       RECE659-FILE
      *
           OPEN    I-O                 RECE641-FILE
           OPEN    I-O                 RECE649-FILE
      *
           OPEN    I-O                 RECE651-FILE
           OPEN    I-O                 RECE659-FILE
      *
           OPEN    OUTPUT              RECE655-FILE
      *
           .
       110-FILE-OPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                      TBL-HASSYO-INF
           INITIALIZE                      TBL-HASSYO9-INF
           INITIALIZE                      TBL-ZAISIEN-INF
           INITIALIZE                      TBL-RYOCOM-INF
           INITIALIZE                      TBL-CYOJKSN-INF
           INITIALIZE                      TBL-CYOJKSN2-INF
           INITIALIZE                      TBL-SEISIN-SKJ-INF
           INITIALIZE                      TBL-GENBAKU-INF
           INITIALIZE                      WRK-JIDO-SETCOM-AREA
      *
      *    療養レセコメント取得
      *
           MOVE      ZERO     TO      FLG-RYOCOM
      *
           IF    RECE62-SRYYM   >=  "200610"
           AND   RECE62-SRYYM   <=  "201803"
             INITIALIZE                   ORCSRYOCOM-AREA
             MOVE    RECE62-HOSPNUM   TO  LNK-RYOCOM-HOSPNUM
             MOVE    RECE62-PTID      TO  LNK-RYOCOM-PTID
             MOVE    RECE62-SRYYM     TO  LNK-RYOCOM-SANTEIYM
             CALL    "ORCSRYOCOM"         USING
                                          ORCSRYOCOM-AREA
                                          SPA-AREA
             IF      LNK-RYOCOM-RC    =   ZERO
               MOVE       1           TO  FLG-RYOCOM
               PERFORM  VARYING  RYO-IDX  FROM  1  BY  1
                         UNTIL   RYO-IDX   >   31
                 MOVE  LNK-RYOCOM-NYUINKBN(RYO-IDX)
                                 TO  TBL-RYOCOM-NYUINKBN(RYO-IDX)
                 MOVE  LNK-RYOCOM-MEISAISU(RYO-IDX)
                                 TO  TBL-RYOCOM-MEISAISU(RYO-IDX)
                 MOVE  LNK-RYOCOM-SRYCDTOTAL(RYO-IDX)
                                 TO  TBL-RYOCOM-SRYCDTOTAL(RYO-IDX)
                 PERFORM  VARYING  RYO-IDX2  FROM  1  BY  1
                           UNTIL   RYO-IDX2   >   30
                   MOVE  LNK-RYOCOM-IRYO-SRYCD(RYO-IDX RYO-IDX2)
                           TO  TBL-RYOCOM-IRYO-SRYCD(RYO-IDX RYO-IDX2)
                 END-PERFORM
                 MOVE  LNK-RYOCOM-ADL-SRYCD(RYO-IDX)
                                 TO  TBL-RYOCOM-ADL-SRYCD(RYO-IDX)
                 MOVE  LNK-RYOCOM-ADL-TEN(RYO-IDX)
                                 TO  TBL-RYOCOM-ADL-TEN(RYO-IDX)
                 MOVE  LNK-RYOCOM-ADL-TEN-MOJI(RYO-IDX)
                                 TO  TBL-RYOCOM-ADL-TEN-MOJI(RYO-IDX)
                 PERFORM  VARYING  RYO-IDX2  FROM  1  BY  1
                           UNTIL   RYO-IDX2   >    4
                   MOVE  LNK-RYOCOM-ADL-SRYCD-H22(RYO-IDX RYO-IDX2)
                      TO  TBL-RYOCOM-ADL-SRYCD-H22(RYO-IDX RYO-IDX2)
                   MOVE  LNK-RYOCOM-ADL-TEN-H22(RYO-IDX RYO-IDX2)
                      TO  TBL-RYOCOM-ADL-TEN-H22(RYO-IDX RYO-IDX2)
                   MOVE  LNK-RYOCOM-ADL-TEN-MOJI-H22(RYO-IDX RYO-IDX2)
                      TO  TBL-RYOCOM-ADL-TEN-MOJI-H22(RYO-IDX RYO-IDX2)
                 END-PERFORM
               END-PERFORM
             END-IF
           END-IF
      *
           MOVE  RECE62-KOHBUNRI-MIN   TO  WRK-KOHBUNRI-JIDO
           MOVE  ZERO                  TO  WRK-HKNCOMBI-JIDO
      *
           MOVE  ZERO        TO    CNT-KENSA
           IF   RECE62-KENSA-BNPFLG     =   1
                 ADD    1    TO    CNT-KENSA
           END-IF
           IF   RECE62-KENSA-NTPROFLG   =   1
                 ADD    1    TO    CNT-KENSA
           END-IF
           IF   RECE62-KENSA-ANPFLG     =   1
                 ADD    1    TO    CNT-KENSA
           END-IF
      *
           IF   RECE62-COMCNTFLG   =   "1"
      *
             INITIALIZE                      RECE64-REC
             MOVE    RECE62-KEY          TO  RECE64-KEY1
             MOVE    ZERO                TO  RECE64-ZAINUM
      *
             START   RECE64-FILE
                 KEY IS    >=   RECE64-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE64
                 NOT INVALID
                     PERFORM 990-RECE64-NEXT-SEC
             END-START
             PERFORM
               UNTIL  ( FLG-RECE64           =   1          )
               OR     ( RECE62-KEY       NOT =  RECE64-KEY1 )
      *
               EVALUATE      RECE64-SRYKBN
                   WHEN      "14"
      *            （在支援に関する診療のカウント）
                      IF  RECE64-FLG-ZAISIEN-COMMENT   =  "1" OR "2"
                        MOVE    RECE64-REC    TO   WRK-RECE64-REC
                        COMPUTE WRK-KOHBUNRI
                                   =    RECE64-KOHBUNRI  +  1
                        EVALUATE  RECE64-FLG-ZAISIEN-COMMENT
                         WHEN      "1"
                          MOVE 1 TO TBL-ZAISIEN-COMMENT(WRK-KOHBUNRI)
                         WHEN      "2"
                          MOVE 2 TO TBL-ZAISIEN-COMMENT(WRK-KOHBUNRI)
                        END-EVALUATE
                        MOVE  RECE64-HKNCOMBI  TO
                                     TBL-ZAISIEN-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
                   WHEN      "40"
      *            （介達牽引あり）
                      IF  RECE64-KAITATU-COMMENT    =  "1"
                        MOVE    1    TO    TBL-HASSYO-KAITATU-FLG
                        COMPUTE  WRK-KOHBUNRI
                                   =    RECE64-KOHBUNRI  +  1
                        MOVE    1    TO
                               TBL-HASSYO-KAITATU-COMMENT(WRK-KOHBUNRI)
                        MOVE   RECE64-HKNCOMBI  TO
                                      TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                      END-IF
                   WHEN      "80"
      *            （リハビリあり）
                      IF  RECE64-SINDAI-COMMENT     =  "1"
                        MOVE    9    TO    TBL-HASSYO-SINDAI-FLG
                      END-IF
                      IF  RECE64-KOKYUKI-COMMENT    =  "1"
                        MOVE    9    TO    TBL-HASSYO-KOKYUKI-FLG
                      END-IF
                      IF  RECE64-NOUKE-COMMENT      =  "1"
                        MOVE    9    TO    TBL-HASSYO-NOUKE-FLG
                      END-IF
                      IF  RECE64-UNDOKI-COMMENT     =  "1"
                        MOVE    9    TO    TBL-HASSYO-UNDOKI-FLG
                      END-IF
                      IF  RECE64-NANBYO-COMMENT     =  "1"
                        MOVE    9    TO    TBL-HASSYO-NANBYO-FLG
                      END-IF
                      IF  RECE64-SYOGAI-COMMENT     =  "1"
                        MOVE    9    TO    TBL-HASSYO-SYOGAI-FLG
                      END-IF
                      IF  RECE64-SESSYOKU-COMMENT   =  "1"
                        MOVE    9    TO    TBL-HASSYO-SESSYOKU-FLG
                      END-IF
                      IF  RECE64-HAIYO-COMMENT      =  "1"
                        MOVE    9    TO    TBL-HASSYO-HAIYO-FLG
                      END-IF
      *
                      IF  RECE64-SINDAI9-COMMENT    =  "1"
                        MOVE    9    TO    TBL-HASSYO9-SINDAI-FLG
                      END-IF
                      IF  RECE64-KOKYUKI9-COMMENT   =  "1"
                        MOVE    9    TO    TBL-HASSYO9-KOKYUKI-FLG
                      END-IF
                      IF  RECE64-HAIYO9-COMMENT     =  "1"
                        MOVE    9    TO    TBL-HASSYO9-HAIYO-FLG
                      END-IF
                   WHEN      "90"
                   WHEN      "92"
                      IF  (RECE62-CYOJKSNFLG  NOT = SPACE     )
                      AND (RECE64-FLG-CYOJKSN-COMMENT =
                                      "1" OR "2" OR "3" OR "4")
                        PERFORM   20011-CYOJKSN-KAISHIDAY-SEC
                      END-IF
               END-EVALUATE
      *
               PERFORM 990-RECE64-NEXT-SEC
      *
             END-PERFORM
      *
           END-IF
      *
      *    パラメータ０４ファイルより編集
           INITIALIZE                      RECE64-REC
           MOVE    RECE62-KEY          TO  RECE64-KEY1
           MOVE    ZERO                TO  RECE64-ZAINUM
      *
           START   RECE64-FILE
               KEY IS    >=   RECE64-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE64
               NOT INVALID
                   PERFORM 990-RECE64-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE64           =   1          )
               OR     ( RECE62-KEY       NOT =  RECE64-KEY1 )
      *
               INITIALIZE    WRK-JIDO-SETCOM-AREA
               MOVE    ZERO   TO   FLG-KAISIEN
      *
               IF   WRK-HKNCOMBI-JIDO   =   ZERO
                 IF    WRK-KOHBUNRI-JIDO   =   RECE64-KOHBUNRI
                       MOVE   RECE64-HKNCOMBI   TO   WRK-HKNCOMBI-JIDO
                 END-IF
               END-IF
      *
               IF   RECE64-JIHOKBN  NOT =  SPACE
                 INITIALIZE  WRK-JIDO-SETCOM-AREA
                 PERFORM     20011-HOKATU-COMMENT-SEC
               ELSE
                 EVALUATE      RECE64-SRYKBN
                   WHEN      "11"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYODOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE64-TENSOU-SEC
                      END-IF
                   WHEN      "13"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-YAKUKANSIDO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NYUEIYOSIDO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HOUMONSIDO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYOBYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-RYOYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-TAIIN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KAISIEN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYUEIYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-JINNAI-HANTEI-SEC
                      END-IF
      *               診区１３・１４共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SINPESUR-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-GANSIDO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HAIJRT-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-TAIKYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE64-TENSOU-SEC
                      END-IF
                   WHEN      "14"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HOUSIJI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HINKAI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-OSIN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KAISYOKU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-YOUATU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ZAIKYO-HANTEI-SEC
                      END-IF
      *               診区１３・１４共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SINPESUR-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ZAIHANKO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE64-TENSOU-SEC
                      END-IF
                   WHEN      "40"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-JINZOUETC-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KYOKUIN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-BOKOHO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KIATU-HANTEI-SEC
                      END-IF
      *               診区４０・８０共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SCPRO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE64-TENSOU-SEC
                      END-IF
                   WHEN      "50"
                      PERFORM      2001-SHUJUTSU-HANTEI-SEC
                   WHEN      "54"
                      PERFORM      2001-MASUI-HANTEI-SEC
                   WHEN      "60"
                   WHEN      "64"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KENSEN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KENSA-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KENKSN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE64-TENSOU-SEC
                      END-IF
                   WHEN      "70"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-CTMRI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KINGAZ-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE64-TENSOU-SEC
                      END-IF
                   WHEN      "80"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HOUMONSIDO-SEI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        IF    RECE64-RIHABIRI-COMMENT  NOT = SPACE
                          IF  RECE64-RIHABIRI-COMMENT      =  "1"
                            MOVE      99   TO   FLG-COMMENT
                            PERFORM   2001-RIHA-COMMENT-SEC
                          ELSE
                            MOVE      99   TO   FLG-COMMENT
                            PERFORM   2001-RIHAIGAKU-COMMENT-SEC
                          END-IF
                        END-IF
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-RIHASEN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SEIKEA-HANTEI-SEC
                      END-IF
      *               診区４０・８０共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SCPRO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE64-TENSOU-SEC
                      END-IF
                   WHEN      "90"
                   WHEN      "92"
                      EVALUATE    RECE64-ZAISKBKBN
                        WHEN   "2"
                          PERFORM      2001-NYU-COMMENT-SEC
                        WHEN   "6"
                          MOVE   ZERO   TO  FLG-COMMENT
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-KAKURI-HANTEI-SEC
                          END-IF
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-EISAPO-HANTEI-SEC
                          END-IF
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-KOKKEA-HANTEI-SEC
                          END-IF
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-JYUSYO-HANTEI-SEC
                          END-IF
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-SEIRIE-HANTEI-SEC
                          END-IF
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-BYOYAK-HANTEI-SEC
                          END-IF
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-CYOJKSN-HANTEI-SEC
                          END-IF
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2001-HAIJSK-HANTEI-SEC
                          END-IF
                          IF   FLG-COMMENT  =  ZERO
                            PERFORM    2099-RECE64-TENSOU-SEC
                          END-IF
                        WHEN   OTHER
                          PERFORM      2099-RECE64-TENSOU-SEC
                      END-EVALUATE
                   WHEN   OTHER
                      PERFORM          2099-RECE64-TENSOU-SEC
                 END-EVALUATE
               END-IF
      *
               PERFORM 990-RECE64-NEXT-SEC
      *
           END-PERFORM
      *
           INITIALIZE    WRK-JIDO-SETCOM-AREA
      *
      *    他保険にて入院料算定済みのコメント
      *
           IF   RECE62-NYUINRYOFLG   NOT =   SPACE
      *
              IF    RECE62-RECESYUBETU  =  8888
                  CONTINUE
              ELSE
                  PERFORM   2001-NYUINRYO-COMMENT-SEC
              END-IF
      *
           END-IF
      *
      *    公費受診日のコメント
      *
           IF   RECE62-KOHJNISSUTBLINF    =   ALL " "
              CONTINUE
           ELSE
              PERFORM   2001-KOHJYUSIN-COMMENT-SEC
           END-IF
      *
      *    ３歳児視覚聴覚精密健康診査（地域公費）の点数コメント
      *
           IF   RECE62-SEIKENFLG      =  1
           AND  RECE62-SEIKENTEN  NOT = ZERO
              PERFORM   2001-KOHTEN-COMMENT-SEC
           END-IF
      *
      *    類事老人窓口徴収額のコメント
      *
           IF   RECE62-KOH-SAIKEIFLG    =   1
              PERFORM   2001-KOHSAIKEI-COMMENT-SEC
           END-IF
      *
      *    発症日のコメント
           IF   RECE62-SRYYM      >=    "200604"
               PERFORM   2001-HASSYO-HANTEI-SEC
           ELSE
               MOVE      ZERO            TO   FLG-COMOKIKAE
               MOVE      CONS-KYUSEI     TO   WRK-SRYCD
               PERFORM   2001-KYUSEIHASSYO-HANTEI-SEC
           END-IF
      *
      *    在支援コメント
           PERFORM   VARYING   IDX    FROM   1   BY   1
                      UNTIL    IDX      >    5
              IF     TBL-ZAISIEN-COMMENT(IDX)   NOT =  ZERO
                  PERFORM    2001-ZAISIEN-COMMENT-SEC
              END-IF
           END-PERFORM
      *
      *    障害のコメント（７５歳到達月の特例）
      *
           IF   RECE62-RJNGAIKOHFTN(3)  =   1
              PERFORM   2001-KOUREI-TOKUREI-COMMENT-SEC
           END-IF
      *
      *    特別のコメント（特別な関係にある他院）
      *
           IF   RECE62-TAINFLG          =  "1"
              PERFORM   2001-TAIN-TOKUBETU-COMMENT-SEC
           END-IF
      *
      *    標準負担額経過措置（精神）入院年月日のコメント
      *
           IF   RECE62-SEISIN-SKJFLG        =   1
           AND  RECE62-SEISIN-NYUINYMD  NOT =  SPACE
             INITIALIZE                         STS-AREA-DAY
             INITIALIZE                         LNK-DAY2-AREA
             MOVE    "21"                   TO  LNK-DAY2-IRAI
             MOVE  RECE62-SEISIN-NYUINYMD   TO  LNK-DAY2-YMD
             CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                            LNK-DAY2-AREA
             IF      STS-DAY-RC1            =   ZERO
                PERFORM 2001-SEISIN-SKJ-COMMENT-SEC
             END-IF
           END-IF
      *
      *    １９原爆一般特殊計算の負担割合コメント
      *
           IF   RECE62-GENBAKU-TOKFLG   =   1
                PERFORM 2001-GENBAKU-COMMENT-SEC
           END-IF
      *
      *    妊婦のコメント
      *
           IF   RECE62-NINPUFLG   =   1
                PERFORM 2001-NINPU-COMMENT-SEC
           END-IF
      *
      *****差込編集２**********************
           PERFORM     250-SASHIKOMI2-SEC
      *
      *****選択式コメント編集**************
           IF   RECE62-SRYYM  >=  "201810"
             PERFORM   270-SENTAKUCOM-HENSYU-SEC
           END-IF
      *
      *****関係コメント編集****************
           PERFORM     260-SETCOM-HENSYU-SEC
      *
           PERFORM 990-RECE62-READ-SEC
      *
           IF  CNT-IN-COMMIT   =    300
                 MOVE  ZERO          TO      CNT-IN-COMMIT
                 PERFORM 900-DBCOMMIT-SEC
                 PERFORM 900-DBSTART-SEC
           END-IF
      *
      *    処理中止設定処理
           IF      CNT-IN-CANCEL       =   50
               MOVE    ZERO                TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    他保険にて入院料算定済みのコメント編集
      *****************************************************************
       2001-NYUINRYO-COMMENT-SEC          SECTION.
      *
           PERFORM 500-RECE64-KEY-HENSYU-SEC
           MOVE    99999999            TO  RECE649-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE649-KOHBUNRI
           MOVE    "90"                TO  RECE649-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE649-HKNCOMBI
      *
           MOVE    1                   TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           PERFORM 500-RECE65-KEY-HENSYU-SEC
           MOVE    99999999            TO  RECE659-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE659-KOHBUNRI
           MOVE    "90"                TO  RECE659-SRYKBN
      *    コメントの挿入
           MOVE    SPACE               TO  WRK-RECE65XX-REC
           INITIALIZE                      WRK-RECE65XX-REC
           MOVE    "810000001"         TO  WRK-RECE65XX-SRYCD(1 1)
           MOVE     ZERO               TO  WRK-RECE65XX-SRYSURYO(1 1)
           EVALUATE   RECE62-NYUINRYOFLG
             WHEN     "1"
               MOVE     10             TO  WRK-RECE65XX-YUKOKETA(1 1)
               MOVE    "自費にて入院料算定済" 
                                       TO  WRK-RECE65XX-NAME(1 1)
             WHEN     "2"
               MOVE     10             TO  WRK-RECE65XX-YUKOKETA(1 1)
               MOVE    "労災にて入院料算定済" 
                                       TO  WRK-RECE65XX-NAME(1 1)
             WHEN     "3"
               MOVE     11             TO  WRK-RECE65XX-YUKOKETA(1 1)
               MOVE    "自賠責にて入院料算定済" 
                                       TO  WRK-RECE65XX-NAME(1 1)
             WHEN     "4"
               MOVE     10             TO  WRK-RECE65XX-YUKOKETA(1 1)
               MOVE    "国保にて入院料算定済" 
                                       TO  WRK-RECE65XX-NAME(1 1)
             WHEN     "5"
               MOVE     10             TO  WRK-RECE65XX-YUKOKETA(1 1)
               MOVE    "社保にて入院料算定済" 
                                       TO  WRK-RECE65XX-NAME(1 1)
             WHEN     "6"
               MOVE     11             TO  WRK-RECE65XX-YUKOKETA(1 1)
               MOVE    "他保険にて入院料算定済" 
                                       TO  WRK-RECE65XX-NAME(1 1)
             WHEN     "7"
               MOVE     10             TO  WRK-RECE65XX-YUKOKETA(1 1)
               MOVE    "公害にて入院料算定済" 
                                       TO  WRK-RECE65XX-NAME(1 1)
             WHEN     "8"
               MOVE     10             TO  WRK-RECE65XX-YUKOKETA(1 1)
               MOVE    "後保にて入院料算定済" 
                                       TO  WRK-RECE65XX-NAME(1 1)
           END-EVALUATE
      *
           MOVE    WRK-RECE65XX-SRYACT-TBL(1 1)
                                       TO  RECE659-SRYACT-TBL(1)
           WRITE   RECE659-REC
           ADD     1                   TO  CNT-RECE659
      *
           .
       2001-NYUINRYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    在支援のコメント編集
      *****************************************************************
       2001-ZAISIEN-COMMENT-SEC          SECTION.
      *
           PERFORM 500-RECE64-KEY-HENSYU-SEC
           COMPUTE WRK-ZAINUM    =  99999964  +  IDX
           MOVE    WRK-ZAINUM          TO  RECE649-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE649-KOHBUNRI
           MOVE    WRK-RECE64-SRYKBN   TO  RECE649-SRYKBN
           MOVE    TBL-ZAISIEN-HKNCOMBI(IDX)
                                       TO  RECE649-HKNCOMBI
      *
           MOVE    1                   TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           PERFORM 500-RECE65-KEY-HENSYU-SEC
           COMPUTE WRK-ZAINUM    =  99999964  +  IDX
           MOVE    WRK-ZAINUM          TO  RECE659-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE659-KOHBUNRI
           MOVE    WRK-RECE64-SRYKBN   TO  RECE659-SRYKBN
      *    在支援コメントの挿入
           MOVE    SPACE               TO  WRK-RECE65XX-REC
           INITIALIZE                      WRK-RECE65XX-REC
           MOVE    "810000001"         TO  WRK-RECE65XX-SRYCD(1 1)
           MOVE     ZERO               TO  WRK-RECE65XX-SRYSURYO(1 1)
           MOVE     3                  TO  WRK-RECE65XX-YUKOKETA(1 1)
           EVALUATE  TBL-ZAISIEN-COMMENT(IDX)
             WHEN      1
               MOVE    "在支援"        TO  WRK-RECE65XX-NAME(1 1)
             WHEN      2
               MOVE    "在支病"        TO  WRK-RECE65XX-NAME(1 1)
           END-EVALUATE
      *
           MOVE    WRK-RECE65XX-SRYACT-TBL(1 1)
                                       TO  RECE659-SRYACT-TBL(1)
           WRITE   RECE659-REC
           ADD     1                   TO  CNT-RECE659
      *
           .
       2001-ZAISIEN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    障害のコメント（７５歳到達月の特例）
      *****************************************************************
       2001-KOUREI-TOKUREI-COMMENT-SEC          SECTION.
      *
           PERFORM 500-RECE64-KEY-HENSYU-SEC
           MOVE    99999998            TO  RECE649-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE649-KOHBUNRI
           MOVE    "99"                TO  RECE649-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE649-HKNCOMBI
      *
           MOVE    1                   TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           PERFORM 500-RECE65-KEY-HENSYU-SEC
           MOVE    99999998            TO  RECE659-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE659-KOHBUNRI
           MOVE    "99"                TO  RECE659-SRYKBN
      *    コメントの挿入
           MOVE    SPACE               TO  WRK-RECE65XX-REC
           INITIALIZE                      WRK-RECE65XX-REC
           MOVE    "820000143"         TO  WRK-RECE65XX-SRYCD(1 1)
           MOVE     ZERO               TO  WRK-RECE65XX-SRYSURYO(1 1)
           MOVE     4                  TO  WRK-RECE65XX-YUKOKETA(1 1)
           MOVE    "（障害）"          TO  WRK-RECE65XX-NAME(1 1)
      *
           MOVE    WRK-RECE65XX-SRYACT-TBL(1 1)
                                       TO  RECE659-SRYACT-TBL(1)
           WRITE   RECE659-REC
           ADD     1                   TO  CNT-RECE659
      *
           .
       2001-KOUREI-TOKUREI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    特別のコメント（特別な関係にある他院）
      *****************************************************************
       2001-TAIN-TOKUBETU-COMMENT-SEC          SECTION.
      *
           PERFORM 500-RECE64-KEY-HENSYU-SEC
           MOVE    99999990            TO  RECE649-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE649-KOHBUNRI
           MOVE    "99"                TO  RECE649-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE649-HKNCOMBI
      *
           MOVE    1                   TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           PERFORM 500-RECE65-KEY-HENSYU-SEC
           MOVE    99999990            TO  RECE659-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE659-KOHBUNRI
           MOVE    "99"                TO  RECE659-SRYKBN
      *    コメントの挿入
           MOVE    SPACE               TO  WRK-RECE65XX-REC
           INITIALIZE                      WRK-RECE65XX-REC
           MOVE    "810000001"         TO  WRK-RECE65XX-SRYCD(1 1)
           MOVE     ZERO               TO  WRK-RECE65XX-SRYSURYO(1 1)
           MOVE     4                  TO  WRK-RECE65XX-YUKOKETA(1 1)
           MOVE    "（特別）"          TO  WRK-RECE65XX-NAME(1 1)
      *
           MOVE    WRK-RECE65XX-SRYACT-TBL(1 1)
                                       TO  RECE659-SRYACT-TBL(1)
           WRITE   RECE659-REC
           ADD     1                   TO  CNT-RECE659
      *
           .
       2001-TAIN-TOKUBETU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    標準負担額経過措置（精神）入院年月日のコメント
      *****************************************************************
       2001-SEISIN-SKJ-COMMENT-SEC          SECTION.
      *
           PERFORM  VARYING  IDX34  FROM  1  BY  1
                     UNTIL  (IDX34   >   10                )
                      OR    (RECE62-HKNCOMBI(IDX34) =  ZERO)
             COMPUTE  WRK-KOHBUNRI = RECE62-RIHA-KOHBUNRI(IDX34)
                                   + 1
             PERFORM  VARYING  IDX35  FROM  1  BY  1
                       UNTIL   IDX35   >   31
               IF   RECE62-SEISIN-SKJSAN(IDX34 IDX35)  =  1
                  MOVE  1   TO  TBL-SEISIN-SKJ-COMMENT(WRK-KOHBUNRI)
                  MOVE  RECE62-HKNCOMBI(IDX34)
                            TO  TBL-SEISIN-SKJ-HKNCOMBI(WRK-KOHBUNRI)
               END-IF
             END-PERFORM
           END-PERFORM
      *
           PERFORM  VARYING  IDX34  FROM  1  BY  1
                     UNTIL   IDX34   >    5
      *
             IF  TBL-SEISIN-SKJ-COMMENT(IDX34)  =  1
      *
               PERFORM 500-RECE64-KEY-HENSYU-SEC
               COMPUTE WRK-ZAINUM    =  99999974  +  IDX34
               MOVE    WRK-ZAINUM    TO  RECE649-ZAINUM
               COMPUTE WRK-KOHBUNRI  =  IDX34  -  1
               MOVE    WRK-KOHBUNRI  TO  RECE649-KOHBUNRI
               MOVE    "99"          TO  RECE649-SRYKBN
               MOVE    "991"         TO  RECE649-SRYSYUKBN
               MOVE    TBL-SEISIN-SKJ-HKNCOMBI(IDX34)
                                     TO  RECE649-HKNCOMBI
               MOVE    1             TO  RECE649-MEISAISU
               WRITE   RECE649-REC
               ADD     1             TO  CNT-RECE649
      *
               PERFORM 500-RECE65-KEY-HENSYU-SEC
               COMPUTE WRK-ZAINUM    =  99999974  +  IDX34
               MOVE    WRK-ZAINUM    TO  RECE659-ZAINUM
               COMPUTE WRK-KOHBUNRI  =  IDX34  -  1
               MOVE    WRK-KOHBUNRI  TO  RECE659-KOHBUNRI
               MOVE    "99"          TO  RECE659-SRYKBN
               MOVE    "991"         TO  RECE659-SRYSYUKBN
      *        コメントの挿入
               MOVE    SPACE         TO  WRK-RECE65XX-REC
               INITIALIZE                WRK-RECE65XX-REC
               EVALUATE   LNK-DAY2-EDTYMD1-G
                 WHEN       "H"
                   MOVE  "840000136" TO  WRK-RECE65XX-SRYCD(1 1)
                   MOVE   ZERO       TO  WRK-RECE65XX-SRYSURYO(1 1)
                   MOVE   31         TO  WRK-RECE65XX-YUKOKETA(1 1)
                   MOVE  "標準負担額経過措置（精神）入院年月日："
                                     TO  WRK-RECE65XX-NAME(1 1)(1:38)
                   MOVE  "平成　　　年　　月　　日"
                                     TO  WRK-RECE65XX-NAME(1 1)(39:24)
                 WHEN       "S"
                   MOVE  "840000140" TO  WRK-RECE65XX-SRYCD(1 1)
                   MOVE   ZERO       TO  WRK-RECE65XX-SRYSURYO(1 1)
                   MOVE   31         TO  WRK-RECE65XX-YUKOKETA(1 1)
                   MOVE  "標準負担額経過措置（精神）入院年月日："
                                     TO  WRK-RECE65XX-NAME(1 1)(1:38)
                   MOVE  "昭和　　　年　　月　　日"
                                     TO  WRK-RECE65XX-NAME(1 1)(39:24)
               END-EVALUATE
      *
      ***      INITIALIZE                         STS-AREA-DAY
      ***      INITIALIZE                         LNK-DAY2-AREA
      ***      MOVE    "21"                   TO  LNK-DAY2-IRAI
      ***      MOVE  RECE62-SEISIN-NYUINYMD   TO  LNK-DAY2-YMD
      ***      CALL    "ORCSDAY"    USING     STS-AREA-DAY
      ***                                     LNK-DAY2-AREA
      ***      IF      STS-DAY-RC1            =   ZERO
                  MOVE    LNK-DAY2-EDTYMD1-Y  TO  WRK-TAIHI-XX
                  INITIALIZE                      ORCSKANACHKAREA
                  MOVE    "2"                 TO  KANACHK-SYORI
                  MOVE    WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                  CALL    "ORCSKANACHK"    USING  ORCSKANACHKAREA
                  IF      KANACHK-RC      =   ZERO
                    MOVE    KANACHK-OUT-INPUT(1:4)
                                    TO  WRK-RECE65XX-NAME(1 1)(45:4)
                    MOVE    KANACHK-OUT-INPUT(1:4)
                             TO  WRK-RECE65XX-INPUTCOMENT(1 1)(1:4)
                  END-IF
      *
                  MOVE    LNK-DAY2-EDTYMD1-M  TO  WRK-TAIHI-XX
                  INITIALIZE                      ORCSKANACHKAREA
                  MOVE    "2"                 TO  KANACHK-SYORI
                  MOVE    WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                  CALL    "ORCSKANACHK"    USING  ORCSKANACHKAREA
                  IF      KANACHK-RC      =   ZERO
                    MOVE    KANACHK-OUT-INPUT(1:4)
                                    TO  WRK-RECE65XX-NAME(1 1)(51:4)
                    MOVE    KANACHK-OUT-INPUT(1:4)
                             TO  WRK-RECE65XX-INPUTCOMENT(1 1)(5:4)
                  END-IF
      *
                  MOVE    LNK-DAY2-EDTYMD1-D  TO  WRK-TAIHI-XX
                  INITIALIZE                      ORCSKANACHKAREA
                  MOVE    "2"                 TO  KANACHK-SYORI
                  MOVE    WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                  CALL    "ORCSKANACHK"    USING  ORCSKANACHKAREA
                  IF      KANACHK-RC      =   ZERO
                    MOVE    KANACHK-OUT-INPUT(1:4)
                                    TO  WRK-RECE65XX-NAME(1 1)(57:4)
                    MOVE    KANACHK-OUT-INPUT(1:4)
                             TO  WRK-RECE65XX-INPUTCOMENT(1 1)(9:4)
                  END-IF
      *
                  PERFORM  VARYING  IDX33  FROM  1  BY  2
                            UNTIL   IDX33    >  12
                    IF  WRK-RECE65XX-INPUTCOMENT(1 1)(IDX33:2) = "　"
                       MOVE  "０"   TO
                                 WRK-RECE65XX-INPUTCOMENT(1 1)(IDX33:2)
                    END-IF
                  END-PERFORM
      ***      END-IF
      *
               MOVE    WRK-RECE65XX-SRYACT-TBL(1 1)
                                           TO  RECE659-SRYACT-TBL(1)
               WRITE   RECE659-REC
               ADD     1                   TO  CNT-RECE659
      *
             END-IF
      *
           END-PERFORM
      *
           .
       2001-SEISIN-SKJ-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    １９原爆一般特殊計算の負担割合コメント
      *****************************************************************
       2001-GENBAKU-COMMENT-SEC          SECTION.
      *
           PERFORM  VARYING  IDX34  FROM  1  BY  1
                     UNTIL  (IDX34   >   10                )
                      OR    (RECE62-HKNCOMBI(IDX34) =  ZERO)
             COMPUTE  WRK-KOHBUNRI = RECE62-RIHA-KOHBUNRI(IDX34)
                                   + 1
             IF   RECE62-GENBAKU-FTNKIN(IDX34)  NOT =  ZERO
               MOVE  1   TO  TBL-GENBAKU-COMMENT(WRK-KOHBUNRI)
               MOVE  RECE62-HKNCOMBI(IDX34)
                         TO  TBL-GENBAKU-HKNCOMBI(WRK-KOHBUNRI)
      *
               MOVE  RECE62-GENBAKU-IDX(IDX34)  TO  IDX35
               COMPUTE WRK-GENBAKU-TOTALKIN = RECE62-TOTALTEN(IDX35)
                                            * 10
               MOVE  RECE62-GENBAKU-FTNKIN(IDX34)
                                                TO  WRK-GENBAKU-FTNKIN
               COMPUTE TBL-GENBAKU-FTNRATE(WRK-KOHBUNRI) =
                       (WRK-GENBAKU-FTNKIN / WRK-GENBAKU-TOTALKIN)
                                           * 100
             END-IF
           END-PERFORM
      *
           PERFORM  VARYING  IDX34  FROM  1  BY  1
                     UNTIL   IDX34   >    5
      *
             IF  TBL-GENBAKU-COMMENT(IDX34)  =  1
      *
               PERFORM 500-RECE64-KEY-HENSYU-SEC
               COMPUTE WRK-ZAINUM    =  99999954  +  IDX34
               MOVE    WRK-ZAINUM    TO  RECE649-ZAINUM
               COMPUTE WRK-KOHBUNRI  =  IDX34  -  1
               MOVE    WRK-KOHBUNRI  TO  RECE649-KOHBUNRI
               MOVE    "99"          TO  RECE649-SRYKBN
               MOVE    "990"         TO  RECE649-SRYSYUKBN
               MOVE    TBL-GENBAKU-HKNCOMBI(IDX34)
                                     TO  RECE649-HKNCOMBI
               MOVE    1             TO  RECE649-MEISAISU
               WRITE   RECE649-REC
               ADD     1             TO  CNT-RECE649
      *
               PERFORM 500-RECE65-KEY-HENSYU-SEC
               COMPUTE WRK-ZAINUM    =  99999954  +  IDX34
               MOVE    WRK-ZAINUM    TO  RECE659-ZAINUM
               COMPUTE WRK-KOHBUNRI  =  IDX34  -  1
               MOVE    WRK-KOHBUNRI  TO  RECE659-KOHBUNRI
               MOVE    "99"          TO  RECE659-SRYKBN
               MOVE    "990"         TO  RECE659-SRYSYUKBN
      *        コメントの挿入
               MOVE    SPACE         TO  WRK-RECE65XX-REC
               INITIALIZE                WRK-RECE65XX-REC
               MOVE    "810000001"   TO  WRK-RECE65XX-SRYCD(1 1)
               MOVE    ZERO          TO  WRK-RECE65XX-SRYSURYO(1 1)
      *
               MOVE    TBL-GENBAKU-FTNRATE(IDX34)
                                     TO  WRK-GENBAKU-FTNRATE
               IF   WRK-GENBAKU-FTNRATE2  =  ZERO
                 MOVE  7             TO  WRK-RECE65XX-YUKOKETA(1 1)
                 MOVE  "自己負担　　割"
                                     TO  WRK-RECE65XX-NAME(1 1)
                 MOVE  WRK-GENBAKU-FTNRATE1
                                     TO  DAY-IDX2
                 IF    DAY-IDX2 = ZERO
                   MOVE  "　０"      TO  WRK-RECE65XX-NAME(1 1)(9:4)
                 ELSE
                   MOVE  TBL-DAY(DAY-IDX2)
                                     TO  WRK-RECE65XX-NAME(1 1)(9:4)
                 END-IF
               ELSE
                 MOVE  9             TO  WRK-RECE65XX-YUKOKETA(1 1)
                 MOVE  "自己負担　　割　分"
                                     TO  WRK-RECE65XX-NAME(1 1)
                 MOVE  WRK-GENBAKU-FTNRATE1
                                     TO  DAY-IDX2
                 IF    DAY-IDX2 = ZERO
                   MOVE  "　０"      TO  WRK-RECE65XX-NAME(1 1)(9:4)
                 ELSE
                   MOVE  TBL-DAY(DAY-IDX2)
                                     TO  WRK-RECE65XX-NAME(1 1)(9:4)
                 END-IF
                 MOVE  WRK-GENBAKU-FTNRATE2
                                     TO  DAY-IDX2
                 MOVE  TBL-DAY(DAY-IDX2)(3:2)
                                     TO  WRK-RECE65XX-NAME(1 1)(15:2)
               END-IF
      *
               MOVE    WRK-RECE65XX-SRYACT-TBL(1 1)
                                     TO  RECE659-SRYACT-TBL(1)
               WRITE   RECE659-REC
               ADD     1             TO  CNT-RECE659
      *
             END-IF
      *
           END-PERFORM
      *
           .
       2001-GENBAKU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費受診日のコメント編集
      *****************************************************************
       2001-KOHJYUSIN-COMMENT-SEC          SECTION.
      *
           INITIALIZE                TBL-NYU-DAY
      *
           MOVE    ZERO          TO  DAY-IDX
           MOVE    ZERO          TO  FLG-NYU-RENZOKU
      *
           PERFORM    VARYING    IDX  FROM   1   BY   1
                      UNTIL     (IDX      >   31)  OR
                                (DAY-IDX  >   31)
                IF   RECE62-KOHJNISSUTBL(IDX)   = SPACE
                      MOVE  ZERO  TO  FLG-NYU-RENZOKU
                ELSE
                      IF    FLG-NYU-RENZOKU  =  ZERO
                         ADD   1     TO  DAY-IDX
                         MOVE  IDX   TO  TBL-NYU-DAY-ST(DAY-IDX)
                      END-IF
                      MOVE  IDX      TO  TBL-NYU-DAY-ED(DAY-IDX)
                      MOVE  1        TO  FLG-NYU-RENZOKU
                END-IF
           END-PERFORM
      *
           PERFORM    VARYING    DAY-IDX  FROM   1   BY   1
                      UNTIL  (DAY-IDX                      >   31)  OR
                             (TBL-NYU-DAY-ST(DAY-IDX)  = ZERO)
                IF    TBL-NYU-DAY-ST(DAY-IDX) 
                                    =   TBL-NYU-DAY-ED(DAY-IDX)
                      MOVE  CONS-SANTEI-COMMENT1
                                    TO  TBL-NYU-DAY-CD(DAY-IDX)
                ELSE
                      MOVE  CONS-SANTEI-COMMENT2
                                    TO  TBL-NYU-DAY-CD(DAY-IDX)
                END-IF
           END-PERFORM
      *
           MOVE       ZERO       TO   IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           MOVE       5          TO   FLG-COMMENT
      *
           PERFORM  20011-FREE-COMMENT3-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                TO  IDX-RECE65
           MOVE    SPACE               TO  WRK-RECE65XX-REC
           INITIALIZE                      WRK-RECE65XX-REC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                ADD    1       TO  IDX-RECE65
                MOVE   "810000001"
                               TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                               TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                MOVE   WK-FREE-NAME2(IDX)
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
           END-PERFORM 
      *
      *    受診日の編集を含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
               PERFORM 500-RECE65-KEY-HENSYU-SEC
                MOVE    99999980             TO  RECE659-ZAINUM
                MOVE    WRK-KOHBUNRI-JIDO    TO  RECE659-KOHBUNRI
                MOVE    "99"                 TO  RECE659-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE659-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                         UNTIL   (IDX       >    5)   OR
                                 (FLG-WRKRECE65   =   1)
                   ADD         1             TO    IDX-RECE65
                   IF (WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE)
                   OR (IDX-RECE65                       >  100 )
                       MOVE    1             TO    FLG-WRKRECE65
                   ELSE
                       MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                        TO   RECE659-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF  
                END-PERFORM
                IF   RECE659-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE659-REC
                   ADD     1            TO   CNT-RECE659
                END-IF
           END-PERFORM
      *
      *
           PERFORM 500-RECE64-KEY-HENSYU-SEC
           MOVE    99999980            TO  RECE649-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE649-KOHBUNRI
           MOVE    "99"                TO  RECE649-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE649-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           .
       2001-KOHJYUSIN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ３歳児視覚聴覚精密健康診査（地域公費）の点数コメント編集
      *****************************************************************
       2001-KOHTEN-COMMENT-SEC          SECTION.
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    SPACE               TO  WRK-RECE65XX-REC
           INITIALIZE                      WRK-RECE65XX-REC
      *
           MOVE    "810000001"         TO  WRK-RECE65XX-SRYCD(1 1)
           MOVE    ZERO                TO  WRK-RECE65XX-SRYSURYO(1 1)
      *
           MOVE    ZERO                TO  IDX31
           MOVE    RECE62-SEIKENTEN    TO  WRK-SEIKENTEN
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-SEIKENTEN       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF    ( KANACHK-RC      =   ZERO )
           AND   ( KANACHK-MAX     >   ZERO )
             PERFORM  VARYING   IDX   FROM   1   BY   2
                       UNTIL    IDX     >    KANACHK-MAX
               IF  KANACHK-OUT-INPUT(IDX:2)  NOT =  "　"
                   MOVE   IDX          TO  IDX31
                   MOVE   KANACHK-MAX  TO  IDX
               END-IF
             END-PERFORM
             IF    IDX31  NOT = ZERO
               COMPUTE  IDX32  =  KANACHK-MAX - IDX31 + 1
               STRING  "３歳児精密健診　"  DELIMITED  BY  SIZE
                        KANACHK-OUT-INPUT(IDX31:IDX32)
                                           DELIMITED  BY  SIZE
                       "点"                DELIMITED  BY  SIZE
                                           INTO  WRK-RECE65XX-NAME(1 1)
               END-STRING
             END-IF
           END-IF
           COMPUTE  WRK-RECE65XX-YUKOKETA(1 1) = 9 + (IDX32 / 2)
      *
      *    受診日の編集を含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
               PERFORM 500-RECE65-KEY-HENSYU-SEC
                MOVE    99999970             TO  RECE659-ZAINUM
                MOVE    WRK-KOHBUNRI-JIDO    TO  RECE659-KOHBUNRI
                MOVE    "99"                 TO  RECE659-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE659-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                         UNTIL   (IDX       >    5)   OR
                                 (FLG-WRKRECE65   =   1)
                   ADD         1             TO    IDX-RECE65
                   IF (WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE)
                   OR (IDX-RECE65                       >  100 )
                       MOVE    1             TO    FLG-WRKRECE65
                   ELSE
                       MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                        TO   RECE659-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF  
                END-PERFORM
                IF   RECE659-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE659-REC
                   ADD     1            TO   CNT-RECE659
                END-IF
           END-PERFORM
      *
      *
           PERFORM 500-RECE64-KEY-HENSYU-SEC
           MOVE    99999970            TO  RECE649-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE649-KOHBUNRI
           MOVE    "99"                TO  RECE649-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE649-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           .
       2001-KOHTEN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    類事老人窓口徴収額のコメント編集
      *****************************************************************
       2001-KOHSAIKEI-COMMENT-SEC          SECTION.
      *
           PERFORM 500-RECE64-KEY-HENSYU-SEC
           MOVE    99999997            TO  RECE649-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE649-KOHBUNRI
           MOVE    "99"                TO  RECE649-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE649-HKNCOMBI
      *
           MOVE    1                   TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           PERFORM 500-RECE65-KEY-HENSYU-SEC
           MOVE    99999997            TO  RECE659-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE659-KOHBUNRI
           MOVE    "99"                TO  RECE659-SRYKBN
      *    コメントの挿入
           MOVE    SPACE               TO  WRK-RECE65XX-REC
           INITIALIZE                      WRK-RECE65XX-REC
           MOVE    "810000001"         TO  WRK-RECE65XX-SRYCD(1 1)
           MOVE     ZERO               TO  WRK-RECE65XX-SRYSURYO(1 1)
           MOVE     10                 TO  WRK-RECE65XX-YUKOKETA(1 1)
           MOVE    "類事老人窓口徴収額　"
                                       TO  WRK-RECE65XX-NAME(1 1)
      *
           MOVE     ZERO               TO  IDA
           MOVE     ZERO               TO  IDB
           MOVE     ZERO               TO  FLG-HENSYUKIN
           MOVE     RECE62-KOH-SAIKEI  TO  WRK-HENSYU-KINGAKU
           MOVE     SPACE              TO  WRK-HENSYU-KINGAKU2
           MOVE     SPACE              TO  WRK-HENSYU-KINGAKU3
      *
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-HENSYU-KINGAKU  TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   ZERO
              MOVE  KANACHK-OUT-INPUT(1:18)
                                       TO  WRK-HENSYU-KINGAKU2
           END-IF
           PERFORM   VARYING   IDA   FROM   1   BY   2
                      UNTIL    IDA    >    18
              IF   WRK-HENSYU-KINGAKU2(IDA:2)  =  "０"
                    IF  (FLG-HENSYUKIN = ZERO)
                    AND (IDA       NOT =  17 )
                       CONTINUE
                    ELSE
                       MOVE  1   TO  FLG-HENSYUKIN
                       ADD   1   TO  IDB
                       MOVE  WRK-HENSYU-KINGAKU2(IDA:2)  TO
                                 WRK-HENSYU-KINGAKU3(IDB:2)
                       ADD   1   TO  IDB
                    END-IF
              ELSE
                    MOVE  1   TO  FLG-HENSYUKIN
                    ADD   1   TO  IDB
                    MOVE  WRK-HENSYU-KINGAKU2(IDA:2)  TO
                              WRK-HENSYU-KINGAKU3(IDB:2)
                    ADD   1   TO  IDB
              END-IF
           END-PERFORM
      *
           COMPUTE  IDA  =  ( IDB  /  2 )  +  1
           COMPUTE  WRK-RECE65XX-YUKOKETA(1 1)  =
                    WRK-RECE65XX-YUKOKETA(1 1)  +  IDA
      *
           MOVE     WRK-HENSYU-KINGAKU3(1:IDB)
                                   TO  WRK-RECE65XX-NAME(1 1)(21:IDB)
           COMPUTE  IDA  =  21  +  IDB
           MOVE    "円"            TO  WRK-RECE65XX-NAME(1 1)(IDA:2)
      *
           MOVE    "1"             TO  WRK-RECE65XX-RECEPTFLG(1 1)
      *
           MOVE    WRK-RECE65XX-SRYACT-TBL(1 1)
                                   TO  RECE659-SRYACT-TBL(1)
           WRITE   RECE659-REC
           ADD     1               TO  CNT-RECE659
      *
           .
       2001-KOHSAIKEI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    妊婦のコメント編集
      *****************************************************************
       2001-NINPU-COMMENT-SEC          SECTION.
      *
           PERFORM 500-RECE64-KEY-HENSYU-SEC
           MOVE    99999960            TO  RECE649-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE649-KOHBUNRI
           MOVE    "99"                TO  RECE649-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE649-HKNCOMBI
           MOVE    1                   TO  RECE649-MEISAISU
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           PERFORM 500-RECE65-KEY-HENSYU-SEC
           MOVE    99999960            TO  RECE659-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE659-KOHBUNRI
           MOVE    "99"                TO  RECE659-SRYKBN
      *    コメントの挿入
           MOVE    SPACE               TO  WRK-RECE65XX-REC
           INITIALIZE                      WRK-RECE65XX-REC
      *    「妊婦」
           MOVE    "820100348"         TO  WRK-TENSU-SRYCD
           MOVE    RECE62-SRYYM        TO  WRK-TENSU-SRYYM
           PERFORM 900-TENSU-TBL-SEC
      *
           MOVE    TNS-SRYCD           TO  WRK-RECE65XX-SRYCD(1 1)
           MOVE    ZERO                TO  WRK-RECE65XX-SRYSURYO(1 1)
           MOVE    TNS-YUKOKETA        TO  WRK-RECE65XX-YUKOKETA(1 1)
           MOVE    TNS-NAME            TO  WRK-RECE65XX-NAME(1 1)
                                           WRK-RECE65XX-INPUTCOMENT(1 1)
           MOVE    WRK-RECE65XX-SRYACT-TBL(1 1)
                                       TO  RECE659-SRYACT-TBL(1)
           WRITE   RECE659-REC
           ADD     1                   TO  CNT-RECE659
      *
           .
       2001-NINPU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    発症日があるか判定
      *****************************************************************
       2001-HASSYO-HANTEI-SEC          SECTION.
      *
           PERFORM     2002-RIHA-HENSYU-SEC
      *
           IF   TBL-HASSYO-KAITATU-FLG   =   1
             MOVE      ZERO            TO   FLG-COMOKIKAE
             MOVE      CONS-KYUSEI     TO   WRK-SRYCD
             PERFORM   2001-KYUSEIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-SINDAI-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO-SINDAI-COMMENT(IDX14)   =  1
                 MOVE      ZERO            TO   FLG-COMOKIKAE
                 MOVE      CONS-SINDAI     TO   WRK-SRYCD
                 MOVE      CONS-SINDAI-E   TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO-SINDAI-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO-SINYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      ***
           IF   TBL-HASSYO-KOKYUKI-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO-KOKYUKI-COMMENT(IDX14)   =  1
                 MOVE      ZERO            TO   FLG-COMOKIKAE
                 MOVE      CONS-KOKYUKI    TO   WRK-SRYCD
                 MOVE      CONS-KOKYUKI-E  TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO-KOKYUKI-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO-KOKYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      ***
           IF   TBL-HASSYO-NOUKE-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO-NOUKE-COMMENT(IDX14)   =  1
                 MOVE      1               TO   FLG-COMOKIKAE
                 MOVE      CONS-NOUKE      TO   WRK-SRYCD
                 MOVE      CONS-NOUKE-E    TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO-NOUKE-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO-NOUYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      ***
           IF   TBL-HASSYO-UNDOKI-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO-UNDOKI-COMMENT(IDX14)   =  1
                 MOVE      1               TO   FLG-COMOKIKAE
                 MOVE      CONS-UNDOKI     TO   WRK-SRYCD
                 MOVE      CONS-UNDOKI-E   TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO-UNDOKI-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO-UNDYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      ***
           IF   TBL-HASSYO-NANBYO-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO-NANBYO-COMMENT(IDX14)   =  1
                 MOVE      1               TO   FLG-COMOKIKAE
                 MOVE      CONS-NANBYO     TO   WRK-SRYCD
                 MOVE      CONS-NANBYO-E   TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO-NANBYO-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO-NANYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      ***
           IF   TBL-HASSYO-SYOGAI-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO-SYOGAI-COMMENT(IDX14)   =  1
                 MOVE      ZERO            TO   FLG-COMOKIKAE
                 MOVE      CONS-SYOGAI     TO   WRK-SRYCD
                 MOVE      CONS-SYOGAI-E   TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO-SYOGAI-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO-SYOYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      ***
           IF   TBL-HASSYO-SESSYOKU-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO-SESSYOKU-COMMENT(IDX14)   =  1
                 MOVE      ZERO            TO   FLG-COMOKIKAE
                 MOVE      CONS-SESSYOKU   TO   WRK-SRYCD
                 MOVE      CONS-SESSYOKU-E TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO-SESSYOKU-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO-SESYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      ***
           IF   TBL-HASSYO-HAIYO-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO-HAIYO-COMMENT(IDX14)   =  1
                 MOVE      1               TO   FLG-COMOKIKAE
                 MOVE      CONS-HAIYO      TO   WRK-SRYCD
                 MOVE      CONS-HAIYO-E    TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO-HAIYO-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO-HAIYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *####
           IF   TBL-HASSYO9-SINDAI-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO9-SINDAI-COMMENT(IDX14)   =  1
                 MOVE      1               TO   FLG-COMOKIKAE
                 MOVE      CONS-SINDAI-K   TO   WRK-SRYCD
                 MOVE      "999999999"     TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO9-SINDAI-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO9-SINYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      ***
           IF   TBL-HASSYO9-KOKYUKI-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO9-KOKYUKI-COMMENT(IDX14)   =  1
                 MOVE      1               TO   FLG-COMOKIKAE
                 MOVE      CONS-KOKYUKI-K  TO   WRK-SRYCD
                 MOVE      "999999999"     TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO9-KOKYUKI-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO9-KOKYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      ***
           IF   TBL-HASSYO9-HAIYO-FLG   =   1 OR 3
             PERFORM   VARYING   IDX14    FROM   1   BY   1
                        UNTIL    IDX14      >    5
               IF   TBL-HASSYO9-HAIYO-COMMENT(IDX14)   =  1
                 MOVE      1               TO   FLG-COMOKIKAE
                 MOVE      CONS-HAIYO-K    TO   WRK-SRYCD
                 MOVE      "999999999"     TO   WRK-SRYCD2
                 MOVE      TBL-HASSYO9-HAIYO-DAY-T2(IDX14)
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
                 MOVE      TBL-HASSYO9-HAIYMD1-DAY-T2(IDX14)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2001X-HASSYO-HENSYU-SEC
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
           PERFORM  2002-DAISAN-HASSYO-HANTEI-SEC
      *
           .
       2001-HASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ編集
      *****************************************************************
       2002-RIHA-HENSYU-SEC             SECTION.
      *
           IF   TBL-HASSYO-SINDAI-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE62-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE62-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-SINDAI(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-SINDAI2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-SINDAI2-COMMENT
                                                      (IDX22)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE62-RIHA2-SINDAI(IDX22)    =  1
                 AND TBL-HASSYO-SINYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE62-RIHA2-SINDAI(IDX22)
                              TO  TBL-HASSYO-SINYMD2-FLG(IDX22)
                   MOVE  RECE62-RIHA2-SINYMD-OCC1(IDX22)
                              TO  TBL-HASSYO-SINYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE62-RIHA2-SINYMD-OCC2(IDX22)
                              TO  TBL-HASSYO-SINYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE62-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-SINDAI(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-SINDAI-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-SINDAI-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE62-RIHA2-SINDAI(IDX22)           =  1
                 AND TBL-HASSYO-SINYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE62-RIHA2-SINDAI(IDX22)
                           TO  TBL-HASSYO-SINYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-SINYMD-OCC1(IDX22)
                           TO  TBL-HASSYO-SINYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-SINYMD-OCC2(IDX22)
                           TO  TBL-HASSYO-SINYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO-SINDAI-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO-SINDAI-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO-SINDAI-FLG
             END-EVALUATE
           END-IF
      *
           IF   TBL-HASSYO-KOKYUKI-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE62-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE62-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-KOKYUKI(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-KOKYUKI2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-KOKYUKI2-COMMENT
                                                      (IDX22)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE62-RIHA2-KOKYUKI(IDX22)   =  1
                 AND TBL-HASSYO-KOKYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE62-RIHA2-KOKYUKI(IDX22)
                              TO  TBL-HASSYO-KOKYMD2-FLG(IDX22)
                   MOVE  RECE62-RIHA2-KOKYMD-OCC1(IDX22)
                              TO  TBL-HASSYO-KOKYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE62-RIHA2-KOKYMD-OCC2(IDX22)
                              TO  TBL-HASSYO-KOKYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE62-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-KOKYUKI(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-KOKYUKI-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-KOKYUKI-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE62-RIHA2-KOKYUKI(IDX22)          =  1
                 AND TBL-HASSYO-KOKYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE62-RIHA2-KOKYUKI(IDX22)
                           TO  TBL-HASSYO-KOKYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-KOKYMD-OCC1(IDX22)
                           TO  TBL-HASSYO-KOKYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-KOKYMD-OCC2(IDX22)
                           TO  TBL-HASSYO-KOKYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO-KOKYUKI-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO-KOKYUKI-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO-KOKYUKI-FLG
             END-EVALUATE
           END-IF
      *
           IF   TBL-HASSYO-NOUKE-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE62-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE62-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-NOUKE(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-NOUKE2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-NOUKE2-COMMENT
                                                      (IDX22)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE62-RIHA2-NOUKE(IDX22)     =  1
                 AND TBL-HASSYO-NOUYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE62-RIHA2-NOUKE(IDX22)
                              TO  TBL-HASSYO-NOUYMD2-FLG(IDX22)
                   MOVE  RECE62-RIHA2-NOUYMD-OCC1(IDX22)
                              TO  TBL-HASSYO-NOUYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE62-RIHA2-NOUYMD-OCC2(IDX22)
                              TO  TBL-HASSYO-NOUYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE62-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-NOUKE(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-NOUKE-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-NOUKE-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE62-RIHA2-NOUKE(IDX22)            =  1
                 AND TBL-HASSYO-NOUYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE62-RIHA2-NOUKE(IDX22)
                           TO  TBL-HASSYO-NOUYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-NOUYMD-OCC1(IDX22)
                           TO  TBL-HASSYO-NOUYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-NOUYMD-OCC2(IDX22)
                           TO  TBL-HASSYO-NOUYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO-NOUKE-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO-NOUKE-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO-NOUKE-FLG
             END-EVALUATE
           END-IF
      *
           IF   TBL-HASSYO-UNDOKI-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE62-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE62-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-UNDOKI(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-UNDOKI2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-UNDOKI2-COMMENT
                                                      (IDX22)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE62-RIHA2-UNDOKI(IDX22)    =  1
                 AND TBL-HASSYO-UNDYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE62-RIHA2-UNDOKI(IDX22)
                              TO  TBL-HASSYO-UNDYMD2-FLG(IDX22)
                   MOVE  RECE62-RIHA2-UNDYMD-OCC1(IDX22)
                              TO  TBL-HASSYO-UNDYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE62-RIHA2-UNDYMD-OCC2(IDX22)
                              TO  TBL-HASSYO-UNDYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE62-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-UNDOKI(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-UNDOKI-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-UNDOKI-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE62-RIHA2-UNDOKI(IDX22)           =  1
                 AND TBL-HASSYO-UNDYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE62-RIHA2-UNDOKI(IDX22)
                           TO  TBL-HASSYO-UNDYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-UNDYMD-OCC1(IDX22)
                           TO  TBL-HASSYO-UNDYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-UNDYMD-OCC2(IDX22)
                           TO  TBL-HASSYO-UNDYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO-UNDOKI-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO-UNDOKI-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO-UNDOKI-FLG
             END-EVALUATE
           END-IF
      *
           IF   TBL-HASSYO-NANBYO-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE62-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE62-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-NANBYO(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-NANBYO2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-NANBYO2-COMMENT
                                                      (IDX22)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE62-RIHA2-NANBYO(IDX22)    =  1
                 AND TBL-HASSYO-NANYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE62-RIHA2-NANBYO(IDX22)
                              TO  TBL-HASSYO-NANYMD2-FLG(IDX22)
                   MOVE  RECE62-RIHA2-NANYMD-OCC1(IDX22)
                              TO  TBL-HASSYO-NANYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE62-RIHA2-NANYMD-OCC2(IDX22)
                              TO  TBL-HASSYO-NANYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE62-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-NANBYO(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-NANBYO-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-NANBYO-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE62-RIHA2-NANBYO(IDX22)           =  1
                 AND TBL-HASSYO-NANYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE62-RIHA2-NANBYO(IDX22)
                           TO  TBL-HASSYO-NANYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-NANYMD-OCC1(IDX22)
                           TO  TBL-HASSYO-NANYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-NANYMD-OCC2(IDX22)
                           TO  TBL-HASSYO-NANYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO-NANBYO-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO-NANBYO-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO-NANBYO-FLG
             END-EVALUATE
           END-IF
      *
           IF   TBL-HASSYO-SYOGAI-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE62-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE62-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-SYOGAI(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-SYOGAI2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-SYOGAI2-COMMENT
                                                      (IDX22)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE62-RIHA2-SYOGAI(IDX22)    =  1
                 AND TBL-HASSYO-SYOYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE62-RIHA2-SYOGAI(IDX22)
                              TO  TBL-HASSYO-SYOYMD2-FLG(IDX22)
                   MOVE  RECE62-RIHA2-SYOYMD-OCC1(IDX22)
                              TO  TBL-HASSYO-SYOYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE62-RIHA2-SYOYMD-OCC2(IDX22)
                              TO  TBL-HASSYO-SYOYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE62-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-SYOGAI(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-SYOGAI-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-SYOGAI-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE62-RIHA2-SYOGAI(IDX22)           =  1
                 AND TBL-HASSYO-SYOYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE62-RIHA2-SYOGAI(IDX22)
                           TO  TBL-HASSYO-SYOYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-SYOYMD-OCC1(IDX22)
                           TO  TBL-HASSYO-SYOYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-SYOYMD-OCC2(IDX22)
                           TO  TBL-HASSYO-SYOYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO-SYOGAI-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO-SYOGAI-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO-SYOGAI-FLG
             END-EVALUATE
           END-IF
      *
           IF   TBL-HASSYO-SESSYOKU-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE62-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE62-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-SESSYOKU(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-SESSYOKU2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-SESSYOKU2-COMMENT
                                                      (IDX22)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE62-RIHA2-SESSYOKU(IDX22)  =  1
                 AND TBL-HASSYO-SESYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE62-RIHA2-SESSYOKU(IDX22)
                              TO  TBL-HASSYO-SESYMD2-FLG(IDX22)
                   MOVE  RECE62-RIHA2-SESYMD-OCC1(IDX22)
                              TO  TBL-HASSYO-SESYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE62-RIHA2-SESYMD-OCC2(IDX22)
                              TO  TBL-HASSYO-SESYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE62-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-SESSYOKU(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-SESSYOKU-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-SESSYOKU-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE62-RIHA2-SESSYOKU(IDX22)         =  1
                 AND TBL-HASSYO-SESYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE62-RIHA2-SESSYOKU(IDX22)
                           TO  TBL-HASSYO-SESYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-SESYMD-OCC1(IDX22)
                           TO  TBL-HASSYO-SESYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-SESYMD-OCC2(IDX22)
                           TO  TBL-HASSYO-SESYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO-SESSYOKU-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO-SESSYOKU-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO-SESSYOKU-FLG
             END-EVALUATE
           END-IF
      *
           IF   TBL-HASSYO-HAIYO-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE62-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE62-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-HAIYO(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-HAIYO2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-HAIYO2-COMMENT
                                                      (IDX22)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE62-RIHA2-HAIYO(IDX22)     =  1
                 AND TBL-HASSYO-HAIYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE62-RIHA2-HAIYO(IDX22)
                              TO  TBL-HASSYO-HAIYMD2-FLG(IDX22)
                   MOVE  RECE62-RIHA2-HAIYMD-OCC1(IDX22)
                              TO  TBL-HASSYO-HAIYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE62-RIHA2-HAIYMD-OCC2(IDX22)
                              TO  TBL-HASSYO-HAIYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE62-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-HAIYO(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO-HAIYO-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO-HAIYO-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE62-RIHA2-HAIYO(IDX22)            =  1
                 AND TBL-HASSYO-HAIYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE62-RIHA2-HAIYO(IDX22)
                           TO  TBL-HASSYO-HAIYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-HAIYMD-OCC1(IDX22)
                           TO  TBL-HASSYO-HAIYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-HAIYMD-OCC2(IDX22)
                           TO  TBL-HASSYO-HAIYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO-HAIYO-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO-HAIYO-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO-HAIYO-FLG
             END-EVALUATE
           END-IF
      *
      *****
      *
           IF   TBL-HASSYO9-SINDAI-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE62-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE62-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-SINDAI9(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO9-SINDAI2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO9-SINDAI2-COMMENT
                                                      (IDX22)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO9-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE62-RIHA2-SINDAI9(IDX22)    =  1
                 AND TBL-HASSYO9-SINYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE62-RIHA2-SINDAI9(IDX22)
                              TO  TBL-HASSYO9-SINYMD2-FLG(IDX22)
                   MOVE  RECE62-RIHA2-SIN9YMD-OCC1(IDX22)
                              TO  TBL-HASSYO9-SINYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE62-RIHA2-SIN9YMD-OCC2(IDX22)
                              TO  TBL-HASSYO9-SINYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE62-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-SINDAI9(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO9-SINDAI-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO9-SINDAI-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO9-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE62-RIHA2-SINDAI9(IDX22)           =  1
                 AND TBL-HASSYO9-SINYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE62-RIHA2-SINDAI9(IDX22)
                          TO  TBL-HASSYO9-SINYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-SIN9YMD-OCC1(IDX22)
                          TO  TBL-HASSYO9-SINYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-SIN9YMD-OCC2(IDX22)
                          TO  TBL-HASSYO9-SINYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO9-SINDAI-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO9-SINDAI-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO9-SINDAI-FLG
             END-EVALUATE
           END-IF
      *
           IF   TBL-HASSYO9-KOKYUKI-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE62-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE62-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-KOKYUKI9(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO9-KOKYUKI2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO9-KOKYUKI2-COMMENT
                                                      (IDX22)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO9-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE62-RIHA2-KOKYUKI9(IDX22)   =  1
                 AND TBL-HASSYO9-KOKYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE62-RIHA2-KOKYUKI9(IDX22)
                              TO  TBL-HASSYO9-KOKYMD2-FLG(IDX22)
                   MOVE  RECE62-RIHA2-KOK9YMD-OCC1(IDX22)
                              TO  TBL-HASSYO9-KOKYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE62-RIHA2-KOK9YMD-OCC2(IDX22)
                              TO  TBL-HASSYO9-KOKYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE62-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-KOKYUKI9(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO9-KOKYUKI-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO9-KOKYUKI-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO9-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE62-RIHA2-KOKYUKI9(IDX22)          =  1
                 AND TBL-HASSYO9-KOKYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE62-RIHA2-KOKYUKI9(IDX22)
                          TO  TBL-HASSYO9-KOKYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-KOK9YMD-OCC1(IDX22)
                          TO  TBL-HASSYO9-KOKYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-KOK9YMD-OCC2(IDX22)
                          TO  TBL-HASSYO9-KOKYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO9-KOKYUKI-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO9-KOKYUKI-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO9-KOKYUKI-FLG
             END-EVALUATE
           END-IF
      *
           IF   TBL-HASSYO9-HAIYO-FLG   =   9
             MOVE    ZERO       TO  FLG-DAISAN1
             MOVE    ZERO       TO  FLG-DAISAN2
             PERFORM  VARYING  IDX22  FROM  1  BY  1
                       UNTIL  (IDX22   >   10                )
                        OR    (RECE62-HKNCOMBI(IDX22) =  ZERO)
               IF  RECE62-DAISANFLG(IDX22) =  1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-HAIYO9(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO9-HAIYO2-SANTEI
                                                 (IDX22 IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO9-HAIYO2-COMMENT
                                                      (IDX22)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO9-HKNCOMBI2(IDX22)
                   MOVE    1  TO  FLG-DAISAN1
                 END-IF
                 IF  RECE62-RIHA2-HAIYO9(IDX22)     =  1
                 AND TBL-HASSYO9-HAIYMD2-FLG(IDX22) = ZERO
                   MOVE  RECE62-RIHA2-HAIYO9(IDX22)
                              TO  TBL-HASSYO9-HAIYMD2-FLG(IDX22)
                   MOVE  RECE62-RIHA2-HAI9YMD-OCC1(IDX22)
                              TO  TBL-HASSYO9-HAIYMD2-DAY-OCC1(IDX22)
                   MOVE  RECE62-RIHA2-HAI9YMD-OCC2(IDX22)
                              TO  TBL-HASSYO9-HAIYMD2-DAY-OCC2(IDX22)
                 END-IF
               ELSE
                 COMPUTE WRK-KOHBUNRI = RECE62-RIHA-KOHBUNRI(IDX22)
                                      + 1
                 MOVE     ZERO   TO   FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >   31
                  IF   RECE62-RIHA-HAIYO9(IDX22 IDX1)  NOT = ZERO
                    MOVE   1  TO  TBL-HASSYO9-HAIYO-SANTEI
                                                (WRK-KOHBUNRI IDX1)
                    MOVE   1  TO  FLG-ARI
                  END-IF
                 END-PERFORM
                 IF  FLG-ARI = 1
                   MOVE    1  TO  TBL-HASSYO9-HAIYO-COMMENT
                                                     (WRK-KOHBUNRI)
                   MOVE  RECE62-HKNCOMBI(IDX22)
                              TO  TBL-HASSYO9-HKNCOMBI(WRK-KOHBUNRI)
                   MOVE    1  TO  FLG-DAISAN2
                 END-IF
                 IF  RECE62-RIHA2-HAIYO9(IDX22)            =  1
                 AND TBL-HASSYO9-HAIYMD1-FLG(WRK-KOHBUNRI) = ZERO
                   MOVE  RECE62-RIHA2-HAIYO9(IDX22)
                          TO  TBL-HASSYO9-HAIYMD1-FLG(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-HAI9YMD-OCC1(IDX22)
                          TO  TBL-HASSYO9-HAIYMD1-DAY-OCC1(WRK-KOHBUNRI)
                   MOVE  RECE62-RIHA2-HAI9YMD-OCC2(IDX22)
                          TO  TBL-HASSYO9-HAIYMD1-DAY-OCC2(WRK-KOHBUNRI)
                 END-IF
               END-IF
             END-PERFORM
             EVALUATE   FLG-DAISAN1   ALSO   FLG-DAISAN2
               WHEN         1         ALSO       1
                 MOVE    3    TO    TBL-HASSYO9-HAIYO-FLG
               WHEN         1         ALSO       0
                 MOVE    2    TO    TBL-HASSYO9-HAIYO-FLG
               WHEN         0         ALSO       1
                 MOVE    1    TO    TBL-HASSYO9-HAIYO-FLG
             END-EVALUATE
           END-IF
      *
           .
       2002-RIHA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    発症日編集
      *****************************************************************
       2001X-HASSYO-HENSYU-SEC          SECTION.
      *
           IF  WRK-RIHASANTEI-YMD3 NOT = SPACE
             MOVE     ZERO   TO  FLG-RIHA
             MOVE     WRK-RIHASANTEI-YMD3 TO  WRK-KYUSEIYMD
             PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
             IF   RECE62-SRYYM >= "201905"
               PERFORM  20011-HASSYO-COM-201905-SEC
             ELSE
               PERFORM  20011-HASSYO-COMMENT-SEC
             END-IF
           END-IF
      *
           IF  WRK-RIHASANTEI-YMD  NOT = SPACE
             MOVE     1      TO  FLG-RIHA
             MOVE     WRK-RIHASANTEI-YMD  TO  WRK-KYUSEIYMD
             PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
             IF   RECE62-SRYYM >= "201905"
               PERFORM  20011-HASSYO-COM-201905-SEC
             ELSE
               PERFORM  20011-HASSYO-COMMENT-SEC
             END-IF
           END-IF
           PERFORM   VARYING   IDX16    FROM   1   BY   1
                      UNTIL    IDX16      >   31
             IF  TB-RIHASANTEI-DAY(IDX16)  NOT = ZERO
               MOVE   2      TO  FLG-RIHA
               MOVE   RECE62-SRYYM        TO  WRK-KYUSEIYMD(1:6)
               MOVE   IDX16               TO  WRK-KYUSEIYMD(7:2)
               PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
               IF   RECE62-SRYYM >= "201905"
                 PERFORM  20011-HASSYO-COM-201905-SEC
               ELSE
                 PERFORM  20011-HASSYO-COMMENT-SEC
               END-IF
             END-IF
           END-PERFORM
      *
           .
       2001X-HASSYO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    発症日があるか判定（第三者行為）
      *****************************************************************
       2002-DAISAN-HASSYO-HANTEI-SEC          SECTION.
      *
           PERFORM  VARYING  IDX22  FROM  1  BY  1
                     UNTIL   IDX22   >   10
      *
             IF   TBL-HASSYO-SINDAI-FLG   =   2 OR 3
               IF  TBL-HASSYO-SINDAI2-COMMENT(IDX22)  =  1
                 MOVE      ZERO            TO   FLG-COMOKIKAE
                 MOVE      CONS-SINDAI     TO   WRK-SRYCD
                 MOVE      CONS-SINDAI-E   TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO-SINDAI2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO-SINYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
             IF   TBL-HASSYO-KOKYUKI-FLG   =   2 OR 3
               IF  TBL-HASSYO-KOKYUKI2-COMMENT(IDX22)  =  1
                 MOVE      ZERO            TO   FLG-COMOKIKAE
                 MOVE      CONS-KOKYUKI    TO   WRK-SRYCD
                 MOVE      CONS-KOKYUKI-E  TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO-KOKYUKI2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO-KOKYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
             IF   TBL-HASSYO-NOUKE-FLG   =   2 OR 3
               IF  TBL-HASSYO-NOUKE2-COMMENT(IDX22)  =  1
                 MOVE      1               TO   FLG-COMOKIKAE
                 MOVE      CONS-NOUKE      TO   WRK-SRYCD
                 MOVE      CONS-NOUKE-E    TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO-NOUKE2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO-NOUYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
             IF   TBL-HASSYO-UNDOKI-FLG   =   2 OR 3
               IF  TBL-HASSYO-UNDOKI2-COMMENT(IDX22)  =  1
                 MOVE      1                TO   FLG-COMOKIKAE
                 MOVE      CONS-UNDOKI      TO   WRK-SRYCD
                 MOVE      CONS-UNDOKI-E    TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO-UNDOKI2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO-UNDYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
             IF   TBL-HASSYO-NANBYO-FLG   =   2 OR 3
               IF  TBL-HASSYO-NANBYO2-COMMENT(IDX22)  =  1
                 MOVE      1                TO   FLG-COMOKIKAE
                 MOVE      CONS-NANBYO      TO   WRK-SRYCD
                 MOVE      CONS-NANBYO-E    TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO-NANBYO2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO-NANYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
             IF   TBL-HASSYO-SYOGAI-FLG   =   2 OR 3
               IF  TBL-HASSYO-SYOGAI2-COMMENT(IDX22)  =  1
                 MOVE      ZERO             TO   FLG-COMOKIKAE
                 MOVE      CONS-SYOGAI      TO   WRK-SRYCD
                 MOVE      CONS-SYOGAI-E    TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO-SYOGAI2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO-SYOYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
             IF   TBL-HASSYO-SESSYOKU-FLG   =   2 OR 3
               IF  TBL-HASSYO-SESSYOKU2-COMMENT(IDX22)  =  1
                 MOVE      ZERO               TO   FLG-COMOKIKAE
                 MOVE      CONS-SESSYOKU      TO   WRK-SRYCD
                 MOVE      CONS-SESSYOKU-E    TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO-SESSYOKU2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO-SESYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
             IF   TBL-HASSYO-HAIYO-FLG   =   2 OR 3
               IF  TBL-HASSYO-HAIYO2-COMMENT(IDX22)  =  1
                 MOVE      1                TO   FLG-COMOKIKAE
                 MOVE      CONS-HAIYO       TO   WRK-SRYCD
                 MOVE      CONS-HAIYO-E     TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO-HAIYO2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO-HAIYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
      *****
      *
             IF   TBL-HASSYO9-SINDAI-FLG   =   2 OR 3
               IF  TBL-HASSYO9-SINDAI2-COMMENT(IDX22)  =  1
                 MOVE      1               TO   FLG-COMOKIKAE
                 MOVE      CONS-SINDAI-K   TO   WRK-SRYCD
                 MOVE      "999999999"     TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO9-SINDAI2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO9-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO9-SINYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
             IF   TBL-HASSYO9-KOKYUKI-FLG   =   2 OR 3
               IF  TBL-HASSYO9-KOKYUKI2-COMMENT(IDX22)  =  1
                 MOVE      1               TO   FLG-COMOKIKAE
                 MOVE      CONS-KOKYUKI-K  TO   WRK-SRYCD
                 MOVE      "999999999"     TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO9-KOKYUKI2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO9-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO9-KOKYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
             IF   TBL-HASSYO9-HAIYO-FLG   =   2 OR 3
               IF  TBL-HASSYO9-HAIYO2-COMMENT(IDX22)  =  1
                 MOVE      1                TO   FLG-COMOKIKAE
                 MOVE      CONS-HAIYO-K     TO   WRK-SRYCD
                 MOVE      "999999999"      TO   WRK-SRYCD2
                 MOVE  TBL-HASSYO9-HAIYO2-DAY-OCC(IDX22)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
                 MOVE  TBL-HASSYO9-HKNCOMBI2(IDX22)
                                       TO   WRK-RIHA-HKNCOMBI
                 MOVE  TBL-HASSYO9-HAIYMD2-DAY-T2(IDX22)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                 PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
                 IF        FLG-KYUSEI-COMMENT    =      1
                   PERFORM   2002X-HASSYO-HENSYU2-SEC
                 END-IF
               END-IF
             END-IF
      *
           END-PERFORM
      *
           .
       2002-DAISAN-HASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    発症日編集（第三者行為）
      *****************************************************************
       2002X-HASSYO-HENSYU2-SEC          SECTION.
      *
           IF  WRK-RIHASANTEI-YMD3 NOT = SPACE
             MOVE     ZERO   TO  FLG-RIHA
             MOVE     WRK-RIHASANTEI-YMD3 TO  WRK-KYUSEIYMD
             MOVE     ZERO   TO  FLG-CHECK-OK
             PERFORM      20011-KYUSEIYMD-CHECK-SEC
             IF   FLG-CHECK-OK = 1
               PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
               IF   RECE62-SRYYM >= "201905"
                 PERFORM  20011-HASSYO-COM2-201905-SEC
               ELSE
                 PERFORM  20011-HASSYO-COMMENT2-SEC
               END-IF
             END-IF
           END-IF
      *
           IF  WRK-RIHASANTEI-YMD  NOT = SPACE
             MOVE     1      TO  FLG-RIHA
             MOVE     WRK-RIHASANTEI-YMD  TO  WRK-KYUSEIYMD
             MOVE     ZERO   TO  FLG-CHECK-OK
             PERFORM      20011-KYUSEIYMD-CHECK-SEC
             IF   FLG-CHECK-OK = 1
               PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
               IF   RECE62-SRYYM >= "201905"
                 PERFORM  20011-HASSYO-COM2-201905-SEC
               ELSE
                 PERFORM  20011-HASSYO-COMMENT2-SEC
               END-IF
             END-IF
           END-IF
           PERFORM   VARYING   IDX16    FROM   1   BY   1
                      UNTIL    IDX16      >   31
             IF  TB-RIHASANTEI-DAY(IDX16)  NOT = ZERO
               MOVE   2      TO  FLG-RIHA
               MOVE   RECE62-SRYYM        TO  WRK-KYUSEIYMD(1:6)
               MOVE   IDX16               TO  WRK-KYUSEIYMD(7:2)
               MOVE   ZERO   TO  FLG-CHECK-OK
               PERFORM    20011-KYUSEIYMD-CHECK-SEC
               IF   FLG-CHECK-OK = 1
                 PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                 IF   RECE62-SRYYM >= "201905"
                   PERFORM  20011-HASSYO-COM2-201905-SEC
                 ELSE
                   PERFORM  20011-HASSYO-COMMENT2-SEC
                 END-IF
               END-IF
             END-IF
           END-PERFORM
      *
           .
       2002X-HASSYO-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    脳血管疾患急性発症日があるか判定
      *    脳血管疾患急性発症日の１８０日以内か判定
      *****************************************************************
       2001-KYUSEIHASSYO-HANTEI-SEC          SECTION.
      *
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT2
           MOVE     ZERO     TO     WRK-KYUSEIYMD
      *
      *    脳血管疾患急性発症日
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE62-PTID         TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      *
                MOVE    "tbl_santei"        TO      MCP-TABLE
                MOVE    "key3"              TO      MCP-PATHNAME
                PERFORM        900-SANTEI-READ-SEC
      *
                PERFORM   UNTIL  ( FLG-SANTEI         =   1 ) OR
                                 ( FLG-KYUSEI-COMMENT =   1 )
      *
                   PERFORM  VARYING   IDX3   FROM  31  BY  -1
                             UNTIL   ( IDX3               <    1 )
                               OR    ( FLG-KYUSEI-COMMENT =    1 )
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                         MOVE   SANTEI-SRYYM   TO  WRK-FSANTEIYMD(1:6)
                         MOVE   IDX3           TO  WRK-FSANTEIYMD(7:2)
      *
                         INITIALIZE                  STS-AREA-DAY
                         INITIALIZE                  LNK-DAY6-AREA
                         MOVE    "61"            TO  LNK-DAY6-IRAI
                         MOVE    WRK-FSANTEIYMD  TO  LNK-DAY6-KIJUN
                         MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                         MOVE     179            TO  LNK-DAY6-ZOGEN
                         CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                         LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN TO  WRK-AFTER180
      *
                         IF   WRK-FSANTEIYMD(1:6) <=  RECE62-SRYYM
                         AND  RECE62-SRYYM        <=  WRK-AFTER180(1:6)
                               MOVE    1    TO   FLG-KYUSEI-COMMENT
                               MOVE  WRK-FSANTEIYMD
                                            TO   WRK-KYUSEIYMD
                         END-IF
      *
                     END-IF
      *
                   END-PERFORM
      *
                   MOVE    "tbl_santei"        TO      MCP-TABLE
                   MOVE    "key3"              TO      MCP-PATHNAME
                   PERFORM        900-SANTEI-READ-SEC
      *
                END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_santei"        TO      MCP-TABLE
           MOVE    "key3"              TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    脳血管疾患急性発症終了日
      *
           IF      FLG-KYUSEI-COMMENT    =      1
      *       算定履歴検索
              INITIALIZE                  SANTEI-REC
              MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
              MOVE    RECE62-PTID         TO  SANTEI-PTID
              MOVE    CONS-KYUSEI-END     TO  SANTEI-SRYCD
              MOVE    ZERO                TO  SANTEI-NYUGAIKBN
              MOVE    ZERO                TO  SANTEI-SRYKA
              MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
              MOVE    SANTEI-REC          TO  MCPDATA-REC
              MOVE    "tbl_santei"        TO      MCP-TABLE
              MOVE    "key3"              TO      MCP-PATHNAME
              PERFORM   900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
      *
                   MOVE    "tbl_santei"        TO      MCP-TABLE
                   MOVE    "key3"              TO      MCP-PATHNAME
                   PERFORM        900-SANTEI-READ-SEC
      *
                   PERFORM   UNTIL ( FLG-SANTEI          =   1         )
                              OR   ( SANTEI-SRYYM  < WRK-KYUSEIYMD(1:6))
                              OR   ( FLG-KYUSEI-COMMENT2 =   1         )
      *
                      PERFORM  VARYING   IDX3   FROM  31  BY  -1
                             UNTIL ( IDX3                  <    1      )
                              OR   ( FLG-KYUSEI-COMMENT2   =    1      )
      *
                        IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                           MOVE   SANTEI-SRYYM   TO  WRK-FSANTEIYMD(1:6)
                           MOVE   IDX3           TO  WRK-FSANTEIYMD(7:2)
      *
                           IF   WRK-KYUSEIYMD        <  WRK-FSANTEIYMD
                           AND  WRK-FSANTEIYMD(1:6)  <  RECE62-SRYYM
                                  MOVE    1    TO   FLG-KYUSEI-COMMENT2
                           END-IF
      *
                        END-IF
      *
                      END-PERFORM
      *
                      MOVE    "tbl_santei"        TO      MCP-TABLE
                      MOVE    "key3"              TO      MCP-PATHNAME
                      PERFORM        900-SANTEI-READ-SEC
      *
                   END-PERFORM
      *
              END-IF
      *
              MOVE    "tbl_santei"        TO      MCP-TABLE
              MOVE    "key3"              TO      MCP-PATHNAME
              PERFORM   900-CLOSE-SEC
           END-IF
      *
           IF      FLG-KYUSEI-COMMENT    =      1
           AND     FLG-KYUSEI-COMMENT2   =      ZERO
               MOVE       ZERO            TO      WRK-RIHA-HKNCOMBI
               PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
               IF   RECE62-SRYYM      >=    "200604"
                   PERFORM   VARYING   IDX14    FROM   1   BY   1
                              UNTIL    IDX14      >    5
                      IF   TBL-HASSYO-KAITATU-COMMENT(IDX14)   =  1
                          MOVE       WRK-KYUSEIYMD(7:2)  TO  IDX16
                          IF   RECE62-SRYYM >= "201905"
                            PERFORM  20011-HASSYO-COM-201905-SEC
                          ELSE
                            PERFORM  20011-HASSYO-COMMENT-SEC
                          END-IF
                      END-IF
                   END-PERFORM
               ELSE
                   PERFORM    20011-KYUSEIHASSYO-COMMENT-SEC
               END-IF
           END-IF
      *
           .
       2001-KYUSEIHASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    脳血管疾患急性発症日等の検索
      *****************************************************************
       20011-KYUSEIHASSYO-KENSAKU-SEC          SECTION.
      *
           MOVE    ZERO                  TO  FLG-KYUSEI-OK
           MOVE    SPACE                 TO  WRK-KYUSEI-NYUGAIKBN
           MOVE    SPACE                 TO  WRK-KYUSEI-SRYKA
           MOVE    ZERO                  TO  WRK-KYUSEI-ZAINUM
           MOVE    ZERO                  TO  FLG-COMOKIKAE2
           MOVE    ZERO                  TO  FLG-COMARI
           MOVE    ZERO                  TO  FLG-COMARI2
      *
           MOVE    ZERO                  TO  IDZ
           INITIALIZE                        WRK-FREE-AREA
      *++++
           PERFORM   VARYING   IDX15    FROM   1   BY   1
                      UNTIL   (IDX15          >    2)
                       OR     (FLG-KYUSEI-OK  =    1)
      *
           INITIALIZE                        SRYACT-REC
           MOVE    RECE62-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    IDX15                 TO  SRY-NYUGAIKBN
           MOVE    RECE62-PTID           TO  SRY-PTID
           MOVE    WRK-KYUSEIYMD(1:6)    TO  SRY-SRYYM
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (1)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (2)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (3)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (4)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (5)
           MOVE    SRYACT-REC            TO  MCPDATA-REC
           MOVE    "tbl_sryact"          TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"      TO     MCP-TABLE
               MOVE    "key7"            TO     MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL   FLG-SRYACT     =   1
                         OR     FLG-KYUSEI-OK  =   1
                 INITIALIZE                     SRYACCT-REC
                 MOVE    SRY-HOSPNUM      TO    ACCT-HOSPNUM
                 MOVE    SRY-NYUGAIKBN    TO    ACCT-NYUGAIKBN
                 MOVE    SRY-PTID         TO    ACCT-PTID
                 MOVE    SRY-SRYKA        TO    ACCT-SRYKA
                 MOVE    SRY-SRYYM        TO    ACCT-SRYYM
                 MOVE    SRY-ZAINUM       TO    ACCT-ZAINUM
                 MOVE    SRYACCT-REC      TO    MCPDATA-REC
                 MOVE    "tbl_sryacct"    TO    MCP-TABLE
                 MOVE    "key31"          TO    MCP-PATHNAME
                 PERFORM   900-DBSELECT-SEC
                 IF     MCP-RC            =  ZERO
                    MOVE    "tbl_sryacct"    TO    MCP-TABLE
                    MOVE    "key31"          TO    MCP-PATHNAME
                    PERFORM   900-DBFETCH-SEC
                    IF      MCP-RC        =       ZERO
                        MOVE    MCPDATA-REC  TO  SRYACCT-REC
      *
                        MOVE    WRK-KYUSEIYMD(7:2)  TO  DAY-IDX2
                        IF   ACCT-DAY(1 DAY-IDX2)  NOT =  ZERO
      *                   保険組合せマスタの検索
                          INITIALIZE                 HKNCOMBI-REC
                          MOVE    ACCT-HOSPNUM   TO  COMB-HOSPNUM
                          MOVE    ACCT-PTID      TO  COMB-PTID
                          MOVE    ACCT-HKNCOMBI  TO  COMB-HKNCOMBINUM
                          MOVE    HKNCOMBI-REC   TO  MCPDATA-REC
                          MOVE    "tbl_hkncombi" TO  MCP-TABLE
                          MOVE    "key"          TO  MCP-PATHNAME
                          PERFORM   900-DBSELECT-SEC
                          IF      MCP-RC         =       ZERO
                             MOVE    "tbl_hkncombi" TO  MCP-TABLE
                             MOVE    "key"          TO  MCP-PATHNAME
                             PERFORM   900-DBFETCH-SEC
                             IF      MCP-RC        =       ZERO
                               MOVE   MCPDATA-REC  TO   HKNCOMBI-REC
                               IF   WRK-RIHA-HKNCOMBI = ZERO
                                 IF  (COMB-HKNNUM = "971" OR "973"
                                                          OR "975")
                                 OR  (COMB-KOH1HKNNUM = "970"     )
                                     CONTINUE
                                 ELSE
                                   MOVE   1   TO  FLG-KYUSEI-OK
                                   MOVE   ACCT-NYUGAIKBN
                                              TO  WRK-KYUSEI-NYUGAIKBN
                                   MOVE   ACCT-SRYKA
                                              TO  WRK-KYUSEI-SRYKA
                                   MOVE   ACCT-ZAINUM
                                              TO  WRK-KYUSEI-ZAINUM
                                 END-IF
                               ELSE
                                 IF WRK-RIHA-HKNCOMBI =
                                                    COMB-HKNCOMBINUM
                                   MOVE   1   TO  FLG-KYUSEI-OK
                                   MOVE   ACCT-NYUGAIKBN
                                              TO  WRK-KYUSEI-NYUGAIKBN
                                   MOVE   ACCT-SRYKA
                                              TO  WRK-KYUSEI-SRYKA
                                   MOVE   ACCT-ZAINUM
                                              TO  WRK-KYUSEI-ZAINUM
                                 END-IF
                               END-IF
                             END-IF
                          END-IF
                          INITIALIZE                   MCPAREA
                          MOVE    "tbl_hkncombi" TO  MCP-TABLE
                          MOVE    "key"          TO  MCP-PATHNAME
                          PERFORM   900-CLOSE-SEC
      *
                        END-IF
                    END-IF
                 END-IF
                 INITIALIZE                     MCPAREA
                 MOVE    "tbl_sryacct"    TO    MCP-TABLE
                 MOVE    "key31"          TO    MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
      *
                 MOVE    "tbl_sryact"          TO      MCP-TABLE
                 MOVE    "key7"                TO      MCP-PATHNAME
                 PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
           INITIALIZE                        MCPAREA
           MOVE    "tbl_sryact"          TO      MCP-TABLE
           MOVE    "key7"                TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           END-PERFORM
      *++++
           IF      WRK-KYUSEI-ZAINUM    NOT =    ZERO
              INITIALIZE                           SRYACT-REC
              MOVE    RECE62-HOSPNUM         TO    SRY-HOSPNUM
              MOVE    WRK-KYUSEI-NYUGAIKBN   TO    SRY-NYUGAIKBN
              MOVE    RECE62-PTID            TO    SRY-PTID
              MOVE    WRK-KYUSEI-SRYKA       TO    SRY-SRYKA
              MOVE    WRK-KYUSEIYMD(1:6)     TO    SRY-SRYYM
              MOVE    WRK-KYUSEI-ZAINUM      TO    SRY-ZAINUM
              MOVE    SRYACT-REC             TO    MCPDATA-REC
              MOVE    "tbl_sryact"           TO    MCP-TABLE
              MOVE    "key2"                 TO    MCP-PATHNAME
              PERFORM   900-DBSELECT-SEC
              IF     MCP-RC            =  ZERO
                 MOVE    "tbl_sryact"        TO    MCP-TABLE
                 MOVE    "key2"              TO    MCP-PATHNAME
                 PERFORM  900-SRYACT-READ-SEC
                 PERFORM  UNTIL   FLG-SRYACT     =   1
                   PERFORM  VARYING  IDX12  FROM  1  BY  1
                             UNTIL  (IDX12             >    5   )  OR
                                    (SRY-SRYCD(IDX12)  =   SPACE)
                      IF  ((SRY-SRYCD(IDX12)(1:1)     =   "8"  )  OR
                           (SRY-SRYCD(IDX12)(1:3)     =   "008"))
                      AND ( SRY-SRYCD(IDX12)(1:4) NOT =
                                              "0085" AND "0086" )
                       IF   SRY-INPUTNUM(IDX12)   NOT =   ZERO
                         INITIALIZE                      PTCOM-REC
                         MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
                         MOVE    SRY-PTID            TO  PTCOM-PTID
                         MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
                         MOVE    SRY-SRYCD(IDX12)    TO  PTCOM-SRYCD
                         MOVE    SRY-INPUTNUM(IDX12) TO  PTCOM-RENNUM
                         PERFORM 900-PTCOM-READ-SEC
                         ADD     1     TO    IDZ
                         MOVE    PTCOM-INPUTCOMENT
                                                TO  WK-FREE-NAME(IDZ)
                         PERFORM   VARYING   IDX13   FROM   300   BY  -1
                                    UNTIL    IDX13    <    1
                           IF  WK-FREE-NAME(IDZ)(IDX13:1)  NOT =  SPACE
                             COMPUTE  WK-FREE-YUKOKETA(IDZ)  =
                                      IDX13    /    2
                             MOVE     1     TO      IDX13
                           END-IF
                         END-PERFORM
                       ELSE
                         MOVE  SRY-SRYCD(IDX12) TO  WRK-TENSU-SRYCD
                         MOVE  SRY-SRYYM        TO  WRK-TENSU-SRYYM
                         PERFORM      900-TENSU-TBL-SEC
                         ADD     1           TO  IDZ
                         MOVE    TNS-NAME    TO  WK-FREE-NAME(IDZ)
                         PERFORM   VARYING   IDX13   FROM   300   BY  -1
                                    UNTIL    IDX13    <    1
                           IF  WK-FREE-NAME(IDZ)(IDX13:1)  NOT =  SPACE
                             COMPUTE  WK-FREE-YUKOKETA(IDZ)  =
                                      IDX13    /    2
                             MOVE     1     TO      IDX13
                           END-IF
                         END-PERFORM
                       END-IF
                       IF   FLG-COMOKIKAE  =   1
                          EVALUATE  SRY-SRYCD(IDX12)
                            WHEN     "840000042"
                              IF   FLG-COMOKIKAE2 = ZERO
                                 MOVE    1    TO  FLG-COMOKIKAE2
                                 MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                                 MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                                 COMPUTE  IDZ  =  IDZ - 1
                              END-IF
                            WHEN     "840000101"
                              IF   FLG-COMOKIKAE2 = ZERO
                                 MOVE    2    TO  FLG-COMOKIKAE2
                                 MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                                 MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                                 COMPUTE  IDZ  =  IDZ - 1
                              END-IF
                            WHEN     "840000058"
                              IF   FLG-COMOKIKAE2 = ZERO
                                 MOVE    3    TO  FLG-COMOKIKAE2
                                 MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                                 MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                                 COMPUTE  IDZ  =  IDZ - 1
                              END-IF
                          END-EVALUATE
                       END-IF
                       IF   FLG-COMOKIKAE  =   1
                          IF  SRY-SRYCD(IDX12) = "840000042" OR
                                                 "840000101" OR
                                                 "840000058"
                            MOVE    1    TO   FLG-COMARI2
                          ELSE
                            MOVE    1    TO   FLG-COMARI
                          END-IF
                       ELSE
                            MOVE    1    TO   FLG-COMARI
                       END-IF
                      END-IF
                   END-PERFORM
                   MOVE    "tbl_sryact"      TO    MCP-TABLE
                   MOVE    "key2"            TO    MCP-PATHNAME
                   PERFORM   900-SRYACT-READ-SEC
                 END-PERFORM
              END-IF
              INITIALIZE                     MCPAREA
              MOVE    "tbl_sryact"      TO    MCP-TABLE
              MOVE    "key2"            TO    MCP-PATHNAME
              PERFORM   900-CLOSE-SEC
           END-IF
      *++++
           IF      FLG-COMARI    =    ZERO
              INITIALIZE                      SANTEIPLUS-REC
              MOVE    RECE62-HOSPNUM      TO  SANTEIPLUS-HOSPNUM
              MOVE    RECE62-PTID         TO  SANTEIPLUS-PTID
              MOVE    WRK-KYUSEIYMD(1:6)  TO  SANTEIPLUS-SRYYM
              MOVE    WRK-SRYCD           TO  SANTEIPLUS-SRYCD
              MOVE    ZERO                TO  SANTEIPLUS-NYUGAIKBN
              MOVE    ZERO                TO  SANTEIPLUS-SRYKA
              MOVE    WRK-RIHA-HKNCOMBI   TO  SANTEIPLUS-HKNCOMBINUM
              MOVE    WRK-KYUSEIYMD(7:2)  TO  SANTEIPLUS-DAYKEY
              MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
              MOVE    "tbl_santeiplus"    TO  MCP-TABLE
              MOVE    "key2"              TO  MCP-PATHNAME
              PERFORM 900-DBSELECT-SEC
              IF      MCP-RC               =       ZERO
                 MOVE    "tbl_santeiplus"     TO  MCP-TABLE
                 MOVE    "key2"               TO  MCP-PATHNAME
                 PERFORM  900-SANTEIPLUS-READ-SEC
                 PERFORM  UNTIL   FLG-SANTEIPLUS     =   1
      *            リハコメントの終了日と診療月をチェック
      *            （履歴番号＞０のデータ）
                   IF     SANTEIPLUS-RENNUM   NOT =  ZERO
                    IF  ( SANTEIPLUS-ENDYMD       =  SPACE        )
                    OR  ((SANTEIPLUS-ENDYMD   NOT =  SPACE       ) AND
                         (SANTEIPLUS-ENDYMD(1:6) >=  RECE62-SRYYM))
                     IF  SANTEIPLUS-COMMENT  NOT =  SPACE
                      ADD     1                 TO  IDZ
                      MOVE  SANTEIPLUS-COMMENT  TO  WK-FREE-NAME(IDZ)
                      PERFORM   VARYING   IDX13   FROM   300   BY  -1
                                 UNTIL    IDX13    <     1
                        IF  WK-FREE-NAME(IDZ)(IDX13:1)  NOT =  SPACE
                          COMPUTE  WK-FREE-YUKOKETA(IDZ)  =
                                   IDX13    /    2
                          MOVE     1     TO      IDX13
                        END-IF
                      END-PERFORM
                     END-IF
                     IF   FLG-COMOKIKAE  =    1
                     AND  FLG-COMARI2    =   ZERO
                      IF   SANTEIPLUS-COMMENT(1:8)  = "手術　日"
                        IF   FLG-COMOKIKAE2 = ZERO
                           MOVE    1    TO  FLG-COMOKIKAE2
                           MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                           MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                           COMPUTE  IDZ  =  IDZ - 1
                        END-IF
                      END-IF
                      IF   SANTEIPLUS-COMMENT(1:12) = "急性増悪　日"
                        IF   FLG-COMOKIKAE2 = ZERO
                           MOVE    2    TO  FLG-COMOKIKAE2
                           MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                           MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                           COMPUTE  IDZ  =  IDZ - 1
                        END-IF
                      END-IF
                      IF   SANTEIPLUS-COMMENT(1:8) = "診断　日"
                        IF   FLG-COMOKIKAE2 = ZERO
                           MOVE    3    TO  FLG-COMOKIKAE2
                           MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                           MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                           COMPUTE  IDZ  =  IDZ - 1
                        END-IF
                      END-IF
                     END-IF
                    END-IF
                   END-IF
                   MOVE    "tbl_santeiplus"     TO  MCP-TABLE
                   MOVE    "key2"               TO  MCP-PATHNAME
                   PERFORM  900-SANTEIPLUS-READ-SEC
                 END-PERFORM
              END-IF
              MOVE    "tbl_santeiplus"     TO  MCP-TABLE
              MOVE    "key2"               TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
       20011-KYUSEIHASSYO-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    脳血管疾患急性発症日のコメント編集
      *****************************************************************
       20011-KYUSEIHASSYO-COMMENT-SEC          SECTION.
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO        TO  IDX-RECE65
           MOVE    SPACE       TO  WRK-RECE65XX-REC
           INITIALIZE              WRK-RECE65XX-REC
      *
           ADD     1           TO  IDX-RECE65
           MOVE    "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
           MOVE    ZERO        TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
           MOVE    9           TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
           MOVE    "発症　　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
           MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(7:4)
           MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(13:4)
      *
           PERFORM   VARYING    IDX12   FROM    1   BY  1
                      UNTIL   ( IDX12               >   49    )
                       OR     ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1       TO  IDX-RECE65
                MOVE   "810000001"
                               TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                               TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                MOVE   WK-FREE-NAME(IDX12)
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
           END-PERFORM
      *
      *    科・日付の編集を含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
               PERFORM 500-RECE65-KEY-HENSYU-SEC
                MOVE    99999996             TO  RECE659-ZAINUM
                MOVE    WRK-KOHBUNRI-JIDO    TO  RECE659-KOHBUNRI
                MOVE    "99"                 TO  RECE659-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE659-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE65   =    1)
                   ADD         1             TO    IDX-RECE65
                   IF (WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE)
                   OR (IDX-RECE65                       >  100 )
                       MOVE    1             TO    FLG-WRKRECE65
                   ELSE
                       MOVE  WRK-RECE65XX-SRYACT-TBL (IDX-RECE65 1)
                                        TO   RECE659-SRYACT-TBL (IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE659-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE659-REC
                   ADD     1            TO   CNT-RECE659
                END-IF
           END-PERFORM
      *
      *
           PERFORM 500-RECE64-KEY-HENSYU-SEC
           MOVE    99999996            TO  RECE649-ZAINUM
           MOVE    WRK-KOHBUNRI-JIDO   TO  RECE649-KOHBUNRI
           MOVE    "99"                TO  RECE649-SRYKBN
           MOVE    WRK-HKNCOMBI-JIDO   TO  RECE649-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           .
       20011-KYUSEIHASSYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    発症日のコメント編集
      *****************************************************************
       20011-HASSYO-COMMENT-SEC          SECTION.
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO        TO  IDX-RECE65
           MOVE    SPACE       TO  WRK-RECE65XX-REC
           INITIALIZE              WRK-RECE65XX-REC
      *
           ADD     1           TO  IDX-RECE65
           MOVE    "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
           MOVE    ZERO        TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
      *
           EVALUATE      WRK-SRYCD
             WHEN       CONS-KYUSEI
               MOVE    19   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE    "脳血管疾患急性発症　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
               MOVE    21   TO  IDC1
               MOVE    27   TO  IDC2
               MOVE    33   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               COMPUTE WRK-ZAINUM    =  99980000
                                     + (IDX16 + (IDX14 * 31))
               MOVE    "40" TO  WRK-SRYKBN
             WHEN       CONS-SINDAI
               MOVE    23   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE    "心大血管疾患リハ：治療開始　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
               MOVE    29   TO  IDC1
               MOVE    35   TO  IDC2
               MOVE    41   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980200 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980200 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980200
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SINDAI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    22  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE  "早期・初期リハ加算：発症　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    22  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE  "早期・初期リハ加算：手術　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    24  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE  "早期・初期リハ加算：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980400 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980400 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980400
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-NOUKE
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    21  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE    "脳血管疾患等リハ：発症　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    21  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE    "脳血管疾患等リハ：手術　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    23  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE
                       "脳血管疾患等リハ：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    29  TO  IDC1
                   MOVE    35  TO  IDC2
                   MOVE    41  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       3
                   MOVE    21  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE    "脳血管疾患等リハ：診断　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980600 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980600 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980600
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-HAIYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       1
                 WHEN       3
                   MOVE    20  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE    "廃用症候群リハ：診断　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    23  TO  IDC1
                   MOVE    29  TO  IDC2
                   MOVE    35  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    22  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE
                       "廃用症候群リハ：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980800 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980800 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980800
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-HAIYO-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    22  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE  "早期・初期リハ加算：発症　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    22  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE  "早期・初期リハ加算：手術　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    24  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE  "早期・初期リハ加算：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981000 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981000 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981000
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-UNDOKI
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    18  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE    "運動器リハ：発症　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    19  TO  IDC1
                   MOVE    25  TO  IDC2
                   MOVE    31  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    18  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE    "運動器リハ：手術　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    19  TO  IDC1
                   MOVE    25  TO  IDC2
                   MOVE    31  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    20  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE    "運動器リハ：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    23  TO  IDC1
                   MOVE    29  TO  IDC2
                   MOVE    35  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       3
                   MOVE    18  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE    "運動器リハ：診断　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    19  TO  IDC1
                   MOVE    25  TO  IDC2
                   MOVE    31  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981200 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981200 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981200
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-KOKYUKI
               MOVE    20   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE    "呼吸器リハ：治療開始　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
               MOVE    23   TO  IDC1
               MOVE    29   TO  IDC2
               MOVE    35   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981400 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981400 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981400
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-KOKYUKI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    22  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE  "早期・初期リハ加算：発症　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    22  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE  "早期・初期リハ加算：手術　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    24  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE  "早期・初期リハ加算：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981600 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981600 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981600
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SESSYOKU
               MOVE    21   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE    "摂食機能療法：治療開始　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
               MOVE    25   TO  IDC1
               MOVE    31   TO  IDC2
               MOVE    37   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981800 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981800 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981800
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-NANBYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    19  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE    "難病患者リハ：発症　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    21  TO  IDC1
                   MOVE    27  TO  IDC2
                   MOVE    33  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    19  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE    "難病患者リハ：手術　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    21  TO  IDC1
                   MOVE    27  TO  IDC2
                   MOVE    33  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    21  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE    "難病患者リハ：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99982000 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99982000 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99982000
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SYOGAI
               MOVE    21   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE    "障害児（者）リハ：発症　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
               MOVE    25   TO  IDC1
               MOVE    31   TO  IDC2
               MOVE    37   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99982200 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99982200 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99982200
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
           END-EVALUATE
      *
           PERFORM   VARYING    IDX12   FROM    1   BY  1
                      UNTIL   ( IDX12               >   49    )
                       OR     ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1       TO  IDX-RECE65
                MOVE   "810000001"
                               TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                               TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                MOVE   WK-FREE-NAME(IDX12)
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
           END-PERFORM
      *
      *    科・日付の編集を含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
               PERFORM 500-RECE65-KEY-HENSYU-SEC
                MOVE    WRK-ZAINUM           TO  RECE659-ZAINUM
                COMPUTE WRK-KOHBUNRI  =  IDX14  -  1
                MOVE    WRK-KOHBUNRI         TO  RECE659-KOHBUNRI
                MOVE    WRK-SRYKBN           TO  RECE659-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE659-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE65   =    1)
                   ADD         1             TO    IDX-RECE65
                   IF (WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE)
                   OR (IDX-RECE65                       >  100 )
                       MOVE    1             TO    FLG-WRKRECE65
                   ELSE
                       MOVE  WRK-RECE65XX-SRYACT-TBL (IDX-RECE65 1)
                                        TO   RECE659-SRYACT-TBL (IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE659-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE659-REC
                   ADD     1            TO   CNT-RECE659
                END-IF
           END-PERFORM
      *
      *
           PERFORM 500-RECE64-KEY-HENSYU-SEC
           MOVE    WRK-ZAINUM          TO  RECE649-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX14  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE649-KOHBUNRI
           MOVE    WRK-SRYKBN          TO  RECE649-SRYKBN
           MOVE    TBL-HASSYO-HKNCOMBI(IDX14)
                                       TO  RECE649-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           .
       20011-HASSYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    発症日のコメント編集（第三者行為）
      *****************************************************************
       20011-HASSYO-COMMENT2-SEC          SECTION.
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO        TO  IDX-RECE65
           MOVE    SPACE       TO  WRK-RECE65XX-REC
           INITIALIZE              WRK-RECE65XX-REC
      *
           ADD     1           TO  IDX-RECE65
           MOVE    "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
           MOVE    ZERO        TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
           EVALUATE      WRK-SRYCD
             WHEN       CONS-SINDAI
               IF  RECE62-DAISAN-TOKKI  =  ZERO
                 MOVE  30   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "（第三者行為）"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE  "心大血管疾患リハ：治療開始　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                 MOVE  43   TO  IDC1
                 MOVE  49   TO  IDC2
                 MOVE  55   TO  IDC3
               ELSE
                 MOVE  23   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "心大血管疾患リハ：治療開始　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                 MOVE  29   TO  IDC1
                 MOVE  35   TO  IDC2
                 MOVE  41   TO  IDC3
               END-IF
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99983000 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99983000 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99983000
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SINDAI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  29  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE "早期・初期リハ加算：発症　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  41  TO  IDC1
                     MOVE  47  TO  IDC2
                     MOVE  53  TO  IDC3
                   ELSE
                     MOVE  22  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "早期・初期リハ加算：発症　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  27  TO  IDC1
                     MOVE  33  TO  IDC2
                     MOVE  39  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  29  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE "早期・初期リハ加算：手術　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  41  TO  IDC1
                     MOVE  47  TO  IDC2
                     MOVE  53  TO  IDC3
                   ELSE
                     MOVE  22  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "早期・初期リハ加算：手術　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  27  TO  IDC1
                     MOVE  33  TO  IDC2
                     MOVE  39  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  31  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE "早期・初期リハ加算：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  45  TO  IDC1
                     MOVE  51  TO  IDC2
                     MOVE  57  TO  IDC3
                   ELSE
                     MOVE  24  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "早期・初期リハ加算：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  31  TO  IDC1
                     MOVE  37  TO  IDC2
                     MOVE  43  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99983400 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99983400 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99983400
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-NOUKE
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  28  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE  "脳血管疾患等リハ：発症　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  39  TO  IDC1
                     MOVE  45  TO  IDC2
                     MOVE  51  TO  IDC3
                   ELSE
                     MOVE  21  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "脳血管疾患等リハ：発症　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  25  TO  IDC1
                     MOVE  31  TO  IDC2
                     MOVE  37  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  28  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE  "脳血管疾患等リハ：手術　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  39  TO  IDC1
                     MOVE  45  TO  IDC2
                     MOVE  51  TO  IDC3
                   ELSE
                     MOVE  21  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "脳血管疾患等リハ：手術　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  25  TO  IDC1
                     MOVE  31  TO  IDC2
                     MOVE  37  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  30  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE
                       "脳血管疾患等リハ：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  43  TO  IDC1
                     MOVE  49  TO  IDC2
                     MOVE  55  TO  IDC3
                   ELSE
                     MOVE  23  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE
                       "脳血管疾患等リハ：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  29  TO  IDC1
                     MOVE  35  TO  IDC2
                     MOVE  41  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       3
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  28  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE  "脳血管疾患等リハ：診断　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  39  TO  IDC1
                     MOVE  45  TO  IDC2
                     MOVE  51  TO  IDC3
                   ELSE
                     MOVE  21  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "脳血管疾患等リハ：診断　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  25  TO  IDC1
                     MOVE  31  TO  IDC2
                     MOVE  37  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99983800 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99983800 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99983800
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-HAIYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       1
                 WHEN       3
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  27  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE  "廃用症候群リハ：診断　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  37  TO  IDC1
                     MOVE  43  TO  IDC2
                     MOVE  49  TO  IDC3
                   ELSE
                     MOVE  20  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "廃用症候群リハ：診断　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  23  TO  IDC1
                     MOVE  29  TO  IDC2
                     MOVE  35  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  29  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE
                       "廃用症候群リハ：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  41  TO  IDC1
                     MOVE  47  TO  IDC2
                     MOVE  53  TO  IDC3
                   ELSE
                     MOVE  22  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE
                       "廃用症候群リハ：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  27  TO  IDC1
                     MOVE  33  TO  IDC2
                     MOVE  39  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99984200 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99984200 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99984200
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-HAIYO-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  29  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE "早期・初期リハ加算：発症　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  41  TO  IDC1
                     MOVE  47  TO  IDC2
                     MOVE  53  TO  IDC3
                   ELSE
                     MOVE  22  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "早期・初期リハ加算：発症　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  27  TO  IDC1
                     MOVE  33  TO  IDC2
                     MOVE  39  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  29  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE "早期・初期リハ加算：手術　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  41  TO  IDC1
                     MOVE  47  TO  IDC2
                     MOVE  53  TO  IDC3
                   ELSE
                     MOVE  22  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "早期・初期リハ加算：手術　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  27  TO  IDC1
                     MOVE  33  TO  IDC2
                     MOVE  39  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  31  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE "早期・初期リハ加算：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  45  TO  IDC1
                     MOVE  51  TO  IDC2
                     MOVE  57  TO  IDC3
                   ELSE
                     MOVE  24  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "早期・初期リハ加算：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  31  TO  IDC1
                     MOVE  37  TO  IDC2
                     MOVE  43  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99984600 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99984600 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99984600
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-UNDOKI
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  25  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE  "運動器リハ：発症　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  33  TO  IDC1
                     MOVE  39  TO  IDC2
                     MOVE  45  TO  IDC3
                   ELSE
                     MOVE  18  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "運動器リハ：発症　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  19  TO  IDC1
                     MOVE  25  TO  IDC2
                     MOVE  31  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  25  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE  "運動器リハ：手術　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  33  TO  IDC1
                     MOVE  39  TO  IDC2
                     MOVE  45  TO  IDC3
                   ELSE
                     MOVE  18  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "運動器リハ：手術　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  19  TO  IDC1
                     MOVE  25  TO  IDC2
                     MOVE  31  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  27  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE  "運動器リハ：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  37  TO  IDC1
                     MOVE  43  TO  IDC2
                     MOVE  49  TO  IDC3
                   ELSE
                     MOVE  20  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "運動器リハ：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  23  TO  IDC1
                     MOVE  29  TO  IDC2
                     MOVE  35  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       3
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  25  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE  "運動器リハ：診断　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  33  TO  IDC1
                     MOVE  39  TO  IDC2
                     MOVE  45  TO  IDC3
                   ELSE
                     MOVE  18  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "運動器リハ：診断　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  19  TO  IDC1
                     MOVE  25  TO  IDC2
                     MOVE  31  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99985000 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99985000 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99985000
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-KOKYUKI
               IF  RECE62-DAISAN-TOKKI  =  ZERO
                 MOVE  27   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "（第三者行為）"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE  "呼吸器リハ：治療開始　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                 MOVE  37   TO  IDC1
                 MOVE  43   TO  IDC2
                 MOVE  49   TO  IDC3
               ELSE
                 MOVE  20   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "呼吸器リハ：治療開始　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                 MOVE  23   TO  IDC1
                 MOVE  29   TO  IDC2
                 MOVE  35   TO  IDC3
               END-IF
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99985400 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99985400 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99985400
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-KOKYUKI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  29  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE "早期・初期リハ加算：発症　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  41  TO  IDC1
                     MOVE  47  TO  IDC2
                     MOVE  53  TO  IDC3
                   ELSE
                     MOVE  22  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "早期・初期リハ加算：発症　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  27  TO  IDC1
                     MOVE  33  TO  IDC2
                     MOVE  39  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  29  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE "早期・初期リハ加算：手術　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  41  TO  IDC1
                     MOVE  47  TO  IDC2
                     MOVE  53  TO  IDC3
                   ELSE
                     MOVE  22  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "早期・初期リハ加算：手術　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  27  TO  IDC1
                     MOVE  33  TO  IDC2
                     MOVE  39  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  31  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE "早期・初期リハ加算：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  45  TO  IDC1
                     MOVE  51  TO  IDC2
                     MOVE  57  TO  IDC3
                   ELSE
                     MOVE  24  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "早期・初期リハ加算：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  31  TO  IDC1
                     MOVE  37  TO  IDC2
                     MOVE  43  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99985800 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99985800 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99985800
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SESSYOKU
               IF  RECE62-DAISAN-TOKKI  =  ZERO
                 MOVE  28   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "（第三者行為）"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE  "摂食機能療法：治療開始　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                 MOVE  39   TO  IDC1
                 MOVE  45   TO  IDC2
                 MOVE  51   TO  IDC3
               ELSE
                 MOVE  21   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "摂食機能療法：治療開始　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                 MOVE  25   TO  IDC1
                 MOVE  31   TO  IDC2
                 MOVE  37   TO  IDC3
               END-IF
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99986200 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99986200 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99986200
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-NANBYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  26  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE  "難病患者リハ：発症　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  35  TO  IDC1
                     MOVE  41  TO  IDC2
                     MOVE  47  TO  IDC3
                   ELSE
                     MOVE  19  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "難病患者リハ：発症　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  21  TO  IDC1
                     MOVE  27  TO  IDC2
                     MOVE  33  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  26  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE  "難病患者リハ：手術　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  35  TO  IDC1
                     MOVE  41  TO  IDC2
                     MOVE  47  TO  IDC3
                   ELSE
                     MOVE  19  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "難病患者リハ：手術　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  21  TO  IDC1
                     MOVE  27  TO  IDC2
                     MOVE  33  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  28  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE  "難病患者リハ：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  39  TO  IDC1
                     MOVE  45  TO  IDC2
                     MOVE  51  TO  IDC3
                   ELSE
                     MOVE  21  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE  "難病患者リハ：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  25  TO  IDC1
                     MOVE  31  TO  IDC2
                     MOVE  37  TO  IDC3
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99986600 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99986600 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99986600
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SYOGAI
               IF  RECE62-DAISAN-TOKKI  =  ZERO
                 MOVE  28   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "（第三者行為）"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE  "障害児（者）リハ：発症　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                 MOVE  39   TO  IDC1
                 MOVE  45   TO  IDC2
                 MOVE  51   TO  IDC3
               ELSE
                 MOVE  21   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "障害児（者）リハ：発症　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                 MOVE  25   TO  IDC1
                 MOVE  31   TO  IDC2
                 MOVE  37   TO  IDC3
               END-IF
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99987000 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99987000 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99987000
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
           END-EVALUATE
      *
           PERFORM   VARYING    IDX12   FROM    1   BY  1
                      UNTIL   ( IDX12               >   49    )
                       OR     ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1       TO  IDX-RECE65
                MOVE   "810000001"
                               TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                               TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                MOVE   WK-FREE-NAME(IDX12)
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
           END-PERFORM
      *
      *    科・日付の編集を含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
               PERFORM 500-RECE65-KEY-HENSYU-SEC
                MOVE    WRK-ZAINUM           TO  RECE659-ZAINUM
                COMPUTE WRK-KOHBUNRI  =  WRK-KOHBUNRI-JIDO
                MOVE    WRK-KOHBUNRI         TO  RECE659-KOHBUNRI
                MOVE    WRK-SRYKBN           TO  RECE659-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE659-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE65   =    1)
                   ADD         1             TO    IDX-RECE65
                   IF (WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE)
                   OR (IDX-RECE65                       >  100 )
                       MOVE    1             TO    FLG-WRKRECE65
                   ELSE
                       MOVE  WRK-RECE65XX-SRYACT-TBL (IDX-RECE65 1)
                                        TO   RECE659-SRYACT-TBL (IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE659-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE659-REC
                   ADD     1            TO   CNT-RECE659
                END-IF
           END-PERFORM
      *
      *
           PERFORM 500-RECE64-KEY-HENSYU-SEC
           MOVE    WRK-ZAINUM          TO  RECE649-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  WRK-KOHBUNRI-JIDO
           MOVE    WRK-KOHBUNRI        TO  RECE649-KOHBUNRI
           MOVE    WRK-SRYKBN          TO  RECE649-SRYKBN
           MOVE    TBL-HASSYO-HKNCOMBI2(IDX22)
                                       TO  RECE649-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           .
       20011-HASSYO-COMMENT2-EXT.
           EXIT.
      *
      *****************************************************************
      *    発症日のコメント編集
      *****************************************************************
       20011-HASSYO-COM-201905-SEC          SECTION.
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO        TO  IDX-RECE65
           MOVE    SPACE       TO  WRK-RECE65XX-REC
           INITIALIZE              WRK-RECE65XX-REC
      *
           ADD     1           TO  IDX-RECE65
           MOVE    "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
           MOVE    ZERO        TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
      *
           EVALUATE      WRK-SRYCD
             WHEN       CONS-KYUSEI
               MOVE    21   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE "脳血管疾患急性発症　　　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
               MOVE    25   TO  IDC1
               MOVE    31   TO  IDC2
               MOVE    37   TO  IDC3
               MOVE    21   TO  IDC4
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               COMPUTE WRK-ZAINUM    =  99980000
                                     + (IDX16 + (IDX14 * 31))
               MOVE    "40" TO  WRK-SRYKBN
             WHEN       CONS-SINDAI
               MOVE    25   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE "心大血管疾患リハ：治療開始　　　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
               MOVE    33   TO  IDC1
               MOVE    39   TO  IDC2
               MOVE    45   TO  IDC3
               MOVE    29   TO  IDC4
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980200 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980200 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980200
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SINDAI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    24  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "早期・初期リハ加算：発症　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    24  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "早期・初期リハ加算：手術　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    26  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
             MOVE "早期・初期リハ加算：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    35  TO  IDC1
                   MOVE    41  TO  IDC2
                   MOVE    47  TO  IDC3
                   MOVE    31  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980400 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980400 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980400
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-NOUKE
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    23  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE "脳血管疾患等リハ：発症　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    29  TO  IDC1
                   MOVE    35  TO  IDC2
                   MOVE    41  TO  IDC3
                   MOVE    25  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    23  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE "脳血管疾患等リハ：手術　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    29  TO  IDC1
                   MOVE    35  TO  IDC2
                   MOVE    41  TO  IDC3
                   MOVE    25  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    25  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE "脳血管疾患等リハ：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    33  TO  IDC1
                   MOVE    39  TO  IDC2
                   MOVE    45  TO  IDC3
                   MOVE    29  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       3
                   MOVE    23  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE "脳血管疾患等リハ：診断　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    29  TO  IDC1
                   MOVE    35  TO  IDC2
                   MOVE    41  TO  IDC3
                   MOVE    25  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980600 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980600 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980600
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-HAIYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       1
                 WHEN       3
                   MOVE    22  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE "廃用症候群リハ：診断　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   MOVE    23  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    24  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE "廃用症候群リハ：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980800 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980800 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980800
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-HAIYO-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    24  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE "早期・初期リハ加算：発症　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    24  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE "早期・初期リハ加算：手術　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    26  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
           MOVE "早期・初期リハ加算：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    35  TO  IDC1
                   MOVE    41  TO  IDC2
                   MOVE    47  TO  IDC3
                   MOVE    31  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981000 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981000 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981000
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-UNDOKI
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    20  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE "運動器リハ：発症　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    23  TO  IDC1
                   MOVE    29  TO  IDC2
                   MOVE    35  TO  IDC3
                   MOVE    19  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    20  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE "運動器リハ：手術　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    23  TO  IDC1
                   MOVE    29  TO  IDC2
                   MOVE    35  TO  IDC3
                   MOVE    19  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    22  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE "運動器リハ：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   MOVE    23  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       3
                   MOVE    20  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE "運動器リハ：診断　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    23  TO  IDC1
                   MOVE    29  TO  IDC2
                   MOVE    35  TO  IDC3
                   MOVE    19  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981200 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981200 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981200
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-KOKYUKI
               MOVE    22   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE "呼吸器リハ：治療開始　　　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
               MOVE    27   TO  IDC1
               MOVE    33   TO  IDC2
               MOVE    39   TO  IDC3
               MOVE    23   TO  IDC4
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981400 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981400 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981400
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-KOKYUKI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    24  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "早期・初期リハ加算：発症　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    24  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "早期・初期リハ加算：手術　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    26  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
             MOVE "早期・初期リハ加算：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    35  TO  IDC1
                   MOVE    41  TO  IDC2
                   MOVE    47  TO  IDC3
                   MOVE    31  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981600 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981600 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981600
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SESSYOKU
               MOVE    23   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE "摂食機能療法：治療開始　　　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
               MOVE    29   TO  IDC1
               MOVE    35   TO  IDC2
               MOVE    41   TO  IDC3
               MOVE    25   TO  IDC4
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981800 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981800 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981800
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-NANBYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    21  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE "難病患者リハ：発症　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   MOVE    21  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    21  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE "難病患者リハ：手術　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   MOVE    21  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    23  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE "難病患者リハ：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                   MOVE    29  TO  IDC1
                   MOVE    35  TO  IDC2
                   MOVE    41  TO  IDC3
                   MOVE    25  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99982000 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99982000 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99982000
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SYOGAI
               MOVE    23   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE "障害児（者）リハ：発症　　　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
               MOVE    29   TO  IDC1
               MOVE    35   TO  IDC2
               MOVE    41   TO  IDC3
               MOVE    25   TO  IDC4
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99982200 + IDX14
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99982200 + 10 + IDX14
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99982200
                                         + (IDX16 + (IDX14 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
           END-EVALUATE
      *
           PERFORM   VARYING    IDX12   FROM    1   BY  1
                      UNTIL   ( IDX12               >   49    )
                       OR     ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1       TO  IDX-RECE65
                MOVE   "810000001"
                               TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                               TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                MOVE   WK-FREE-NAME(IDX12)
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
           END-PERFORM
      *
      *    科・日付の編集を含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
               PERFORM 500-RECE65-KEY-HENSYU-SEC
                MOVE    WRK-ZAINUM           TO  RECE659-ZAINUM
                COMPUTE WRK-KOHBUNRI  =  IDX14  -  1
                MOVE    WRK-KOHBUNRI         TO  RECE659-KOHBUNRI
                MOVE    WRK-SRYKBN           TO  RECE659-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE659-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE65   =    1)
                   ADD         1             TO    IDX-RECE65
                   IF (WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE)
                   OR (IDX-RECE65                       >  100 )
                       MOVE    1             TO    FLG-WRKRECE65
                   ELSE
                       MOVE  WRK-RECE65XX-SRYACT-TBL (IDX-RECE65 1)
                                        TO   RECE659-SRYACT-TBL (IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE659-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE659-REC
                   ADD     1            TO   CNT-RECE659
                END-IF
           END-PERFORM
      *
      *
           PERFORM 500-RECE64-KEY-HENSYU-SEC
           MOVE    WRK-ZAINUM          TO  RECE649-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  IDX14  -  1
           MOVE    WRK-KOHBUNRI        TO  RECE649-KOHBUNRI
           MOVE    WRK-SRYKBN          TO  RECE649-SRYKBN
           MOVE    TBL-HASSYO-HKNCOMBI(IDX14)
                                       TO  RECE649-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           .
       20011-HASSYO-COM-201905-EXT.
           EXIT.
      *
      *****************************************************************
      *    発症日のコメント編集（第三者行為）
      *****************************************************************
       20011-HASSYO-COM2-201905-SEC          SECTION.
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO        TO  IDX-RECE65
           MOVE    SPACE       TO  WRK-RECE65XX-REC
           INITIALIZE              WRK-RECE65XX-REC
      *
           ADD     1           TO  IDX-RECE65
           MOVE    "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
           MOVE    ZERO        TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
           EVALUATE      WRK-SRYCD
             WHEN       CONS-SINDAI
               IF  RECE62-DAISAN-TOKKI  =  ZERO
                 MOVE  32   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "（第三者行為）"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
               MOVE "心大血管疾患リハ：治療開始　　　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                 MOVE  47   TO  IDC1
                 MOVE  53   TO  IDC2
                 MOVE  59   TO  IDC3
                 MOVE  43   TO  IDC4
               ELSE
                 MOVE  25   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE "心大血管疾患リハ：治療開始　　　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                 MOVE  33   TO  IDC1
                 MOVE  39   TO  IDC2
                 MOVE  45   TO  IDC3
                 MOVE  29   TO  IDC4
               END-IF
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99983000 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99983000 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99983000
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SINDAI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  31  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE "早期・初期リハ加算：発症　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  45  TO  IDC1
                     MOVE  51  TO  IDC2
                     MOVE  57  TO  IDC3
                     MOVE  41  TO  IDC4
                   ELSE
                     MOVE  24  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "早期・初期リハ加算：発症　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  31  TO  IDC1
                     MOVE  37  TO  IDC2
                     MOVE  43  TO  IDC3
                     MOVE  27  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  31  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE "早期・初期リハ加算：手術　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  45  TO  IDC1
                     MOVE  51  TO  IDC2
                     MOVE  57  TO  IDC3
                     MOVE  41  TO  IDC4
                   ELSE
                     MOVE  24  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "早期・初期リハ加算：手術　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  31  TO  IDC1
                     MOVE  37  TO  IDC2
                     MOVE  43  TO  IDC3
                     MOVE  27  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  33  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
             MOVE "早期・初期リハ加算：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  49  TO  IDC1
                     MOVE  55  TO  IDC2
                     MOVE  61  TO  IDC3
                     MOVE  45  TO  IDC4
                   ELSE
                     MOVE  26  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
             MOVE "早期・初期リハ加算：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  35  TO  IDC1
                     MOVE  41  TO  IDC2
                     MOVE  47  TO  IDC3
                     MOVE  31  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99983400 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99983400 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99983400
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-NOUKE
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  30  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                   MOVE "脳血管疾患等リハ：発症　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  43  TO  IDC1
                     MOVE  49  TO  IDC2
                     MOVE  55  TO  IDC3
                     MOVE  39  TO  IDC4
                   ELSE
                     MOVE  23  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE "脳血管疾患等リハ：発症　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  29  TO  IDC1
                     MOVE  35  TO  IDC2
                     MOVE  41  TO  IDC3
                     MOVE  25  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  30  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                   MOVE "脳血管疾患等リハ：手術　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  43  TO  IDC1
                     MOVE  49  TO  IDC2
                     MOVE  55  TO  IDC3
                     MOVE  39  TO  IDC4
                   ELSE
                     MOVE  23  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE "脳血管疾患等リハ：手術　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  29  TO  IDC1
                     MOVE  35  TO  IDC2
                     MOVE  41  TO  IDC3
                     MOVE  25  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  32  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
               MOVE "脳血管疾患等リハ：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  47  TO  IDC1
                     MOVE  53  TO  IDC2
                     MOVE  59  TO  IDC3
                     MOVE  43  TO  IDC4
                   ELSE
                     MOVE  25  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE "脳血管疾患等リハ：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  33  TO  IDC1
                     MOVE  39  TO  IDC2
                     MOVE  45  TO  IDC3
                     MOVE  29  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       3
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  30  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                   MOVE "脳血管疾患等リハ：診断　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  43  TO  IDC1
                     MOVE  49  TO  IDC2
                     MOVE  55  TO  IDC3
                     MOVE  39  TO  IDC4
                   ELSE
                     MOVE  23  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE "脳血管疾患等リハ：診断　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  29  TO  IDC1
                     MOVE  35  TO  IDC2
                     MOVE  41  TO  IDC3
                     MOVE  25  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99983800 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99983800 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99983800
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-HAIYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       1
                 WHEN       3
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  29  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE "廃用症候群リハ：診断　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  41  TO  IDC1
                     MOVE  47  TO  IDC2
                     MOVE  53  TO  IDC3
                     MOVE  37  TO  IDC4
                   ELSE
                     MOVE  22  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "廃用症候群リハ：診断　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  27  TO  IDC1
                     MOVE  33  TO  IDC2
                     MOVE  39  TO  IDC3
                     MOVE  23  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  31  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE "廃用症候群リハ：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  45  TO  IDC1
                     MOVE  51  TO  IDC2
                     MOVE  57  TO  IDC3
                     MOVE  41  TO  IDC4
                   ELSE
                     MOVE  24  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "廃用症候群リハ：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  31  TO  IDC1
                     MOVE  37  TO  IDC2
                     MOVE  43  TO  IDC3
                     MOVE  27  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99984200 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99984200 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99984200
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-HAIYO-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  31  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE "早期・初期リハ加算：発症　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  45  TO  IDC1
                     MOVE  51  TO  IDC2
                     MOVE  57  TO  IDC3
                     MOVE  41  TO  IDC4
                   ELSE
                     MOVE  24  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "早期・初期リハ加算：発症　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  31  TO  IDC1
                     MOVE  37  TO  IDC2
                     MOVE  43  TO  IDC3
                     MOVE  27  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  31  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE "早期・初期リハ加算：手術　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  45  TO  IDC1
                     MOVE  51  TO  IDC2
                     MOVE  57  TO  IDC3
                     MOVE  41  TO  IDC4
                   ELSE
                     MOVE  24  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "早期・初期リハ加算：手術　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  31  TO  IDC1
                     MOVE  37  TO  IDC2
                     MOVE  43  TO  IDC3
                     MOVE  27  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  33  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
             MOVE "早期・初期リハ加算：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  49  TO  IDC1
                     MOVE  55  TO  IDC2
                     MOVE  61  TO  IDC3
                     MOVE  45  TO  IDC4
                   ELSE
                     MOVE  26  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
             MOVE "早期・初期リハ加算：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  35  TO  IDC1
                     MOVE  41  TO  IDC2
                     MOVE  47  TO  IDC3
                     MOVE  31  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99984600 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99984600 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99984600
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-UNDOKI
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  27  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE "運動器リハ：発症　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  37  TO  IDC1
                     MOVE  43  TO  IDC2
                     MOVE  49  TO  IDC3
                     MOVE  33  TO  IDC4
                   ELSE
                     MOVE  20  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "運動器リハ：発症　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  23  TO  IDC1
                     MOVE  29  TO  IDC2
                     MOVE  35  TO  IDC3
                     MOVE  19  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  27  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE "運動器リハ：手術　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  37  TO  IDC1
                     MOVE  43  TO  IDC2
                     MOVE  49  TO  IDC3
                     MOVE  33  TO  IDC4
                   ELSE
                     MOVE  20  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "運動器リハ：手術　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  23  TO  IDC1
                     MOVE  29  TO  IDC2
                     MOVE  35  TO  IDC3
                     MOVE  19  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  29  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE "運動器リハ：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  41  TO  IDC1
                     MOVE  47  TO  IDC2
                     MOVE  53  TO  IDC3
                     MOVE  37  TO  IDC4
                   ELSE
                     MOVE  22  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "運動器リハ：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  27  TO  IDC1
                     MOVE  33  TO  IDC2
                     MOVE  39  TO  IDC3
                     MOVE  23  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       3
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  27  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE "運動器リハ：診断　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  37  TO  IDC1
                     MOVE  43  TO  IDC2
                     MOVE  49  TO  IDC3
                     MOVE  33  TO  IDC4
                   ELSE
                     MOVE  20  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "運動器リハ：診断　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  23  TO  IDC1
                     MOVE  29  TO  IDC2
                     MOVE  35  TO  IDC3
                     MOVE  19  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99985000 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99985000 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99985000
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-KOKYUKI
               IF  RECE62-DAISAN-TOKKI  =  ZERO
                 MOVE  29   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "（第三者行為）"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE "呼吸器リハ：治療開始　　　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                 MOVE  41   TO  IDC1
                 MOVE  47   TO  IDC2
                 MOVE  53   TO  IDC3
                 MOVE  37   TO  IDC4
               ELSE
                 MOVE  22   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "呼吸器リハ：治療開始　　　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                 MOVE  27   TO  IDC1
                 MOVE  33   TO  IDC2
                 MOVE  39   TO  IDC3
                 MOVE  23   TO  IDC4
               END-IF
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99985400 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99985400 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99985400
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-KOKYUKI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  31  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE "早期・初期リハ加算：発症　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  45  TO  IDC1
                     MOVE  51  TO  IDC2
                     MOVE  57  TO  IDC3
                     MOVE  41  TO  IDC4
                   ELSE
                     MOVE  24  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "早期・初期リハ加算：発症　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  31  TO  IDC1
                     MOVE  37  TO  IDC2
                     MOVE  43  TO  IDC3
                     MOVE  27  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  31  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE "早期・初期リハ加算：手術　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  45  TO  IDC1
                     MOVE  51  TO  IDC2
                     MOVE  57  TO  IDC3
                     MOVE  41  TO  IDC4
                   ELSE
                     MOVE  24  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "早期・初期リハ加算：手術　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  31  TO  IDC1
                     MOVE  37  TO  IDC2
                     MOVE  43  TO  IDC3
                     MOVE  27  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  33  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
             MOVE "早期・初期リハ加算：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  49  TO  IDC1
                     MOVE  55  TO  IDC2
                     MOVE  61  TO  IDC3
                     MOVE  45  TO  IDC4
                   ELSE
                     MOVE  26  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
             MOVE "早期・初期リハ加算：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  35  TO  IDC1
                     MOVE  41  TO  IDC2
                     MOVE  47  TO  IDC3
                     MOVE  31  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99985800 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99985800 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99985800
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SESSYOKU
               IF  RECE62-DAISAN-TOKKI  =  ZERO
                 MOVE  30   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "（第三者行為）"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE "摂食機能療法：治療開始　　　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                 MOVE  43   TO  IDC1
                 MOVE  49   TO  IDC2
                 MOVE  55   TO  IDC3
                 MOVE  39   TO  IDC4
               ELSE
                 MOVE  23   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "摂食機能療法：治療開始　　　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                 MOVE  29   TO  IDC1
                 MOVE  35   TO  IDC2
                 MOVE  41   TO  IDC3
                 MOVE  25   TO  IDC4
               END-IF
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99986200 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99986200 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99986200
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-NANBYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  28  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE "難病患者リハ：発症　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  39  TO  IDC1
                     MOVE  45  TO  IDC2
                     MOVE  51  TO  IDC3
                     MOVE  35  TO  IDC4
                   ELSE
                     MOVE  21  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "難病患者リハ：発症　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  25  TO  IDC1
                     MOVE  31  TO  IDC2
                     MOVE  37  TO  IDC3
                     MOVE  21  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  28  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                     MOVE "難病患者リハ：手術　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  39  TO  IDC1
                     MOVE  45  TO  IDC2
                     MOVE  51  TO  IDC3
                     MOVE  35  TO  IDC4
                   ELSE
                     MOVE  21  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "難病患者リハ：手術　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  25  TO  IDC1
                     MOVE  31  TO  IDC2
                     MOVE  37  TO  IDC3
                     MOVE  21  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   IF  RECE62-DAISAN-TOKKI  =  ZERO
                     MOVE  30  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                     MOVE "（第三者行為）"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                   MOVE "難病患者リハ：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                     MOVE  43  TO  IDC1
                     MOVE  49  TO  IDC2
                     MOVE  55  TO  IDC3
                     MOVE  39  TO  IDC4
                   ELSE
                     MOVE  23  TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                   MOVE "難病患者リハ：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                     MOVE  29  TO  IDC1
                     MOVE  35  TO  IDC2
                     MOVE  41  TO  IDC3
                     MOVE  25  TO  IDC4
                   END-IF
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99986600 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99986600 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99986600
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
             WHEN       CONS-SYOGAI
               IF  RECE62-DAISAN-TOKKI  =  ZERO
                 MOVE  30   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "（第三者行為）"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE "障害児（者）リハ：発症　　　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:)
                 MOVE  43   TO  IDC1
                 MOVE  49   TO  IDC2
                 MOVE  55   TO  IDC3
                 MOVE  39   TO  IDC4
               ELSE
                 MOVE  23   TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE "障害児（者）リハ：発症　　　　　年　　月　　日"
                            TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:)
                 MOVE  29   TO  IDC1
                 MOVE  35   TO  IDC2
                 MOVE  41   TO  IDC3
                 MOVE  25   TO  IDC4
               END-IF
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99987000 + IDX22
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99987000 + 10 + IDX22
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99987000
                                         + (IDX16 + (IDX22 * 31))
               END-EVALUATE
               MOVE    "80" TO  WRK-SRYKBN
           END-EVALUATE
      *
           PERFORM   VARYING    IDX12   FROM    1   BY  1
                      UNTIL   ( IDX12               >   49    )
                       OR     ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1       TO  IDX-RECE65
                MOVE   "810000001"
                               TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                               TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                MOVE   WK-FREE-NAME(IDX12)
                               TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
           END-PERFORM
      *
      *    科・日付の編集を含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
               PERFORM 500-RECE65-KEY-HENSYU-SEC
                MOVE    WRK-ZAINUM           TO  RECE659-ZAINUM
                COMPUTE WRK-KOHBUNRI  =  WRK-KOHBUNRI-JIDO
                MOVE    WRK-KOHBUNRI         TO  RECE659-KOHBUNRI
                MOVE    WRK-SRYKBN           TO  RECE659-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE659-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE65   =    1)
                   ADD         1             TO    IDX-RECE65
                   IF (WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE)
                   OR (IDX-RECE65                       >  100 )
                       MOVE    1             TO    FLG-WRKRECE65
                   ELSE
                       MOVE  WRK-RECE65XX-SRYACT-TBL (IDX-RECE65 1)
                                        TO   RECE659-SRYACT-TBL (IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE659-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE659-REC
                   ADD     1            TO   CNT-RECE659
                END-IF
           END-PERFORM
      *
      *
           PERFORM 500-RECE64-KEY-HENSYU-SEC
           MOVE    WRK-ZAINUM          TO  RECE649-ZAINUM
           COMPUTE WRK-KOHBUNRI  =  WRK-KOHBUNRI-JIDO
           MOVE    WRK-KOHBUNRI        TO  RECE649-KOHBUNRI
           MOVE    WRK-SRYKBN          TO  RECE649-SRYKBN
           MOVE    TBL-HASSYO-HKNCOMBI2(IDX22)
                                       TO  RECE649-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           .
       20011-HASSYO-COM2-201905-EXT.
           EXIT.
      *
      *****************************************************************
      *    発症日のコメント編集（年月日）
      *****************************************************************
       200111-HASSYO-COMMENT-DATE-SEC          SECTION.
      *
           INITIALIZE                         STS-AREA-DAY
           INITIALIZE                         LNK-DAY2-AREA
           MOVE    "21"                   TO  LNK-DAY2-IRAI
           MOVE    WRK-KYUSEIYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                          LNK-DAY2-AREA
           IF      STS-DAY-RC1            =   ZERO
              MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                       TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(IDC1:4)
              END-IF
      *
              MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                       TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(IDC2:4)
              END-IF
      *
              MOVE    LNK-DAY2-EDTYMD1-D   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                       TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(IDC3:4)
              END-IF
              IF   RECE62-SRYYM >= "201905"
                 MOVE    LNK-DAY2-EDTYMD2-G
                       TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(IDC4:4)
              END-IF
           END-IF
      *
           .
       200111-HASSYO-COMMENT-DATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ発症日があるか判定
      *****************************************************************
       20011-RIHABIRIHASSYO-HANTEI-SEC          SECTION.
      *
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT2
           MOVE     SPACE    TO     WRK-RIHASANTEI-END
           INITIALIZE               TBL-RIHASANTEI-DAY-END
           INITIALIZE               TBL-RIHASANTEI-DAY-END2
           INITIALIZE               TBL-RIHASANTEI-DAY-END3
      *
      *    リハ終了日
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE62-PTID         TO  SANTEI-PTID
           MOVE    WRK-SRYCD2          TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    WRK-RIHA-HKNCOMBI   TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      *
                MOVE    "tbl_santei"        TO  MCP-TABLE
                MOVE    "key3"              TO  MCP-PATHNAME
                PERFORM        900-SANTEI-READ-SEC
      *
                PERFORM   UNTIL  ( FLG-SANTEI          =  1 ) OR
                                 ( FLG-KYUSEI-COMMENT2 =  1 )
      *
                 IF   SANTEI-SRYYM  <=  RECE62-SRYYM
                  IF   SANTEI-SRYYM  =  RECE62-SRYYM
                   PERFORM   VARYING   IDX3   FROM   1  BY   1
                              UNTIL    IDX3    >    31
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                        MOVE    1   TO   TB-RIHASANTEI-DAY-END(IDX3)
      *
                     END-IF
      *
                   END-PERFORM
                  ELSE
                   PERFORM   VARYING   IDX3   FROM  31  BY  -1
                              UNTIL  ( IDX3                <   1 )
                                OR   ( FLG-KYUSEI-COMMENT2 =   1 )
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                        MOVE  SANTEI-SRYYM  TO  WRK-RIHASANTEI-END(1:6)
                        MOVE  IDX3          TO  WRK-RIHASANTEI-END(7:2)
      *
                        MOVE  1             TO  FLG-KYUSEI-COMMENT2
      *
                     END-IF
      *
                   END-PERFORM
                  END-IF
                 END-IF
      *
                 MOVE    "tbl_santei"        TO  MCP-TABLE
                 MOVE    "key3"              TO  MCP-PATHNAME
                 PERFORM        900-SANTEI-READ-SEC
      *
                END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      ********
      *
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT2
           MOVE     ZERO     TO     WRK-KYUSEIYMD
           INITIALIZE               TBL-RIHASANTEI-DAY
           MOVE     SPACE    TO     WRK-RIHASANTEI-YMD
           MOVE     SPACE    TO     WRK-RIHASANTEI-YMD2
           MOVE     SPACE    TO     WRK-RIHASANTEI-YMD3
           MOVE     ZERO     TO     WRK-RIHASANTEI41-FLG
      *
      *    リハ開始日
      *
      *    当月の先頭のリハビリ算定日
           MOVE     ZERO           TO  RIHA-ST2
           PERFORM  VARYING  RIHA-IDX  FROM   1  BY   1
                     UNTIL   RIHA-IDX    >   31
             IF     TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX) NOT = ZERO
               MOVE     RIHA-IDX   TO  RIHA-ST2
               MOVE     31         TO  RIHA-IDX
             END-IF
           END-PERFORM
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE62-PTID         TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    WRK-RIHA-HKNCOMBI   TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      *
                MOVE    "tbl_santei"        TO  MCP-TABLE
                MOVE    "key3"              TO  MCP-PATHNAME
                PERFORM        900-SANTEI-READ-SEC
      *
                PERFORM   UNTIL  ( FLG-SANTEI          =  1 ) OR
                                 ( FLG-KYUSEI-COMMENT2 =  1 )
      *
                 IF   SANTEI-SRYYM  <=  RECE62-SRYYM
                  IF   SANTEI-SRYYM  =  RECE62-SRYYM
                   PERFORM  VARYING   IDX3   FROM   1  BY  1
                             UNTIL    IDX3    >    31
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                         MOVE   1     TO  TB-RIHASANTEI-DAY(IDX3)
      *
                         MOVE   1     TO  FLG-KYUSEI-COMMENT
      *
                         IF    (SANTEI-SRYYM = "200604")
                         AND   (IDX3         =  1      )
                            MOVE    1    TO   WRK-RIHASANTEI41-FLG
                         END-IF
      **>                IF    (SANTEI-SRYYM  NOT = "200604")
      **>                AND   (IDX3              =  1      )
      **>                   MOVE   1     TO   FLG-KYUSEI-COMMENT2
      **>                END-IF
      *
                     END-IF
      *
                   END-PERFORM
                  ELSE
                   PERFORM  VARYING   IDX3   FROM  31  BY  -1
                             UNTIL   ( IDX3                <   1 )
                               OR    ( FLG-KYUSEI-COMMENT2 =   1 )
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
                       PERFORM  200111-RIHA-SANTEIPLUS-SEC
                     END-IF
      *
                     IF  (SANTEI-DAY(IDX3)  NOT =  ZERO)
                     AND (FLG-SANTEIPLUS-HANTEI =  ZERO)
                       MOVE  SANTEI-SRYYM    TO WRK-RIHASANTEI-YMD2(1:6)
                       MOVE  IDX3            TO WRK-RIHASANTEI-YMD2(7:2)
                       IF   (WRK-RIHASANTEI-YMD   = SPACE     )
                       OR   (WRK-RIHASANTEI-YMD   = "20060401")
                         IF  (WRK-RIHASANTEI-END  = SPACE              )
                         OR  (WRK-RIHASANTEI-END  < WRK-RIHASANTEI-YMD2)
                           MOVE  WRK-RIHASANTEI-YMD2
                                             TO WRK-RIHASANTEI-YMD
                           MOVE  1           TO FLG-KYUSEI-COMMENT
                           IF  (SANTEIPLUS-ENDYMD   NOT =  SPACE       )
                           AND (SANTEIPLUS-ENDYMD(1:6)  =  RECE62-SRYYM)
                           AND (IDX43                  >=  RIHA-ST2    )
                             MOVE   1   TO TB-RIHASANTEI-DAY-END3(IDX43)
                           END-IF
                         END-IF
                         IF  (WRK-RIHASANTEI-END  = SPACE              )
                         OR  (WRK-RIHASANTEI-END  < WRK-RIHASANTEI-YMD2)
                           IF   (WRK-RIHASANTEI41-FLG =  1        )
                           AND  (WRK-RIHASANTEI-YMD2  < "20060401")
                             MOVE  ZERO      TO TB-RIHASANTEI-DAY(1)
                           END-IF
                         END-IF
                       ELSE
                         IF  (WRK-RIHASANTEI-END  = SPACE              )
                         OR  (WRK-RIHASANTEI-END  < WRK-RIHASANTEI-YMD2)
                           MOVE  WRK-RIHASANTEI-YMD2
                                             TO WRK-RIHASANTEI-YMD3
                         END-IF
                         MOVE     1          TO FLG-KYUSEI-COMMENT2
                         IF  (WRK-RIHASANTEI-END  = SPACE              )
                         OR  (WRK-RIHASANTEI-END  < WRK-RIHASANTEI-YMD2)
                           IF    WRK-RIHASANTEI-YMD2  = "20060401"
                             MOVE  ZERO      TO FLG-KYUSEI-COMMENT2
                           END-IF
                         END-IF
                       END-IF
                     END-IF
      *
                   END-PERFORM
                  END-IF
                 END-IF
      *
                 MOVE    "tbl_santei"        TO  MCP-TABLE
                 MOVE    "key3"              TO  MCP-PATHNAME
                 PERFORM        900-SANTEI-READ-SEC
      *
                END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      ***
      *
      *    当月先頭のリハビリ算定日より前に当月のリハビリ終了日があれば
      *    前月以前の直近のリハビリ開始日をクリアする
      *    及び、
      *    当月のリハビリ終了日より前の当月のリハビリ開始日をクリアする
           IF    RIHA-ST2            NOT = ZERO
             PERFORM  VARYING  RIHA-IDX  FROM   1  BY   1
                       UNTIL   RIHA-IDX    >   31
               IF    (TB-RIHASANTEI-DAY-END(RIHA-IDX) NOT = ZERO)
               AND   (RIHA-ST2  >  RIHA-IDX                     )
                 MOVE   SPACE      TO  WRK-RIHASANTEI-YMD
                 MOVE   SPACE      TO  WRK-RIHASANTEI-YMD3
                 PERFORM  VARYING  RIHA-IDX2  FROM  1  BY  1
                           UNTIL   RIHA-IDX2    >  31
                   IF    (TB-RIHASANTEI-DAY(RIHA-IDX2)  NOT = ZERO)
                   AND   (RIHA-IDX  >= RIHA-IDX2                  )
                     MOVE  ZERO    TO  TB-RIHASANTEI-DAY(RIHA-IDX2)
                   END-IF
                 END-PERFORM
               END-IF
             END-PERFORM
           END-IF
      *
      *    リハビリ開始日を退避
           INITIALIZE                     WRK-CHECK-RIHASANTEI-AREA
           MOVE  TBL-RIHASANTEI-DAY   TO  WRK-CHECK-RIHASANTEI-DAY-OCC
           MOVE  WRK-RIHASANTEI-YMD   TO  WRK-CHECK-RIHASANTEI-YMD
           MOVE  WRK-RIHASANTEI-YMD3  TO  WRK-CHECK-RIHASANTEI-YMD3
      *
      *    当月のリハビリ開始日より後に当月のリハビリ算定日がなければ
      *    当月のリハビリ開始日をクリアする
           MOVE     ZERO       TO  FLG-OK
           PERFORM  VARYING  RIHA-IDX  FROM  31  BY  -1
                     UNTIL   RIHA-IDX    <    1
             IF     TB-RIHASANTEI-DAY(RIHA-IDX)  NOT = ZERO
               PERFORM   200111-RIHA-SANTEIPLUS2-SEC
               MOVE      ZERO  TO  FLG-OK
               PERFORM  VARYING  RIHA-IDX2  FROM  1  BY  1
                         UNTIL   RIHA-IDX2    >  31
                 IF    (TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX2) NOT = ZERO)
                 AND   (RIHA-IDX  <=  RIHA-IDX2                      )
                 AND   (IDX43     >=  RIHA-IDX2                      )
                   MOVE  ZERO  TO  TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX2)
                   MOVE  1     TO  FLG-OK
                 END-IF
               END-PERFORM
               IF   FLG-OK  =  ZERO
                 MOVE    ZERO  TO  TB-RIHASANTEI-DAY(RIHA-IDX)
               ELSE
                 IF  IDX43  NOT =  99
                   MOVE  1     TO  TB-RIHASANTEI-DAY-END2(IDX43)
                 END-IF
               END-IF
             END-IF
           END-PERFORM
      *
      *    当月の先頭のリハビリ開始日
           MOVE     ZERO           TO  RIHA-ST
           PERFORM  VARYING  RIHA-IDX  FROM   1  BY   1
                     UNTIL   RIHA-IDX    >   31
             IF     TB-RIHASANTEI-DAY(RIHA-IDX)  NOT = ZERO
               MOVE   RIHA-IDX     TO  RIHA-ST
               MOVE   31           TO  RIHA-IDX
             END-IF
           END-PERFORM
      *
      *    当月のリハビリ開始日に当月の終了日の入力（算定履歴で入力）
      *    があり、その中で一番月末に近い日
           MOVE     99             TO  RIHA-ED
           PERFORM  VARYING  RIHA-IDX  FROM  31  BY  -1
                     UNTIL   RIHA-IDX    <    1
             IF     TB-RIHASANTEI-DAY-END2(RIHA-IDX)  NOT = ZERO
               MOVE   RIHA-IDX     TO  RIHA-ED
               MOVE   1            TO  RIHA-IDX
             END-IF
           END-PERFORM
      *
      *    直近のリハビリ開始日に入力した当月の終了日（算定履歴で入力）
           MOVE     99             TO  RIHA-ED2
           PERFORM  VARYING  RIHA-IDX  FROM  31  BY  -1
                     UNTIL   RIHA-IDX    <    1
             IF     TB-RIHASANTEI-DAY-END3(RIHA-IDX)  NOT = ZERO
               MOVE   RIHA-IDX     TO  RIHA-ED2
               MOVE   1            TO  RIHA-IDX
             END-IF
           END-PERFORM
      *
      *    当月先頭のリハビリ開始日より前に当月のリハビリ算定日がなければ
      *    前月以前の直近のリハビリ開始日をクリアする
           MOVE     ZERO           TO  FLG-OK
           IF    WRK-RIHASANTEI-YMD  NOT = SPACE
             IF  RIHA-ST             NOT = ZERO
               PERFORM  VARYING  RIHA-IDX  FROM  1  BY  1
                         UNTIL   RIHA-IDX    >  31
                 IF ((TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX) NOT = ZERO) AND
                     (RIHA-ST   >  RIHA-IDX                       ))
                 OR ((TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX) NOT = ZERO) AND
                     (RIHA-ED   <  RIHA-IDX                       ))
                   MOVE   1        TO  FLG-OK
                 END-IF
               END-PERFORM
               IF    FLG-OK  =  ZERO
                 MOVE     SPACE    TO  WRK-RIHASANTEI-YMD
                 MOVE     SPACE    TO  WRK-RIHASANTEI-YMD3
               ELSE
                 MOVE     ZERO     TO  FLG-OK
                 PERFORM  VARYING  RIHA-IDX  FROM  1  BY  1
                           UNTIL   RIHA-IDX    >  31
                   IF  (TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX) NOT = ZERO)
                   AND (RIHA-ED2  <  RIHA-IDX                       )
                     MOVE   1      TO  FLG-OK
                   END-IF
                 END-PERFORM
                 IF    FLG-OK  =  ZERO
                   MOVE     SPACE  TO  WRK-RIHASANTEI-YMD3
                 END-IF
               END-IF
             ELSE
               PERFORM  VARYING  RIHA-IDX  FROM  1  BY  1
                         UNTIL   RIHA-IDX    >  31
                 IF  (TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX) NOT = ZERO)
                 AND (RIHA-ED2  <  RIHA-IDX                       )
                   MOVE   1        TO  FLG-OK
                 END-IF
               END-PERFORM
               IF    FLG-OK  =  ZERO
                 MOVE     SPACE    TO  WRK-RIHASANTEI-YMD3
               END-IF
             END-IF
           END-IF
      *
           IF   (TBL-HASSYO-TAIHIYMD1-FLG  =  1)
           AND  (FLG-KYUSEI-COMMENT        =  1)
             INITIALIZE                     TBL-RIHASANTEI-DAY
             MOVE     SPACE            TO   WRK-RIHASANTEI-YMD
             MOVE     SPACE            TO   WRK-RIHASANTEI-YMD3
      *
             IF  (TBL-HASSYO-TAIHIYMD11(1) NOT = SPACE                )
             AND (TBL-HASSYO-TAIHIYMD11(1) = WRK-CHECK-RIHASANTEI-YMD
                                          OR WRK-CHECK-RIHASANTEI-YMD3)
                 CONTINUE
             ELSE
                 MOVE  SPACE  TO  TBL-HASSYO-TAIHIYMD11(1)
             END-IF
             IF  (TBL-HASSYO-TAIHIYMD11(2) NOT = SPACE                )
             AND (TBL-HASSYO-TAIHIYMD11(2) = WRK-CHECK-RIHASANTEI-YMD
                                          OR WRK-CHECK-RIHASANTEI-YMD3)
                 CONTINUE
             ELSE
                 MOVE  SPACE  TO  TBL-HASSYO-TAIHIYMD11(2)
             END-IF
      *
             IF  (TBL-HASSYO-TAIHIYMD11(1) NOT = SPACE)
             AND (TBL-HASSYO-TAIHIYMD11(2) NOT = SPACE)
               IF TBL-HASSYO-TAIHIYMD11(2) > TBL-HASSYO-TAIHIYMD11(1)
                 MOVE TBL-HASSYO-TAIHIYMD11(1) TO WRK-RIHASANTEI-YMD3
                 MOVE TBL-HASSYO-TAIHIYMD11(2) TO WRK-RIHASANTEI-YMD
               ELSE
                 MOVE TBL-HASSYO-TAIHIYMD11(2) TO WRK-RIHASANTEI-YMD3
                 MOVE TBL-HASSYO-TAIHIYMD11(1) TO WRK-RIHASANTEI-YMD
               END-IF
             ELSE
               IF TBL-HASSYO-TAIHIYMD11(1) NOT = SPACE
                 MOVE TBL-HASSYO-TAIHIYMD11(1) TO WRK-RIHASANTEI-YMD
               END-IF
               IF TBL-HASSYO-TAIHIYMD11(2) NOT = SPACE
                 MOVE TBL-HASSYO-TAIHIYMD11(2) TO WRK-RIHASANTEI-YMD
               END-IF
             END-IF
      *
             PERFORM  VARYING  RIHA-IDX  FROM  1  BY  1
                       UNTIL   RIHA-IDX    >  31
               IF    (TBL-HASSYO-TAIHIYMD12(RIHA-IDX)  NOT = ZERO)
               AND   (WRK-CHECK-RIHASANTEI-DAY(RIHA-IDX)   = ZERO)
                 MOVE  ZERO  TO  TBL-HASSYO-TAIHIYMD12(RIHA-IDX)
               END-IF
             END-PERFORM
             MOVE  TBL-HASSYO-TAIHIYMD1-DAY-OCC2
                             TO  TBL-RIHASANTEI-DAY
           END-IF
      *
           .
       20011-RIHABIRIHASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ・算定履歴付加判定
      *    （リハ開始日の終了日と診療月をチェック）（履歴番号＝０のデータ）
      *****************************************************************
       200111-RIHA-SANTEIPLUS-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SANTEIPLUS-HANTEI
           MOVE    ZERO                TO  IDX43
      *
           INITIALIZE                      SANTEIPLUS-REC
           MOVE    RECE62-HOSPNUM      TO  SANTEIPLUS-HOSPNUM
           MOVE    RECE62-PTID         TO  SANTEIPLUS-PTID
           MOVE    SANTEI-SRYYM        TO  SANTEIPLUS-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEIPLUS-SRYCD
           MOVE    ZERO                TO  SANTEIPLUS-NYUGAIKBN
           MOVE    ZERO                TO  SANTEIPLUS-SRYKA
           MOVE    WRK-RIHA-HKNCOMBI   TO  SANTEIPLUS-HKNCOMBINUM
           MOVE    IDX3                TO  SANTEIPLUS-DAYKEY
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC           =  ZERO
             MOVE  "tbl_santeiplus"    TO  MCP-TABLE
             MOVE  "key4"              TO  MCP-PATHNAME
             PERFORM  900-SANTEIPLUS-READ-SEC
             IF    FLG-SANTEIPLUS   =  ZERO
               MOVE  SANTEIPLUS-ENDYMD(7:2)  TO  IDX43
               IF  ((SANTEIPLUS-ENDYMD   NOT =  SPACE       ) AND
                    (SANTEIPLUS-ENDYMD(1:6)  <  RECE62-SRYYM))
               OR  ((SANTEIPLUS-ENDYMD   NOT =  SPACE       ) AND
                    (SANTEIPLUS-ENDYMD(1:6)  =  RECE62-SRYYM) AND
                    (IDX43                   <  RIHA-ST2    ))
                  MOVE     1           TO  FLG-SANTEIPLUS-HANTEI
               END-IF
             END-IF
           END-IF
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200111-RIHA-SANTEIPLUS-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ・算定履歴付加判定
      *    （リハ開始日の終了日と診療月をチェック）（履歴番号＝０のデータ）
      *****************************************************************
       200111-RIHA-SANTEIPLUS2-SEC          SECTION.
      *
           MOVE    99                  TO  IDX43
      *
           INITIALIZE                      SANTEIPLUS-REC
           MOVE    RECE62-HOSPNUM      TO  SANTEIPLUS-HOSPNUM
           MOVE    RECE62-PTID         TO  SANTEIPLUS-PTID
           MOVE    RECE62-SRYYM        TO  SANTEIPLUS-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEIPLUS-SRYCD
           MOVE    ZERO                TO  SANTEIPLUS-NYUGAIKBN
           MOVE    ZERO                TO  SANTEIPLUS-SRYKA
           MOVE    WRK-RIHA-HKNCOMBI   TO  SANTEIPLUS-HKNCOMBINUM
           MOVE    RIHA-IDX            TO  SANTEIPLUS-DAYKEY
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC           =  ZERO
             MOVE  "tbl_santeiplus"    TO  MCP-TABLE
             MOVE  "key4"              TO  MCP-PATHNAME
             PERFORM  900-SANTEIPLUS-READ-SEC
             IF    FLG-SANTEIPLUS   =  ZERO
               IF   (SANTEIPLUS-ENDYMD   NOT =  SPACE       )
               AND  (SANTEIPLUS-ENDYMD(1:6)  =  RECE62-SRYYM)
                 MOVE  SANTEIPLUS-ENDYMD(7:2)  TO  IDX43
               END-IF
             END-IF
           END-IF
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200111-RIHA-SANTEIPLUS2-EXT.
           EXIT.
      *
      *****************************************************************
      *    発症日チェック（第三者行為）
      *****************************************************************
       20011-KYUSEIYMD-CHECK-SEC          SECTION.
      *
           IF   TBL-HASSYO-SINDAI-FLG   =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO-SINDAI-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO-SINDAI-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO-SINDAI-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
           IF   TBL-HASSYO-KOKYUKI-FLG  =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO-KOKYUKI-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO-KOKYUKI-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO-KOKYUKI-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
           IF   TBL-HASSYO-NOUKE-FLG    =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO-NOUKE-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO-NOUKE-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO-NOUKE-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
           IF   TBL-HASSYO-UNDOKI-FLG   =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO-UNDOKI-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO-UNDOKI-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO-UNDOKI-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
           IF   TBL-HASSYO-NANBYO-FLG   =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO-NANBYO-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO-NANBYO-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO-NANBYO-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
           IF   TBL-HASSYO-SYOGAI-FLG   =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO-SYOGAI-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO-SYOGAI-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO-SYOGAI-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
           IF   TBL-HASSYO-SESSYOKU-FLG =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO-SESSYOKU-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO-SESSYOKU-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO-SESSYOKU-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
           IF   TBL-HASSYO-HAIYO-FLG    =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO-HAIYO-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO-HAIYO-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO-HAIYO-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
      *****
      *
           IF   TBL-HASSYO9-SINDAI-FLG  =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO9-SINDAI-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO9-SINDAI-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO9-SINDAI-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
           IF   TBL-HASSYO9-HAIYO-FLG   =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO9-HAIYO-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO9-HAIYO-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO9-HAIYO-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
           IF   TBL-HASSYO9-KOKYUKI-FLG =   2 OR 3
             MOVE     ZERO   TO    FLG-CHECK-OK2
             PERFORM  VARYING  IDX47  FROM  1  BY  1
                       UNTIL  (IDX47             >  50   )
                        OR    (TBL-HASSYO9-KOKYUKI-KYUSEIYMD(IDX47)
                                                 =  SPACE)
                        OR    (FLG-CHECK-OK2     =  1    )
               IF  TBL-HASSYO9-KOKYUKI-KYUSEIYMD(IDX47) = WRK-KYUSEIYMD
                 MOVE   1    TO    FLG-CHECK-OK2
               END-IF
             END-PERFORM
             IF    FLG-CHECK-OK2 = ZERO
               MOVE   WRK-KYUSEIYMD
                             TO    TBL-HASSYO9-KOKYUKI-KYUSEIYMD(IDX47)
               MOVE     1    TO    FLG-CHECK-OK
             END-IF
           END-IF
      *
           .
       20011-KYUSEIYMD-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診（同日複数診療科）があるか判定
      *****************************************************************
       2001-SYODOU-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-SYODOU-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE      "1"       TO     WRK-JIDO-SETCOMFLG
                MOVE       4        TO     WRK-JIDO-SETCOMATFLG
                MOVE       99       TO     FLG-COMMENT
                PERFORM    20011-SYODOU-COMMENT-SEC
           END-IF
      *
           .
       2001-SYODOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診（同日複数診療科）のコメント編集
      *****************************************************************
       20011-SYODOU-COMMENT-SEC          SECTION.
      *
           MOVE       ZERO       TO   IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                      UNTIL   (IDX                       >   10  )
                       OR     (RECE64-SYODOU-SRYKA(IDX)  =  SPACE)
      *
             ADD      1                  TO  IDZ
             MOVE    "複初算定診療科："  TO  WK-FREE-NAME2(IDZ)(1:16)
      *      診療科の取得
             INITIALIZE                      SYS-1005-REC
             MOVE    "1005"              TO  SYS-1005-KANRICD
             MOVE    RECE64-SYODOU-SRYKA(IDX)
                                         TO  SYS-1005-KBNCD
             MOVE    RECE62-SRYYM        TO  SYS-1005-STYUKYMD(1:6)
             MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
             MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
             MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
             MOVE    SYS-1005-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM   900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI      =   ZERO
                  MOVE  SYS-1005-SRYKANAME(1:40)
                                         TO  WK-FREE-NAME2(IDZ)(17:40)
               END-IF
             END-IF
      *
             INITIALIZE                        MCPAREA
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
             IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
               MOVE  "2"                   TO  WK-FREE-MEISKYFLG2(IDZ)
               MOVE  WRK-JIDO-SETCOMATFLG  TO  WK-FREE-SETCOMATFLG2(IDZ)
             END-IF
      *
             PERFORM   VARYING   IDW   FROM   300   BY  -1
                        UNTIL    IDW   <   1
                IF   WK-FREE-NAME2(IDZ)(IDW:1)  NOT =  SPACE
                     COMPUTE WK-FREE-YUKOKETA2(IDZ) =  IDW  /   2
                     MOVE    1   TO    IDW
                END-IF
             END-PERFORM
      *
           END-PERFORM
      *
           PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           .
       20011-SYODOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤管理指導料があるか判定
      *****************************************************************
       2001-YAKUKANSIDO-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-YAKUKAN-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE      101      TO     WRK-JIDO-SETCOMATFLG
                MOVE       3       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-YAKUKANSIDO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院栄養食事指導料があるか判定
      *****************************************************************
       2001-NYUEIYOSIDO-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-NYUEIYO-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE      102      TO     WRK-JIDO-SETCOMATFLG
                MOVE       3       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-NYUEIYOSIDO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    退院前訪問指導料があるか判定
      *****************************************************************
       2001-HOUMONSIDO-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-HOUMON-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE      103      TO     WRK-JIDO-SETCOMATFLG
                MOVE       3       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-HOUMONSIDO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    傷病手当金意見書交付料があるか判定
      *****************************************************************
       2001-SYOBYO-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-SYOBYO-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE       6       TO     WRK-JIDO-SETCOMATFLG
                MOVE       4       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-SYOBYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    療養費同意書交付料があるか判定
      *****************************************************************
       2001-RYOYO-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-RYOYO-COMMENT    =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE       7       TO     WRK-JIDO-SETCOMATFLG
                MOVE       4       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-RYOYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    退院時服薬指導加算を算定時、当月に薬剤管理指導料がなく、前月
      *    に薬剤管理指導料があるか判定
      *****************************************************************
       2001-TAIIN-HANTEI-SEC          SECTION.
      *
           MOVE      ZERO            TO      WRK-TUKI-YAKUKAN
           MOVE      ZERO            TO      WRK-DAY-YAKUKAN
      *
           IF      RECE64-FLG-TAIIN-COMMENT   =    "1"
      *
             MOVE      ZERO            TO      FLG-YAKUKAN-COMMENT
      *
             IF    RECE62-SRYYM    >=  "200804"
               PERFORM   VARYING   IDX6   FROM    1   BY    1
                          UNTIL  ( IDX6   >   TBL-YAKUKAN-MAX       )
                            OR   ( FLG-YAKUKAN-COMMENT   =    1     )
                  MOVE   TBL-YAKUKAN(IDX6)     TO  WRK-YAKUKAN-SRYCD
                  PERFORM  2001X-TAIIN-HANTEI-TOUGETU-SEC
               END-PERFORM
             ELSE
                  MOVE   CONS-YAKUKAN-COMMENT  TO  WRK-YAKUKAN-SRYCD
                  PERFORM  2001X-TAIIN-HANTEI-TOUGETU-SEC
             END-IF
      *
             IF      FLG-YAKUKAN-COMMENT   =   ZERO
      *
               MOVE      ZERO            TO      WRK-ZENYYMM
      *
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    RECE62-SRYYM        TO  LNK-DAY6-KIJUN(1:6)
               MOVE    "01"                TO  LNK-DAY6-KIJUN(7:2)
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE     -1                 TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN(1:6)
                                           TO  WRK-ZENYYMM
      *
               IF    RECE62-SRYYM    >=  "200804"
                 PERFORM   VARYING   IDX6   FROM    1   BY    1
                            UNTIL    IDX6   >   TBL-YAKUKAN-MAX
                    MOVE   TBL-YAKUKAN(IDX6)     TO  WRK-YAKUKAN-SRYCD
                    PERFORM  2001Y-TAIIN-HANTEI-ZENGETU-SEC
                 END-PERFORM
               ELSE
                    MOVE   CONS-YAKUKAN-COMMENT  TO  WRK-YAKUKAN-SRYCD
                    PERFORM  2001Y-TAIIN-HANTEI-ZENGETU-SEC
               END-IF
      *
             END-IF
      *
             MOVE       99                TO     FLG-COMMENT
             IF    WRK-DAY-YAKUKAN    NOT   =   ZERO
                MOVE    2                 TO     FLG-HANTEI
                MOVE    WRK-ZENYYMM(5:2)  TO     WRK-TUKI-YAKUKAN
             ELSE
                MOVE    1                 TO     FLG-HANTEI
             END-IF
             PERFORM    20011-TAIIN-COMMENT-SEC
      *
           END-IF
      *
           .
       2001-TAIIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    当月に薬剤管理指導料があるかチェック
      *****************************************************************
       2001X-TAIIN-HANTEI-TOUGETU-SEC          SECTION.
      *
           INITIALIZE                    SRYACT-REC
           MOVE    RECE62-HOSPNUM    TO  SRY-HOSPNUM
           MOVE    RECE62-NYUGAIKBN  TO  SRY-NYUGAIKBN
           MOVE    RECE62-PTID       TO  SRY-PTID
           MOVE    RECE62-SRYYM      TO  SRY-SRYYM
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (1)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (2)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (3)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (4)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (5)
           MOVE    SRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_sryact"      TO  MCP-TABLE
           MOVE    "key7"            TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
      *
           IF      MCP-RC               =       ZERO
             MOVE    "tbl_sryact"    TO  MCP-TABLE
             MOVE    "key7"          TO  MCP-PATHNAME
             PERFORM  900-SRYACT-READ-SEC
             IF      FLG-SRYACT  =  ZERO
                   MOVE    1         TO  FLG-YAKUKAN-COMMENT
             END-IF
           END-IF
      *
           INITIALIZE                    MCPAREA
           MOVE    "tbl_sryact"      TO  MCP-TABLE
           MOVE    "key7"            TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       2001X-TAIIN-HANTEI-TOUGETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    前月に薬剤管理指導料があるかチェック
      *****************************************************************
       2001Y-TAIIN-HANTEI-ZENGETU-SEC          SECTION.
      *
           INITIALIZE                    SRYACT-REC
           MOVE    RECE62-HOSPNUM    TO  SRY-HOSPNUM
           MOVE    RECE62-NYUGAIKBN  TO  SRY-NYUGAIKBN
           MOVE    RECE62-PTID       TO  SRY-PTID
           MOVE    WRK-ZENYYMM       TO  SRY-SRYYM
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (1)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (2)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (3)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (4)
           MOVE    WRK-YAKUKAN-SRYCD TO  SRY-SRYCD (5)
           MOVE    SRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_sryact"      TO  MCP-TABLE
           MOVE    "key7"            TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
      *
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO   MCP-TABLE
               MOVE    "key7"           TO   MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
      *
               PERFORM  UNTIL   FLG-SRYACT   =  1
      *
                 INITIALIZE                  SRYACCT-REC
                 MOVE    SRY-HOSPNUM    TO   ACCT-HOSPNUM
                 MOVE    SRY-NYUGAIKBN  TO   ACCT-NYUGAIKBN
                 MOVE    SRY-PTID       TO   ACCT-PTID
                 MOVE    SRY-SRYKA      TO   ACCT-SRYKA
                 MOVE    SRY-SRYYM      TO   ACCT-SRYYM
                 MOVE    SRY-ZAINUM     TO   ACCT-ZAINUM
                 MOVE    SRYACCT-REC    TO   MCPDATA-REC
                 MOVE    "tbl_sryacct"  TO   MCP-TABLE
                 MOVE    "key31"        TO   MCP-PATHNAME
                 PERFORM   900-DBSELECT-SEC
      *
                 IF  MCP-RC               =  ZERO
                     MOVE    "tbl_sryacct"  TO  MCP-TABLE
                     MOVE    "key31"        TO  MCP-PATHNAME
                     PERFORM   900-DBFETCH-SEC
                     IF      MCP-RC       =  ZERO
                       MOVE    MCPDATA-REC  TO  SRYACCT-REC
                       PERFORM   VARYING   IDX  FROM   1   BY   1
                                  UNTIL    IDX    >   31
                         IF    ACCT-DAY(1 IDX)  NOT =  ZERO
                            IF    IDX   >   WRK-DAY-YAKUKAN
                                MOVE  IDX   TO  WRK-DAY-YAKUKAN
                            END-IF
                         END-IF
                       END-PERFORM
                     END-IF
                 END-IF
      *
                 INITIALIZE                  MCPAREA
                 MOVE    "tbl_sryacct"  TO   MCP-TABLE
                 MOVE    "key31"        TO   MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
      *
                 MOVE    "tbl_sryact"   TO   MCP-TABLE
                 MOVE    "key7"         TO   MCP-PATHNAME
                 PERFORM   900-SRYACT-READ-SEC
      *
               END-PERFORM
           END-IF
      *
           INITIALIZE                        MCPAREA
           MOVE    "tbl_sryact"      TO      MCP-TABLE
           MOVE    "key7"            TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       2001Y-TAIIN-HANTEI-ZENGETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    介護支援連携指導料があるか判定
      *****************************************************************
       2001-KAISIEN-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-KAISIEN-COMMENT   =    "1"
               MOVE    ZERO                TO  FLG-KAISIEN
               MOVE    SPACE               TO  WRK-KAISIEN-FSANTEIYMD
               INITIALIZE                      SANTEI-REC
               MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
               MOVE    RECE62-PTID         TO  SANTEI-PTID
               MOVE    CONS-KAISIEN        TO  SANTEI-SRYCD
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-SRYKA
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"      TO  MCP-TABLE
                 MOVE    "key3"            TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI   =  1 )
                            OR    ( FLG-KAISIEN  =  1 )
                   IF   SANTEI-SRYYM  <  RECE62-SRYYM
                     PERFORM  VARYING   IDX3   FROM  31  BY  -1
                               UNTIL  ( IDX3        <   1 )
                                OR    ( FLG-KAISIEN =   1 )
                       IF   SANTEI-DAY(IDX3)  NOT =  ZERO
                         MOVE    1    TO  FLG-KAISIEN
                         MOVE   SANTEI-SRYYM
                                      TO  WRK-KAISIEN-FSANTEIYMD(1:6)
                         MOVE   IDX3  TO  WRK-KAISIEN-FSANTEIYMD(7:2)
                       END-IF
                     END-PERFORM
                   END-IF
                   MOVE    "tbl_santei"    TO  MCP-TABLE
                   MOVE    "key3"          TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
               END-IF
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM  900-CLOSE-SEC
      *
               IF   FLG-KAISIEN  =   1
      *           剤の最初の日検索
                  MOVE       SPACE         TO  WRK-KAISIEN-YMD
                  PERFORM  VARYING   IDX3   FROM  1  BY  1
                            UNTIL    IDX3  >   31
                    IF   RECE64-KAISU(1 IDX3)  NOT =  ZERO
                       MOVE  RECE62-SRYYM  TO  WRK-KAISIEN-YMD(1:6)
                       MOVE  IDX3          TO  WRK-KAISIEN-YMD(7:2)
                       MOVE  31            TO  IDX3
                    END-IF
                  END-PERFORM
      *
                  IF   WRK-KAISIEN-YMD   NOT =  SPACE
                    MOVE  WRK-KAISIEN-YMD     TO  WRK-KAISIEN-CHECKYMD
                    PERFORM  2001-KAISIEN-PTNYUINRRK-CHK-SEC
                    MOVE  WRK-KAISIEN-RRKNUM  TO  WRK-KAISIEN-RRKNUM-X
      *
                    MOVE  WRK-KAISIEN-FSANTEIYMD
                                              TO  WRK-KAISIEN-CHECKYMD
                    PERFORM  2001-KAISIEN-PTNYUINRRK-CHK-SEC
                    MOVE  WRK-KAISIEN-RRKNUM  TO  WRK-KAISIEN-RRKNUM-Y
      *
                    IF  (WRK-KAISIEN-RRKNUM-X  NOT = ZERO           )
                    AND (WRK-KAISIEN-RRKNUM-Y  NOT = ZERO           )
                    AND (WRK-KAISIEN-RRKNUM-X = WRK-KAISIEN-RRKNUM-Y)
                       CONTINUE
                    ELSE
                       MOVE   ZERO   TO   FLG-KAISIEN
                    END-IF
                  ELSE
                       MOVE   ZERO   TO   FLG-KAISIEN
                  END-IF
               END-IF
      *
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE      106      TO     WRK-JIDO-SETCOMATFLG
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-KAISIEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴チェック
      *****************************************************************
       2001-KAISIEN-PTNYUINRRK-CHK-SEC          SECTION.
      *
           MOVE    ZERO                  TO  WRK-KAISIEN-RRKNUM
           INITIALIZE                        PTNYUINRRK-REC
           MOVE    RECE62-HOSPNUM        TO  PTNYUINRRK-HOSPNUM
           MOVE    RECE62-PTID           TO  PTNYUINRRK-PTID
           MOVE    WRK-KAISIEN-CHECKYMD  TO  PTNYUINRRK-NYUINYMD
           MOVE    WRK-KAISIEN-CHECKYMD  TO  PTNYUINRRK-TAIINYMD
           MOVE    PTNYUINRRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"      TO  MCP-TABLE
           MOVE    "key11"               TO  MCP-PATHNAME
           PERFORM  900-DBSELECT-SEC
           IF      MCP-RC   =   ZERO
             MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
             MOVE    "key11"             TO  MCP-PATHNAME
             PERFORM  900-PTNYUINRRK-READ-SEC
             IF    FLG-PTNYUINRRK  =  ZERO
                MOVE  PTNYUINRRK-RRKNUM  TO  WRK-KAISIEN-RRKNUM
             END-IF
           END-IF
           MOVE    "tbl_ptnyuinrrk"      TO  MCP-TABLE
           MOVE    "key11"               TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       2001-KAISIEN-PTNYUINRRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    集団栄養食事指導料があるか判定
      *****************************************************************
       2001-SYUEIYO-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-SYUEIYO-COMMENT   =    "1"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       18      TO     WRK-JIDO-SETCOMATFLG
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-SYUEIYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    人工内耳機器調整加算があるか判定
      *****************************************************************
       2001-JINNAI-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-JINNAI-COMMENT   NOT =   SPACE
             MOVE      ZERO                TO  FLG-JINNAI-COMMENT
             MOVE      SPACE               TO  WRK-JINNAI-YMD
             PERFORM   VARYING   IDX   FROM    1   BY  1
                        UNTIL   (IDX                 >  2)
                         OR     (FLG-JINNAI-COMMENT  =  1)
               INITIALIZE                      SANTEI-REC
               MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
               MOVE    RECE62-PTID         TO  SANTEI-PTID
               EVALUATE  RECE64-FLG-JINNAI-COMMENT
                 WHEN      "1"
                       MOVE  CONS-JINNAI1  TO  SANTEI-SRYCD
                 WHEN      "2"
                   EVALUATE  IDX
                     WHEN     1
                       MOVE  CONS-JINNAI2  TO  SANTEI-SRYCD
                     WHEN     2
                       MOVE  CONS-JINNAI1  TO  SANTEI-SRYCD
                   END-EVALUATE
               END-EVALUATE
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-SRYKA
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"      TO  MCP-TABLE
                 MOVE    "key3"            TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                             OR   ( FLG-JINNAI-COMMENT  =   1 )
                   IF   SANTEI-SRYYM  <  RECE62-SRYYM
                     MOVE  SANTEI-SRYYM     TO  WRK-JINNAI-YMD(1:6)
                     MOVE  SANTEI-MSANTEID  TO  WRK-JINNAI-YMD(7:2)
                     MOVE    1              TO  FLG-JINNAI-COMMENT
                   END-IF
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
               END-IF
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               EVALUATE  RECE64-FLG-JINNAI-COMMENT
                 WHEN      "1"
                   MOVE     2              TO  IDX
               END-EVALUATE
             END-PERFORM
      *
             INITIALIZE                WRK-JIDO-SETCOM-AREA
             MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
             MOVE       45      TO     WRK-JIDO-SETCOMATFLG
             MOVE       99      TO     FLG-COMMENT
      *
             IF   WRK-JINNAI-YMD   NOT =  SPACE
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                MOVE       19      TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "前回算定年月日　　　　　年　　月　　日"
                                   TO     WK-FREE-NAME2(IDZ)
                MOVE       21      TO     IDX41
                MOVE       27      TO     IDX42
                MOVE       33      TO     IDX48
                MOVE       17      TO     IDX49
      *
                INITIALIZE                      STS-AREA-DAY
                INITIALIZE                      LNK-DAY2-AREA
                MOVE    "21"                TO  LNK-DAY2-IRAI
                MOVE    WRK-JINNAI-YMD      TO  LNK-DAY2-YMD
                CALL    "ORCSDAY"    USING      STS-AREA-DAY
                                                LNK-DAY2-AREA
                IF      STS-DAY-RC1  =  ZERO
                  MOVE  LNK-DAY2-EDTYMD1-Y  TO  WRK-TAIHI-XX
                  INITIALIZE                    ORCSKANACHKAREA
                  MOVE  "2"                 TO  KANACHK-SYORI
                  MOVE  WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                  CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                  IF    KANACHK-RC  =  ZERO
                    MOVE  KANACHK-OUT-INPUT(1:4)
                               TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                  END-IF
      *
                  MOVE  LNK-DAY2-EDTYMD1-M  TO  WRK-TAIHI-XX
                  INITIALIZE                    ORCSKANACHKAREA
                  MOVE  "2"                 TO  KANACHK-SYORI
                  MOVE  WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                  CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                  IF    KANACHK-RC  =  ZERO
                    MOVE  KANACHK-OUT-INPUT(1:4)
                               TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                  END-IF
      *
                  MOVE  LNK-DAY2-EDTYMD1-D  TO  WRK-TAIHI-XX
                  INITIALIZE                    ORCSKANACHKAREA
                  MOVE  "2"                 TO  KANACHK-SYORI
                  MOVE  WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                  CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                  IF    KANACHK-RC  =  ZERO
                    MOVE  KANACHK-OUT-INPUT(1:4)
                               TO  WK-FREE-NAME2(IDZ)(IDX48:4)
                  END-IF
      *
                  MOVE  LNK-DAY2-EDTYMD2-G
                               TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                END-IF
             ELSE
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                MOVE       6       TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "（初回算定）"  TO     WK-FREE-NAME2(IDZ)
             END-IF
      *
             IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
               MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
               MOVE  WRK-JIDO-SETCOMATFLG
                                TO     WK-FREE-SETCOMATFLG2(IDZ)
             END-IF
      *
             PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           END-IF
      *
           .
       2001-JINNAI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    心臓ペースメーカー指導管理料（ロ）があるか判定
      *    遠隔モニタリング加算があるか判定
      *****************************************************************
       2001-SINPESUR-HANTEI-SEC          SECTION.
      *
           IF   ((RECE62-SRYYM                  <    "201804"  ) AND
                 (RECE64-FLG-SINPESUR-COMMENT   =    "1" OR "2"))
           OR   ((RECE62-SRYYM                 >=    "201804"  ) AND
                 (RECE64-FLG-SINPESUR-COMMENT   =    "1" OR "2") AND
                 (RECE64-FLG-SINPESU-COMMENT    =     SPACE    ))
           OR   ((RECE62-SRYYM                 >=    "201804"  ) AND
                 (RECE64-FLG-SINPESU-COMMENT    =    "A"       ) AND
                 (RECE64-FLG-SINPESUR-COMMENT   =    "A"       ))
           OR   ((RECE62-SRYYM                 >=    "201804"  ) AND
                 (RECE64-FLG-SINPESU-COMMENT    =    "B"       ) AND
                 (RECE64-FLG-SINPESUR-COMMENT   =    "B"       ))
             MOVE    ZERO                TO  FLG-SINPESUR-COMMENT
             MOVE    SPACE               TO  WRK-SINPESUR-YMD
             INITIALIZE                      SANTEI-REC
             MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE62-PTID         TO  SANTEI-PTID
             EVALUATE  RECE64-FLG-SINPESUR-COMMENT
               WHEN     "1"
                 MOVE    CONS-SINPESUR1  TO  SANTEI-SRYCD
               WHEN     "2"
                 MOVE    CONS-SINPESUR2  TO  SANTEI-SRYCD
               WHEN     "A"
                 MOVE    CONS-ZAISAN     TO  SANTEI-SRYCD
               WHEN     "B"
                 MOVE    CONS-YOUATU2    TO  SANTEI-SRYCD
             END-EVALUATE
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI           =   1 )
                             OR   ( FLG-SINPESUR-COMMENT =   1 )
      *
                   IF   SANTEI-SRYYM  <  RECE62-SRYYM
                     MOVE  SANTEI-SRYYM     TO  WRK-SINPESUR-YMD(1:6)
                     MOVE  SANTEI-MSANTEID  TO  WRK-SINPESUR-YMD(7:2)
                     MOVE    1              TO  FLG-SINPESUR-COMMENT
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             IF   WRK-SINPESUR-YMD   NOT =  SPACE
      *
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE       27      TO     WRK-JIDO-SETCOMATFLG
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                EVALUATE  RECE64-FLG-SINPESUR-COMMENT
                  WHEN     "1"
                  WHEN     "2"
                     IF   RECE62-SRYYM >= "201905"
                       MOVE    15    TO     WK-FREE-YUKOKETA2(IDZ)
                       MOVE   "前回算定年月　　　　　年　　月"
                                     TO     WK-FREE-NAME2(IDZ)
                       MOVE    19    TO     IDX41
                       MOVE    25    TO     IDX42
                       MOVE    15    TO     IDX49
                     ELSE
                       MOVE    13    TO     WK-FREE-YUKOKETA2(IDZ)
                       MOVE   "前回算定年月　　　年　　月"
                                     TO     WK-FREE-NAME2(IDZ)
                       MOVE    15    TO     IDX41
                       MOVE    21    TO     IDX42
                     END-IF
                  WHEN     "A"
                  WHEN     "B"
                     IF   RECE62-SRYYM >= "201905"
                       MOVE    20    TO     WK-FREE-YUKOKETA2(IDZ)
                       MOVE   "指導管理料前回算定年月　　　　　年　　月"
                                     TO     WK-FREE-NAME2(IDZ)
                       MOVE    29    TO     IDX41
                       MOVE    35    TO     IDX42
                       MOVE    25    TO     IDX49
                     ELSE
                       MOVE    18    TO     WK-FREE-YUKOKETA2(IDZ)
                       MOVE   "指導管理料前回算定年月　　　年　　月"
                                     TO     WK-FREE-NAME2(IDZ)
                       MOVE    25    TO     IDX41
                       MOVE    31    TO     IDX42
                     END-IF
                END-EVALUATE
      *
                INITIALIZE                         STS-AREA-DAY
                INITIALIZE                         LNK-DAY2-AREA
                MOVE    "21"                   TO  LNK-DAY2-IRAI
                MOVE    WRK-SINPESUR-YMD       TO  LNK-DAY2-YMD
                CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                               LNK-DAY2-AREA
                IF      STS-DAY-RC1            =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                   END-IF
      *
                   IF   RECE62-SRYYM >= "201905"
                      MOVE    LNK-DAY2-EDTYMD2-G
                                        TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                   END-IF
                END-IF
      *
                IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                  MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                  MOVE  WRK-JIDO-SETCOMATFLG
                                   TO     WK-FREE-SETCOMATFLG2(IDZ)
                END-IF
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-SINPESUR-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    がん患者指導管理料２・３があるか判定
      *****************************************************************
       2001-GANSIDO-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-GANSIDO-COMMENT   =    "1" OR "2"
             MOVE    ZERO                TO  IDX27
             INITIALIZE                      WRK-GANSIDO-YMD-AREA
             INITIALIZE                      SANTEI-REC
             MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE62-PTID         TO  SANTEI-PTID
             EVALUATE  RECE64-FLG-GANSIDO-COMMENT
               WHEN     "1"
                 MOVE    CONS-GANSIDO1   TO  SANTEI-SRYCD
               WHEN     "2"
                 MOVE    CONS-GANSIDO2   TO  SANTEI-SRYCD
             END-EVALUATE
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL   FLG-SANTEI  =   1
      *
      ***>         IF   SANTEI-SRYYM  <= RECE62-SRYYM
                   IF   SANTEI-SRYYM  <  RECE62-SRYYM
                     PERFORM  VARYING  IDX26  FROM  31  BY  -1
                               UNTIL   IDX26   <     1
                       IF  (SANTEI-DAY(IDX26)  NOT =  ZERO)
                       AND (IDX27              <    10    )
                         ADD   1      TO  IDX27
                         MOVE  SANTEI-SRYYM
                                      TO  WRK-GANSIDO-YMD(IDX27)(1:6)
                         MOVE  IDX26  TO  WRK-GANSIDO-YMD(IDX27)(7:2)
                       END-IF
                     END-PERFORM
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             IF   WRK-GANSIDO-YMD(1) NOT =  SPACE
      *
                INITIALIZE                WRK-JIDO-SETCOM-AREA
      ***       MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
      ***       MOVE       28      TO     WRK-JIDO-SETCOMATFLG
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                PERFORM  VARYING  IDX26  FROM  10  BY  -1
                          UNTIL   IDX26   <     1
                 IF  WRK-GANSIDO-YMD(IDX26)  NOT =  SPACE
                  ADD        1       TO     IDZ
      *
                  IF   WRK-GANSIDO-YMD(IDX26)(1:6) = RECE62-SRYYM
                    IF   RECE62-SRYYM >= "201905"
                      MOVE     17      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "当月算定日　　　　　年　　月　　日"
                                       TO     WK-FREE-NAME2(IDZ)
                      MOVE     17      TO     IDX41
                      MOVE     23      TO     IDX42
                      MOVE     29      TO     IDX48
                      MOVE     13      TO     IDX49
                    ELSE
                      MOVE     15      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "当月算定日　　　年　　月　　日"
                                       TO     WK-FREE-NAME2(IDZ)
                      MOVE     13      TO     IDX41
                      MOVE     19      TO     IDX42
                      MOVE     25      TO     IDX48
                    END-IF
                  ELSE
                    IF   RECE62-SRYYM >= "201905"
                      MOVE     17      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "過去算定日　　　　　年　　月　　日"
                                       TO     WK-FREE-NAME2(IDZ)
                      MOVE     17      TO     IDX41
                      MOVE     23      TO     IDX42
                      MOVE     29      TO     IDX48
                      MOVE     13      TO     IDX49
                    ELSE
                      MOVE     15      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "過去算定日　　　年　　月　　日"
                                       TO     WK-FREE-NAME2(IDZ)
                      MOVE     13      TO     IDX41
                      MOVE     19      TO     IDX42
                      MOVE     25      TO     IDX48
                    END-IF
                  END-IF
      *
                  INITIALIZE                         STS-AREA-DAY
                  INITIALIZE                         LNK-DAY2-AREA
                  MOVE    "21"                   TO  LNK-DAY2-IRAI
                  MOVE    WRK-GANSIDO-YMD(IDX26) TO  LNK-DAY2-YMD
                  CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                                 LNK-DAY2-AREA
                  IF      STS-DAY-RC1            =   ZERO
                     MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                     INITIALIZE                       ORCSKANACHKAREA
                     MOVE    "2"                  TO  KANACHK-SYORI
                     MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                     CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                     IF      KANACHK-RC      =   ZERO
                        MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                     END-IF
      *
                     MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                     INITIALIZE                       ORCSKANACHKAREA
                     MOVE    "2"                  TO  KANACHK-SYORI
                     MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                     CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                     IF      KANACHK-RC      =   ZERO
                        MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                     END-IF
      *
                     MOVE    LNK-DAY2-EDTYMD1-D   TO  WRK-TAIHI-XX
                     INITIALIZE                       ORCSKANACHKAREA
                     MOVE    "2"                  TO  KANACHK-SYORI
                     MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                     CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                     IF      KANACHK-RC      =   ZERO
                        MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX48:4)
                     END-IF
      *
                     IF   RECE62-SRYYM >= "201905"
                        MOVE    LNK-DAY2-EDTYMD2-G
                                   TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                     END-IF
                  END-IF
                  IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                    MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                    MOVE  WRK-JIDO-SETCOMATFLG
                                     TO     WK-FREE-SETCOMATFLG2(IDZ)
                  END-IF
                 END-IF
                END-PERFORM
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-GANSIDO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    排尿自立指導料があるか判定
      *****************************************************************
       2001-HAIJRT-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-HAIJRT-COMMENT   =    "1"
             MOVE    SPACE               TO  WRK-HAIJRT-YMD
             INITIALIZE                      SANTEI-REC
             MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE62-PTID         TO  SANTEI-PTID
             MOVE    RECE62-SRYYM        TO  SANTEI-SRYYM
             MOVE    CONS-HAIJRT         TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  WRK-MCP-TABLE
             MOVE    "key"               TO  WRK-MCP-PATHNAME
             PERFORM  900-SANTEI-INV-SEC
             IF       FLG-SANTEI        =   ZERO
                 MOVE SANTEI-FSANTEIYMD  TO  WRK-HAIJRT-YMD
             END-IF
      *
             IF   WRK-HAIJRT-YMD   NOT =  SPACE
      *
                MOVE    ZERO             TO  WRK-HAIJRT-KAISU
                INITIALIZE                   SANTEI-REC
                MOVE    RECE62-HOSPNUM   TO  SANTEI-HOSPNUM
                MOVE    RECE62-PTID      TO  SANTEI-PTID
                MOVE    CONS-HAIJRT      TO  SANTEI-SRYCD
                MOVE    ZERO             TO  SANTEI-NYUGAIKBN
                MOVE    ZERO             TO  SANTEI-SRYKA
                MOVE    ZERO             TO  SANTEI-HKNCOMBINUM
                MOVE    SANTEI-REC       TO  MCPDATA-REC
                MOVE    "tbl_santei"     TO  MCP-TABLE
                MOVE    "key3"           TO  MCP-PATHNAME
                PERFORM 900-DBSELECT-SEC
                IF      MCP-RC              =   ZERO
                  MOVE    "tbl_santei"      TO  MCP-TABLE
                  MOVE    "key3"            TO  MCP-PATHNAME
                  PERFORM  900-SANTEI-READ-SEC
                  PERFORM   UNTIL   FLG-SANTEI  =   1
                    IF   SANTEI-SRYYM  <=  RECE62-SRYYM
                    AND  SANTEI-SRYYM  >=  WRK-HAIJRT-YMD(1:6)
                      MOVE  WRK-HAIJRT-YMD(7:2)  TO  IDX28
                      PERFORM  VARYING  IDX26  FROM  31  BY  -1
                                UNTIL   IDX26   <     1
                        IF   SANTEI-DAY(IDX26)  NOT =  ZERO
                          IF  (SANTEI-SRYYM = WRK-HAIJRT-YMD(1:6))
                          AND (IDX26        < IDX28              )
                            CONTINUE
                          ELSE
                            ADD    1    TO   WRK-HAIJRT-KAISU
                          END-IF
                        END-IF
                      END-PERFORM
                    END-IF
                    MOVE    "tbl_santei"    TO  MCP-TABLE
                    MOVE    "key3"          TO  MCP-PATHNAME
                    PERFORM  900-SANTEI-READ-SEC
                  END-PERFORM
                END-IF
                MOVE    "tbl_santei"        TO  MCP-TABLE
                MOVE    "key3"              TO  MCP-PATHNAME
                PERFORM 900-CLOSE-SEC
      *
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE       100     TO     WRK-JIDO-SETCOMATFLG
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "初回算定　　　月　　日"
                                   TO     WK-FREE-NAME2(IDZ)
      *
                MOVE    WRK-HAIJRT-YMD(5:2)   TO  DAY-IDX2
                MOVE    TBL-DAY(DAY-IDX2)
                                   TO  WK-FREE-NAME2(IDZ)(11:4)
                MOVE    WRK-HAIJRT-YMD(7:2)   TO  DAY-IDX2
                MOVE    TBL-DAY(DAY-IDX2)
                                   TO  WK-FREE-NAME2(IDZ)(17:4)
      *
                IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                  MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                  MOVE  WRK-JIDO-SETCOMATFLG
                                   TO     WK-FREE-SETCOMATFLG2(IDZ)
                END-IF
      *
                IF   WRK-HAIJRT-KAISU  NOT =  ZERO
                AND  WRK-HAIJRT-KAISU     <=   31
                  ADD        1       TO     IDZ
      *
                  MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE     "通算実施回数（　　回）"
                                     TO     WK-FREE-NAME2(IDZ)
                  MOVE      WRK-HAIJRT-KAISU TO  DAY-IDX2
                  MOVE      TBL-DAY(DAY-IDX2)
                                     TO     WK-FREE-NAME2(IDZ)(15:4)
      *
                  IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                    MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                    MOVE  WRK-JIDO-SETCOMATFLG
                                     TO     WK-FREE-SETCOMATFLG2(IDZ)
                  END-IF
                END-IF
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-HAIJRT-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    退院時共同指導料２があるか判定
      *****************************************************************
       2001-TAIKYO-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-TAIKYO-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE       108     TO     WRK-JIDO-SETCOMATFLG
                MOVE       10      TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-TAIKYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    訪問看護指示料があるか判定
      *****************************************************************
       2001-HOUSIJI-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-HOUSIJI-COMMENT   =    "1"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       11      TO     WRK-JIDO-SETCOMATFLG
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-HOUSIJI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅血液透析頻回指導管理料があるか判定
      *    在宅血液透析指導管理料算定日の歴月で２月以内か判定
      *****************************************************************
       2001-HINKAI-HANTEI-SEC          SECTION.
      *
           IF   RECE64-FLG-HINKAI-COMMENT   =    "1"
      *
             MOVE    ZERO    TO   WRK-DAY
             PERFORM  VARYING   IDX3   FROM  1  BY  1
                       UNTIL    IDX3  >   31
                 IF   RECE64-KAISU(1 IDX3)  NOT =  ZERO
                        MOVE  IDX3  TO  WRK-DAY
                        MOVE  31    TO  IDX3
                 END-IF
             END-PERFORM
      *
             MOVE     ZERO     TO     FLG-HINKAI-COMMENT
             MOVE     ZERO     TO     WRK-TOSEKIYMD
      *
      *      算定履歴検索
             INITIALIZE                  SANTEI-REC
             MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE62-PTID         TO  SANTEI-PTID
             MOVE    CONS-TOSEKI         TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM   900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                    MOVE    "tbl_santei"        TO  MCP-TABLE
                    MOVE    "key3"              TO  MCP-PATHNAME
                    PERFORM        900-SANTEI-READ-SEC
                    PERFORM   UNTIL  ( FLG-SANTEI         =   1 ) OR
                                     ( FLG-HINKAI-COMMENT =   1 )
      *
                      IF   SANTEI-SRYYM  <=  RECE62-SRYYM
      ***              MOVE   SANTEI-SRYYM     TO  WRK-FSANTEIYMD(1:6)
      ***              MOVE   SANTEI-MSANTEID  TO  WRK-FSANTEIYMD(7:2)
                       MOVE   SANTEI-FSANTEIYMD
                                               TO  WRK-FSANTEIYMD
      *
                       INITIALIZE                  STS-AREA-DAY
                       INITIALIZE                  LNK-DAY6-AREA
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    WRK-FSANTEIYMD  TO  LNK-DAY6-KIJUN
                       MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
                       MOVE     2              TO  LNK-DAY6-ZOGEN
                       CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN TO  WRK-AFTER2
      *
                       MOVE    RECE62-SRYYM    TO  WRK-HINKAIYMD(1:6)
                       MOVE    WRK-DAY         TO  WRK-HINKAIYMD(7:2)
      *
                       IF     WRK-DAY      NOT  =  ZERO
                       AND    WRK-FSANTEIYMD   <=  WRK-HINKAIYMD
                         IF ((WRK-HINKAIYMD    <   WRK-AFTER2) AND
                             (RECE62-SRYYM     <= "201003"   ))
                         OR ( RECE62-SRYYM     >= "201004"    )
                             MOVE    1    TO   FLG-HINKAI-COMMENT
                             MOVE  WRK-FSANTEIYMD
                                          TO   WRK-TOSEKIYMD
                         END-IF
                       END-IF
                      END-IF
      * 
                      MOVE    "tbl_santei"     TO  MCP-TABLE
                      MOVE    "key3"           TO  MCP-PATHNAME
                      PERFORM        900-SANTEI-READ-SEC
                    END-PERFORM
             END-IF
      *
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
             IF      FLG-HINKAI-COMMENT    =      1
                 INITIALIZE            WRK-JIDO-SETCOM-AREA
                 MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                 MOVE       12      TO     WRK-JIDO-SETCOMATFLG
                 MOVE       99      TO     FLG-COMMENT
                 PERFORM    20011-HINKAI-COMMENT-SEC
             END-IF
      *
           END-IF
      *
           .
       2001-HINKAI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅血液透析頻回指導管理料のコメント編集
      *****************************************************************
       20011-HINKAI-COMMENT-SEC          SECTION.
      *
             PERFORM   200111-RECE65-TAIHI-SEC
      *
      *      ＲＥＣＥ０５ファイルの内容をテーブルに退避
             MOVE    ZERO                 TO  IDX-RECE65
             MOVE    SPACE                TO  WRK-RECE65XX-REC
             INITIALIZE                       WRK-RECE65XX-REC
      *      コメントの挿入
             COMPUTE  MAX-MEISAISU        =   RECE64-MEISAISU  +  1
      *
             MOVE    "810000001"
                           TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO  TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    16    TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "初回指導管理算定日　　　月　　日"
                           TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
             MOVE    WRK-TOSEKIYMD(5:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(21:4)
             MOVE    WRK-TOSEKIYMD(7:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(27:4)
      *
             IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
               MOVE  "2"   TO  WRK-RECE65XX-MEISKYFLG(MAX-MEISAISU 1)
               MOVE  WRK-JIDO-SETCOMATFLG
                           TO  WRK-RECE65XX-SETCOMATFLG(MAX-MEISAISU 1)
             END-IF
      *
             PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-HINKAI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    往診・特別往診があるか判定
      *****************************************************************
       2001-OSIN-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-OSIN-COMMENT   =    "1"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE       6       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-OSIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    介護職員等喀痰吸引等指示料があるか判定
      *****************************************************************
       2001-KAISYOKU-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-KAISYOKU-COMMENT   =    "1"
             MOVE    ZERO                TO  FLG-KAISYOKU-COMMENT
             MOVE    SPACE               TO  WRK-KAISYOKU-YMD
             INITIALIZE                      SANTEI-REC
             MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE62-PTID         TO  SANTEI-PTID
             MOVE    CONS-KAISYOKU       TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI           =   1 )
                             OR   ( FLG-KAISYOKU-COMMENT =   1 )
      *
                   IF   SANTEI-SRYYM  <  RECE62-SRYYM
                     MOVE  SANTEI-SRYYM     TO  WRK-KAISYOKU-YMD(1:6)
                     MOVE  SANTEI-MSANTEID  TO  WRK-KAISYOKU-YMD(7:2)
                     MOVE    1              TO  FLG-KAISYOKU-COMMENT
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             INITIALIZE                WRK-JIDO-SETCOM-AREA
             MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
             MOVE       24      TO     WRK-JIDO-SETCOMATFLG
             MOVE       99      TO     FLG-COMMENT
      *
             IF   WRK-KAISYOKU-YMD   NOT =  SPACE
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                IF   RECE62-SRYYM >= "201905"
                  MOVE       19      TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE   "前回交付年月日　　　　　年　　月　　日"
                                     TO     WK-FREE-NAME2(IDZ)
                  MOVE       21      TO     IDX41
                  MOVE       27      TO     IDX42
                  MOVE       33      TO     IDX48
                  MOVE       17      TO     IDX49
                ELSE
                  MOVE       17      TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE   "前回交付年月日　　　年　　月　　日"
                                     TO     WK-FREE-NAME2(IDZ)
                  MOVE       17      TO     IDX41
                  MOVE       23      TO     IDX42
                  MOVE       29      TO     IDX48
                END-IF
      *
                INITIALIZE                         STS-AREA-DAY
                INITIALIZE                         LNK-DAY2-AREA
                MOVE    "21"                   TO  LNK-DAY2-IRAI
                MOVE    WRK-KAISYOKU-YMD       TO  LNK-DAY2-YMD
                CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                               LNK-DAY2-AREA
                IF      STS-DAY-RC1            =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-D   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX48:4)
                   END-IF
      *
                   IF   RECE62-SRYYM >= "201905"
                      MOVE    LNK-DAY2-EDTYMD2-G
                                   TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                   END-IF
                END-IF
             ELSE
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                MOVE       6       TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "（初回交付）"  TO     WK-FREE-NAME2(IDZ)
             END-IF
      *
             IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
               MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
               MOVE  WRK-JIDO-SETCOMATFLG
                                TO     WK-FREE-SETCOMATFLG2(IDZ)
             END-IF
      *
             PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           END-IF
      *
           .
       2001-KAISYOKU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅持続陽圧呼吸療法指導管理料があるか判定
      *****************************************************************
       2001-YOUATU-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-YOUATU-COMMENT   =    "1"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       26      TO     WRK-JIDO-SETCOMATFLG
               MOVE       99      TO     FLG-COMMENT
               PERFORM    20011-YOUATU-COMMENT-SEC
           END-IF
      *
           .
       2001-YOUATU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅持続陽圧呼吸療法指導管理料のコメント編集
      *****************************************************************
       20011-YOUATU-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-SANTEI-DAY-SEC
      *
           MOVE     ZERO         TO  IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           ADD      1            TO  IDZ
           MOVE    "実施日　　　年　　月　　日"
                                 TO  WK-FREE-NAME2(IDZ)(1:26)
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    RECE62-SRYYM        TO  WRK-HENYMD(1:6)
           MOVE    "01"                TO  WRK-HENYMD(7:2)
           MOVE    WRK-HENYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF  STS-DAY-RC1             =   ZERO
             MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
             INITIALIZE                       ORCSKANACHKAREA
             MOVE    "2"                  TO  KANACHK-SYORI
             MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
             CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
             IF      KANACHK-RC      =   ZERO
                MOVE    KANACHK-OUT-INPUT(1:4)
                                  TO  WK-FREE-NAME2(IDZ)(9:4)
             END-IF
      *
             MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
             INITIALIZE                       ORCSKANACHKAREA
             MOVE    "2"                  TO  KANACHK-SYORI
             MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
             CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
             IF      KANACHK-RC      =   ZERO
                MOVE    KANACHK-OUT-INPUT(1:4)
                                  TO  WK-FREE-NAME2(IDZ)(15:4)
             END-IF
           END-IF
      *
           MOVE    TB-SANTEI-DAY(1)       TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)      TO  WK-FREE-NAME2(IDZ)(21:4)
      *
           MOVE    13                     TO  WK-FREE-YUKOKETA2(IDZ)
      *
           IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
             MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
             MOVE  WRK-JIDO-SETCOMATFLG
                              TO     WK-FREE-SETCOMATFLG2(IDZ)
           END-IF
      *
           PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           .
       20011-YOUATU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅患者共同診療料があるか判定
      *****************************************************************
       2001-ZAIKYO-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-ZAIKYO-COMMENT   =    "1"
             MOVE   CONS-ZAIKYO   TO   WRK-CHECK-SRYCD
             PERFORM       20011-ZAIKYO-CHECK-SEC
      *
             IF   WRK-ZAIKYO-YMD   NOT =  SPACE
      *
      *         在宅患者共同診療料開始日の剤検索
                MOVE       ZERO                 TO  WRK-ZAIKYO-KAISU
                MOVE       ZERO                 TO  FLG-ZAIKYOCOMARI
                PERFORM    20011-ZAIKYO-KENSAKU-SEC
                IF    FLG-ZAIKYOCOMARI  =  1
                  PERFORM  VARYING  IDX30  FROM  1  BY  1
                            UNTIL   IDX30   >   TBL-ZAIKYO-CHECK-MAX
                    INITIALIZE                      SANTEI-REC
                    MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
                    MOVE    RECE62-PTID         TO  SANTEI-PTID
                    MOVE    TBL-ZAIKYO-CHECK(IDX30)
                                                TO  SANTEI-SRYCD
                    MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                    MOVE    ZERO                TO  SANTEI-SRYKA
                    MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
                    MOVE    SANTEI-REC          TO  MCPDATA-REC
                    MOVE    "tbl_santei"        TO  MCP-TABLE
                    MOVE    "key3"              TO  MCP-PATHNAME
                    PERFORM 900-DBSELECT-SEC
                    IF      MCP-RC              =   ZERO
                      MOVE    "tbl_santei"      TO  MCP-TABLE
                      MOVE    "key3"            TO  MCP-PATHNAME
                      PERFORM  900-SANTEI-READ-SEC
                      PERFORM   UNTIL   FLG-SANTEI  =   1
      *
                        IF   SANTEI-SRYYM  <=  RECE62-SRYYM
                        AND  SANTEI-SRYYM  >=  WRK-ZAIKYO-YMD(1:6)
                          MOVE  WRK-ZAIKYO-YMD(7:2)  TO  IDX28
                          MOVE  WRK-ZAIKYO-YMD9(7:2) TO  IDX29
                          PERFORM  VARYING  IDX26  FROM  31  BY  -1
                                    UNTIL   IDX26   <     1
                            IF   SANTEI-DAY(IDX26)  NOT =  ZERO
                              IF ((SANTEI-SRYYM =
                                          WRK-ZAIKYO-YMD(1:6) ) AND
                                  (IDX26        < IDX28       ))
                              OR ((SANTEI-SRYYM =
                                          WRK-ZAIKYO-YMD9(1:6)) AND
                                  (IDX26       >= IDX29       ))
                                CONTINUE
                              ELSE
                                ADD    1    TO   WRK-ZAIKYO-KAISU
                              END-IF
                            END-IF
                          END-PERFORM
                        END-IF
      *
                        MOVE    "tbl_santei"    TO  MCP-TABLE
                        MOVE    "key3"          TO  MCP-PATHNAME
                        PERFORM  900-SANTEI-READ-SEC
                      END-PERFORM
                    END-IF
                    MOVE    "tbl_santei"        TO  MCP-TABLE
                    MOVE    "key3"              TO  MCP-PATHNAME
                    PERFORM 900-CLOSE-SEC
                  END-PERFORM
                END-IF
      *
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE       29      TO     WRK-JIDO-SETCOMATFLG
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "初回算定　　　月　　日"
                                   TO     WK-FREE-NAME2(IDZ)
      *
                MOVE    WRK-ZAIKYO-YMD(5:2)   TO  DAY-IDX2
                MOVE    TBL-DAY(DAY-IDX2)
                                   TO  WK-FREE-NAME2(IDZ)(11:4)
                MOVE    WRK-ZAIKYO-YMD(7:2)   TO  DAY-IDX2
                MOVE    TBL-DAY(DAY-IDX2)
                                   TO  WK-FREE-NAME2(IDZ)(17:4)
      *
                IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                  MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                  MOVE  WRK-JIDO-SETCOMATFLG
                                   TO     WK-FREE-SETCOMATFLG2(IDZ)
                END-IF
      *
                IF   WRK-ZAIKYO-KAISU  NOT =  ZERO
                AND  WRK-ZAIKYO-KAISU     <=   31
                  ADD        1       TO     IDZ
      *
                  MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE     "通算実施回数（　　回）"
                                     TO     WK-FREE-NAME2(IDZ)
                  MOVE      WRK-ZAIKYO-KAISU TO  DAY-IDX2
                  MOVE      TBL-DAY(DAY-IDX2)
                                     TO     WK-FREE-NAME2(IDZ)(15:4)
      *
                  IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                    MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                    MOVE  WRK-JIDO-SETCOMATFLG
                                     TO     WK-FREE-SETCOMATFLG2(IDZ)
                  END-IF
                END-IF
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-ZAIKYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    
      *****************************************************************
       20011-ZAIKYO-CHECK-SEC          SECTION.
      *
           MOVE    RECE62-SRYYM        TO  WRK-ZAIKYO-YMD9(1:6)
           MOVE    "99"                TO  WRK-ZAIKYO-YMD9(7:2)
      *
           MOVE    ZERO                TO  FLG-ZAIKYO-COMMENT
           MOVE    SPACE               TO  WRK-ZAIKYO-YMD
           MOVE    ZERO                TO  FLG-RETRY
           INITIALIZE                      SANTEI-REC
           MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE62-PTID         TO  SANTEI-PTID
           MOVE    WRK-CHECK-SRYCD     TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"    TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM  900-SANTEI-READ-SEC
               PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                           OR   ( FLG-ZAIKYO-COMMENT  =   1 )
      *
                 IF   SANTEI-SRYYM  <=  RECE62-SRYYM
                   MOVE  SANTEI-SRYYM     TO  WRK-ZAIKYO-YMD(1:6)
                   MOVE  SANTEI-MSANTEID  TO  WRK-ZAIKYO-YMD(7:2)
                   MOVE    1              TO  FLG-ZAIKYO-COMMENT
      *
                   IF   SANTEI-SRYYM  =  RECE62-SRYYM
                     MOVE   ZERO  TO  FLG-CHECK-MAE
                     MOVE   ZERO  TO  FLG-CHECK-ATO
                     MOVE  WRK-ZAIKYO-YMD(7:2)  TO  IDX28
                     PERFORM  VARYING  IDX26  FROM   1  BY  1
                               UNTIL   IDX26   >    31
                       IF  RECE64-KAISU(1 IDX26)  NOT =  ZERO
                         IF    IDX26  <  IDX28
                           MOVE    1   TO  FLG-CHECK-MAE
                         ELSE
                           MOVE    1   TO  FLG-CHECK-ATO
                         END-IF
                       END-IF
                     END-PERFORM
                     IF  ((FLG-CHECK-MAE = 1   ) AND
                          (FLG-CHECK-ATO = ZERO))
                     OR  ((FLG-CHECK-MAE = 1   ) AND
                          (FLG-CHECK-ATO = 1   ))
                       MOVE    WRK-ZAIKYO-YMD
                                       TO  WRK-ZAIKYO-YMD9
                       MOVE    SPACE   TO  WRK-ZAIKYO-YMD
                       MOVE    1       TO  FLG-RETRY
                     END-IF
                   END-IF
                 END-IF
      *
                 MOVE    "tbl_santei"  TO  MCP-TABLE
                 MOVE    "key3"        TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF   FLG-RETRY =  1
             MOVE    ZERO                TO  FLG-ZAIKYO-COMMENT
             MOVE    SPACE               TO  WRK-ZAIKYO-YMD
             MOVE    ZERO                TO  FLG-RETRY
             INITIALIZE                      SANTEI-REC
             MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE62-PTID         TO  SANTEI-PTID
             MOVE    WRK-CHECK-SRYCD     TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"    TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM  900-SANTEI-READ-SEC
               PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                           OR   ( FLG-ZAIKYO-COMMENT  =   1 )
      *
                 IF   SANTEI-SRYYM  <   RECE62-SRYYM
                   MOVE  SANTEI-SRYYM     TO  WRK-ZAIKYO-YMD(1:6)
                   MOVE  SANTEI-MSANTEID  TO  WRK-ZAIKYO-YMD(7:2)
                   MOVE    1              TO  FLG-ZAIKYO-COMMENT
                 END-IF
      *
                 MOVE    "tbl_santei"  TO  MCP-TABLE
                 MOVE    "key3"        TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
               END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
       20011-ZAIKYO-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    
      *****************************************************************
       20011-ZAIKYO-CHECK2-SEC          SECTION.
      *
      *    算定履歴検索
           MOVE    SPACE               TO  WRK-ZAIKYO-YMD2
           INITIALIZE                      SANTEI-REC
           MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE62-PTID         TO  SANTEI-PTID
           MOVE    RECE62-SRYYM        TO  SANTEI-SRYYM
           MOVE    WRK-CHECK-SRYCD     TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM  900-SANTEI-INV-SEC
           IF       FLG-SANTEI        =   ZERO
             MOVE   SANTEI-SRYYM       TO  WRK-ZAIKYO-YMD2(1:6)
             MOVE   SANTEI-MSANTEID    TO  WRK-ZAIKYO-YMD2(7:2)
             IF     WRK-ZAIKYO-YMD     <=  WRK-ZAIKYO-YMD2
               MOVE   ZERO  TO  FLG-CHECK-MAE
               MOVE   ZERO  TO  FLG-CHECK-ATO
               MOVE   WRK-ZAIKYO-YMD2(7:2)  TO  IDX28
               PERFORM  VARYING  IDX26  FROM   1  BY  1
                         UNTIL   IDX26   >    31
                 IF  RECE64-KAISU(1 IDX26)  NOT =  ZERO
                   IF    IDX26  <=  IDX28
                     MOVE    1   TO  FLG-CHECK-MAE
                   ELSE
                     MOVE    1   TO  FLG-CHECK-ATO
                   END-IF
                 END-IF
               END-PERFORM
               IF    FLG-CHECK-MAE = ZERO
                 MOVE    SPACE   TO  WRK-ZAIKYO-YMD
                 MOVE    SPACE   TO  WRK-ZAIKYO-YMD2
               END-IF
             ELSE
                 MOVE    SPACE   TO  WRK-ZAIKYO-YMD2
             END-IF
           END-IF
      *
           .
       20011-ZAIKYO-CHECK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅患者共同診療料開始日の剤検索
      *****************************************************************
       20011-ZAIKYO-KENSAKU-SEC          SECTION.
      *
           MOVE    ZERO                  TO  FLG-ZAIKYO-OK
           MOVE    SPACE                 TO  WRK-ZAIKYO-NYUGAIKBN
           MOVE    SPACE                 TO  WRK-ZAIKYO-SRYKA
           MOVE    ZERO                  TO  WRK-ZAIKYO-ZAINUM
      *
           PERFORM   VARYING   IDX15    FROM   2   BY  -1
                      UNTIL   (IDX15          <    1)
                       OR     (FLG-ZAIKYO-OK  =    1)
             INITIALIZE                      SRYACT-REC
             MOVE    RECE62-HOSPNUM      TO  SRY-HOSPNUM
             MOVE    IDX15               TO  SRY-NYUGAIKBN
             MOVE    RECE62-PTID         TO  SRY-PTID
             MOVE    WRK-ZAIKYO-YMD(1:6) TO  SRY-SRYYM
             MOVE    CONS-ZAIKYO         TO  SRY-SRYCD (1)
             MOVE    CONS-ZAIKYO         TO  SRY-SRYCD (2)
             MOVE    CONS-ZAIKYO         TO  SRY-SRYCD (3)
             MOVE    CONS-ZAIKYO         TO  SRY-SRYCD (4)
             MOVE    CONS-ZAIKYO         TO  SRY-SRYCD (5)
             MOVE    SRYACT-REC          TO  MCPDATA-REC
             MOVE    "tbl_sryact"        TO  MCP-TABLE
             MOVE    "key7"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"      TO  MCP-TABLE
               MOVE    "key7"            TO  MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL   FLG-SRYACT     =   1
                         OR     FLG-ZAIKYO-OK  =   1
                 INITIALIZE                  SRYACCT-REC
                 MOVE    SRY-HOSPNUM     TO  ACCT-HOSPNUM
                 MOVE    SRY-NYUGAIKBN   TO  ACCT-NYUGAIKBN
                 MOVE    SRY-PTID        TO  ACCT-PTID
                 MOVE    SRY-SRYKA       TO  ACCT-SRYKA
                 MOVE    SRY-SRYYM       TO  ACCT-SRYYM
                 MOVE    SRY-ZAINUM      TO  ACCT-ZAINUM
                 MOVE    SRYACCT-REC     TO  MCPDATA-REC
                 MOVE    "tbl_sryacct"   TO  MCP-TABLE
                 MOVE    "key31"         TO  MCP-PATHNAME
                 PERFORM 900-DBSELECT-SEC
                 IF      MCP-RC          =   ZERO
                    MOVE "tbl_sryacct"   TO  MCP-TABLE
                    MOVE "key31"         TO  MCP-PATHNAME
                    PERFORM 900-DBFETCH-SEC
                    IF      MCP-RC       =   ZERO
                      MOVE  MCPDATA-REC  TO  SRYACCT-REC
      *
                      MOVE  WRK-ZAIKYO-YMD(7:2)
                                         TO  DAY-IDX2
                      IF   ACCT-DAY(1 DAY-IDX2)  NOT =  ZERO
                        MOVE   1   TO  FLG-ZAIKYO-OK
                        MOVE   ACCT-NYUGAIKBN
                                   TO  WRK-ZAIKYO-NYUGAIKBN
                        MOVE   ACCT-SRYKA
                                   TO  WRK-ZAIKYO-SRYKA
                        MOVE   ACCT-ZAINUM
                                   TO  WRK-ZAIKYO-ZAINUM
                      END-IF
                    END-IF
                 END-IF
                 MOVE    "tbl_sryacct"   TO  MCP-TABLE
                 MOVE    "key31"         TO  MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
      *
                 MOVE    "tbl_sryact"    TO  MCP-TABLE
                 MOVE    "key7"          TO  MCP-PATHNAME
                 PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
             END-IF
             MOVE    "tbl_sryact"        TO  MCP-TABLE
             MOVE    "key7"              TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
           END-PERFORM
      *
      *++++
      *
           IF      WRK-ZAIKYO-ZAINUM    NOT =    ZERO
              INITIALIZE                         SRYACT-REC
              MOVE    RECE62-HOSPNUM         TO  SRY-HOSPNUM
              MOVE    WRK-ZAIKYO-NYUGAIKBN   TO  SRY-NYUGAIKBN
              MOVE    RECE62-PTID            TO  SRY-PTID
              MOVE    WRK-ZAIKYO-SRYKA       TO  SRY-SRYKA
              MOVE    WRK-ZAIKYO-YMD(1:6)    TO  SRY-SRYYM
              MOVE    WRK-ZAIKYO-ZAINUM      TO  SRY-ZAINUM
              MOVE    SRYACT-REC             TO  MCPDATA-REC
              MOVE    "tbl_sryact"           TO  MCP-TABLE
              MOVE    "key2"                 TO  MCP-PATHNAME
              PERFORM 900-DBSELECT-SEC
              IF      MCP-RC                 =   ZERO
                 MOVE    "tbl_sryact"        TO  MCP-TABLE
                 MOVE    "key2"              TO  MCP-PATHNAME
                 PERFORM  900-SRYACT-READ-SEC
                 PERFORM  UNTIL  (FLG-SRYACT       =  1)
                           OR    (FLG-ZAIKYOCOMARI =  1)
                   PERFORM  VARYING  IDX12  FROM  1  BY  1
                             UNTIL  (IDX12             >    5   )
                              OR    (SRY-SRYCD(IDX12)  =   SPACE)
                              OR    (FLG-ZAIKYOCOMARI  =    1   )
                      IF  SRY-SRYCD(IDX12)  =   "840000086"
                         MOVE    1    TO   FLG-ZAIKYOCOMARI
                      END-IF
                   END-PERFORM
                   MOVE    "tbl_sryact"      TO  MCP-TABLE
                   MOVE    "key2"            TO  MCP-PATHNAME
                   PERFORM   900-SRYACT-READ-SEC
                 END-PERFORM
              END-IF
              MOVE    "tbl_sryact"           TO  MCP-TABLE
              MOVE    "key2"                 TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
       20011-ZAIKYO-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅半固形栄養経管栄養法指導管理料があるか判定
      *****************************************************************
       2001-ZAIHANKO-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-ZAIHANKO-COMMENT   NOT =  SPACE
               MOVE    SPACE                TO  WRK-ZAIHANKO-FSANTEIYMD
               INITIALIZE                       SANTEI-REC
               MOVE    RECE62-HOSPNUM       TO  SANTEI-HOSPNUM
               MOVE    RECE62-PTID          TO  SANTEI-PTID
               MOVE    RECE62-SRYYM         TO  SANTEI-SRYYM
               EVALUATE  RECE64-FLG-ZAIHANKO-COMMENT
                 WHEN      "9"
                   MOVE  CONS-ZAIHANKO      TO  SANTEI-SRYCD
               END-EVALUATE
               MOVE    ZERO                 TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                 TO  SANTEI-SRYKA
               MOVE    ZERO                 TO  SANTEI-HKNCOMBINUM
               MOVE    SANTEI-REC           TO  MCPDATA-REC
               MOVE    "tbl_santei"         TO  WRK-MCP-TABLE
               MOVE    "key"                TO  WRK-MCP-PATHNAME
               PERFORM  900-SANTEI-INV-SEC
               IF       FLG-SANTEI        =   ZERO
                  MOVE  SANTEI-FSANTEIYMD   TO  WRK-ZAIHANKO-FSANTEIYMD
               END-IF
      *
               IF   WRK-ZAIHANKO-FSANTEIYMD   NOT =  SPACE
      *
                  INITIALIZE                WRK-JIDO-SETCOM-AREA
                  MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                  MOVE       19      TO     WRK-JIDO-SETCOMATFLG
                  MOVE       99      TO     FLG-COMMENT
      *
                  MOVE      ZERO     TO     IDZ
                  INITIALIZE                WRK-FREE-AREA2
      *
                  ADD        1       TO     IDZ
      *
                  MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE   "初回算定　　　月　　日"
                                     TO     WK-FREE-NAME2(IDZ)
                  MOVE   WRK-ZAIHANKO-FSANTEIYMD(5:2)   TO  DAY-IDX2
                  MOVE   TBL-DAY(DAY-IDX2)
                                     TO     WK-FREE-NAME2(IDZ)(11:4)
                  MOVE   WRK-ZAIHANKO-FSANTEIYMD(7:2)   TO  DAY-IDX2
                  MOVE   TBL-DAY(DAY-IDX2)
                                     TO     WK-FREE-NAME2(IDZ)(17:4)
      *
                  IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                    MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                    MOVE  WRK-JIDO-SETCOMATFLG
                                     TO     WK-FREE-SETCOMATFLG2(IDZ)
                  END-IF
      *
                  PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
               END-IF
           END-IF
      *
           .
       2001-ZAIHANKO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    磁気による膀胱等刺激法があるか判定
      *****************************************************************
       2001-BOKOHO-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-BOKOHO-COMMENT   =    "1"
             MOVE    RECE62-SRYYM        TO  WRK-BOKOHO-YMD9(1:6)
             MOVE    "99"                TO  WRK-BOKOHO-YMD9(7:2)
      *
             MOVE    ZERO                TO  FLG-BOKOHO-COMMENT
             MOVE    SPACE               TO  WRK-BOKOHO-YMD
             MOVE    ZERO                TO  FLG-RETRY
             INITIALIZE                      SANTEI-REC
             MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE62-PTID         TO  SANTEI-PTID
             MOVE    CONS-BOKOHO1        TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                             OR   ( FLG-BOKOHO-COMMENT  =   1 )
      *
                   IF   SANTEI-SRYYM  <=  RECE62-SRYYM
                     MOVE  SANTEI-SRYYM     TO  WRK-BOKOHO-YMD(1:6)
                     MOVE  SANTEI-MSANTEID  TO  WRK-BOKOHO-YMD(7:2)
                     MOVE    1              TO  FLG-BOKOHO-COMMENT
      *
                     IF   SANTEI-SRYYM  =  RECE62-SRYYM
                       MOVE   ZERO  TO  FLG-CHECK-MAE
                       MOVE   ZERO  TO  FLG-CHECK-ATO
                       MOVE  WRK-BOKOHO-YMD(7:2)  TO  IDX28
                       PERFORM  VARYING  IDX26  FROM   1  BY  1
                                 UNTIL   IDX26   >    31
                         IF  RECE64-KAISU(1 IDX26)  NOT =  ZERO
                           IF    IDX26  <  IDX28
                             MOVE    1   TO  FLG-CHECK-MAE
                           ELSE
                             MOVE    1   TO  FLG-CHECK-ATO
                           END-IF
                         END-IF
                       END-PERFORM
                       IF  ((FLG-CHECK-MAE = 1   ) AND
                            (FLG-CHECK-ATO = ZERO))
                       OR  ((FLG-CHECK-MAE = 1   ) AND
                            (FLG-CHECK-ATO = 1   ))
                         MOVE    WRK-BOKOHO-YMD
                                         TO  WRK-BOKOHO-YMD9
                         MOVE    SPACE   TO  WRK-BOKOHO-YMD
                         MOVE    1       TO  FLG-RETRY
                       END-IF
                     END-IF
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             IF   FLG-RETRY =  1
               MOVE    ZERO                TO  FLG-BOKOHO-COMMENT
               MOVE    SPACE               TO  WRK-BOKOHO-YMD
               MOVE    ZERO                TO  FLG-RETRY
               INITIALIZE                      SANTEI-REC
               MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
               MOVE    RECE62-PTID         TO  SANTEI-PTID
               MOVE    CONS-BOKOHO1        TO  SANTEI-SRYCD
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-SRYKA
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                             OR   ( FLG-BOKOHO-COMMENT  =   1 )
      *
                   IF   SANTEI-SRYYM  <   RECE62-SRYYM
                     MOVE  SANTEI-SRYYM     TO  WRK-BOKOHO-YMD(1:6)
                     MOVE  SANTEI-MSANTEID  TO  WRK-BOKOHO-YMD(7:2)
                     MOVE    1              TO  FLG-BOKOHO-COMMENT
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
               END-IF
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
             END-IF
      *
             IF   WRK-BOKOHO-YMD   NOT =  SPACE
               MOVE    ZERO                TO  IDX27
               INITIALIZE                      WRK-BOKOHO-YMD2-AREA
               INITIALIZE                      SANTEI-REC
               MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
               MOVE    RECE62-PTID         TO  SANTEI-PTID
               MOVE    CONS-BOKOHO2        TO  SANTEI-SRYCD
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-SRYKA
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL   FLG-SANTEI  =   1
      *
                   IF   SANTEI-SRYYM  <=  RECE62-SRYYM
                   AND  SANTEI-SRYYM  >=  WRK-BOKOHO-YMD(1:6)
                     MOVE  WRK-BOKOHO-YMD(7:2)  TO  IDX28
                     MOVE  WRK-BOKOHO-YMD9(7:2) TO  IDX29
                     PERFORM  VARYING  IDX26  FROM  31  BY  -1
                               UNTIL   IDX26   <     1
                       IF  (SANTEI-DAY(IDX26)  NOT =  ZERO)
                       AND (IDX27              <    30    )
                         IF ((SANTEI-SRYYM = WRK-BOKOHO-YMD(1:6) ) AND
                             (IDX26        < IDX28               ))
                         OR ((SANTEI-SRYYM = WRK-BOKOHO-YMD9(1:6)) AND
                             (IDX26       >= IDX29               ))
      *                  初回実施日は通算実施日として記載しない
                         OR ((SANTEI-SRYYM = WRK-BOKOHO-YMD(1:6) ) AND
                             (IDX26        = IDX28               ))
                           CONTINUE
                         ELSE
                           ADD   1      TO  IDX27
                           MOVE  SANTEI-SRYYM
                                        TO  WRK-BOKOHO-YMD2(IDX27)(1:6)
                           MOVE  IDX26  TO  WRK-BOKOHO-YMD2(IDX27)(7:2)
                         END-IF
                       END-IF
                     END-PERFORM
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
               END-IF
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               INITIALIZE                WRK-JIDO-SETCOM-AREA
      ***      MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
      ***      MOVE       31      TO     WRK-JIDO-SETCOMATFLG
               MOVE       99      TO     FLG-COMMENT
      *
               MOVE      ZERO     TO     IDZ
               INITIALIZE                WRK-FREE-AREA2
      *
               ADD        1       TO     IDZ
      *
               MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
               MOVE   "初回実施　　　月　　日"
                                  TO     WK-FREE-NAME2(IDZ)
      *
               MOVE    WRK-BOKOHO-YMD(5:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                                  TO     WK-FREE-NAME2(IDZ)(11:4)
               MOVE    WRK-BOKOHO-YMD(7:2)   TO  DAY-IDX2
               MOVE    TBL-DAY(DAY-IDX2)
                                  TO     WK-FREE-NAME2(IDZ)(17:4)
      *
               IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                 MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                 MOVE  WRK-JIDO-SETCOMATFLG
                                  TO     WK-FREE-SETCOMATFLG2(IDZ)
               END-IF
      *
               IF   WRK-BOKOHO-YMD2(1) NOT =  SPACE
                 MOVE         1       TO     IDX27
                 PERFORM  VARYING  IDX26  FROM  30  BY  -1
                           UNTIL   IDX26   <     1
                  IF  WRK-BOKOHO-YMD2(IDX26)  NOT =  SPACE
                   ADD        1       TO     IDZ
      *
                   ADD        1       TO     IDX27
                   IF    IDX27   <    10
                     MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                     MOVE     "　回目　　　　月　　日"
                                        TO     WK-FREE-NAME2(IDZ)
                     MOVE      IDX27    TO     DAY-IDX2
                     MOVE      TBL-DAY(DAY-IDX2)(3:2)
                                        TO     WK-FREE-NAME2(IDZ)(1:2)
                   ELSE
                     MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                     MOVE     "　　回目　　　月　　日"
                                        TO     WK-FREE-NAME2(IDZ)
                     MOVE      IDX27    TO     DAY-IDX2
                     MOVE      TBL-DAY(DAY-IDX2)
                                        TO     WK-FREE-NAME2(IDZ)(1:4)
                   END-IF
                   MOVE      WRK-BOKOHO-YMD2(IDX26)(5:2) TO  DAY-IDX2
                   MOVE      TBL-DAY(DAY-IDX2)
                                        TO     WK-FREE-NAME2(IDZ)(11:4)
                   MOVE      WRK-BOKOHO-YMD2(IDX26)(7:2) TO  DAY-IDX2
                   MOVE      TBL-DAY(DAY-IDX2)
                                        TO     WK-FREE-NAME2(IDZ)(17:4)
      *
                   IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                     MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                     MOVE  WRK-JIDO-SETCOMATFLG
                                      TO     WK-FREE-SETCOMATFLG2(IDZ)
                   END-IF
                  END-IF
                 END-PERFORM
               END-IF
      *
               PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-BOKOHO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    高気圧酸素治療があるか判定
      *****************************************************************
       2001-KIATU-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-KIATU-COMMENT    NOT =  SPACE
             MOVE   CONS-KIATU3   TO   WRK-CHECK-SRYCD
             PERFORM       20011-ZAIKYO-CHECK-SEC
      *
             MOVE       SPACE               TO  WRK-SJIYMD
             MOVE       ZERO                TO  WRK-ZAIKYO-KAISU
             IF   WRK-ZAIKYO-YMD   NOT =  SPACE
                INITIALIZE                      SANTEI-REC
                MOVE    RECE62-HOSPNUM      TO  SANTEI-HOSPNUM
                MOVE    RECE62-PTID         TO  SANTEI-PTID
                EVALUATE  RECE64-FLG-KIATU-COMMENT
                  WHEN      "1"
                    MOVE    CONS-KIATU      TO  SANTEI-SRYCD
                  WHEN      "2"
                    MOVE    CONS-KIATU2     TO  SANTEI-SRYCD
                END-EVALUATE
                MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                MOVE    ZERO                TO  SANTEI-SRYKA
                MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
                MOVE    SANTEI-REC          TO  MCPDATA-REC
                MOVE    "tbl_santei"        TO  MCP-TABLE
                MOVE    "key3"              TO  MCP-PATHNAME
                PERFORM 900-DBSELECT-SEC
                IF      MCP-RC              =   ZERO
                  MOVE    "tbl_santei"      TO  MCP-TABLE
                  MOVE    "key3"            TO  MCP-PATHNAME
                  PERFORM  900-SANTEI-READ-SEC
                  PERFORM   UNTIL   FLG-SANTEI  =   1
      *
                    IF   SANTEI-SRYYM  <=  RECE62-SRYYM
                    AND  SANTEI-SRYYM  >=  WRK-ZAIKYO-YMD(1:6)
                      MOVE  WRK-ZAIKYO-YMD(7:2)  TO  IDX28
                      MOVE  WRK-ZAIKYO-YMD9(7:2) TO  IDX29
                      PERFORM  VARYING  IDX26  FROM  31  BY  -1
                                UNTIL   IDX26   <     1
                        IF   SANTEI-DAY(IDX26)  NOT =  ZERO
                          IF ((SANTEI-SRYYM =
                                      WRK-ZAIKYO-YMD(1:6) ) AND
                              (IDX26        < IDX28       ))
                          OR ((SANTEI-SRYYM =
                                      WRK-ZAIKYO-YMD9(1:6)) AND
                              (IDX26       >= IDX29       ))
                            CONTINUE
                          ELSE
                            ADD   1       TO  WRK-ZAIKYO-KAISU
                            MOVE  SANTEI-SRYYM
                                          TO  WRK-SJIYMD(1:6)
                            MOVE  IDX26   TO  WRK-SJIYMD(7:2)
                          END-IF
                        END-IF
                      END-PERFORM
                    END-IF
      *
                    MOVE    "tbl_santei"    TO  MCP-TABLE
                    MOVE    "key3"          TO  MCP-PATHNAME
                    PERFORM  900-SANTEI-READ-SEC
                  END-PERFORM
                END-IF
                MOVE    "tbl_santei"        TO  MCP-TABLE
                MOVE    "key3"              TO  MCP-PATHNAME
                PERFORM 900-CLOSE-SEC
             END-IF
      *
             IF   WRK-ZAIKYO-KAISU  NOT =  ZERO
             AND  WRK-ZAIKYO-KAISU     <=   31
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE       35      TO     WRK-JIDO-SETCOMATFLG
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                IF    RECE64-FLG-KIATU-COMMENT  =  "1"
      *
                  ADD        1       TO     IDZ
      *
                  MOVE       9       TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE   "発症　　　月　　日"
                                     TO     WK-FREE-NAME2(IDZ)
      *
                  MOVE    WRK-ZAIKYO-YMD(5:2)  TO  DAY-IDX2
                  MOVE    TBL-DAY(DAY-IDX2)
                                     TO     WK-FREE-NAME2(IDZ)(7:4)
                  MOVE    WRK-ZAIKYO-YMD(7:2)  TO  DAY-IDX2
                  MOVE    TBL-DAY(DAY-IDX2)
                                     TO     WK-FREE-NAME2(IDZ)(13:4)
      *
                  IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                    MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                    MOVE  WRK-JIDO-SETCOMATFLG
                                     TO     WK-FREE-SETCOMATFLG2(IDZ)
                  END-IF
      *
                END-IF
      *
                ADD        1       TO     IDZ
      *
                MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "初回実施　　　月　　日"
                                   TO     WK-FREE-NAME2(IDZ)
      *
                MOVE    WRK-SJIYMD(5:2)   TO  DAY-IDX2
                MOVE    TBL-DAY(DAY-IDX2)
                                   TO     WK-FREE-NAME2(IDZ)(11:4)
                MOVE    WRK-SJIYMD(7:2)   TO  DAY-IDX2
                MOVE    TBL-DAY(DAY-IDX2)
                                   TO     WK-FREE-NAME2(IDZ)(17:4)
      *
                IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                  MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                  MOVE  WRK-JIDO-SETCOMATFLG
                                   TO     WK-FREE-SETCOMATFLG2(IDZ)
                END-IF
      *
                ADD        1       TO     IDZ
      *
                MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "通算実施回数（　　回）"
                                   TO     WK-FREE-NAME2(IDZ)
                MOVE    WRK-ZAIKYO-KAISU  TO  DAY-IDX2
                MOVE    TBL-DAY(DAY-IDX2)
                                   TO     WK-FREE-NAME2(IDZ)(15:4)
      *
                IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                  MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                  MOVE  WRK-JIDO-SETCOMATFLG
                                   TO     WK-FREE-SETCOMATFLG2(IDZ)
                END-IF
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-KIATU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    人工腎臓等のコメントがあるか判定
      *****************************************************************
       2001-JINZOUETC-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-JINZOU-COMMENT   =    "1"
             IF   RECE62-SRYYM  >= "201810"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       41      TO     WRK-JIDO-SETCOMATFLG
               MOVE       1       TO     WRK-JIDO-JYOUGEFLG
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
             ELSE
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
             END-IF
           END-IF
      *
           .
       2001-JINZOUETC-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    局所陰圧閉鎖処置のコメントがあるか判定
      *****************************************************************
       2001-KYOKUIN-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-KYOKUIN-COMMENT   =    "1"
             IF   RECE62-SRYYM  >= "201810"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       42      TO     WRK-JIDO-SETCOMATFLG
               MOVE       1       TO     WRK-JIDO-JYOUGEFLG
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
             ELSE
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
             END-IF
           END-IF
      *
           .
       2001-KYOKUIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術日のコメントがあるか判定
      *****************************************************************
       2001-SHUJUTSU-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-SHUJUTU-COMMENT   =    SPACE
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE       1       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
                PERFORM    2099-RECE64-TENSOU-SEC
           END-IF
      *
           .
       2001-SHUJUTSU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    麻酔日のコメントがあるか判定
      *****************************************************************
       2001-MASUI-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-MASUI-COMMENT   =    SPACE
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE       8       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
                PERFORM    2099-RECE64-TENSOU-SEC
           END-IF
      *
           .
       2001-MASUI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    （検選）のコメントがあるか判定
      *****************************************************************
       2001-KENSEN-HANTEI-SEC          SECTION.
      *
           IF     RECE64-SAFLG   =    "2"
               MOVE       99      TO     FLG-COMMENT
               PERFORM    20011-KENSENTOU-COMMENT-SEC
           END-IF
      *
           .
       2001-KENSEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    （検選）等のコメント編集
      *****************************************************************
       20011-KENSENTOU-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-RECE65-TAIHI-SEC
      *
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           PERFORM
               UNTIL  ( FLG-RECE65           =   1                   )
               OR     ( RECE64-KEY1      NOT =   RECE65-KEY1         )
               OR     ( RECE64-ZAINUM    NOT =   RECE65-ZAINUM       )
               OR     ( RECE64-ZAIREN    NOT =   RECE65-ZAIREN       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   5     )
                         OR      ( RECE65-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE65
                    MOVE   RECE65-SRYACT-TBL(IDX)
                             TO   WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
      *++++++
                    IF    IDX-RECE65  =  RECE64-MEISAISU
      *
                      IF  WRK-RECE65XX-SRYCD(IDX-RECE65 1) = "810000001"
                      OR  WRK-RECE65XX-SRYCD(IDX-RECE65 1)(1:3)  = "008"
      *
                        COMPUTE   IDV  =
                         (WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) * 2) + 1
      *
                        IF   WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) <=  36
                          MOVE    "　×"   TO
                           WRK-RECE65XX-INPUTCOMENT(IDX-RECE65 1)(IDV:4)
                        END-IF
                        IF   WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) <=  96
                          MOVE    "　×"   TO
                           WRK-RECE65XX-NAME(IDX-RECE65 1)(IDV:4)
                        END-IF
      *
                        COMPUTE   IDV  =  IDV  +  4
      *
                        MOVE    RECE64-ZAIKAISU   TO   DAY-IDX2
                        PERFORM 20011-KAISU-HENSYU-SEC
                        IF   WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) <=  36
                          MOVE    WRK-HENSYU-KAISU2  TO
                           WRK-RECE65XX-INPUTCOMENT(IDX-RECE65 1)(IDV:4)
                        END-IF
                        IF   WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) <=  96
                          MOVE    WRK-HENSYU-KAISU2  TO
                           WRK-RECE65XX-NAME(IDX-RECE65 1)(IDV:4)
                          COMPUTE WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) =
                                  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1) +
                                  4
                        END-IF
      *
                      END-IF
      *
                    END-IF
      *++++++
               END-PERFORM
      *
               PERFORM 990-RECE65-NEXT-SEC
      *
           END-PERFORM
      *
      *    コメントを含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
                    INITIALIZE                        RECE659-REC
                    MOVE    WRK-RECE65-KEY       TO   RECE659-KEY
                    ADD     1                    TO   WRK-RENNUM
                    MOVE    WRK-RENNUM           TO   RECE659-RENNUM
                    MOVE    WRK-RECE65-KOHBUNRI  TO   RECE659-KOHBUNRI
                    MOVE    WRK-RECE65-PTNUM     TO   RECE659-PTNUM
                    MOVE    WRK-RECE65-SRYKA     TO   RECE659-SRYKA
                    MOVE    WRK-RECE65-SRYKBN    TO   RECE659-SRYKBN
                    MOVE    WRK-RECE65-SRYSYUKBN TO   RECE659-SRYSYUKBN
                    MOVE    WRK-RECE65-JIHIMONEY TO   RECE659-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE65   =   1)
                       ADD         1             TO    IDX-RECE65
                       IF (WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE)
                       OR (IDX-RECE65                       >  100 )
                           MOVE    1             TO    FLG-WRKRECE65
                       ELSE
                           MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                            TO   RECE659-SRYACT-TBL(IDX)
                           ADD   1          TO   WRK-CNT-MEISAI
                       END-IF
                    END-PERFORM
                    IF   RECE659-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE659-REC
                       ADD     1            TO   CNT-RECE659
                    END-IF
           END-PERFORM
      *
           MOVE    RECE64-REC        TO      RECE649-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE649-MEISAISU
           WRITE   RECE649-REC
           ADD     1                 TO      CNT-RECE649
      *
           .
       20011-KENSENTOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数編集
      *****************************************************************
       20011-KAISU-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE                TO  WRK-HENSYU-KAISU
           MOVE    SPACE                TO  WRK-HENSYU-KAISU2
      *
           MOVE    DAY-IDX2             TO  WRK-HENSYU-KAISU
           IF      WRK-HENSYU-KAISU(1:1) =  "0"
               MOVE      SPACE          TO  WRK-HENSYU-KAISU(1:1)
           END-IF
      *
           INITIALIZE                       ORCSKANACHKAREA
           MOVE    "2"                  TO  KANACHK-SYORI
           MOVE    WRK-HENSYU-KAISU     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   ZERO
                MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WRK-HENSYU-KAISU2
           END-IF
      *
           .
       20011-KAISU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査（ＢＮＰ、ＮＴ−ｐｒｏＢＮＰ、ＨＡＮＰ）があるか判定
      *****************************************************************
       2001-KENSA-HANTEI-SEC          SECTION.
      *
           IF  ((RECE62-SRYYM  >= "201804"       ) AND
                (RECE64-FLG-KENSA-COMMENT  =  "1") AND
                (CNT-KENSA                >=   2 ))
           OR  ((RECE62-SRYYM  <= "201803"       ) AND
                (RECE64-FLG-KENSA-COMMENT  =  "1"))
               INITIALIZE            WRK-JIDO-SETCOM-AREA
      ***      MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
      ***      MOVE       15      TO     WRK-JIDO-SETCOMATFLG
               MOVE       7       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-KENSA-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間外緊急院内検査加算・外来迅速検体検査加算があるか判定
      *****************************************************************
       2001-KENKSN-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-KENKSN-COMMENT  =  "1"
               MOVE    ZERO                 TO  FLG-OK2
               PERFORM  20011-NYUINRRK-KENSAKU-SEC
      *
               IF   FLG-OK2  =  1
                  INITIALIZE                WRK-JIDO-SETCOM-AREA
                  MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                  MOVE       43      TO     WRK-JIDO-SETCOMATFLG
                  MOVE       99      TO     FLG-COMMENT
      *
                  PERFORM  200111-RECE65-TAIHI-SEC
      *
      *           ＲＥＣＥ０５ファイルの内容をテーブルに退避
                  MOVE    ZERO                 TO  IDX-RECE65
                  MOVE    SPACE                TO  WRK-RECE65XX-REC
                  INITIALIZE                       WRK-RECE65XX-REC
      *
                  MOVE    RECE64-MEISAISU      TO  MAX-MEISAISU
      *           「引き続き入院」
                  MOVE    "820100129"          TO  WRK-TENSU-SRYCD
                  MOVE    RECE62-SRYYM         TO  WRK-TENSU-SRYYM
                  PERFORM 900-TENSU-TBL-SEC
      *
                  COMPUTE MAX-MEISAISU = MAX-MEISAISU + 1
                  MOVE  TNS-SRYCD
                             TO WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
                  MOVE  ZERO TO WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
                  MOVE  TNS-YUKOKETA
                             TO WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                  MOVE  TNS-NAME
                             TO WRK-RECE65XX-NAME(MAX-MEISAISU 1)
                                WRK-RECE65XX-INPUTCOMENT(MAX-MEISAISU 1)
                  MOVE  "2"  TO WRK-RECE65XX-MEISKYFLG(MAX-MEISAISU 1)
                  MOVE  WRK-JIDO-SETCOMATFLG
                             TO WRK-RECE65XX-SETCOMATFLG(MAX-MEISAISU 1)
      *
                  PERFORM 200111-SRYACTINF-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       2001-KENKSN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴検索
      *****************************************************************
       20011-NYUINRRK-KENSAKU-SEC          SECTION.
      *
           PERFORM  VARYING  IDX44  FROM  1  BY  1
                     UNTIL  (IDX44    >  31)
                      OR    (FLG-OK2  =   1)
             IF   RECE64-KAISU(1 IDX44)  NOT =  ZERO
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    RECE62-HOSPNUM      TO  PTNYUINRRK-HOSPNUM
               MOVE    RECE62-PTID         TO  PTNYUINRRK-PTID
               MOVE    RECE62-SRYYM        TO  PTNYUINRRK-NYUINYMD(1:6)
               MOVE    IDX44               TO  PTNYUINRRK-NYUINYMD(7:2)
               MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key39"             TO  MCP-PATHNAME
               PERFORM  900-DBSELECT-SEC
               IF      MCP-RC   =   ZERO
                 MOVE    "tbl_ptnyuinrrk"  TO  MCP-TABLE
                 MOVE    "key39"           TO  MCP-PATHNAME
                 PERFORM  900-PTNYUINRRK-READ-SEC
                 IF    FLG-PTNYUINRRK  =  ZERO
                    MOVE    1              TO  FLG-OK2
                 END-IF
               END-IF
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key39"             TO  MCP-PATHNAME
               PERFORM  900-CLOSE-SEC
             END-IF
           END-PERFORM
      *
           .
       20011-NYUINRRK-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＴ・ＭＲＩがあるか判定
      *****************************************************************
       2001-CTMRI-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-CTMRI-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE       2       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           END-IF
      *
           .
       2001-CTMRI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間外緊急院内画像診断加算があるか判定
      *****************************************************************
       2001-KINGAZ-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-KINGAZ-COMMENT  =  "1"
               MOVE    ZERO                 TO  FLG-OK2
               PERFORM  20011-NYUINRRK-KENSAKU-SEC
      *
               IF   FLG-OK2  =  1
                  INITIALIZE                WRK-JIDO-SETCOM-AREA
                  MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                  MOVE       44      TO     WRK-JIDO-SETCOMATFLG
                  MOVE       99      TO     FLG-COMMENT
      *
                  PERFORM  200111-RECE65-TAIHI-SEC
      *
      *           ＲＥＣＥ０５ファイルの内容をテーブルに退避
                  MOVE    ZERO                 TO  IDX-RECE65
                  MOVE    SPACE                TO  WRK-RECE65XX-REC
                  INITIALIZE                       WRK-RECE65XX-REC
      *
                  MOVE    RECE64-MEISAISU      TO  MAX-MEISAISU
      *           「引き続き入院」
                  MOVE    "820100129"          TO  WRK-TENSU-SRYCD
                  MOVE    RECE62-SRYYM         TO  WRK-TENSU-SRYYM
                  PERFORM 900-TENSU-TBL-SEC
      *
                  COMPUTE MAX-MEISAISU = MAX-MEISAISU + 1
                  MOVE  TNS-SRYCD
                             TO WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
                  MOVE  ZERO TO WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
                  MOVE  TNS-YUKOKETA
                             TO WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                  MOVE  TNS-NAME
                             TO WRK-RECE65XX-NAME(MAX-MEISAISU 1)
                                WRK-RECE65XX-INPUTCOMENT(MAX-MEISAISU 1)
                  MOVE  "2"  TO WRK-RECE65XX-MEISKYFLG(MAX-MEISAISU 1)
                  MOVE  WRK-JIDO-SETCOMATFLG
                             TO WRK-RECE65XX-SETCOMATFLG(MAX-MEISAISU 1)
      *
                  PERFORM 200111-SRYACTINF-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       2001-KINGAZ-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    精神科退院前訪問指導料があるか判定
      *****************************************************************
       2001-HOUMONSIDO-SEI-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-HOUSEI-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE      105      TO     WRK-JIDO-SETCOMATFLG
                MOVE       3       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-HOUMONSIDO-SEI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    退院時服薬指導加算を算定時の前月薬剤管理指導料のコメント編集
      *****************************************************************
       20011-TAIIN-COMMENT-SEC          SECTION.
      *
           MOVE       RECE64-REC       TO  RECE649-REC
           EVALUATE   FLG-HANTEI
             WHEN       1
                ADD     1              TO  RECE649-MEISAISU
             WHEN       2
                ADD     2              TO  RECE649-MEISAISU
           END-EVALUATE
           MOVE   "1"                  TO  RECE649-SETCOMFLG
           WRITE      RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           INITIALIZE                      RECE65-REC
           MOVE    RECE64-KEY1         TO  RECE65-KEY1
           MOVE    RECE64-ZAINUM       TO  RECE65-ZAINUM
           MOVE    RECE64-ZAIREN       TO  RECE65-ZAIREN
           START   RECE65-FILE
                 KEY IS    >=   RECE65-KEY
                 INVALID
                     MOVE    1         TO  FLG-RECE65
                 NOT INVALID
                     PERFORM 990-RECE65-NEXT-SEC
           END-START
      *
           MOVE    RECE65-REC           TO  RECE659-REC
           EVALUATE   FLG-HANTEI
             WHEN       1
               MOVE    RECE65-SRYACT-TBL(1) TO  RECE659-SRYACT-TBL(1)
               MOVE    RECE65-SRYACT-TBL(2) TO  RECE659-SRYACT-TBL(3)
               MOVE    RECE65-SRYACT-TBL(3) TO  RECE659-SRYACT-TBL(4)
               MOVE    RECE65-SRYACT-TBL(4) TO  RECE659-SRYACT-TBL(5)
      *
               MOVE    SPACE                TO  RECE659-SRYACT-TBL(2)
               INITIALIZE                       RECE659-SRYACT-TBL(2)
               MOVE    "810000001"          TO  RECE659-SRYCD(2)
               MOVE    ZERO                 TO  RECE659-SRYSURYO(2)
               PERFORM  200111-SANTEI-DAY-SEC
               PERFORM  200111-TAIIN-COMMENT-HENSYU-SEC
               MOVE    "2"                  TO  RECE659-MEISKYFLG(2)
               MOVE     104                 TO  RECE659-SETCOMATFLG(2)
             WHEN       2
               MOVE    RECE65-SRYACT-TBL(1) TO  RECE659-SRYACT-TBL(1)
               MOVE    RECE65-SRYACT-TBL(2) TO  RECE659-SRYACT-TBL(4)
               MOVE    RECE65-SRYACT-TBL(3) TO  RECE659-SRYACT-TBL(5)
      *
               MOVE    SPACE                TO  RECE659-SRYACT-TBL(2)
               INITIALIZE                       RECE659-SRYACT-TBL(2)
               MOVE    "810000001"          TO  RECE659-SRYCD(2)
               MOVE    ZERO                 TO  RECE659-SRYSURYO(2)
               PERFORM  200111-SANTEI-DAY-SEC
               PERFORM  200111-TAIIN-COMMENT-HENSYU-SEC
               MOVE    "2"                  TO  RECE659-MEISKYFLG(2)
               MOVE     104                 TO  RECE659-SETCOMATFLG(2)
      *
               MOVE    SPACE                TO  RECE659-SRYACT-TBL(3)
               INITIALIZE                       RECE659-SRYACT-TBL(3)
               MOVE    "810000001"          TO  RECE659-SRYCD(3)
               MOVE    ZERO                 TO  RECE659-SRYSURYO(3)
               MOVE    16                   TO  RECE659-YUKOKETA(3)
               MOVE    "薬剤管理指導料算定　　　月　　日"
                                            TO  RECE659-NAME(3)
               MOVE     WRK-TUKI-YAKUKAN    TO  DAY-IDX
               MOVE     TBL-DAY(DAY-IDX)    TO  RECE659-NAME(3)(21:4)
               MOVE     WRK-DAY-YAKUKAN     TO  DAY-IDX
               MOVE     TBL-DAY(DAY-IDX)    TO  RECE659-NAME(3)(27:4)
               MOVE    "2"                  TO  RECE659-MEISKYFLG(3)
               MOVE     104                 TO  RECE659-SETCOMATFLG(3)
           END-EVALUATE
           WRITE    RECE659-REC
           ADD      1                   TO  CNT-RECE659
      *
           .
       20011-TAIIN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定日編集
      *****************************************************************
       200111-TAIIN-COMMENT-HENSYU-SEC          SECTION.
      *
           MOVE     TB-SANTEI-DAY(1)    TO  DAY-IDX
           IF       DAY-IDX  <  10
             MOVE    5                  TO  RECE659-YUKOKETA(2)
             MOVE    "算定　　日"       TO  RECE659-NAME(2)(1:10)
             MOVE   TBL-DAY(DAY-IDX)    TO  RECE659-NAME(2)(5:4)
             MOVE   11                  TO  IDX17
           ELSE
             MOVE    6                  TO  RECE659-YUKOKETA(2)
             MOVE    "算定　　　日"     TO  RECE659-NAME(2)(1:12)
             MOVE   TBL-DAY(DAY-IDX)    TO  RECE659-NAME(2)(7:4)
             MOVE   13                  TO  IDX17
           END-IF
           IF       TB-SANTEI-DAY(2)  NOT = ZERO
             MOVE   TB-SANTEI-DAY(2)    TO  DAY-IDX
             IF       DAY-IDX  <  10
               ADD   3              TO  RECE659-YUKOKETA(2)
               MOVE  "、　日"       TO  RECE659-NAME(2)(IDX17:6)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)(3:2)
                                    TO  RECE659-NAME(2)(IDX18:2)
               COMPUTE  IDX17 = IDX17 + 6
             ELSE
               ADD   4              TO  RECE659-YUKOKETA(2)
               MOVE  "、　　日"     TO  RECE659-NAME(2)(IDX17:8)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)
                                    TO  RECE659-NAME(2)(IDX18:4)
               COMPUTE  IDX17 = IDX17 + 8
             END-IF
           END-IF
           IF       TB-SANTEI-DAY(3)  NOT = ZERO
             MOVE   TB-SANTEI-DAY(3)    TO  DAY-IDX
             IF       DAY-IDX  <  10
               ADD   3              TO  RECE659-YUKOKETA(2)
               MOVE  "、　日"       TO  RECE659-NAME(2)(IDX17:6)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)(3:2)
                                    TO  RECE659-NAME(2)(IDX18:2)
               COMPUTE  IDX17 = IDX17 + 6
             ELSE
               ADD   4              TO  RECE659-YUKOKETA(2)
               MOVE  "、　　日"     TO  RECE659-NAME(2)(IDX17:8)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)
                                    TO  RECE659-NAME(2)(IDX18:4)
               COMPUTE  IDX17 = IDX17 + 8
             END-IF
           END-IF
           IF       TB-SANTEI-DAY(4)  NOT = ZERO
             MOVE   TB-SANTEI-DAY(4)    TO  DAY-IDX
             IF       DAY-IDX  <  10
               ADD   3              TO  RECE659-YUKOKETA(2)
               MOVE  "、　日"       TO  RECE659-NAME(2)(IDX17:6)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)(3:2)
                                    TO  RECE659-NAME(2)(IDX18:2)
               COMPUTE  IDX17 = IDX17 + 6
             ELSE
               ADD   4              TO  RECE659-YUKOKETA(2)
               MOVE  "、　　日"     TO  RECE659-NAME(2)(IDX17:8)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)
                                    TO  RECE659-NAME(2)(IDX18:4)
               COMPUTE  IDX17 = IDX17 + 8
             END-IF
           END-IF
           IF       TB-SANTEI-DAY(5)  NOT = ZERO
             MOVE   TB-SANTEI-DAY(5)    TO  DAY-IDX
             IF       DAY-IDX  <  10
               ADD   3              TO  RECE659-YUKOKETA(2)
               MOVE  "、　日"       TO  RECE659-NAME(2)(IDX17:6)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)(3:2)
                                    TO  RECE659-NAME(2)(IDX18:2)
               COMPUTE  IDX17 = IDX17 + 6
             ELSE
               ADD   4              TO  RECE659-YUKOKETA(2)
               MOVE  "、　　日"     TO  RECE659-NAME(2)(IDX17:8)
               COMPUTE  IDX18 = IDX17 + 2
               MOVE TBL-DAY(DAY-IDX)
                                    TO  RECE659-NAME(2)(IDX18:4)
               COMPUTE  IDX17 = IDX17 + 8
             END-IF
           END-IF
      *
           .
       200111-TAIIN-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術日等のコメント編集（差込：上）
      *****************************************************************
       20011-SHUJUTSU-ETC-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-SANTEI-DAY-SEC
      *
           MOVE       ZERO       TO   IDZ                               
           INITIALIZE            WRK-FREE-AREA2                         
      *
           PERFORM  20011-FREE-COMMENT2-SEC
      *
           PERFORM  200111-RECE65-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                ADD    1     TO  IDX-RECE65
                MOVE   "810000001"
                             TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                MOVE   ZERO  TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                IF     WRK-JIDO-JYOUGEFLG  =  1
                  MOVE  "1"  TO  WRK-RECE65XX-RECEDENFLG(IDX-RECE65 1)
                END-IF
           END-PERFORM 
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-SHUJUTSU-ETC-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術日等のコメント編集２（差込：下）
      *****************************************************************
       20011-SHUJUTSU-ETC2-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-SANTEI-DAY-SEC
      *
           MOVE       ZERO       TO   IDZ                               
           INITIALIZE            WRK-FREE-AREA2                         
      *
           PERFORM  20011-FREE-COMMENT2-SEC
      *
           PERFORM  200111-RECE65-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           MOVE    RECE64-MEISAISU      TO  MAX-MEISAISU
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                              TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO   TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
                MOVE   WK-FREE-MEISKYFLG2(IDX)
                          TO  WRK-RECE65XX-MEISKYFLG(MAX-MEISAISU 1)
                MOVE   WK-FREE-SETCOMATFLG2(IDX)
                          TO  WRK-RECE65XX-SETCOMATFLG(MAX-MEISAISU 1)
           END-PERFORM 
      *
           IF    FLG-KAISIEN  =  1
              COMPUTE  MAX-MEISAISU  =   MAX-MEISAISU  +  1
              MOVE   "810000001"
                            TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
              MOVE   ZERO   TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
              MOVE   12     TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
              MOVE   "初回算定日　　　月　　日"
                            TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
              MOVE   WRK-KAISIEN-FSANTEIYMD(5:2)   TO  DAY-IDX2
              MOVE   TBL-DAY(DAY-IDX2)
                       TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(13:4)
              MOVE   WRK-KAISIEN-FSANTEIYMD(7:2)   TO  DAY-IDX2
              MOVE   TBL-DAY(DAY-IDX2)
                       TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(19:4)
              MOVE   "2"    TO  WRK-RECE65XX-MEISKYFLG(MAX-MEISAISU 1)
              MOVE   106    TO  WRK-RECE65XX-SETCOMATFLG(MAX-MEISAISU 1)
      *
              MOVE   ZERO   TO  FLG-KAISIEN
           END-IF
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-SHUJUTSU-ETC2-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括診療分のコメント編集（差込：上）
      *****************************************************************
       20011-HOKATU-COMMENT-SEC          SECTION.
      *
           PERFORM  200111-RECE65-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           ADD    1             TO  IDX-RECE65
           MOVE  "810000001"    TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
           MOVE   ZERO          TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
      *
           IF    RECE64-ZAIKBN  =  3 OR 4
             EVALUATE  RECE64-JISANYAKKBN
              WHEN     "1"
               MOVE  19    TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE "【持参薬】（入院契機治療・自院処方分）"
                           TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
              WHEN     "2"
               MOVE  19    TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE "【持参薬】（入院契機治療・他院処方分）"
                           TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
              WHEN     "3"
               MOVE  20    TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE "【持参薬】（入院契機治療外・自院処方分）"
                           TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
              WHEN     "4"
               MOVE  20    TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE "【持参薬】（入院契機治療外・他院処方分）"
                           TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
              WHEN     "5"
               MOVE  24    TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE "【持参薬】（入院契機治療・自院処方分・数量不明）"
                           TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
              WHEN     "6"
               MOVE  24    TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE "【持参薬】（入院契機治療・他院処方分・数量不明）"
                           TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
              WHEN     "7"
               MOVE  25    TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE "【持参薬】（入院契機治療外・自院処方分・数量不明）"
                           TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
              WHEN     "8"
               MOVE  25    TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE "【持参薬】（入院契機治療外・他院処方分・数量不明）"
                           TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
             END-EVALUATE
           ELSE
               MOVE   6    TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
               MOVE "【包括診療】"
                           TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
           END-IF
      *
           PERFORM  200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-HOKATU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    超重症児（者）入院診療加算・準超重症児（者）入院診療加算
      *    の算定開始日検索
      *****************************************************************
       20011-CYOJKSN-KAISHIDAY-SEC          SECTION.
      *
           MOVE   SPACE   TO  WRK-CYOJKSN-YMD-XXX
                              WRK-CYOJKSN-COM-XXX
           MOVE   SPACE   TO  WRK-CYOJKSN-YMD-YYY
                              WRK-CYOJKSN-COM-YYY
      *
           IF     RECE62-CYOJKSNFLG   =   "X" OR "Z"
      *
             MOVE     "key21"     TO   WRK-SRYACCT-PATHNAME
             MOVE      1          TO   FLG-CYOJKSN
             PERFORM  200111-CYOJKSN-KAISHIDAY4-SEC
      *
             IF    WRK-CYOJKSN-KAISHIDAY-XXX(1)  NOT = SPACE
                 MOVE  WRK-CYOJKSN-KAISHIDAY-XXX(1)
                                  TO   WRK-CYOJKSN-YMD-XXX
             ELSE
               IF  WRK-CYOJKSN-KAISHIDAY-XXX(2)  NOT = SPACE
                 MOVE  WRK-CYOJKSN-KAISHIDAY-XXX(2)
                                  TO   WRK-CYOJKSN-YMD-XXX
               END-IF
             END-IF
      *
      ****   IF     FLG-CHECK-MAE  =  1
      ****       MOVE   SPACE   TO  WRK-CYOJKSN-YMD-XXX
      ****   END-IF
      *
      *      算定開始日コメントがあるか検索
      *
             MOVE     "key22"     TO   WRK-SRYACCT-PATHNAME
             MOVE      2          TO   FLG-CYOJKSN
             PERFORM  200111-CYOJKSN-KAISHIDAY4-SEC
      *
             IF    WRK-CYOJKSN-KAISHIDAY-XXX(1)  NOT = SPACE
             AND   WRK-CYOJKSN-KAISHIDAY-XXX(2)  NOT = SPACE
               IF    WRK-CYOJKSN-KAISHIDAY-XXX(1)(1:6) =
                           WRK-CYOJKSN-KAISHIDAY-XXX(2)(1:6)
                   MOVE  WRK-CYOJKSN-INPUTCOMENT-XXX(1)
                                  TO   WRK-CYOJKSN-COM-XXX
               ELSE
                   MOVE  WRK-CYOJKSN-INPUTCOMENT-XXX(2)
                                  TO   WRK-CYOJKSN-COM-XXX
               END-IF
             ELSE
               IF    WRK-CYOJKSN-KAISHIDAY-XXX(1)  NOT = SPACE
                   MOVE  WRK-CYOJKSN-INPUTCOMENT-XXX(1)
                                  TO   WRK-CYOJKSN-COM-XXX
               ELSE
                 IF  WRK-CYOJKSN-KAISHIDAY-XXX(2)  NOT = SPACE
                   MOVE  WRK-CYOJKSN-INPUTCOMENT-XXX(2)
                                  TO   WRK-CYOJKSN-COM-XXX
                 END-IF
               END-IF
             END-IF
      *
           END-IF
      *
           IF     RECE62-CYOJKSNFLG   =   "Y" OR "Z"
      *
             MOVE     "key21"     TO   WRK-SRYACCT-PATHNAME
             MOVE      1          TO   FLG-CYOJKSN
             PERFORM  200111-CYOJKSN-KAISHIDAY5-SEC
      *
             IF    WRK-CYOJKSN-KAISHIDAY-YYY(1)  NOT = SPACE
                 MOVE  WRK-CYOJKSN-KAISHIDAY-YYY(1)
                                  TO   WRK-CYOJKSN-YMD-YYY
             ELSE
               IF  WRK-CYOJKSN-KAISHIDAY-YYY(2)  NOT = SPACE
                 MOVE  WRK-CYOJKSN-KAISHIDAY-YYY(2)
                                  TO   WRK-CYOJKSN-YMD-YYY
               END-IF
             END-IF
      *
      ****   IF     FLG-CHECK-MAE  =  1
      ****       MOVE   SPACE   TO  WRK-CYOJKSN-YMD-YYY
      ****   END-IF
      *
      *      算定開始日コメントがあるか検索
      *
             MOVE     "key22"     TO   WRK-SRYACCT-PATHNAME
             MOVE      2          TO   FLG-CYOJKSN
             PERFORM  200111-CYOJKSN-KAISHIDAY5-SEC
      *
             IF    WRK-CYOJKSN-KAISHIDAY-YYY(1)  NOT = SPACE
             AND   WRK-CYOJKSN-KAISHIDAY-YYY(2)  NOT = SPACE
               IF    WRK-CYOJKSN-KAISHIDAY-YYY(1)(1:6) =
                           WRK-CYOJKSN-KAISHIDAY-YYY(2)(1:6)
                   MOVE  WRK-CYOJKSN-INPUTCOMENT-YYY(1)
                                  TO   WRK-CYOJKSN-COM-YYY
               ELSE
                   MOVE  WRK-CYOJKSN-INPUTCOMENT-YYY(2)
                                  TO   WRK-CYOJKSN-COM-YYY
               END-IF
             ELSE
               IF    WRK-CYOJKSN-KAISHIDAY-YYY(1)  NOT = SPACE
                   MOVE  WRK-CYOJKSN-INPUTCOMENT-YYY(1)
                                  TO   WRK-CYOJKSN-COM-YYY
               ELSE
                 IF  WRK-CYOJKSN-KAISHIDAY-YYY(2)  NOT = SPACE
                   MOVE  WRK-CYOJKSN-INPUTCOMENT-YYY(2)
                                  TO   WRK-CYOJKSN-COM-YYY
                 END-IF
               END-IF
             END-IF
      *
           END-IF
      *
           .
       20011-CYOJKSN-KAISHIDAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    超重症児（者）入院診療加算・準超重症児（者）入院診療加算
      *    の算定開始日検索２
      *****************************************************************
       200111-CYOJKSN-KAISHIDAY2-SEC          SECTION.
      *
           INITIALIZE                   SRYACCT-REC
           MOVE    SRY-HOSPNUM      TO  ACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN    TO  ACCT-NYUGAIKBN
           MOVE    SRY-PTID         TO  ACCT-PTID
           MOVE    SRY-SRYKA        TO  ACCT-SRYKA
           MOVE    SRY-SRYYM        TO  ACCT-SRYYM
           MOVE    SRY-ZAINUM       TO  ACCT-ZAINUM
           MOVE    SRYACCT-REC      TO  MCPDATA-REC
           MOVE    "tbl_sryacct"    TO  MCP-TABLE
           MOVE    "key31"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC           =   ZERO
              MOVE "tbl_sryacct"    TO  MCP-TABLE
              MOVE "key31"          TO  MCP-PATHNAME
              PERFORM 900-DBFETCH-SEC
              IF      MCP-RC        =   ZERO
                MOVE  MCPDATA-REC   TO  SRYACCT-REC
                IF (WRK-CYOJKSN-TAIHIYM = SPACE     )
                OR (WRK-CYOJKSN-TAIHIYM = ACCT-SRYYM)
                 IF  ( ACCT-SRYYM   >  RECE62-SRYYM )
                 OR  ((FLG-CYOJKSN  =   2      ) AND
                      (ACCT-SRYYM   <  "201404")    )
                  CONTINUE
                 ELSE
                  MOVE    RECE62-NYUINYMD(7:2) TO  IDX28
                  PERFORM  VARYING  IDX26  FROM  1  BY  1
                            UNTIL   IDX26   >   31
                    IF   ACCT-DAY(1 IDX26)  NOT =  ZERO
                      IF  (ACCT-SRYYM = RECE62-NYUINYMD(1:6))
                      AND (IDX26      <  IDX28              )
                          MOVE   1           TO  FLG-CHECK-MAE
                      ELSE
                        EVALUATE  FLG-CYOJKSN
                          WHEN       1
                            IF   WRK-CYOJKSN-TAIHIYM = SPACE
                              MOVE  ACCT-SRYYM  TO  WRK-CYOJKSN-TAIHIYM
                              MOVE  IDX26       TO  WRK-CYOJKSN-TAIHIDD
                              MOVE  31          TO  IDX26
                            ELSE
                              IF   IDX26  <  WRK-CYOJKSN-TAIHIDD
                                MOVE   IDX26    TO  WRK-CYOJKSN-TAIHIDD
                                MOVE   31       TO  IDX26
                              END-IF
                            END-IF
                          WHEN       2
                            IF   WRK-CYOJKSN-TAIHIYM = SPACE
                               MOVE ZERO   TO FLG-CYOJKSNCOMARI
                               MOVE SPACE  TO WRK-CYOJKSN-INPUTCOMENT
                               PERFORM 200111-CYOJKSN-KAISHIDAY3-SEC
                               IF   FLG-CYOJKSNCOMARI  =  1
                                 MOVE  WRK-CYOJKSN-INPUTCOMENT
                                           TO WRK-CYOJKSN-INPUTCOMENT2
      *
                                 MOVE ACCT-SRYYM TO WRK-CYOJKSN-TAIHIYM
                                 MOVE IDX26      TO WRK-CYOJKSN-TAIHIDD
                               END-IF
                               MOVE   31         TO IDX26
                            ELSE
                             IF   IDX26  <  WRK-CYOJKSN-TAIHIDD
                               MOVE ZERO   TO FLG-CYOJKSNCOMARI
                               MOVE SPACE  TO WRK-CYOJKSN-INPUTCOMENT
                               PERFORM 200111-CYOJKSN-KAISHIDAY3-SEC
                               IF   FLG-CYOJKSNCOMARI  =  1
                                 MOVE  WRK-CYOJKSN-INPUTCOMENT
                                           TO WRK-CYOJKSN-INPUTCOMENT2
      *
                                 MOVE IDX26      TO WRK-CYOJKSN-TAIHIDD
                               END-IF
                               MOVE   31         TO IDX26
                             END-IF
                            END-IF
                        END-EVALUATE
                      END-IF
                    END-IF
                  END-PERFORM
                 END-IF
                ELSE
                  MOVE    1         TO  FLG-CYOJKSN-OK
                END-IF
              END-IF
           END-IF
           MOVE    "tbl_sryacct"    TO  MCP-TABLE
           MOVE    "key31"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       200111-CYOJKSN-KAISHIDAY2-EXT.
           EXIT.
      *
      *****************************************************************
      *    超重症児（者）入院診療加算・準超重症児（者）入院診療加算
      *    の算定開始日検索３
      *****************************************************************
       200111-CYOJKSN-KAISHIDAY3-SEC          SECTION.
      *
           INITIALIZE                         SRYACT-REC
           MOVE    ACCT-HOSPNUM           TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN         TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID              TO  SRY-PTID
           MOVE    ACCT-SRYKA             TO  SRY-SRYKA
           MOVE    ACCT-SRYYM             TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM            TO  SRY-ZAINUM
           MOVE    SRYACT-REC             TO  MCPDATA-REC
           MOVE    "tbl_sryact"           TO  MCP-TABLE
           MOVE    "key2"                 TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC                 =   ZERO
              MOVE    "tbl_sryact"        TO  MCP-TABLE
              MOVE    "key2"              TO  MCP-PATHNAME
              PERFORM  900-SRYACT-READ-SEC
              PERFORM  UNTIL  (FLG-SRYACT        =  1)
                        OR    (FLG-CYOJKSNCOMARI =  1)
                PERFORM  VARYING  IDX12  FROM  1  BY  1
                          UNTIL  (IDX12             >    5   )
                           OR    (SRY-SRYCD(IDX12)  =   SPACE)
                           OR    (FLG-CYOJKSNCOMARI =    1   )
                   IF  SRY-SRYCD(IDX12)  =  "840000056" OR "840000556"
                      MOVE    1    TO   FLG-CYOJKSNCOMARI
                      IF   SRY-INPUTNUM(IDX12)  NOT =  ZERO
                        INITIALIZE                    PTCOM-REC
                        MOVE  SRY-HOSPNUM         TO  PTCOM-HOSPNUM
                        MOVE  SRY-PTID            TO  PTCOM-PTID
                        MOVE  SRY-ZAINUM          TO  PTCOM-ZAINUM
                        MOVE  SRY-SRYCD(IDX12)    TO  PTCOM-SRYCD
                        MOVE  SRY-INPUTNUM(IDX12) TO  PTCOM-RENNUM
                        PERFORM 900-PTCOM-READ-SEC
                        MOVE  PTCOM-INPUTCOMENT
                                       TO  WRK-CYOJKSN-INPUTCOMENT
                      END-IF
                   END-IF
                END-PERFORM
                MOVE    "tbl_sryact"      TO  MCP-TABLE
                MOVE    "key2"            TO  MCP-PATHNAME
                PERFORM   900-SRYACT-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_sryact"           TO  MCP-TABLE
           MOVE    "key2"                 TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200111-CYOJKSN-KAISHIDAY3-EXT.
           EXIT.
      *
      *****************************************************************
      *    超重症児（者）入院診療加算・準超重症児（者）入院診療加算
      *    の算定開始日検索４
      *****************************************************************
       200111-CYOJKSN-KAISHIDAY4-SEC          SECTION.
      *
           INITIALIZE         WRK-CYOJKSN-KAISHIDAY-XXX-AREA
      *
           MOVE      ZERO                 TO  FLG-CHECK-MAE
      *
           PERFORM  VARYING  IDX30  FROM  1  BY  1
                     UNTIL   IDX30   >   TBL-CYOJKSN-CHECK-XXX-MAX
      *
             MOVE    ZERO                 TO  FLG-CYOJKSN-OK
             MOVE    SPACE                TO  WRK-CYOJKSN-TAIHIYM
             MOVE    SPACE                TO  WRK-CYOJKSN-TAIHIDD
             MOVE    SPACE                TO  WRK-CYOJKSN-INPUTCOMENT2
      *
             INITIALIZE                       SRYACT-REC
             MOVE    RECE62-HOSPNUM       TO  SRY-HOSPNUM
             MOVE    RECE62-NYUGAIKBN     TO  SRY-NYUGAIKBN
             MOVE    RECE62-PTID          TO  SRY-PTID
             MOVE    RECE62-NYUINYMD(1:6) TO  SRY-SRYYM
             MOVE    TBL-CYOJKSN-CHECK-XXX(IDX30)
                                          TO  SRY-SRYCD (1)
                                              SRY-SRYCD (2)
                                              SRY-SRYCD (3)
                                              SRY-SRYCD (4)
                                              SRY-SRYCD (5)
             MOVE    SRYACT-REC           TO  MCPDATA-REC
             MOVE    "tbl_sryact"         TO  MCP-TABLE
             MOVE    WRK-SRYACCT-PATHNAME TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC               =   ZERO
               MOVE    "tbl_sryact"       TO  MCP-TABLE
               MOVE    WRK-SRYACCT-PATHNAME
                                          TO  MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL   FLG-SRYACT      =   1
                         OR     FLG-CYOJKSN-OK  =   1
      *
                 PERFORM  200111-CYOJKSN-KAISHIDAY2-SEC
      *
                 MOVE    "tbl_sryact"     TO  MCP-TABLE
                 MOVE    WRK-SRYACCT-PATHNAME
                                          TO  MCP-PATHNAME
                 PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
             END-IF
             MOVE    "tbl_sryact"         TO  MCP-TABLE
             MOVE    WRK-SRYACCT-PATHNAME TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
             IF   WRK-CYOJKSN-TAIHIYM  NOT = SPACE
             AND  WRK-CYOJKSN-TAIHIDD  NOT = SPACE
               MOVE  WRK-CYOJKSN-TAIHIYM
                           TO WRK-CYOJKSN-KAISHIDAY-XXX(IDX30)(1:6)
               MOVE  WRK-CYOJKSN-TAIHIDD
                           TO WRK-CYOJKSN-KAISHIDAY-XXX(IDX30)(7:2)
               MOVE  WRK-CYOJKSN-INPUTCOMENT2
                           TO WRK-CYOJKSN-INPUTCOMENT-XXX(IDX30)
             END-IF
      *
           END-PERFORM
      *
           .
       200111-CYOJKSN-KAISHIDAY4-EXT.
           EXIT.
      *
      *****************************************************************
      *    超重症児（者）入院診療加算・準超重症児（者）入院診療加算
      *    の算定開始日検索５
      *****************************************************************
       200111-CYOJKSN-KAISHIDAY5-SEC          SECTION.
      *
           INITIALIZE         WRK-CYOJKSN-KAISHIDAY-YYY-AREA
      *
           MOVE      ZERO                 TO  FLG-CHECK-MAE
      *
           PERFORM  VARYING  IDX30  FROM  1  BY  1
                     UNTIL   IDX30   >   TBL-CYOJKSN-CHECK-YYY-MAX
      *
             MOVE    ZERO                 TO  FLG-CYOJKSN-OK
             MOVE    SPACE                TO  WRK-CYOJKSN-TAIHIYM
             MOVE    SPACE                TO  WRK-CYOJKSN-TAIHIDD
             MOVE    SPACE                TO  WRK-CYOJKSN-INPUTCOMENT2
      *
             INITIALIZE                       SRYACT-REC
             MOVE    RECE62-HOSPNUM       TO  SRY-HOSPNUM
             MOVE    RECE62-NYUGAIKBN     TO  SRY-NYUGAIKBN
             MOVE    RECE62-PTID          TO  SRY-PTID
             MOVE    RECE62-NYUINYMD(1:6) TO  SRY-SRYYM
             MOVE    TBL-CYOJKSN-CHECK-YYY(IDX30)
                                          TO  SRY-SRYCD (1)
                                              SRY-SRYCD (2)
                                              SRY-SRYCD (3)
                                              SRY-SRYCD (4)
                                              SRY-SRYCD (5)
             MOVE    SRYACT-REC           TO  MCPDATA-REC
             MOVE    "tbl_sryact"         TO  MCP-TABLE
             MOVE    WRK-SRYACCT-PATHNAME TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC               =   ZERO
               MOVE    "tbl_sryact"       TO  MCP-TABLE
               MOVE    WRK-SRYACCT-PATHNAME
                                          TO  MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL   FLG-SRYACT      =   1
                         OR     FLG-CYOJKSN-OK  =   1
      *
                 PERFORM  200111-CYOJKSN-KAISHIDAY2-SEC
      *
                 MOVE    "tbl_sryact"     TO  MCP-TABLE
                 MOVE    WRK-SRYACCT-PATHNAME
                                          TO  MCP-PATHNAME
                 PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
             END-IF
             MOVE    "tbl_sryact"         TO  MCP-TABLE
             MOVE    WRK-SRYACCT-PATHNAME TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
      *
             IF   WRK-CYOJKSN-TAIHIYM  NOT = SPACE
             AND  WRK-CYOJKSN-TAIHIDD  NOT = SPACE
               MOVE  WRK-CYOJKSN-TAIHIYM
                           TO WRK-CYOJKSN-KAISHIDAY-YYY(IDX30)(1:6)
               MOVE  WRK-CYOJKSN-TAIHIDD
                           TO WRK-CYOJKSN-KAISHIDAY-YYY(IDX30)(7:2)
               MOVE  WRK-CYOJKSN-INPUTCOMENT2
                           TO WRK-CYOJKSN-INPUTCOMENT-YYY(IDX30)
             END-IF
      *
           END-PERFORM
      *
           .
       200111-CYOJKSN-KAISHIDAY5-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定日の確定
      *****************************************************************
       200111-SANTEI-DAY-SEC          SECTION.
      *
           INITIALIZE                TBL-SANTEI-DAY
      *
           MOVE    ZERO           TO  DAY-IDX
           PERFORM    VARYING    IDX  FROM   1   BY   1
                      UNTIL     (IDX      >   31)  OR
                                (DAY-IDX  >   31)
                IF    RECE64-KAISU(1 IDX)  NOT =  ZERO
                      ADD   1     TO  DAY-IDX
                      MOVE  IDX   TO  TB-SANTEI-DAY(DAY-IDX)
                END-IF
           END-PERFORM
      *
           .
       200111-SANTEI-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術日等の日付の編集
      *****************************************************************
       20011-FREE-COMMENT2-SEC          SECTION.                         
      *
           ADD      1          TO    IDZ
      *
           EVALUATE   FLG-COMMENT
              WHEN    1
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "手術"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
              WHEN    2
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "実施"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
              WHEN    3
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "算定"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
              WHEN    4
                 IF   RECE62-SRYYM >= "201905"
                   MOVE     22         TO    WRK-KETA
                                             WRK-KETA-TAIHI
                   MOVE     11         TO    WRK-KETA2
                   MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                   MOVE    "交付　　　　　年　　月"
                                 TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
                   MOVE     11         TO     IDX41
                   MOVE     17         TO     IDX42
                   MOVE      7         TO     IDX49
                 ELSE
                   MOVE     18         TO    WRK-KETA
                                             WRK-KETA-TAIHI
                   MOVE     9          TO    WRK-KETA2
                   MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                   MOVE    "交付　　　年　　月"
                                 TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
                   MOVE      7         TO     IDX41
                   MOVE     13         TO     IDX42
                 END-IF
      *
                 INITIALIZE                      STS-AREA-DAY
                 INITIALIZE                      LNK-DAY2-AREA
                 MOVE    "21"                TO  LNK-DAY2-IRAI
                 MOVE    RECE62-SRYYM        TO  WRK-HENYMD(1:6)
                 MOVE    "01"                TO  WRK-HENYMD(7:2)
                 MOVE    WRK-HENYMD          TO  LNK-DAY2-YMD
                 CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                             LNK-DAY2-AREA
                 IF  STS-DAY-RC1             =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                   END-IF
      *
                   IF   RECE62-SRYYM >= "201905"
                      MOVE    LNK-DAY2-EDTYMD2-G
                                   TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                   END-IF
                 END-IF
              WHEN    6
                 MOVE     0          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     0          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
              WHEN    7
                 MOVE     8          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     4          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "検査実施"
                             TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
              WHEN    8
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "麻酔"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
              WHEN    10
                 MOVE     6          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     3          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "指導日"
                                 TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
           END-EVALUATE
      *
           IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
             MOVE  "2"                   TO  WK-FREE-MEISKYFLG2(IDZ)
             MOVE  WRK-JIDO-SETCOMATFLG  TO  WK-FREE-SETCOMATFLG2(IDZ)
           END-IF
      *
           MOVE      ZERO          TO  FLG-FIRST
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                          UNTIL   (IDX                 >    31)
                           OR     (TB-SANTEI-DAY(IDX)  =  ZERO)
      *
                 IF  FLG-FIRST  =  ZERO
                    IF  TB-SANTEI-DAY(IDX)     <  10 
      *
                       MOVE   TB-SANTEI-DAY(IDX)  TO  IDY
      *
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                       MOVE    "　"
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    TBL-DAY(IDY)(3:2)     
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    "日"
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                    ELSE
      *
                       MOVE   TB-SANTEI-DAY(IDX)  TO  IDY
      *
                       IF   FLG-COMMENT   =   4 OR 6
                         ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                         MOVE    TBL-DAY(IDY)     
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
                       ELSE
                         ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                         MOVE    "　"
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                         ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                         MOVE    TBL-DAY(IDY)     
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
                       END-IF
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                       MOVE    "日"
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                    END-IF
                    MOVE      1              TO  FLG-FIRST
                 ELSE
                    IF  TB-SANTEI-DAY(IDX)     <  10 
      *
                       MOVE   TB-SANTEI-DAY(IDX)  TO  IDY
      *
                       IF    WK-FREE-YUKOKETA2(IDZ)  =  25
                          PERFORM  200111-FREE-COMMENT2-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    "、"
                                    TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "、"
                                    TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                       END-IF
      *
                       IF    WK-FREE-YUKOKETA2(IDZ)  =  24 OR 25
                          PERFORM  200111-FREE-COMMENT2-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    TBL-DAY(IDY)(3:2)     
                                    TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "日"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2) 
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)(3:2)     
                                    TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "日"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2) 
                       END-IF
                    ELSE
      *
                       MOVE   TB-SANTEI-DAY(IDX)  TO  IDY
      *
                       IF    WK-FREE-YUKOKETA2(IDZ)  =  25
                          PERFORM  200111-FREE-COMMENT2-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    "、"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "、"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       END-IF
      *
                       IF    WK-FREE-YUKOKETA2(IDZ)  =  23 OR 24 OR 25
                          PERFORM  200111-FREE-COMMENT2-KAIGYO-SEC
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    TBL-DAY(IDY)     
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "日"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)     
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "日"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       END-IF
                    END-IF
                 END-IF
      *
           END-PERFORM
           .
       20011-FREE-COMMENT2-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術日等の日付の編集（改行）
      *****************************************************************
       200111-FREE-COMMENT2-KAIGYO-SEC          SECTION.                  
      *
           ADD      1              TO  IDZ
      *
           MOVE    WRK-KETA-TAIHI  TO  WRK-KETA
           MOVE    WRK-KETA2       TO  WK-FREE-YUKOKETA2(IDZ)
           MOVE    ALL "　"        TO  WK-FREE-NAME2(IDZ)(1:WRK-KETA)
      *
           IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
             MOVE  "2"                   TO  WK-FREE-MEISKYFLG2(IDZ)
             MOVE  WRK-JIDO-SETCOMATFLG  TO  WK-FREE-SETCOMATFLG2(IDZ)
           END-IF
      *
           .
       200111-FREE-COMMENT2-KAIGYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費受診日の日付の編集
      *****************************************************************
       20011-FREE-COMMENT3-SEC          SECTION.                         
      *
           ADD      1          TO    IDZ
      *
           EVALUATE   FLG-COMMENT
              WHEN    5
                 MOVE     3          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "福祉"  TO    WK-FREE-NAME2(IDZ)(1:4)
           END-EVALUATE
      *
           MOVE      ZERO          TO  FLG-FIRST
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                          UNTIL   (IDX                  >    31 )
                           OR     (TBL-NYU-DAY-ST(IDX)  =   ZERO)
      *
                 IF  TBL-NYU-DAY-CD(IDX)   =  CONS-SANTEI-COMMENT1
      *
                       MOVE   TBL-NYU-DAY-ST(IDX) TO  IDY
      *
                       IF  FLG-FIRST  =  ZERO
                         ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                         MOVE    "　"
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                         MOVE  1  TO  FLG-FIRST 
                       ELSE
                         IF    WK-FREE-YUKOKETA2(IDZ)  =  25
                           PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                         ELSE
                           ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                           COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                           MOVE    "、"
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                         END-IF
                       END-IF
      *
                       IF  TBL-NYU-DAY-ST(IDX)     <  10 
                          IF    WK-FREE-YUKOKETA2(IDZ)  =
                                                       24 OR 25
                            PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                          END-IF
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)(3:2)     
                              TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "日"
                              TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       ELSE
                          IF    WK-FREE-YUKOKETA2(IDZ)  =
                                                 23 OR 24 OR 25
                            PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                          END-IF
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)     
                              TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "日"
                              TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       END-IF
      *
                 ELSE
      *
                       MOVE   TBL-NYU-DAY-ST(IDX) TO  IDY
                       MOVE   TBL-NYU-DAY-ED(IDX) TO  IDY2
      *
                       IF  FLG-FIRST  =  ZERO
                         ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                         MOVE    "　"
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                         MOVE  1  TO  FLG-FIRST 
                       ELSE
                         IF    WK-FREE-YUKOKETA2(IDZ)  =  25
                           PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                         ELSE
                           ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                           COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                           MOVE    "、"
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                         END-IF
                       END-IF
      *
                       IF  TBL-NYU-DAY-ST(IDX)     <  10 
                          IF  TBL-NYU-DAY-ED(IDX)  <  10
                            IF    WK-FREE-YUKOKETA2(IDZ)  =  
                                       21 OR 22 OR 23 OR 24 OR 25
                              PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                            END-IF
                            ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                            MOVE    TBL-DAY(IDY)(3:2)     
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                            ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                            MOVE    "日〜"
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                            ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                            MOVE    TBL-DAY(IDY2)(3:2)     
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                            ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                            MOVE    "日"
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                          ELSE
                            IF    WK-FREE-YUKOKETA2(IDZ)  =  
                                 20 OR 21 OR 22 OR 23 OR 24 OR 25
                              PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                            END-IF
                            ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                            MOVE    TBL-DAY(IDY)(3:2)     
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                            ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                            MOVE    "日〜"
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                            ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                            MOVE    TBL-DAY(IDY2)
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                            ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                            COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                            MOVE    "日"
                                TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                          END-IF
                       ELSE
                          IF    WK-FREE-YUKOKETA2(IDZ)  =  
                               19 OR 20 OR 21 OR 22 OR 23 OR 24 OR 25
                              PERFORM  200111-FREE-COMMENT3-KAIGYO-SEC
                          END-IF
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)     
                              TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "日〜"
                              TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    TBL-DAY(IDY2)
                              TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "日"
                              TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       END-IF
      *
                 END-IF
      *
           END-PERFORM
           .
       20011-FREE-COMMENT3-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費受診日の日付の編集（改行）
      *****************************************************************
       200111-FREE-COMMENT3-KAIGYO-SEC          SECTION.                  
      *
           ADD      1              TO  IDZ
      *
           MOVE    WRK-KETA-TAIHI  TO  WRK-KETA
           MOVE    WRK-KETA2       TO  WK-FREE-YUKOKETA2(IDZ)
           MOVE    ALL "　"        TO  WK-FREE-NAME2(IDZ)(1:4)
      *
           .
       200111-FREE-COMMENT3-KAIGYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリのコメント編集
      *****************************************************************
       2001-RIHA-COMMENT-SEC          SECTION.
      *
             INITIALIZE            WRK-JIDO-SETCOM-AREA
             MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
             MOVE       16      TO     WRK-JIDO-SETCOMATFLG
      *
             PERFORM   200111-RECE65-TAIHI-SEC
      *
      *      ＲＥＣＥ０５ファイルの内容をテーブルに退避
             MOVE    ZERO                 TO  IDX-RECE65
             MOVE    SPACE                TO  WRK-RECE65XX-REC
             INITIALIZE                       WRK-RECE65XX-REC
      *      リハビリコメントの挿入
             COMPUTE  MAX-MEISAISU        =   RECE64-MEISAISU  +  1
      *
             MOVE    "810000001"
                            TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO   TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    7      TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "実施日数　　日"
                            TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
             MOVE    ZERO                 TO  CNT-RIHA
             PERFORM  VARYING   IDX  FROM  1  BY  1
                       UNTIL    IDX   >   31
                 IF   RECE64-KAISU(1 IDX)  NOT = ZERO
                        ADD      1        TO  CNT-RIHA
                 END-IF
             END-PERFORM
             IF      CNT-RIHA             NOT =    ZERO
               MOVE    CNT-RIHA             TO  DAY-IDX
               MOVE    TBL-DAY(DAY-IDX)
                           TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(9:4)
             END-IF
      *
             IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
               MOVE  "2"   TO  WRK-RECE65XX-MEISKYFLG(MAX-MEISAISU 1)
               MOVE  WRK-JIDO-SETCOMATFLG
                           TO  WRK-RECE65XX-SETCOMATFLG(MAX-MEISAISU 1)
             END-IF
      *
             PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       2001-RIHA-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ医学管理料のコメント編集
      *****************************************************************
       2001-RIHAIGAKU-COMMENT-SEC          SECTION.
      *
           INITIALIZE            WRK-JIDO-SETCOM-AREA
      *
           EVALUATE      RECE64-RIHABIRI-COMMENT
              WHEN           "2"
                  MOVE   CONS-SINDAI-IGAKU1   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-SINDAI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "3"
                  MOVE   CONS-SINDAI-IGAKU2   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-SINDAI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "4"
                  MOVE   CONS-KOKYUKI-IGAKU1  TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-KOKYUKI-DUMMY   TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "5"
                  MOVE   CONS-KOKYUKI-IGAKU2  TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-KOKYUKI-DUMMY   TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "6"
                  MOVE   CONS-NOUKE-IGAKU1    TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-NOUKE-DUMMY     TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "7"
                  MOVE   CONS-NOUKE-IGAKU2    TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-NOUKE-DUMMY     TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "8"
                  MOVE   CONS-UNDOKI-IGAKU1   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-UNDOKI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "9"
                  MOVE   CONS-UNDOKI-IGAKU2   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-UNDOKI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
           END-EVALUATE
      ***
           MOVE          SPACE          TO  WRK-RIHAIGAKU-FSANTEIYMD
      *    医学管理：算定履歴検索
           INITIALIZE                       SANTEI-REC
           MOVE    RECE62-HOSPNUM       TO  SANTEI-HOSPNUM
           MOVE    RECE62-PTID          TO  SANTEI-PTID
           MOVE    RECE62-SRYYM         TO  SANTEI-SRYYM
           MOVE    WRK-RIHAIGAKU-SRYCD  TO  SANTEI-SRYCD
           MOVE    ZERO                 TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                 TO  SANTEI-SRYKA
           MOVE    ZERO                 TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC           TO  MCPDATA-REC
           MOVE    "tbl_santei"         TO  WRK-MCP-TABLE
           MOVE    "key"                TO  WRK-MCP-PATHNAME
           PERFORM  900-SANTEI-INV-SEC
           IF       FLG-SANTEI        =   ZERO
                MOVE  SANTEI-FSANTEIYMD TO  WRK-RIHAIGAKU-FSANTEIYMD
           END-IF
      ***
           INITIALIZE      WRK-RIHAIGAKU-SANTEI-AREA
      *    リハ実施ダミー：算定履歴検索
           INITIALIZE                       SANTEI-REC
           MOVE    RECE62-HOSPNUM       TO  SANTEI-HOSPNUM
           MOVE    RECE62-PTID          TO  SANTEI-PTID
           MOVE    RECE62-SRYYM         TO  SANTEI-SRYYM
           MOVE    WRK-RIHAIGAKU-SRYCD2 TO  SANTEI-SRYCD
           MOVE    ZERO                 TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                 TO  SANTEI-SRYKA
           MOVE    ZERO                 TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC           TO  MCPDATA-REC
           MOVE    "tbl_santei"         TO  WRK-MCP-TABLE
           MOVE    "key"                TO  WRK-MCP-PATHNAME
           PERFORM  900-SANTEI-INV-SEC
           IF       FLG-SANTEI        =   ZERO
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL   IDX    >  31
                 IF   SANTEI-DAY(IDX)   NOT =  ZERO
                    MOVE  "1"  TO  WRK-RIHAIGAKU-SANTEI-FLG
                    MOVE  SANTEI-DAY(IDX)
                               TO  WRK-RIHAIGAKU-SANTEI-DAY(IDX)
                 END-IF
               END-PERFORM
           END-IF
      *
      ***
      *
           PERFORM  200111-RECE65-TAIHI-SEC
      *
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           COMPUTE  MAX-MEISAISU        =   RECE64-MEISAISU  +  1
      *
           MOVE    "810000001"
                         TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
           MOVE    ZERO  TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
           MOVE    9     TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
           MOVE    "開始　　　月　　日"
                         TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           IF      WRK-RIHAIGAKU-FSANTEIYMD  NOT = SPACE
             MOVE    WRK-RIHAIGAKU-FSANTEIYMD(5:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                         TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(7:4)
             MOVE    WRK-RIHAIGAKU-FSANTEIYMD(7:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                         TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(13:4)
           END-IF
      *
           IF      WRK-RIHAIGAKU-SANTEI-FLG  NOT = SPACE
             PERFORM    20011-RIHAIGAKU-DUMMY-COM-SEC
           ELSE
             PERFORM    200111-SRYACTINF-HENSYU-SEC
           END-IF
      *
           .
       2001-RIHAIGAKU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ医学管理料のコメント編集（リハ実施日）
      *****************************************************************
       20011-RIHAIGAKU-DUMMY-COM-SEC          SECTION.
      *
           INITIALIZE             TBL-SANTEI-DAY
      *
           MOVE     ZERO      TO   DAY-IDX
           PERFORM  VARYING  IDX  FROM   1   BY   1
                     UNTIL   IDX    >   31
             IF  WRK-RIHAIGAKU-SANTEI-DAY(IDX)  NOT = ZERO
                 ADD    1     TO   DAY-IDX
                 MOVE   IDX   TO   TB-SANTEI-DAY(DAY-IDX)
             END-IF
           END-PERFORM
      *
           MOVE     ZERO      TO   IDZ
           INITIALIZE         WRK-FREE-AREA2
      *
           MOVE     2         TO   FLG-COMMENT
           PERFORM  20011-FREE-COMMENT2-SEC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                             TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           END-PERFORM
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-RIHAIGAKU-DUMMY-COM-EXT.
           EXIT.
      *
      *****************************************************************
      *    （リハ選）のコメントがあるか判定
      *****************************************************************
       2001-RIHASEN-HANTEI-SEC          SECTION.
      *
           IF     RECE64-SAFLG   =    "2"
               MOVE       99      TO     FLG-COMMENT
               PERFORM    20011-KENSENTOU-COMMENT-SEC
           END-IF
      *
           .
       2001-RIHASEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    精神科ショート・ケア・精神科デイ・ケアがあるか判定
      *****************************************************************
       2001-SEIKEA-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-SEIKEA-COMMENT   =    "1"
           AND    RECE64-FLG-SCPRO-COMMENT    =    SPACE
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       107     TO     WRK-JIDO-SETCOMATFLG
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-SEIKEA-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    疾患別等専門プログラム加算があるか判定
      *    経頭蓋磁気刺激療法があるか判定
      *    心不全に対する遠赤外線温熱療法があるか判定
      *****************************************************************
       2001-SCPRO-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-SCPRO-COMMENT   NOT =  SPACE
             MOVE         ZERO     TO     IDZ
             INITIALIZE                   WRK-FREE-AREA2
             IF   RECE64-FLG-SEIKEA-COMMENT  =  "1"
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE       107     TO     WRK-JIDO-SETCOMATFLG
      **********MOVE       99      TO     FLG-COMMENT
                MOVE       3       TO     FLG-COMMENT
                PERFORM   200111-SANTEI-DAY-SEC
                PERFORM   20011-FREE-COMMENT2-SEC
             END-IF
      *
             EVALUATE  RECE64-FLG-SCPRO-COMMENT
               WHEN      "2"
                 MOVE  CONS-SCPRO  TO     WRK-CHECK-SRYCD
               WHEN      "4"
                 MOVE  CONS-JIKSGK TO     WRK-CHECK-SRYCD
               WHEN      "5"
                 MOVE  CONS-ONNETU TO     WRK-CHECK-SRYCD
             END-EVALUATE
             PERFORM       20011-ZAIKYO-CHECK-SEC
      *
             IF   RECE64-FLG-SCPRO-COMMENT    =  "4"
             AND  WRK-ZAIKYO-YMD          NOT =  SPACE
               MOVE    CONS-JIKSGK-E TO  WRK-CHECK-SRYCD
               PERFORM     20011-ZAIKYO-CHECK2-SEC
             END-IF
      *
             IF   WRK-ZAIKYO-YMD   NOT =  SPACE
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                EVALUATE  RECE64-FLG-SCPRO-COMMENT
                  WHEN      "2"
                    MOVE   40      TO     WRK-JIDO-SETCOMATFLG
                  WHEN      "4"
                    MOVE   47      TO     WRK-JIDO-SETCOMATFLG
                  WHEN      "5"
                    MOVE  110      TO     WRK-JIDO-SETCOMATFLG
                END-EVALUATE
                MOVE       99      TO     FLG-COMMENT
      *
                ADD        1       TO     IDZ
      *
                MOVE       12      TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "治療開始日　　　月　　日"
                                   TO     WK-FREE-NAME2(IDZ)
      *
                MOVE    WRK-ZAIKYO-YMD(5:2)   TO  DAY-IDX2
                MOVE    TBL-DAY(DAY-IDX2)
                                   TO     WK-FREE-NAME2(IDZ)(13:4)
                MOVE    WRK-ZAIKYO-YMD(7:2)   TO  DAY-IDX2
                MOVE    TBL-DAY(DAY-IDX2)
                                   TO     WK-FREE-NAME2(IDZ)(19:4)
      *
                IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                  MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                  MOVE  WRK-JIDO-SETCOMATFLG
                                   TO     WK-FREE-SETCOMATFLG2(IDZ)
                END-IF
      *
                IF   WRK-ZAIKYO-YMD2   NOT =  SPACE
      *
                  ADD        1       TO     IDZ
      *
                  MOVE       12      TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE   "治療終了日　　　月　　日"
                                     TO     WK-FREE-NAME2(IDZ)
      *
                  MOVE    WRK-ZAIKYO-YMD2(5:2)  TO  DAY-IDX2
                  MOVE    TBL-DAY(DAY-IDX2)
                                     TO     WK-FREE-NAME2(IDZ)(13:4)
                  MOVE    WRK-ZAIKYO-YMD2(7:2)  TO  DAY-IDX2
                  MOVE    TBL-DAY(DAY-IDX2)
                                     TO     WK-FREE-NAME2(IDZ)(19:4)
      *
                  IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                    MOVE  "2"        TO     WK-FREE-MEISKYFLG2(IDZ)
                    MOVE  WRK-JIDO-SETCOMATFLG
                                     TO     WK-FREE-SETCOMATFLG2(IDZ)
                  END-IF
      *
                END-IF
             END-IF
      *
             IF  (RECE64-FLG-SEIKEA-COMMENT  =  "1"  )
             OR  (WRK-ZAIKYO-YMD         NOT =  SPACE)
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-SCPRO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    外泊日のコメント編集
      *    精神科隔離病室管理加算（入院料加算）のコメント編集
      *****************************************************************
       2001-NYU-COMMENT-SEC          SECTION.
      *
           INITIALIZE  WRK-JIDO-SETCOM-AREA
      *
           PERFORM     20011-NYU-DAY-SEC
      *
           PERFORM     200111-RECE65-TAIHI-SEC
      *
           MOVE     ZERO    TO    FLG-NG
           IF   (RECE64-ZAISKBKBN  =  "2"                          )
           AND  (FLG-RECE65        =  ZERO                         )
           AND  (WRK-RECE65-SRYCD(1)  =  "190108890" OR "190142190"
                                      OR "190108790" OR "190142090"
                                      OR "190108990" OR "190109090"
                                      OR "190142290" OR "190141590"
                                      OR "999999982" OR "999999992"
                                      OR "999999983" OR "999999993"
                                      OR "190116890" OR "999999984"
                                      OR "999999994"
                                      OR "190153190" OR "190153390"
                                      OR "190153290" OR "190153090"
                                      OR "190182990"
                                      OR "190213790" OR "190214290"
                                      OR "190213890" OR "190214090"
                                      OR "190214390" OR "190213990"
                                      OR "190214190"
                                      OR "190200970" OR "190214470"
                                      OR "190205270" OR "190205370"
                                      OR "190205470" OR "190205870"
                                      OR "190212670"               )
              MOVE    1     TO    FLG-NG
           END-IF
           IF   (RECE64-ZAISKBKBN  =  "2"                          )
           AND  (FLG-RECE65        =  ZERO                         )
           AND  (WRK-RECE65-SRYCD(2)  =  "190161470" OR "190161570"
                                      OR "190161870" OR "190162070"
                                      OR "190161670" OR "190161770"
                                      OR "190161970" OR "190162170"
                                      OR "190198570" OR "190198670")
              MOVE  ZERO    TO    FLG-NG
           END-IF
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           MOVE    RECE64-MEISAISU      TO  MAX-MEISAISU
      *
           IF    FLG-NG  =  ZERO
      *      算定日コメントの挿入
             PERFORM  VARYING      IDX    FROM  1   BY   1
                      UNTIL       (IDX    >     31)      OR
                                  (TBL-NYU-DAY-ST(IDX)  =  ZERO)
               COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
      ***      MOVE    TBL-NYU-DAY-CD(IDX)
               MOVE    "810000001"
                             TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
               MOVE    ZERO  TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
               IF    TBL-NYU-DAY-CD(IDX)  =  CONS-SANTEI-COMMENT1
                 MOVE    3   TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                 MOVE    "　　日"
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
                 MOVE    TBL-NYU-DAY-ST(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(1:4)
               ELSE
                 MOVE    7   TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                 MOVE    "　　日〜　　日"
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
                 MOVE    TBL-NYU-DAY-ST(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(1:4)
                 MOVE    TBL-NYU-DAY-ED(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(9:4)
               END-IF
               IF   RECE64-ZAITEN   NOT =   ZERO
                 MOVE   "1"  TO  WRK-RECE65XX-RECEDENFLG(MAX-MEISAISU 1)
               END-IF
             END-PERFORM
           END-IF
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           IF     RECE64-ZAISKBKBN             =    "6"
           AND    RECE64-MEISAISU              =     1
             IF (RECE64-FLG-KAKURI-COMMENT = "1"                     )
             OR (RECE64-FLG-EISAPO-COMMENT = "1" OR "2"              )
             OR (RECE64-FLG-KOKKEA-COMMENT = "1"                     )
             OR (RECE64-FLG-JYUSYO-COMMENT = "1" OR "2" OR "3" OR "4")
             OR (RECE64-FLG-SEIRIE-COMMENT = "1"                     )
             OR (RECE64-FLG-BYOYAK-COMMENT = "1" OR "2" OR "3"       )
               PERFORM    20012-NYUKASAN-HENSYU-SEC
             END-IF
           END-IF
      *
           .
       2001-NYU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    外泊日の確定
      *    精神科隔離病室管理加算日（入院料加算）の確定
      *****************************************************************
       20011-NYU-DAY-SEC          SECTION.
      *
           INITIALIZE                TBL-NYU-DAY
      *
           MOVE    ZERO          TO  DAY-IDX
           MOVE    ZERO          TO  FLG-NYU-RENZOKU
      *
           PERFORM    VARYING    IDX  FROM   1   BY   1
                      UNTIL     (IDX      >   31)  OR
                                (DAY-IDX  >   31)
                IF    RECE64-KAISU(1 IDX)   =  ZERO
                      MOVE  ZERO  TO  FLG-NYU-RENZOKU
                ELSE
                  IF  (RECE64-ZAISKBKBN         =  "2")
                  AND (FLG-RYOCOM               =   1 )
                  AND (TBL-RYOCOM-NYUINKBN(IDX) =  "1")
                  AND (RECE62-SRYYM       >=  "201004")
                      MOVE  ZERO  TO  FLG-NYU-RENZOKU
                  ELSE
                      IF    FLG-NYU-RENZOKU  =  ZERO
                         ADD   1     TO  DAY-IDX
                         MOVE  IDX   TO  TBL-NYU-DAY-ST(DAY-IDX)
                      END-IF
                      MOVE  IDX      TO  TBL-NYU-DAY-ED(DAY-IDX)
                      MOVE  1        TO  FLG-NYU-RENZOKU
                  END-IF
                END-IF
           END-PERFORM
      *
           PERFORM    VARYING    DAY-IDX  FROM   1   BY   1
                      UNTIL  (DAY-IDX                      >   31)  OR
                             (TBL-NYU-DAY-ST(DAY-IDX)  = ZERO)
                IF    TBL-NYU-DAY-ST(DAY-IDX) 
                                    =   TBL-NYU-DAY-ED(DAY-IDX)
                      MOVE  CONS-SANTEI-COMMENT1
                                    TO  TBL-NYU-DAY-CD(DAY-IDX)
                ELSE
                      MOVE  CONS-SANTEI-COMMENT2
                                    TO  TBL-NYU-DAY-CD(DAY-IDX)
                END-IF
           END-PERFORM
      *
           .
       20011-NYU-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    超重症児（者）入院診療加算・準超重症児（者）入院診療加算
      *    の算定開始日コメント編集
      *****************************************************************
       2001-NYU-COMMENT2-SEC          SECTION.
      *
           INITIALIZE  WRK-JIDO-SETCOM-AREA
      *
           MOVE    SPACE                  TO  WRK-CYOJKSN-YMD
                                              WRK-CYOJKSN-INPUTCOMENT
           EVALUATE  RECE64-FLG-CYOJKSN-COMMENT
             WHEN      "1"
             WHEN      "3"
               MOVE  WRK-CYOJKSN-YMD-XXX  TO  WRK-CYOJKSN-YMD
               MOVE  WRK-CYOJKSN-COM-XXX  TO  WRK-CYOJKSN-INPUTCOMENT
             WHEN      "2"
             WHEN      "4"
               MOVE  WRK-CYOJKSN-YMD-YYY  TO  WRK-CYOJKSN-YMD
               MOVE  WRK-CYOJKSN-COM-YYY  TO  WRK-CYOJKSN-INPUTCOMENT
           END-EVALUATE
      *
           PERFORM     200111-RECE65-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           MOVE    RECE64-MEISAISU      TO  MAX-MEISAISU
      *
           IF    WRK-CYOJKSN-YMD  NOT =  SPACE
      *
      *      算定日コメントの挿入
             COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
             MOVE   "810000001"
                           TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO  TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
             IF   RECE62-SRYYM >= "201905"
               MOVE  16    TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
               MOVE  "算定開始　　　　　年　　月　　日"
                           TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
               MOVE  15    TO  IDX41
               MOVE  21    TO  IDX42
               MOVE  27    TO  IDX48
               MOVE  11    TO  IDX49
             ELSE
               MOVE  14    TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
               MOVE  "算定開始　　　年　　月　　日"
                           TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
               MOVE  11    TO  IDX41
               MOVE  17    TO  IDX42
               MOVE  23    TO  IDX48
             END-IF
      *
             INITIALIZE                         STS-AREA-DAY
             INITIALIZE                         LNK-DAY2-AREA
             MOVE    "21"                   TO  LNK-DAY2-IRAI
             MOVE    WRK-CYOJKSN-YMD        TO  LNK-DAY2-YMD
             CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                            LNK-DAY2-AREA
             IF      STS-DAY-RC1            =   ZERO
                MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                INITIALIZE                       ORCSKANACHKAREA
                MOVE    "2"                  TO  KANACHK-SYORI
                MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                IF      KANACHK-RC      =   ZERO
                   MOVE    KANACHK-OUT-INPUT(1:4)
                    TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(IDX41:4)
                END-IF
      *
                MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                INITIALIZE                       ORCSKANACHKAREA
                MOVE    "2"                  TO  KANACHK-SYORI
                MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                IF      KANACHK-RC      =   ZERO
                   MOVE    KANACHK-OUT-INPUT(1:4)
                    TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(IDX42:4)
                END-IF
      *
                MOVE    LNK-DAY2-EDTYMD1-D   TO  WRK-TAIHI-XX
                INITIALIZE                       ORCSKANACHKAREA
                MOVE    "2"                  TO  KANACHK-SYORI
                MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                IF      KANACHK-RC      =   ZERO
                   MOVE    KANACHK-OUT-INPUT(1:4)
                    TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(IDX48:4)
                END-IF
      *
                IF   RECE62-SRYYM >= "201905"
                   MOVE    LNK-DAY2-EDTYMD2-G
                    TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(IDX49:4)
                END-IF
             END-IF
      *
             IF    WRK-CYOJKSN-INPUTCOMENT  NOT =  SPACE
               MOVE    SPACE  TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
               IF    WRK-CYOJKSN-INPUTCOMENT(15:2) = "年"
                 MOVE  14     TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                 MOVE  WRK-CYOJKSN-INPUTCOMENT(1:28)
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
               ELSE
                 MOVE  16     TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                 MOVE  WRK-CYOJKSN-INPUTCOMENT(1:32)
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
               END-IF
             END-IF
      *
             MOVE    "1"   TO  WRK-RECE65XX-RECEDENFLG(MAX-MEISAISU 1)
      *
           END-IF
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           IF    WRK-CYOJKSN-YMD  NOT =  SPACE
             COMPUTE IDX30   =  RECE64-KOHBUNRI + 1
             EVALUATE  RECE64-FLG-CYOJKSN-COMMENT
               WHEN      "1"
               WHEN      "3"
                 IF    TBL-CYOJKSN-X(IDX30) =  ZERO
                   PERFORM    20012-NYUKASAN-HENSYU2-SEC
                   MOVE   1   TO   TBL-CYOJKSN-X(IDX30)
                 END-IF
               WHEN      "2"
               WHEN      "4"
                 IF    TBL-CYOJKSN-Y(IDX30) =  ZERO
                   PERFORM    20012-NYUKASAN-HENSYU2-SEC
                   MOVE   1   TO   TBL-CYOJKSN-Y(IDX30)
                 END-IF
             END-EVALUATE
           END-IF
      *
           .
       2001-NYU-COMMENT2-EXT.
           EXIT.
      *
      *****************************************************************
      *    超重症児（者）入院診療加算・準超重症児（者）入院診療加算
      *    の算定開始日コメント編集
      *****************************************************************
       2001-NYU-COMMENT3-SEC          SECTION.
      *
           MOVE        SPACE    TO    WRK-CYOJKSN-INPUTCOMENT3
      *
           PERFORM     200111-RECE65-TAIHI-SEC
      *
           PERFORM
               UNTIL  ( FLG-RECE65           =   1                   )
               OR     ( RECE64-KEY1      NOT =   RECE65-KEY1         )
               OR     ( RECE64-ZAINUM    NOT =   RECE65-ZAINUM       )
               OR     ( RECE64-ZAIREN    NOT =   RECE65-ZAIREN       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   5     )
                         OR      ( RECE65-SRYCD(IDX) =   SPACE )
                  IF  (RECE65-SRYCD(IDX) = "840000056" OR "840000556")
                  AND (WRK-CYOJKSN-INPUTCOMENT3  =  SPACE            )
                     MOVE  RECE65-NAME(IDX)(1:80)
                                     TO  WRK-CYOJKSN-INPUTCOMENT3
                  END-IF
               END-PERFORM
      *
               PERFORM 990-RECE65-NEXT-SEC
      *
           END-PERFORM
      *
           IF  WRK-CYOJKSN-INPUTCOMENT3  NOT = SPACE
               COMPUTE   IDX31   =  RECE64-KOHBUNRI + 1
               EVALUATE  RECE64-FLG-CYOJKSN-COMMENT
                 WHEN      "5"
                 WHEN      "7"
                   MOVE     ZERO     TO    FLG-ARI
                   PERFORM  VARYING  IDX32    FROM    1   BY  1
                             UNTIL  (IDX32                  >  10  )
                              OR    (TBL-CYOJKSN2-X-TAIHIYMD
                                              (IDX31 IDX32) = SPACE)
                              OR    (FLG-ARI                =   1  )
                     IF  TBL-CYOJKSN2-X-TAIHIYMD(IDX31 IDX32)
                                     =  WRK-CYOJKSN-INPUTCOMENT3
                        MOVE   1     TO    FLG-ARI
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI  =  ZERO
                   AND   IDX32   <=   10
                     PERFORM    20012-NYUKASAN-HENSYU3-SEC
                     MOVE   WRK-CYOJKSN-INPUTCOMENT3
                              TO  TBL-CYOJKSN2-X-TAIHIYMD(IDX31 IDX32)
                   END-IF
                 WHEN      "6"
                 WHEN      "8"
                   MOVE     ZERO     TO    FLG-ARI
                   PERFORM  VARYING  IDX32    FROM    1   BY  1
                             UNTIL  (IDX32                  >  10  )
                              OR    (TBL-CYOJKSN2-Y-TAIHIYMD
                                              (IDX31 IDX32) = SPACE)
                              OR    (FLG-ARI                =   1  )
                     IF  TBL-CYOJKSN2-Y-TAIHIYMD(IDX31 IDX32)
                                     =  WRK-CYOJKSN-INPUTCOMENT3
                        MOVE   1     TO    FLG-ARI
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI  =  ZERO
                   AND   IDX32   <=   10
                     PERFORM    20012-NYUKASAN-HENSYU3-SEC
                     MOVE   WRK-CYOJKSN-INPUTCOMENT3
                              TO  TBL-CYOJKSN2-Y-TAIHIYMD(IDX31 IDX32)
                   END-IF
               END-EVALUATE
           END-IF
      *
           .
       2001-NYU-COMMENT3-EXT.
           EXIT.
      *
      *****************************************************************
      *    精神科隔離病室管理加算（入院料加算）のコメントがあるか判定
      *    （診療行為入力）
      *****************************************************************
       2001-KAKURI-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-KAKURI-COMMENT    =    "1"
               MOVE       99      TO     FLG-COMMENT
               PERFORM   2001-NYU-COMMENT-SEC
           END-IF         
      *
           .
       2001-KAKURI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    栄養サポートチーム加算（入院料加算）のコメントがあるか判定
      *    （診療行為入力）
      *****************************************************************
       2001-EISAPO-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-EISAPO-COMMENT    =    "1" OR "2"
               MOVE       99      TO     FLG-COMMENT
               PERFORM   2001-NYU-COMMENT-SEC
           END-IF
      *
           .
       2001-EISAPO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    呼吸ケアチーム加算（入院料加算）のコメントがあるか判定
      *    （診療行為入力）
      *****************************************************************
       2001-KOKKEA-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-KOKKEA-COMMENT    =    "1"
               MOVE       99      TO     FLG-COMMENT
               PERFORM   2001-NYU-COMMENT-SEC
           END-IF
      *
           .
       2001-KOKKEA-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    重症者加算（精神療養病棟入院料）（入院料加算）のコメントがあるか判定
      *    （診療行為入力）
      *****************************************************************
       2001-JYUSYO-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-JYUSYO-COMMENT     =
                                         "1" OR "2" OR "3" OR "4"
               MOVE       99      TO     FLG-COMMENT
               PERFORM   2001-NYU-COMMENT-SEC
           END-IF
      *
           .
       2001-JYUSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    精神科リエゾンチーム加算があるか判定
      *    （診療行為入力）
      *****************************************************************
       2001-SEIRIE-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-SEIRIE-COMMENT    =    "1"
               MOVE       99      TO     FLG-COMMENT
               PERFORM   2001-NYU-COMMENT-SEC
           END-IF
      *
           .
       2001-SEIRIE-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟薬剤業務実施加算があるか判定
      *    （診療行為入力）
      *****************************************************************
       2001-BYOYAK-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-BYOYAK-COMMENT    =    "1" OR "2" OR "3"
               MOVE       99      TO     FLG-COMMENT
               PERFORM   2001-NYU-COMMENT-SEC
           END-IF
      *
           .
       2001-BYOYAK-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    超重症児（者）入院診療加算・準超重症児（者）入院診療加算
      *    があるか判定（診療行為入力）
      *****************************************************************
       2001-CYOJKSN-HANTEI-SEC          SECTION.
      *
           IF      RECE64-FLG-CYOJKSN-COMMENT   =
                   "1" OR "2" OR "3" OR "4" OR "5" OR "6" OR "7" OR "8"
             EVALUATE  RECE64-FLG-CYOJKSN-COMMENT
               WHEN      "1"
               WHEN      "2"
               WHEN      "3"
               WHEN      "4"
                 MOVE       99      TO     FLG-COMMENT
                 PERFORM   2001-NYU-COMMENT2-SEC
               WHEN      "5"
               WHEN      "6"
               WHEN      "7"
               WHEN      "8"
                 PERFORM   2001-NYU-COMMENT3-SEC
             END-EVALUATE
           END-IF
      *
           .
       2001-CYOJKSN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    排尿自立支援加算があるか判定（診療行為入力）
      *****************************************************************
       2001-HAIJSK-HANTEI-SEC          SECTION.
      *
           IF     RECE64-FLG-HAIJSK-COMMENT   =    "1"
      *
             MOVE  SPACE             TO  WRK-HAIJSK-FSANTEIYM
             MOVE  ZERO              TO  WRK-HAIJSK-KAISU
      *
             INITIALIZE                  ORCSRECESRCHKAREA
             MOVE  "1"               TO  LNK-RECESRCHK-SYORI
             MOVE  RECE62-PTID       TO  LNK-RECESRCHK-PTID
             MOVE  RECE62-SRYYM      TO  LNK-RECESRCHK-SRYYM
             MOVE  RECE62-NYUGAIKBN  TO  LNK-RECESRCHK-NYUGAIKBN
             IF    RECE62-NYUINYMD   <   "20200401"
               MOVE  "20200401"      TO  LNK-RECESRCHK-SRYYMD(1)
             ELSE
               MOVE  RECE62-NYUINYMD TO  LNK-RECESRCHK-SRYYMD(1)
             END-IF
             IF    RECE62-HAIJSKYMD  <   LNK-RECESRCHK-SRYYMD(1)
               MOVE  RECE62-HAIJSKYMD  TO  LNK-RECESRCHK-SRYYMD(2)
             END-IF
             CALL  "ORCSRECESRCHK"   USING  SPA-AREA
                                            ORCSRECESRCHKAREA
             IF    LNK-RECESRCHK-RC  =  ZERO
                MOVE  LNK-RECESRCHK-FSANTEIYMD
                                           TO  WRK-HAIJSK-FSANTEIYM
                MOVE  LNK-RECESRCHK-KAISU  TO  WRK-HAIJSK-KAISU
             END-IF
      *
             IF   WRK-HAIJSK-FSANTEIYM  NOT = SPACE
             AND  WRK-HAIJSK-KAISU      NOT = ZERO
      *
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "初回算定　　　月　　日"
                                   TO     WK-FREE-NAME2(IDZ)
      *
                MOVE    WRK-HAIJSK-FSANTEIYM(5:2)  TO  DAY-IDX2
                MOVE    TBL-DAY(DAY-IDX2)
                                   TO  WK-FREE-NAME2(IDZ)(11:4)
                MOVE    WRK-HAIJSK-FSANTEIYM(7:2)  TO  DAY-IDX2
                MOVE    TBL-DAY(DAY-IDX2)
                                   TO  WK-FREE-NAME2(IDZ)(17:4)
      *
                IF    WRK-HAIJSK-KAISU  <=  31
                  ADD      1       TO     IDZ
      *
                  MOVE     11      TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE   "通算実施回数（　　回）"
                                   TO     WK-FREE-NAME2(IDZ)
                  MOVE    WRK-HAIJSK-KAISU  TO  DAY-IDX2
                  MOVE    TBL-DAY(DAY-IDX2)
                                   TO     WK-FREE-NAME2(IDZ)(15:4)
                END-IF
      *
                PERFORM    200111-RECE65-TAIHI-SEC
      *
      *         ＲＥＣＥ０５ファイルの内容をテーブルに退避
                MOVE    ZERO                 TO  IDX-RECE65
                MOVE    SPACE                TO  WRK-RECE65XX-REC
                INITIALIZE                       WRK-RECE65XX-REC
      *
                MOVE    RECE64-MEISAISU      TO  MAX-MEISAISU
      *
                PERFORM   VARYING   IDX   FROM    1   BY  1
                           UNTIL   (IDX                >   50   )
                             OR    (WK-FREE-NAME2(IDX) =   SPACE)
                  COMPUTE  MAX-MEISAISU = MAX-MEISAISU + 1
                  MOVE "810000001"
                            TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
                  MOVE ZERO TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
                  MOVE WK-FREE-YUKOKETA2(IDX)
                            TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                  MOVE WK-FREE-NAME2(IDX)
                            TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
                  MOVE "1"  TO  WRK-RECE65XX-RECEDENFLG(MAX-MEISAISU 1)
                END-PERFORM
      *
                PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
                PERFORM    20012-NYUKASAN-HENSYU4-SEC
      *
             END-IF
      *
           END-IF
      *
           .
       2001-HAIJSK-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    （レセ電用）
      *****************************************************************
       20012-NYUKASAN-HENSYU-SEC          SECTION.
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           MOVE    1                    TO  MAX-MEISAISU
      *
           MOVE  "810000001"  TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
           MOVE   ZERO        TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
           IF  (RECE64-FLG-KAKURI-COMMENT    =    "1")
             MOVE   13        TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "精神科隔離室管理加算算定日"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           END-IF
           IF  (RECE64-FLG-EISAPO-COMMENT    =    "1")
             MOVE   14        TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "栄養サポートチーム加算算定日"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           END-IF
           IF  (RECE64-FLG-EISAPO-COMMENT    =    "2")
             MOVE   20        TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "栄養サポートチーム加算（指定地域）算定日"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           END-IF
           IF  (RECE64-FLG-KOKKEA-COMMENT    =    "1")
             MOVE   12        TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "呼吸ケアチーム加算算定日"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           END-IF
           IF  (RECE64-FLG-JYUSYO-COMMENT    =    "2")
            IF  RECE62-SRYYM   >=  "201404"
              MOVE   20       TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
              MOVE  "重症者加算１（精神療養病棟入院料）算定日"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
            ELSE
              MOVE   9        TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
              MOVE  "重症者加算１算定日"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
            END-IF
           END-IF
           IF  (RECE64-FLG-JYUSYO-COMMENT    =    "1")
            IF  RECE62-SRYYM   >=  "201404"
              MOVE   20       TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
              MOVE  "重症者加算２（精神療養病棟入院料）算定日"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
            ELSE
             IF  RECE62-SRYYM   >=  "201204"
              MOVE   9        TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
              MOVE  "重症者加算２算定日"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
             ELSE
              MOVE   19       TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
              MOVE  "重症者加算（精神療養病棟入院料）算定日"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
             END-IF
            END-IF
           END-IF
           IF  (RECE64-FLG-JYUSYO-COMMENT    =    "3")
              MOVE   24       TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
              MOVE  "重症者加算１（地域移行機能強化病棟入院料）算定日"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           END-IF
           IF  (RECE64-FLG-JYUSYO-COMMENT    =    "4")
              MOVE   24       TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
              MOVE  "重症者加算２（地域移行機能強化病棟入院料）算定日"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           END-IF
           IF  (RECE64-FLG-SEIRIE-COMMENT    =    "1")
             MOVE   15        TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "精神科リエゾンチーム加算算定日"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           END-IF
           IF  (RECE64-FLG-BYOYAK-COMMENT    =    "1")
             MOVE   13        TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "病棟薬剤業務実施加算算定日"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           END-IF
           IF  (RECE64-FLG-BYOYAK-COMMENT    =    "2")
             MOVE   14        TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "病棟薬剤業務実施加算１算定日"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           END-IF
           IF  (RECE64-FLG-BYOYAK-COMMENT    =    "3")
             MOVE   14        TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "病棟薬剤業務実施加算２算定日"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           END-IF
           MOVE  "1"          TO  WRK-RECE65XX-RECEPTFLG(MAX-MEISAISU 1)
      *    算定日コメントの挿入
           PERFORM  VARYING      IDX    FROM  1   BY   1
                    UNTIL       (IDX    >     31)      OR
                                (TBL-NYU-DAY-ST(IDX)  =  ZERO)
               COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
               MOVE    "810000001"
                             TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
               MOVE    ZERO  TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
               IF    TBL-NYU-DAY-CD(IDX)  =  CONS-SANTEI-COMMENT1
                 MOVE    3   TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                 MOVE    "　　日"
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
                 MOVE    TBL-NYU-DAY-ST(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(1:4)
               ELSE
                 MOVE    7   TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                 MOVE    "　　日〜　　日"
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
                 MOVE    TBL-NYU-DAY-ST(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(1:4)
                 MOVE    TBL-NYU-DAY-ED(IDX)
                             TO  DAY-IDX
                 MOVE    TBL-DAY(DAY-IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(9:4)
               END-IF
               MOVE   "1"    TO  WRK-RECE65XX-RECEPTFLG(MAX-MEISAISU 1)
           END-PERFORM
      *
      *    コメントを含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
               INITIALIZE                        RECE659-REC
               MOVE    RECE64-KEY           TO   RECE659-KEY
               COMPUTE IDX19              =  RECE64-KOHBUNRI + 1
               EVALUATE  RECE64-SRYKBN
                 WHEN      "90"
                   IF  (RECE64-FLG-KAKURI-COMMENT    =    "1")
                     COMPUTE RECE659-ZAINUM =  99994000  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-EISAPO-COMMENT    =    "1")
                     COMPUTE RECE659-ZAINUM =  99994005  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-EISAPO-COMMENT    =    "2")
                     COMPUTE RECE659-ZAINUM =  99994010  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-KOKKEA-COMMENT    =    "1")
                     COMPUTE RECE659-ZAINUM =  99994015  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-JYUSYO-COMMENT    =    "2")
                     COMPUTE RECE659-ZAINUM =  99994020  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-JYUSYO-COMMENT    =    "1")
                     COMPUTE RECE659-ZAINUM =  99994025  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-JYUSYO-COMMENT    =    "3")
                     COMPUTE RECE659-ZAINUM =  99994030  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-JYUSYO-COMMENT    =    "4")
                     COMPUTE RECE659-ZAINUM =  99994035  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-SEIRIE-COMMENT    =    "1")
                     COMPUTE RECE659-ZAINUM =  99994040  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-BYOYAK-COMMENT    =    "1")
                     COMPUTE RECE659-ZAINUM =  99994045  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-BYOYAK-COMMENT    =    "2")
                     COMPUTE RECE659-ZAINUM =  99994050  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-BYOYAK-COMMENT    =    "3")
                     COMPUTE RECE659-ZAINUM =  99994055  +  IDX19
                   END-IF
                   MOVE    "900"            TO   RECE659-SRYSYUKBN
                 WHEN      "92"
                   IF  (RECE64-FLG-KAKURI-COMMENT    =    "1")
                     COMPUTE RECE659-ZAINUM =  99994500  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-EISAPO-COMMENT    =    "1")
                     COMPUTE RECE659-ZAINUM =  99994505  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-EISAPO-COMMENT    =    "2")
                     COMPUTE RECE659-ZAINUM =  99994510  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-KOKKEA-COMMENT    =    "1")
                     COMPUTE RECE659-ZAINUM =  99994515  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-JYUSYO-COMMENT    =    "2")
                     COMPUTE RECE659-ZAINUM =  99994520  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-JYUSYO-COMMENT    =    "1")
                     COMPUTE RECE659-ZAINUM =  99994525  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-JYUSYO-COMMENT    =    "3")
                     COMPUTE RECE659-ZAINUM =  99994530  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-JYUSYO-COMMENT    =    "4")
                     COMPUTE RECE659-ZAINUM =  99994535  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-SEIRIE-COMMENT    =    "1")
                     COMPUTE RECE659-ZAINUM =  99994540  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-BYOYAK-COMMENT    =    "1")
                     COMPUTE RECE659-ZAINUM =  99994545  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-BYOYAK-COMMENT    =    "2")
                     COMPUTE RECE659-ZAINUM =  99994550  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-BYOYAK-COMMENT    =    "3")
                     COMPUTE RECE659-ZAINUM =  99994555  +  IDX19
                   END-IF
                   MOVE    "920"            TO   RECE659-SRYSYUKBN
               END-EVALUATE
               MOVE    ZERO                 TO   RECE659-ZAIREN
               ADD     1                    TO   WRK-RENNUM
               MOVE    WRK-RENNUM           TO   RECE659-RENNUM
               MOVE    RECE64-KOHBUNRI      TO   RECE659-KOHBUNRI
               MOVE    RECE64-PTNUM         TO   RECE659-PTNUM
               MOVE    RECE64-SRYKA         TO   RECE659-SRYKA
               MOVE    RECE64-SRYKBN        TO   RECE659-SRYKBN
               MOVE    ZERO                 TO   RECE659-JIHIMONEY
               PERFORM  VARYING   IDX   FROM   1   BY   1
                         UNTIL   (IDX       >    5)   OR
                                 (FLG-WRKRECE65   =   1)
                   ADD        1             TO    IDX-RECE65
                   IF (WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE)
                   OR (IDX-RECE65                       >  100 )
                       MOVE   1             TO    FLG-WRKRECE65
                   ELSE
                       MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                        TO   RECE659-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
               END-PERFORM
               IF   RECE659-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE659-REC
                   ADD     1            TO   CNT-RECE659
               END-IF
           END-PERFORM
      *
           PERFORM 500-RECE64-KEY-HENSYU-SEC
           COMPUTE IDX19                =  RECE64-KOHBUNRI + 1
           EVALUATE  RECE64-SRYKBN
             WHEN      "90"
               IF  (RECE64-FLG-KAKURI-COMMENT    =    "1")
                 COMPUTE RECE649-ZAINUM =  99994000  +  IDX19
               END-IF
               IF  (RECE64-FLG-EISAPO-COMMENT    =    "1")
                 COMPUTE RECE649-ZAINUM =  99994005  +  IDX19
               END-IF
               IF  (RECE64-FLG-EISAPO-COMMENT    =    "2")
                 COMPUTE RECE649-ZAINUM =  99994010  +  IDX19
               END-IF
               IF  (RECE64-FLG-KOKKEA-COMMENT    =    "1")
                 COMPUTE RECE649-ZAINUM =  99994015  +  IDX19
               END-IF
               IF  (RECE64-FLG-JYUSYO-COMMENT    =    "2")
                 COMPUTE RECE649-ZAINUM =  99994020  +  IDX19
               END-IF
               IF  (RECE64-FLG-JYUSYO-COMMENT    =    "1")
                 COMPUTE RECE649-ZAINUM =  99994025  +  IDX19
               END-IF
               IF  (RECE64-FLG-JYUSYO-COMMENT    =    "3")
                 COMPUTE RECE649-ZAINUM =  99994030  +  IDX19
               END-IF
               IF  (RECE64-FLG-JYUSYO-COMMENT    =    "4")
                 COMPUTE RECE649-ZAINUM =  99994035  +  IDX19
               END-IF
               IF  (RECE64-FLG-SEIRIE-COMMENT    =    "1")
                 COMPUTE RECE649-ZAINUM =  99994040  +  IDX19
               END-IF
               IF  (RECE64-FLG-BYOYAK-COMMENT    =    "1")
                 COMPUTE RECE649-ZAINUM =  99994045  +  IDX19
               END-IF
               IF  (RECE64-FLG-BYOYAK-COMMENT    =    "2")
                 COMPUTE RECE649-ZAINUM =  99994050  +  IDX19
               END-IF
               IF  (RECE64-FLG-BYOYAK-COMMENT    =    "3")
                 COMPUTE RECE649-ZAINUM =  99994055  +  IDX19
               END-IF
             WHEN      "92"
               IF  (RECE64-FLG-KAKURI-COMMENT    =    "1")
                 COMPUTE RECE649-ZAINUM =  99994500  +  IDX19
               END-IF
               IF  (RECE64-FLG-EISAPO-COMMENT    =    "1")
                 COMPUTE RECE649-ZAINUM =  99994505  +  IDX19
               END-IF
               IF  (RECE64-FLG-EISAPO-COMMENT    =    "2")
                 COMPUTE RECE649-ZAINUM =  99994510  +  IDX19
               END-IF
               IF  (RECE64-FLG-KOKKEA-COMMENT    =    "1")
                 COMPUTE RECE649-ZAINUM =  99994515  +  IDX19
               END-IF
               IF  (RECE64-FLG-JYUSYO-COMMENT    =    "2")
                 COMPUTE RECE649-ZAINUM =  99994520  +  IDX19
               END-IF
               IF  (RECE64-FLG-JYUSYO-COMMENT    =    "1")
                 COMPUTE RECE649-ZAINUM =  99994525  +  IDX19
               END-IF
               IF  (RECE64-FLG-JYUSYO-COMMENT    =    "3")
                 COMPUTE RECE649-ZAINUM =  99994530  +  IDX19
               END-IF
               IF  (RECE64-FLG-JYUSYO-COMMENT    =    "4")
                 COMPUTE RECE649-ZAINUM =  99994535  +  IDX19
               END-IF
               IF  (RECE64-FLG-SEIRIE-COMMENT    =    "1")
                 COMPUTE RECE649-ZAINUM =  99994540  +  IDX19
               END-IF
               IF  (RECE64-FLG-BYOYAK-COMMENT    =    "1")
                 COMPUTE RECE649-ZAINUM =  99994545  +  IDX19
               END-IF
               IF  (RECE64-FLG-BYOYAK-COMMENT    =    "2")
                 COMPUTE RECE649-ZAINUM =  99994550  +  IDX19
               END-IF
               IF  (RECE64-FLG-BYOYAK-COMMENT    =    "3")
                 COMPUTE RECE649-ZAINUM =  99994555  +  IDX19
               END-IF
           END-EVALUATE
           MOVE    ZERO                TO  RECE649-ZAIREN
           MOVE    RECE64-KOHBUNRI     TO  RECE649-KOHBUNRI
           MOVE    RECE64-SRYKBN       TO  RECE649-SRYKBN
           MOVE    "6"                 TO  RECE649-ZAISKBKBN
           MOVE    RECE64-PTNUM        TO  RECE649-PTNUM
           MOVE    RECE64-SRYKA        TO  RECE649-SRYKA
           MOVE    RECE64-HKNCOMBI     TO  RECE649-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           .
       20012-NYUKASAN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    （レセ電用）
      *****************************************************************
       20012-NYUKASAN-HENSYU2-SEC          SECTION.
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           MOVE    1                    TO  MAX-MEISAISU
      *
           MOVE  "810000001"  TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
           MOVE   ZERO        TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
           IF  (RECE64-FLG-CYOJKSN-COMMENT    =    "1" OR "3")
             MOVE   17        TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "超重症児（者）入院診療加算算定開始"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           END-IF
           IF  (RECE64-FLG-CYOJKSN-COMMENT    =    "2" OR "4")
             MOVE   18        TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "準超重症児（者）入院診療加算算定開始"
                              TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           END-IF
           MOVE  "1"          TO  WRK-RECE65XX-RECEPTFLG(MAX-MEISAISU 1)
      *
      *    算定日コメントの挿入
           COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
           MOVE   "810000001"
                         TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
           MOVE    ZERO  TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
           IF   RECE62-SRYYM >= "201905"
             MOVE  11    TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "　　　　年　　月　　日"
                         TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
             MOVE   5    TO  IDX41
             MOVE  11    TO  IDX42
             MOVE  17    TO  IDX48
             MOVE   1    TO  IDX49
           ELSE
             MOVE   9    TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "　　年　　月　　日"
                         TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
             MOVE   1    TO  IDX41
             MOVE   7    TO  IDX42
             MOVE  13    TO  IDX48
           END-IF
      *
           INITIALIZE                         STS-AREA-DAY
           INITIALIZE                         LNK-DAY2-AREA
           MOVE    "21"                   TO  LNK-DAY2-IRAI
           MOVE    WRK-CYOJKSN-YMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                          LNK-DAY2-AREA
           IF      STS-DAY-RC1            =   ZERO
              MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                    TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(IDX41:4)
              END-IF
      *
              MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                    TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(IDX42:4)
              END-IF
      *
              MOVE    LNK-DAY2-EDTYMD1-D   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                    TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(IDX48:4)
              END-IF
      *
              IF   RECE62-SRYYM >= "201905"
                 MOVE    LNK-DAY2-EDTYMD2-G
                    TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(IDX49:4)
              END-IF
           END-IF
      *
           IF    WRK-CYOJKSN-INPUTCOMENT  NOT =  SPACE
              MOVE    SPACE  TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
              IF    WRK-CYOJKSN-INPUTCOMENT(15:2) = "年"
                MOVE   9     TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE  WRK-CYOJKSN-INPUTCOMENT(11:18)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
              ELSE
                MOVE  11     TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE  WRK-CYOJKSN-INPUTCOMENT(11:22)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
              END-IF
           END-IF
      *
           MOVE   "1"    TO  WRK-RECE65XX-RECEPTFLG(MAX-MEISAISU 1)
      *
      *    コメントを含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
               INITIALIZE                        RECE659-REC
               MOVE    RECE64-KEY           TO   RECE659-KEY
               COMPUTE IDX19              =  RECE64-KOHBUNRI + 1
               EVALUATE  RECE64-SRYKBN
                 WHEN      "90"
                   IF  (RECE64-FLG-CYOJKSN-COMMENT   =    "1" OR "3")
                     COMPUTE RECE659-ZAINUM =  99994200  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-CYOJKSN-COMMENT   =    "2" OR "4")
                     COMPUTE RECE659-ZAINUM =  99994205  +  IDX19
                   END-IF
                   MOVE    "900"            TO   RECE659-SRYSYUKBN
                 WHEN      "92"
                   IF  (RECE64-FLG-CYOJKSN-COMMENT   =    "1" OR "3")
                     COMPUTE RECE659-ZAINUM =  99994700  +  IDX19
                   END-IF
                   IF  (RECE64-FLG-CYOJKSN-COMMENT   =    "2" OR "4")
                     COMPUTE RECE659-ZAINUM =  99994705  +  IDX19
                   END-IF
                   MOVE    "920"            TO   RECE659-SRYSYUKBN
               END-EVALUATE
               MOVE    ZERO                 TO   RECE659-ZAIREN
               ADD     1                    TO   WRK-RENNUM
               MOVE    WRK-RENNUM           TO   RECE659-RENNUM
               MOVE    RECE64-KOHBUNRI      TO   RECE659-KOHBUNRI
               MOVE    RECE64-PTNUM         TO   RECE659-PTNUM
               MOVE    RECE64-SRYKA         TO   RECE659-SRYKA
               MOVE    RECE64-SRYKBN        TO   RECE659-SRYKBN
               MOVE    ZERO                 TO   RECE659-JIHIMONEY
               PERFORM  VARYING   IDX   FROM   1   BY   1
                         UNTIL   (IDX       >    5)   OR
                                 (FLG-WRKRECE65   =   1)
                   ADD        1             TO    IDX-RECE65
                   IF (WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE)
                   OR (IDX-RECE65                       >  100 )
                       MOVE   1             TO    FLG-WRKRECE65
                   ELSE
                       MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                        TO   RECE659-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
               END-PERFORM
               IF   RECE659-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE659-REC
                   ADD     1            TO   CNT-RECE659
               END-IF
           END-PERFORM
      *
           PERFORM 500-RECE64-KEY-HENSYU-SEC
           COMPUTE IDX19                =  RECE64-KOHBUNRI + 1
           EVALUATE  RECE64-SRYKBN
             WHEN      "90"
               IF  (RECE64-FLG-CYOJKSN-COMMENT   =    "1" OR "3")
                 COMPUTE RECE649-ZAINUM =  99994200  +  IDX19
               END-IF
               IF  (RECE64-FLG-CYOJKSN-COMMENT   =    "2" OR "4")
                 COMPUTE RECE649-ZAINUM =  99994205  +  IDX19
               END-IF
             WHEN      "92"
               IF  (RECE64-FLG-CYOJKSN-COMMENT   =    "1" OR "3")
                 COMPUTE RECE649-ZAINUM =  99994700  +  IDX19
               END-IF
               IF  (RECE64-FLG-CYOJKSN-COMMENT   =    "2" OR "4")
                 COMPUTE RECE649-ZAINUM =  99994705  +  IDX19
               END-IF
           END-EVALUATE
           MOVE    ZERO                TO  RECE649-ZAIREN
           MOVE    RECE64-KOHBUNRI     TO  RECE649-KOHBUNRI
           MOVE    RECE64-SRYKBN       TO  RECE649-SRYKBN
           MOVE    "6"                 TO  RECE649-ZAISKBKBN
           MOVE    RECE64-PTNUM        TO  RECE649-PTNUM
           MOVE    RECE64-SRYKA        TO  RECE649-SRYKA
           MOVE    RECE64-HKNCOMBI     TO  RECE649-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           .
       20012-NYUKASAN-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    （レセ電用）
      *****************************************************************
       20012-NYUKASAN-HENSYU3-SEC          SECTION.
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           MOVE    1                    TO  MAX-MEISAISU
      *
           MOVE  "810000001"  TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
           MOVE   ZERO        TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
           IF  (RECE64-FLG-CYOJKSN-COMMENT    =    "5" OR "7")
             MOVE   30        TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "超重症児（者）入院診療加算算定開始"
                          TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(1:34)
             MOVE  "（入院年月日以前の算定分）"
                          TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(35:26)
           END-IF
           IF  (RECE64-FLG-CYOJKSN-COMMENT    =    "6" OR "8")
             MOVE   31        TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "準超重症児（者）入院診療加算算定開始"
                          TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(1:36)
             MOVE  "（入院年月日以前の算定分）"
                          TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(37:26)
           END-IF
           MOVE  "1"          TO  WRK-RECE65XX-RECEPTFLG(MAX-MEISAISU 1)
      *
      *    算定日コメントの挿入
           COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
           MOVE   "810000001"
                         TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
           MOVE    ZERO  TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
      *
           IF    WRK-CYOJKSN-INPUTCOMENT3(15:2) = "年"
             MOVE   9    TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  WRK-CYOJKSN-INPUTCOMENT3(11:18)
                         TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           ELSE
             MOVE  11    TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  WRK-CYOJKSN-INPUTCOMENT3(11:22)
                         TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           END-IF
      *
           MOVE   "1"    TO  WRK-RECE65XX-RECEPTFLG(MAX-MEISAISU 1)
      *
      *    コメントを含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
               INITIALIZE                        RECE659-REC
               MOVE    RECE64-KEY           TO   RECE659-KEY
               COMPUTE IDX19              =  RECE64-KOHBUNRI + 1
               EVALUATE  RECE64-SRYKBN
                 WHEN      "90"
                   IF  (RECE64-FLG-CYOJKSN-COMMENT   =    "5" OR "7")
                     COMPUTE RECE659-ZAINUM = 99994100 + (IDX19 * IDX32)
                   END-IF
                   IF  (RECE64-FLG-CYOJKSN-COMMENT   =    "6" OR "8")
                     COMPUTE RECE659-ZAINUM = 99994150 + (IDX19 * IDX32)
                   END-IF
                   MOVE    "900"            TO   RECE659-SRYSYUKBN
                 WHEN      "92"
                   IF  (RECE64-FLG-CYOJKSN-COMMENT   =    "5" OR "7")
                     COMPUTE RECE659-ZAINUM = 99994600 + (IDX19 * IDX32)
                   END-IF
                   IF  (RECE64-FLG-CYOJKSN-COMMENT   =    "6" OR "8")
                     COMPUTE RECE659-ZAINUM = 99994650 + (IDX19 * IDX32)
                   END-IF
                   MOVE    "920"            TO   RECE659-SRYSYUKBN
               END-EVALUATE
               MOVE    ZERO                 TO   RECE659-ZAIREN
               ADD     1                    TO   WRK-RENNUM
               MOVE    WRK-RENNUM           TO   RECE659-RENNUM
               MOVE    RECE64-KOHBUNRI      TO   RECE659-KOHBUNRI
               MOVE    RECE64-PTNUM         TO   RECE659-PTNUM
               MOVE    RECE64-SRYKA         TO   RECE659-SRYKA
               MOVE    RECE64-SRYKBN        TO   RECE659-SRYKBN
               MOVE    ZERO                 TO   RECE659-JIHIMONEY
               PERFORM  VARYING   IDX   FROM   1   BY   1
                         UNTIL   (IDX       >    5)   OR
                                 (FLG-WRKRECE65   =   1)
                   ADD        1             TO    IDX-RECE65
                   IF (WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE)
                   OR (IDX-RECE65                       >  100 )
                       MOVE   1             TO    FLG-WRKRECE65
                   ELSE
                       MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                        TO   RECE659-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
               END-PERFORM
               IF   RECE659-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE659-REC
                   ADD     1            TO   CNT-RECE659
               END-IF
           END-PERFORM
      *
           PERFORM 500-RECE64-KEY-HENSYU-SEC
           COMPUTE IDX19                =  RECE64-KOHBUNRI + 1
           EVALUATE  RECE64-SRYKBN
             WHEN      "90"
               IF  (RECE64-FLG-CYOJKSN-COMMENT   =    "5" OR "7")
                 COMPUTE RECE649-ZAINUM = 99994100 + (IDX19 * IDX32)
               END-IF
               IF  (RECE64-FLG-CYOJKSN-COMMENT   =    "6" OR "8")
                 COMPUTE RECE649-ZAINUM = 99994150 + (IDX19 * IDX32)
               END-IF
             WHEN      "92"
               IF  (RECE64-FLG-CYOJKSN-COMMENT   =    "5" OR "7")
                 COMPUTE RECE649-ZAINUM = 99994600 + (IDX19 * IDX32)
               END-IF
               IF  (RECE64-FLG-CYOJKSN-COMMENT   =    "6" OR "8")
                 COMPUTE RECE649-ZAINUM = 99994650 + (IDX19 * IDX32)
               END-IF
           END-EVALUATE
           MOVE    ZERO                TO  RECE649-ZAIREN
           MOVE    RECE64-KOHBUNRI     TO  RECE649-KOHBUNRI
           MOVE    RECE64-SRYKBN       TO  RECE649-SRYKBN
           MOVE    "6"                 TO  RECE649-ZAISKBKBN
           MOVE    RECE64-PTNUM        TO  RECE649-PTNUM
           MOVE    RECE64-SRYKA        TO  RECE649-SRYKA
           MOVE    RECE64-HKNCOMBI     TO  RECE649-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           .
       20012-NYUKASAN-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    （レセ電用）
      *****************************************************************
       20012-NYUKASAN-HENSYU4-SEC          SECTION.
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           MOVE    1                    TO  MAX-MEISAISU
      *
           MOVE  "810000001"   TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
           MOVE   ZERO         TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
           MOVE   19           TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
           MOVE  "排尿自立支援加算初回算定　　　月　　日"
                          TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
           MOVE    WRK-HAIJSK-FSANTEIYM(5:2)  TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)
                          TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(27:4)
           MOVE    WRK-HAIJSK-FSANTEIYM(7:2)  TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)
                          TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(33:4)
           MOVE  "1"      TO  WRK-RECE65XX-RECEPTFLG(MAX-MEISAISU 1)
      *
           IF    WRK-HAIJSK-KAISU  <=  31
             COMPUTE  MAX-MEISAISU = MAX-MEISAISU + 1
      *
             MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
             MOVE   ZERO       TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE   19         TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE  "排尿自立支援加算通算実施回数（　　回）"
                          TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
             MOVE    WRK-HAIJSK-KAISU  TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                          TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)(31:4)
             MOVE  "1"    TO  WRK-RECE65XX-RECEPTFLG(MAX-MEISAISU 1)
           END-IF
      *
      *    コメントを含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
               INITIALIZE                        RECE659-REC
               MOVE    RECE64-KEY           TO   RECE659-KEY
               COMPUTE IDX19              =  RECE64-KOHBUNRI + 1
               EVALUATE  RECE64-SRYKBN
                 WHEN      "90"
                   COMPUTE RECE659-ZAINUM =  99994210  +  IDX19
                   MOVE    "900"            TO   RECE659-SRYSYUKBN
                 WHEN      "92"
                   COMPUTE RECE659-ZAINUM =  99994710  +  IDX19
                   MOVE    "920"            TO   RECE659-SRYSYUKBN
               END-EVALUATE
               MOVE    ZERO                 TO   RECE659-ZAIREN
               ADD     1                    TO   WRK-RENNUM
               MOVE    WRK-RENNUM           TO   RECE659-RENNUM
               MOVE    RECE64-KOHBUNRI      TO   RECE659-KOHBUNRI
               MOVE    RECE64-PTNUM         TO   RECE659-PTNUM
               MOVE    RECE64-SRYKA         TO   RECE659-SRYKA
               MOVE    RECE64-SRYKBN        TO   RECE659-SRYKBN
               MOVE    ZERO                 TO   RECE659-JIHIMONEY
               PERFORM  VARYING   IDX   FROM   1   BY   1
                         UNTIL   (IDX       >    5)   OR
                                 (FLG-WRKRECE65   =   1)
                   ADD        1             TO    IDX-RECE65
                   IF (WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE)
                   OR (IDX-RECE65                       >  100 )
                       MOVE   1             TO    FLG-WRKRECE65
                   ELSE
                       MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                        TO   RECE659-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
               END-PERFORM
               IF   RECE659-SRYCD(1)  NOT =  SPACE
                   WRITE   RECE659-REC
                   ADD     1            TO   CNT-RECE659
               END-IF
           END-PERFORM
      *
           PERFORM 500-RECE64-KEY-HENSYU-SEC
           COMPUTE IDX19              =  RECE64-KOHBUNRI + 1
           EVALUATE  RECE64-SRYKBN
             WHEN      "90"
               COMPUTE RECE649-ZAINUM =  99994210  +  IDX19
             WHEN      "92"
               COMPUTE RECE649-ZAINUM =  99994710  +  IDX19
           END-EVALUATE
           MOVE    ZERO                TO  RECE649-ZAIREN
           MOVE    RECE64-KOHBUNRI     TO  RECE649-KOHBUNRI
           MOVE    RECE64-SRYKBN       TO  RECE649-SRYKBN
           MOVE    "6"                 TO  RECE649-ZAISKBKBN
           MOVE    RECE64-PTNUM        TO  RECE649-PTNUM
           MOVE    RECE64-SRYKA        TO  RECE649-SRYKA
           MOVE    RECE64-HKNCOMBI     TO  RECE649-HKNCOMBI
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE649-MEISAISU
      *
           WRITE   RECE649-REC
           ADD     1                   TO  CNT-RECE649
      *
           .
       20012-NYUKASAN-HENSYU4-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０４転送処理
      *****************************************************************
       2099-RECE64-TENSOU-SEC     SECTION.
      *
           MOVE       RECE64-REC    TO   RECE649-REC
           WRITE      RECE649-REC
           ADD        1             TO   CNT-RECE649
           PERFORM    20991-RECE65-TENSOU-SEC
      *
           .
       2099-RECE64-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０５転送処理
      *****************************************************************
       20991-RECE65-TENSOU-SEC     SECTION.
      *
           INITIALIZE                      RECE65-REC
           MOVE    RECE64-KEY1         TO  RECE65-KEY1
           MOVE    RECE64-ZAINUM       TO  RECE65-ZAINUM
           MOVE    RECE64-ZAIREN       TO  RECE65-ZAIREN
           START   RECE65-FILE
               KEY IS    >=   RECE65-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE65
               NOT INVALID
                   PERFORM 990-RECE65-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE65           =   1                   )
               OR     ( RECE64-KEY1      NOT =   RECE65-KEY1         )
               OR     ( RECE64-ZAINUM    NOT =   RECE65-ZAINUM       )
               OR     ( RECE64-ZAIREN    NOT =   RECE65-ZAIREN       )
      *
      *
               MOVE          RECE65-REC       TO      RECE659-REC
               WRITE         RECE659-REC
               ADD     1                 TO  CNT-RECE659
      *
               PERFORM 990-RECE65-NEXT-SEC
      *
           END-PERFORM
      *
           .
       20991-RECE65-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ０５　退避処理
      *****************************************************************
       200111-RECE65-TAIHI-SEC     SECTION.
      *
           INITIALIZE                      RECE65-REC
           MOVE    RECE64-KEY1         TO  RECE65-KEY1
           MOVE    RECE64-ZAINUM       TO  RECE65-ZAINUM
           MOVE    RECE64-ZAIREN       TO  RECE65-ZAIREN
           START   RECE65-FILE
                 KEY IS    >=   RECE65-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE65
                 NOT INVALID
                     PERFORM 990-RECE65-NEXT-SEC
                     MOVE    RECE65-REC  TO  WRK-RECE65-REC
           END-START
      *
           .
       200111-RECE65-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ０５　ＳＲＹＡＣＴＩＮＦ編集処理
      *****************************************************************
       200111-SRYACTINF-HENSYU-SEC     SECTION.
      *
           PERFORM
               UNTIL  ( FLG-RECE65           =   1                   )
               OR     ( RECE64-KEY1      NOT =   RECE65-KEY1         )
               OR     ( RECE64-ZAINUM    NOT =   RECE65-ZAINUM       )
               OR     ( RECE64-ZAIREN    NOT =   RECE65-ZAIREN       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   5     )
                         OR      ( RECE65-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE65
                    MOVE   RECE65-SRYACT-TBL(IDX)
                             TO   WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                    IF   RECE64-ZAITEN   =   ZERO
                       PERFORM   2001111-SRYACTINF-NAME-HEN-SEC
                    END-IF
                    IF  (RECE64-JIHOKBN  NOT =  SPACE)
                    AND (RECE64-ZAIKBN       =   4   )
                       MOVE  ZERO
                             TO   WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                    END-IF
               END-PERFORM
      *
               PERFORM 990-RECE65-NEXT-SEC
      *
           END-PERFORM
      *
           IF     WRK-JIDO-JYOUGEFLG  =  1
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX                >   50    )
                       OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                ADD    1                  TO  IDX-RECE65
                MOVE   "810000001"
                              TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                MOVE   ZERO   TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                              TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                MOVE   WK-FREE-NAME2(IDX)
                              TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
                MOVE   WK-FREE-MEISKYFLG2(IDX)
                          TO  WRK-RECE65XX-MEISKYFLG(IDX-RECE65 1)
                MOVE   WK-FREE-SETCOMATFLG2(IDX)
                          TO  WRK-RECE65XX-SETCOMATFLG(IDX-RECE65 1)
                MOVE   "1"    TO  WRK-RECE65XX-RECEPTFLG(IDX-RECE65 1)
             END-PERFORM
           END-IF
      *
      *    コメントを含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
                    INITIALIZE                        RECE659-REC
                    MOVE    WRK-RECE65-KEY       TO   RECE659-KEY
                    ADD     1                    TO   WRK-RENNUM
                    MOVE    WRK-RENNUM           TO   RECE659-RENNUM
                    MOVE    WRK-RECE65-KOHBUNRI  TO   RECE659-KOHBUNRI
                    MOVE    WRK-RECE65-PTNUM     TO   RECE659-PTNUM
                    MOVE    WRK-RECE65-SRYKA     TO   RECE659-SRYKA
                    MOVE    WRK-RECE65-SRYKBN    TO   RECE659-SRYKBN
                    MOVE    WRK-RECE65-SRYSYUKBN TO   RECE659-SRYSYUKBN
                    MOVE    WRK-RECE65-JIHIMONEY TO   RECE659-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE65   =   1)
                       ADD         1             TO    IDX-RECE65
                       IF (WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE)
                       OR (IDX-RECE65                       >  100 )
                           MOVE    1             TO    FLG-WRKRECE65
                       ELSE
                           MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                            TO   RECE659-SRYACT-TBL(IDX)
                           ADD   1          TO   WRK-CNT-MEISAI
                       END-IF
                    END-PERFORM
                    IF   RECE659-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE659-REC
                       ADD     1            TO   CNT-RECE659
                    END-IF
           END-PERFORM
      *
           MOVE    RECE64-REC        TO      RECE649-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE649-MEISAISU
           IF   WRK-JIDO-SETCOMFLG   =  "1"
             MOVE  WRK-JIDO-SETCOMFLG
                                     TO      RECE649-SETCOMFLG
           END-IF
           IF  (RECE64-JIHOKBN  NOT =  SPACE)
           AND (RECE64-ZAIKBN       =   4   )
             MOVE  ZERO              TO      RECE649-ZAITEN
             MOVE  ZERO              TO      RECE649-SYUTEN1
             MOVE  ZERO              TO      RECE649-SYUTEN2
             MOVE  ZERO              TO      RECE649-YKZTEN
             MOVE  ZERO              TO      RECE649-KIZAITEN
             MOVE  ZERO              TO      RECE649-HOKATU-ZAITEN
             MOVE  ZERO              TO      RECE649-HOKATU-SYUTEN1
             MOVE  ZERO              TO      RECE649-HOKATU-SYUTEN2
             MOVE  ZERO              TO      RECE649-HOKATU-YKZTEN
             MOVE  ZERO              TO      RECE649-HOKATU-KIZAITEN
           END-IF
           WRITE   RECE649-REC
           ADD     1                 TO      CNT-RECE649
      *
           .
       200111-SRYACTINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ０５　ＳＲＹＡＣＴＩＮＦ　名称編集処理
      *****************************************************************
       2001111-SRYACTINF-NAME-HEN-SEC     SECTION.
      *
           IF    RECE62-SRYYM      >=    "201604"
             EVALUATE   WRK-RECE65XX-SRYCD(IDX-RECE65 1)
      *
               WHEN   "190111590"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   8       TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "外泊（選定療養）"
                                TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
             END-EVALUATE
           ELSE
            IF   RECE62-SRYYM      >=    "201004"
             EVALUATE   WRK-RECE65XX-SRYCD(IDX-RECE65 1)
      *
               WHEN   "190111590"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   8       TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "外泊（選定療養）"
                                TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
      *
               WHEN   "999999982"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   28      TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "特定入院料減算"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE  "（他医受診・包括診療行為算定）（選定療養）"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:42)
      *
               WHEN   "999999983"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   33      TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "療養病棟入院基本料等減算"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:24)
                 MOVE  "（他医受診・包括診療行為算定）（選定療養）"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(25:42)
      *
               WHEN   "999999984"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   37      TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "有床診療所療養病床入院基本料減算"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:32)
                 MOVE  "（他医受診・包括診療行為算定）（選定療養）"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(33:42)
      *
               WHEN   "999999992"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   29      TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "特定入院料減算"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE  "（他医受診・包括診療行為未算定）（選定療養）"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:44)
      *
               WHEN   "999999993"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   34      TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "療養病棟入院基本料等減算"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:24)
                 MOVE  "（他医受診・包括診療行為未算定）（選定療養）"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(25:44)
      *
               WHEN   "999999994"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   38      TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "有床診療所療養病床入院基本料減算"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:32)
                 MOVE  "（他医受診・包括診療行為未算定）（選定療養）"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(33:44)
             END-EVALUATE
            ELSE
             EVALUATE   WRK-RECE65XX-SRYCD(IDX-RECE65 1)
      *
               WHEN   "190111590"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   8       TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "外泊（選定療養）"
                                TO  WRK-RECE65XX-NAME(IDX-RECE65 1)
      *
               WHEN   "999999982"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   22      TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "特定入院料減算"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:14)
                 MOVE  "（他医療機関受診）（選定療養）"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(15:30)
      *
               WHEN   "999999983"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   26      TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "療養病棟入院基本料減算"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:22)
                 MOVE  "（他医療機関受診）（選定療養）"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(23:30)
      *
               WHEN   "999999984"
                 MOVE  "810000001" TO  WRK-RECE65XX-SRYCD(IDX-RECE65 1)
                 MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(IDX-RECE65 1)
                 MOVE   31      TO  WRK-RECE65XX-YUKOKETA(IDX-RECE65 1)
                 MOVE  "有床診療所療養病床入院基本料減算"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(1:32)
                 MOVE  "（他医療機関受診）（選定療養）"
                             TO  WRK-RECE65XX-NAME(IDX-RECE65 1)(33:30)
             END-EVALUATE
            END-IF
           END-IF
      *
           .
       2001111-SRYACTINF-NAME-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ０５　ＳＲＹＡＣＴＩＮＦ編集処理　まとめ
      *****************************************************************
       200111-SRYACTINF-HENSYU-MAT-SEC     SECTION.
      *
           PERFORM    200111-RECE65-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           MOVE    RECE64-MEISAISU      TO  MAX-MEISAISU
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                      UNTIL   (IDX                >   50   )
                        OR    (WK-FREE-NAME2(IDX) =   SPACE)
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                             TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
                MOVE   WK-FREE-MEISKYFLG2(IDX)
                          TO  WRK-RECE65XX-MEISKYFLG(MAX-MEISAISU 1)
                MOVE   WK-FREE-SETCOMATFLG2(IDX)
                          TO  WRK-RECE65XX-SETCOMATFLG(MAX-MEISAISU 1)
           END-PERFORM
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       200111-SRYACTINF-HENSYU-MAT-EXT.
           EXIT.
      *
      *****************************************************************
      *    差込編集２
      *****************************************************************
       250-SASHIKOMI2-SEC     SECTION.
      *
      *    パラメータ６４９ファイルより編集
           INITIALIZE                      RECE649-REC
           MOVE    RECE62-KEY          TO  RECE649-KEY1
           MOVE    ZERO                TO  RECE649-ZAINUM
      *
           START   RECE649-FILE
               KEY IS    >=   RECE649-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE649
               NOT INVALID
                   PERFORM 990-RECE649-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE649          =   1                    )
               OR     ( RECE62-KEY       NOT =   RECE649-KEY1         )
      *
               EVALUATE  RECE649-SRYKBN
                 WHEN      "90"
                 WHEN      "92"
                    IF    FLG-RYOCOM  =  ZERO
                       PERFORM       2599-RECE649-TENSOU-SEC
                    ELSE
                       EVALUATE  RECE649-ZAISKBKBN
                         WHEN     "1"
                         WHEN     "2"
                           PERFORM   2502-RYOCOM-HANTEI-SEC
                         WHEN   OTHER
                           PERFORM   2599-RECE649-TENSOU-SEC
                       END-EVALUATE
                    END-IF
                 WHEN      OTHER
                    MOVE   ZERO   TO  FLG-COMMENT
      *             （酸素計算式のコメントのセット）
                    IF   FLG-COMMENT  =  ZERO
                        PERFORM   2501-SANSO-HANTEI-SEC
                    END-IF
      *             （フイルムのコメントのセット）
                    IF   FLG-COMMENT  =  ZERO
                        PERFORM   2503-FILM-HANTEI-SEC
                    END-IF
                    IF   FLG-COMMENT  =  ZERO
                        PERFORM   2599-RECE649-TENSOU-SEC
                    END-IF
               END-EVALUATE
      *
               PERFORM 990-RECE649-NEXT-SEC
      *
           END-PERFORM
      *
           .
       250-SASHIKOMI2-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素があるか判定
      *****************************************************************
       2501-SANSO-HANTEI-SEC          SECTION.
      *
           IF     RECE649-FLG-SANSO-COMMENT   =    "1"
               MOVE       99      TO     FLG-COMMENT
      *
               PERFORM    25011-SANSO-TAIHI-SEC
      *
               PERFORM    25011-SANSO-COMMENT-SEC
      *
           END-IF
      *
           .
       2501-SANSO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素の数量・単価退避
      *****************************************************************
       25011-SANSO-TAIHI-SEC          SECTION.
      *
           MOVE       ZERO    TO     IDX4
           INITIALIZE                TBL-SANSO-INF
      *
           INITIALIZE                       RECE659-REC
           MOVE    RECE649-KEY1         TO  RECE659-KEY1
           MOVE    RECE649-ZAINUM       TO  RECE659-ZAINUM
           MOVE    RECE649-ZAIREN       TO  RECE659-ZAIREN
           START   RECE659-FILE
               KEY IS    >=   RECE659-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE659
               NOT INVALID
                   PERFORM 990-RECE659-NEXT-SEC
           END-START
           PERFORM
               UNTIL ( FLG-RECE659           =  1                    )
               OR    ( RECE649-KEY1      NOT =  RECE659-KEY1         )
               OR    ( RECE649-ZAINUM    NOT =  RECE659-ZAINUM       )
               OR    ( RECE649-ZAIREN    NOT =  RECE659-ZAIREN       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                          UNTIL  ( IDX                >   5     )
                           OR    ( RECE659-SRYCD(IDX) =   SPACE )
      *
                  SET     TBL-SANSO    TO     1
                  SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                       AT  END
                           MOVE    ZERO    TO   FLG-SANSO-OK
                       WHEN   RECE659-SRYCD(IDX) = TBL-SANSO-SRYCD
                                                       (TBL-SANSO)
                           MOVE    1       TO   FLG-SANSO-OK
                  END-SEARCH
                  IF      FLG-SANSO-OK        =    1
                     ADD      1             TO  IDX4
                     MOVE  RECE659-TEN(IDX) TO  TBL-SANSO-TEN(IDX4)
                     MOVE  RECE659-SRYSURYO(IDX)
                                            TO  TBL-SANSO-SRYSURYO
                                                             (IDX4)
                  END-IF
                  IF    RECE659-SRYCD(IDX) = "770030070"
                     IF   TBL-KASAN-SRYSURYO  =  ZERO
                        MOVE   RECE659-SRYSURYO(IDX)
                                     TO  TBL-KASAN-SRYSURYO
                     ELSE
                        COMPUTE   TBL-KASAN-SRYSURYO  =  
                                  TBL-KASAN-SRYSURYO  *  
                                  RECE659-SRYSURYO(IDX)   
                     END-IF
                  END-IF
      *
               END-PERFORM 
      *
               PERFORM 990-RECE659-NEXT-SEC
      *
           END-PERFORM
      *
           .
       25011-SANSO-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素のコメント編集（差込：下）
      *****************************************************************
       25011-SANSO-COMMENT-SEC          SECTION.
      *
           MOVE       ZERO       TO   IDZ                               
           INITIALIZE            WRK-FREE-AREA2                         
           INITIALIZE            WRK-FREE-AREA3
      *
           PERFORM  250111-SANSO-FREE-COMMENT-SEC
      *
           PERFORM  250111-RECE659-TAIHI-SEC
      *
      *    ＲＥＣＥ６５９ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
           MOVE    ZERO                 TO  WRK-JYUNBAN
                                            WRK-GYOSUU
                                            WRK-GYOSUU2
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
      *
                ADD       1      TO     WRK-GYOSUU
      *
           END-PERFORM 
      *
           PERFORM    250111-SRYACTINF2-HENSYU-SEC
      *
           .
       25011-SANSO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素計算式編集
      *****************************************************************
       250111-SANSO-FREE-COMMENT-SEC          SECTION.
      *
           MOVE     1          TO    WRK-KETA
           MOVE    ZERO        TO    WRK-KEISAN4
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                      UNTIL   (IDX                 >    10)
                       OR     (TBL-SANSO-TEN(IDX)  =  ZERO)
      *
             IF   IDX  >  1
                MOVE    "＋"   TO  WK-FREE-NAME3(WRK-KETA:2)
                COMPUTE  WRK-KETA   =  WRK-KETA   +  2
             END-IF
      *
             MOVE    "（"   TO  WK-FREE-NAME3(WRK-KETA:2)
             COMPUTE  WRK-KETA   =  WRK-KETA   +  2
      *単価
             MOVE     TBL-SANSO-TEN(IDX)   TO   WRK-KEISAN
             COMPUTE  WRK-KEISAN2  = (TBL-SANSO-TEN(IDX) -  WRK-KEISAN)
                                   *   1000
      *
             PERFORM  2501111-SANSO-FREE-COM-NUM-SEC
      *
             MOVE    "円×" TO  WK-FREE-NAME3(WRK-KETA:4)
             COMPUTE  WRK-KETA   =  WRK-KETA   +  4
      *数量
             MOVE     TBL-SANSO-SRYSURYO(IDX)  TO  WRK-KEISAN
             COMPUTE  WRK-KEISAN2  
                             = (TBL-SANSO-SRYSURYO(IDX) -  WRK-KEISAN)
                             *   1000
      *
             PERFORM  2501111-SANSO-FREE-COM-NUM-SEC
      *
             IF    TBL-KASAN-SRYSURYO  =  ZERO
                MOVE    "Ｌ×１．３）" TO  WK-FREE-NAME3(WRK-KETA:12)
                COMPUTE  WRK-KETA   =  WRK-KETA   +  12
      *
      ***       COMPUTE  WRK-KEISAN3   =   WRK-KEISAN3  +
      ***         (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3)
                COMPUTE  WRK-KEISAN3   =
                  (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3)
                                                                + 0.5
                COMPUTE  WRK-KEISAN4  = WRK-KEISAN4 + WRK-KEISAN3
             ELSE
                MOVE    "Ｌ×１．３×" TO  WK-FREE-NAME3(WRK-KETA:12)
                COMPUTE  WRK-KETA   =  WRK-KETA   +  12
      *
                MOVE     TBL-KASAN-SRYSURYO  TO  WRK-KEISAN
                COMPUTE  WRK-KEISAN2  
                             = (TBL-KASAN-SRYSURYO -  WRK-KEISAN)
                             *   1000
      *
                PERFORM  2501111-SANSO-FREE-COM-NUM-SEC
      *
                MOVE    "）" TO  WK-FREE-NAME3(WRK-KETA:2)
                COMPUTE  WRK-KETA   =  WRK-KETA   +  2
      *
      ***       COMPUTE  WRK-KEISAN3   =   WRK-KEISAN3  +
      ***         (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3
      ***                             * TBL-KASAN-SRYSURYO)
                COMPUTE  WRK-KEISAN3   =
                  (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3
                                      * TBL-KASAN-SRYSURYO)     + 0.5
                COMPUTE  WRK-KEISAN4  = WRK-KEISAN4 + WRK-KEISAN3
             END-IF
      *
           END-PERFORM
      *
           IF   IDX  =  2
              MOVE    "÷１０＝"  TO  WK-FREE-NAME3(WRK-KETA:8)
              COMPUTE  WRK-KETA   =  WRK-KETA   +  8
      *
      ***     COMPUTE  WRK-KEISAN =   WRK-KEISAN3  +  0.5
      ***     COMPUTE  WRK-KEISAN =  (WRK-KEISAN   /  10)  +  0.5
              COMPUTE  WRK-KEISAN =  (WRK-KEISAN4  /  10)  +  0.5
              MOVE     ZERO       TO   WRK-KEISAN2
      *
              PERFORM  2501111-SANSO-FREE-COM-NUM-SEC
      *
              MOVE    "点"   TO  WK-FREE-NAME3(WRK-KETA:2)
              COMPUTE  WRK-KETA   =  WRK-KETA   +  2
           ELSE
              IF   IDX  >  2
                  MOVE   WK-FREE-NAME3  TO  WK-FREE-NAME3-TAIHI
                  MOVE    "（"    TO   WK-FREE-NAME3(1:2)
                  MOVE   WK-FREE-NAME3-TAIHI
                                  TO   WK-FREE-NAME3(3:)
                  COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                  MOVE    "）÷１０＝"  TO  WK-FREE-NAME3(WRK-KETA:10)
                  COMPUTE  WRK-KETA   =  WRK-KETA   +  10
      *
      ***         COMPUTE  WRK-KEISAN =   WRK-KEISAN3  +  0.5
      ***         COMPUTE  WRK-KEISAN =  (WRK-KEISAN   /  10)  +  0.5
                  COMPUTE  WRK-KEISAN =  (WRK-KEISAN4  /  10)  +  0.5
                  MOVE     ZERO       TO   WRK-KEISAN2
      *
                  PERFORM  2501111-SANSO-FREE-COM-NUM-SEC
      *
                  MOVE    "点"   TO  WK-FREE-NAME3(WRK-KETA:2)
                  COMPUTE  WRK-KETA   =  WRK-KETA   +  2
              END-IF
           END-IF
      *
           IF     WRK-KETA    >   1
               PERFORM   VARYING   IDX4    FROM   1   BY   50
                          UNTIL   (IDX4     >    1000)
                            OR    (WK-FREE-NAME3(IDX4:1) = SPACE)
                   ADD     1      TO    IDZ
                   MOVE    ZERO   TO    WRK-KETA2
                   PERFORM   VARYING   IDX5    FROM   1   BY   2
                              UNTIL   (IDX5     >    50)
                      IF   WK-FREE-NAME3(IDX5:2)    = SPACE
                          MOVE   IDX5   TO   WRK-KETA2
                          MOVE   50     TO   IDX5
                      END-IF
                   END-PERFORM
                   IF   WRK-KETA2 =  ZERO
                        MOVE   25   TO  WK-FREE-YUKOKETA2(IDZ)
                   ELSE
                        COMPUTE   WK-FREE-YUKOKETA2(IDZ)  =
                                       ( WRK-KETA2 - 1 )  /  2
                   END-IF
                   MOVE   WK-FREE-NAME3(IDX4:50)
                                   TO    WK-FREE-NAME2(IDZ)
               END-PERFORM
           END-IF
      *
           .
       250111-SANSO-FREE-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素計算式・数値編集
      *****************************************************************
       2501111-SANSO-FREE-COM-NUM-SEC          SECTION.          
      *
           IF    WRK-KEISAN  NOT  =  ZERO
               PERFORM   VARYING   IDX4    FROM   1   BY   1
                          UNTIL   (IDX4               >    9)
                     IF   WRK-KEISAN(IDX4:1) =   "0"
                        CONTINUE
                     ELSE
                        MOVE  IDX4   TO  IDX5
                        MOVE   9     TO  IDX4
                     END-IF
               END-PERFORM
               MOVE  SPACE             TO  WRK-KEISAN-TAIHI
               MOVE  WRK-KEISAN(IDX5:) TO  WRK-KEISAN-TAIHI
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI
               MOVE  WRK-KEISAN-TAIHI  TO  KANACHK-MAE-INPUT
               CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                               TO  WK-FREE-NAME3(WRK-KETA:KANACHK-MAX)
                   COMPUTE  WRK-KETA   =  WRK-KETA   +  KANACHK-MAX
               END-IF
           END-IF
      *
           IF    WRK-KEISAN2  NOT  =  ZERO
                PERFORM   VARYING   IDX4    FROM   3   BY  -1
                           UNTIL   (IDX4               <    1)
                      IF   WRK-KEISAN2(IDX4:1) =   "0"
                         CONTINUE
                      ELSE
                         MOVE  IDX4   TO  IDX5
                         MOVE   1     TO  IDX4
                      END-IF
                END-PERFORM
                MOVE  SPACE               TO  WRK-KEISAN-TAIHI
                MOVE  WRK-KEISAN2(1:IDX5) TO  WRK-KEISAN-TAIHI
      *
                IF    WRK-KEISAN  NOT  =  ZERO
                   MOVE    "．"     TO  WK-FREE-NAME3(WRK-KETA:2)
                   COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                ELSE
                   MOVE    "０．"   TO  WK-FREE-NAME3(WRK-KETA:4)
                   COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                END-IF
      *
                INITIALIZE                  ORCSKANACHKAREA
                MOVE    "2"             TO  KANACHK-SYORI
                MOVE  WRK-KEISAN-TAIHI  TO  KANACHK-MAE-INPUT
                CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                IF      KANACHK-RC      =   ZERO
                    MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                TO  WK-FREE-NAME3(WRK-KETA:KANACHK-MAX)
                    COMPUTE  WRK-KETA   =  WRK-KETA   +  KANACHK-MAX
                END-IF
           END-IF
      *
           IF    WRK-KEISAN    =  ZERO
           AND   WRK-KEISAN2   =  ZERO
               MOVE    "０"   TO  WK-FREE-NAME3(WRK-KETA:2)
               COMPUTE  WRK-KETA   =  WRK-KETA   +  2
           END-IF
      *
           .
       2501111-SANSO-FREE-COM-NUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ６５９　退避処理
      *****************************************************************
       250111-RECE659-TAIHI-SEC     SECTION.
      *
           INITIALIZE                       RECE659-REC
           MOVE    RECE649-KEY1         TO  RECE659-KEY1
           MOVE    RECE649-ZAINUM       TO  RECE659-ZAINUM
           MOVE    RECE649-ZAIREN       TO  RECE659-ZAIREN
           START   RECE659-FILE
                 KEY IS    >=   RECE659-KEY
                 INVALID
                     MOVE    1          TO  FLG-RECE659
                 NOT INVALID
                     PERFORM 990-RECE659-NEXT-SEC
                     MOVE   RECE659-REC TO  WRK-RECE65-REC
           END-START
      *
           .
       250111-RECE659-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ６５９　ＳＲＹＡＣＴＩＮＦ編集処理
      *****************************************************************
       250111-SRYACTINF2-HENSYU-SEC     SECTION.
      *
           PERFORM
               UNTIL  ( FLG-RECE659           =   1                    )
               OR     ( RECE649-KEY1      NOT =   RECE659-KEY1         )
               OR     ( RECE649-ZAINUM    NOT =   RECE659-ZAINUM       )
               OR     ( RECE649-ZAIREN    NOT =   RECE659-ZAIREN       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   5     )
                         OR      ( RECE659-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE65
                    MOVE   RECE659-SRYACT-TBL(IDX)
                             TO   WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
      *
                    IF   RECE659-SRYCD(IDX) = "770020070" OR "770030070"
                         MOVE   IDX-RECE65   TO   WRK-JYUNBAN
                    END-IF
      *
               END-PERFORM
      *
               PERFORM 990-RECE659-NEXT-SEC
      *
           END-PERFORM
      *
           IF     WRK-GYOSUU   NOT =  ZERO
      *
      *      補正率又は高気圧酸素加算以降の明細をずらす
             IF   WRK-JYUNBAN  NOT =  ZERO
      *
               PERFORM   VARYING   IDX   FROM   IDX-RECE65   BY  -1
                          UNTIL  ( IDX    <    1           )
                           OR    ( IDX    <=   WRK-JYUNBAN )
      *
                   COMPUTE   WRK-GYOSUU2  =  IDX  +  WRK-GYOSUU
                   MOVE   WRK-RECE65XX-SRYACT-TBL(IDX 1)  TO
                          WRK-RECE65XX-SRYACT-TBL(WRK-GYOSUU2 1)
                   INITIALIZE   WRK-RECE65XX-SRYACT-TBL(IDX 1)
      *
               END-PERFORM
      *
             END-IF
      *
      *      計算式の差し込み
             IF   WRK-JYUNBAN  NOT =  ZERO
                 MOVE      WRK-JYUNBAN    TO     MAX-MEISAISU
             ELSE
                 MOVE      IDX-RECE65     TO     MAX-MEISAISU
             END-IF
             PERFORM   VARYING   IDX   FROM    1   BY  1
                        UNTIL  ( IDX                >   50    )
                         OR    ( WK-FREE-NAME2(IDX) =   SPACE )
      *
                ADD    1     TO  MAX-MEISAISU
                MOVE   "810000001"
                             TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
                MOVE   "1"   TO  WRK-RECE65XX-RECEDENFLG(MAX-MEISAISU 1)
      *
             END-PERFORM
      *
           END-IF
      *
      *    コメントを含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
                    INITIALIZE                        RECE651-REC
                    MOVE    WRK-RECE65-KEY       TO   RECE651-KEY
                    ADD     1                    TO   WRK-RENNUM
                    MOVE    WRK-RENNUM           TO   RECE651-RENNUM
                    MOVE    WRK-RECE65-KOHBUNRI  TO   RECE651-KOHBUNRI
                    MOVE    WRK-RECE65-PTNUM     TO   RECE651-PTNUM
                    MOVE    WRK-RECE65-SRYKA     TO   RECE651-SRYKA
                    MOVE    WRK-RECE65-SRYKBN    TO   RECE651-SRYKBN
                    MOVE    WRK-RECE65-SRYSYUKBN TO   RECE651-SRYSYUKBN
                    MOVE    WRK-RECE65-JIHIMONEY TO   RECE651-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE65   =   1)
                       ADD         1             TO    IDX-RECE65
                       IF (WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE)
                       OR (IDX-RECE65                       >  100 )
                           MOVE    1             TO    FLG-WRKRECE65
                       ELSE
                           MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                            TO   RECE651-SRYACT-TBL(IDX)
                           ADD   1          TO   WRK-CNT-MEISAI
                       END-IF
                    END-PERFORM
                    IF   RECE651-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE651-REC
                       ADD     1            TO   CNT-RECE65
                    END-IF
           END-PERFORM
      *
           MOVE    RECE649-REC       TO      RECE641-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE641-MEISAISU
           WRITE   RECE641-REC
           ADD     1                 TO      CNT-RECE64
      *
           .
       250111-SRYACTINF2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    療養病棟コメントがあるか判定
      *****************************************************************
       2502-RYOCOM-HANTEI-SEC          SECTION.
      *
           MOVE     ZERO     TO     FLG-RYOCOM2
           INITIALIZE               TBL-RYOCOM2-INF
      *
           PERFORM  VARYING  RYO-IDX  FROM  1  BY  1
                     UNTIL   RYO-IDX   >   31
      *
               IF    (RECE649-KAISU(1 RYO-IDX)   NOT =  ZERO)
               AND   (TBL-RYOCOM-NYUINKBN(RYO-IDX)   =  "1" )
      *
                  MOVE     ZERO     TO    FLG-ARI
                  PERFORM  VARYING  RYO-IDX3  FROM  1  BY  1
                            UNTIL  (RYO-IDX3   >   31)
                             OR    (TBL-RYOCOM2-MEISAISU(RYO-IDX3)
                                               = ZERO)
                             OR    (FLG-ARI    =    1)
                   IF   RECE62-SRYYM  >=  "201004"
                     IF  (TBL-RYOCOM2-MEISAISU  (RYO-IDX3) =
                                    TBL-RYOCOM-MEISAISU  (RYO-IDX))
                     AND (TBL-RYOCOM2-SRYCDTOTAL(RYO-IDX3) =
                                    TBL-RYOCOM-SRYCDTOTAL(RYO-IDX))
                     AND (TBL-RYOCOM2-ADL-TEN-H22(RYO-IDX3 1) =
                                 TBL-RYOCOM-ADL-TEN-H22(RYO-IDX 1))
                     AND (TBL-RYOCOM2-ADL-TEN-H22(RYO-IDX3 2) =
                                 TBL-RYOCOM-ADL-TEN-H22(RYO-IDX 2))
                     AND (TBL-RYOCOM2-ADL-TEN-H22(RYO-IDX3 3) =
                                 TBL-RYOCOM-ADL-TEN-H22(RYO-IDX 3))
                     AND (TBL-RYOCOM2-ADL-TEN-H22(RYO-IDX3 4) =
                                 TBL-RYOCOM-ADL-TEN-H22(RYO-IDX 4))
                        MOVE   1   TO  TBL-RYOCOM2-DAY(RYO-IDX3 RYO-IDX)
                        MOVE   1   TO  FLG-ARI
                     END-IF
                   ELSE
                     IF  (TBL-RYOCOM2-MEISAISU  (RYO-IDX3) =
                                    TBL-RYOCOM-MEISAISU  (RYO-IDX))
                     AND (TBL-RYOCOM2-SRYCDTOTAL(RYO-IDX3) =
                                    TBL-RYOCOM-SRYCDTOTAL(RYO-IDX))
                     AND (TBL-RYOCOM2-ADL-TEN   (RYO-IDX3) =
                                    TBL-RYOCOM-ADL-TEN   (RYO-IDX))
                        MOVE   1   TO  TBL-RYOCOM2-DAY(RYO-IDX3 RYO-IDX)
                        MOVE   1   TO  FLG-ARI
                     END-IF
                   END-IF
                  END-PERFORM
                  IF  FLG-ARI  =  ZERO
                     MOVE  TBL-RYOCOM-AREA(RYO-IDX)
                               TO  TBL-RYOCOM2-AREA(RYO-IDX3)
                     MOVE  1   TO  TBL-RYOCOM2-DAY (RYO-IDX3 RYO-IDX)
                  END-IF
      *
                  MOVE      1      TO     FLG-RYOCOM2
      *
               END-IF
      *
           END-PERFORM
      *
           IF  FLG-RYOCOM2   =   ZERO
              PERFORM   2599-RECE649-TENSOU-SEC
           ELSE
            IF   RECE62-SRYYM  >=  "201004"
              PERFORM   25021-RYOCOM-HENSYU-H22-SEC
            ELSE
              PERFORM   25021-RYOCOM-HENSYU-SEC
            END-IF
           END-IF
      *
           .
       2502-RYOCOM-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    療養病棟コメント編集（平成２２年４月診療〜）
      *****************************************************************
       25021-RYOCOM-HENSYU-H22-SEC          SECTION.
      *
           PERFORM  250111-RECE659-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX2-REC
           INITIALIZE                       WRK-RECE65XX2-REC
      *
           MOVE    RECE649-MEISAISU     TO  MAX-MEISAISU
      *
      *
           PERFORM  VARYING  RYO-IDX3  FROM  1  BY  1
                     UNTIL  (RYO-IDX3   >   31                    )
                      OR    (TBL-RYOCOM2-MEISAISU(RYO-IDX3) = ZERO)
      *
              INITIALIZE                TBL-NYU-DAY
      *
              MOVE    ZERO          TO  DAY-IDX
              MOVE    ZERO          TO  FLG-NYU-RENZOKU
      *
              PERFORM    VARYING    IDX  FROM   1   BY   1
                          UNTIL    (IDX      >   31)  OR
                                   (DAY-IDX  >   31)
                IF    TBL-RYOCOM2-DAY(RYO-IDX3 IDX)   =  ZERO
                      MOVE  ZERO  TO  FLG-NYU-RENZOKU
                ELSE
                      IF    FLG-NYU-RENZOKU  =  ZERO
                         ADD   1     TO  DAY-IDX
                         MOVE  IDX   TO  TBL-NYU-DAY-ST(DAY-IDX)
                      END-IF
                      MOVE  IDX      TO  TBL-NYU-DAY-ED(DAY-IDX)
                      MOVE  1        TO  FLG-NYU-RENZOKU
                END-IF
              END-PERFORM
      *
      ***     PERFORM  VARYING  DAY-IDX  FROM   1   BY   1
      ***               UNTIL  (DAY-IDX                  >   31)
      ***                OR    (TBL-NYU-DAY-ST(DAY-IDX)  = ZERO)
      ***       IF    TBL-NYU-DAY-ST(DAY-IDX) 
      ***                           =   TBL-NYU-DAY-ED(DAY-IDX)
      ***             MOVE  "840000059"
      ***                           TO  TBL-NYU-DAY-CD(DAY-IDX)
      ***       ELSE
      ***             MOVE  "810000001"
      ***                           TO  TBL-NYU-DAY-CD(DAY-IDX)
      ***       END-IF
      ***     END-PERFORM
      *
      *       算定日コメントの挿入
              PERFORM  VARYING   IDX    FROM  1   BY   1
                        UNTIL   (IDX                  >    31)
                         OR     (TBL-NYU-DAY-ST(IDX)  =  ZERO)
               COMPUTE  MAX-MEISAISU   =   MAX-MEISAISU  +  1
               IF  MAX-MEISAISU <= 300
                 MOVE  "810000001"
                             TO  WRK-RECE65XX2-SRYCD(MAX-MEISAISU 1)
                 MOVE   ZERO TO  WRK-RECE65XX2-SRYSURYO(MAX-MEISAISU 1)
      ***        IF    TBL-NYU-DAY-CD(IDX)  =  "840000059"
      ***          MOVE   16 TO  WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
      ***          MOVE   "療養病棟入院期間　　　日〜　　日"
      ***                  TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
      ***          MOVE   TBL-NYU-DAY-ST(IDX) TO  DAY-IDX2
      ***          MOVE   TBL-DAY(DAY-IDX2)
      ***                  TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(19:4)
      ***          MOVE   TBL-NYU-DAY-ED(IDX) TO  DAY-IDX2
      ***          MOVE   TBL-DAY(DAY-IDX2)
      ***                  TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(27:4)
      ***        ELSE
                   MOVE   16 TO  WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
                   EVALUATE RECE62-HOSPSBT
                     WHEN    "1"
                       MOVE   "療養病棟入院期間　　　日〜　　日"
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
                     WHEN    "2"
                       MOVE   "療養病床入院期間　　　日〜　　日"
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
                   END-EVALUATE
                   MOVE   TBL-NYU-DAY-ST(IDX) TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(19:4)
                   MOVE   TBL-NYU-DAY-ED(IDX) TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(27:4)
      ***        END-IF
                 MOVE  "1" TO  WRK-RECE65XX2-RECEDENFLG(MAX-MEISAISU 1)
               END-IF
              END-PERFORM
      *
              PERFORM  VARYING  IDX  FROM   1   BY   1
                  UNTIL  (IDX                                  >  30  )
                   OR    (TBL-RYOCOM2-IRYO-SRYCD(RYO-IDX3 IDX) = SPACE)
      *
               COMPUTE  MAX-MEISAISU  =  MAX-MEISAISU  +  1
               IF  MAX-MEISAISU <= 300
                   MOVE "810000001"
                              TO WRK-RECE65XX2-SRYCD(MAX-MEISAISU 1)
                   MOVE  ZERO TO WRK-RECE65XX2-SRYSURYO(MAX-MEISAISU 1)
                   MOVE  4    TO WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
                   PERFORM       250212-RYOCOM-NAME-HENSYU-SEC
                   MOVE "1" TO WRK-RECE65XX2-RECEDENFLG(MAX-MEISAISU 1)
               END-IF
      *
              END-PERFORM
      *
              PERFORM  VARYING  IDX  FROM   1   BY   1
                UNTIL (IDX   >   4)
      *
                IF  TBL-RYOCOM2-ADL-SRYCD-H22(RYO-IDX3 IDX) NOT = SPACE
                  COMPUTE  MAX-MEISAISU  =  MAX-MEISAISU  +  1
                  IF  MAX-MEISAISU <= 300
                    MOVE   "810000001"
                             TO WRK-RECE65XX2-SRYCD(MAX-MEISAISU 1)
                    MOVE   ZERO
                             TO WRK-RECE65XX2-SRYSURYO(MAX-MEISAISU 1)
                    EVALUATE  IDX
                      WHEN     1
                        MOVE 17
                           TO WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
                        MOVE "ＡＤＬａ　ベッド上の可動性　　　点"
                           TO WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
                        MOVE TBL-RYOCOM2-ADL-TEN-MOJI-H22(RYO-IDX3 IDX)
                           TO WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(29:4)
                      WHEN     2
                        MOVE 11
                           TO WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
                        MOVE "ＡＤＬｂ　移乗　　　点"
                           TO WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
                        MOVE TBL-RYOCOM2-ADL-TEN-MOJI-H22(RYO-IDX3 IDX)
                           TO WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(17:4)
                      WHEN     3
                        MOVE 11
                           TO WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
                        MOVE "ＡＤＬｃ　食事　　　点"
                           TO WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
                        MOVE TBL-RYOCOM2-ADL-TEN-MOJI-H22(RYO-IDX3 IDX)
                           TO WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(17:4)
                      WHEN     4
                        MOVE 15
                           TO WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
                        MOVE "ＡＤＬｄ　トイレの使用　　　点"
                           TO WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
                        MOVE TBL-RYOCOM2-ADL-TEN-MOJI-H22(RYO-IDX3 IDX)
                           TO WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(25:4)
                    END-EVALUATE
                    MOVE "1" TO WRK-RECE65XX2-RECEDENFLG(MAX-MEISAISU 1)
                  END-IF
                END-IF
      *
              END-PERFORM
      *
           END-PERFORM
      *
      *
           PERFORM    250211-SRYACTINF3-HENSYU-SEC
      *
           .
       25021-RYOCOM-HENSYU-H22-EXT.
           EXIT.
      *
      *****************************************************************
      *    療養病棟コメント編集
      *****************************************************************
       25021-RYOCOM-HENSYU-SEC          SECTION.
      *
           PERFORM  250111-RECE659-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX2-REC
           INITIALIZE                       WRK-RECE65XX2-REC
      *
           MOVE    RECE649-MEISAISU     TO  MAX-MEISAISU
      *
      *
           PERFORM  VARYING  RYO-IDX3  FROM  1  BY  1
                     UNTIL  (RYO-IDX3   >   31                    )
                      OR    (TBL-RYOCOM2-MEISAISU(RYO-IDX3) = ZERO)
      *
              INITIALIZE                TBL-NYU-DAY
      *
              MOVE    ZERO          TO  DAY-IDX
              MOVE    ZERO          TO  FLG-NYU-RENZOKU
      *
              PERFORM    VARYING    IDX  FROM   1   BY   1
                          UNTIL    (IDX      >   31)  OR
                                   (DAY-IDX  >   31)
                IF    TBL-RYOCOM2-DAY(RYO-IDX3 IDX)   =  ZERO
                      MOVE  ZERO  TO  FLG-NYU-RENZOKU
                ELSE
                      IF    FLG-NYU-RENZOKU  =  ZERO
                         ADD   1     TO  DAY-IDX
                         MOVE  IDX   TO  TBL-NYU-DAY-ST(DAY-IDX)
                      END-IF
                      MOVE  IDX      TO  TBL-NYU-DAY-ED(DAY-IDX)
                      MOVE  1        TO  FLG-NYU-RENZOKU
                END-IF
              END-PERFORM
      *
              PERFORM  VARYING  DAY-IDX  FROM   1   BY   1
                        UNTIL  (DAY-IDX                  >   31)
                         OR    (TBL-NYU-DAY-ST(DAY-IDX)  = ZERO)
                IF    TBL-NYU-DAY-ST(DAY-IDX) 
                                    =   TBL-NYU-DAY-ED(DAY-IDX)
                      MOVE  "840000059"
                                    TO  TBL-NYU-DAY-CD(DAY-IDX)
                ELSE
                      MOVE  "810000001"
                                    TO  TBL-NYU-DAY-CD(DAY-IDX)
                END-IF
              END-PERFORM
      *
      *       算定日コメントの挿入
              PERFORM  VARYING   IDX    FROM  1   BY   1
                        UNTIL   (IDX                  >    31)
                         OR     (TBL-NYU-DAY-ST(IDX)  =  ZERO)
               COMPUTE  MAX-MEISAISU   =   MAX-MEISAISU  +  1
               IF  MAX-MEISAISU <= 300
                 MOVE  "810000001"
                             TO  WRK-RECE65XX2-SRYCD(MAX-MEISAISU 1)
                 MOVE   ZERO TO  WRK-RECE65XX2-SRYSURYO(MAX-MEISAISU 1)
                 IF    TBL-NYU-DAY-CD(IDX)  =  "840000059"
                   MOVE   6  TO  WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
                   MOVE   "　　月　　日"
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
                   MOVE   RECE62-SRYYM(5:2)   TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(1:4)
                   MOVE   TBL-NYU-DAY-ST(IDX) TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(7:4)
                 ELSE
                   MOVE   13 TO  WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
                   MOVE   "　　月　　日〜　　月　　日"
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
                   MOVE   RECE62-SRYYM(5:2)   TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(1:4)
                               WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(15:4)
                   MOVE   TBL-NYU-DAY-ST(IDX) TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(7:4)
                   MOVE   TBL-NYU-DAY-ED(IDX) TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                           TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(21:4)
                 END-IF
                 MOVE  "1" TO  WRK-RECE65XX2-RECEDENFLG(MAX-MEISAISU 1)
               END-IF
              END-PERFORM
      *
              PERFORM  VARYING  IDX  FROM   1   BY   1
                  UNTIL  (IDX                                  >  30  )
                   OR    (TBL-RYOCOM2-IRYO-SRYCD(RYO-IDX3 IDX) = SPACE)
      *
               COMPUTE  MAX-MEISAISU  =  MAX-MEISAISU  +  1
               IF  MAX-MEISAISU <= 300
                   MOVE "810000001"
                              TO WRK-RECE65XX2-SRYCD(MAX-MEISAISU 1)
                   MOVE  ZERO TO WRK-RECE65XX2-SRYSURYO(MAX-MEISAISU 1)
                   MOVE  4    TO WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
                   PERFORM       250212-RYOCOM-NAME-HENSYU-SEC
                   MOVE "1" TO WRK-RECE65XX2-RECEDENFLG(MAX-MEISAISU 1)
               END-IF
      *
              END-PERFORM
      *
              IF   TBL-RYOCOM2-ADL-SRYCD(RYO-IDX3)  NOT =  SPACE
               COMPUTE  MAX-MEISAISU  =  MAX-MEISAISU  +  1
               IF  MAX-MEISAISU <= 300
                MOVE   "810000001"
                             TO WRK-RECE65XX2-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO WRK-RECE65XX2-SRYSURYO(MAX-MEISAISU 1)
                MOVE   7     TO WRK-RECE65XX2-YUKOKETA(MAX-MEISAISU 1)
                MOVE   "ＡＤＬ　　　点"
                             TO WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
                MOVE   TBL-RYOCOM2-ADL-TEN-MOJI(RYO-IDX3)
                             TO WRK-RECE65XX2-NAME(MAX-MEISAISU 1)(9:4)
                MOVE "1" TO WRK-RECE65XX2-RECEDENFLG(MAX-MEISAISU 1)
               END-IF
              END-IF
      *
           END-PERFORM
      *
      *
           PERFORM    250211-SRYACTINF3-HENSYU-SEC
      *
           .
       25021-RYOCOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ６５９　ＳＲＹＡＣＴＩＮＦ編集処理
      *****************************************************************
       250211-SRYACTINF3-HENSYU-SEC     SECTION.
      *
           PERFORM
               UNTIL  ( FLG-RECE659           =   1                    )
               OR     ( RECE649-KEY1      NOT =   RECE659-KEY1         )
               OR     ( RECE649-ZAINUM    NOT =   RECE659-ZAINUM       )
               OR     ( RECE649-ZAIREN    NOT =   RECE659-ZAIREN       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                          UNTIL  ( IDX                >   5     )
                           OR    ( RECE659-SRYCD(IDX) =   SPACE )
                    ADD    1    TO    IDX-RECE65
                    MOVE   RECE659-SRYACT-TBL(IDX)
                           TO   WRK-RECE65XX2-SRYACT-TBL(IDX-RECE65 1)
               END-PERFORM
      *
               PERFORM 990-RECE659-NEXT-SEC
      *
           END-PERFORM
      *
      *    コメントを含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
               INITIALIZE                        RECE651-REC
               MOVE    WRK-RECE65-KEY       TO   RECE651-KEY
               ADD     1                    TO   WRK-RENNUM
               MOVE    WRK-RENNUM           TO   RECE651-RENNUM
               MOVE    WRK-RECE65-KOHBUNRI  TO   RECE651-KOHBUNRI
               MOVE    WRK-RECE65-PTNUM     TO   RECE651-PTNUM
               MOVE    WRK-RECE65-SRYKA     TO   RECE651-SRYKA
               MOVE    WRK-RECE65-SRYKBN    TO   RECE651-SRYKBN
               MOVE    WRK-RECE65-SRYSYUKBN TO   RECE651-SRYSYUKBN
               MOVE    WRK-RECE65-JIHIMONEY TO   RECE651-JIHIMONEY
               PERFORM  VARYING   IDX   FROM   1   BY   1
                         UNTIL   (IDX             >   5)
                          OR     (FLG-WRKRECE65   =   1)
                   ADD        1             TO   IDX-RECE65
                   IF (WRK-RECE65XX2-SRYCD(IDX-RECE65 1) = SPACE)
                   OR (IDX-RECE65                        >  300 )
                      MOVE    1             TO    FLG-WRKRECE65
                   ELSE
                      MOVE  WRK-RECE65XX2-SRYACT-TBL(IDX-RECE65 1)
                                       TO   RECE651-SRYACT-TBL(IDX)
                      ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                   IF   IDX-RECE65   >= 300
                      MOVE    1        TO   FLG-WRKRECE65
                   END-IF
               END-PERFORM
               IF  RECE651-SRYCD(1)  NOT =  SPACE
                  WRITE   RECE651-REC
                  ADD     1            TO   CNT-RECE65
               END-IF
           END-PERFORM
      *
           MOVE    RECE649-REC       TO      RECE641-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE641-MEISAISU
      *
           MOVE    RECE649-MEISAISU  TO      RECE641-RYOTENKISAISU
      *
           WRITE   RECE641-REC
           ADD     1                 TO      CNT-RECE64
      *
           .
       250211-SRYACTINF3-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    療養病棟コメント名称編集
      *****************************************************************
       250212-RYOCOM-NAME-HENSYU-SEC     SECTION.
      *
           EVALUATE  TBL-RYOCOM2-IRYO-SRYCD(RYO-IDX3 IDX)
             WHEN      "820000057"
             WHEN      "820000096"
                MOVE "「０１」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000058"
             WHEN      "820000097"
                MOVE "「０２」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000059"
             WHEN      "820000098"
                MOVE "「０３」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000060"
             WHEN      "820000099"
                MOVE "「０４」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000061"
             WHEN      "820000100"
                MOVE "「０５」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000062"
             WHEN      "820000101"
                MOVE "「０６」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000063"
             WHEN      "820000102"
                MOVE "「０７」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000064"
             WHEN      "820000103"
                MOVE "「０８」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000065"
             WHEN      "820000104"
                MOVE "「０９」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000066"
             WHEN      "820000105"
                MOVE "「１０」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000067"
             WHEN      "820000106"
                MOVE "「１１」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000068"
             WHEN      "820000107"
                MOVE "「１２」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000069"
             WHEN      "820000108"
                MOVE "「１３」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000070"
             WHEN      "820000109"
                MOVE "「１４」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000071"
             WHEN      "820000110"
                MOVE "「１５」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000072"
             WHEN      "820000111"
                MOVE "「１６」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000073"
             WHEN      "820000112"
             WHEN      "820000229"
                MOVE "「１７」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000074"
             WHEN      "820000113"
                MOVE "「１８」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000075"
             WHEN      "820000114"
             WHEN      "820000230"
                MOVE "「１９」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000076"
             WHEN      "820000115"
                MOVE "「２０」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000077"
             WHEN      "820000116"
                MOVE "「２１」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000078"
             WHEN      "820000117"
                MOVE "「２２」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000079"
             WHEN      "820000118"
             WHEN      "820000231"
                MOVE "「２３」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000080"
             WHEN      "820000119"
                MOVE "「２４」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000081"
             WHEN      "820000120"
                MOVE "「２５」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000082"
             WHEN      "820000121"
                MOVE "「２６」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000083"
             WHEN      "820000122"
                MOVE "「２７」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000084"
             WHEN      "820000123"
                MOVE "「２８」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000085"
             WHEN      "820000124"
                MOVE "「２９」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000086"
             WHEN      "820000125"
                MOVE "「３０」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000087"
             WHEN      "820000126"
                MOVE "「３１」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000088"
             WHEN      "820000127"
                MOVE "「３２」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000089"
             WHEN      "820000128"
                MOVE "「３３」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000090"
             WHEN      "820000129"
                MOVE "「３４」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000091"
             WHEN      "820000130"
                MOVE "「３５」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000092"
             WHEN      "820000131"
                MOVE "「３６」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000093"
             WHEN      "820000132"
                MOVE "「３７」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000232"
                MOVE "「３８」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000158"
             WHEN      "820000160"
                MOVE "「９１」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
             WHEN      "820000159"
                MOVE "「９２」"
                       TO  WRK-RECE65XX2-NAME(MAX-MEISAISU 1)
           END-EVALUATE
      *
           .
       250212-RYOCOM-NAME-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    フイルムがあるか判定
      *****************************************************************
       2503-FILM-HANTEI-SEC          SECTION.
      *
           IF     RECE649-FLG-FILM-COMMENT   =    "1"
               MOVE       99      TO     FLG-COMMENT
               PERFORM    25031-FILM-COMMENT-SEC
           END-IF
      *
           .
       2503-FILM-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    フイルムのコメント編集（差込：下）
      *****************************************************************
       25031-FILM-COMMENT-SEC          SECTION.
      *
           INITIALIZE                WRK-JIDO-SETCOM-AREA
           MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
           MOVE       20      TO     WRK-JIDO-SETCOMATFLG
      *
           PERFORM  250111-RECE659-TAIHI-SEC
      *
      *    ＲＥＣＥ６５９ファイルの内容をテーブルに退避
           MOVE    ZERO       TO     IDX-RECE65
           MOVE    SPACE      TO     WRK-RECE65XX-REC
           INITIALIZE                WRK-RECE65XX-REC
           MOVE    ZERO       TO     WRK-JYUNBAN
      *
           PERFORM  250311-SRYACTINF2-HENSYU-SEC
      *
           .
       25031-FILM-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ６５９　ＳＲＹＡＣＴＩＮＦ編集処理
      *****************************************************************
       250311-SRYACTINF2-HENSYU-SEC     SECTION.
      *
           PERFORM
               UNTIL  ( FLG-RECE659           =   1                    )
               OR     ( RECE649-KEY1      NOT =   RECE659-KEY1         )
               OR     ( RECE649-ZAINUM    NOT =   RECE659-ZAINUM       )
               OR     ( RECE649-ZAIREN    NOT =   RECE659-ZAIREN       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   5     )
                         OR      ( RECE659-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE65
                    MOVE   RECE659-SRYACT-TBL(IDX)
                             TO   WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
      *
                    IF   RECE659-SRYCD(IDX) =
                         "700010000" OR "700030000" OR "700050000" OR
                         "700080000" OR "700110000" OR "700140000" OR
                         "700290000" OR "700310000" OR "700330000" OR
                         "700710000" OR "700730000" OR "700750000" OR
                         "700780000" OR "700810000" OR "700840000" OR
                         "700870000" OR "700160000" OR "700180000" OR
                         "700190000" OR "700200000" OR "700210000" OR
                         "700220000" OR "700230000" OR "700240000" OR
                         "700250000" OR "700260000" OR "700270000" OR
                         "700280000" OR "700590000" OR "700600000" OR
                         "700630000" OR "700640000" OR "700650000" OR
                         "700680000" OR "729780000"
                         MOVE   IDX-RECE65   TO   WRK-JYUNBAN
                    END-IF
      *
               END-PERFORM
      *
               PERFORM 990-RECE659-NEXT-SEC
      *
           END-PERFORM
      *
      *    フィルム以降の明細をずらす
           IF   WRK-JYUNBAN  NOT =  ZERO
      *
               PERFORM   VARYING   IDX   FROM   IDX-RECE65   BY  -1
                          UNTIL  ( IDX    <    1           )
                           OR    ( IDX    <=   WRK-JYUNBAN )
      *
                   COMPUTE   WRK-GYOSUU2  =  IDX  +  1
                   MOVE   WRK-RECE65XX-SRYACT-TBL(IDX 1)  TO
                          WRK-RECE65XX-SRYACT-TBL(WRK-GYOSUU2 1)
                   INITIALIZE   WRK-RECE65XX-SRYACT-TBL(IDX 1)
      *
               END-PERFORM
      *
      *        分画数の差し込み
               MOVE   WRK-JYUNBAN     TO  MAX-MEISAISU
      *
               ADD    1               TO  MAX-MEISAISU
               MOVE  "810000001"
                              TO  WRK-RECE65XX-SRYCD(MAX-MEISAISU 1)
               MOVE   ZERO    TO  WRK-RECE65XX-SRYSURYO(MAX-MEISAISU 1)
      *
               MOVE   WRK-RECE65XX-SRYKAISU(WRK-JYUNBAN 1)
                                      TO  WRK-SRYKAISU-X
      *        全角変換
               INITIALIZE                     WRK-CHAR-HENKAN-AREA
               MOVE     WRK-SRYKAISU-X  TO    WRK-CHAR-X
               PERFORM  2503111-NUM-ZENKAKU-HENKAN-SEC
               STRING   WRK-CHAR-N      DELIMITED  BY  SPACE
                        "分画"          DELIMITED  BY  SIZE
                                        INTO  WRK-BUNKAKU-N
               END-STRING
               COMPUTE WRK-BUNKAKUKETA   =   WRK-CHAR-N-CNT  +  4
      *
               COMPUTE WRK-RECE65XX-YUKOKETA(MAX-MEISAISU 1) =
                       WRK-BUNKAKUKETA   /   2
               MOVE    WRK-BUNKAKU-N
                           TO  WRK-RECE65XX-NAME(MAX-MEISAISU 1)
               MOVE    "1" TO  WRK-RECE65XX-RECEPTFLG(MAX-MEISAISU 1)
      *
               IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                 MOVE  "2" TO  WRK-RECE65XX-MEISKYFLG(MAX-MEISAISU 1)
                 MOVE  WRK-JIDO-SETCOMATFLG
                           TO  WRK-RECE65XX-SETCOMATFLG(MAX-MEISAISU 1)
               END-IF
      *
           END-IF
      *
      *    コメントを含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
                    INITIALIZE                        RECE651-REC
                    MOVE    WRK-RECE65-KEY       TO   RECE651-KEY
                    ADD     1                    TO   WRK-RENNUM
                    MOVE    WRK-RENNUM           TO   RECE651-RENNUM
                    MOVE    WRK-RECE65-KOHBUNRI  TO   RECE651-KOHBUNRI
                    MOVE    WRK-RECE65-PTNUM     TO   RECE651-PTNUM
                    MOVE    WRK-RECE65-SRYKA     TO   RECE651-SRYKA
                    MOVE    WRK-RECE65-SRYKBN    TO   RECE651-SRYKBN
                    MOVE    WRK-RECE65-SRYSYUKBN TO   RECE651-SRYSYUKBN
                    MOVE    WRK-RECE65-JIHIMONEY TO   RECE651-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE65   =   1)
                       ADD         1             TO    IDX-RECE65
                       IF (WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE)
                       OR (IDX-RECE65                       >  100 )
                           MOVE    1             TO    FLG-WRKRECE65
                       ELSE
                           MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                            TO   RECE651-SRYACT-TBL(IDX)
                           ADD   1          TO   WRK-CNT-MEISAI
                       END-IF  
                    END-PERFORM
                    IF   RECE651-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE651-REC
                       ADD     1            TO   CNT-RECE65
                    END-IF
           END-PERFORM
      *
           MOVE    RECE649-REC          TO   RECE641-REC
           MOVE    WRK-CNT-MEISAI       TO   RECE641-MEISAISU
           IF   WRK-JIDO-SETCOMFLG   =  "1"
             MOVE  WRK-JIDO-SETCOMFLG   TO   RECE641-SETCOMFLG
           END-IF
           WRITE   RECE641-REC
           ADD     1                    TO   CNT-RECE64
      *
           .
       250311-SRYACTINF2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    数値半角→全角変換処理
      *****************************************************************
       2503111-NUM-ZENKAKU-HENKAN-SEC    SECTION.
      *
           MOVE     1       TO   WRK-CHAR-IDX
           MOVE    ZERO     TO   WRK-CHAR-N-CNT
      *
           PERFORM   VARYING   IDX20   FROM    1   BY  1
                      UNTIL    IDX20     >     3
              IF  WRK-CHAR-X(IDX20:1)  IS   NUMERIC
                 IF   WRK-CHAR-X(IDX20:1) = "0"
                   MOVE   "０"      TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                 ELSE
                   MOVE   WRK-CHAR-X(IDX20:1)
                                    TO  IDX21
                   MOVE   TBL-DAY(IDX21)(3:2)
                                    TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                 END-IF
                 ADD    2           TO  WRK-CHAR-IDX
                                        WRK-CHAR-N-CNT
              END-IF
           END-PERFORM
      *
           .
       2503111-NUM-ZENKAKU-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト６４９転送処理
      *****************************************************************
       2599-RECE649-TENSOU-SEC     SECTION.
      *
           MOVE       RECE649-REC    TO   RECE641-REC
           WRITE      RECE641-REC
           ADD        1              TO   CNT-RECE64
           PERFORM    25991-RECE659-TENSOU-SEC
      *
           .
       2599-RECE649-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト６５９転送処理
      *****************************************************************
       25991-RECE659-TENSOU-SEC     SECTION.
      *
           PERFORM  250111-RECE659-TAIHI-SEC
      *
      *    ＲＥＣＥ６５９ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE65
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           PERFORM
               UNTIL  ( FLG-RECE659           =   1                    )
               OR     ( RECE649-KEY1      NOT =   RECE659-KEY1         )
               OR     ( RECE649-ZAINUM    NOT =   RECE659-ZAINUM       )
               OR     ( RECE649-ZAIREN    NOT =   RECE659-ZAIREN       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   5     )
                         OR      ( RECE659-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE65
                    MOVE   RECE659-SRYACT-TBL(IDX)
                             TO   WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
               END-PERFORM
      *
               PERFORM 990-RECE659-NEXT-SEC
      *
           END-PERFORM
      *
      *    コメントを含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE65
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE65
           MOVE       ZERO             TO      WRK-RENNUM
           PERFORM    UNTIL     FLG-WRKRECE65  =    1
                    INITIALIZE                        RECE651-REC
                    MOVE    WRK-RECE65-KEY       TO   RECE651-KEY
                    ADD     1                    TO   WRK-RENNUM
                    MOVE    WRK-RENNUM           TO   RECE651-RENNUM
                    MOVE    WRK-RECE65-KOHBUNRI  TO   RECE651-KOHBUNRI
                    MOVE    WRK-RECE65-PTNUM     TO   RECE651-PTNUM
                    MOVE    WRK-RECE65-SRYKA     TO   RECE651-SRYKA
                    MOVE    WRK-RECE65-SRYKBN    TO   RECE651-SRYKBN
                    MOVE    WRK-RECE65-SRYSYUKBN TO   RECE651-SRYSYUKBN
                    MOVE    WRK-RECE65-JIHIMONEY TO   RECE651-JIHIMONEY
                    PERFORM  VARYING   IDX   FROM   1   BY   1
                             UNTIL   (IDX       >    5)   OR
                                     (FLG-WRKRECE65   =   1)
                       ADD         1             TO    IDX-RECE65
                       IF (WRK-RECE65XX-SRYCD(IDX-RECE65 1) = SPACE)
                       OR (IDX-RECE65                       >  100 )
                           MOVE    1             TO    FLG-WRKRECE65
                       ELSE
                           MOVE  WRK-RECE65XX-SRYACT-TBL(IDX-RECE65 1)
                                            TO   RECE651-SRYACT-TBL(IDX)
                       END-IF
                    END-PERFORM
                    IF   RECE651-SRYCD(1)  NOT =  SPACE
                       WRITE   RECE651-REC
                       ADD     1            TO   CNT-RECE65
                    END-IF
           END-PERFORM
      *
           .
       25991-RECE659-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択式コメント編集
      *****************************************************************
       270-SENTAKUCOM-HENSYU-SEC     SECTION.
      *
      *    パラメータ６４１ファイルより編集
           INITIALIZE                      RECE641-REC
           MOVE    RECE62-KEY          TO  RECE641-KEY1
           MOVE    ZERO                TO  RECE641-ZAINUM
      *
           START   RECE641-FILE
               KEY IS    >=   RECE641-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE641
               NOT INVALID
                   PERFORM 990-RECE641-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE641          =   1                    )
               OR     ( RECE62-KEY       NOT =   RECE641-KEY1         )
      *
               IF     RECE641-SENTAKUCOMFLG  =  "1"
                  MOVE    ZERO    TO    FLG-SENTAKU-OKIKAE
                  PERFORM   2701-SENTAKUCOM-HENSYU2-SEC
                  IF  FLG-SENTAKU-OKIKAE     =   1
                    PERFORM 2702-SENTAKUCOM-HENSYU4-SEC
                  END-IF
               END-IF
      *
               PERFORM 990-RECE641-NEXT-SEC
      *
           END-PERFORM
      *
           .
       270-SENTAKUCOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択式コメント編集２
      *****************************************************************
       2701-SENTAKUCOM-HENSYU2-SEC     SECTION.
      *
           MOVE    ZERO                 TO  IDX45
           MOVE    SPACE                TO  WRK-RECE65XX-REC
           INITIALIZE                       WRK-RECE65XX-REC
      *
           INITIALIZE                       RECE651-REC
           MOVE    RECE641-KEY1         TO  RECE651-KEY1
           MOVE    RECE641-ZAINUM       TO  RECE651-ZAINUM
           MOVE    RECE641-ZAIREN       TO  RECE651-ZAIREN
           START   RECE651-FILE
               KEY IS    >=   RECE651-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE651
               NOT INVALID
                   PERFORM 990-RECE651-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE651           =   1                    )
               OR     ( RECE641-KEY1      NOT =   RECE651-KEY1         )
               OR     ( RECE641-ZAINUM    NOT =   RECE651-ZAINUM       )
               OR     ( RECE641-ZAIREN    NOT =   RECE651-ZAIREN       )
      *
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL  (IDX                >   5  )
                          OR    (RECE651-SRYCD(IDX) = SPACE)
                 ADD    1   TO   IDX45
                 MOVE   RECE651-SRYACT-TBL(IDX)
                            TO   WRK-RECE65XX-SRYACT-TBL(IDX45 1)
      *
                 IF   (IDX45                                >   1   )
                 AND  (WRK-RECE65XX-SRYCD(IDX45 1)(1:4)     = "8201")
                 AND  (WRK-RECE65XX-MEISKYFLG(IDX45 1)  NOT = "2"   )
                   MOVE      ZERO    TO   FLG-GET
                   MOVE      ZERO    TO   FLG-ETC
                   COMPUTE   IDY12   =    IDX45  -  1
                   PERFORM  VARYING  IDY11  FROM  IDY12  BY  -1
                             UNTIL  (IDY11    <  1)
                              OR    (FLG-GET  =  1)
                     IF (WRK-RECE65XX-SRYCD(IDY11 1)(1:1) = "6" OR "7")
                     OR (WRK-RECE65XX-SRYCD(IDY11 1)(1:3) = "059"     )
                       MOVE    1     TO   FLG-ETC
                       MOVE  IDY11   TO   IDY13
                     ELSE
                       IF  WRK-RECE65XX-SRYCD(IDY11 1)(1:1) = "1"
                         PERFORM 27011-SENTAKUCOM-HENSYU3-SEC
                         IF   FLG-GET = ZERO
                           MOVE    1     TO   FLG-ETC
                           MOVE  IDY11   TO   IDY13
                         END-IF
                       END-IF
                     END-IF
                   END-PERFORM
                   IF     FLG-GET  =   1
                     IF   RECE641-SRYKBN = "90" OR "92" OR "97"
                       CONTINUE
                     ELSE
                       MOVE    1    TO   FLG-SENTAKU-OKIKAE
                       MOVE   "2"   TO   WRK-RECE65XX-MEISKYFLG(IDX45 1)
                     END-IF
                     IF   FLG-ETC  =   1
                       MOVE    1    TO   FLG-SENTAKU-OKIKAE
                       MOVE    ZERO      TO  IDX46
                       MOVE    SPACE     TO  WRK-RECE65ZZ-REC
                       INITIALIZE            WRK-RECE65ZZ-REC
                       PERFORM  VARYING  IDY11  FROM  1  BY  1
                                 UNTIL   IDY11    >   IDY12
                         IF   IDY11 = IDY13
                           ADD   1   TO   IDX46
                           MOVE  WRK-RECE65XX-SRYACT-TBL(IDX45 1)
                                 TO   WRK-RECE65ZZ-SRYACT-TBL(IDX46 1)
                         END-IF
                         ADD     1   TO   IDX46
                         MOVE    WRK-RECE65XX-SRYACT-TBL(IDY11 1)
                                 TO   WRK-RECE65ZZ-SRYACT-TBL(IDX46 1)
                       END-PERFORM
                       PERFORM  VARYING  IDY11  FROM  1  BY  1
                                 UNTIL   IDY11    >   IDX45
                         MOVE   WRK-RECE65ZZ-SRYACT-TBL(IDY11 1)
                                 TO   WRK-RECE65XX-SRYACT-TBL(IDY11 1)
                       END-PERFORM
                     END-IF
                   END-IF
                 END-IF
               END-PERFORM
      *
               PERFORM 990-RECE651-NEXT-SEC
      *
           END-PERFORM
      *
           .
       2701-SENTAKUCOM-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択式コメント編集３
      *****************************************************************
       27011-SENTAKUCOM-HENSYU3-SEC     SECTION.
      *
           INITIALIZE                      RKISAI-REC
           MOVE  WRK-RECE65XX-SRYCD(IDY11 1)
                                       TO  RKISAI-SRYCD
           MOVE  RECE62-SRYYM          TO  RKISAI-STYM
           MOVE  RECE62-SRYYM          TO  RKISAI-EDYM
           MOVE  RKISAI-REC            TO  MCPDATA-REC
           MOVE  "tbl_recekisai"       TO  MCP-TABLE
           MOVE  "key5"                TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC  =  ZERO
             MOVE  "tbl_recekisai"     TO  MCP-TABLE
             MOVE  "key5"              TO  MCP-PATHNAME
             PERFORM  900-RKISAI-READ-SEC
             PERFORM  UNTIL (FLG-RKISAI  =  1)
                       OR   (FLG-GET     =  1)
               IF   RKISAI-C-CD  =  WRK-RECE65XX-SRYCD(IDX45 1)
                 MOVE    1    TO    FLG-GET
               END-IF
               MOVE  "tbl_recekisai"   TO  MCP-TABLE
               MOVE  "key5"            TO  MCP-PATHNAME
               PERFORM  900-RKISAI-READ-SEC
             END-PERFORM
           END-IF
           MOVE  "tbl_recekisai"       TO  MCP-TABLE
           MOVE  "key5"                TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       27011-SENTAKUCOM-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択式コメント編集４
      *****************************************************************
       2702-SENTAKUCOM-HENSYU4-SEC     SECTION.
      *
           MOVE    ZERO                 TO  IDX45
      *
           INITIALIZE                       RECE651-REC
           MOVE    RECE641-KEY1         TO  RECE651-KEY1
           MOVE    RECE641-ZAINUM       TO  RECE651-ZAINUM
           MOVE    RECE641-ZAIREN       TO  RECE651-ZAIREN
           START   RECE651-FILE
               KEY IS    >=   RECE651-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE651
               NOT INVALID
                   PERFORM 990-RECE651-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE651           =   1                    )
               OR     ( RECE641-KEY1      NOT =   RECE651-KEY1         )
               OR     ( RECE641-ZAINUM    NOT =   RECE651-ZAINUM       )
               OR     ( RECE641-ZAIREN    NOT =   RECE651-ZAIREN       )
      *
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL  (IDX                >   5  )
                          OR    (RECE651-SRYCD(IDX) = SPACE)
                 ADD    1   TO   IDX45
                 MOVE   WRK-RECE65XX-SRYACT-TBL(IDX45 1)
                            TO   RECE651-SRYACT-TBL(IDX)
               END-PERFORM
      *
               REWRITE   RECE651-REC
               ADD       1        TO   CNT-RECE65-RE
      *
               PERFORM 990-RECE651-NEXT-SEC
      *
           END-PERFORM
      *
           IF   RECE641-SRYKBN = "90" OR "92" OR "97"
             CONTINUE
           ELSE
             MOVE "1"     TO   RECE641-SETCOMFLG
           END-IF
           REWRITE   RECE641-REC
           ADD     1      TO   CNT-RECE64-RE
      *
           .
       2702-SENTAKUCOM-HENSYU4-EXT.
           EXIT.
      *
      *****************************************************************
      *    関係コメント編集
      *****************************************************************
       260-SETCOM-HENSYU-SEC     SECTION.
      *
      *    パラメータ６４１ファイルより編集
           INITIALIZE                      RECE641-REC
           MOVE    RECE62-KEY          TO  RECE641-KEY1
           MOVE    ZERO                TO  RECE641-ZAINUM
      *
           START   RECE641-FILE
               KEY IS    >=   RECE641-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE641
               NOT INVALID
                   PERFORM 990-RECE641-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE641          =   1                    )
               OR     ( RECE62-KEY       NOT =   RECE641-KEY1         )
      *
               IF     RECE641-SETCOMFLG   =    "1"
                  PERFORM 2601-SETCOM-HENSYU2-SEC
                  IF  RECE641-SRYYM   >=  "200804"
                    CONTINUE
                  ELSE
                    MOVE   SPACE   TO   RECE641-SETCOMFLG
                    REWRITE   RECE641-REC
                    ADD     1      TO   CNT-RECE64-RE
                  END-IF
               END-IF
      *
               PERFORM 990-RECE641-NEXT-SEC
      *
           END-PERFORM
      *
           .
       260-SETCOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    関係コメント編集２
      *****************************************************************
       2601-SETCOM-HENSYU2-SEC     SECTION.
      *
           MOVE    ZERO    TO    FLG-SETCOM
      *
           MOVE    ZERO    TO    WRK-HEN-RENNUM-X
                                 WRK-HEN-SHORI-RENNUM-X
                                 WRK-HEN-SEQNO-X
           MOVE    ZERO    TO    WRK-HEN-RENNUM-J
                                 WRK-HEN-SHORI-RENNUM-J
                                 WRK-HEN-SEQNO-J
           MOVE    ZERO    TO    WRK-HEN-RENNUM-J2
                                 WRK-HEN-SHORI-RENNUM-J2
                                 WRK-HEN-SEQNO-J2
           MOVE    ZERO    TO    WRK-HEN-RENNUM-J3
                                 WRK-HEN-SHORI-RENNUM-J3
                                 WRK-HEN-SEQNO-J3
      *
           INITIALIZE                       RECE651-REC
           MOVE    RECE641-KEY1         TO  RECE651-KEY1
           MOVE    RECE641-ZAINUM       TO  RECE651-ZAINUM
           MOVE    RECE641-ZAIREN       TO  RECE651-ZAIREN
           START   RECE651-FILE
               KEY IS    >=   RECE651-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE651
               NOT INVALID
                   PERFORM 990-RECE651-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE651           =   1                    )
               OR     ( RECE641-KEY1      NOT =   RECE651-KEY1         )
               OR     ( RECE641-ZAINUM    NOT =   RECE651-ZAINUM       )
               OR     ( RECE641-ZAIREN    NOT =   RECE651-ZAIREN       )
      *
               MOVE      ZERO      TO    FLG-MEISKY-OKIKAE
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   5     )
                         OR      ( RECE651-SRYCD(IDX) =   SPACE )
                 IF   RECE651-MEISKYFLG(IDX)  =  "2"
                   IF      RECE651-SRYYM  >=  "200804"
                    IF     RECE651-SETCOMATFLG(IDX) = ZERO
                      IF   (FLG-SETCOM              =  1   )
                      AND  (RECE651-RECEDENFLG(IDX) = SPACE)
                        ADD    1                 TO  WRK-HEN-SEQNO-X
                        IF   WRK-HEN-SEQNO-X  >  3
                          MOVE   SPACE   TO  RECE651-MEISKYFLG(IDX)
                          MOVE   1       TO  FLG-MEISKY-OKIKAE
                        ELSE
                          MOVE   WRK-HEN-RENNUM-X  TO  WRK-HEN-RENNUM
                          MOVE   WRK-HEN-SHORI-RENNUM-X
                                         TO  WRK-HEN-SHORI-RENNUM
                          MOVE   WRK-HEN-SEQNO-X   TO  WRK-HEN-SEQNO
                          PERFORM  26011-SETCOM-HENSYU3-SEC
                        END-IF
                        IF  (WRK-HEN-RENNUM-X = WRK-HEN-RENNUM-J )
                        AND (WRK-HEN-SHORI-RENNUM-X
                                        = WRK-HEN-SHORI-RENNUM-J )
                          ADD  1                 TO  WRK-HEN-SEQNO-J
                        END-IF
                        IF  (WRK-HEN-RENNUM-X = WRK-HEN-RENNUM-J2)
                        AND (WRK-HEN-SHORI-RENNUM-X
                                        = WRK-HEN-SHORI-RENNUM-J2)
                          ADD  1                 TO  WRK-HEN-SEQNO-J2
                        END-IF
                      END-IF
                    ELSE
                      EVALUATE   RECE651-SETCOMATFLG(IDX)
                        WHEN    1
                        WHEN    3  THRU   38
                        WHEN   41
                        WHEN   43  THRU   47
                        WHEN  100  THRU  110
                          ADD  1                 TO  WRK-HEN-SEQNO-J
                          IF   WRK-HEN-SEQNO-J  >  3
                            MOVE   SPACE   TO  RECE651-MEISKYFLG(IDX)
                            MOVE   1       TO  FLG-MEISKY-OKIKAE
                          ELSE
                            MOVE WRK-HEN-RENNUM-J  TO  WRK-HEN-RENNUM
                            MOVE WRK-HEN-SHORI-RENNUM-J
                                         TO  WRK-HEN-SHORI-RENNUM
                            MOVE WRK-HEN-SEQNO-J   TO  WRK-HEN-SEQNO
                            PERFORM  26011-SETCOM-HENSYU3-SEC
                          END-IF
                          IF  (WRK-HEN-RENNUM-X = WRK-HEN-RENNUM-J )
                          AND (WRK-HEN-SHORI-RENNUM-X
                                          = WRK-HEN-SHORI-RENNUM-J )
                            ADD    1             TO  WRK-HEN-SEQNO-X
                          END-IF
                        WHEN    2
                        WHEN   39  THRU   40
                        WHEN   42
                          ADD  1                 TO  WRK-HEN-SEQNO-J2
                          IF   WRK-HEN-SEQNO-J2 >  3
                            MOVE   SPACE   TO  RECE651-MEISKYFLG(IDX)
                            MOVE   1       TO  FLG-MEISKY-OKIKAE
                          ELSE
                            MOVE WRK-HEN-RENNUM-J2 TO  WRK-HEN-RENNUM
                            MOVE WRK-HEN-SHORI-RENNUM-J2
                                         TO  WRK-HEN-SHORI-RENNUM
                            MOVE WRK-HEN-SEQNO-J2  TO  WRK-HEN-SEQNO
                            PERFORM  26011-SETCOM-HENSYU3-SEC
                          END-IF
                          IF  (WRK-HEN-RENNUM-X = WRK-HEN-RENNUM-J2)
                          AND (WRK-HEN-SHORI-RENNUM-X
                                          = WRK-HEN-SHORI-RENNUM-J2)
                            ADD    1             TO  WRK-HEN-SEQNO-X
                          END-IF
                      END-EVALUATE
                    END-IF
                   ELSE
                    MOVE   SPACE   TO  RECE651-MEISKYFLG(IDX)
                    MOVE   1       TO  FLG-MEISKY-OKIKAE
                   END-IF
                 ELSE
                    IF   (RECE651-SRYCD(IDX)(1:1) = "1" OR "6" OR "7")
                    OR   (RECE651-SRYCD(IDX)(1:3) = "059"            )
                      MOVE   1               TO  FLG-SETCOM
                      MOVE   RECE651-RENNUM  TO  WRK-HEN-RENNUM-X
                      MOVE   IDX             TO  WRK-HEN-SHORI-RENNUM-X
                      MOVE   ZERO            TO  WRK-HEN-SEQNO-X
                    END-IF
      *
                    IF    RECE651-SRYCD(IDX) =
      *1
                          "113000410" OR "113000510" OR "180000110" OR
                          "180000210" OR "113023910" OR
      *3
                          "113001210" OR "113001310" OR "113002110" OR
      *4
                          "111011810" OR "111012610" OR "111012810" OR
      *6
                          "180000710" OR
      *7
                          "113004310" OR
      *11
                          "114008010" OR
      *12
                          "114009410" OR
      *16
                          "180027410" OR "180027510" OR "180028010" OR
                          "180028110" OR "180027610" OR "180027710" OR
                          "180030810" OR "180027810" OR "180027910" OR
                          "180028210" OR "180028310" OR "180028410" OR
                          "180032410" OR "180032510" OR "180032610" OR
                          "180032710" OR "180033110" OR
                          "180033910" OR "180034010" OR "180034110" OR
                          "180034210" OR "180034310" OR "180034410" OR
                          "180034510" OR "180034610" OR "180034710" OR
                          "180044310" OR "180044410" OR "180044510" OR
                          "180045810" OR "180045910" OR "180046010" OR
                          "180044610" OR "180044710" OR "180044810" OR
                          "180044910" OR "180045110" OR "180045310" OR
                          "180050330" OR "180050430" OR "180050530" OR
                          "180050630" OR "180050830" OR "180051030" OR
                          "180052730" OR "180052830" OR "180052930" OR
                          "180053030" OR "180053230" OR "180053430" OR
                          "180051530" OR "180051630" OR "180051730" OR
                          "180051830" OR "180052030" OR "180052230" OR
      *18
                          "113003410" OR
      *19
                          "114045510" OR
      *20
                          "700010000" OR "700030000" OR "700050000" OR
                          "700080000" OR "700110000" OR "700140000" OR
                          "700290000" OR "700310000" OR "700330000" OR
                          "700710000" OR "700730000" OR "700750000" OR
                          "700780000" OR "700810000" OR "700840000" OR
                          "700870000" OR "700160000" OR "700180000" OR
                          "700190000" OR "700200000" OR "700210000" OR
                          "700220000" OR "700230000" OR "700240000" OR
                          "700250000" OR "700260000" OR "700270000" OR
                          "700280000" OR "700590000" OR "700600000" OR
                          "700630000" OR "700640000" OR "700650000" OR
                          "700680000" OR "729780000" OR
      *24
                          "114020910" OR
      *26
                          "114009710" OR
      *27
                          "113015010" OR "113015130" OR "114045470" OR
                          "114045670" OR
      *29
                          "114027610" OR "114027710" OR "114027810" OR
                          "114027910" OR
      *35
                          "140057510" OR "140057610" OR
      *41
                          "140007710" OR "140008510" OR "140029850" OR
                          "140008210" OR "140039210" OR "140057810" OR
                          "140057910" OR "140058010" OR "140058110" OR
                          "140058210" OR "140058310" OR "140058410" OR
                          "140058510" OR "140058610" OR "140059310" OR
                          "140059410" OR "140059510" OR
                          "140060210" OR "140060310" OR "140060410" OR
                          "140060510" OR "140060610" OR "140060710" OR
                          "140060810" OR "140060910" OR "140061010" OR
      *43
                          "160000210" OR "160177770" OR
      *44
                          "170016010" OR
      *45
                          "113030670" OR "113030770" OR
      *47
                          "180058650" OR
      *
      *<入院のみ>
      *100
                          "113022810" OR
      *101
                          "113010710" OR "113010810" OR "120001610" OR
      *102
                          "113003310" OR "113014810" OR "113014910" OR
                          "113017610" OR "113017710" OR "113017810" OR
                          "113017910" OR
      *103
                          "190030310" OR
      *104
                          "113006270" OR
      *105
                          "180018310" OR "188003110" OR
      *106
                          "113011710" OR
      *107
                          "180039030" OR "180036030" OR "180039130" OR
                          "180039230" OR "180039330" OR "180036130" OR
                          "180039430" OR "180039530" OR
                          "180048930" OR "180049030" OR
      *108
                          "113008910" OR
      *110
                          "140061210"
      *
                       MOVE   RECE651-RENNUM  TO  WRK-HEN-RENNUM-J
                       MOVE   IDX       TO  WRK-HEN-SHORI-RENNUM-J
                       MOVE   ZERO            TO  WRK-HEN-SEQNO-J
                    END-IF
      *
                    IF    RECE651-SRYCD(IDX) =
      *2
                          "113000670" OR "113000770" OR "113029170" OR
                          "113029270" OR "113029370" OR
      *40
                          "180056570" OR
      *42
                          "140052170" OR "140052270" OR "140052370"
      *
                       MOVE   RECE651-RENNUM  TO  WRK-HEN-RENNUM-J2
                       MOVE   IDX       TO  WRK-HEN-SHORI-RENNUM-J2
                       MOVE   ZERO            TO  WRK-HEN-SEQNO-J2
                    END-IF
      *
      *             医療情報連携編集
                    IF    RECE651-INGAIFLG(IDX)  =   "1"
                       MOVE   RECE651-RENNUM  TO  WRK-HEN-RENNUM-J3
                       MOVE   IDX       TO  WRK-HEN-SHORI-RENNUM-J3
                       MOVE   ZERO            TO  WRK-HEN-SEQNO-J3
      *
                       ADD    1         TO  WRK-HEN-SEQNO-J3
                       MOVE   WRK-HEN-RENNUM-J3
                                        TO  WRK-HEN-RENNUM
                       MOVE   WRK-HEN-SHORI-RENNUM-J3
                                        TO  WRK-HEN-SHORI-RENNUM
                       MOVE   WRK-HEN-SEQNO-J3
                                        TO  WRK-HEN-SEQNO
                       PERFORM  26011-SETCOM-HENSYU3-SEC
                       IF  (WRK-HEN-RENNUM-J3 = WRK-HEN-RENNUM-X)
                       AND (WRK-HEN-SHORI-RENNUM-J3
                                        = WRK-HEN-SHORI-RENNUM-X)
                         ADD  1         TO  WRK-HEN-SEQNO-X
                       END-IF
                    END-IF
                 END-IF
               END-PERFORM
      *
               IF    FLG-MEISKY-OKIKAE  =  1
                  REWRITE   RECE651-REC
                  ADD       1        TO   CNT-RECE65-RE
               END-IF
      *
               PERFORM 990-RECE651-NEXT-SEC
      *
           END-PERFORM
      *
           .
       2601-SETCOM-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    関係コメント編集３
      *****************************************************************
       26011-SETCOM-HENSYU3-SEC     SECTION.
      *
           MOVE    SPACE                 TO  RECE655-REC
           INITIALIZE                        RECE655-REC
      *
           MOVE    RECE651-KEY           TO  RECE655-KEY
      *
           MOVE    WRK-HEN-RENNUM        TO  RECE655-RENNUM
           MOVE    WRK-HEN-SHORI-RENNUM  TO  RECE655-SHORI-RENNUM
           MOVE    WRK-HEN-SEQNO         TO  RECE655-SEQNO
      *
           IF    RECE651-INGAIFLG(IDX)  =   "1"
             IF   RECE651-GENERIC-KBN(IDX)  =  "1"
               MOVE  RECE651-YAKKAKJNCD(IDX)(1:9)
                                         TO  RECE655-SRYCD
             END-IF
      *      後ろの全角空白を削除する
             INITIALIZE                      ORCSKANACHKAREA
             MOVE    "1"                 TO  KANACHK-SYORI
             MOVE    "1"                 TO  KANACHK-SYORIKBN
             MOVE    RECE651-GENERICNAME (IDX)
                                         TO  KANACHK-MAE-INPUT
             CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
             IF    ( KANACHK-RC          =   ZERO )
             AND   ( KANACHK-MAX         >   ZERO )
               MOVE    SPACE             TO  RECE651-GENERICNAME (IDX)
               MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                         TO  RECE651-GENERICNAME (IDX)
             END-IF
             MOVE    RECE651-GENERICNAME(IDX)
                                         TO  RECE655-NAME
                                             RECE655-INPUTCOMENT
           ELSE
             MOVE    RECE651-SRYCD(IDX)  TO  RECE655-SRYCD
      *      後ろの全角空白を削除する
             INITIALIZE                      ORCSKANACHKAREA
             MOVE    "1"                 TO  KANACHK-SYORI
             MOVE    "1"                 TO  KANACHK-SYORIKBN
             MOVE    RECE651-NAME (IDX) TO  KANACHK-MAE-INPUT
             CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
             IF    ( KANACHK-RC          =   ZERO )
             AND   ( KANACHK-MAX         >   ZERO )
               MOVE    SPACE             TO  RECE651-NAME (IDX)
               MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                         TO  RECE651-NAME (IDX)
             END-IF
             MOVE    RECE651-NAME(IDX)   TO  RECE655-NAME
      *      後ろの全角空白を削除する
             INITIALIZE                      ORCSKANACHKAREA
             MOVE    "1"                 TO  KANACHK-SYORI
             MOVE    "1"                 TO  KANACHK-SYORIKBN
             MOVE    RECE651-INPUTCOMENT(IDX)
                                         TO  KANACHK-MAE-INPUT
             CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
             IF    ( KANACHK-RC          =   ZERO )
             AND   ( KANACHK-MAX         >   ZERO )
               MOVE    SPACE             TO  RECE651-INPUTCOMENT (IDX)
               MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                         TO  RECE651-INPUTCOMENT (IDX)
             END-IF
             MOVE    RECE651-INPUTCOMENT(IDX)
                                         TO  RECE655-INPUTCOMENT
           END-IF
      *
           WRITE   RECE655-REC
           ADD     1                     TO  CNT-RECE655
      *
           .
       26011-SETCOM-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＫＥＹ編集処理
      *****************************************************************
       500-RECE64-KEY-HENSYU-SEC       SECTION.
      *
           INITIALIZE                      RECE649-REC
           MOVE    RECE62-KEY          TO  RECE649-KEY1
           .
       500-RECE64-KEY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＫＥＹ編集処理
      *****************************************************************
       500-RECE65-KEY-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE               TO  RECE659-REC
           INITIALIZE                      RECE659-REC
           MOVE    RECE62-KEY          TO  RECE659-KEY1
           .
       500-RECE65-KEY-HENSYU-EXT.
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
           STRING  "ORCR0660 "             DELIMITED   BY  SIZE
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ファイルＣＬＯＳＥ処理
           PERFORM 310-FILE-CLOSE-SEC
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
           DISPLAY "RECE62  CNT-IN  :" CNT-RECE62 
           DISPLAY "RECE649 CNT-WK  :" CNT-RECE649
           DISPLAY "RECE659 CNT-WK  :" CNT-RECE659
           DISPLAY "RECE64  CNT-OUT :" CNT-RECE64 
           DISPLAY "RECE65  CNT-OUT :" CNT-RECE65 
           DISPLAY "RECE64  REWRITE :" CNT-RECE64-RE
           DISPLAY "RECE65  REWRITE :" CNT-RECE65-RE
           DISPLAY "RECE655 CNT-OUT :" CNT-RECE655
           DISPLAY "*** ORCR0660 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルＣＬＯＳＥ処理
      *****************************************************************
       310-FILE-CLOSE-SEC              SECTION.
      *
           CLOSE                       RECE62-FILE
      *
           CLOSE                       RECE64-FILE
      *
           CLOSE                       RECE65-FILE
      *
           CLOSE                       RECE641-FILE     
           CLOSE                       RECE651-FILE     
      *
           CLOSE                       RECE649-FILE
           CLOSE                       RECE659-FILE
      *
           CLOSE                       RECE655-FILE
      *
           .
       310-FILE-CLOSE-EXT.
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
      *    レセプト０２データ順検索処理
      *****************************************************************
       990-RECE62-READ-SEC           SECTION.
      *
           READ    RECE62-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
               NOT AT  END
                   ADD     1           TO  CNT-RECE62
                   ADD     1           TO  CNT-IN-COMMIT
                                           CNT-IN-CANCEL
      *
                   DISPLAY "RECE62  PTNUM=" RECE62-PTNUM
      *
           END-READ
      *
           .
       990-RECE62-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０４データ読み
      ****************************************************************
       990-RECE64-NEXT-SEC           SECTION.
      *
           READ    RECE64-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE64
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE64
           END-READ
      *
           .
       990-RECE64-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト６４９データ読み
      ****************************************************************
       990-RECE649-NEXT-SEC           SECTION.
      *
           READ    RECE649-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE649
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE649
           END-READ
      *
           .
       990-RECE649-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト６４１データ読み
      ****************************************************************
       990-RECE641-NEXT-SEC           SECTION.
      *
           READ    RECE641-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE641
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE641
           END-READ
      *
           .
       990-RECE641-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０５データ読み
      ****************************************************************
       990-RECE65-NEXT-SEC           SECTION.
      *
           READ    RECE65-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE65
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE65
           END-READ
      *
           .
       990-RECE65-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト６５９データ読み
      ****************************************************************
       990-RECE659-NEXT-SEC           SECTION.
      *
           READ    RECE659-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE659
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE659
           END-READ
      *
           .
       990-RECE659-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト６５１データ読み
      ****************************************************************
       990-RECE651-NEXT-SEC           SECTION.
      *
           READ    RECE651-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE651
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE651
           END-READ
      *
           .
       990-RECE651-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴付加読込
      *****************************************************************
       900-SANTEIPLUS-READ-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SANTEIPLUS-REC
               MOVE    ZERO            TO  FLG-SANTEIPLUS
           ELSE
               INITIALIZE                  SANTEIPLUS-REC
               MOVE    1               TO  FLG-SANTEIPLUS
           END-IF
      *
           .
       900-SANTEIPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト記載
      *****************************************************************
       900-RKISAI-READ-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  RKISAI-REC
               MOVE    ZERO            TO  FLG-RKISAI
           ELSE
               INITIALIZE                  RKISAI-REC
               MOVE    1               TO  FLG-RKISAI
           END-IF
      *
           .
       900-RKISAI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴マスタ読み込み
      *****************************************************************
       900-SANTEI-INV-SEC             SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SANTEI-REC
                   MOVE    ZERO                TO  FLG-SANTEI
               ELSE
                   MOVE    1                   TO  FLG-SANTEI
                   INITIALIZE                      SANTEI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SANTEI
               INITIALIZE                      SANTEI-REC
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SANTEI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       900-SANTEI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       900-SANTEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴マスタ読み込み
      *****************************************************************
       900-PTNYUINRRK-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
               MOVE    ZERO                TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       900-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       900-PTCOM-READ-SEC           SECTION.
      *
           MOVE     PTCOM-REC          TO      MCPDATA-REC
           MOVE    "tbl_ptcom"         TO      MCP-TABLE
           MOVE    "key"               TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"       TO      MCP-TABLE
               MOVE    "key"             TO      MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                      PTCOM-REC
               END-IF
           ELSE
               MOVE    1                       TO  FLG-PTCOM
               INITIALIZE                          PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO      MCP-TABLE
           MOVE    "key"               TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       900-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数テーブルの検索
      *****************************************************************
       900-TENSU-TBL-SEC               SECTION.
      *
           INITIALIZE                           TNS-TENSU-REC
           MOVE    WRK-TENSU-SRYCD      TO      TNS-SRYCD
           MOVE    WRK-TENSU-SRYYM      TO      TNS-YUKOSTYMD(1:6)
           MOVE    "01"                 TO      TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD        TO      TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM          TO      TNS-HOSPNUM
           MOVE    TNS-TENSU-REC        TO      MCPDATA-REC
           MOVE    "tbl_tensu"          TO      MCP-TABLE
           MOVE    "key"                TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_tensu"      TO      MCP-TABLE
               MOVE    "key"            TO      MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC           =       ZERO
                   MOVE    MCPDATA-REC  TO      TNS-TENSU-REC
                   MOVE    ZERO         TO      FLG-TENSU
               ELSE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE    1            TO      FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                       TNS-TENSU-REC
               MOVE    1                TO      FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"          TO      MCP-TABLE
           MOVE    "key"                TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       900-TENSU-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
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
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
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
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
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
       120-DBOPEN-SEC                SECTION.
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
       120-DBOPEN-EXT.
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
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　ＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.
