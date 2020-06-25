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
       PROGRAM-ID.             ORCSRIHASTDAY.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : リハビリ開始日コメント編集サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.01    NACL-門脇    16/03/28  平成２８年４月改正対応
      *  04.08.02    NACL-門脇    16/09/15  リハビリマスタ追加対応
      *  04.08.03    NACL-門脇    18/04/05  平成３０年４月改正対応
      *  04.08.04    NACL-門脇    18/06/21  リハビリ開始日記載修正
      *  04.08.05    NACL-門脇    18/07/02  リハビリ開始日記載修正
      *
      *  05.00.01    NACL-門脇    19/04/18  自動記載コメント改元対応
      *  05.00.02    NACL-門脇    19/06/24  リハビリ発症日レセプト記載対応
      *                                     （機能追加）
      *  05.00.03    NACL-門脇    19/08/01  リハビリ発症日レセプト記載対応
      *                                     （機能追加）修正
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-HKNCOMBI                  PIC 9(01).
           03  FLG-SANTEI                    PIC 9(01).
           03  FLG-SANTEIPLUS                PIC 9(01).
           03  FLG-SRYACCT                   PIC 9(01).
           03  FLG-SRYACT                    PIC 9(01).
           03  FLG-PTCOM                     PIC 9(01).
           03  FLG-TENSU                     PIC 9(01).
           03  FLG-HKNCOMBI-SET              PIC 9(01).
           03  FLG-COMOKIKAE                 PIC 9(01).
           03  FLG-COMOKIKAE2                PIC 9(01).
           03  FLG-KYUSEI-COMMENT            PIC 9(01).
           03  FLG-KYUSEI-COMMENT2           PIC 9(01).
           03  FLG-SANTEIPLUS-HANTEI         PIC 9(01).
           03  FLG-OK                        PIC 9(01).
           03  FLG-KYUSEI-OK                 PIC 9(01).
           03  FLG-COMARI                    PIC 9(01).
           03  FLG-COMARI2                   PIC 9(01).
           03  FLG-ARI                       PIC 9(01).
           03  FLG-ARI2                      PIC 9(01).
           03  FLG-SET                       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                           PIC 9(03).
           03  IDX1                          PIC 9(03).
           03  IDX3                          PIC 9(02).
           03  IDX12                         PIC 9(02).
           03  IDX13                         PIC 9(03).
           03  IDX15                         PIC 9(01).
           03  IDX16                         PIC 9(02).
           03  IDX17                         PIC 9(02).
           03  IDX18                         PIC 9(02).
           03  IDX19                         PIC 9(02).
           03  IDX20                         PIC 9(02).
           03  IDY                           PIC 9(02).
           03  IDZ                           PIC 9(03).
           03  IDW                           PIC 9(02).
           03  IDW2                          PIC 9(02).
           03  IDC1                          PIC 9(02).
           03  IDC2                          PIC 9(02).
           03  IDC3                          PIC 9(02).
           03  IDC4                          PIC 9(02).
           03  RIHA-ST                       PIC 9(02).
           03  RIHA-ST2                      PIC 9(02).
           03  RIHA-IDX                      PIC 9(02).
           03  RIHA-IDX2                     PIC 9(02).
           03  DAY-IDX2                      PIC 9(02).
           03  RIHA-ED                       PIC 9(02).
           03  RIHA-ED2                      PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYCD                     PIC X(09).
           03  WRK-SRYCD2                    PIC X(09).
           03  WRK-SRYCD3                    PIC X(09).
           03  WRK-RIHA-HKNCOMBI             PIC 9(04).
           03  TBL-HASSYO-TAIHI-DAY-T.
               05  TBL-HASSYO-TAIHI-SANTEI   PIC 9(01)   OCCURS  31.
      *
           03  TBL-HASSYO-TAIHIYMD1-DAY-T.
               05  TBL-HASSYO-TAIHIYMD1-FLG  PIC 9(01).
               05  TBL-HASSYO-TAIHIYMD1-DAY-OCC1.
                   07  TBL-HASSYO-TAIHIYMD11 PIC X(08)   OCCURS   2.
               05  TBL-HASSYO-TAIHIYMD1-DAY-OCC2.
                   07  TBL-HASSYO-TAIHIYMD12 PIC 9(01)   OCCURS  31.
      *
           03  WRK-KYUSEIYMD                 PIC 9(08).
      *
           03  WRK-RIHASANTEI-END            PIC X(08).
      *
           03  WRK-RIHASANTEI-YMD            PIC X(08).
      *
           03  WRK-RIHASANTEI-YMD2           PIC X(08).
           03  WRK-RIHASANTEI-YMD3           PIC X(08).
      *
           03  TBL-RIHASANTEI-DAY.
               05  TB-RIHASANTEI-DAY         PIC 9(01)   OCCURS  31.
      *
           03  TBL-RIHASANTEI-DAY-END.
               05  TB-RIHASANTEI-DAY-END     PIC 9(01)   OCCURS  31.
           03  TBL-RIHASANTEI-DAY-END2.
               05  TB-RIHASANTEI-DAY-END2    PIC 9(01)   OCCURS  31.
           03  TBL-RIHASANTEI-DAY-END3.
               05  TB-RIHASANTEI-DAY-END3    PIC 9(01)   OCCURS  31.
      *
           03  WRK-RIHASANTEI41-FLG          PIC 9(01).
      *
      *    リハビリ開始日指定チェック用
           03  WRK-CHECK-RIHASANTEI-AREA.
               05  WRK-CHECK-RIHASANTEI-YMD  PIC X(08).
               05  WRK-CHECK-RIHASANTEI-YMD3 PIC X(08).
               05  WRK-CHECK-RIHASANTEI-DAY-OCC.
                 07  WRK-CHECK-RIHASANTEI-DAY
                                             PIC 9(01)   OCCURS  31.
      *
           03  WRK-KYUSEI-NYUGAIKBN          PIC X(01).
           03  WRK-KYUSEI-SRYKA              PIC X(02).
           03  WRK-KYUSEI-ZAINUM             PIC 9(08).
      *
           03  WRK-TAIHI-XX                  PIC X(02).
      *
           03  WRK-SRYKBN                    PIC X(02).
      *
           03  WRK-RIHA-YMD                  PIC X(07).
           03  WRK-RIHA-YMD-SEI              PIC X(08).
      *
       01  WRK-FREE-AREA.
           03  WRK-FREEINF                   OCCURS 49.
               05  WK-FREE-SRYCD             PIC X(09).
               05  WK-FREE-YUKOKETA          PIC 9(03).
               05  WK-FREE-NAME              PIC X(200).
      *
      *  １：心大血管疾患
      *  ２：心大血管疾患の早期・初期リハ加算
      *  ３：脳血管疾患等
      *  ４：廃用症候群
      *  ５：廃用症候群の早期・初期リハ加算
      *  ６：運動器
      *  ７：呼吸器
      *  ８：呼吸器の早期・初期リハ加算
      *  ９：摂食
      *１０：難病患者
      *１１：障害児（者）
       01  WRK-RIHASANTEI-AREA.
           03  WRK-RIHASANTEI-OCC            OCCURS 11.
               05  WRK-RIHASANTEI-FLG        PIC 9(01).
               05  WRK-RIHASANTEI-DAY-TBL.
                   06  WRK-RIHASANTEI-DAY    PIC 9(01)   OCCURS  31.
               05  WRK-RIHASANTEI-SRYCD-TBL.
                   06  WRK-RIHASANTEI-SRYCD  PIC X(09)   OCCURS  50.
      *
               05  WRK-RIHASANTEI-DAY-TBL2.
                   06  WRK-RIHASANTEI-TAIHIYMD-FLG
                                             PIC 9(01).
                   06  WRK-RIHASANTEI-TAIHIYMD-OCC1.
                       07  WRK-RIHASANTEI-TAIHIYMD1
                                             PIC X(08)    OCCURS  2.
                   06  WRK-RIHASANTEI-TAIHIYMD-OCC2.
                       07  WRK-RIHASANTEI-TAIHIYMD2
                                             PIC 9(01)    OCCURS 31.
      *
      *    固定値
       01  CONST-AREA.
      *
      *    定形コメント
           03  CONST-RIHA-SYSCD-VAL.
      *        心大リハ開始日
               05  CONS-SINDAI            PIC X(09)   VALUE "099800111".
      *        呼吸器リハ開始日
               05  CONS-KOKYUKI           PIC X(09)   VALUE "099800141".
      *        脳血管疾患等リハ開始日
               05  CONS-NOUKE             PIC X(09)   VALUE "099800121".
      *        運動器リハ開始日
               05  CONS-UNDOKI            PIC X(09)   VALUE "099800131".
      *        難病患者リハ開始日
               05  CONS-NANBYO            PIC X(09)   VALUE "099800161".
      *        障害児（者）リハ開始日
               05  CONS-SYOGAI            PIC X(09)   VALUE "099800171".
      *        摂食療法開始日
               05  CONS-SESSYOKU          PIC X(09)   VALUE "099800151".
      *        廃用症候群リハ開始日
               05  CONS-HAIYO             PIC X(09)   VALUE "099800191".
      *
      *        心大リハ加算開始日
               05  CONS-SINDAI-K          PIC X(09)   VALUE "099800211".
      *        呼吸器リハ加算開始日
               05  CONS-KOKYUKI-K         PIC X(09)   VALUE "099800241".
      *        廃用症候群リハ加算開始日
               05  CONS-HAIYO-K           PIC X(09)   VALUE "099800291".
      *
      *        心大リハ終了日
               05  CONS-SINDAI-E          PIC X(09)   VALUE "099800112".
      *        呼吸器リハ終了日
               05  CONS-KOKYUKI-E         PIC X(09)   VALUE "099800142".
      *        脳血管疾患等リハ終了日
               05  CONS-NOUKE-E           PIC X(09)   VALUE "099800122".
      *        運動器リハ終了日
               05  CONS-UNDOKI-E          PIC X(09)   VALUE "099800132".
      *        難病患者リハ終了日
               05  CONS-NANBYO-E          PIC X(09)   VALUE "099800162".
      *        障害児（者）リハ終了日
               05  CONS-SYOGAI-E          PIC X(09)   VALUE "099800172".
      *        摂食療法終了日
               05  CONS-SESSYOKU-E        PIC X(09)   VALUE "099800152".
      *        廃用症候群リハ開始日
               05  CONS-HAIYO-E           PIC X(09)   VALUE "099800192".
      *
           03  CONS-RIHA-SYSCD-R REDEFINES CONST-RIHA-SYSCD-VAL.
               05  CONS-RIHA-SYSCD     PIC X(09)   OCCURS 19.
           03  CONS-RIHA-SYSCD-MAX     PIC 9(03)   VALUE  19.
      *
      *****
      *    リハビリ手技料
           03  CONST-RIHA-COMMENT-VAL.
      *        心大血管疾患リハビリテーション料（１）
               05  CONS-SINDAI1-COMMENT   PIC X(09)   VALUE "180027410".
      *        心大血管疾患リハビリテーション料（２）
               05  CONS-SINDAI2-COMMENT   PIC X(09)   VALUE "180027510".
      *!       心大血管疾患リハビリテーション料（１）
               05  CONS-SINDAI7-COMMENT   PIC X(09)   VALUE "101800080".
      *!       心大血管疾患リハビリテーション料（２）
               05  CONS-SINDAI8-COMMENT   PIC X(09)   VALUE "101800090".
      *#       心大血管疾患リハビリテーション料（１）
               05  CONS-SINDAI9-COMMENT   PIC X(09)   VALUE "101800230".
      *#       心大血管疾患リハビリテーション料（２）
               05  CONS-SINDAI10-COMMENT  PIC X(09)   VALUE "101800240".
      *        呼吸器リハビリテーション料（１）
               05  CONS-KOKYUKI1-COMMENT  PIC X(09)   VALUE "180028010".
      *        呼吸器リハビリテーション料（２）
               05  CONS-KOKYUKI2-COMMENT  PIC X(09)   VALUE "180028110".
      *!       呼吸器リハビリテーション料（１）
               05  CONS-KOKYUKI7-COMMENT  PIC X(09)   VALUE "101800150".
      *!       呼吸器リハビリテーション料（２）
               05  CONS-KOKYUKI8-COMMENT  PIC X(09)   VALUE "101800160".
      *#       呼吸器リハビリテーション料（１）
               05  CONS-KOKYUKI9-COMMENT  PIC X(09)   VALUE "101800340".
      *#       呼吸器リハビリテーション料（２）
               05  CONS-KOKYUKI10-COMMENT PIC X(09)   VALUE "101800350".
      *        脳血管疾患等リハビリテーション料（１）
               05  CONS-NOUKE1-COMMENT    PIC X(09)   VALUE "180027610".
      *        脳血管疾患等リハビリテーション料（２）
               05  CONS-NOUKE2-COMMENT    PIC X(09)   VALUE "180027710".
      *        脳血管疾患等リハビリテーション料（３）
               05  CONS-NOUKE7-COMMENT    PIC X(09)   VALUE "180030810".
      *!       脳血管疾患等リハビリテーション料（１）
               05  CONS-NOUKE8-COMMENT    PIC X(09)   VALUE "101800100".
      *!       脳血管疾患等リハビリテーション料（２）
               05  CONS-NOUKE9-COMMENT    PIC X(09)   VALUE "101800110".
      *!       脳血管疾患等リハビリテーション料（３）
               05  CONS-NOUKE10-COMMENT   PIC X(09)   VALUE "101800120".
      *        脳血管疾患等リハビリテーション料（１）（廃用症候群）
               05  CONS-NOUKE11-COMMENT   PIC X(09)   VALUE "180032410".
      *        脳血管疾患等リハビリテーション料（２）（廃用症候群）
               05  CONS-NOUKE12-COMMENT   PIC X(09)   VALUE "180032510".
      *        脳血管疾患等リハビリテーション料（３）（廃用症候群）
               05  CONS-NOUKE13-COMMENT   PIC X(09)   VALUE "180032610".
      *        脳血管疾患等リハビリテーション料（１）（要介護・廃用症候群以外）
               05  CONS-NOUKE14-COMMENT   PIC X(09)   VALUE "180033910".
      *        脳血管疾患等リハビリテーション料（１）（要介護・廃用症候群）
               05  CONS-NOUKE15-COMMENT   PIC X(09)   VALUE "180034010".
      *        脳血管疾患等リハビリテーション料（２）（要介護・廃用症候群以外）
               05  CONS-NOUKE16-COMMENT   PIC X(09)   VALUE "180034110".
      *        脳血管疾患等リハビリテーション料（２）（要介護・廃用症候群）
               05  CONS-NOUKE17-COMMENT   PIC X(09)   VALUE "180034210".
      *        脳血管疾患等リハビリテーション料（３）（要介護・廃用症候群以外）
               05  CONS-NOUKE18-COMMENT   PIC X(09)   VALUE "180034310".
      *        脳血管疾患等リハビリテーション料（３）（要介護・廃用症候群）
               05  CONS-NOUKE19-COMMENT   PIC X(09)   VALUE "180034410".
      *#       脳血管疾患等リハビリテーション料（１）
               05  CONS-NOUKE20-COMMENT   PIC X(09)   VALUE "101800250".
      *#       脳血管疾患等リハビリテーション料（２）
               05  CONS-NOUKE21-COMMENT   PIC X(09)   VALUE "101800260".
      *#       脳血管疾患等リハビリテーション料（３）
               05  CONS-NOUKE22-COMMENT   PIC X(09)   VALUE "101800270".
      *        脳血管疾患等リハビリテーション料（１）（要介護・入院）
               05  CONS-NOUKE23-COMMENT   PIC X(09)   VALUE "180044310".
      *        脳血管疾患等リハビリテーション料（２）（要介護・入院）
               05  CONS-NOUKE24-COMMENT   PIC X(09)   VALUE "180044410".
      *        脳血管疾患等リハビリテーション料（３）（要介護・入院）
               05  CONS-NOUKE25-COMMENT   PIC X(09)   VALUE "180044510".
      *        脳血管疾患等リハビリテーション料１（要介・廃用症以外）基準不適合
               05  CONS-NOUKE26-COMMENT   PIC X(09)   VALUE "180043430".
      *        脳血管疾患等リハビリテーション料１（要介・廃用症）基準不適合
               05  CONS-NOUKE27-COMMENT   PIC X(09)   VALUE "180043530".
      *        脳血管疾患等リハビリテーション料２（要介・廃用症以外）基準不適合
               05  CONS-NOUKE28-COMMENT   PIC X(09)   VALUE "180043630".
      *        脳血管疾患等リハビリテーション料２（要介・廃用症）基準不適合
               05  CONS-NOUKE29-COMMENT   PIC X(09)   VALUE "180043730".
      *        脳血管疾患等リハビリテーション料３（要介・廃用症以外）基準不適合
               05  CONS-NOUKE30-COMMENT   PIC X(09)   VALUE "180043830".
      *        脳血管疾患等リハビリテーション料３（要介・廃用症）基準不適合
               05  CONS-NOUKE31-COMMENT   PIC X(09)   VALUE "180043930".
      *        脳血管疾患等リハビリテーション料（１）（リ減）
               05  CONS-NOUKE32-COMMENT   PIC X(09)   VALUE "180050330".
      *        脳血管疾患等リハビリテーション料（２）（リ減）
               05  CONS-NOUKE33-COMMENT   PIC X(09)   VALUE "180050430".
      *        脳血管疾患等リハビリテーション料（３）（リ減）
               05  CONS-NOUKE34-COMMENT   PIC X(09)   VALUE "180050530".
      *        脳血管疾患等リハビリテーション料（１）（要介護・入院）（リ減）
               05  CONS-NOUKE35-COMMENT   PIC X(09)   VALUE "180050630".
      *        脳血管疾患等リハビリテーション料（１）（要介護・入院外）（リ減）
               05  CONS-NOUKE36-COMMENT   PIC X(09)   VALUE "180050730".
      *        脳血管疾患等リハビリテーション料（２）（要介護・入院）（リ減）
               05  CONS-NOUKE37-COMMENT   PIC X(09)   VALUE "180050830".
      *        脳血管疾患等リハビリテーション料（２）（要介護・入院外）（リ減）
               05  CONS-NOUKE38-COMMENT   PIC X(09)   VALUE "180050930".
      *        脳血管疾患等リハビリテーション料（３）（要介護・入院）（リ減）
               05  CONS-NOUKE39-COMMENT   PIC X(09)   VALUE "180051030".
      *        脳血管疾患等リハビリテーション料（３）（要介護・入院外）（リ減）
               05  CONS-NOUKE40-COMMENT   PIC X(09)   VALUE "180051130".
      *        脳血管疾患等リハビリテーション料（１）（要介護）基準不適合・リ減
               05  CONS-NOUKE41-COMMENT   PIC X(09)   VALUE "180051230".
      *        脳血管疾患等リハビリテーション料（２）（要介護）基準不適合・リ減
               05  CONS-NOUKE42-COMMENT   PIC X(09)   VALUE "180051330".
      *        脳血管疾患等リハビリテーション料（３）（要介護）基準不適合・リ減
               05  CONS-NOUKE43-COMMENT   PIC X(09)   VALUE "180051430".
      *        運動器リハビリテーション料（１）
               05  CONS-UNDOKI1-COMMENT   PIC X(09)   VALUE "180027810".
      *        運動器リハビリテーション料（２）
               05  CONS-UNDOKI2-COMMENT   PIC X(09)   VALUE "180027910".
      *!       運動器リハビリテーション料（１）
               05  CONS-UNDOKI7-COMMENT   PIC X(09)   VALUE "101800130".
      *!       運動器リハビリテーション料（２）
               05  CONS-UNDOKI8-COMMENT   PIC X(09)   VALUE "101800140".
      *        運動器リハビリテーション料（１）
               05  CONS-UNDOKI9-COMMENT   PIC X(09)   VALUE "180032710".
      *!       運動器リハビリテーション料（１）
               05  CONS-UNDOKI10-COMMENT  PIC X(09)   VALUE "101800125".
      *        運動器リハビリテーション料（１）（要介護）
               05  CONS-UNDOKI11-COMMENT  PIC X(09)   VALUE "180034510".
      *        運動器リハビリテーション料（２）（要介護）
               05  CONS-UNDOKI12-COMMENT  PIC X(09)   VALUE "180034610".
      *        運動器リハビリテーション料（３）（要介護）
               05  CONS-UNDOKI13-COMMENT  PIC X(09)   VALUE "180034710".
      *#       運動器リハビリテーション料（１）
               05  CONS-UNDOKI14-COMMENT  PIC X(09)   VALUE "101800280".
      *#       運動器リハビリテーション料（２）
               05  CONS-UNDOKI15-COMMENT  PIC X(09)   VALUE "101800290".
      *#       運動器リハビリテーション料（３）
               05  CONS-UNDOKI16-COMMENT  PIC X(09)   VALUE "101800330".
      *        運動器リハビリテーション料（１）（要介護・入院）
               05  CONS-UNDOKI17-COMMENT  PIC X(09)   VALUE "180045810".
      *        運動器リハビリテーション料（２）（要介護・入院）
               05  CONS-UNDOKI18-COMMENT  PIC X(09)   VALUE "180045910".
      *        運動器リハビリテーション料（３）（要介護・入院）
               05  CONS-UNDOKI19-COMMENT  PIC X(09)   VALUE "180046010".
      *#       運動器リハビリテーション料（１）（１．５倍）
               05  CONS-UNDOKI20-COMMENT  PIC X(09)   VALUE "101801070".
      *#       運動器リハビリテーション料（３）（１．５倍）
               05  CONS-UNDOKI21-COMMENT  PIC X(09)   VALUE "101801080".
      *        運動器リハビリテーション料（１）（要介護）（施設基準不適合）
               05  CONS-UNDOKI22-COMMENT  PIC X(09)   VALUE "180044030".
      *        運動器リハビリテーション料（２）（要介護）（施設基準不適合）
               05  CONS-UNDOKI23-COMMENT  PIC X(09)   VALUE "180044130".
      *        運動器リハビリテーション料（３）（要介護）（施設基準不適合）
               05  CONS-UNDOKI24-COMMENT  PIC X(09)   VALUE "180044230".
      *        運動器リハビリテーション料（１）（リ減）
               05  CONS-UNDOKI25-COMMENT  PIC X(09)   VALUE "180052730".
      *        運動器リハビリテーション料（２）（リ減）
               05  CONS-UNDOKI26-COMMENT  PIC X(09)   VALUE "180052830".
      *        運動器リハビリテーション料（３）（リ減）
               05  CONS-UNDOKI27-COMMENT  PIC X(09)   VALUE "180052930".
      *        運動器リハビリテーション料（１）（要介護・入院）（リ減）
               05  CONS-UNDOKI28-COMMENT  PIC X(09)   VALUE "180053030".
      *        運動器リハビリテーション料（１）（要介護・入院外）（リ減）
               05  CONS-UNDOKI29-COMMENT  PIC X(09)   VALUE "180053130".
      *        運動器リハビリテーション料（２）（要介護・入院）（リ減）
               05  CONS-UNDOKI30-COMMENT  PIC X(09)   VALUE "180053230".
      *        運動器リハビリテーション料（２）（要介護・入院外）（リ減）
               05  CONS-UNDOKI31-COMMENT  PIC X(09)   VALUE "180053330".
      *        運動器リハビリテーション料（３）（要介護・入院）（リ減）
               05  CONS-UNDOKI32-COMMENT  PIC X(09)   VALUE "180053430".
      *        運動器リハビリテーション料（３）（要介護・入院外）（リ減）
               05  CONS-UNDOKI33-COMMENT  PIC X(09)   VALUE "180053530".
      *        運動器リハビリテーション料（１）（要介護）基準不適合・リ減
               05  CONS-UNDOKI34-COMMENT  PIC X(09)   VALUE "180053630".
      *        運動器リハビリテーション料（２）（要介護）基準不適合・リ減
               05  CONS-UNDOKI35-COMMENT  PIC X(09)   VALUE "180053730".
      *        運動器リハビリテーション料（３）（要介護）基準不適合・リ減
               05  CONS-UNDOKI36-COMMENT  PIC X(09)   VALUE "180053830".
      *        難病患者リハビリテーション料
               05  CONS-NANBYO-COMMENT    PIC X(09)   VALUE "180017910".
      *        障害児（者）リハビリテーション料（６歳未満）
               05  CONS-SYOGAI1-COMMENT   PIC X(09)   VALUE "180028210".
      *        障害児（者）リハビリテーション料（６歳以上１８歳未満）
               05  CONS-SYOGAI2-COMMENT   PIC X(09)   VALUE "180028310".
      *        障害児（者）リハビリテーション料（１８歳以上）
               05  CONS-SYOGAI3-COMMENT   PIC X(09)   VALUE "180028410".
      *        摂食機能療法
               05  CONS-SESYOKU-COMMENT   PIC X(09)   VALUE "180016610".
      *        摂食機能療法（３０分未満）
               05  CONS-SESYOKU2-COMMENT  PIC X(09)   VALUE "180054310".
      *        廃用症候群リハビリテーション料（１）（１単位）
               05  CONS-HAIYO1-COMMENT    PIC X(09)   VALUE "180044610".
      *        廃用症候群リハビリテーション料（２）（１単位）
               05  CONS-HAIYO2-COMMENT    PIC X(09)   VALUE "180044710".
      *        廃用症候群リハビリテーション料（３）（１単位）
               05  CONS-HAIYO3-COMMENT    PIC X(09)   VALUE "180044810".
      *        廃用症候群リハビリテーション料（１）（要介護・入院）
               05  CONS-HAIYO4-COMMENT    PIC X(09)   VALUE "180044910".
      *        廃用症候群リハビリテーション料（１）（要介護・入院外）
               05  CONS-HAIYO5-COMMENT    PIC X(09)   VALUE "180045010".
      *        廃用症候群リハビリテーション料（２）（要介護・入院）
               05  CONS-HAIYO6-COMMENT    PIC X(09)   VALUE "180045110".
      *        廃用症候群リハビリテーション料（２）（要介護・入院外）
               05  CONS-HAIYO7-COMMENT    PIC X(09)   VALUE "180045210".
      *        廃用症候群リハビリテーション料（３）（要介護・入院）
               05  CONS-HAIYO8-COMMENT    PIC X(09)   VALUE "180045310".
      *        廃用症候群リハビリテーション料（３）（要介護・入院外）
               05  CONS-HAIYO9-COMMENT    PIC X(09)   VALUE "180045410".
      *        廃用症候群リハビリテーション料１（要介護）基準不適合
               05  CONS-HAIYO10-COMMENT   PIC X(09)   VALUE "180045530".
      *        廃用症候群リハビリテーション料２（要介護）基準不適合
               05  CONS-HAIYO11-COMMENT   PIC X(09)   VALUE "180045630".
      *        廃用症候群リハビリテーション料３（要介護）基準不適合
               05  CONS-HAIYO12-COMMENT   PIC X(09)   VALUE "180045730".
      *#       廃用症候群リハビリテーション料（１）（１単位）
               05  CONS-HAIYO13-COMMENT   PIC X(09)   VALUE "101801110".
      *#       廃用症候群リハビリテーション料（２）（１単位）
               05  CONS-HAIYO14-COMMENT   PIC X(09)   VALUE "101801120".
      *#       廃用症候群リハビリテーション料（３）（１単位）
               05  CONS-HAIYO15-COMMENT   PIC X(09)   VALUE "101801130".
      *        廃用症候群リハビリテーション料（１）（リ減）
               05  CONS-HAIYO16-COMMENT   PIC X(09)   VALUE "180051530".
      *        廃用症候群リハビリテーション料（２）（リ減）
               05  CONS-HAIYO17-COMMENT   PIC X(09)   VALUE "180051630".
      *        廃用症候群リハビリテーション料（３）（リ減）
               05  CONS-HAIYO18-COMMENT   PIC X(09)   VALUE "180051730".
      *        廃用症候群リハビリテーション料（１）（要介護・入院）（リ減）
               05  CONS-HAIYO19-COMMENT   PIC X(09)   VALUE "180051830".
      *        廃用症候群リハビリテーション料（１）（要介護・入院外）（リ減）
               05  CONS-HAIYO20-COMMENT   PIC X(09)   VALUE "180051930".
      *        廃用症候群リハビリテーション料（２）（要介護・入院）（リ減）
               05  CONS-HAIYO21-COMMENT   PIC X(09)   VALUE "180052030".
      *        廃用症候群リハビリテーション料（２）（要介護・入院外）（リ減）
               05  CONS-HAIYO22-COMMENT   PIC X(09)   VALUE "180052130".
      *        廃用症候群リハビリテーション料（３）（要介護・入院）（リ減）
               05  CONS-HAIYO23-COMMENT   PIC X(09)   VALUE "180052230".
      *        廃用症候群リハビリテーション料（３）（要介護・入院外）（リ減）
               05  CONS-HAIYO24-COMMENT   PIC X(09)   VALUE "180052330".
      *        廃用症候群リハビリテーション料（１）（要介護）基準不適合・リ減
               05  CONS-HAIYO25-COMMENT   PIC X(09)   VALUE "180052430".
      *        廃用症候群リハビリテーション料（２）（要介護）基準不適合・リ減
               05  CONS-HAIYO26-COMMENT   PIC X(09)   VALUE "180052530".
      *        廃用症候群リハビリテーション料（３）（要介護）基準不適合・リ減
               05  CONS-HAIYO27-COMMENT   PIC X(09)   VALUE "180052630".
      *        初期加算（リハビリテーション料）
               05  CONS-RIHAKSN1-COMMENT  PIC X(09)   VALUE "180033870".
      *        早期リハビリテーション加算
               05  CONS-RIHAKSN2-COMMENT  PIC X(09)   VALUE "180030770".
      *
           03  CONS-RIHA-COMMENT-R REDEFINES CONST-RIHA-COMMENT-VAL.
               05  CONS-RIHA-COMMENT      PIC X(09)   OCCURS 118.
           03  CONS-RIHA-COMMENT-MAX      PIC 9(03)   VALUE  118.
      *
      *****
      *    リハビリ開始日指定コード
           03  CONST-RIHA-SYSCD2-VAL.
      *        リハビリ開始日指定（心大血管疾患）
               05  CONS-SINDAI-SITEI      PIC X(09)   VALUE "099999935".
      *        リハビリ開始日指定（脳血管疾患等）
               05  CONS-NOUKE-SITEI       PIC X(09)   VALUE "099999936".
      *        リハビリ開始日指定（運動器）
               05  CONS-UNDOKI-SITEI      PIC X(09)   VALUE "099999937".
      *        リハビリ開始日指定（呼吸器）
               05  CONS-KOKYUKI-SITEI     PIC X(09)   VALUE "099999938".
      *        リハビリ開始日指定（摂食機能療法）
               05  CONS-SESSYOKU-SITEI    PIC X(09)   VALUE "099999939".
      *        リハビリ開始日指定（廃用症候群）
               05  CONS-HAIYO-SITEI       PIC X(09)   VALUE "099999940".
      *        リハビリ開始日指定（心大早期・初期加算）
               05  CONS-SINDAI-K-SITEI    PIC X(09)   VALUE "099999941".
      *        リハビリ開始日指定（呼吸器早期・初期加算）
               05  CONS-KOKYUKI-K-SITEI   PIC X(09)   VALUE "099999942".
      *        リハビリ開始日指定（廃用早期・初期加算）
               05  CONS-HAIYO-K-SITEI     PIC X(09)   VALUE "099999943".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    共通領域
           COPY    "MCPAREA".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    算定履歴付加
       01  SANTEIPLUS-REC.
           COPY    "CPSANTEIPLUS.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSRIHASTDAY.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSRIHASTDAYAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           MOVE    ZERO                TO  LNK-RIHASTDAY-RC
           INITIALIZE                      LNK-RIHASTDAY-OUT-AREA
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  WRK-FREE-AREA
           INITIALIZE                  WRK-RIHASANTEI-AREA
      *
      *    パラメタチェック
           PERFORM     100-PRM-CHK-SEC
           IF    LNK-RIHASTDAY-RC  =  ZERO
            AND  LNK-RIHASTDAY-HOSPNUM NOT =   ZERO
             PERFORM   200-MAIN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    パラメタチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    医療機関番号チェック(=ZEROの時は一覧返却)
           IF      LNK-RIHASTDAY-HOSPNUM   =  ZERO
      *
               PERFORM VARYING IDX1    FROM   1 BY 1
                       UNTIL ( IDX1    >   CONS-RIHA-SYSCD-MAX )
                   MOVE    CONS-RIHA-SYSCD (IDX1)
                                TO     LNK-RIHASTDAY-SYSCDLST (IDX1)
               END-PERFORM
               MOVE    CONS-RIHA-SYSCD-MAX
                                TO     LNK-RIHASTDAY-SYSCDLST-IDX
      *
               PERFORM VARYING IDX1    FROM   1 BY 1
                       UNTIL ( IDX1    >   CONS-RIHA-COMMENT-MAX )
                   MOVE    CONS-RIHA-COMMENT (IDX1)
                                TO     LNK-RIHASTDAY-SRYCDLST (IDX1)
               END-PERFORM
               MOVE    CONS-RIHA-COMMENT-MAX
                                TO     LNK-RIHASTDAY-SRYCDLST-IDX
      *
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    患者ＩＤチェック
           IF      LNK-RIHASTDAY-PTID      =  ZERO
               MOVE    1                 TO  LNK-RIHASTDAY-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    診療年月チェック
           IF      LNK-RIHASTDAY-SRYYM     =  SPACE
           OR      LNK-RIHASTDAY-SRYYM     < "200804"
               MOVE    2                 TO  LNK-RIHASTDAY-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    保険組合せチェック
           IF      LNK-RIHASTDAY-HKNCOMBI  =  ZERO
               MOVE    3                 TO  LNK-RIHASTDAY-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           MOVE   ZERO                   TO  FLG-HKNCOMBI-SET
           INITIALIZE                        HKNCOMBI-REC
           MOVE   LNK-RIHASTDAY-HOSPNUM  TO  COMB-HOSPNUM
           MOVE   LNK-RIHASTDAY-PTID     TO  COMB-PTID
           MOVE   LNK-RIHASTDAY-HKNCOMBI TO  COMB-HKNCOMBINUM
           MOVE   HKNCOMBI-REC           TO  MCPDATA-REC
           MOVE   "tbl_hkncombi"         TO  MCP-TABLE
           MOVE   "key"                  TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC  =  ZERO
             MOVE   "tbl_hkncombi"       TO  MCP-TABLE
             MOVE   "key"                TO  MCP-PATHNAME
             PERFORM 900-HKNCOMBI-READ-SEC
             IF      FLG-HKNCOMBI  =  ZERO
               IF  (COMB-HKNNUM      =  "971" OR "973" OR "975")
               OR  (COMB-KOH1HKNNUM  =  "970"                  )
                 MOVE  1                 TO  FLG-HKNCOMBI-SET
               END-IF
             ELSE
               MOVE    3                 TO  LNK-RIHASTDAY-RC
             END-IF
           ELSE
               MOVE    3                 TO  LNK-RIHASTDAY-RC
           END-IF
           MOVE   "tbl_hkncombi"         TO  MCP-TABLE
           MOVE   "key"                  TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主取得
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *    リハビリ算定情報の取得
           PERFORM     210-RIHASANTEI-HENSYU-SEC
      *
      *    リハビリ開始日指定コードの取得
           PERFORM     230-RIHACDGET-HENSYU-SEC
      *
      *    リハビリ開始日の取得・コメント編集
           PERFORM     220-RIHAKAISHI-HENSYU-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ算定情報の取得
      *****************************************************************
       210-RIHASANTEI-HENSYU-SEC          SECTION.
      *
           MOVE      1      TO     FLG-SET
           MOVE     "80"    TO     WRK-SRYKBN
           PERFORM   800-SRYACCT-DBSELECT-SEC
      *
           .
       210-RIHASANTEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ算定情報の集計
      *****************************************************************
       2101-RIHASANTEI-SYUKEI-SEC          SECTION.
      *
           PERFORM  VARYING  IDX12  FROM  1  BY  1
                     UNTIL  (IDX12            >   5   )
                       OR   (SRY-SRYCD(IDX12) =  SPACE)
      *
             MOVE    ZERO        TO    IDX17
             MOVE    SPACE       TO    WRK-SRYCD3
             IF   SRY-SRYCD(IDX12)  =    CONS-SINDAI1-COMMENT
                                   OR    CONS-SINDAI2-COMMENT
                 PERFORM  VARYING  IDX3  FROM  1  BY  1
                           UNTIL   IDX3   >   31
                   IF   ACCT-DAY(1 IDX3)  NOT = ZERO
                     MOVE   1    TO    WRK-RIHASANTEI-FLG(1)
                     MOVE   1    TO    WRK-RIHASANTEI-DAY(1 IDX3)
                     MOVE   1    TO    IDX17
                     MOVE   1    TO    IDX19
                     MOVE   SRY-SRYCD(IDX12)
                                 TO    WRK-SRYCD3
                   END-IF
                 END-PERFORM
             END-IF
             IF   SRY-SRYCD(IDX12)  =    CONS-NOUKE1-COMMENT
                                   OR    CONS-NOUKE2-COMMENT
                                   OR    CONS-NOUKE7-COMMENT
                                   OR    CONS-NOUKE11-COMMENT
                                   OR    CONS-NOUKE12-COMMENT
                                   OR    CONS-NOUKE13-COMMENT
                                   OR    CONS-NOUKE14-COMMENT
                                   OR    CONS-NOUKE15-COMMENT
                                   OR    CONS-NOUKE16-COMMENT
                                   OR    CONS-NOUKE17-COMMENT
                                   OR    CONS-NOUKE18-COMMENT
                                   OR    CONS-NOUKE19-COMMENT
                                   OR    CONS-NOUKE23-COMMENT
                                   OR    CONS-NOUKE24-COMMENT
                                   OR    CONS-NOUKE25-COMMENT
                                   OR    CONS-NOUKE26-COMMENT
                                   OR    CONS-NOUKE27-COMMENT
                                   OR    CONS-NOUKE28-COMMENT
                                   OR    CONS-NOUKE29-COMMENT
                                   OR    CONS-NOUKE30-COMMENT
                                   OR    CONS-NOUKE31-COMMENT
                                   OR    CONS-NOUKE32-COMMENT
                                   OR    CONS-NOUKE33-COMMENT
                                   OR    CONS-NOUKE34-COMMENT
                                   OR    CONS-NOUKE35-COMMENT
                                   OR    CONS-NOUKE36-COMMENT
                                   OR    CONS-NOUKE37-COMMENT
                                   OR    CONS-NOUKE38-COMMENT
                                   OR    CONS-NOUKE39-COMMENT
                                   OR    CONS-NOUKE40-COMMENT
                                   OR    CONS-NOUKE41-COMMENT
                                   OR    CONS-NOUKE42-COMMENT
                                   OR    CONS-NOUKE43-COMMENT
                 PERFORM  VARYING  IDX3  FROM  1  BY  1
                           UNTIL   IDX3   >   31
                   IF   ACCT-DAY(1 IDX3)  NOT = ZERO
                     MOVE   1    TO    WRK-RIHASANTEI-FLG(3)
                     MOVE   1    TO    WRK-RIHASANTEI-DAY(3 IDX3)
                     MOVE   3    TO    IDX17
                     MOVE   3    TO    IDX19
                     MOVE   SRY-SRYCD(IDX12)
                                 TO    WRK-SRYCD3
                   END-IF
                 END-PERFORM
             END-IF
             IF   SRY-SRYCD(IDX12)  =    CONS-HAIYO1-COMMENT
                                   OR    CONS-HAIYO2-COMMENT
                                   OR    CONS-HAIYO3-COMMENT
                                   OR    CONS-HAIYO4-COMMENT
                                   OR    CONS-HAIYO5-COMMENT
                                   OR    CONS-HAIYO6-COMMENT
                                   OR    CONS-HAIYO7-COMMENT
                                   OR    CONS-HAIYO8-COMMENT
                                   OR    CONS-HAIYO9-COMMENT
                                   OR    CONS-HAIYO10-COMMENT
                                   OR    CONS-HAIYO11-COMMENT
                                   OR    CONS-HAIYO12-COMMENT
                                   OR    CONS-HAIYO13-COMMENT
                                   OR    CONS-HAIYO14-COMMENT
                                   OR    CONS-HAIYO15-COMMENT
                                   OR    CONS-HAIYO16-COMMENT
                                   OR    CONS-HAIYO17-COMMENT
                                   OR    CONS-HAIYO18-COMMENT
                                   OR    CONS-HAIYO19-COMMENT
                                   OR    CONS-HAIYO20-COMMENT
                                   OR    CONS-HAIYO21-COMMENT
                                   OR    CONS-HAIYO22-COMMENT
                                   OR    CONS-HAIYO23-COMMENT
                                   OR    CONS-HAIYO24-COMMENT
                                   OR    CONS-HAIYO25-COMMENT
                                   OR    CONS-HAIYO26-COMMENT
                                   OR    CONS-HAIYO27-COMMENT
                 PERFORM  VARYING  IDX3  FROM  1  BY  1
                           UNTIL   IDX3   >   31
                   IF   ACCT-DAY(1 IDX3)  NOT = ZERO
                     MOVE   1    TO    WRK-RIHASANTEI-FLG(4)
                     MOVE   1    TO    WRK-RIHASANTEI-DAY(4 IDX3)
                     MOVE   4    TO    IDX17
                     MOVE   4    TO    IDX19
                     MOVE   SRY-SRYCD(IDX12)
                                 TO    WRK-SRYCD3
                   END-IF
                 END-PERFORM
             END-IF
             IF   SRY-SRYCD(IDX12)  =    CONS-UNDOKI1-COMMENT
                                   OR    CONS-UNDOKI2-COMMENT
                                   OR    CONS-UNDOKI9-COMMENT
                                   OR    CONS-UNDOKI11-COMMENT
                                   OR    CONS-UNDOKI12-COMMENT
                                   OR    CONS-UNDOKI13-COMMENT
                                   OR    CONS-UNDOKI17-COMMENT
                                   OR    CONS-UNDOKI18-COMMENT
                                   OR    CONS-UNDOKI19-COMMENT
                                   OR    CONS-UNDOKI22-COMMENT
                                   OR    CONS-UNDOKI23-COMMENT
                                   OR    CONS-UNDOKI24-COMMENT
                                   OR    CONS-UNDOKI25-COMMENT
                                   OR    CONS-UNDOKI26-COMMENT
                                   OR    CONS-UNDOKI27-COMMENT
                                   OR    CONS-UNDOKI28-COMMENT
                                   OR    CONS-UNDOKI29-COMMENT
                                   OR    CONS-UNDOKI30-COMMENT
                                   OR    CONS-UNDOKI31-COMMENT
                                   OR    CONS-UNDOKI32-COMMENT
                                   OR    CONS-UNDOKI33-COMMENT
                                   OR    CONS-UNDOKI34-COMMENT
                                   OR    CONS-UNDOKI35-COMMENT
                                   OR    CONS-UNDOKI36-COMMENT
                 PERFORM  VARYING  IDX3  FROM  1  BY  1
                           UNTIL   IDX3   >   31
                   IF   ACCT-DAY(1 IDX3)  NOT = ZERO
                     MOVE   1    TO    WRK-RIHASANTEI-FLG(6)
                     MOVE   1    TO    WRK-RIHASANTEI-DAY(6 IDX3)
                     MOVE   6    TO    IDX17
                     MOVE   6    TO    IDX19
                     MOVE   SRY-SRYCD(IDX12)
                                 TO    WRK-SRYCD3
                   END-IF
                 END-PERFORM
             END-IF
             IF   SRY-SRYCD(IDX12)  =    CONS-KOKYUKI1-COMMENT
                                   OR    CONS-KOKYUKI2-COMMENT
                 PERFORM  VARYING  IDX3  FROM  1  BY  1
                           UNTIL   IDX3   >   31
                   IF   ACCT-DAY(1 IDX3)  NOT = ZERO
                     MOVE   1    TO    WRK-RIHASANTEI-FLG(7)
                     MOVE   1    TO    WRK-RIHASANTEI-DAY(7 IDX3)
                     MOVE   7    TO    IDX17
                     MOVE   7    TO    IDX19
                     MOVE   SRY-SRYCD(IDX12)
                                 TO    WRK-SRYCD3
                   END-IF
                 END-PERFORM
             END-IF
             IF   SRY-SRYCD(IDX12)  =    CONS-SESYOKU-COMMENT
                                   OR    CONS-SESYOKU2-COMMENT
                 PERFORM  VARYING  IDX3  FROM  1  BY  1
                           UNTIL   IDX3   >   31
                   IF   ACCT-DAY(1 IDX3)  NOT = ZERO
                     MOVE   1    TO    WRK-RIHASANTEI-FLG(9)
                     MOVE   1    TO    WRK-RIHASANTEI-DAY(9 IDX3)
                     MOVE   9    TO    IDX17
                     MOVE   9    TO    IDX19
                     MOVE   SRY-SRYCD(IDX12)
                                 TO    WRK-SRYCD3
                   END-IF
                 END-PERFORM
             END-IF
             IF  (SRY-SRYCD(IDX12)  =    CONS-NANBYO-COMMENT)
             AND (ACCT-SRYYM       <=    "201803"           )
                 PERFORM  VARYING  IDX3  FROM  1  BY  1
                           UNTIL   IDX3   >   31
                   IF   ACCT-DAY(1 IDX3)  NOT = ZERO
                     MOVE   1    TO    WRK-RIHASANTEI-FLG(10)
                     MOVE   1    TO    WRK-RIHASANTEI-DAY(10 IDX3)
                     MOVE   10   TO    IDX17
                     MOVE   10   TO    IDX19
                     MOVE   SRY-SRYCD(IDX12)
                                 TO    WRK-SRYCD3
                   END-IF
                 END-PERFORM
             END-IF
             IF  (SRY-SRYCD(IDX12)  =    CONS-SYOGAI1-COMMENT
                                   OR    CONS-SYOGAI2-COMMENT
                                   OR    CONS-SYOGAI3-COMMENT)
             AND (ACCT-SRYYM       <=    "201803"            )
                 PERFORM  VARYING  IDX3  FROM  1  BY  1
                           UNTIL   IDX3   >   31
                   IF   ACCT-DAY(1 IDX3)  NOT = ZERO
                     MOVE   1    TO    WRK-RIHASANTEI-FLG(11)
                     MOVE   1    TO    WRK-RIHASANTEI-DAY(11 IDX3)
                     MOVE   11   TO    IDX17
                     MOVE   11   TO    IDX19
                     MOVE   SRY-SRYCD(IDX12)
                                 TO    WRK-SRYCD3
                   END-IF
                 END-PERFORM
             END-IF
      *
             IF      ACCT-SRYYM   >=   "201307"
               IF SRY-SRYCD(IDX12)  =    CONS-SINDAI9-COMMENT
                                   OR    CONS-SINDAI10-COMMENT
                 PERFORM  VARYING  IDX3  FROM  1  BY  1
                           UNTIL   IDX3   >   31
                   IF   ACCT-DAY(1 IDX3)  NOT = ZERO
                     MOVE   1    TO    WRK-RIHASANTEI-FLG(1)
                     MOVE   1    TO    WRK-RIHASANTEI-DAY(1 IDX3)
                     MOVE   1    TO    IDX17
                     MOVE   1    TO    IDX19
                     MOVE   SRY-SRYCD(IDX12)
                                 TO    WRK-SRYCD3
                   END-IF
                 END-PERFORM
               END-IF
               IF SRY-SRYCD(IDX12)  =    CONS-NOUKE20-COMMENT
                                   OR    CONS-NOUKE21-COMMENT
                                   OR    CONS-NOUKE22-COMMENT
                 PERFORM  VARYING  IDX3  FROM  1  BY  1
                           UNTIL   IDX3   >   31
                   IF   ACCT-DAY(1 IDX3)  NOT = ZERO
                     MOVE   1    TO    WRK-RIHASANTEI-FLG(3)
                     MOVE   1    TO    WRK-RIHASANTEI-DAY(3 IDX3)
                     MOVE   3    TO    IDX17
                     MOVE   3    TO    IDX19
                     MOVE   SRY-SRYCD(IDX12)
                                 TO    WRK-SRYCD3
                   END-IF
                 END-PERFORM
               END-IF
               IF SRY-SRYCD(IDX12)  =    CONS-UNDOKI14-COMMENT
                                   OR    CONS-UNDOKI15-COMMENT
                                   OR    CONS-UNDOKI16-COMMENT
                                   OR    CONS-UNDOKI20-COMMENT
                                   OR    CONS-UNDOKI21-COMMENT
                 PERFORM  VARYING  IDX3  FROM  1  BY  1
                           UNTIL   IDX3   >   31
                   IF   ACCT-DAY(1 IDX3)  NOT = ZERO
                     MOVE   1    TO    WRK-RIHASANTEI-FLG(6)
                     MOVE   1    TO    WRK-RIHASANTEI-DAY(6 IDX3)
                     MOVE   6    TO    IDX17
                     MOVE   6    TO    IDX19
                     MOVE   SRY-SRYCD(IDX12)
                                 TO    WRK-SRYCD3
                   END-IF
                 END-PERFORM
               END-IF
               IF SRY-SRYCD(IDX12)  =    CONS-KOKYUKI9-COMMENT
                                   OR    CONS-KOKYUKI10-COMMENT
                 PERFORM  VARYING  IDX3  FROM  1  BY  1
                           UNTIL   IDX3   >   31
                   IF   ACCT-DAY(1 IDX3)  NOT = ZERO
                     MOVE   1    TO    WRK-RIHASANTEI-FLG(7)
                     MOVE   1    TO    WRK-RIHASANTEI-DAY(7 IDX3)
                     MOVE   7    TO    IDX17
                     MOVE   7    TO    IDX19
                     MOVE   SRY-SRYCD(IDX12)
                                 TO    WRK-SRYCD3
                   END-IF
                 END-PERFORM
               END-IF
             ELSE
               IF SRY-SRYCD(IDX12)  =    CONS-SINDAI7-COMMENT
                                   OR    CONS-SINDAI8-COMMENT
                 PERFORM  VARYING  IDX3  FROM  1  BY  1
                           UNTIL   IDX3   >   31
                   IF   ACCT-DAY(1 IDX3)  NOT = ZERO
                     MOVE   1    TO    WRK-RIHASANTEI-FLG(1)
                     MOVE   1    TO    WRK-RIHASANTEI-DAY(1 IDX3)
                     MOVE   1    TO    IDX17
                     MOVE   1    TO    IDX19
                     MOVE   SRY-SRYCD(IDX12)
                                 TO    WRK-SRYCD3
                   END-IF
                 END-PERFORM
               END-IF
               IF SRY-SRYCD(IDX12)  =    CONS-NOUKE8-COMMENT
                                   OR    CONS-NOUKE9-COMMENT
                                   OR    CONS-NOUKE10-COMMENT
                 PERFORM  VARYING  IDX3  FROM  1  BY  1
                           UNTIL   IDX3   >   31
                   IF   ACCT-DAY(1 IDX3)  NOT = ZERO
                     MOVE   1    TO    WRK-RIHASANTEI-FLG(3)
                     MOVE   1    TO    WRK-RIHASANTEI-DAY(3 IDX3)
                     MOVE   3    TO    IDX17
                     MOVE   3    TO    IDX19
                     MOVE   SRY-SRYCD(IDX12)
                                 TO    WRK-SRYCD3
                   END-IF
                 END-PERFORM
               END-IF
               IF SRY-SRYCD(IDX12)  =    CONS-UNDOKI7-COMMENT
                                   OR    CONS-UNDOKI8-COMMENT
                                   OR    CONS-UNDOKI10-COMMENT
                 PERFORM  VARYING  IDX3  FROM  1  BY  1
                           UNTIL   IDX3   >   31
                   IF   ACCT-DAY(1 IDX3)  NOT = ZERO
                     MOVE   1    TO    WRK-RIHASANTEI-FLG(6)
                     MOVE   1    TO    WRK-RIHASANTEI-DAY(6 IDX3)
                     MOVE   6    TO    IDX17
                     MOVE   6    TO    IDX19
                     MOVE   SRY-SRYCD(IDX12)
                                 TO    WRK-SRYCD3
                   END-IF
                 END-PERFORM
               END-IF
               IF SRY-SRYCD(IDX12)  =    CONS-KOKYUKI7-COMMENT
                                   OR    CONS-KOKYUKI8-COMMENT
                 PERFORM  VARYING  IDX3  FROM  1  BY  1
                           UNTIL   IDX3   >   31
                   IF   ACCT-DAY(1 IDX3)  NOT = ZERO
                     MOVE   1    TO    WRK-RIHASANTEI-FLG(7)
                     MOVE   1    TO    WRK-RIHASANTEI-DAY(7 IDX3)
                     MOVE   7    TO    IDX17
                     MOVE   7    TO    IDX19
                     MOVE   SRY-SRYCD(IDX12)
                                 TO    WRK-SRYCD3
                   END-IF
                 END-PERFORM
               END-IF
             END-IF
      *
             IF   SRY-SRYCD(IDX12)  =    CONS-RIHAKSN1-COMMENT
                                   OR    CONS-RIHAKSN2-COMMENT
               EVALUATE  IDX19
                 WHEN      1
                   PERFORM  VARYING  IDX3  FROM  1  BY  1
                             UNTIL   IDX3   >   31
                     IF   ACCT-DAY(1 IDX3)  NOT = ZERO
                       MOVE   1    TO    WRK-RIHASANTEI-FLG(2)
                       MOVE   1    TO    WRK-RIHASANTEI-DAY(2 IDX3)
                       MOVE   2    TO    IDX17
                       MOVE   SRY-SRYCD(IDX12)
                                   TO    WRK-SRYCD3
                     END-IF
                   END-PERFORM
                 WHEN      4
                   PERFORM  VARYING  IDX3  FROM  1  BY  1
                             UNTIL   IDX3   >   31
                     IF   ACCT-DAY(1 IDX3)  NOT = ZERO
                       MOVE   1    TO    WRK-RIHASANTEI-FLG(5)
                       MOVE   1    TO    WRK-RIHASANTEI-DAY(5 IDX3)
                       MOVE   5    TO    IDX17
                       MOVE   SRY-SRYCD(IDX12)
                                   TO    WRK-SRYCD3
                     END-IF
                   END-PERFORM
                 WHEN      7
                   PERFORM  VARYING  IDX3  FROM  1  BY  1
                             UNTIL   IDX3   >   31
                     IF   ACCT-DAY(1 IDX3)  NOT = ZERO
                       MOVE   1    TO    WRK-RIHASANTEI-FLG(8)
                       MOVE   1    TO    WRK-RIHASANTEI-DAY(8 IDX3)
                       MOVE   8    TO    IDX17
                       MOVE   SRY-SRYCD(IDX12)
                                   TO    WRK-SRYCD3
                     END-IF
                   END-PERFORM
               END-EVALUATE
             END-IF
      *
             IF    IDX17  NOT =  ZERO
               PERFORM  21011-RIHA-SRYCD-HENSYU-SEC
             END-IF
      *
           END-PERFORM
      *
           .
       2101-RIHASANTEI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ診療コード編集
      *****************************************************************
       21011-RIHA-SRYCD-HENSYU-SEC          SECTION.
      *
           MOVE     ZERO       TO   FLG-ARI
           PERFORM  VARYING  IDX18  FROM  1  BY  1
                     UNTIL  (IDX18                             >  50  )
                      OR    (WRK-RIHASANTEI-SRYCD(IDX17 IDX18) = SPACE)
                      OR    (FLG-ARI                           =   1  )
             IF  WRK-SRYCD3  =  WRK-RIHASANTEI-SRYCD(IDX17 IDX18)
                MOVE     1     TO   FLG-ARI
             END-IF
           END-PERFORM
           IF    FLG-ARI  =  ZERO
           AND   IDX18   <=   50
             MOVE  WRK-SRYCD3  TO   WRK-RIHASANTEI-SRYCD(IDX17 IDX18)
           END-IF
      *
           .
       21011-RIHA-SRYCD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ開始日の取得・コメント編集
      *****************************************************************
       220-RIHAKAISHI-HENSYU-SEC          SECTION.
      *
      *    心大血管疾患
           IF   WRK-RIHASANTEI-FLG(1)  =  1
             MOVE      1               TO   IDY
             MOVE      ZERO            TO   FLG-COMOKIKAE
             MOVE      CONS-SINDAI     TO   WRK-SRYCD
             MOVE      CONS-SINDAI-E   TO   WRK-SRYCD2
             MOVE      WRK-RIHASANTEI-DAY-TBL(1)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
             MOVE      WRK-RIHASANTEI-DAY-TBL2(1)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
             MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
             IF   FLG-HKNCOMBI-SET  =  1
               MOVE    LNK-RIHASTDAY-HKNCOMBI
                                       TO   WRK-RIHA-HKNCOMBI
             END-IF
             PERFORM   2201-RIHAKAISHI-HANTEI-SEC
             IF   FLG-KYUSEI-COMMENT  =  1
               PERFORM 2202-RIHAKAISHI-COMMENT-SEC
               MOVE    WRK-RIHASANTEI-SRYCD-TBL(1)
                                       TO   LNK-RIHASTDAY-OUT-OCC3(1)
             END-IF
           END-IF
      *
      *    心大血管疾患の早期・初期リハ加算
           IF   WRK-RIHASANTEI-FLG(2)  =  1
             MOVE      2               TO   IDY
             MOVE      1               TO   FLG-COMOKIKAE
             MOVE      CONS-SINDAI-K   TO   WRK-SRYCD
             MOVE      "999999999"     TO   WRK-SRYCD2
             MOVE      WRK-RIHASANTEI-DAY-TBL(2)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
             MOVE      WRK-RIHASANTEI-DAY-TBL2(2)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
             MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
             IF   FLG-HKNCOMBI-SET  =  1
               MOVE    LNK-RIHASTDAY-HKNCOMBI
                                       TO   WRK-RIHA-HKNCOMBI
             END-IF
             PERFORM   2201-RIHAKAISHI-HANTEI-SEC
             IF   FLG-KYUSEI-COMMENT  =  1
               PERFORM 2202-RIHAKAISHI-COMMENT-SEC
               MOVE    WRK-RIHASANTEI-SRYCD-TBL(2)
                                       TO   LNK-RIHASTDAY-OUT-OCC3(2)
             END-IF
           END-IF
      *
      *    脳血管疾患等
           IF   WRK-RIHASANTEI-FLG(3)  =  1
             MOVE      3               TO   IDY
             MOVE      1               TO   FLG-COMOKIKAE
             MOVE      CONS-NOUKE      TO   WRK-SRYCD
             MOVE      CONS-NOUKE-E    TO   WRK-SRYCD2
             MOVE      WRK-RIHASANTEI-DAY-TBL(3)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
             MOVE      WRK-RIHASANTEI-DAY-TBL2(3)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
             MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
             IF   FLG-HKNCOMBI-SET  =  1
               MOVE    LNK-RIHASTDAY-HKNCOMBI
                                       TO   WRK-RIHA-HKNCOMBI
             END-IF
             PERFORM   2201-RIHAKAISHI-HANTEI-SEC
             IF   FLG-KYUSEI-COMMENT  =  1
               PERFORM 2202-RIHAKAISHI-COMMENT-SEC
               MOVE    WRK-RIHASANTEI-SRYCD-TBL(3)
                                       TO   LNK-RIHASTDAY-OUT-OCC3(3)
             END-IF
           END-IF
      *
      *    廃用症候群
           IF   WRK-RIHASANTEI-FLG(4)  =  1
             MOVE      4               TO   IDY
             MOVE      1               TO   FLG-COMOKIKAE
             MOVE      CONS-HAIYO      TO   WRK-SRYCD
             MOVE      CONS-HAIYO-E    TO   WRK-SRYCD2
             MOVE      WRK-RIHASANTEI-DAY-TBL(4)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
             MOVE      WRK-RIHASANTEI-DAY-TBL2(4)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
             MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
             IF   FLG-HKNCOMBI-SET  =  1
               MOVE    LNK-RIHASTDAY-HKNCOMBI
                                       TO   WRK-RIHA-HKNCOMBI
             END-IF
             PERFORM   2201-RIHAKAISHI-HANTEI-SEC
             IF   FLG-KYUSEI-COMMENT  =  1
               PERFORM 2202-RIHAKAISHI-COMMENT-SEC
               MOVE    WRK-RIHASANTEI-SRYCD-TBL(4)
                                       TO   LNK-RIHASTDAY-OUT-OCC3(4)
             END-IF
           END-IF
      *
      *    廃用症候群の早期・初期リハ加算
           IF   WRK-RIHASANTEI-FLG(5)  =  1
             MOVE      5               TO   IDY
             MOVE      1               TO   FLG-COMOKIKAE
             MOVE      CONS-HAIYO-K    TO   WRK-SRYCD
             MOVE      "999999999"     TO   WRK-SRYCD2
             MOVE      WRK-RIHASANTEI-DAY-TBL(5)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
             MOVE      WRK-RIHASANTEI-DAY-TBL2(5)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
             MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
             IF   FLG-HKNCOMBI-SET  =  1
               MOVE    LNK-RIHASTDAY-HKNCOMBI
                                       TO   WRK-RIHA-HKNCOMBI
             END-IF
             PERFORM   2201-RIHAKAISHI-HANTEI-SEC
             IF   FLG-KYUSEI-COMMENT  =  1
               PERFORM 2202-RIHAKAISHI-COMMENT-SEC
               MOVE    WRK-RIHASANTEI-SRYCD-TBL(5)
                                       TO   LNK-RIHASTDAY-OUT-OCC3(5)
             END-IF
           END-IF
      *
      *    運動器
           IF   WRK-RIHASANTEI-FLG(6)  =  1
             MOVE      6               TO   IDY
             MOVE      1               TO   FLG-COMOKIKAE
             MOVE      CONS-UNDOKI     TO   WRK-SRYCD
             MOVE      CONS-UNDOKI-E   TO   WRK-SRYCD2
             MOVE      WRK-RIHASANTEI-DAY-TBL(6)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
             MOVE      WRK-RIHASANTEI-DAY-TBL2(6)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
             MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
             IF   FLG-HKNCOMBI-SET  =  1
               MOVE    LNK-RIHASTDAY-HKNCOMBI
                                       TO   WRK-RIHA-HKNCOMBI
             END-IF
             PERFORM   2201-RIHAKAISHI-HANTEI-SEC
             IF   FLG-KYUSEI-COMMENT  =  1
               PERFORM 2202-RIHAKAISHI-COMMENT-SEC
               MOVE    WRK-RIHASANTEI-SRYCD-TBL(6)
                                       TO   LNK-RIHASTDAY-OUT-OCC3(6)
             END-IF
           END-IF
      *
      *    呼吸器
           IF   WRK-RIHASANTEI-FLG(7)  =  1
             MOVE      7               TO   IDY
             MOVE      ZERO            TO   FLG-COMOKIKAE
             MOVE      CONS-KOKYUKI    TO   WRK-SRYCD
             MOVE      CONS-KOKYUKI-E  TO   WRK-SRYCD2
             MOVE      WRK-RIHASANTEI-DAY-TBL(7)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
             MOVE      WRK-RIHASANTEI-DAY-TBL2(7)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
             MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
             IF   FLG-HKNCOMBI-SET  =  1
               MOVE    LNK-RIHASTDAY-HKNCOMBI
                                       TO   WRK-RIHA-HKNCOMBI
             END-IF
             PERFORM   2201-RIHAKAISHI-HANTEI-SEC
             IF   FLG-KYUSEI-COMMENT  =  1
               PERFORM 2202-RIHAKAISHI-COMMENT-SEC
               MOVE    WRK-RIHASANTEI-SRYCD-TBL(7)
                                       TO   LNK-RIHASTDAY-OUT-OCC3(7)
             END-IF
           END-IF
      *
      *    呼吸器の早期・初期リハ加算
           IF   WRK-RIHASANTEI-FLG(8)  =  1
             MOVE      8               TO   IDY
             MOVE      1               TO   FLG-COMOKIKAE
             MOVE      CONS-KOKYUKI-K  TO   WRK-SRYCD
             MOVE      "999999999"     TO   WRK-SRYCD2
             MOVE      WRK-RIHASANTEI-DAY-TBL(8)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
             MOVE      WRK-RIHASANTEI-DAY-TBL2(8)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
             MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
             IF   FLG-HKNCOMBI-SET  =  1
               MOVE    LNK-RIHASTDAY-HKNCOMBI
                                       TO   WRK-RIHA-HKNCOMBI
             END-IF
             PERFORM   2201-RIHAKAISHI-HANTEI-SEC
             IF   FLG-KYUSEI-COMMENT  =  1
               PERFORM 2202-RIHAKAISHI-COMMENT-SEC
               MOVE    WRK-RIHASANTEI-SRYCD-TBL(8)
                                       TO   LNK-RIHASTDAY-OUT-OCC3(8)
             END-IF
           END-IF
      *
      *    摂食
           IF   WRK-RIHASANTEI-FLG(9)  =  1
             MOVE      9               TO   IDY
             MOVE      ZERO            TO   FLG-COMOKIKAE
             MOVE      CONS-SESSYOKU   TO   WRK-SRYCD
             MOVE      CONS-SESSYOKU-E TO   WRK-SRYCD2
             MOVE      WRK-RIHASANTEI-DAY-TBL(9)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
             MOVE      WRK-RIHASANTEI-DAY-TBL2(9)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
             MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
             IF   FLG-HKNCOMBI-SET  =  1
               MOVE    LNK-RIHASTDAY-HKNCOMBI
                                       TO   WRK-RIHA-HKNCOMBI
             END-IF
             PERFORM   2201-RIHAKAISHI-HANTEI-SEC
             IF   FLG-KYUSEI-COMMENT  =  1
               PERFORM 2202-RIHAKAISHI-COMMENT-SEC
               MOVE    WRK-RIHASANTEI-SRYCD-TBL(9)
                                       TO   LNK-RIHASTDAY-OUT-OCC3(9)
             END-IF
           END-IF
      *
      *    難病患者
           IF   WRK-RIHASANTEI-FLG(10)  =  1
             MOVE      10              TO   IDY
             MOVE      1               TO   FLG-COMOKIKAE
             MOVE      CONS-NANBYO     TO   WRK-SRYCD
             MOVE      CONS-NANBYO-E   TO   WRK-SRYCD2
             MOVE      WRK-RIHASANTEI-DAY-TBL(10)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
             MOVE      WRK-RIHASANTEI-DAY-TBL2(10)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
             MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
             IF   FLG-HKNCOMBI-SET  =  1
               MOVE    LNK-RIHASTDAY-HKNCOMBI
                                       TO   WRK-RIHA-HKNCOMBI
             END-IF
             PERFORM   2201-RIHAKAISHI-HANTEI-SEC
             IF   FLG-KYUSEI-COMMENT  =  1
               PERFORM 2202-RIHAKAISHI-COMMENT-SEC
               MOVE    WRK-RIHASANTEI-SRYCD-TBL(10)
                                       TO   LNK-RIHASTDAY-OUT-OCC3(10)
             END-IF
           END-IF
      *
      *    障害児（者）
           IF   WRK-RIHASANTEI-FLG(11)  =  1
             MOVE      11              TO   IDY
             MOVE      ZERO            TO   FLG-COMOKIKAE
             MOVE      CONS-SYOGAI     TO   WRK-SRYCD
             MOVE      CONS-SYOGAI-E   TO   WRK-SRYCD2
             MOVE      WRK-RIHASANTEI-DAY-TBL(11)
                                       TO   TBL-HASSYO-TAIHI-DAY-T
             MOVE      WRK-RIHASANTEI-DAY-TBL2(11)
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
             MOVE      ZERO            TO   WRK-RIHA-HKNCOMBI
             IF   FLG-HKNCOMBI-SET  =  1
               MOVE    LNK-RIHASTDAY-HKNCOMBI
                                       TO   WRK-RIHA-HKNCOMBI
             END-IF
             PERFORM   2201-RIHAKAISHI-HANTEI-SEC
             IF   FLG-KYUSEI-COMMENT  =  1
               PERFORM 2202-RIHAKAISHI-COMMENT-SEC
               MOVE    WRK-RIHASANTEI-SRYCD-TBL(11)
                                       TO   LNK-RIHASTDAY-OUT-OCC3(11)
             END-IF
           END-IF
      *
           .
       220-RIHAKAISHI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ開始日取得
      *****************************************************************
       2201-RIHAKAISHI-HANTEI-SEC          SECTION.
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
           INITIALIZE                           SANTEI-REC
           MOVE    LNK-RIHASTDAY-HOSPNUM    TO  SANTEI-HOSPNUM
           MOVE    LNK-RIHASTDAY-PTID       TO  SANTEI-PTID
           MOVE    WRK-SRYCD2               TO  SANTEI-SRYCD
           MOVE    ZERO                     TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                     TO  SANTEI-SRYKA
           MOVE    WRK-RIHA-HKNCOMBI        TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC               TO  MCPDATA-REC
           MOVE    "tbl_santei"             TO  MCP-TABLE
           MOVE    "key3"                   TO  MCP-PATHNAME
           PERFORM  900-DBSELECT-SEC
           IF       MCP-RC  =  ZERO
              MOVE    "tbl_santei"          TO  MCP-TABLE
              MOVE    "key3"                TO  MCP-PATHNAME
              PERFORM  900-SANTEI-READ-SEC
              PERFORM  UNTIL  (FLG-SANTEI          =  1)
                         OR   (FLG-KYUSEI-COMMENT2 =  1)
                IF   SANTEI-SRYYM  <=  LNK-RIHASTDAY-SRYYM
                  IF   SANTEI-SRYYM  =  LNK-RIHASTDAY-SRYYM
                   PERFORM   VARYING   IDX3   FROM   1  BY   1
                              UNTIL    IDX3    >    31
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
                        MOVE    1   TO   TB-RIHASANTEI-DAY-END(IDX3)
                     END-IF
                   END-PERFORM
                  ELSE
                   PERFORM   VARYING   IDX3   FROM  31  BY  -1
                              UNTIL   (IDX3                <  1)
                                OR    (FLG-KYUSEI-COMMENT2 =  1)
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
                        MOVE  SANTEI-SRYYM  TO  WRK-RIHASANTEI-END(1:6)
                        MOVE  IDX3          TO  WRK-RIHASANTEI-END(7:2)
                        MOVE  1             TO  FLG-KYUSEI-COMMENT2
                     END-IF
                   END-PERFORM
                  END-IF
                END-IF
                MOVE    "tbl_santei"        TO  MCP-TABLE
                MOVE    "key3"              TO  MCP-PATHNAME
                PERFORM  900-SANTEI-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_santei"             TO  MCP-TABLE
           MOVE    "key3"                   TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
      *****
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
           INITIALIZE                           SANTEI-REC
           MOVE    LNK-RIHASTDAY-HOSPNUM    TO  SANTEI-HOSPNUM
           MOVE    LNK-RIHASTDAY-PTID       TO  SANTEI-PTID
           MOVE    WRK-SRYCD                TO  SANTEI-SRYCD
           MOVE    ZERO                     TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                     TO  SANTEI-SRYKA
           MOVE    WRK-RIHA-HKNCOMBI        TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC               TO  MCPDATA-REC
           MOVE    "tbl_santei"             TO  MCP-TABLE
           MOVE    "key3"                   TO  MCP-PATHNAME
           PERFORM  900-DBSELECT-SEC
           IF       MCP-RC  =  ZERO
              MOVE    "tbl_santei"          TO  MCP-TABLE
              MOVE    "key3"                TO  MCP-PATHNAME
              PERFORM  900-SANTEI-READ-SEC
              PERFORM  UNTIL  (FLG-SANTEI          =  1)
                         OR   (FLG-KYUSEI-COMMENT2 =  1)
                IF   SANTEI-SRYYM  <=  LNK-RIHASTDAY-SRYYM
                  IF   SANTEI-SRYYM  =  LNK-RIHASTDAY-SRYYM
                   PERFORM   VARYING   IDX3   FROM   1  BY   1
                              UNTIL    IDX3    >    31
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
                       MOVE    1     TO  TB-RIHASANTEI-DAY(IDX3)
                       MOVE    1     TO  FLG-KYUSEI-COMMENT
                       IF    (SANTEI-SRYYM = "200604")
                       AND   (IDX3         =  1      )
                         MOVE  1     TO  WRK-RIHASANTEI41-FLG
                       END-IF
      **>              IF    (SANTEI-SRYYM  NOT = "200604")
      **>              AND   (IDX3              =  1      )
      **>                MOVE  1     TO  FLG-KYUSEI-COMMENT2
      **>              END-IF
                     END-IF
                   END-PERFORM
                  ELSE
                   PERFORM   VARYING   IDX3   FROM  31  BY  -1
                              UNTIL   (IDX3                <  1)
                                OR    (FLG-KYUSEI-COMMENT2 =  1)
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
                       PERFORM  22011-RIHA-SANTEIPLUS-SEC
                     END-IF
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
                           AND (SANTEIPLUS-ENDYMD(1:6)  =
                                                    LNK-RIHASTDAY-SRYYM)
                           AND (IDX20                  >=  RIHA-ST2    )
                             MOVE   1   TO TB-RIHASANTEI-DAY-END3(IDX20)
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
                   END-PERFORM
                  END-IF
                END-IF
                MOVE    "tbl_santei"        TO  MCP-TABLE
                MOVE    "key3"              TO  MCP-PATHNAME
                PERFORM  900-SANTEI-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_santei"             TO  MCP-TABLE
           MOVE    "key3"                   TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
      *****
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
               PERFORM   22011-RIHA-SANTEIPLUS2-SEC
               MOVE      ZERO  TO  FLG-OK
               PERFORM  VARYING  RIHA-IDX2  FROM  1  BY  1
                         UNTIL   RIHA-IDX2    >  31
                 IF    (TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX2) NOT = ZERO)
                 AND   (RIHA-IDX  <=  RIHA-IDX2                      )
                 AND   (IDX20     >=  RIHA-IDX2                      )
                   MOVE  ZERO  TO  TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX2)
                   MOVE  1     TO  FLG-OK
                 END-IF
               END-PERFORM
               IF   FLG-OK  =  ZERO
                 MOVE    ZERO  TO  TB-RIHASANTEI-DAY(RIHA-IDX)
               ELSE
                 IF  IDX20  NOT =  99
                   MOVE  1     TO  TB-RIHASANTEI-DAY-END2(IDX20)
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
       2201-RIHAKAISHI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ・算定履歴付加判定
      *    （リハ開始日の終了日と診療月をチェック）（履歴番号＝０のデータ）
      *****************************************************************
       22011-RIHA-SANTEIPLUS-SEC          SECTION.
      *
           MOVE    ZERO                     TO  FLG-SANTEIPLUS-HANTEI
           MOVE    ZERO                     TO  IDX20
      *
           INITIALIZE                           SANTEIPLUS-REC
           MOVE    LNK-RIHASTDAY-HOSPNUM    TO  SANTEIPLUS-HOSPNUM
           MOVE    LNK-RIHASTDAY-PTID       TO  SANTEIPLUS-PTID
           MOVE    SANTEI-SRYYM             TO  SANTEIPLUS-SRYYM
           MOVE    WRK-SRYCD                TO  SANTEIPLUS-SRYCD
           MOVE    ZERO                     TO  SANTEIPLUS-NYUGAIKBN
           MOVE    ZERO                     TO  SANTEIPLUS-SRYKA
           MOVE    WRK-RIHA-HKNCOMBI        TO  SANTEIPLUS-HKNCOMBINUM
           MOVE    IDX3                     TO  SANTEIPLUS-DAYKEY
           MOVE    SANTEIPLUS-REC           TO  MCPDATA-REC
           MOVE    "tbl_santeiplus"         TO  MCP-TABLE
           MOVE    "key4"                   TO  MCP-PATHNAME
           PERFORM  900-DBSELECT-SEC
           IF       MCP-RC  =  ZERO
             MOVE    "tbl_santeiplus"       TO  MCP-TABLE
             MOVE    "key4"                 TO  MCP-PATHNAME
             PERFORM  900-SANTEIPLUS-READ-SEC
             IF       FLG-SANTEIPLUS  =  ZERO
               MOVE SANTEIPLUS-ENDYMD(7:2)  TO  IDX20
               IF ((SANTEIPLUS-ENDYMD   NOT =  SPACE              ) AND
                   (SANTEIPLUS-ENDYMD(1:6)  <  LNK-RIHASTDAY-SRYYM))
               OR ((SANTEIPLUS-ENDYMD   NOT =  SPACE              ) AND
                   (SANTEIPLUS-ENDYMD(1:6)  =  LNK-RIHASTDAY-SRYYM) AND
                   (IDX20                   <  RIHA-ST2           ))
                  MOVE     1     TO     FLG-SANTEIPLUS-HANTEI
               END-IF
             END-IF
           END-IF
           MOVE    "tbl_santeiplus"         TO  MCP-TABLE
           MOVE    "key4"                   TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       22011-RIHA-SANTEIPLUS-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ・算定履歴付加判定
      *    （リハ開始日の終了日と診療月をチェック）（履歴番号＝０のデータ）
      *****************************************************************
       22011-RIHA-SANTEIPLUS2-SEC          SECTION.
      *
           MOVE    99                       TO  IDX20
      *
           INITIALIZE                           SANTEIPLUS-REC
           MOVE    LNK-RIHASTDAY-HOSPNUM    TO  SANTEIPLUS-HOSPNUM
           MOVE    LNK-RIHASTDAY-PTID       TO  SANTEIPLUS-PTID
           MOVE    LNK-RIHASTDAY-SRYYM      TO  SANTEIPLUS-SRYYM
           MOVE    WRK-SRYCD                TO  SANTEIPLUS-SRYCD
           MOVE    ZERO                     TO  SANTEIPLUS-NYUGAIKBN
           MOVE    ZERO                     TO  SANTEIPLUS-SRYKA
           MOVE    WRK-RIHA-HKNCOMBI        TO  SANTEIPLUS-HKNCOMBINUM
           MOVE    RIHA-IDX                 TO  SANTEIPLUS-DAYKEY
           MOVE    SANTEIPLUS-REC           TO  MCPDATA-REC
           MOVE    "tbl_santeiplus"         TO  MCP-TABLE
           MOVE    "key4"                   TO  MCP-PATHNAME
           PERFORM  900-DBSELECT-SEC
           IF       MCP-RC  =  ZERO
             MOVE    "tbl_santeiplus"       TO  MCP-TABLE
             MOVE    "key4"                 TO  MCP-PATHNAME
             PERFORM  900-SANTEIPLUS-READ-SEC
             IF       FLG-SANTEIPLUS  =  ZERO
               IF  (SANTEIPLUS-ENDYMD   NOT =  SPACE              )
               AND (SANTEIPLUS-ENDYMD(1:6)  =  LNK-RIHASTDAY-SRYYM)
                 MOVE  SANTEIPLUS-ENDYMD(7:2)  TO  IDX20
               END-IF
             END-IF
           END-IF
           MOVE    "tbl_santeiplus"         TO  MCP-TABLE
           MOVE    "key4"                   TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       22011-RIHA-SANTEIPLUS2-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ開始日コメント編集
      *****************************************************************
       2202-RIHAKAISHI-COMMENT-SEC          SECTION.
      *
           MOVE         ZERO   TO  IDX
      *
           IF  WRK-RIHASANTEI-YMD3 NOT = SPACE
             MOVE       WRK-RIHASANTEI-YMD3   TO  WRK-KYUSEIYMD
             PERFORM    22021-RIHAKAISHI-COMGET-SEC
             IF   LNK-RIHASTDAY-SRYYM  >=  "201905"
              PERFORM   22022-RIHAKAISHI-COM-201905-SEC
             ELSE
              PERFORM   22022-RIHAKAISHI-COMHEN-SEC
             END-IF
           END-IF
           IF  WRK-RIHASANTEI-YMD  NOT = SPACE
             MOVE       WRK-RIHASANTEI-YMD    TO  WRK-KYUSEIYMD
             PERFORM    22021-RIHAKAISHI-COMGET-SEC
             IF   LNK-RIHASTDAY-SRYYM  >=  "201905"
              PERFORM   22022-RIHAKAISHI-COM-201905-SEC
             ELSE
              PERFORM   22022-RIHAKAISHI-COMHEN-SEC
             END-IF
           END-IF
           PERFORM   VARYING   IDX16    FROM   1   BY   1
                      UNTIL    IDX16      >   31
             IF  TB-RIHASANTEI-DAY(IDX16)  NOT = ZERO
               MOVE     LNK-RIHASTDAY-SRYYM   TO  WRK-KYUSEIYMD(1:6)
               MOVE     IDX16                 TO  WRK-KYUSEIYMD(7:2)
               PERFORM  22021-RIHAKAISHI-COMGET-SEC
               IF   LNK-RIHASTDAY-SRYYM  >=  "201905"
                PERFORM 22022-RIHAKAISHI-COM-201905-SEC
               ELSE
                PERFORM 22022-RIHAKAISHI-COMHEN-SEC
               END-IF
             END-IF
           END-PERFORM
      *
           .
       2202-RIHAKAISHI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ開始日と同一剤のコメント取得
      *****************************************************************
       22021-RIHAKAISHI-COMGET-SEC          SECTION.
      *
           MOVE    ZERO                    TO  FLG-KYUSEI-OK
           MOVE    SPACE                   TO  WRK-KYUSEI-NYUGAIKBN
           MOVE    SPACE                   TO  WRK-KYUSEI-SRYKA
           MOVE    ZERO                    TO  WRK-KYUSEI-ZAINUM
           MOVE    ZERO                    TO  FLG-COMOKIKAE2
           MOVE    ZERO                    TO  FLG-COMARI
           MOVE    ZERO                    TO  FLG-COMARI2
      *
           MOVE    ZERO                    TO  IDZ
           INITIALIZE                          WRK-FREE-AREA
      *
           PERFORM   VARYING   IDX15    FROM   2   BY  -1
                      UNTIL   (IDX15          <  1)
                       OR     (FLG-KYUSEI-OK  =  1)
      *
             INITIALIZE                        SRYACT-REC
             MOVE    LNK-RIHASTDAY-HOSPNUM TO  SRY-HOSPNUM
             MOVE    IDX15                 TO  SRY-NYUGAIKBN
             MOVE    LNK-RIHASTDAY-PTID    TO  SRY-PTID
             MOVE    WRK-KYUSEIYMD(1:6)    TO  SRY-SRYYM
             MOVE    WRK-SRYCD             TO  SRY-SRYCD (1)
             MOVE    WRK-SRYCD             TO  SRY-SRYCD (2)
             MOVE    WRK-SRYCD             TO  SRY-SRYCD (3)
             MOVE    WRK-SRYCD             TO  SRY-SRYCD (4)
             MOVE    WRK-SRYCD             TO  SRY-SRYCD (5)
             MOVE    SRYACT-REC            TO  MCPDATA-REC
             MOVE    "tbl_sryact"          TO  MCP-TABLE
             MOVE    "key7"                TO  MCP-PATHNAME
             PERFORM  900-DBSELECT-SEC
             IF       MCP-RC  =  ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL  (FLG-SRYACT    =  1)
                         OR    (FLG-KYUSEI-OK =  1)
                 INITIALIZE                    SRYACCT-REC
                 MOVE    SRY-HOSPNUM       TO  ACCT-HOSPNUM
                 MOVE    SRY-NYUGAIKBN     TO  ACCT-NYUGAIKBN
                 MOVE    SRY-PTID          TO  ACCT-PTID
                 MOVE    SRY-SRYKA         TO  ACCT-SRYKA
                 MOVE    SRY-SRYYM         TO  ACCT-SRYYM
                 MOVE    SRY-ZAINUM        TO  ACCT-ZAINUM
                 MOVE    SRYACCT-REC       TO  MCPDATA-REC
                 MOVE    "tbl_sryacct"     TO  MCP-TABLE
                 MOVE    "key31"           TO  MCP-PATHNAME
                 PERFORM  900-DBSELECT-SEC
                 IF       MCP-RC  =  ZERO
                    MOVE    "tbl_sryacct"  TO  MCP-TABLE
                    MOVE    "key31"        TO  MCP-PATHNAME
                    PERFORM  900-SRYACCT-READ-SEC
                    IF       FLG-SRYACCT  =  ZERO
                      MOVE   WRK-KYUSEIYMD(7:2)
                                           TO  DAY-IDX2
                      IF   ACCT-DAY(1 DAY-IDX2)  NOT =  ZERO
                        INITIALIZE                  HKNCOMBI-REC
                        MOVE     ACCT-HOSPNUM   TO  COMB-HOSPNUM
                        MOVE     ACCT-PTID      TO  COMB-PTID
                        MOVE     ACCT-HKNCOMBI  TO  COMB-HKNCOMBINUM
                        MOVE     HKNCOMBI-REC   TO  MCPDATA-REC
                        MOVE     "tbl_hkncombi" TO  MCP-TABLE
                        MOVE     "key"          TO  MCP-PATHNAME
                        PERFORM   900-DBSELECT-SEC
                        IF        MCP-RC  =  ZERO
                          MOVE   "tbl_hkncombi" TO  MCP-TABLE
                          MOVE   "key"          TO  MCP-PATHNAME
                          PERFORM 900-HKNCOMBI-READ-SEC
                          IF      FLG-HKNCOMBI  =  ZERO
                            IF  WRK-RIHA-HKNCOMBI = ZERO
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
                              IF   WRK-RIHA-HKNCOMBI =
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
                        MOVE     "tbl_hkncombi" TO  MCP-TABLE
                        MOVE     "key"          TO  MCP-PATHNAME
                        PERFORM   900-CLOSE-SEC
                      END-IF
                    END-IF
                 END-IF
                 MOVE    "tbl_sryacct"     TO  MCP-TABLE
                 MOVE    "key31"           TO  MCP-PATHNAME
                 PERFORM  900-CLOSE-SEC
      *
                 MOVE    "tbl_sryact"      TO  MCP-TABLE
                 MOVE    "key7"            TO  MCP-PATHNAME
                 PERFORM  900-SRYACT-READ-SEC
               END-PERFORM
             END-IF
             MOVE    "tbl_sryact"          TO  MCP-TABLE
             MOVE    "key7"                TO  MCP-PATHNAME
             PERFORM  900-CLOSE-SEC
      *
           END-PERFORM
      *
           IF      WRK-KYUSEI-ZAINUM    NOT =    ZERO
              INITIALIZE                         SRYACT-REC
              MOVE    LNK-RIHASTDAY-HOSPNUM  TO  SRY-HOSPNUM
              MOVE    WRK-KYUSEI-NYUGAIKBN   TO  SRY-NYUGAIKBN
              MOVE    LNK-RIHASTDAY-PTID     TO  SRY-PTID
              MOVE    WRK-KYUSEI-SRYKA       TO  SRY-SRYKA
              MOVE    WRK-KYUSEIYMD(1:6)     TO  SRY-SRYYM
              MOVE    WRK-KYUSEI-ZAINUM      TO  SRY-ZAINUM
              MOVE    SRYACT-REC             TO  MCPDATA-REC
              MOVE    "tbl_sryact"           TO  MCP-TABLE
              MOVE    "key2"                 TO  MCP-PATHNAME
              PERFORM  900-DBSELECT-SEC
              IF       MCP-RC  =  ZERO
                 MOVE    "tbl_sryact"        TO  MCP-TABLE
                 MOVE    "key2"              TO  MCP-PATHNAME
                 PERFORM  900-SRYACT-READ-SEC
                 PERFORM  UNTIL  FLG-SRYACT =  1
                   PERFORM  VARYING  IDX12  FROM  1  BY  1
                             UNTIL  (IDX12            >   5   )
                               OR   (SRY-SRYCD(IDX12) =  SPACE)
                     IF  ((SRY-SRYCD(IDX12)(1:1)     =   "8"  )  OR
                          (SRY-SRYCD(IDX12)(1:3)     =   "008"))
                     AND ( SRY-SRYCD(IDX12)(1:4) NOT =
                                             "0085" AND "0086" )
                       IF   SRY-INPUTNUM(IDX12)   NOT =   ZERO
                         PERFORM 900-PTCOM-READ-SEC
                         ADD     1           TO  IDZ
                         MOVE    "810000001" TO  WK-FREE-SRYCD(IDZ)
                         MOVE    PTCOM-INPUTCOMENT
                                             TO  WK-FREE-NAME(IDZ)
                         PERFORM   VARYING   IDX13   FROM   200   BY  -1
                                    UNTIL    IDX13    <    1
                           IF  WK-FREE-NAME(IDZ)(IDX13:1)  NOT =  SPACE
                             COMPUTE  WK-FREE-YUKOKETA(IDZ)  =
                                      IDX13    /    2
                             MOVE     1      TO  IDX13
                           END-IF
                         END-PERFORM
                       ELSE
                         PERFORM 900-TENSU-READ-SEC
                         ADD     1           TO  IDZ
                         MOVE    "810000001" TO  WK-FREE-SRYCD(IDZ)
                         MOVE    TNS-NAME    TO  WK-FREE-NAME(IDZ)
                         PERFORM   VARYING   IDX13   FROM   200   BY  -1
                                    UNTIL    IDX13    <    1
                           IF  WK-FREE-NAME(IDZ)(IDX13:1)  NOT =  SPACE
                             COMPUTE  WK-FREE-YUKOKETA(IDZ)  =
                                      IDX13    /    2
                             MOVE     1      TO  IDX13
                           END-IF
                         END-PERFORM
                       END-IF
                       IF   FLG-COMOKIKAE  =   1
                          EVALUATE  SRY-SRYCD(IDX12)
                            WHEN     "840000042"
                              IF   FLG-COMOKIKAE2 = ZERO
                                 MOVE    1   TO  FLG-COMOKIKAE2
                                 MOVE  SPACE TO  WK-FREE-SRYCD(IDZ)
                                 MOVE  SPACE TO  WK-FREE-NAME(IDZ)
                                 MOVE  ZERO  TO  WK-FREE-YUKOKETA(IDZ)
                                 COMPUTE  IDZ  =  IDZ - 1
                              END-IF
                            WHEN     "840000101"
                              IF   FLG-COMOKIKAE2 = ZERO
                                 MOVE    2   TO  FLG-COMOKIKAE2
                                 MOVE  SPACE TO  WK-FREE-SRYCD(IDZ)
                                 MOVE  SPACE TO  WK-FREE-NAME(IDZ)
                                 MOVE  ZERO  TO  WK-FREE-YUKOKETA(IDZ)
                                 COMPUTE  IDZ  =  IDZ - 1
                              END-IF
                            WHEN     "840000058"
                              IF   FLG-COMOKIKAE2 = ZERO
                                 MOVE    3   TO  FLG-COMOKIKAE2
                                 MOVE  SPACE TO  WK-FREE-SRYCD(IDZ)
                                 MOVE  SPACE TO  WK-FREE-NAME(IDZ)
                                 MOVE  ZERO  TO  WK-FREE-YUKOKETA(IDZ)
                                 COMPUTE  IDZ  =  IDZ - 1
                              END-IF
                          END-EVALUATE
                       END-IF
                       IF   FLG-COMOKIKAE  =   1
                          IF  SRY-SRYCD(IDX12) = "840000042" OR
                                                 "840000101" OR
                                                 "840000058"
                            MOVE    1        TO  FLG-COMARI2
                          ELSE
                            MOVE    1        TO  FLG-COMARI
                          END-IF
                       ELSE
                            MOVE    1        TO  FLG-COMARI
                       END-IF
                     END-IF
                   END-PERFORM
                   MOVE    "tbl_sryact"      TO  MCP-TABLE
                   MOVE    "key2"            TO  MCP-PATHNAME
                   PERFORM  900-SRYACT-READ-SEC
                 END-PERFORM
              END-IF
              MOVE    "tbl_sryact"           TO  MCP-TABLE
              MOVE    "key2"                 TO  MCP-PATHNAME
              PERFORM  900-CLOSE-SEC
           END-IF
      *
           IF      FLG-COMARI    =    ZERO
              INITIALIZE                         SANTEIPLUS-REC
              MOVE    LNK-RIHASTDAY-HOSPNUM  TO  SANTEIPLUS-HOSPNUM
              MOVE    LNK-RIHASTDAY-PTID     TO  SANTEIPLUS-PTID
              MOVE    WRK-KYUSEIYMD(1:6)     TO  SANTEIPLUS-SRYYM
              MOVE    WRK-SRYCD              TO  SANTEIPLUS-SRYCD
              MOVE    ZERO                   TO  SANTEIPLUS-NYUGAIKBN
              MOVE    ZERO                   TO  SANTEIPLUS-SRYKA
              MOVE    WRK-RIHA-HKNCOMBI      TO  SANTEIPLUS-HKNCOMBINUM
              MOVE    WRK-KYUSEIYMD(7:2)     TO  SANTEIPLUS-DAYKEY
              MOVE    SANTEIPLUS-REC         TO  MCPDATA-REC
              MOVE    "tbl_santeiplus"       TO  MCP-TABLE
              MOVE    "key2"                 TO  MCP-PATHNAME
              PERFORM  900-DBSELECT-SEC
              IF       MCP-RC  =  ZERO
                MOVE    "tbl_santeiplus"     TO  MCP-TABLE
                MOVE    "key2"               TO  MCP-PATHNAME
                PERFORM  900-SANTEIPLUS-READ-SEC
                PERFORM  UNTIL  FLG-SANTEIPLUS =  1
      *           リハコメントの終了日と診療月をチェック
      *           （履歴番号＞０のデータ）
                  IF      SANTEIPLUS-RENNUM   NOT =  ZERO
                    IF  ( SANTEIPLUS-ENDYMD       =  SPACE        )
                    OR  ((SANTEIPLUS-ENDYMD   NOT =  SPACE       ) AND
                         (SANTEIPLUS-ENDYMD(1:6) >=
                                              LNK-RIHASTDAY-SRYYM))
                     IF  SANTEIPLUS-COMMENT  NOT =  SPACE
                      ADD     1         TO  IDZ
                      MOVE  "810000001" TO  WK-FREE-SRYCD(IDZ)
                      MOVE  SANTEIPLUS-COMMENT
                                        TO  WK-FREE-NAME(IDZ)
                      PERFORM   VARYING   IDX13   FROM   200   BY  -1
                                 UNTIL    IDX13    <     1
                        IF  WK-FREE-NAME(IDZ)(IDX13:1)  NOT =  SPACE
                          COMPUTE  WK-FREE-YUKOKETA(IDZ)  =
                                   IDX13    /    2
                          MOVE     1    TO  IDX13
                        END-IF
                      END-PERFORM
                     END-IF
                     IF   FLG-COMOKIKAE  =    1
                     AND  FLG-COMARI2    =   ZERO
                      IF   SANTEIPLUS-COMMENT(1:8)  = "手術　日"
                        IF   FLG-COMOKIKAE2 = ZERO
                           MOVE    1    TO  FLG-COMOKIKAE2
                           MOVE  SPACE  TO  WK-FREE-SRYCD(IDZ)
                           MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                           MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                           COMPUTE  IDZ  =  IDZ - 1
                        END-IF
                      END-IF
                      IF   SANTEIPLUS-COMMENT(1:12) = "急性増悪　日"
                        IF   FLG-COMOKIKAE2 = ZERO
                           MOVE    2    TO  FLG-COMOKIKAE2
                           MOVE  SPACE  TO  WK-FREE-SRYCD(IDZ)
                           MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                           MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                           COMPUTE  IDZ  =  IDZ - 1
                        END-IF
                      END-IF
                      IF   SANTEIPLUS-COMMENT(1:8)  = "診断　日"
                        IF   FLG-COMOKIKAE2 = ZERO
                           MOVE    3    TO  FLG-COMOKIKAE2
                           MOVE  SPACE  TO  WK-FREE-SRYCD(IDZ)
                           MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                           MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                           COMPUTE  IDZ  =  IDZ - 1
                        END-IF
                      END-IF
                     END-IF
                    END-IF
                  END-IF
                  MOVE    "tbl_santeiplus"   TO  MCP-TABLE
                  MOVE    "key2"             TO  MCP-PATHNAME
                  PERFORM  900-SANTEIPLUS-READ-SEC
                END-PERFORM
              END-IF
              MOVE    "tbl_santeiplus"       TO  MCP-TABLE
              MOVE    "key2"                 TO  MCP-PATHNAME
              PERFORM  900-CLOSE-SEC
           END-IF
      *
           .
       22021-RIHAKAISHI-COMGET-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ開始日及びリハビリ開始日と同一剤のコメントを返却
      *    エリアに編集
      *****************************************************************
       22022-RIHAKAISHI-COMHEN-SEC          SECTION.
      *
           ADD          1     TO  IDX
           IF   IDX  <=  100
             ADD          1   TO  LNK-RIHASTDAY-IDX(IDY)
             MOVE        "*"  TO  LNK-RIHASTDAY-MARK(IDY IDX)
             EVALUATE    IDY
               WHEN       1
                 MOVE    23   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                 MOVE  "心大血管疾患リハ：治療開始　　　年　　月　　日"
                              TO  LNK-RIHASTDAY-COM(IDY IDX)
                 MOVE    29   TO  IDC1
                 MOVE    35   TO  IDC2
                 MOVE    41   TO  IDC3
                 PERFORM 220221-RIHA-COM-DATE-SEC
               WHEN       2
                 EVALUATE   FLG-COMOKIKAE2
                   WHEN       0
                   WHEN       3
                     MOVE    22   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                     MOVE "早期・初期リハ加算：発症　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    27   TO  IDC1
                     MOVE    33   TO  IDC2
                     MOVE    39   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       1
                     MOVE    22   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                     MOVE "早期・初期リハ加算：手術　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    27   TO  IDC1
                     MOVE    33   TO  IDC2
                     MOVE    39   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       2
                     MOVE    24   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                     MOVE
                      "早期・初期リハ加算：急性増悪　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    31   TO  IDC1
                     MOVE    37   TO  IDC2
                     MOVE    43   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                 END-EVALUATE
               WHEN       3
                 EVALUATE   FLG-COMOKIKAE2
                   WHEN       0
                     MOVE    21   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                     MOVE  "脳血管疾患等リハ：発症　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    25   TO  IDC1
                     MOVE    31   TO  IDC2
                     MOVE    37   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       1
                     MOVE    21   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                     MOVE  "脳血管疾患等リハ：手術　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    25   TO  IDC1
                     MOVE    31   TO  IDC2
                     MOVE    37   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       2
                     MOVE    23   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                 MOVE  "脳血管疾患等リハ：急性増悪　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    29   TO  IDC1
                     MOVE    35   TO  IDC2
                     MOVE    41   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       3
                     MOVE    21   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                     MOVE  "脳血管疾患等リハ：診断　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    25   TO  IDC1
                     MOVE    31   TO  IDC2
                     MOVE    37   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                 END-EVALUATE
               WHEN       4
                 EVALUATE   FLG-COMOKIKAE2
                   WHEN       0
                   WHEN       1
                   WHEN       3
                     MOVE    20   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                     MOVE  "廃用症候群リハ：診断　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    23   TO  IDC1
                     MOVE    29   TO  IDC2
                     MOVE    35   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       2
                     MOVE    22   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                 MOVE  "廃用症候群リハ：急性増悪　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    27   TO  IDC1
                     MOVE    33   TO  IDC2
                     MOVE    39   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                 END-EVALUATE
               WHEN       5
                 EVALUATE   FLG-COMOKIKAE2
                   WHEN       0
                   WHEN       3
                     MOVE    22   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                     MOVE "早期・初期リハ加算：発症　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    27   TO  IDC1
                     MOVE    33   TO  IDC2
                     MOVE    39   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       1
                     MOVE    22   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                     MOVE "早期・初期リハ加算：手術　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    27   TO  IDC1
                     MOVE    33   TO  IDC2
                     MOVE    39   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       2
                     MOVE    24   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                     MOVE
                      "早期・初期リハ加算：急性増悪　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    31   TO  IDC1
                     MOVE    37   TO  IDC2
                     MOVE    43   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                 END-EVALUATE
               WHEN       6
                 EVALUATE   FLG-COMOKIKAE2
                   WHEN       0
                     MOVE    18   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                     MOVE  "運動器リハ：発症　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    19   TO  IDC1
                     MOVE    25   TO  IDC2
                     MOVE    31   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       1
                     MOVE    18   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                     MOVE  "運動器リハ：手術　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    19   TO  IDC1
                     MOVE    25   TO  IDC2
                     MOVE    31   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       2
                     MOVE    20   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                     MOVE  "運動器リハ：急性増悪　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    23   TO  IDC1
                     MOVE    29   TO  IDC2
                     MOVE    35   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       3
                     MOVE    18   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                     MOVE  "運動器リハ：診断　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    19   TO  IDC1
                     MOVE    25   TO  IDC2
                     MOVE    31   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                 END-EVALUATE
               WHEN       7
                 MOVE    20   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                 MOVE  "呼吸器リハ：治療開始　　　年　　月　　日"
                              TO  LNK-RIHASTDAY-COM(IDY IDX)
                 MOVE    23   TO  IDC1
                 MOVE    29   TO  IDC2
                 MOVE    35   TO  IDC3
                 PERFORM 220221-RIHA-COM-DATE-SEC
               WHEN       8
                 EVALUATE   FLG-COMOKIKAE2
                   WHEN       0
                   WHEN       3
                     MOVE    22   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                     MOVE "早期・初期リハ加算：発症　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    27   TO  IDC1
                     MOVE    33   TO  IDC2
                     MOVE    39   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       1
                     MOVE    22   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                     MOVE "早期・初期リハ加算：手術　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    27   TO  IDC1
                     MOVE    33   TO  IDC2
                     MOVE    39   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       2
                     MOVE    24   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                     MOVE
                      "早期・初期リハ加算：急性増悪　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    31   TO  IDC1
                     MOVE    37   TO  IDC2
                     MOVE    43   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                 END-EVALUATE
               WHEN       9
                 MOVE    21   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                 MOVE  "摂食機能療法：治療開始　　　年　　月　　日"
                              TO  LNK-RIHASTDAY-COM(IDY IDX)
                 MOVE    25   TO  IDC1
                 MOVE    31   TO  IDC2
                 MOVE    37   TO  IDC3
                 PERFORM 220221-RIHA-COM-DATE-SEC
               WHEN       10
                 EVALUATE   FLG-COMOKIKAE2
                   WHEN       0
                   WHEN       3
                     MOVE    19   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                     MOVE  "難病患者リハ：発症　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    21   TO  IDC1
                     MOVE    27   TO  IDC2
                     MOVE    33   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       1
                     MOVE    19   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                     MOVE  "難病患者リハ：手術　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    21   TO  IDC1
                     MOVE    27   TO  IDC2
                     MOVE    33   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       2
                     MOVE    21   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                     MOVE  "難病患者リハ：急性増悪　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    25   TO  IDC1
                     MOVE    31   TO  IDC2
                     MOVE    37   TO  IDC3
                     PERFORM 220221-RIHA-COM-DATE-SEC
                 END-EVALUATE
               WHEN       11
                 MOVE    21   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
                 MOVE  "障害児（者）リハ：発症　　　年　　月　　日"
                              TO  LNK-RIHASTDAY-COM(IDY IDX)
                 MOVE    25   TO  IDC1
                 MOVE    31   TO  IDC2
                 MOVE    37   TO  IDC3
                 PERFORM 220221-RIHA-COM-DATE-SEC
             END-EVALUATE
           END-IF
      *
      *    リハビリ開始日と同一剤のコメント
           PERFORM  VARYING  IDZ  FROM  1  BY  1
                     UNTIL  (IDZ                >  49   )
                       OR   (WK-FREE-SRYCD(IDZ) =  SPACE)
             ADD      1     TO  IDX
             IF   IDX  <=  100
               ADD      1   TO  LNK-RIHASTDAY-IDX(IDY)
               MOVE   SPACE TO  LNK-RIHASTDAY-MARK(IDY IDX)
               MOVE   WK-FREE-YUKOKETA(IDZ)
                            TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
               MOVE   WK-FREE-NAME(IDZ)
                            TO  LNK-RIHASTDAY-COM(IDY IDX)
             END-IF
           END-PERFORM
      *
           .
       22022-RIHAKAISHI-COMHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ開始日及びリハビリ開始日と同一剤のコメントを返却
      *    エリアに編集
      *****************************************************************
       22022-RIHAKAISHI-COM-201905-SEC          SECTION.
      *
           ADD          1     TO  IDX
           IF   IDX  <=  100
             ADD          1   TO  LNK-RIHASTDAY-IDX(IDY)
             MOVE        "*"  TO  LNK-RIHASTDAY-MARK(IDY IDX)
             EVALUATE    IDY
               WHEN       1
                 MOVE    25   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "心大血管疾患リハ：治療開始　　　　　年　　月　　日"
                              TO  LNK-RIHASTDAY-COM(IDY IDX)
                 MOVE    33   TO  IDC1
                 MOVE    39   TO  IDC2
                 MOVE    45   TO  IDC3
                 MOVE    29   TO  IDC4
                 PERFORM 220221-RIHA-COM-DATE-SEC
               WHEN       2
                 EVALUATE   FLG-COMOKIKAE2
                   WHEN       0
                   WHEN       3
                     MOVE    24   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "早期・初期リハ加算：発症　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    31   TO  IDC1
                     MOVE    37   TO  IDC2
                     MOVE    43   TO  IDC3
                     MOVE    27   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       1
                     MOVE    24   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "早期・初期リハ加算：手術　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    31   TO  IDC1
                     MOVE    37   TO  IDC2
                     MOVE    43   TO  IDC3
                     MOVE    27   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       2
                     MOVE    26   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "早期・初期リハ加算：急性増悪　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    35   TO  IDC1
                     MOVE    41   TO  IDC2
                     MOVE    47   TO  IDC3
                     MOVE    31   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                 END-EVALUATE
               WHEN       3
                 EVALUATE   FLG-COMOKIKAE2
                   WHEN       0
                     MOVE    23   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "脳血管疾患等リハ：発症　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    29   TO  IDC1
                     MOVE    35   TO  IDC2
                     MOVE    41   TO  IDC3
                     MOVE    25   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       1
                     MOVE    23   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "脳血管疾患等リハ：手術　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    29   TO  IDC1
                     MOVE    35   TO  IDC2
                     MOVE    41   TO  IDC3
                     MOVE    25   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       2
                     MOVE    25   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "脳血管疾患等リハ：急性増悪　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    33   TO  IDC1
                     MOVE    39   TO  IDC2
                     MOVE    45   TO  IDC3
                     MOVE    29   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       3
                     MOVE    23   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "脳血管疾患等リハ：診断　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    29   TO  IDC1
                     MOVE    35   TO  IDC2
                     MOVE    41   TO  IDC3
                     MOVE    25   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                 END-EVALUATE
               WHEN       4
                 EVALUATE   FLG-COMOKIKAE2
                   WHEN       0
                   WHEN       1
                   WHEN       3
                     MOVE    22   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "廃用症候群リハ：診断　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    27   TO  IDC1
                     MOVE    33   TO  IDC2
                     MOVE    39   TO  IDC3
                     MOVE    23   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       2
                     MOVE    24   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "廃用症候群リハ：急性増悪　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    31   TO  IDC1
                     MOVE    37   TO  IDC2
                     MOVE    43   TO  IDC3
                     MOVE    27   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                 END-EVALUATE
               WHEN       5
                 EVALUATE   FLG-COMOKIKAE2
                   WHEN       0
                   WHEN       3
                     MOVE    24   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "早期・初期リハ加算：発症　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    31   TO  IDC1
                     MOVE    37   TO  IDC2
                     MOVE    43   TO  IDC3
                     MOVE    27   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       1
                     MOVE    24   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "早期・初期リハ加算：手術　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    31   TO  IDC1
                     MOVE    37   TO  IDC2
                     MOVE    43   TO  IDC3
                     MOVE    27   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       2
                     MOVE    26   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "早期・初期リハ加算：急性増悪　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    35   TO  IDC1
                     MOVE    41   TO  IDC2
                     MOVE    47   TO  IDC3
                     MOVE    31   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                 END-EVALUATE
               WHEN       6
                 EVALUATE   FLG-COMOKIKAE2
                   WHEN       0
                     MOVE    20   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "運動器リハ：発症　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    23   TO  IDC1
                     MOVE    29   TO  IDC2
                     MOVE    35   TO  IDC3
                     MOVE    19   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       1
                     MOVE    20   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "運動器リハ：手術　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    23   TO  IDC1
                     MOVE    29   TO  IDC2
                     MOVE    35   TO  IDC3
                     MOVE    19   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       2
                     MOVE    22   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "運動器リハ：急性増悪　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    27   TO  IDC1
                     MOVE    33   TO  IDC2
                     MOVE    39   TO  IDC3
                     MOVE    23   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       3
                     MOVE    20   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "運動器リハ：診断　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    23   TO  IDC1
                     MOVE    29   TO  IDC2
                     MOVE    35   TO  IDC3
                     MOVE    19   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                 END-EVALUATE
               WHEN       7
                 MOVE    22   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "呼吸器リハ：治療開始　　　　　年　　月　　日"
                              TO  LNK-RIHASTDAY-COM(IDY IDX)
                 MOVE    27   TO  IDC1
                 MOVE    33   TO  IDC2
                 MOVE    39   TO  IDC3
                 MOVE    23   TO  IDC4
                 PERFORM 220221-RIHA-COM-DATE-SEC
               WHEN       8
                 EVALUATE   FLG-COMOKIKAE2
                   WHEN       0
                   WHEN       3
                     MOVE    24   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "早期・初期リハ加算：発症　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    31   TO  IDC1
                     MOVE    37   TO  IDC2
                     MOVE    43   TO  IDC3
                     MOVE    27   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       1
                     MOVE    24   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "早期・初期リハ加算：手術　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    31   TO  IDC1
                     MOVE    37   TO  IDC2
                     MOVE    43   TO  IDC3
                     MOVE    27   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       2
                     MOVE    26   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "早期・初期リハ加算：急性増悪　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    35  TO  IDC1
                     MOVE    41  TO  IDC2
                     MOVE    47  TO  IDC3
                     MOVE    31  TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                 END-EVALUATE
               WHEN       9
                 MOVE    23   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "摂食機能療法：治療開始　　　　　年　　月　　日"
                              TO  LNK-RIHASTDAY-COM(IDY IDX)
                 MOVE    29   TO  IDC1
                 MOVE    35   TO  IDC2
                 MOVE    41   TO  IDC3
                 MOVE    25   TO  IDC4
                 PERFORM 220221-RIHA-COM-DATE-SEC
               WHEN       10
                 EVALUATE   FLG-COMOKIKAE2
                   WHEN       0
                   WHEN       3
                     MOVE    21   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "難病患者リハ：発症　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    25   TO  IDC1
                     MOVE    31   TO  IDC2
                     MOVE    37   TO  IDC3
                     MOVE    21   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       1
                     MOVE    21   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "難病患者リハ：手術　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    25   TO  IDC1
                     MOVE    31   TO  IDC2
                     MOVE    37   TO  IDC3
                     MOVE    21   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                   WHEN       2
                     MOVE    23   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "難病患者リハ：急性増悪　　　　　年　　月　　日"
                                  TO  LNK-RIHASTDAY-COM(IDY IDX)
                     MOVE    29   TO  IDC1
                     MOVE    35   TO  IDC2
                     MOVE    41   TO  IDC3
                     MOVE    25   TO  IDC4
                     PERFORM 220221-RIHA-COM-DATE-SEC
                 END-EVALUATE
               WHEN       11
                 MOVE    23   TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
           MOVE "障害児（者）リハ：発症　　　　　年　　月　　日"
                              TO  LNK-RIHASTDAY-COM(IDY IDX)
                 MOVE    29   TO  IDC1
                 MOVE    35   TO  IDC2
                 MOVE    41   TO  IDC3
                 MOVE    25   TO  IDC4
                 PERFORM 220221-RIHA-COM-DATE-SEC
             END-EVALUATE
           END-IF
      *
      *    リハビリ開始日と同一剤のコメント
           PERFORM  VARYING  IDZ  FROM  1  BY  1
                     UNTIL  (IDZ                >  49   )
                       OR   (WK-FREE-SRYCD(IDZ) =  SPACE)
             ADD      1     TO  IDX
             IF   IDX  <=  100
               ADD      1   TO  LNK-RIHASTDAY-IDX(IDY)
               MOVE   SPACE TO  LNK-RIHASTDAY-MARK(IDY IDX)
               MOVE   WK-FREE-YUKOKETA(IDZ)
                            TO  LNK-RIHASTDAY-YUKOKETA(IDY IDX)
               MOVE   WK-FREE-NAME(IDZ)
                            TO  LNK-RIHASTDAY-COM(IDY IDX)
             END-IF
           END-PERFORM
      *
           .
       22022-RIHAKAISHI-COM-201905-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント日付編集（年月日）
      *****************************************************************
       220221-RIHA-COM-DATE-SEC          SECTION.
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
                       TO  LNK-RIHASTDAY-COM(IDY IDX)(IDC1:4)
              END-IF
      *
              MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                       TO  LNK-RIHASTDAY-COM(IDY IDX)(IDC2:4)
              END-IF
      *
              MOVE    LNK-DAY2-EDTYMD1-D   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                       TO  LNK-RIHASTDAY-COM(IDY IDX)(IDC3:4)
              END-IF
              IF   LNK-RIHASTDAY-SRYYM  >=  "201905"
                 MOVE    LNK-DAY2-EDTYMD2-G
                       TO  LNK-RIHASTDAY-COM(IDY IDX)(IDC4:4)
              END-IF
           END-IF
      *
           .
       220221-RIHA-COM-DATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ開始日指定コードの取得
      *****************************************************************
       230-RIHACDGET-HENSYU-SEC          SECTION.
      *
           IF   WRK-RIHASANTEI-FLG(1)   =  1
           OR   WRK-RIHASANTEI-FLG(2)   =  1
           OR   WRK-RIHASANTEI-FLG(3)   =  1
           OR   WRK-RIHASANTEI-FLG(4)   =  1
           OR   WRK-RIHASANTEI-FLG(5)   =  1
           OR   WRK-RIHASANTEI-FLG(6)   =  1
           OR   WRK-RIHASANTEI-FLG(7)   =  1
           OR   WRK-RIHASANTEI-FLG(8)   =  1
           OR   WRK-RIHASANTEI-FLG(9)   =  1
           OR   WRK-RIHASANTEI-FLG(10)  =  1
           OR   WRK-RIHASANTEI-FLG(11)  =  1
      *
             MOVE      2      TO     FLG-SET
             MOVE     "99"    TO     WRK-SRYKBN
             PERFORM   800-SRYACCT-DBSELECT-SEC
      *
           END-IF
      *
           .
       230-RIHACDGET-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ開始日指定コードの集計
      *****************************************************************
       2301-RIHACDGET-SYUKEI-SEC          SECTION.
      *
           PERFORM  VARYING  IDX12  FROM  1  BY  1
                     UNTIL  (IDX12            >   5   )
                       OR   (SRY-SRYCD(IDX12) =  SPACE)
      *
             IF   SRY-SRYCD(IDX12)  =    CONS-SINDAI-SITEI
             AND  WRK-RIHASANTEI-TAIHIYMD-FLG(1) = ZERO
                 MOVE   1    TO    IDW
             END-IF
             IF   SRY-SRYCD(IDX12)  =    CONS-NOUKE-SITEI
             AND  WRK-RIHASANTEI-TAIHIYMD-FLG(3) = ZERO
                 MOVE   3    TO    IDW
             END-IF
             IF   SRY-SRYCD(IDX12)  =    CONS-HAIYO-SITEI
             AND  WRK-RIHASANTEI-TAIHIYMD-FLG(4) = ZERO
                 MOVE   4    TO    IDW
             END-IF
             IF   SRY-SRYCD(IDX12)  =    CONS-UNDOKI-SITEI
             AND  WRK-RIHASANTEI-TAIHIYMD-FLG(6) = ZERO
                 MOVE   6    TO    IDW
             END-IF
             IF   SRY-SRYCD(IDX12)  =    CONS-KOKYUKI-SITEI
             AND  WRK-RIHASANTEI-TAIHIYMD-FLG(7) = ZERO
                 MOVE   7    TO    IDW
             END-IF
             IF   SRY-SRYCD(IDX12)  =    CONS-SESSYOKU-SITEI
             AND  WRK-RIHASANTEI-TAIHIYMD-FLG(9) = ZERO
                 MOVE   9    TO    IDW
             END-IF
      *
             IF   SRY-SRYCD(IDX12)  =    CONS-SINDAI-K-SITEI
             AND  WRK-RIHASANTEI-TAIHIYMD-FLG(2) = ZERO
                 MOVE   2    TO    IDW
             END-IF
             IF   SRY-SRYCD(IDX12)  =    CONS-HAIYO-K-SITEI
             AND  WRK-RIHASANTEI-TAIHIYMD-FLG(5) = ZERO
                 MOVE   5    TO    IDW
             END-IF
             IF   SRY-SRYCD(IDX12)  =    CONS-KOKYUKI-K-SITEI
             AND  WRK-RIHASANTEI-TAIHIYMD-FLG(8) = ZERO
                 MOVE   8    TO    IDW
             END-IF
      *
             IF   IDW  NOT =  ZERO
                 PERFORM   23011-RIHACDGET-SYUKEI2-SEC
             END-IF
      *
           END-PERFORM
      *
           .
       2301-RIHACDGET-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ開始日指定コードの集計２
      *****************************************************************
       23011-RIHACDGET-SYUKEI2-SEC          SECTION.
      *
           MOVE       SPACE    TO    WRK-RIHA-YMD
           MOVE       SPACE    TO    WRK-RIHA-YMD-SEI
      *
           IF    SRY-SRYCD(IDX12) = "840000061" OR "840000561"
             PERFORM  900-PTCOM-READ-SEC
      *
             IF    SRY-SRYCD(IDX12) = "840000061"
               MOVE     "4"    TO    WRK-RIHA-YMD(1:1)
             END-IF
             IF    SRY-SRYCD(IDX12) = "840000561"
               MOVE     "5"    TO    WRK-RIHA-YMD(1:1)
             END-IF
             MOVE  PTCOM-INPUTCHI1(1:2)  TO  WRK-RIHA-YMD(2:2)
             MOVE  PTCOM-INPUTCHI2(1:2)  TO  WRK-RIHA-YMD(4:2)
             MOVE  PTCOM-INPUTCHI3(1:2)  TO  WRK-RIHA-YMD(6:2)
      *
             IF    WRK-RIHA-YMD  IS  NUMERIC
               INITIALIZE                   LNK-DAY1-AREA
               INITIALIZE                   STS-AREA-DAY
               MOVE    "12"           TO    LNK-DAY1-IRAI
               MOVE    WRK-RIHA-YMD   TO    LNK-DAY1-WYMD
               CALL    "ORCSDAY"      USING STS-AREA-DAY
                                            LNK-DAY1-AREA
               IF      STS-DAY-RC1    =     ZERO
                 MOVE  LNK-DAY1-YMD   TO  WRK-RIHA-YMD-SEI
               ELSE
                 MOVE  SPACE          TO  WRK-RIHA-YMD
               END-IF
             ELSE
               MOVE    SPACE          TO  WRK-RIHA-YMD
             END-IF
           END-IF
      *
           MOVE       1     TO   WRK-RIHASANTEI-TAIHIYMD-FLG(IDW)
           IF   WRK-RIHA-YMD-SEI  NOT = SPACE
             IF   WRK-RIHA-YMD-SEI(1:6) = LNK-RIHASTDAY-SRYYM
               MOVE   WRK-RIHA-YMD-SEI(7:2)  TO  IDW2
               MOVE   1     TO   WRK-RIHASANTEI-TAIHIYMD2(IDW IDW2)
             ELSE
               MOVE   ZERO  TO   FLG-ARI2
               PERFORM  VARYING  IDW2  FROM  1  BY  1
                         UNTIL  (IDW2                     >   2  )
                          OR    (WRK-RIHASANTEI-TAIHIYMD1(IDW IDW2)
                                                          = SPACE)
                          OR    (FLG-ARI2                 =   1  )
                 IF  WRK-RIHA-YMD-SEI =
                                 WRK-RIHASANTEI-TAIHIYMD1(IDW IDW2)
                   MOVE  1  TO   FLG-ARI2
                 END-IF
               END-PERFORM
               IF    FLG-ARI2 = ZERO
               AND   IDW2    <=  2
                 MOVE  WRK-RIHA-YMD-SEI
                            TO   WRK-RIHASANTEI-TAIHIYMD1(IDW IDW2)
               END-IF
             END-IF
           END-IF
      *
           .
       23011-RIHACDGET-SYUKEI2-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタセレクト
      *****************************************************************
       800-SRYACCT-DBSELECT-SEC           SECTION.
      *
           IF   LNK-RIHASTDAY-SRYKA  =  SPACE
             INITIALIZE                             SRYACCT-REC
             MOVE    LNK-RIHASTDAY-HOSPNUM      TO  ACCT-HOSPNUM
             MOVE    LNK-RIHASTDAY-NYUGAIKBN    TO  ACCT-NYUGAIKBN
             MOVE    LNK-RIHASTDAY-PTID         TO  ACCT-PTID
             MOVE    LNK-RIHASTDAY-SRYYM        TO  ACCT-SRYYM
             MOVE    LNK-RIHASTDAY-HKNCOMBI     TO  ACCT-HKNCOMBI
             MOVE    WRK-SRYKBN                 TO  ACCT-SRYKBN
             MOVE    SRYACCT-REC                TO  MCPDATA-REC
             MOVE    "tbl_sryacct"              TO  MCP-TABLE
             MOVE    "key65"                    TO  MCP-PATHNAME
             PERFORM  900-DBSELECT-SEC
             IF       MCP-RC  =  ZERO
                MOVE    "tbl_sryacct"           TO  MCP-TABLE
                MOVE    "key65"                 TO  MCP-PATHNAME
                PERFORM  900-SRYACCT-READ-SEC
                PERFORM  UNTIL  FLG-SRYACCT =  1
      *
                  PERFORM  800-SRYACT-DBSELECT-SEC
      *
                  MOVE    "tbl_sryacct"         TO  MCP-TABLE
                  MOVE    "key65"               TO  MCP-PATHNAME
                  PERFORM  900-SRYACCT-READ-SEC
                END-PERFORM
             END-IF
             MOVE    "tbl_sryacct"              TO  MCP-TABLE
             MOVE    "key65"                    TO  MCP-PATHNAME
             PERFORM  900-CLOSE-SEC
           ELSE
             INITIALIZE                             SRYACCT-REC
             MOVE    LNK-RIHASTDAY-HOSPNUM      TO  ACCT-HOSPNUM
             MOVE    LNK-RIHASTDAY-NYUGAIKBN    TO  ACCT-NYUGAIKBN
             MOVE    LNK-RIHASTDAY-PTID         TO  ACCT-PTID
             MOVE    LNK-RIHASTDAY-SRYKA        TO  ACCT-SRYKA
             MOVE    LNK-RIHASTDAY-SRYYM        TO  ACCT-SRYYM
             MOVE    LNK-RIHASTDAY-HKNCOMBI     TO  ACCT-HKNCOMBI
             MOVE    WRK-SRYKBN                 TO  ACCT-SRYKBN
             MOVE    SRYACCT-REC                TO  MCPDATA-REC
             MOVE    "tbl_sryacct"              TO  MCP-TABLE
             MOVE    "key66"                    TO  MCP-PATHNAME
             PERFORM  900-DBSELECT-SEC
             IF       MCP-RC  =  ZERO
                MOVE    "tbl_sryacct"           TO  MCP-TABLE
                MOVE    "key66"                 TO  MCP-PATHNAME
                PERFORM  900-SRYACCT-READ-SEC
                PERFORM  UNTIL  FLG-SRYACCT =  1
      *
                  PERFORM  800-SRYACT-DBSELECT-SEC
      *
                  MOVE    "tbl_sryacct"         TO  MCP-TABLE
                  MOVE    "key66"               TO  MCP-PATHNAME
                  PERFORM  900-SRYACCT-READ-SEC
                END-PERFORM
             END-IF
             MOVE    "tbl_sryacct"              TO  MCP-TABLE
             MOVE    "key66"                    TO  MCP-PATHNAME
             PERFORM  900-CLOSE-SEC
           END-IF
      *
           .
       800-SRYACCT-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタセレクト
      *****************************************************************
       800-SRYACT-DBSELECT-SEC           SECTION.
      *
           MOVE    ZERO                  TO  IDW
      *
           INITIALIZE                        SRYACT-REC
           MOVE    ACCT-HOSPNUM          TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN        TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID             TO  SRY-PTID
           MOVE    ACCT-SRYYM            TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM           TO  SRY-ZAINUM
           MOVE    SRYACT-REC            TO  MCPDATA-REC
           MOVE    "tbl_sryact"          TO  MCP-TABLE
           MOVE    "key9"                TO  MCP-PATHNAME
           PERFORM  900-DBSELECT-SEC
           IF       MCP-RC  =  ZERO
              MOVE    "tbl_sryact"       TO  MCP-TABLE
              MOVE    "key9"             TO  MCP-PATHNAME
              PERFORM  900-SRYACT-READ-SEC
              PERFORM  UNTIL  FLG-SRYACT =  1
      *
                EVALUATE   FLG-SET
                  WHEN        1
                    PERFORM  2101-RIHASANTEI-SYUKEI-SEC
                  WHEN        2
                    PERFORM  2301-RIHACDGET-SYUKEI-SEC
                END-EVALUATE
      *
                MOVE    "tbl_sryact"     TO  MCP-TABLE
                MOVE    "key9"           TO  MCP-PATHNAME
                PERFORM  900-SRYACT-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_sryact"          TO  MCP-TABLE
           MOVE    "key9"                TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       800-SRYACT-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読み込み
      *****************************************************************
       900-HKNCOMBI-READ-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴マスタ読み込み
      *****************************************************************
       900-SANTEI-READ-SEC             SECTION.
      *
           PERFORM 900-DBFETCH-SEC
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
      *    算定履歴付加マスタ読み込み
      *****************************************************************
       900-SANTEIPLUS-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEIPLUS-REC
               MOVE    ZERO                TO  FLG-SANTEIPLUS
           ELSE
               INITIALIZE                      SANTEIPLUS-REC
               MOVE    1                   TO  FLG-SANTEIPLUS
           END-IF
      *
           .
       900-SANTEIPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ読み込み
      *****************************************************************
       900-SRYACCT-READ-SEC            SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ読み込み
      *****************************************************************
       900-SRYACT-READ-SEC             SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       900-PTCOM-READ-SEC              SECTION.
      *
           INITIALIZE                      PTCOM-REC
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    SRY-SRYCD(IDX12)    TO  PTCOM-SRYCD
           MOVE    SRY-INPUTNUM(IDX12) TO  PTCOM-RENNUM
           MOVE     PTCOM-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM  900-DBSELECT-SEC
           IF       MCP-RC             =   ZERO
               MOVE    "tbl_ptcom"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM  900-DBFETCH-SEC
               IF       MCP-RC         =   ZERO
                   MOVE   MCPDATA-REC  TO  PTCOM-REC
                   MOVE   ZERO         TO  FLG-PTCOM
               ELSE
                   INITIALIZE              PTCOM-REC
                   MOVE   1            TO  FLG-PTCOM
               END-IF
           ELSE
                   INITIALIZE              PTCOM-REC
                   MOVE   1            TO  FLG-PTCOM
           END-IF
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       900-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数テーブルの検索
      *****************************************************************
       900-TENSU-READ-SEC              SECTION.
      *
           INITIALIZE                     TNS-TENSU-REC
           MOVE     SRY-HOSPNUM      TO   TNS-HOSPNUM
           MOVE     SRY-SRYCD(IDX12) TO   TNS-SRYCD
           MOVE     SRY-SRYYM        TO   TNS-YUKOSTYMD(1:6)
           MOVE     "01"             TO   TNS-YUKOSTYMD(7:2)
           MOVE     SRY-SRYYM        TO   TNS-YUKOEDYMD(1:6)
           MOVE     "01"             TO   TNS-YUKOEDYMD(7:2)
           MOVE     TNS-TENSU-REC    TO   MCPDATA-REC
           MOVE     "tbl_tensu"      TO   MCP-TABLE
           MOVE     "key"            TO   MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF        MCP-RC          =    ZERO
             MOVE   "tbl_tensu"      TO   MCP-TABLE
             MOVE   "key"            TO   MCP-PATHNAME
             PERFORM 900-DBFETCH-SEC
             IF      MCP-RC          =    ZERO
                 MOVE   MCPDATA-REC  TO   TNS-TENSU-REC
                 MOVE   ZERO         TO   FLG-TENSU
             ELSE
                 INITIALIZE               TNS-TENSU-REC
                 MOVE    1           TO   FLG-TENSU
             END-IF
           ELSE
                 INITIALIZE               TNS-TENSU-REC
                 MOVE    1           TO   FLG-TENSU
           END-IF
           MOVE     "tbl_tensu"      TO   MCP-TABLE
           MOVE     "key"            TO   MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                 SECTION.
      *
           MOVE    "DBSELECT"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC               =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table="  MCP-TABLE
                       " pathname="         MCP-PATHNAME
           END-IF
           .
      *
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                  SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC                SECTION.
      *
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-CLOSE-SEC                    SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
