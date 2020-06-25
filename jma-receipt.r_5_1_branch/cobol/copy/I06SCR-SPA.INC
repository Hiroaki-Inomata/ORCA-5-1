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
       01  SPA-I06.
           03  SPA-I06-AREA.
               05  SPA-GMN-ALLIDOLST-PAGE          PIC 9(03).
               05  SPA-GMN-CUR                     PIC 9(03).
               05  SPA-GMN-HEAD-AREA.
                   07  SPA-GMN-PTNUM               PIC X(20).
                   07  SPA-GMN-NAME                PIC X(100).
                   07  SPA-GMN-SEX                 PIC X(4).
                   07  SPA-GMN-BIRTHDAY            PIC X(9).
                   07  SPA-GMN-AGE                 PIC X(9).
                   07  SPA-GMN-HSRYKA              PIC X(10).
                   07  SPA-GMN-HHKNCOMBI           PIC X(46).
                   07  SPA-GMN-IDOJIYU             PIC X(30).
                   07  SPA-GMN-HNYUINYMD           PIC X(10).
                   07  SPA-GMN-LBL-LASTIDOYMD      PIC X(12).
                   07  SPA-GMN-LASTIDOYMD          PIC X(10).
               05  SPA-GMN-INPUT-AREA.
                   07  SPA-GMN-BRMNUM              PIC X(6).
                   07  SPA-GMN-BRMNUM-ORG          PIC X(6).
                   07  SPA-GMN-BTUNAME-G.
                       09  SPA-GMN-BTUNAME         PIC X(2).
                       09  SPA-GMN-BTUNAMEFL       PIC X(1).
                       09  SPA-GMN-BTUNAMEMEI      PIC X(20).
                   07  SPA-GMN-BTUNAME-ORG         PIC X(2).
                   07  SPA-GMN-SAGAKU-G.
                       09  SPA-GMN-SAGAKU          PIC X(2).
                       09  SPA-GMN-SAGAKUFL        PIC X(1).
                       09  SPA-GMN-SAGAKUMEI.
                           11  SPA-GMN-SAGAKUMEI-NUM PIC X(05).
                           11  SPA-GMN-SAGAKUMEI-EN  PIC X(02).
                           11  SPA-GMN-SAGAKUMEI-FLI PIC X(13).
                   07  SPA-GMN-SENTEIKBN-G.
                       09  SPA-GMN-SENTEIKBN       PIC X(1).
                       09  SPA-GMN-SENTEIKBNFL     PIC X(1).
                       09  SPA-GMN-SENTEIKBNMEI    PIC X(22).
                   07  SPA-GMN-NYUINYMD            PIC X(10).
                   07  SPA-GMN-NYUINKA-G.
                       09  SPA-GMN-NYUINKA         PIC X(02).
                       09  SPA-GMN-NYUINKAFL       PIC X(01).
                       09  SPA-GMN-NYUINKAMEI      PIC X(24).
                   07  SPA-GMN-SHOKAI-G.
                       09  SPA-GMN-SHOKAI          PIC X(01).
                       09  SPA-GMN-SHOKAIFL        PIC X(01).
                       09  SPA-GMN-SHOKAIMEI       PIC X(06).
                   07  SPA-GMN-SHONUM              PIC S9(2).
                   07  SPA-GMN-LBL-SHONYUINYMD     PIC X(16).
                   07  SPA-GMN-KAIGOKBN-G.
                       09  SPA-GMN-KAIGOKBN        PIC X(1).
                       09  SPA-GMN-KAIGOKBNFL      PIC X(1).
                       09  SPA-GMN-KAIGOKBNMEI     PIC X(16).
                   07  SPA-GMN-TAIINYMD            PIC X(10).
                   07  SPA-GMN-TAIINCD-G.
                       09  SPA-GMN-TAIINCD         PIC X(02).
                       09  SPA-GMN-TAIINCDFL       PIC X(01).
                       09  SPA-GMN-TAIINCDMEI      PIC X(20).
                   07  SPA-GMN-DR-G                OCCURS  3.
                       09  SPA-GMN-DR              PIC X(04).
                       09  SPA-GMN-DRFL            PIC X(01).
                       09  SPA-GMN-DRMEI           PIC X(22).
                   07  SPA-GMN-HKNCOMBI-G.
                       09  SPA-GMN-HKNCOMBI        PIC X(04).
                       09  SPA-GMN-HKNCOMBIFL      PIC X(01).
                       09  SPA-GMN-HKNCOMBIMEI     PIC X(46).
                   07  SPA-GMN-BUNBENKBN-G.
                       09  SPA-GMN-BUNBENKBN       PIC X(01).
                       09  SPA-GMN-BUNBENKBNFL     PIC X(01).
                       09  SPA-GMN-BUNBENKBNMEI    PIC X(18).
                   07  SPA-GMN-RIYOUKBN-G.
                       09  SPA-GMN-RIYOUKBN        PIC X(01).
                       09  SPA-GMN-RIYOUKBNFL      PIC X(01).
                       09  SPA-GMN-RIYOUKBNMEI     PIC X(18).
                   07  SPA-GMN-NYUKBN-G.
                       09  SPA-GMN-NYUKBN          PIC X(02).
                       09  SPA-GMN-NYUKBNFL        PIC X(01).
                       09  SPA-GMN-NYUKBNMEI       PIC X(08).
                   07  SPA-GMN-TOKNYUIN-G.
                       09  SPA-GMN-TOKNYUIN        PIC X(02).
                       09  SPA-GMN-TOKNYUINFL      PIC X(01).
                       09  SPA-GMN-TOKNYUINMEI     PIC X(60).
                   07  SPA-GMN-TOKHIGAITOKBN-G.
                       09  SPA-GMN-TOKHIGAITOKBN     PIC X(1).
                       09  SPA-GMN-TOKHIGAITOKBNFL   PIC X(1).
                       09  SPA-GMN-TOKHIGAITOKBNMEI  PIC X(28).
                   07  SPA-GMN-SNTNYUIN            PIC X(60).
                   07  SPA-GMN-NACCTKBN-G.
                       09  SPA-GMN-NACCTKBN        PIC X(1).
                       09  SPA-GMN-NACCTKBNFL      PIC X(1).
                       09  SPA-GMN-NACCTKBNMEI     PIC X(28).
                   07  SPA-GMN-KSNCD-G         OCCURS 3.
                       09  SPA-GMN-KSNCD       PIC X(02).
                       09  SPA-GMN-KSNCDFL     PIC X(1).
                       09  SPA-GMN-KSNCDMEI    PIC X(70).
                   07  SPA-GMN-TEIKIKBN-G.
                       09  SPA-GMN-TEIKIKBN        PIC X(1).
                       09  SPA-GMN-TEIKIKBNFL      PIC X(1).
                       09  SPA-GMN-TEIKIKBNMEI     PIC X(22).
                   07  SPA-GMN-DISPKBN-G.
                       09  SPA-GMN-DISPKBN         PIC X(1).
                       09  SPA-GMN-DISPKBNFL       PIC X(1).
                       09  SPA-GMN-DISPKBNMEI      PIC X(08).
                   07  SPA-GMN-SKJTIME-G.
                       09  SPA-GMN-SKJTIME         PIC X(02).
                       09  SPA-GMN-SKJTIMEFL       PIC X(01).
                       09  SPA-GMN-SKJTIMEMEI      PIC X(08).
                   07  SPA-GMN-NISSU               PIC X(100).
               05  SPA-GMN-ALLIDOLIST-INPUT-AREA.
                   07  SPA-GMN-SELNUM              PIC S9(4).
                   07  SPA-GMN-SELUKEYMD           PIC X(10).
                   07  SPA-GMN-SELIDOYMD           PIC X(10).
                   07  SPA-GMN-SELPTNUM            PIC X(20).
                   07  SPA-GMN-SELNAME             PIC X(50).
                   07  SPA-GMN-SELSTS-G.
                       09  SPA-GMN-SELSTS          PIC X(01).
                       09  SPA-GMN-SELSTSFL        PIC X(01).
                       09  SPA-GMN-SELSTSMEI       PIC X(20).
                   07  SPA-GMN-SELSORT-G.
                       09  SPA-GMN-SELSORT         PIC X(02).
                       09  SPA-GMN-SELSORTFL       PIC X(01).
                       09  SPA-GMN-SELSORTMEI      PIC X(50).
               05  SPA-GMN-ALLIDOLST-AREA.
                   07  SPA-GMN-ALLIDOLST-NEXT      PIC 9(01).
                   07  SPA-GMN-ALLIDOLST-MAX       PIC  9(5).
                   07  SPA-GMN-ALLIDOLST-SEL       PIC  9(5).
                   07  SPA-GMN-ALLIDOLST-OCC       OCCURS  100.
                       09  SPA-GMN-TALLNUM         PIC  9(5).
                       09  SPA-GMN-TALLUKEYMD-G.
                           11  SPA-GMN-TALLUKEYMD  PIC X(10).
                           11  SPA-GMN-TALLUKEHMS  PIC X(05).
                       09  SPA-GMN-TALLIDOYMD      PIC X(10).
                       09  SPA-GMN-TALLIDOJIYU     PIC X(20).
                       09  SPA-GMN-TALLPTNUM       PIC X(20).
                       09  SPA-GMN-TALLNAME        PIC X(50).
                       09  SPA-GMN-TALLSTS         PIC 9(1).
                       09  SPA-GMN-TALLERRCD       PIC X(4).
               05  SPA-GMN-PTIDOLST-AREA.
                   07  SPA-GMN-PTIDOLST-MAX       PIC  9(5).
                   07  SPA-GMN-PTIDOLST-SEL       PIC  9(5).
                   07  SPA-GMN-PTIDOLST-OCC       OCCURS  50.
                       09  SPA-GMN-TPTUKEYMD-G.
                           11  SPA-GMN-TPTUKEYMD  PIC X(10).
                           11  SPA-GMN-TPTUKEHMS  PIC X(05).
                       09  SPA-GMN-TPTIDOYMD      PIC X(10).
                       09  SPA-GMN-TPTIDOJIYU     PIC X(20).
                       09  SPA-GMN-TPTSTS         PIC 9(1).
               05  SPA-GMN-ERRMSGLST-AREA.
                   07  SPA-GMN-ERRMSGLST-MAX       PIC  9(5).
                   07  SPA-GMN-ERRMSGLST-SEL       PIC  9(5).
                   07  SPA-GMN-ERRMSGLST-OCC       OCCURS  10.
                       09  SPA-GMN-TERRCD          PIC X(04).
                       09  SPA-GMN-TERRMSG         PIC X(100).
               05  SPA-GMN-ALLIDOLIST-COMBO-AREA.
                   07  SPA-GMN-SELSTSLST-G.
                       09  SPA-GMN-SELSTS-MAX          PIC 9(05).
                       09  SPA-GMN-SELSTSLST           OCCURS  5.
                           11  SPA-GMN-SELSTSL         PIC X(1).
                           11  SPA-GMN-SELSTSFLL       PIC X(1).
                           11  SPA-GMN-SELSTSMEIL      PIC X(20).
                   07  SPA-GMN-SELSORTLST-G.
                       09  SPA-GMN-SELSORT-MAX          PIC 9(05).
                       09  SPA-GMN-SELSORTLST           OCCURS 10.
                           11  SPA-GMN-SELSORTL         PIC X(2).
                           11  SPA-GMN-SELSORTFLL       PIC X(1).
                           11  SPA-GMN-SELSORTMEIL      PIC X(50).
               05  SPA-GMN-COMBO-AREA.
                   07  SPA-GMN-BTUNAMELST-G.
                       09  SPA-GMN-BTUNAME-MAX     PIC 9(05).
                       09  SPA-GMN-BTUNAMELST      OCCURS  50.
                           11  SPA-GMN-BTUNAMEL    PIC X(02).
                           11  SPA-GMN-BTUNAMEFLL  PIC X(01).
                           11  SPA-GMN-BTUNAMEMEIL PIC X(20).
                   07  SPA-GMN-SAGAKULST-G.
                       09  SPA-GMN-SAGAKU-MAX      PIC 9(05).
                       09  SPA-GMN-SAGAKULST       OCCURS  99.
                           11  SPA-GMN-SAGAKUL     PIC X(02).
                           11  SPA-GMN-SAGAKUFLL   PIC X(01).
                           11  SPA-GMN-SAGAKUMEIL  PIC X(20).
                   07  SPA-GMN-SENTEIKBNLST-G.
                       09  SPA-GMN-SENTEIKBN-MAX      PIC 9(05).
                       09  SPA-GMN-SENTEIKBNLST       OCCURS  10.
                           11  SPA-GMN-SENTEIKBNL     PIC X(01).
                           11  SPA-GMN-SENTEIKBNFLL   PIC X(01).
                           11  SPA-GMN-SENTEIKBNMEIL  PIC X(22).
                   07  SPA-GMN-NYUINKALST-G.
                       09  SPA-GMN-NYUINKA-MAX     PIC 9(05).
                       09  SPA-GMN-NYUINKALST      OCCURS  99.
                           11  SPA-GMN-NYUINKAL    PIC X(02).
                           11  SPA-GMN-NYUINKAFLL  PIC X(01).
                           11  SPA-GMN-NYUINKAMEIL PIC X(24).
                   07  SPA-GMN-SHOKAILST-G.
                       09  SPA-GMN-SHOKAI-MAX      PIC 9(05).
                       09  SPA-GMN-SHOKAILST       OCCURS  5.
                           11  SPA-GMN-SHOKAIL     PIC X(01).
                           11  SPA-GMN-SHOKAIFLL   PIC X(01).
                           11  SPA-GMN-SHOKAIMEIl  PIC X(06).
                   07  SPA-GMN-KAIGOKBNLST-G.
                       09  SPA-GMN-KAIGOKBN-MAX      PIC 9(05).
                       09  SPA-GMN-KAIGOKBNLST       OCCURS  5.
                           11  SPA-GMN-KAIGOKBNL     PIC X(01).
                           11  SPA-GMN-KAIGOKBNFLL   PIC X(01).
                           11  SPA-GMN-KAIGOKBNMEIL  PIC X(16).
                   07  SPA-GMN-TAIINCDLST-G.
                       09  SPA-GMN-TAIINCD-MAX     PIC 9(05).
                       09  SPA-GMN-TAIINCDLST      OCCURS  50.
                           11  SPA-GMN-TAIINCDL    PIC X(02).
                           11  SPA-GMN-TAIINCDFLL  PIC X(01).
                           11  SPA-GMN-TAIINCDMEIL PIC X(20).
                   07  SPA-GMN-DRLST-G.
                       09  SPA-GMN-DR-MAX          PIC 9(05).
                       09  SPA-GMN-DRLST           OCCURS  99.
                           11  SPA-GMN-DRL         PIC X(04).
                           11  SPA-GMN-DRFLL       PIC X(01).
                           11  SPA-GMN-DRMEIL      PIC X(22).
                   07  SPA-GMN-HKNCOMBILST-G.
                       09  SPA-GMN-HKNCOMBI-MAX        PIC 9(05).
                       09  SPA-GMN-HKNCOMBILST         OCCURS  30.
                           11  SPA-GMN-HKNCOMBIL       PIC X(04).
                           11  SPA-GMN-HKNCOMBIFLL     PIC X(01).
                           11  SPA-GMN-HKNCOMBIMEIL    PIC X(46).
                   07  SPA-GMN-BUNBENKBNLST-G.
                       09  SPA-GMN-BUNBENKBN-MAX       PIC 9(05).
                       09  SPA-GMN-BUNBENKBNLST        OCCURS  5.
                           11  SPA-GMN-BUNBENKBNL      PIC X(01).
                           11  SPA-GMN-BUNBENKBNFLL    PIC X(01).
                           11  SPA-GMN-BUNBENKBNMEIL   PIC X(18).
                   07  SPA-GMN-RIYOUKBNLST-G.
                       09  SPA-GMN-RIYOUKBN-MAX        PIC 9(05).
                       09  SPA-GMN-RIYOUKBNLST         OCCURS  5.
                           11  SPA-GMN-RIYOUKBNL       PIC X(01).
                           11  SPA-GMN-RIYOUKBNFLL     PIC X(01).
                           11  SPA-GMN-RIYOUKBNMEIL    PIC X(18).
                   07  SPA-GMN-NYUKBNLST-G.
                       09  SPA-GMN-NYUKBN-MAX          PIC 9(05).
                       09  SPA-GMN-NYUKBNLST           OCCURS  2.
                           11  SPA-GMN-NYUKBNL         PIC X(2).
                           11  SPA-GMN-NYUKBNFLL       PIC X(1).
                           11  SPA-GMN-NYUKBNMEIL      PIC X(8).
                   07  SPA-GMN-TOKNYUINLST-G.
                       09  SPA-GMN-TOKNYUIN-SRYCDTTL   PIC 9(14).
                       09  SPA-GMN-TOKNYUIN-MAX        PIC 9(05).
                       09  SPA-GMN-TOKNYUINLST-OCC     OCCURS  100.
                           11  SPA-GMN-TOKNYUINLST.
                               13  SPA-GMN-TOKNYUINL       PIC X(02).
                               13  SPA-GMN-TOKNYUINFLL     PIC X(01).
                               13  SPA-GMN-TOKNYUINMEIL    PIC X(60).
                           11  SPA-GMN-TOKNYUINL2.
                               13  SPA-GMN-TOKNYUINL2-KBN    PIC X(01).
                               13  SPA-GMN-TOKNYUINL2-NYUIN  PIC X(02).
                           11  SPA-GMN-TOKNYUINL-NYUINRYOKBN
                                                           PIC X(02).
                           11  SPA-GMN-TOKNYUINL-SRYCD     PIC X(09).
                           11  SPA-GMN-TOKNYUINL-NASRYCD   PIC X(09).
                   07  SPA-GMN-TOKHIGAITOKBNLST-G.
                       09  SPA-GMN-TOKHIGAITOKBN-MAX      PIC 9(05).
                       09  SPA-GMN-TOKHIGAITOKBNLST       OCCURS  5.
                           11  SPA-GMN-TOKHIGAITOKBNL     PIC X(01).
                           11  SPA-GMN-TOKHIGAITOKBNFLL   PIC X(01).
                           11  SPA-GMN-TOKHIGAITOKBNMEIL  PIC X(28).
                   07  SPA-GMN-NACCTKBNLST-G.
                       09  SPA-GMN-NACCTKBN-MAX        PIC 9(05).
                       09  SPA-GMN-NACCTKBNLST         OCCURS  5.
                           11  SPA-GMN-NACCTKBNL       PIC X(01).
                           11  SPA-GMN-NACCTKBNFLL     PIC X(01).
                           11  SPA-GMN-NACCTKBNMEIL    PIC X(28).
                   07  SPA-GMN-KSNCDLST-G.
                       09  SPA-GMN-KSNCD-MAX        PIC 9(05).
                       09  SPA-GMN-KSNCDLST-OCC         OCCURS  20.
                           11  SPA-GMN-KSNCDLST.
                               13  SPA-GMN-KSNCDL       PIC X(02).
                               13  SPA-GMN-KSNCDFLL     PIC X(01).
                               13  SPA-GMN-KSNCDMEIL    PIC X(70).
                           11  SPA-GMN-KSNCDL-SRYCD     PIC X(09).
                   07  SPA-GMN-TEIKIKBNLST-G.
                       09  SPA-GMN-TEIKIKBN-MAX        PIC 9(05).
                       09  SPA-GMN-TEIKIKBNLST         OCCURS  5.
                           11  SPA-GMN-TEIKIKBNL       PIC X(01).
                           11  SPA-GMN-TEIKIKBNFLL     PIC X(01).
                           11  SPA-GMN-TEIKIKBNMEIL    PIC X(22).
                   07  SPA-GMN-DISPKBNLST-G.
                       09  SPA-GMN-DISPKBN-MAX         PIC 9(05).
                       09  SPA-GMN-DISPKBNLST          OCCURS  5.
                           11  SPA-GMN-DISPKBNL        PIC X(01).
                           11  SPA-GMN-DISPKBNFLL      PIC X(01).
                           11  SPA-GMN-DISPKBNMEIL     PIC X(08).
                   07  SPA-GMN-SKJTIMELST-G.
                       09  SPA-GMN-SKJTIME-MAX         PIC 9(05).
                       09  SPA-GMN-SKJTIMELST          OCCURS  5.
                           11  SPA-GMN-SKJTIMEL        PIC X(2).
                           11  SPA-GMN-SKJTIMEFLL      PIC X(1).
                           11  SPA-GMN-SKJTIMEMEIL     PIC X(8).
               05  SPA-NAI-KJNYMD                      PIC X(08).
               05  SPA-NAI-HEAD-AREA.
                   07  SPA-NAI-KARTE-KEY           PIC X(36).
                   07  SPA-NAI-ORDERNUM            PIC 9(02).
                   07  SPA-NAI-STATUS-ORDER        PIC 9(01).
                   07  SPA-NAI-HIDOYMD             PIC X(08).
                   07  SPA-NAI-ORDERID             PIC X(04).
                   07  SPA-NAI-PTID                PIC 9(10).
                   07  SPA-NAI-SEX                 PIC X(01).
                   07  SPA-NAI-BIRTHDAY            PIC X(08).
                   07  SPA-NAI-HSRYKA              PIC X(02).
                   07  SPA-NAI-HHKNCOMBI           PIC X(04).
                   07  SPA-NAI-HNYUINYMD           PIC X(08).
                   07  SPA-NAI-LASTIDOYMD          PIC X(08).
                   07  SPA-NAI-BUNBENKBN           PIC X(01).
                   07  SPA-NAI-DIRECT-KHNERR       PIC 9(01).
                   07  SPA-NAI-ORDER-DAT           PIC X(1000).
                   07  SPA-NAI-INF-CHK             PIC 9(05).
               05  SPA-NAI-INPUT-AREA.
                   07  SPA-NAI-STATUS-G.
                       09  SPA-NAI-STS-NYUKBN      PIC 9(01).
                       09  SPA-NAI-STS-BRMNUM      PIC 9(01).
                       09  SPA-NAI-STS-BTUNUM      PIC 9(01).
                       09  SPA-NAI-STS-SAGAKU      PIC 9(01).
                       09  SPA-NAI-STS-SHOKAI      PIC 9(01).
                       09  SPA-NAI-STS-TAIINCD     PIC 9(01).
                       09  SPA-NAI-STS-DR          PIC 9(01)
                                                   OCCURS 3.
                       09  SPA-NAI-STS-TOKNYUIN    PIC 9(01).
                       09  SPA-NAI-STS-HKNCOMBI    PIC 9(01).
                       09  SPA-NAI-STS-NYUINKA     PIC 9(01).
                       09  SPA-NAI-STS-SKJTIME     PIC 9(01).
                       09  SPA-NAI-STS-NISSU       PIC 9(01).
                   07  SPA-NAI-BTU-G.
                       09  SPA-NAI-BTU-KHNSRYCD    PIC X(09).
                       09  SPA-NAI-BTU-SBT         PIC X(02).
                       09  SPA-NAI-BTU-BTUTOK      PIC X(02).
                       09  SPA-NAI-BTU-BRMTOK      PIC X(02).
                       09  SPA-NAI-BTU-BTUKHNKBNCD PIC X(03).
                       09  SPA-NAI-BTU-BRMKHNKBNCD PIC X(03).
                   07  SPA-NAI-DIFF-INIT           PIC X(01).
                   07  SPA-NAI-NYUINYMD            PIC X(08).
                   07  SPA-NAI-SHONYUINYMD         PIC X(08).
                   07  SPA-NAI-TAIINYMD            PIC X(08).
                   07  SPA-NAI-TOKUTEI-G.
                       09  SPA-NAI-TOKUTEI-KBN     PIC X(01).
                       09  SPA-NAI-TOKUTEI-NYUIN   PIC X(02).
                   07  SPA-NAI-TOKSRYCD            PIC X(09).
                   07  SPA-NAI-NATOKSRYCD          PIC X(09).
                   07  SPA-NAI-TOKCD-G.
                       09  SPA-NAI-TOKCD-KBN       PIC X(01).
                       09  SPA-NAI-TOKCD-NYUIN     PIC X(02).
                   07  SPA-NAI-KHNNYUIN            PIC X(60).
                   07  SPA-NAI-KHNSRYCD            PIC X(09).
                   07  SPA-NAI-SNTSRYCD            PIC X(09).
                   07  SPA-NAI-SELNYUKHN           PIC 9(01).
                   07  SPA-NAI-HKNNUM              PIC X(03).
                   07  SPA-NAI-BTUNUM-MOT          PIC X(02).
                   07  SPA-NAI-BRMNUM-MOT          PIC X(06).
                   07  SPA-NAI-HKNCOMBI-MOT        PIC X(04).
                   07  SPA-NAI-KSNCD-G.
                       09  SPA-NAI-KSNCD-OCC       OCCURS 3.
                           11  SPA-NAI-KSNCD-SRYCD PIC X(09).
                   07  SPA-NAI-DTYPE               PIC X(01).
                   07  SPA-NAI-INPUT-COMB-SRYKA    PIC X(01).
               05  SPA-NAI-ALLIDOLIST-INPUT-AREA.
                   07  SPA-NAI-SELUKEYMD           PIC X(08).
                   07  SPA-NAI-SELIDOYMD           PIC X(08).
                   07  SPA-NAI-SELPTID             PIC 9(10).
               05  SPA-NAI-ALLIDOLST-AREA.
                   07  SPA-NAI-ALLIDOLST-OCC       OCCURS  100.
                       09  SPA-NAI-TALLHOSPNUM     PIC 9(02).
                       09  SPA-NAI-TALLKARTE-KEY   PIC X(36).
                       09  SPA-NAI-TALLORDERNUM    PIC 9(02).
                       09  SPA-NAI-TALLUKEYMD      PIC X(08).
                       09  SPA-NAI-TALLIDOYMD      PIC X(08).
                       09  SPA-NAI-TALLORDERID     PIC X(04).
                       09  SPA-NAI-TALLPTID        PIC 9(10).
               05  SPA-NAI-PTIDOLST-AREA.
                   07  SPA-NAI-PTIDOLST-OCC       OCCURS  50.
                       09  SPA-NAI-TPTKARTE-KEY   PIC X(36).
                       09  SPA-NAI-TPTORDERNUM    PIC 9(02).
                       09  SPA-NAI-TPTUKEYMD      PIC X(10).
                       09  SPA-NAI-TPTIDOYMD      PIC X(10).
                       09  SPA-NAI-TPTORDERID     PIC X(04).
               05  SPA-NAI-BTUDEF-AREA.
                   07  SPA-NAI-BTUDEF-MAX          PIC 9(05).
                   07  SPA-NAI-BTUDEF-OCC          OCCURS  50.
                       09  SPA-NAI-BTUDEF-BTUNUM   PIC X(02).
                       09  SPA-NAI-BTUDEF-SAGAKU   PIC X(02).
                       09  SPA-NAI-BTUDEF-NYUINKA  PIC X(02).
                       09  SPA-NAI-BTUDEF-KHNSRYCD PIC X(09).
                       09  SPA-NAI-BTUDEF-BTUTOK   PIC X(02).
                       09  SPA-NAI-BTUDEF-BRMTOK   PIC X(02).
                       09  SPA-NAI-BTUDEF-SBT      PIC X(02).
                       09  SPA-NAI-BTUDEF-BTUKHNKBNCD
                                                   PIC X(03).
                       09  SPA-NAI-BTUDEF-BRMKHNKBNCD
                                                   PIC X(03).
               05  SPA-NAI-COMBO-AREA.
                   07  SPA-NAI-HKNCOMBILST-G.
                       09  SPA-NAI-HKNCOMBILST     OCCURS  30.
                           11  SPA-NAI-HKNNUML     PIC X(03).
