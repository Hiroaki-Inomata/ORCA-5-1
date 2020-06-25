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
       01  QSUB03-LNK.
           03  QSUB03-REQUEST                              PIC X(01).
           03  QSUB03-BASEYMD                              PIC X(08).
           03  QSUB03-SRYYMD-G.
               05  QSUB03-STSRYYMD                         PIC X(08).
               05  QSUB03-EDSRYYMD                         PIC X(08).
           03  QSUB03-ITEM-LIST-AREA.
               05  QSUB03-SELPATTERNLST-G.
                   07  QSUB03-SELPATTERNLST-MAX            PIC 9(05).
                   07  QSUB03-SELPATTERNLST-OCC            OCCURS  50.
                       09  QSUB03-SELPATTERNLST.
                           11  QSUB03-SELPATTERNL          PIC X(02).
                           11  QSUB03-SELPATTERNFLL        PIC X(01).
                           11  QSUB03-SELPATTERNMEIL       PIC X(100).
                       09  QSUB03-SELPATTERN-UIDL          PIC X(36).
                       09  QSUB03-SELPATTERN-TTLKBNL       PIC X(01).
               05  QSUB03-SEXLST-G.
                   07  QSUB03-SEXLST-MAX                   PIC 9(05).
                   07  QSUB03-SEXLST                       OCCURS  3.
                       09  QSUB03-SEXL                     PIC X(01).
                       09  QSUB03-SEXFLL                   PIC X(01).
                       09  QSUB03-SEXMEIL                  PIC X(08).
               05  QSUB03-NYUGAIKBNLST-G.
                   07  QSUB03-NYUGAIKBNLST-MAX             PIC 9(05).
                   07  QSUB03-NYUGAIKBNLST                 OCCURS  2.
                       09  QSUB03-NYUGAIKBNL               PIC X(01).
                       09  QSUB03-NYUGAIKBNFLL             PIC X(01).
                       09  QSUB03-NYUGAIKBNMEIL            PIC X(10).
               05  QSUB03-DRCDLST-G.
                   07  QSUB03-DRCDLST-MAX                    PIC 9(05).
                   07  QSUB03-DRCDLST                        OCCURS  99.
                       09  QSUB03-DRCDL                      PIC X(04).
                       09  QSUB03-DRCDFLL                    PIC X(01).
                       09  QSUB03-DRCDMEIL                   PIC X(24).
               05  QSUB03-SRYKALST-G.
                   07  QSUB03-SRYKALST-MAX                 PIC 9(05).
                   07  QSUB03-SRYKALST                     OCCURS  50.
                       09  QSUB03-SRYKAL                   PIC X(02).
                       09  QSUB03-SRYKAFLL                 PIC X(01).
                       09  QSUB03-SRYKAMEIL                PIC X(20).
               05  QSUB03-DEATHLST-G.
                   07  QSUB03-DEATHLST-MAX                 PIC 9(05).
                   07  QSUB03-DEATHLST                     OCCURS  2.
                       09  QSUB03-DEATHL                   PIC X(01).
                       09  QSUB03-DEATHFLL                 PIC X(01).
                       09  QSUB03-DEATHMEIL                PIC X(08).
               05  QSUB03-AGREEMENTLST-G.
                   07  QSUB03-AGREEMENTLST-MAX             PIC 9(05).
                   07  QSUB03-AGREEMENTLST                 OCCURS  10.
                       09  QSUB03-AGREEMENTL               PIC X(01).
                       09  QSUB03-AGREEMENTFLL             PIC X(01).
                       09  QSUB03-AGREEMENTMEIL            PIC X(20).
               05  QSUB03-TSTPTLST-G.
                   07  QSUB03-TSTPTLST-MAX                 PIC 9(05).
                   07  QSUB03-TSTPTLST                     OCCURS  10.
                       09  QSUB03-TSTPTL                   PIC X(01).
                       09  QSUB03-TSTPTFLL                 PIC X(01).
                       09  QSUB03-TSTPTMEIL                PIC X(30).
               05  QSUB03-CONDITION1LST-G.
                   07  QSUB03-CONDITION1LST-MAX            PIC 9(05).
                   07  QSUB03-CONDITION1LST                OCCURS  100.
                       09  QSUB03-CONDITION1L              PIC X(02).
                       09  QSUB03-CONDITION1FLL            PIC X(01).
                       09  QSUB03-CONDITION1MEIL           PIC X(20).
               05  QSUB03-CONDITION2LST-G.
                   07  QSUB03-CONDITION2LST-MAX            PIC 9(05).
                   07  QSUB03-CONDITION2LST                OCCURS  100.
                       09  QSUB03-CONDITION2L              PIC X(02).
                       09  QSUB03-CONDITION2FLL            PIC X(01).
                       09  QSUB03-CONDITION2MEIL           PIC X(20).
               05  QSUB03-CONDITION3LST-G.
                   07  QSUB03-CONDITION3LST-MAX            PIC 9(05).
                   07  QSUB03-CONDITION3LST                OCCURS  100.
                       09  QSUB03-CONDITION3L              PIC X(02).
                       09  QSUB03-CONDITION3FLL            PIC X(01).
                       09  QSUB03-CONDITION3MEIL           PIC X(20).
               05  QSUB03-SELCOMMENTLST-G.
                   07  QSUB03-SELCOMMENTLST-MAX            PIC 9(05).
                   07  QSUB03-SELCOMMENTLST                OCCURS  5.
                       09  QSUB03-SELCOMMENTL              PIC X(01).
                       09  QSUB03-SELCOMMENTFLL            PIC X(01).
                       09  QSUB03-SELCOMMENTMEIL           PIC X(38).
               05  QSUB03-TEL-TOLST-G.
                   07  QSUB03-TEL-TOLST-MAX                PIC 9(05).
                   07  QSUB03-TEL-TOLST                    OCCURS  5.
                       09  QSUB03-TEL-TOL                  PIC X(01).
                       09  QSUB03-TEL-TOFLL                PIC X(01).
                       09  QSUB03-TEL-TOMEIL               PIC X(18).
               05  QSUB03-TOKKILST-G.
                   07  QSUB03-TOKKILST-MAX                 PIC 9(05).
                   07  QSUB03-TOKKILST                     OCCURS  50.
                       09  QSUB03-TOKKIL                   PIC X(02).
                       09  QSUB03-TOKKIFLL                 PIC X(01).
                       09  QSUB03-TOKKIMEIL                PIC X(30).
               05  QSUB03-TOKKIINFOLST-G.
                   07  QSUB03-TOKKIINFOLST-MAX             PIC 9(05).
                   07  QSUB03-TOKKIINFOLST                 OCCURS  50.
                       09  QSUB03-TOKKIINFOL               PIC X(02).
                       09  QSUB03-TOKKIINFOFLL             PIC X(01).
                       09  QSUB03-TOKKIINFOMEIL            PIC X(30).
               05  QSUB03-SAIGAIKBNLST-G.
                   07  QSUB03-SAIGAIKBNLST-MAX             PIC 9(05).
                   07  QSUB03-SAIGAIKBNLST                 OCCURS  10.
                       09  QSUB03-SAIGAIKBNL               PIC X(01).
                       09  QSUB03-SAIGAIKBNFLL             PIC X(01).
                       09  QSUB03-SAIGAIKBNMEIL            PIC X(20).
               05  QSUB03-TENKENRECELST-G.
                   07  QSUB03-TENKENRECELST-MAX            PIC 9(05).
                   07  QSUB03-TENKENRECELST                OCCURS  5.
                       09  QSUB03-TENKENRECEL              PIC X(01).
                       09  QSUB03-TENKENRECEFLL            PIC X(01).
                       09  QSUB03-TENKENRECEMEIL           PIC X(20).
               05  QSUB03-HONKZKKBNLST-G.
                   07  QSUB03-HONKZKKBNLST-MAX             PIC 9(05).
                   07  QSUB03-HONKZKKBNLST                 OCCURS  5.
                       09  QSUB03-HONKZKKBNL               PIC X(01).
                       09  QSUB03-HONKZKKBNFLL             PIC X(01).
                       09  QSUB03-HONKZKKBNMEIL            PIC X(08).
               05  QSUB03-FTNRATELST-G.
                   07  QSUB03-FTNRATELST-MAX               PIC 9(05).
                   07  QSUB03-FTNRATELST                   OCCURS  10.
                       09  QSUB03-FTNRATEL                 PIC X(01).
                       09  QSUB03-FTNRATEFLL               PIC X(01).
                       09  QSUB03-FTNRATEMEIL              PIC X(34).
               05  QSUB03-HKNNUMLST-G.
                   07  QSUB03-HKNNUMLST-MAX                PIC 9(05).
                   07  QSUB03-HKNNUMLST                    OCCURS  50.
                       09  QSUB03-HKNNUML                  PIC X(03).
                       09  QSUB03-HKNNUMFLL                PIC X(01).
                       09  QSUB03-HKNNUMMEIL               PIC X(26).
               05  QSUB03-KOHNUMLST-G.
                   07  QSUB03-KOHNUMLST-MAX                PIC 9(05).
                   07  QSUB03-KOHNUMLST-OCC                OCCURS 150.
                       09  QSUB03-KOHNUMLST.
                           11  QSUB03-KOHNUML              PIC X(03).
                           11  QSUB03-PAYKBNL              PIC X(01).
                           11  QSUB03-KOHNUMMEIL           PIC X(16).
                       09  QSUB03-KOHNUM-TYPEL             PIC X(01).
               05  QSUB03-UTAGAIKBNLST-G.
                   07  QSUB03-UTAGAIKBNLST-MAX             PIC 9(05).
                   07  QSUB03-UTAGAIKBNLST                 OCCURS 3.
                       09  QSUB03-UTAGAIKBNL               PIC X(1).
                       09  QSUB03-UTAGAIKBNFLL             PIC X(1).
                       09  QSUB03-UTAGAIKBNMEIL            PIC X(18).
               05  QSUB03-KYUSEIKBNLST-G.
                   07  QSUB03-KYUSEIKBNLST-MAX             PIC 9(05).
                   07  QSUB03-KYUSEIKBNLST                 OCCURS 3.
                       09  QSUB03-KYUSEIKBNL               PIC X(1).
                       09  QSUB03-KYUSEIKBNFLL             PIC X(1).
                       09  QSUB03-KYUSEIKBNMEIL            PIC X(18).
               05  QSUB03-TENKIKBNLST-G.
                   07  QSUB03-TENKIKBNLST-MAX              PIC 9(05).
                   07  QSUB03-TENKIKBNLST                  OCCURS 10.
                       09  QSUB03-TENKIKBNL                PIC X(1).
                       09  QSUB03-TENKIKBNFLL              PIC X(1).
                       09  QSUB03-TENKIKBNMEIL             PIC X(18).
               05  QSUB03-SYUBYOKBNLST-G.
                   07  QSUB03-SYUBYOKBNLST-MAX             PIC 9(05).
                   07  QSUB03-SYUBYOKBNLST                 OCCURS 5.
                       09  QSUB03-SYUBYOKBNL               PIC X(1).
                       09  QSUB03-SYUBYOKBNFLL             PIC X(1).
                       09  QSUB03-SYUBYOKBNMEIL            PIC X(18).
               05  QSUB03-UBYOMEILST-G.
                   07  QSUB03-UBYOMEILST-MAX               PIC 9(05).
                   07  QSUB03-UBYOMEILST                   OCCURS 100.
                       09  QSUB03-UBYOMEIL                 PIC X(3).
                       09  QSUB03-UBYOMEIFLL               PIC X(1).
                       09  QSUB03-UBYOMEIMEIL              PIC X(20).
               05  QSUB03-BYOCOMPLST-G.
                   07  QSUB03-BYOCOMPLST-MAX               PIC 9(05).
                   07  QSUB03-BYOCOMPLST                   OCCURS 100.
                       09  QSUB03-BYOCOMPL                 PIC X(1).
                       09  QSUB03-BYOCOMPFLL               PIC X(1).
                       09  QSUB03-BYOCOMPMEIL              PIC X(20).
               05  QSUB03-SRYVALITEMLST-G.
                   07  QSUB03-SRYVALITEMLST-MAX            PIC 9(05).
                   07  QSUB03-SRYVALITEMLST                OCCURS  10.
                       09  QSUB03-SRYVALITEML              PIC X(01).
                       09  QSUB03-SRYVALITEMFLL            PIC X(01).
                       09  QSUB03-SRYVALITEMMEIL           PIC X(30).
               05  QSUB03-SRYVALCOMPLST-G.
                   07  QSUB03-SRYVALCOMPLST-MAX            PIC 9(05).
                   07  QSUB03-SRYVALCOMPLST                OCCURS  10.
                       09  QSUB03-SRYVALCOMPL              PIC X(01).
                       09  QSUB03-SRYVALCOMPFLL            PIC X(01).
                       09  QSUB03-SRYVALCOMPMEIL           PIC X(20).
               05  QSUB03-KMKLST-G.
                   07  QSUB03-KMKLST-MAX                   PIC 9(05).
                   07  QSUB03-KMKLST                       OCCURS 10.
                       09  QSUB03-KMKL                     PIC X(01).
                       09  QSUB03-KMKFLL                   PIC X(01).
                       09  QSUB03-KMKMEIL                  PIC X(28).
               05  QSUB03-TKMKLST-G.
                   07  QSUB03-TKMKLST-MAX                  PIC 9(05).
                   07  QSUB03-TKMKLST                      OCCURS 10.
                       09  QSUB03-TKMKL                    PIC X(01).
                       09  QSUB03-TKMKFLL                  PIC X(01).
                       09  QSUB03-TKMKMEIL                 PIC X(28).
               05  QSUB03-SKMKLST-G.
                   07  QSUB03-SKMKLST-MAX                  PIC 9(05).
                   07  QSUB03-SKMKLST                      OCCURS 10.
                       09  QSUB03-SKMKL                    PIC X(01).
                       09  QSUB03-SKMKFLL                  PIC X(01).
                       09  QSUB03-SKMKMEIL                 PIC X(28).
               05  QSUB03-GENMENLST-G.
                   07  QSUB03-GENMENLST-MAX                PIC 9(05).
                   07  QSUB03-GENMENLST                    OCCURS 99.
                       09  QSUB03-GENMENL                  PIC X(02).
                       09  QSUB03-GENMENFLL                PIC X(01).
                       09  QSUB03-GENMENMEIL               PIC X(20).
               05  QSUB03-TSYLST-G.
                   07  QSUB03-TSYLST-MAX                   PIC 9(05).
                   07  QSUB03-TSYLST                       OCCURS 5.
                       09  QSUB03-TSYL                     PIC X(01).
                       09  QSUB03-TSYFLL                   PIC X(01).
                       09  QSUB03-TSYMEIL                  PIC X(28).
               05  QSUB03-STTSYSKBTLST-G.
                   07  QSUB03-STTSYSKBTLST-MAX             PIC 9(05).
                   07  QSUB03-STTSYSKBTLST                 OCCURS 5.
                       09  QSUB03-STTSYSKBTL               PIC X(01).
                       09  QSUB03-STTSYSKBTFLL             PIC X(01).
                       09  QSUB03-STTSYSKBTMEIL            PIC X(08).
               05  QSUB03-SRYKBNLST-G.
                   07  QSUB03-SRYKBNLST-MAX              PIC 9(05).
                   07  QSUB03-SRYKBNLST                  OCCURS 30.
                       09  QSUB03-SRYKBNL                PIC X(02).
                       09  QSUB03-SRYKBNFLL              PIC X(01).
                       09  QSUB03-SRYKBNMEIL             PIC X(20).
               05  QSUB03-SRYSYUKBN20LST-G.
                   07  QSUB03-SRYSYUKBN20LST-MAX             PIC 9(05).
                   07  QSUB03-SRYSYUKBN20LST                 OCCURS 10.
                       09  QSUB03-SRYSYUKBN20L               PIC X(02).
                       09  QSUB03-SRYSYUKBN20FLL             PIC X(01).
                       09  QSUB03-SRYSYUKBN20MEIL            PIC X(20).
               05  QSUB03-SRYSYUKBN30LST-G.
                   07  QSUB03-SRYSYUKBN30LST-MAX             PIC 9(05).
                   07  QSUB03-SRYSYUKBN30LST                 OCCURS 10.
                       09  QSUB03-SRYSYUKBN30L               PIC X(02).
                       09  QSUB03-SRYSYUKBN30FLL             PIC X(01).
                       09  QSUB03-SRYSYUKBN30MEIL            PIC X(20).
               05  QSUB03-SRYSYUKBN90LST-G.
                   07  QSUB03-SRYSYUKBN90LST-MAX             PIC 9(05).
                   07  QSUB03-SRYSYUKBN90LST                 OCCURS 10.
                       09  QSUB03-SRYSYUKBN90L               PIC X(02).
                       09  QSUB03-SRYSYUKBN90FLL             PIC X(01).
                       09  QSUB03-SRYSYUKBN90MEIL            PIC X(20).
               05  QSUB03-HOKATSUKBNLST-G.
                   07  QSUB03-HOKATSUKBNLST-MAX            PIC 9(05).
                   07  QSUB03-HOKATSUKBNLST                OCCURS 5.
                       09  QSUB03-HOKATSUKBNL              PIC X(01).
                       09  QSUB03-HOKATSUKBNFLL            PIC X(01).
                       09  QSUB03-HOKATSUKBNMEIL           PIC X(20).
               05  QSUB03-JKNHKNJANUMLST-G.
                   07  QSUB03-JKNHKNJANUMLST-MAX           PIC 9(05).
                   07  QSUB03-JKNHKNJANUMLST               OCCURS 5.
                       09  QSUB03-JKNHKNJANUML             PIC X(01).
                       09  QSUB03-JKNHKNJANUMFLL           PIC X(01).
                       09  QSUB03-JKNHKNJANUMMEIL          PIC X(08).
              05  QSUB03-JKNHKNNUMLST-G.
                  07  QSUB03-JKNHKNNUMLST-MAX              PIC 9(05).
                  07  QSUB03-JKNHKNNUMLST                  OCCURS 5.
                      09  QSUB03-JKNHKNNUML                PIC X(01).
                      09  QSUB03-JKNHKNNUMFLL              PIC X(01).
                      09  QSUB03-JKNHKNNUMMEIL             PIC X(08).
              05  QSUB03-JKNKOHNUMLST-G.
                  07  QSUB03-JKNKOHNUMLST-MAX              PIC 9(05).
                  07  QSUB03-JKNKOHNUMLST                  OCCURS 5.
                      09  QSUB03-JKNKOHNUML                PIC X(01).
                      09  QSUB03-JKNKOHNUMFLL              PIC X(01).
                      09  QSUB03-JKNKOHNUMMEIL             PIC X(08).
              05  QSUB03-JKNHKNKOHLST-G.
                  07  QSUB03-JKNHKNKOHLST-MAX              PIC 9(05).
                  07  QSUB03-JKNHKNKOHLST                  OCCURS 5.
                      09  QSUB03-JKNHKNKOHL                PIC X(01).
                      09  QSUB03-JKNHKNKOHFLL              PIC X(01).
                      09  QSUB03-JKNHKNKOHMEIL             PIC X(08).
      *   介護
              05  QSUB03-KHSKBTLST-G.
                  07  QSUB03-KHSKBTLST-MAX                 PIC 9(05).
                  07  QSUB03-KHSKBTLST                     OCCURS 5.
                      09  QSUB03-KHSKBTL                   PIC X(01).
                      09  QSUB03-KHSKBTFLL                 PIC X(01).
                      09  QSUB03-KHSKBTMEIL                PIC X(08).
              05  QSUB03-KNSKBTLST-G.
                  07  QSUB03-KNSKBTLST-MAX                 PIC 9(05).
                  07  QSUB03-KNSKBTLST                     OCCURS 5.
                      09  QSUB03-KNSKBTL                   PIC X(01).
                      09  QSUB03-KNSKBTFLL                 PIC X(01).
                      09  QSUB03-KNSKBTMEIL                PIC X(08).
              05  QSUB03-KNJOTAILST-G.
                  07  QSUB03-KNJOTAILST-MAX                PIC 9(05).
                  07  QSUB03-KNJOTAILST                    OCCURS 20.
                      09  QSUB03-KNJOTAIL                  PIC X(02).
                      09  QSUB03-KNJOTAIFLL                PIC X(01).
                      09  QSUB03-KNJOTAIMEIL               PIC X(16).
              05  QSUB03-JKNBYOMEILST-G.
                  07  QSUB03-JKNBYOMEILST-MAX              PIC 9(05).
                  07  QSUB03-JKNBYOMEILST                  OCCURS 5.
                      09  QSUB03-JKNBYOMEIL                PIC X(01).
                      09  QSUB03-JKNBYOMEIFLL              PIC X(01).
                      09  QSUB03-JKNBYOMEIMEIL             PIC X(08).
              05  QSUB03-JKNSRYACTLST-G.
                  07  QSUB03-JKNSRYACTLST-MAX              PIC 9(05).
                  07  QSUB03-JKNSRYACTLST                  OCCURS 5.
                      09  QSUB03-JKNSRYACTL                PIC X(01).
                      09  QSUB03-JKNSRYACTFLL              PIC X(01).
                      09  QSUB03-JKNSRYACTMEIL             PIC X(08).
              05  QSUB03-PTSORTLST-G.
                  07  QSUB03-PTSORTLST-MAX                 PIC 9(05).
                  07  QSUB03-PTSORTLST                     OCCURS 10.
                      09  QSUB03-PTSORTL                   PIC X(01).
                      09  QSUB03-PTSORTFLL                 PIC X(01).
                      09  QSUB03-PTSORTMEIL                PIC X(28).
              05  QSUB03-TENSORTLST-G.
                  07  QSUB03-TENSORTLST-MAX                PIC 9(05).
                  07  QSUB03-TENSORTLST                    OCCURS 10.
                      09  QSUB03-TENSORTL                  PIC X(01).
                      09  QSUB03-TENSORTFLL                PIC X(01).
                      09  QSUB03-TENSORTMEIL               PIC X(28).
              05  QSUB03-SRYSORTLST-G.
                  07  QSUB03-SRYSORTLST-MAX                PIC 9(05).
                  07  QSUB03-SRYSORTLST                    OCCURS 10.
                      09  QSUB03-SRYSORTL                  PIC X(01).
                      09  QSUB03-SRYSORTFLL                PIC X(01).
                      09  QSUB03-SRYSORTMEIL               PIC X(28).
