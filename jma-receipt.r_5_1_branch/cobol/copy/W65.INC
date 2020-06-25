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
        02  W65.
      *     02  W65-FIXED6.
              03  W65-B06-STATE                  PIC S9(9) BINARY.
              03  W65-B07-STATE                  PIC S9(9) BINARY.
              03  W65-B08-STATE                  PIC S9(9) BINARY.
              03  W65-KJNYMD                     PIC X(10).
              03  W65-SELNUM1                    PIC X(50).
              03  W65-CMB-KOHNUM.
                04  W65-KOHNUM                   PIC X(64).
                04  W65-KOHNUM-STATE             PIC S9(9) BINARY.
                04  W65-KOHNUM-COUNT             PIC S9(9) BINARY.
                04  W65-KOHNUM-ITEM              PIC X(64) OCCURS 200.
                04  W65-KOHNUM-ITEM-STATE        PIC S9(9) BINARY.
              03  W65-NB1.
                04  W65-NB1-PAGENO               PIC S9(9) BINARY.
                04  W65-FXD7.
                  05  W65-CMB-JNISSUKBN.
                    06  W65-JNISSUKBN            PIC X(80).
                    06  W65-JNISSUKBN-COUNT      PIC S9(9) BINARY.
                    06  W65-JNISSUKBN-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-JNISSUTAIKBN.
                    06  W65-JNISSUTAIKBN         PIC X(40).
                    06  W65-JNISSUTAIKBN-STATE   PIC S9(9) BINARY.
                    06  W65-JNISSUTAIKBN-COUNT   PIC S9(9) BINARY.
                    06  W65-JNISSUTAIKBN-ITEM    PIC X(40) OCCURS 10.
                    06  W65-JNISSUTAIKBN-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-TOTALTENKBN.
                    06  W65-TOTALTENKBN          PIC X(80).
                    06  W65-TOTALTENKBN-COUNT    PIC S9(9) BINARY.
                    06  W65-TOTALTENKBN-ITEM     PIC X(80) OCCURS 10.
                  05  W65-CMB-TOTALTENTAIKBN.
                    06  W65-TOTALTENTAIKBN       PIC X(40).
                    06  W65-TOTALTENTAIKBN-STATE PIC S9(9) BINARY.
                    06  W65-TOTALTENTAIKBN-COUNT PIC S9(9) BINARY.
                    06  W65-TOTALTENTAIKBN-ITEM  PIC X(40) OCCURS 10.
                    06  W65-TOTALTENTAIKBN-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-SYUKEIKBN.
                    06  W65-SYUKEIKBN            PIC X(80).
                    06  W65-SYUKEIKBN-COUNT      PIC S9(9) BINARY.
                    06  W65-SYUKEIKBN-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-SYUKEITAIKBN.
                    06  W65-SYUKEITAIKBN         PIC X(40).
                    06  W65-SYUKEITAIKBN-STATE   PIC S9(9) BINARY.
                    06  W65-SYUKEITAIKBN-COUNT   PIC S9(9) BINARY.
                    06  W65-SYUKEITAIKBN-ITEM    PIC X(40) OCCURS 10.
                    06  W65-SYUKEITAIKBN-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-FTNKEIKBNG.
                    06  W65-FTNKEIKBNG           PIC X(80).
                    06  W65-FTNKEIKBNG-COUNT     PIC S9(9) BINARY.
                    06  W65-FTNKEIKBNG-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-FTNKEITAIKBNG.
                    06  W65-FTNKEITAIKBNG        PIC X(40).
                    06  W65-FTNKEITAIKBNG-STATE  PIC S9(9) BINARY.
                    06  W65-FTNKEITAIKBNG-COUNT  PIC S9(9) BINARY.
                    06  W65-FTNKEITAIKBNG-ITEM   PIC X(40) OCCURS 10.
                    06  W65-FTNKEITAIKBNG-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-FTNKEIKBNN.
                    06  W65-FTNKEIKBNN           PIC X(80).
                    06  W65-FTNKEIKBNN-COUNT     PIC S9(9) BINARY.
                    06  W65-FTNKEIKBNN-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-FTNKEITAIKBNN.
                    06  W65-FTNKEITAIKBNN        PIC X(40).
                    06  W65-FTNKEITAIKBNN-STATE  PIC S9(9) BINARY.
                    06  W65-FTNKEITAIKBNN-COUNT  PIC S9(9) BINARY.
                    06  W65-FTNKEITAIKBNN-ITEM   PIC X(40) OCCURS 10.
                    06  W65-FTNKEITAIKBNN-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-FTNMONEYKBNG.
                    06  W65-FTNMONEYKBNG         PIC X(80).
                    06  W65-FTNMONEYKBNG-COUNT   PIC S9(9) BINARY.
                    06  W65-FTNMONEYKBNG-ITEM    PIC X(80) OCCURS 10.
                  05  W65-CMB-FTNMONEYTAIKBNG.
                    06  W65-FTNMONEYTAIKBNG      PIC X(40).
                    06  W65-FTNMONEYTAIKBNG-STATE PIC S9(9) BINARY.
                    06  W65-FTNMONEYTAIKBNG-COUNT PIC S9(9) BINARY.
                    06  W65-FTNMONEYTAIKBNG-ITEM  PIC X(40) OCCURS 10.
                    06  W65-FTNMONEYTAIKBNG-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-FTNMONEYKBNN.
                    06  W65-FTNMONEYKBNN         PIC X(80).
                    06  W65-FTNMONEYKBNN-COUNT   PIC S9(9) BINARY.
                    06  W65-FTNMONEYKBNN-ITEM    PIC X(80) OCCURS 10.
                  05  W65-CMB-FTNMONEYTAIKBNN.
                    06  W65-FTNMONEYTAIKBNN       PIC X(40).
                    06  W65-FTNMONEYTAIKBNN-STATE PIC S9(9) BINARY.
                    06  W65-FTNMONEYTAIKBNN-COUNT PIC S9(9) BINARY.
                    06  W65-FTNMONEYTAIKBNN-ITEM  PIC X(40) OCCURS 10.
                    06  W65-FTNMONEYTAIKBNN-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-FTNZEROKBNG.
                    06  W65-FTNZEROKBNG          PIC X(80).
                    06  W65-FTNZEROKBNG-COUNT    PIC S9(9) BINARY.
                    06  W65-FTNZEROKBNG-ITEM     PIC X(80) OCCURS 10.
                  05  W65-CMB-FTNZEROTAIKBNG.
                    06  W65-FTNZEROTAIKBNG       PIC X(40).
                    06  W65-FTNZEROTAIKBNG-STATE PIC S9(9) BINARY.
                    06  W65-FTNZEROTAIKBNG-COUNT PIC S9(9) BINARY.
                    06  W65-FTNZEROTAIKBNG-ITEM  PIC X(40) OCCURS 10.
                    06  W65-FTNZEROTAIKBNG-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-FTNZEROKBNN.
                    06  W65-FTNZEROKBNN          PIC X(80).
                    06  W65-FTNZEROKBNN-COUNT    PIC S9(9) BINARY.
                    06  W65-FTNZEROKBNN-ITEM     PIC X(80) OCCURS 10.
                  05  W65-CMB-FTNZEROTAIKBNN.
                    06  W65-FTNZEROTAIKBNN       PIC X(40).
                    06  W65-FTNZEROTAIKBNN-STATE PIC S9(9) BINARY.
                    06  W65-FTNZEROTAIKBNN-COUNT PIC S9(9) BINARY.
                    06  W65-FTNZEROTAIKBNN-ITEM  PIC X(40) OCCURS 10.
                    06  W65-FTNZEROTAIKBNN-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-SKJKBN.
                    06  W65-SKJKBN               PIC X(80).
                    06  W65-SKJKBN-COUNT         PIC S9(9) BINARY.
                    06  W65-SKJKBN-ITEM          PIC X(80) OCCURS 10.
                  05  W65-CMB-SKJTAIKBN.
                    06  W65-SKJTAIKBN            PIC X(40).
                    06  W65-SKJTAIKBN-STATE      PIC S9(9) BINARY.
                    06  W65-SKJTAIKBN-COUNT      PIC S9(9) BINARY.
                    06  W65-SKJTAIKBN-ITEM       PIC X(40) OCCURS 10.
                    06  W65-SKJTAIKBN-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-OCRKBN.
                    06  W65-OCRKBN               PIC X(80).
                    06  W65-OCRKBN-COUNT         PIC S9(9) BINARY.
                    06  W65-OCRKBN-ITEM          PIC X(80) OCCURS 10.
                  05  W65-CMB-OCRTAIKBN.
                    06  W65-OCRTAIKBN            PIC X(40).
                    06  W65-OCRTAIKBN-STATE      PIC S9(9) BINARY.
                    06  W65-OCRTAIKBN-COUNT      PIC S9(9) BINARY.
                    06  W65-OCRTAIKBN-ITEM       PIC X(40) OCCURS 10.
                    06  W65-OCRTAIKBN-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-SAIKEIKBN.
                    06  W65-SAIKEIKBN            PIC X(80).
                    06  W65-SAIKEIKBN-COUNT      PIC S9(9) BINARY.
                    06  W65-SAIKEIKBN-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-SAIKEITAIKBN.
                    06  W65-SAIKEITAIKBN         PIC X(40).
                    06  W65-SAIKEITAIKBN-STATE   PIC S9(9) BINARY.
                    06  W65-SAIKEITAIKBN-COUNT   PIC S9(9) BINARY.
                    06  W65-SAIKEITAIKBN-ITEM    PIC X(40) OCCURS 10.
                    06  W65-SAIKEITAIKBN-ITEM-STATE
                                                 PIC S9(9) BINARY.
                04  W65-N01-STATE   PIC S9(9)    BINARY.
                04  W65-FXD8.
                  05  W65-CMB-HKNFTNKBNK.
                    06  W65-HKNFTNKBNK           PIC X(80).
                    06  W65-HKNFTNKBNK-COUNT     PIC S9(9) BINARY.
                    06  W65-HKNFTNKBNK-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-HKNFTNTAIKBNK.
                    06  W65-HKNFTNTAIKBNK        PIC X(40).
                    06  W65-HKNFTNTAIKBNK-STATE  PIC S9(9) BINARY.
                    06  W65-HKNFTNTAIKBNK-COUNT  PIC S9(9) BINARY.
                    06  W65-HKNFTNTAIKBNK-ITEM   PIC X(40) OCCURS 10.
                    06  W65-HKNFTNTAIKBNK-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-HKNFTNKBNY.
                    06  W65-HKNFTNKBNY           PIC X(80).
                    06  W65-HKNFTNKBNY-COUNT     PIC S9(9) BINARY.
                    06  W65-HKNFTNKBNY-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-HKNFTNTAIKBNY.
                    06  W65-HKNFTNTAIKBNY        PIC X(40).
                    06  W65-HKNFTNTAIKBNY-STATE  PIC S9(9) BINARY.
                    06  W65-HKNFTNTAIKBNY-COUNT  PIC S9(9) BINARY.
                    06  W65-HKNFTNTAIKBNY-ITEM   PIC X(40) OCCURS 10.
                    06  W65-HKNFTNTAIKBNY-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-RECEKSIKBNG.
                    06  W65-RECEKSIKBNG          PIC X(80).
                    06  W65-RECEKSIKBNG-COUNT    PIC S9(9) BINARY.
                    06  W65-RECEKSIKBNG-ITEM     PIC X(80) OCCURS 10.
                  05  W65-CMB-RECEKSITAIKBNG.
                    06  W65-RECEKSITAIKBNG       PIC X(40).
                    06  W65-RECEKSITAIKBNG-STATE PIC S9(9) BINARY.
                    06  W65-RECEKSITAIKBNG-COUNT PIC S9(9) BINARY.
                    06  W65-RECEKSITAIKBNG-ITEM  PIC X(40) OCCURS 10.
                    06  W65-RECEKSITAIKBNG-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-RECEKSIKBNN.
                    06  W65-RECEKSIKBNN          PIC X(80).
                    06  W65-RECEKSIKBNN-COUNT    PIC S9(9) BINARY.
                    06  W65-RECEKSIKBNN-ITEM     PIC X(80) OCCURS 10.
                  05  W65-CMB-RECEKSITAIKBNN.
                    06  W65-RECEKSITAIKBNN       PIC X(40).
                    06  W65-RECEKSITAIKBNN-STATE PIC S9(9) BINARY.
                    06  W65-RECEKSITAIKBNN-COUNT PIC S9(9) BINARY.
                    06  W65-RECEKSITAIKBNN-ITEM  PIC X(40) OCCURS 10.
                    06  W65-RECEKSITAIKBNN-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-CHKJGNKBNG.
                    06  W65-CHKJGNKBNG           PIC X(80).
                    06  W65-CHKJGNKBNG-COUNT     PIC S9(9) BINARY.
                    06  W65-CHKJGNKBNG-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-CHKJGNTAIKBNG.
                    06  W65-CHKJGNTAIKBNG        PIC X(40).
                    06  W65-CHKJGNTAIKBNG-STATE  PIC S9(9) BINARY.
                    06  W65-CHKJGNTAIKBNG-COUNT  PIC S9(9) BINARY.
                    06  W65-CHKJGNTAIKBNG-ITEM   PIC X(40) OCCURS 10.
                    06  W65-CHKJGNTAIKBNG-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-CHKJGNKBNN.
                    06  W65-CHKJGNKBNN           PIC X(80).
                    06  W65-CHKJGNKBNN-COUNT     PIC S9(9) BINARY.
                    06  W65-CHKJGNKBNN-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-CHKJGNTAIKBNN.
                    06  W65-CHKJGNTAIKBNN        PIC X(40).
                    06  W65-CHKJGNTAIKBNN-STATE  PIC S9(9) BINARY.
                    06  W65-CHKJGNTAIKBNN-COUNT  PIC S9(9) BINARY.
                    06  W65-CHKJGNTAIKBNN-ITEM   PIC X(40) OCCURS 10.
                    06  W65-CHKJGNTAIKBNN-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-KOHSKYKBNG.
                    06  W65-KOHSKYKBNG           PIC X(80).
                    06  W65-KOHSKYKBNG-COUNT     PIC S9(9) BINARY.
                    06  W65-KOHSKYKBNG-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-KOHSKYTAIKBNG.
                    06  W65-KOHSKYTAIKBNG        PIC X(40).
                    06  W65-KOHSKYTAIKBNG-STATE  PIC S9(9) BINARY.
                    06  W65-KOHSKYTAIKBNG-COUNT  PIC S9(9) BINARY.
                    06  W65-KOHSKYTAIKBNG-ITEM   PIC X(40) OCCURS 10.
                    06  W65-KOHSKYTAIKBNG-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-KOHSKYKBNN.
                    06  W65-KOHSKYKBNN           PIC X(80).
                    06  W65-KOHSKYKBNN-COUNT     PIC S9(9) BINARY.
                    06  W65-KOHSKYKBNN-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-KOHSKYTAIKBNN.
                    06  W65-KOHSKYTAIKBNN        PIC X(40).
                    06  W65-KOHSKYTAIKBNN-STATE  PIC S9(9) BINARY.
                    06  W65-KOHSKYTAIKBNN-COUNT  PIC S9(9) BINARY.
                    06  W65-KOHSKYTAIKBNN-ITEM   PIC X(40) OCCURS 10.
                    06  W65-KOHSKYTAIKBNN-ITEM-STATE
                                                 PIC S9(9) BINARY.
                04  W65-N02-STATE   PIC S9(9)    BINARY.
                04  W65-FXD9.
                  05  W65-CMB-KAKKBNK.
                    06  W65-KAKKBNK              PIC X(80).
                    06  W65-KAKKBNK-COUNT        PIC S9(9) BINARY.
                    06  W65-KAKKBNK-ITEM         PIC X(80) OCCURS 10.
                  05  W65-CMB-KAKTAIKBNK.
                    06  W65-KAKTAIKBNK           PIC X(40).
                    06  W65-KAKTAIKBNK-STATE     PIC S9(9) BINARY.
                    06  W65-KAKTAIKBNK-COUNT     PIC S9(9) BINARY.
                    06  W65-KAKTAIKBNK-ITEM      PIC X(40) OCCURS 10.
                    06  W65-KAKTAIKBNK-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-KAK21KBNK.
                    06  W65-KAK21KBNK            PIC X(80).
                    06  W65-KAK21KBNK-COUNT      PIC S9(9) BINARY.
                    06  W65-KAK21KBNK-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-KAK21TAIKBNK.
                    06  W65-KAK21TAIKBNK         PIC X(40).
                    06  W65-KAK21TAIKBNK-STATE   PIC S9(9) BINARY.
                    06  W65-KAK21TAIKBNK-COUNT   PIC S9(9) BINARY.
                    06  W65-KAK21TAIKBNK-ITEM    PIC X(40) OCCURS 10.
                    06  W65-KAK21TAIKBNK-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-KAK22KBNK.
                    06  W65-KAK22KBNK            PIC X(80).
                    06  W65-KAK22KBNK-COUNT      PIC S9(9) BINARY.
                    06  W65-KAK22KBNK-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-KAK22TAIKBNK.
                    06  W65-KAK22TAIKBNK         PIC X(40).
                    06  W65-KAK22TAIKBNK-STATE   PIC S9(9) BINARY.
                    06  W65-KAK22TAIKBNK-COUNT   PIC S9(9) BINARY.
                    06  W65-KAK22TAIKBNK-ITEM    PIC X(40) OCCURS 10.
                    06  W65-KAK22TAIKBNK-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-KAK23KBNK.
                    06  W65-KAK23KBNK            PIC X(80).
                    06  W65-KAK23KBNK-COUNT      PIC S9(9) BINARY.
                    06  W65-KAK23KBNK-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-KAK23TAIKBNK.
                    06  W65-KAK23TAIKBNK         PIC X(40).
                    06  W65-KAK23TAIKBNK-STATE   PIC S9(9) BINARY.
                    06  W65-KAK23TAIKBNK-COUNT   PIC S9(9) BINARY.
                    06  W65-KAK23TAIKBNK-ITEM    PIC X(40) OCCURS 10.
                    06  W65-KAK23TAIKBNK-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-KAK31KBNK.
                    06  W65-KAK31KBNK            PIC X(80).
                    06  W65-KAK31KBNK-COUNT      PIC S9(9) BINARY.
                    06  W65-KAK31KBNK-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-KAK31TAIKBNK.
                    06  W65-KAK31TAIKBNK         PIC X(40).
                    06  W65-KAK31TAIKBNK-STATE   PIC S9(9) BINARY.
                    06  W65-KAK31TAIKBNK-COUNT   PIC S9(9) BINARY.
                    06  W65-KAK31TAIKBNK-ITEM    PIC X(40) OCCURS 10.
                    06  W65-KAK31TAIKBNK-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-KAK32KBNK.
                    06  W65-KAK32KBNK            PIC X(80).
                    06  W65-KAK32KBNK-COUNT      PIC S9(9) BINARY.
                    06  W65-KAK32KBNK-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-KAK32TAIKBNK.
                    06  W65-KAK32TAIKBNK         PIC X(40).
                    06  W65-KAK32TAIKBNK-STATE   PIC S9(9) BINARY.
                    06  W65-KAK32TAIKBNK-COUNT   PIC S9(9) BINARY.
                    06  W65-KAK32TAIKBNK-ITEM    PIC X(40) OCCURS 10.
                    06  W65-KAK32TAIKBNK-ITEM-STATE   PIC S9(9) BINARY.
                  05  W65-CMB-KAK41KBNK.
                    06  W65-KAK41KBNK            PIC X(80).
                    06  W65-KAK41KBNK-COUNT      PIC S9(9) BINARY.
                    06  W65-KAK41KBNK-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-KAK41TAIKBNK.
                    06  W65-KAK41TAIKBNK         PIC X(40).
                    06  W65-KAK41TAIKBNK-STATE   PIC S9(9) BINARY.
                    06  W65-KAK41TAIKBNK-COUNT   PIC S9(9) BINARY.
                    06  W65-KAK41TAIKBNK-ITEM    PIC X(40) OCCURS 10.
                    06  W65-KAK41TAIKBNK-ITEM-STATE   PIC S9(9) BINARY.
                  05  W65-CMB-KAK42KBNK.
                    06  W65-KAK42KBNK            PIC X(80).
                    06  W65-KAK42KBNK-COUNT      PIC S9(9) BINARY.
                    06  W65-KAK42KBNK-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-KAK42TAIKBNK.
                    06  W65-KAK42TAIKBNK         PIC X(40).
                    06  W65-KAK42TAIKBNK-STATE   PIC S9(9) BINARY.
                    06  W65-KAK42TAIKBNK-COUNT   PIC S9(9) BINARY.
                    06  W65-KAK42TAIKBNK-ITEM    PIC X(40) OCCURS 10.
                    06  W65-KAK42TAIKBNK-ITEM-STATE   PIC S9(9) BINARY.
                  05  W65-CMB-KAK43KBNK.
                    06  W65-KAK43KBNK            PIC X(80).
                    06  W65-KAK43KBNK-COUNT      PIC S9(9) BINARY.
                    06  W65-KAK43KBNK-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-KAK43TAIKBNK.
                    06  W65-KAK43TAIKBNK         PIC X(40).
                    06  W65-KAK43TAIKBNK-STATE   PIC S9(9) BINARY.
                    06  W65-KAK43TAIKBNK-COUNT   PIC S9(9) BINARY.
                    06  W65-KAK43TAIKBNK-ITEM    PIC X(40) OCCURS 10.
                    06  W65-KAK43TAIKBNK-ITEM-STATE PIC S9(9) BINARY.
                04  W65-N03-STATE   PIC S9(9)    BINARY.
                04  W65-FXD10.
                  05  W65-CMB-KAKKBNY.
                    06  W65-KAKKBNY              PIC X(80).
                    06  W65-KAKKBNY-COUNT        PIC S9(9) BINARY.
                    06  W65-KAKKBNY-ITEM         PIC X(80) OCCURS 10.
                  05  W65-CMB-KAKTAIKBNY.
                    06  W65-KAKTAIKBNY           PIC X(40).
                    06  W65-KAKTAIKBNY-STATE     PIC S9(9) BINARY.
                    06  W65-KAKTAIKBNY-COUNT     PIC S9(9) BINARY.
                    06  W65-KAKTAIKBNY-ITEM      PIC X(40) OCCURS 10.
                    06  W65-KAKTAIKBNY-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-KAK21KBNY.
                    06  W65-KAK21KBNY            PIC X(80).
                    06  W65-KAK21KBNY-COUNT      PIC S9(9) BINARY.
                    06  W65-KAK21KBNY-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-KAK21TAIKBNY.
                    06  W65-KAK21TAIKBNY         PIC X(40).
                    06  W65-KAK21TAIKBNY-STATE   PIC S9(9) BINARY.
                    06  W65-KAK21TAIKBNY-COUNT   PIC S9(9) BINARY.
                    06  W65-KAK21TAIKBNY-ITEM    PIC X(40) OCCURS 10.
                    06  W65-KAK21TAIKBNY-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-KAK22KBNY.
                    06  W65-KAK22KBNY            PIC X(80).
                    06  W65-KAK22KBNY-COUNT      PIC S9(9) BINARY.
                    06  W65-KAK22KBNY-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-KAK22TAIKBNY.
                    06  W65-KAK22TAIKBNY         PIC X(40).
                    06  W65-KAK22TAIKBNY-STATE   PIC S9(9) BINARY.
                    06  W65-KAK22TAIKBNY-COUNT   PIC S9(9) BINARY.
                    06  W65-KAK22TAIKBNY-ITEM    PIC X(40) OCCURS 10.
                    06  W65-KAK22TAIKBNY-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-KAK23KBNY.
                    06  W65-KAK23KBNY            PIC X(80).
                    06  W65-KAK23KBNY-COUNT      PIC S9(9) BINARY.
                    06  W65-KAK23KBNY-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-KAK23TAIKBNY.
                    06  W65-KAK23TAIKBNY         PIC X(40).
                    06  W65-KAK23TAIKBNY-STATE   PIC S9(9) BINARY.
                    06  W65-KAK23TAIKBNY-COUNT   PIC S9(9) BINARY.
                    06  W65-KAK23TAIKBNY-ITEM    PIC X(40) OCCURS 10.
                    06  W65-KAK23TAIKBNY-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-KAK31KBNY.
                    06  W65-KAK31KBNY            PIC X(80).
                    06  W65-KAK31KBNY-COUNT      PIC S9(9) BINARY.
                    06  W65-KAK31KBNY-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-KAK31TAIKBNY.
                    06  W65-KAK31TAIKBNY         PIC X(40).
                    06  W65-KAK31TAIKBNY-STATE   PIC S9(9) BINARY.
                    06  W65-KAK31TAIKBNY-COUNT   PIC S9(9) BINARY.
                    06  W65-KAK31TAIKBNY-ITEM    PIC X(40) OCCURS 10.
                    06  W65-KAK31TAIKBNY-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-KAK32KBNY.
                    06  W65-KAK32KBNY            PIC X(80).
                    06  W65-KAK32KBNY-COUNT      PIC S9(9) BINARY.
                    06  W65-KAK32KBNY-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-KAK32TAIKBNY.
                    06  W65-KAK32TAIKBNY         PIC X(40).
                    06  W65-KAK32TAIKBNY-STATE   PIC S9(9) BINARY.
                    06  W65-KAK32TAIKBNY-COUNT   PIC S9(9) BINARY.
                    06  W65-KAK32TAIKBNY-ITEM    PIC X(40) OCCURS 10.
                    06  W65-KAK32TAIKBNY-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-KAK41KBNY.
                    06  W65-KAK41KBNY            PIC X(80).
                    06  W65-KAK41KBNY-COUNT      PIC S9(9) BINARY.
                    06  W65-KAK41KBNY-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-KAK41TAIKBNY.
                    06  W65-KAK41TAIKBNY         PIC X(40).
                    06  W65-KAK41TAIKBNY-STATE   PIC S9(9) BINARY.
                    06  W65-KAK41TAIKBNY-COUNT   PIC S9(9) BINARY.
                    06  W65-KAK41TAIKBNY-ITEM    PIC X(40) OCCURS 10.
                    06  W65-KAK41TAIKBNY-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-KAK42KBNY.
                    06  W65-KAK42KBNY            PIC X(80).
                    06  W65-KAK42KBNY-COUNT      PIC S9(9) BINARY.
                    06  W65-KAK42KBNY-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-KAK42TAIKBNY.
                    06  W65-KAK42TAIKBNY         PIC X(40).
                    06  W65-KAK42TAIKBNY-STATE   PIC S9(9) BINARY.
                    06  W65-KAK42TAIKBNY-COUNT   PIC S9(9) BINARY.
                    06  W65-KAK42TAIKBNY-ITEM    PIC X(40) OCCURS 10.
                    06  W65-KAK42TAIKBNY-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-KAK43KBNY.
                    06  W65-KAK43KBNY            PIC X(80).
                    06  W65-KAK43KBNY-COUNT      PIC S9(9) BINARY.
                    06  W65-KAK43KBNY-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-KAK43TAIKBNY.
                    06  W65-KAK43TAIKBNY         PIC X(40).
                    06  W65-KAK43TAIKBNY-STATE   PIC S9(9) BINARY.
                    06  W65-KAK43TAIKBNY-COUNT   PIC S9(9) BINARY.
                    06  W65-KAK43TAIKBNY-ITEM    PIC X(40) OCCURS 10.
                    06  W65-KAK43TAIKBNY-ITEM-STATE PIC S9(9) BINARY.
                04  W65-N04-STATE   PIC S9(9)    BINARY.
                04  W65-FXD11.
                  05  W65-CMB-KSINGKBNKG.
                    06  W65-KSINGKBNKG           PIC X(80).
                    06  W65-KSINGKBNKG-COUNT     PIC S9(9) BINARY.
                    06  W65-KSINGKBNKG-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-KSINGTAIKBNKG.
                    06  W65-KSINGTAIKBNKG        PIC X(40).
                    06  W65-KSINGTAIKBNKG-STATE  PIC S9(9) BINARY.
                    06  W65-KSINGTAIKBNKG-COUNT  PIC S9(9) BINARY.
                    06  W65-KSINGTAIKBNKG-ITEM   PIC X(40) OCCURS 10.
                    06  W65-KSINGTAIKBNKG-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-KSINGKBNKN.
                    06  W65-KSINGKBNKN           PIC X(80).
                    06  W65-KSINGKBNKN-COUNT     PIC S9(9) BINARY.
                    06  W65-KSINGKBNKN-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-KSINGTAIKBNKN.
                    06  W65-KSINGTAIKBNKN        PIC X(40).
                    06  W65-KSINGTAIKBNKN-STATE  PIC S9(9) BINARY.
                    06  W65-KSINGTAIKBNKN-COUNT  PIC S9(9) BINARY.
                    06  W65-KSINGTAIKBNKN-ITEM   PIC X(40) OCCURS 10.
                    06  W65-KSINGTAIKBNKN-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-KSINGKBNYG.
                    06  W65-KSINGKBNYG           PIC X(80).
                    06  W65-KSINGKBNYG-COUNT     PIC S9(9) BINARY.
                    06  W65-KSINGKBNYG-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-KSINGTAIKBNYG.
                    06  W65-KSINGTAIKBNYG        PIC X(40).
                    06  W65-KSINGTAIKBNYG-STATE  PIC S9(9) BINARY.
                    06  W65-KSINGTAIKBNYG-COUNT  PIC S9(9) BINARY.
                    06  W65-KSINGTAIKBNYG-ITEM   PIC X(40) OCCURS 10.
                    06  W65-KSINGTAIKBNYG-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-KSINGKBNYN.
                    06  W65-KSINGKBNYN           PIC X(80).
                    06  W65-KSINGKBNYN-COUNT     PIC S9(9) BINARY.
                    06  W65-KSINGKBNYN-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-KSINGTAIKBNYN.
                    06  W65-KSINGTAIKBNYN        PIC X(40).
                    06  W65-KSINGTAIKBNYN-STATE  PIC S9(9) BINARY.
                    06  W65-KSINGTAIKBNYN-COUNT  PIC S9(9) BINARY.
                    06  W65-KSINGTAIKBNYN-ITEM   PIC X(40) OCCURS 10.
                    06  W65-KSINGTAIKBNYN-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-ENCHGKBNKG.
                    06  W65-ENCHGKBNKG           PIC X(80).
                    06  W65-ENCHGKBNKG-COUNT     PIC S9(9) BINARY.
                    06  W65-ENCHGKBNKG-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-ENCHGTAIKBNKG.
                    06  W65-ENCHGTAIKBNKG        PIC X(40).
                    06  W65-ENCHGTAIKBNKG-STATE  PIC S9(9) BINARY.
                    06  W65-ENCHGTAIKBNKG-COUNT  PIC S9(9) BINARY.
                    06  W65-ENCHGTAIKBNKG-ITEM   PIC X(40) OCCURS 10.
                    06  W65-ENCHGTAIKBNKG-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-ENCHGKBNKN.
                    06  W65-ENCHGKBNKN           PIC X(80).
                    06  W65-ENCHGKBNKN-COUNT     PIC S9(9) BINARY.
                    06  W65-ENCHGKBNKN-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-ENCHGTAIKBNKN.
                    06  W65-ENCHGTAIKBNKN        PIC X(40).
                    06  W65-ENCHGTAIKBNKN-STATE  PIC S9(9) BINARY.
                    06  W65-ENCHGTAIKBNKN-COUNT  PIC S9(9) BINARY.
                    06  W65-ENCHGTAIKBNKN-ITEM   PIC X(40) OCCURS 10.
                    06  W65-ENCHGTAIKBNKN-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-ENCHGKBNYG.
                    06  W65-ENCHGKBNYG           PIC X(80).
                    06  W65-ENCHGKBNYG-COUNT     PIC S9(9) BINARY.
                    06  W65-ENCHGKBNYG-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-ENCHGTAIKBNYG.
                    06  W65-ENCHGTAIKBNYG        PIC X(40).
                    06  W65-ENCHGTAIKBNYG-STATE  PIC S9(9) BINARY.
                    06  W65-ENCHGTAIKBNYG-COUNT  PIC S9(9) BINARY.
                    06  W65-ENCHGTAIKBNYG-ITEM   PIC X(40) OCCURS 10.
                    06  W65-ENCHGTAIKBNYG-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-ENCHGKBNYN.
                    06  W65-ENCHGKBNYN           PIC X(80).
                    06  W65-ENCHGKBNYN-COUNT     PIC S9(9) BINARY.
                    06  W65-ENCHGKBNYN-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-ENCHGTAIKBNYN.
                    06  W65-ENCHGTAIKBNYN        PIC X(40).
                    06  W65-ENCHGTAIKBNYN-STATE  PIC S9(9) BINARY.
                    06  W65-ENCHGTAIKBNYN-COUNT  PIC S9(9) BINARY.
                    06  W65-ENCHGTAIKBNYN-ITEM   PIC X(40) OCCURS 10.
                    06  W65-ENCHGTAIKBNYN-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-KOH2SKJKBN.
                    06  W65-KOH2SKJKBN           PIC X(80).
                    06  W65-KOH2SKJKBN-COUNT     PIC S9(9) BINARY.
                    06  W65-KOH2SKJKBN-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-KOH2SKJTAIKBN.
                    06  W65-KOH2SKJTAIKBN        PIC X(40).
                    06  W65-KOH2SKJTAIKBN-STATE  PIC S9(9) BINARY.
                    06  W65-KOH2SKJTAIKBN-COUNT  PIC S9(9) BINARY.
                    06  W65-KOH2SKJTAIKBN-ITEM   PIC X(40) OCCURS 10.
                    06  W65-KOH2SKJTAIKBN-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-FTNHASUKBNG.
                    06  W65-FTNHASUKBNG           PIC X(80).
                    06  W65-FTNHASUKBNG-COUNT     PIC S9(9) BINARY.
                    06  W65-FTNHASUKBNG-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-FTNHASUTAIKBNG.
                    06  W65-FTNHASUTAIKBNG        PIC X(40).
                    06  W65-FTNHASUTAIKBNG-STATE  PIC S9(9) BINARY.
                    06  W65-FTNHASUTAIKBNG-COUNT  PIC S9(9) BINARY.
                    06  W65-FTNHASUTAIKBNG-ITEM   PIC X(40) OCCURS 10.
                    06  W65-FTNHASUTAIKBNG-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-FTNHASUKBNN.
                    06  W65-FTNHASUKBNN           PIC X(80).
                    06  W65-FTNHASUKBNN-COUNT     PIC S9(9) BINARY.
                    06  W65-FTNHASUKBNN-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-FTNHASUTAIKBNN.
                    06  W65-FTNHASUTAIKBNN        PIC X(40).
                    06  W65-FTNHASUTAIKBNN-STATE  PIC S9(9) BINARY.
                    06  W65-FTNHASUTAIKBNN-COUNT  PIC S9(9) BINARY.
                    06  W65-FTNHASUTAIKBNN-ITEM   PIC X(40) OCCURS 10.
                    06  W65-FTNHASUTAIKBNN-ITEM-STATE  PIC S9(9) BINARY.
                04  W65-N05-STATE   PIC S9(9)    BINARY.
                04  W65-FXD12.
                  05  W65-CMB-JYUHENKBN.
                    06  W65-JYUHENKBN            PIC X(80).
                    06  W65-JYUHENKBN-COUNT      PIC S9(9) BINARY.
                    06  W65-JYUHENKBN-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-JYUHENTAIKBN.
                    06  W65-JYUHENTAIKBN         PIC X(40).
                    06  W65-JYUHENTAIKBN-STATE   PIC S9(9) BINARY.
                    06  W65-JYUHENTAIKBN-COUNT   PIC S9(9) BINARY.
                    06  W65-JYUHENTAIKBN-ITEM    PIC X(40) OCCURS 15.
                    06  W65-JYUHENTAIKBN-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-JGNFTNKBN.
                    06  W65-JGNFTNKBN            PIC X(80).
                    06  W65-JGNFTNKBN-COUNT      PIC S9(9) BINARY.
                    06  W65-JGNFTNKBN-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-JGNFTNTAIKBN.
                    06  W65-JGNFTNTAIKBN         PIC X(40).
                    06  W65-JGNFTNTAIKBN-STATE   PIC S9(9) BINARY.
                    06  W65-JGNFTNTAIKBN-COUNT   PIC S9(9) BINARY.
                    06  W65-JGNFTNTAIKBN-ITEM    PIC X(40) OCCURS 10.
                    06  W65-JGNFTNTAIKBN-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-PTJGNKBN.
                    06  W65-PTJGNKBN             PIC X(80).
                    06  W65-PTJGNKBN-COUNT       PIC S9(9) BINARY.
                    06  W65-PTJGNKBN-ITEM        PIC X(80) OCCURS 10.
                  05  W65-CMB-PTJGNTAIKBN.
                    06  W65-PTJGNTAIKBN          PIC X(40).
                    06  W65-PTJGNTAIKBN-STATE    PIC S9(9) BINARY.
                    06  W65-PTJGNTAIKBN-COUNT    PIC S9(9) BINARY.
                    06  W65-PTJGNTAIKBN-ITEM     PIC X(40) OCCURS 10.
                    06  W65-PTJGNTAIKBN-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-IRYOKEIKBNG.
                    06  W65-IRYOKEIKBNG          PIC X(80).
                    06  W65-IRYOKEIKBNG-COUNT    PIC S9(9) BINARY.
                    06  W65-IRYOKEIKBNG-ITEM     PIC X(80) OCCURS 10.
                  05  W65-CMB-IRYOKEITAIKBNG.
                    06  W65-IRYOKEITAIKBNG       PIC X(40).
                    06  W65-IRYOKEITAIKBNG-STATE PIC S9(9) BINARY.
                    06  W65-IRYOKEITAIKBNG-COUNT PIC S9(9) BINARY.
                    06  W65-IRYOKEITAIKBNG-ITEM  PIC X(40) OCCURS 10.
                    06  W65-IRYOKEITAIKBNG-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-IRYOKEIKBNN.
                    06  W65-IRYOKEIKBNN          PIC X(80).
                    06  W65-IRYOKEIKBNN-COUNT    PIC S9(9) BINARY.
                    06  W65-IRYOKEIKBNN-ITEM     PIC X(80) OCCURS 10.
                  05  W65-CMB-IRYOKEITAIKBNN.
                    06  W65-IRYOKEITAIKBNN       PIC X(40).
                    06  W65-IRYOKEITAIKBNN-STATE PIC S9(9) BINARY.
                    06  W65-IRYOKEITAIKBNN-COUNT PIC S9(9) BINARY.
                    06  W65-IRYOKEITAIKBNN-ITEM  PIC X(40) OCCURS 10.
                    06  W65-IRYOKEITAIKBNN-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-SKJKEIKBN.
                    06  W65-SKJKEIKBN            PIC X(80).
                    06  W65-SKJKEIKBN-COUNT      PIC S9(9) BINARY.
                    06  W65-SKJKEIKBN-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-SKJKEITAIKBN.
                    06  W65-SKJKEITAIKBN         PIC X(40).
                    06  W65-SKJKEITAIKBN-STATE   PIC S9(9) BINARY.
                    06  W65-SKJKEITAIKBN-COUNT   PIC S9(9) BINARY.
                    06  W65-SKJKEITAIKBN-ITEM    PIC X(40) OCCURS 10.
                    06  W65-SKJKEITAIKBN-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-KINJYOKBNG.
                    06  W65-KINJYOKBNG            PIC X(80).
                    06  W65-KINJYOKBNG-COUNT      PIC S9(9) BINARY.
                    06  W65-KINJYOKBNG-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-KINJYOTAIKBNG.
                    06  W65-KINJYOTAIKBNG         PIC X(40).
                    06  W65-KINJYOTAIKBNG-STATE   PIC S9(9) BINARY.
                    06  W65-KINJYOTAIKBNG-COUNT   PIC S9(9) BINARY.
                    06  W65-KINJYOTAIKBNG-ITEM    PIC X(40) OCCURS 10.
                    06  W65-KINJYOTAIKBNG-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-KINJYOKBNN.
                    06  W65-KINJYOKBNN            PIC X(80).
                    06  W65-KINJYOKBNN-COUNT      PIC S9(9) BINARY.
                    06  W65-KINJYOKBNN-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-KINJYOTAIKBNN.
                    06  W65-KINJYOTAIKBNN         PIC X(40).
                    06  W65-KINJYOTAIKBNN-STATE   PIC S9(9) BINARY.
                    06  W65-KINJYOTAIKBNN-COUNT   PIC S9(9) BINARY.
                    06  W65-KINJYOTAIKBNN-ITEM    PIC X(40) OCCURS 10.
                    06  W65-KINJYOTAIKBNN-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-INNAIKBN.
                    06  W65-INNAIKBN            PIC X(80).
                    06  W65-INNAIKBN-COUNT      PIC S9(9) BINARY.
                    06  W65-INNAIKBN-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-INNAITAIKBN.
                    06  W65-INNAITAIKBN         PIC X(40).
                    06  W65-INNAITAIKBN-STATE   PIC S9(9) BINARY.
                    06  W65-INNAITAIKBN-COUNT   PIC S9(9) BINARY.
                    06  W65-INNAITAIKBN-ITEM    PIC X(40) OCCURS 10.
                    06  W65-INNAITAIKBN-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-JIRITUKBN.
                    06  W65-JIRITUKBN            PIC X(80).
                    06  W65-JIRITUKBN-COUNT      PIC S9(9) BINARY.
                    06  W65-JIRITUKBN-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-JIRITUTAIKBN.
                    06  W65-JIRITUTAIKBN         PIC X(40).
                    06  W65-JIRITUTAIKBN-STATE   PIC S9(9) BINARY.
                    06  W65-JIRITUTAIKBN-COUNT   PIC S9(9) BINARY.
                    06  W65-JIRITUTAIKBN-ITEM    PIC X(40) OCCURS 10.
                    06  W65-JIRITUTAIKBN-ITEM-STATE
                                                 PIC S9(9) BINARY.
                04  W65-N06-STATE   PIC S9(9)    BINARY.
                04  W65-FXD13.
                  05  W65-LBLSKJHON              PIC X(40).
                  05  W65-CMB-SKJRYOKBNHON.
                    06  W65-SKJRYOKBNHON         PIC X(80).
                    06  W65-SKJRYOKBNHON-COUNT   PIC S9(9) BINARY.
                    06  W65-SKJRYOKBNHON-ITEM    PIC X(80) OCCURS 10.
                  05  W65-CMB-SKJRYOTAIKBNHON.
                    06  W65-SKJRYOTAIKBNHON      PIC X(40).
                    06  W65-SKJRYOTAIKBNHON-STATE PIC S9(9) BINARY.
                    06  W65-SKJRYOTAIKBNHON-COUNT PIC S9(9) BINARY.
                    06  W65-SKJRYOTAIKBNHON-ITEM  PIC X(40) OCCURS 10.
                    06  W65-SKJRYOTAIKBNHON-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-SEIRYOKBNHON.
                    06  W65-SEIRYOKBNHON         PIC X(80).
                    06  W65-SEIRYOKBNHON-COUNT   PIC S9(9) BINARY.
                    06  W65-SEIRYOKBNHON-ITEM    PIC X(80) OCCURS 10.
                  05  W65-CMB-SEIRYOTAIKBNHON.
                    06  W65-SEIRYOTAIKBNHON      PIC X(40).
                    06  W65-SEIRYOTAIKBNHON-STATE PIC S9(9) BINARY.
                    06  W65-SEIRYOTAIKBNHON-COUNT PIC S9(9) BINARY.
                    06  W65-SEIRYOTAIKBNHON-ITEM PIC X(40) OCCURS 10.
                    06  W65-SEIRYOTAIKBNHON-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-SEIKANKBNHON.
                    06  W65-SEIKANKBNHON         PIC X(80).
                    06  W65-SEIKANKBNHON-COUNT   PIC S9(9) BINARY.
                    06  W65-SEIKANKBNHON-ITEM    PIC X(80) OCCURS 10.
                  05  W65-CMB-SEIKANTAIKBNHON.
                    06  W65-SEIKANTAIKBNHON      PIC X(40).
                    06  W65-SEIKANTAIKBNHON-STATE PIC S9(9) BINARY.
                    06  W65-SEIKANTAIKBNHON-COUNT PIC S9(9) BINARY.
                    06  W65-SEIKANTAIKBNHON-ITEM PIC X(40) OCCURS 10.
                    06  W65-SEIKANTAIKBNHON-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-LBLSKJTSY              PIC X(40).
                  05  W65-CMB-SKJRYOKBNTSY.
                    06  W65-SKJRYOKBNTSY         PIC X(80).
                    06  W65-SKJRYOKBNTSY-COUNT   PIC S9(9) BINARY.
                    06  W65-SKJRYOKBNTSY-ITEM    PIC X(80) OCCURS 10.
                  05  W65-CMB-SKJRYOTAIKBNTSY.
                    06  W65-SKJRYOTAIKBNTSY      PIC X(40).
                    06  W65-SKJRYOTAIKBNTSY-STATE PIC S9(9) BINARY.
                    06  W65-SKJRYOTAIKBNTSY-COUNT PIC S9(9) BINARY.
                    06  W65-SKJRYOTAIKBNTSY-ITEM  PIC X(40) OCCURS 10.
                    06  W65-SKJRYOTAIKBNTSY-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-SEIRYOKBNTSY.
                    06  W65-SEIRYOKBNTSY         PIC X(80).
                    06  W65-SEIRYOKBNTSY-COUNT   PIC S9(9) BINARY.
                    06  W65-SEIRYOKBNTSY-ITEM    PIC X(80) OCCURS 10.
                  05  W65-CMB-SEIRYOTAIKBNTSY.
                    06  W65-SEIRYOTAIKBNTSY      PIC X(40).
                    06  W65-SEIRYOTAIKBNTSY-STATE PIC S9(9) BINARY.
                    06  W65-SEIRYOTAIKBNTSY-COUNT PIC S9(9) BINARY.
                    06  W65-SEIRYOTAIKBNTSY-ITEM  PIC X(40) OCCURS 10.
                    06  W65-SEIRYOTAIKBNTSY-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-SEIKANKBNTSY.
                    06  W65-SEIKANKBNTSY         PIC X(80).
                    06  W65-SEIKANKBNTSY-COUNT   PIC S9(9) BINARY.
                    06  W65-SEIKANKBNTSY-ITEM    PIC X(80) OCCURS 10.
                  05  W65-CMB-SEIKANTAIKBNTSY.
                    06  W65-SEIKANTAIKBNTSY      PIC X(40).
                    06  W65-SEIKANTAIKBNTSY-STATE PIC S9(9) BINARY.
                    06  W65-SEIKANTAIKBNTSY-COUNT PIC S9(9) BINARY.
                    06  W65-SEIKANTAIKBNTSY-ITEM  PIC X(40) OCCURS 10.
                    06  W65-SEIKANTAIKBNTSY-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-LBLSKJTNK              PIC X(40).
                  05  W65-CMB-SKJRYOKBNTNK.
                    06  W65-SKJRYOKBNTNK         PIC X(80).
                    06  W65-SKJRYOKBNTNK-COUNT   PIC S9(9) BINARY.
                    06  W65-SKJRYOKBNTNK-ITEM    PIC X(80) OCCURS 10.
                  05  W65-CMB-SKJRYOTAIKBNTNK.
                    06  W65-SKJRYOTAIKBNTNK      PIC X(40).
                    06  W65-SKJRYOTAIKBNTNK-STATE PIC S9(9) BINARY.
                    06  W65-SKJRYOTAIKBNTNK-COUNT PIC S9(9) BINARY.
                    06  W65-SKJRYOTAIKBNTNK-ITEM  PIC X(40) OCCURS 10.
                    06  W65-SKJRYOTAIKBNTNK-ITEM-STATE
                                                PIC S9(9) BINARY.
                  05  W65-CMB-SEIRYOKBNTNK.
                    06  W65-SEIRYOKBNTNK        PIC X(80).
                    06  W65-SEIRYOKBNTNK-COUNT  PIC S9(9) BINARY.
                    06  W65-SEIRYOKBNTNK-ITEM   PIC X(80) OCCURS 10.
                  05  W65-CMB-SEIRYOTAIKBNTNK.
                    06  W65-SEIRYOTAIKBNTNK     PIC X(40).
                    06  W65-SEIRYOTAIKBNTNK-STATE PIC S9(9) BINARY.
                    06  W65-SEIRYOTAIKBNTNK-COUNT PIC S9(9) BINARY.
                    06  W65-SEIRYOTAIKBNTNK-ITEM  PIC X(40) OCCURS 10.
                    06  W65-SEIRYOTAIKBNTNK-ITEM-STATE
                                                 PIC S9(9) BINARY.
                  05  W65-CMB-SEIKANKBNTNK.
                    06  W65-SEIKANKBNTNK         PIC X(80).
                    06  W65-SEIKANKBNTNK-COUNT   PIC S9(9) BINARY.
                    06  W65-SEIKANKBNTNK-ITEM    PIC X(80) OCCURS 10.
                  05  W65-CMB-SEIKANTAIKBNTNK.
                    06  W65-SEIKANTAIKBNTNK      PIC X(40).
                    06  W65-SEIKANTAIKBNTNK-STATE PIC S9(9) BINARY.
                    06  W65-SEIKANTAIKBNTNK-COUNT PIC S9(9) BINARY.
                    06  W65-SEIKANTAIKBNTNK-ITEM PIC X(40) OCCURS 10.
                    06  W65-SEIKANTAIKBNTNK-ITEM-STATE
                                                 PIC S9(9) BINARY.
                04  W65-N07-STATE   PIC S9(9)    BINARY.
                04  W65-FXD14.
                  05  W65-CMB-KAICNTKBNG.
                    06  W65-KAICNTKBNG           PIC X(80).
                    06  W65-KAICNTKBNG-COUNT     PIC S9(9) BINARY.
                    06  W65-KAICNTKBNG-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-KAICNTTAIKBNG.
                    06  W65-KAICNTTAIKBNG        PIC X(40).
                    06  W65-KAICNTTAIKBNG-STATE  PIC S9(9) BINARY.
                    06  W65-KAICNTTAIKBNG-COUNT  PIC S9(9) BINARY.
                    06  W65-KAICNTTAIKBNG-ITEM   PIC X(40) OCCURS 10.
                    06  W65-KAICNTTAIKBNG-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-CYOKEIKBNG.
                    06  W65-CYOKEIKBNG           PIC X(80).
                    06  W65-CYOKEIKBNG-COUNT     PIC S9(9) BINARY.
                    06  W65-CYOKEIKBNG-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-CYOKEITAIKBNG.
                    06  W65-CYOKEITAIKBNG        PIC X(40).
                    06  W65-CYOKEITAIKBNG-STATE  PIC S9(9) BINARY.
                    06  W65-CYOKEITAIKBNG-COUNT  PIC S9(9) BINARY.
                    06  W65-CYOKEITAIKBNG-ITEM   PIC X(40) OCCURS 10.
                    06  W65-CYOKEITAIKBNG-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-GNBKEIKBNKG.
                    06  W65-GNBKEIKBNKG          PIC X(80).
                    06  W65-GNBKEIKBNKG-COUNT    PIC S9(9) BINARY.
                    06  W65-GNBKEIKBNKG-ITEM     PIC X(80) OCCURS 10.
                  05  W65-CMB-GNBKEITAIKBNKG.
                    06  W65-GNBKEITAIKBNKG       PIC X(40).
                    06  W65-GNBKEITAIKBNKG-STATE PIC S9(9) BINARY.
                    06  W65-GNBKEITAIKBNKG-COUNT PIC S9(9) BINARY.
                    06  W65-GNBKEITAIKBNKG-ITEM  PIC X(40) OCCURS 10.
                    06  W65-GNBKEITAIKBNKG-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-ZENKEIKBNG.
                    06  W65-ZENKEIKBNG           PIC X(80).
                    06  W65-ZENKEIKBNG-COUNT     PIC S9(9) BINARY.
                    06  W65-ZENKEIKBNG-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-ZENKEITAIKBNG.
                    06  W65-ZENKEITAIKBNG        PIC X(40).
                    06  W65-ZENKEITAIKBNG-STATE  PIC S9(9) BINARY.
                    06  W65-ZENKEITAIKBNG-COUNT  PIC S9(9) BINARY.
                    06  W65-ZENKEITAIKBNG-ITEM   PIC X(40) OCCURS 10.
                    06  W65-ZENKEITAIKBNG-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-SINKEIKBNG.
                    06  W65-SINKEIKBNG           PIC X(80).
                    06  W65-SINKEIKBNG-COUNT     PIC S9(9) BINARY.
                    06  W65-SINKEIKBNG-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-SINKEITAIKBNG.
                    06  W65-SINKEITAIKBNG        PIC X(40).
                    06  W65-SINKEITAIKBNG-STATE  PIC S9(9) BINARY.
                    06  W65-SINKEITAIKBNG-COUNT  PIC S9(9) BINARY.
                    06  W65-SINKEITAIKBNG-ITEM   PIC X(40) OCCURS 10.
                    06  W65-SINKEITAIKBNG-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-SYOKEIKBNG.
                    06  W65-SYOKEIKBNG           PIC X(80).
                    06  W65-SYOKEIKBNG-COUNT     PIC S9(9) BINARY.
                    06  W65-SYOKEIKBNG-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-SYOKEITAIKBNG.
                    06  W65-SYOKEITAIKBNG        PIC X(40).
                    06  W65-SYOKEITAIKBNG-STATE  PIC S9(9) BINARY.
                    06  W65-SYOKEITAIKBNG-COUNT  PIC S9(9) BINARY.
                    06  W65-SYOKEITAIKBNG-ITEM   PIC X(40) OCCURS 10.
                    06  W65-SYOKEITAIKBNG-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-SYOKEIKBNN.
                    06  W65-SYOKEIKBNN           PIC X(80).
                    06  W65-SYOKEIKBNN-COUNT     PIC S9(9) BINARY.
                    06  W65-SYOKEIKBNN-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-SYOKEITAIKBNN.
                    06  W65-SYOKEITAIKBNN        PIC X(40).
                    06  W65-SYOKEITAIKBNN-STATE  PIC S9(9) BINARY.
                    06  W65-SYOKEITAIKBNN-COUNT  PIC S9(9) BINARY.
                    06  W65-SYOKEITAIKBNN-ITEM   PIC X(40) OCCURS 10.
                    06  W65-SYOKEITAIKBNN-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-NYUKEIKBNN.
                    06  W65-NYUKEIKBNN           PIC X(80).
                    06  W65-NYUKEIKBNN-COUNT     PIC S9(9) BINARY.
                    06  W65-NYUKEIKBNN-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-NYUKEITAIKBNN.
                    06  W65-NYUKEITAIKBNN        PIC X(40).
                    06  W65-NYUKEITAIKBNN-STATE  PIC S9(9) BINARY.
                    06  W65-NYUKEITAIKBNN-COUNT  PIC S9(9) BINARY.
                    06  W65-NYUKEITAIKBNN-ITEM   PIC X(40) OCCURS 10.
                    06  W65-NYUKEITAIKBNN-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-DAYKEIKBNN.
                    06  W65-DAYKEIKBNN           PIC X(80).
                    06  W65-DAYKEIKBNN-COUNT     PIC S9(9) BINARY.
                    06  W65-DAYKEIKBNN-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-DAYKEITAIKBNN.
                    06  W65-DAYKEITAIKBNN        PIC X(40).
                    06  W65-DAYKEITAIKBNN-STATE  PIC S9(9) BINARY.
                    06  W65-DAYKEITAIKBNN-COUNT  PIC S9(9) BINARY.
                    06  W65-DAYKEITAIKBNN-ITEM   PIC X(40) OCCURS 10.
                    06  W65-DAYKEITAIKBNN-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-NINNASIKBN.
                    06  W65-NINNASIKBN           PIC X(80).
                    06  W65-NINNASIKBN-COUNT     PIC S9(9) BINARY.
                    06  W65-NINNASIKBN-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-NINNASITAIKBN.
                    06  W65-NINNASITAIKBN        PIC X(40).
                    06  W65-NINNASITAIKBN-STATE  PIC S9(9) BINARY.
                    06  W65-NINNASITAIKBN-COUNT  PIC S9(9) BINARY.
                    06  W65-NINNASITAIKBN-ITEM   PIC X(40) OCCURS 10.
                    06  W65-NINNASITAIKBN-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-NINJYOKBN.
                    06  W65-NINJYOKBN            PIC X(80).
                    06  W65-NINJYOKBN-COUNT      PIC S9(9) BINARY.
                    06  W65-NINJYOKBN-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-NINJYOTAIKBN.
                    06  W65-NINJYOTAIKBN         PIC X(40).
                    06  W65-NINJYOTAIKBN-STATE   PIC S9(9) BINARY.
                    06  W65-NINJYOTAIKBN-COUNT   PIC S9(9) BINARY.
                    06  W65-NINJYOTAIKBN-ITEM    PIC X(40) OCCURS 10.
                    06  W65-NINJYOTAIKBN-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-KOGAKKEIKBN.
                    06  W65-KOGAKKEIKBN            PIC X(80).
                    06  W65-KOGAKKEIKBN-COUNT      PIC S9(9) BINARY.
                    06  W65-KOGAKKEIKBN-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-KOGAKKEITAIKBN.
                    06  W65-KOGAKKEITAIKBN         PIC X(40).
                    06  W65-KOGAKKEITAIKBN-STATE   PIC S9(9) BINARY.
                    06  W65-KOGAKKEITAIKBN-COUNT   PIC S9(9) BINARY.
                    06  W65-KOGAKKEITAIKBN-ITEM    PIC X(40) OCCURS 10.
                    06  W65-KOGAKKEITAIKBN-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-TANKIKBN.
                    06  W65-TANKIKBN            PIC X(80).
                    06  W65-TANKIKBN-COUNT      PIC S9(9) BINARY.
                    06  W65-TANKIKBN-ITEM       PIC X(80) OCCURS 10.
                  05  W65-CMB-TANKITAIKBN.
                    06  W65-TANKITAIKBN         PIC X(40).
                    06  W65-TANKITAIKBN-STATE   PIC S9(9) BINARY.
                    06  W65-TANKITAIKBN-COUNT   PIC S9(9) BINARY.
                    06  W65-TANKITAIKBN-ITEM    PIC X(40) OCCURS 10.
                    06  W65-TANKITAIKBN-ITEM-STATE PIC S9(9) BINARY.
                04  W65-N08-STATE   PIC S9(9)    BINARY.
                04  W65-FXD1.
                  05  W65-CMB-KAICNTKBNN.
                    06  W65-KAICNTKBNN           PIC X(80).
                    06  W65-KAICNTKBNN-COUNT     PIC S9(9) BINARY.
                    06  W65-KAICNTKBNN-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-KAICNTTAIKBNN.
                    06  W65-KAICNTTAIKBNN        PIC X(40).
                    06  W65-KAICNTTAIKBNN-STATE  PIC S9(9) BINARY.
                    06  W65-KAICNTTAIKBNN-COUNT  PIC S9(9) BINARY.
                    06  W65-KAICNTTAIKBNN-ITEM   PIC X(40) OCCURS 10.
                    06  W65-KAICNTTAIKBNN-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-CYOKEIKBNN.
                    06  W65-CYOKEIKBNN           PIC X(80).
                    06  W65-CYOKEIKBNN-COUNT     PIC S9(9) BINARY.
                    06  W65-CYOKEIKBNN-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-CYOKEITAIKBNN.
                    06  W65-CYOKEITAIKBNN        PIC X(40).
                    06  W65-CYOKEITAIKBNN-STATE  PIC S9(9) BINARY.
                    06  W65-CYOKEITAIKBNN-COUNT  PIC S9(9) BINARY.
                    06  W65-CYOKEITAIKBNN-ITEM   PIC X(40) OCCURS 10.
                    06  W65-CYOKEITAIKBNN-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-ZENDAYKBNN.
                    06  W65-ZENDAYKBNN          PIC X(80).
                    06  W65-ZENDAYKBNN-COUNT    PIC S9(9) BINARY.
                    06  W65-ZENDAYKBNN-ITEM     PIC X(80) OCCURS 10.
                  05  W65-CMB-ZENDAYTAIKBNN.
                    06  W65-ZENDAYTAIKBNN       PIC X(40).
                    06  W65-ZENDAYTAIKBNN-STATE PIC S9(9) BINARY.
                    06  W65-ZENDAYTAIKBNN-COUNT PIC S9(9) BINARY.
                    06  W65-ZENDAYTAIKBNN-ITEM  PIC X(40) OCCURS 10.
                    06  W65-ZENDAYTAIKBNN-ITEM-STATE PIC S9(9) BINARY.
                  05  W65-CMB-ZENKEI2KBNG.
                    06  W65-ZENKEI2KBNG           PIC X(80).
                    06  W65-ZENKEI2KBNG-COUNT     PIC S9(9) BINARY.
                    06  W65-ZENKEI2KBNG-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-ZENKEI2TAIKBNG.
                    06  W65-ZENKEI2TAIKBNG        PIC X(40).
                    06  W65-ZENKEI2TAIKBNG-STATE  PIC S9(9) BINARY.
                    06  W65-ZENKEI2TAIKBNG-COUNT  PIC S9(9) BINARY.
                    06  W65-ZENKEI2TAIKBNG-ITEM   PIC X(40) OCCURS 10.
                    06  W65-ZENKEI2TAIKBNG-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-ZENDAY2KBNN.
                    06  W65-ZENDAY2KBNN           PIC X(80).
                    06  W65-ZENDAY2KBNN-COUNT     PIC S9(9) BINARY.
                    06  W65-ZENDAY2KBNN-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-ZENDAY2TAIKBNN.
                    06  W65-ZENDAY2TAIKBNN        PIC X(40).
                    06  W65-ZENDAY2TAIKBNN-STATE  PIC S9(9) BINARY.
                    06  W65-ZENDAY2TAIKBNN-COUNT  PIC S9(9) BINARY.
                    06  W65-ZENDAY2TAIKBNN-ITEM   PIC X(40) OCCURS 10.
                    06  W65-ZENDAY2TAIKBNN-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-SYOKEI2KBNG.
                    06  W65-SYOKEI2KBNG           PIC X(80).
                    06  W65-SYOKEI2KBNG-COUNT     PIC S9(9) BINARY.
                    06  W65-SYOKEI2KBNG-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-SYOKEI2TAIKBNG.
                    06  W65-SYOKEI2TAIKBNG        PIC X(40).
                    06  W65-SYOKEI2TAIKBNG-STATE  PIC S9(9) BINARY.
                    06  W65-SYOKEI2TAIKBNG-COUNT  PIC S9(9) BINARY.
                    06  W65-SYOKEI2TAIKBNG-ITEM   PIC X(40) OCCURS 10.
                    06  W65-SYOKEI2TAIKBNG-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-SYOKEI2KBNN.
                    06  W65-SYOKEI2KBNN           PIC X(80).
                    06  W65-SYOKEI2KBNN-COUNT     PIC S9(9) BINARY.
                    06  W65-SYOKEI2KBNN-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-SYOKEI2TAIKBNN.
                    06  W65-SYOKEI2TAIKBNN        PIC X(40).
                    06  W65-SYOKEI2TAIKBNN-STATE  PIC S9(9) BINARY.
                    06  W65-SYOKEI2TAIKBNN-COUNT  PIC S9(9) BINARY.
                    06  W65-SYOKEI2TAIKBNN-ITEM   PIC X(40) OCCURS 10.
                    06  W65-SYOKEI2TAIKBNN-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-NINNASIKBNK.
                    06  W65-NINNASIKBNK           PIC X(80).
                    06  W65-NINNASIKBNK-COUNT     PIC S9(9) BINARY.
                    06  W65-NINNASIKBNK-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-NINNASITAIKBNK.
                    06  W65-NINNASITAIKBNK        PIC X(40).
                    06  W65-NINNASITAIKBNK-STATE  PIC S9(9) BINARY.
                    06  W65-NINNASITAIKBNK-COUNT  PIC S9(9) BINARY.
                    06  W65-NINNASITAIKBNK-ITEM   PIC X(40) OCCURS 10.
                    06  W65-NINNASITAIKBNK-ITEM-STATE  PIC S9(9) BINARY.
                04  W65-N09-STATE   PIC S9(9)    BINARY.
                04  W65-FXD2.
                  05  W65-CMB-CYOKIFTNKBN.
                    06  W65-CYOKIFTNKBN           PIC X(80).
                    06  W65-CYOKIFTNKBN-COUNT     PIC S9(9) BINARY.
                    06  W65-CYOKIFTNKBN-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-CYOKIFTNTAIKBN.
                    06  W65-CYOKIFTNTAIKBN        PIC X(40).
                    06  W65-CYOKIFTNTAIKBN-STATE  PIC S9(9) BINARY.
                    06  W65-CYOKIFTNTAIKBN-COUNT  PIC S9(9) BINARY.
                    06  W65-CYOKIFTNTAIKBN-ITEM   PIC X(40) OCCURS 10.
                    06  W65-CYOKIFTNTAIKBN-ITEM-STATE  PIC S9(9) BINARY.
                  05  W65-CMB-CYOKIKSIKBN.
                    06  W65-CYOKIKSIKBN           PIC X(80).
                    06  W65-CYOKIKSIKBN-COUNT     PIC S9(9) BINARY.
                    06  W65-CYOKIKSIKBN-ITEM      PIC X(80) OCCURS 10.
                  05  W65-CMB-CYOKIKSITAIKBN.
                    06  W65-CYOKIKSITAIKBN        PIC X(40).
                    06  W65-CYOKIKSITAIKBN-STATE  PIC S9(9) BINARY.
                    06  W65-CYOKIKSITAIKBN-COUNT  PIC S9(9) BINARY.
                    06  W65-CYOKIKSITAIKBN-ITEM   PIC X(40) OCCURS 10.
                    06  W65-CYOKIKSITAIKBN-ITEM-STATE  PIC S9(9) BINARY.
                04  W65-N10-STATE   PIC S9(9)    BINARY.
              03  W65-YUKOSTYMD                  PIC X(10).
              03  W65-YUKOSTYMD-STATE            PIC S9(9) BINARY.
              03  W65-YUKOEDYMD                  PIC X(10).
              03  W65-YUKOEDYMD-STATE            PIC S9(9) BINARY.
              03  W65-SELNUM2 PIC S9(2).
              03  W65-SELNUM2-STATE              PIC S9(9) BINARY.
              03  W65-LBLMODE-STYLE              PIC X(20).
              03  W65-LBLMODE                    PIC X(40).
              03  W65-SCRW1.
                04  W65-KOHLST.
                  05  W65-KOHLST-COUNT           PIC S9(9) BINARY.
                  05  W65-KOHLST-ROW             PIC S9(9) BINARY.
                  05  W65-KOHLST-ROWATTR         PIC S9(9) BINARY.
                  05  W65-KOHLST-ITEM            OCCURS  100.
                    06  W65-TKNO                 PIC X(4).
                    06  W65-TKKOHNUM             PIC X(3).
                    06  W65-TKKOHNAME            PIC X(100).
                    06  W65-TKYUKSTYMD           PIC X(10).
                    06  W65-TKYUKEDYMD           PIC X(10).
                  05  W65-KOHLST-SELECT          PIC X  OCCURS  100.
              03  W65-SCRW29.
                04  W65-RIREKILST.
                  05  W65-RIREKILST-COUNT   PIC S9(9) BINARY.
                  05  W65-RIREKILST-ROW     PIC S9(9) BINARY.
                  05  W65-RIREKILST-ROWATTR PIC S9(9) BINARY.
                  05  W65-RIREKILST-ITEM    OCCURS  20.
                    06  W65-TRNO            PIC X(3).
                    06  W65-TRKIKAN         PIC X(30).
                  05  W65-RIREKILST-SELECT  PIC X   OCCURS  20.
