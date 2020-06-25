        02  W23.
      *     02  W23-FIXED6.
              03  W23-YUKOSTYMD PIC X(9).
              03  W23-YUKOEDYMD PIC X(9).
              03  W23-SEIKYU01  PIC X(3).
              03  W23-SEIKYU02  PIC X(3).
              03  W23-SEIKYU03  PIC X(3).
              03  W23-CMB-NYUKINKBN.
                04  W23-NYUKINKBN          PIC X(60).
                04  W23-NYUKINKBN-COUNT    PIC S9(9)   BINARY.
                04  W23-NYUKINKBNLST       PIC X(60)   OCCURS  5 TIMES.
              03  W23-CMB-NYUKINPRTKBN.
                04  W23-NYUKINPRTKBN       PIC X(30).
                04  W23-NYUKINPRTKBN-STATE PIC S9(9)   BINARY.
                04  W23-NYUKINPRTKBN-COUNT PIC S9(9)   BINARY.
                04  W23-NYUKINPRTKBNLST    PIC X(30)   OCCURS  5 TIMES.
                04  W23-NYUKINPRTKBNLST-STATE
                                           PIC S9(9)   BINARY.
              03  W23-CMB-SAISYORIKBN.
                04  W23-SAISYORIKBN        PIC X(60).
                04  W23-SAISYORIKBN-COUNT  PIC S9(9)   BINARY.
                04  W23-SAISYORIKBNLST     PIC X(60)   OCCURS  5 TIMES.
              03  W23-CMB-CHGWINDOWKBN.
                04  W23-CHGWINDOWKBN       PIC X(20).
                04  W23-CHGWINDOWKBN-COUNT PIC S9(9)   BINARY.
                04  W23-CHGWINDOWKBNLST    PIC X(20)   OCCURS  5 TIMES.
              03  W23-CMB-DELKBN.
                04  W23-DELKBN             PIC X(20).
                04  W23-DELKBN-COUNT       PIC S9(9)   BINARY.
                04  W23-DELKBNLST          PIC X(20)   OCCURS  5 TIMES.
              03  W23-CMB-SKYSUMKBN.
                04  W23-SKYSUMKBN          PIC X(30).
                04  W23-SKYSUMKBN-COUNT    PIC S9(9)   BINARY.
                04  W23-SKYSUMKBNLST       PIC X(30)   OCCURS  5 TIMES.
              03  W23-CMB-SKYPRTKBN.
                04  W23-SKYPRTKBN          PIC X(40).
                04  W23-SKYPRTKBN-COUNT    PIC S9(9)   BINARY.
                04  W23-SKYPRTKBNLST       PIC X(40)   OCCURS  5 TIMES.
              03  W23-CMB-SRYPRTKBN.
                04  W23-SRYPRTKBN          PIC X(40).
                04  W23-SRYPRTKBN-COUNT    PIC S9(9)   BINARY.
                04  W23-SRYPRTKBNLST       PIC X(40)   OCCURS  5 TIMES.
              03  W23-CMB-PTINFREFKBN.
                04  W23-PTINFREFKBN        PIC X(30).
                04  W23-PTINFREFKBN-COUNT  PIC S9(9)   BINARY.
                04  W23-PTINFREFKBNLST     PIC X(30)   OCCURS  5 TIMES.
              03  W23-CMB-PRTJYUN.
                04  W23-PRTJYUN            PIC X(25).
                04  W23-PRTJYUN-COUNT      PIC S9(9)   BINARY.
                04  W23-PRTJYUNLST         PIC X(25)   OCCURS  10 TIMES.
              03  W23-CMB-TAIINGRPKBN.
                04  W23-TAIINGRPKBN        PIC X(30).
                04  W23-TAIINGRPKBN-COUNT  PIC S9(9)   BINARY.
                04  W23-TAIINGRPKBNLST     PIC X(30)   OCCURS  5 TIMES.
              03  W23-FR-REPO.
                04  W23-FR-REPO-VISIBLE    PIC X(01).
                04  W23-LBLMSG01-STYLE     PIC X(20).
                04  W23-LBLMSG01           PIC X(80).
                04  W23-REPOS-NAME         PIC X(128).
                04  W23-SKYPRT-NAME        PIC X(20).
                04  W23-SRYPRT-NAME        PIC X(20).

