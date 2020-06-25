            02  T06.
              03  T06-B08-STATE   PIC S9(9)   BINARY.
              03  T06-B12-STATE   PIC S9(9)   BINARY.
              03  T06-B17-STATE   PIC S9(9)   BINARY.
              03  T06-B18-STATE   PIC S9(9)   BINARY.
              03  T06-TITLE1-STYLE PIC X(20).
              03  T06-TITLE1       PIC X(14).
              03  T06-TITLE2-STYLE PIC X(20).
              03  T06-TITLE2       PIC X(10).
              03  T06-TITLE3-STYLE PIC X(20).
              03  T06-TITLE3       PIC X(50).
              03  T06-SELNUM.
                04  T06-VALUE   PIC S9(4).
              03  T06-SRYYM.
                04  T06-VALUE   PIC X(7).
              03  T06-PTNUM.
                04  T06-VALUE   PIC X(20).
              03  T06-NYUGAIKBN.
                04  T06-VALUE   PIC X(1).
              03  T06-NAME.
                04  T06-VALUE   PIC X(40).
              03  T06-SRYYM-H.
                04  T06-VALUE   PIC X(7).
              03  T06-SRYYMKBN-COMBO.
                04  T06-SRYYMKBN-COM-STATE    PIC S9(9)   BINARY.
                04  T06-SRYYMKBN-COM-VISIBLE  PIC X(01).
                04  T06-SRYYMKBN.
                  05  T06-VALUE    PIC X(18).
                04  T06-SRYYMKBN-COUNT PIC S9(9)   BINARY.
                04  T06-SRYYMKBN-ITEM
                        PIC X(18)   OCCURS  10 TIMES.
              03  T06-SKYYMD-H.
                04  T06-VALUE   PIC X(10).
              03  T06-HKNJANAME-STYLE    PIC X(20).
              03  T06-HKNJANAME          PIC X(30).
              03  T06-TEISYUTUSAKI1-STATE   PIC S9(9)   BINARY.
              03  T06-TEISYUTUSAKI1.
                04  T06-VALUE   PIC X.
              03  T06-TEISYUTUSAKI2-STATE   PIC S9(9)   BINARY.
              03  T06-TEISYUTUSAKI2.
                04  T06-VALUE   PIC X.
              03  T06-TEISYUTUSAKI3-STATE   PIC S9(9)   BINARY.
              03  T06-TEISYUTUSAKI3.
                04  T06-VALUE   PIC X.
              03  T06-TEISYUTUSAKI4-STATE   PIC S9(9)   BINARY.
              03  T06-TEISYUTUSAKI4.
                04  T06-VALUE   PIC X.
              03  T06-SCROLLEDWINDOW3.
                04  T06-KANRI-LIST.
                  05  T06-KANRI-LIST-COUNT   PIC S9(9)   BINARY.
                  05  T06-KANRI-LIST-ROW PIC S9(9)   BINARY.
                  05  T06-KANRI-LIST-ITEM        OCCURS  500 TIMES.
                    06  T06-KANRI-TRENNUM     PIC X(4).
                    06  T06-KANRI-TUKEYMD     PIC X(9).
                    06  T06-KANRI-TUCNT       PIC X(3).
                    06  T06-KANRI-TSKYYMD     PIC X(9).
                    06  T06-KANRI-TSCNT       PIC X(2).
                    06  T06-KANRI-TNOSEND-PATIENT-CNT PIC X(5).
                    06  T06-KANRI-TSEND-TOTAL-CNT     PIC X(5).
                  05  T06-KANRI-LIST-SELECT
                          PIC X       OCCURS  500 TIMES.
              03  T06-SCROLLEDWINDOW2.
                04  T06-BODY-LIST.
                  05  T06-BODY-LIST-COUNT     PIC S9(9)   BINARY.
                  05  T06-BODY-LIST-ROW       PIC S9(9)   BINARY.
                  05  T06-BODY-LIST-ITEM          OCCURS  1000 TIMES.
                    06  T06-BODY-TRENNUM      PIC X(4).
                    06  T06-BODY-TNYUGAIKBN   PIC X(2).
                    06  T06-BODY-THENREI-FLG  PIC X(2).
                    06  T06-BODY-TPTNUM       PIC X(20).
                    06  T06-BODY-TNAME        PIC X(40).
                    06  T06-BODY-TSRYYM       PIC X(6).
                    06  T06-BODY-TBIRTHDAY    PIC X(9).
                    06  T06-BODY-TRECESYUBETU PIC X(4).
                    06  T06-BODY-THKNJANUM    PIC X(8).
                    06  T06-BODY-TKIGO        PIC X(40).
                    06  T06-BODY-TERRCD       PIC X(4).
                    06  T06-BODY-TERR         PIC X(200).
                  05  T06-BODY-LIST-SELECT
                          PIC X       OCCURS  1000 TIMES.
              03  T06-SCROLLEDWINDOW1.
                04  T06-RECE-LIST.
                  05  T06-RECE-LIST-HYODAI1   PIC X(14).
                  05  T06-RECE-LIST-COUNT     PIC S9(9)   BINARY.
                  05  T06-RECE-LIST-ROW       PIC S9(9)   BINARY.
                  05  T06-RECE-LIST-ITEM          OCCURS  1000 TIMES.
                    06  T06-RECE-TRENNUM      PIC X(4).
                    06  T06-RECE-TNYUGAIKBN   PIC X(2).
                    06  T06-RECE-TERR         PIC X(2).
                    06  T06-RECE-TPTNUM       PIC X(20).
                    06  T06-RECE-TNAME        PIC X(40).
                    06  T06-RECE-TSRYYM       PIC X(6).
                    06  T06-RECE-TRECESYUBETU PIC X(10).
                    06  T06-RECE-THKNJANUM    PIC X(8).
                    06  T06-RECE-TKIGO        PIC X(40).
                    06  T06-RECE-TRECEIPT-KBN PIC X(4).
                    06  T06-RECE-TDISASTER    PIC X(2).
                    06  T06-RECE-THOJOKBN     PIC X(2).
                    06  T06-RECE-TCREYMD      PIC X(9).
                  05  T06-RECE-LIST-SELECT
                          PIC X       OCCURS  1000 TIMES.
