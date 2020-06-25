      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
       PROGRAM-ID.             ORCR0055.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �쥻�ץ�
      *  ����ݡ��ͥ��̾  : 
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC��ƣ����   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     01.06.29  �ݸ����ֹ桦
      *                                     Ŭ�ѳ���ǯ�������ɲ�
      *  01.00.02    MCC-ƣ��     02.05.14  Ʊ�쳫��������̾��ž����ʬ�ΤȤ�
      *                                     �ҤȤĤ������Ϥ���
      *  01.00.03    MCC-ƣ��     02.06.11  ž������ɽ������Ȥ�Ʊ�쳫������
      *                                     ��̾��ž����ʬ��ž�����ΤȤ�
      *                                     �ҤȤĤ������Ϥ���
      *  01.00.04    NACL-ƣ��    03.03.12  �����ʬ���ɲ�
      *  ��߽����ɲ�
      *  01.01.01    NACL-ƣ��    04/09/28  ��߽������ɲ�
      *
      *  02.06.01    NACL-����    05/05/30  realtime preview �����ɲ�
      *
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *
      *  04.05.01    NACL-ƣ��    09/11/25  �쥻�ť����ȳ�ĥ�б�
      *                                     ������̾��������̾ʸ�������ɲ�
      *
      *  04.06.01    NACL-ƣ��    10/07/20  �쥻�ץ���̾���ܽ��б�
      *  04.06.02    NACL-ƣ��    10/11/04  ��­�������б�
      *  04.06.03    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *  04.06.04    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      *
      *  04.08.01    NACL-ƣ��    14/07/07  ����ǥ��쥯�ȥ��б�
      *
      *  05.00.01    NACL-ƣ��    18/06/11  Warning �б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    �쥻�ץ����ٽ�  
      *
           SELECT  RECE03-FILE     ASSIGN  RECE03PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE03-KEY
                                   FILE    STATUS  IS  STS-RECE03.
      *
           SELECT  RECE031-FILE    ASSIGN  RECE031PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE031-KEY
                                   FILE    STATUS  IS  STS-RECE031.
      *    ���顼�ե�����
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    �쥻�ץ����ٽ�  
       FD  RECE03-FILE.
       01  RECE03-REC. 
           COPY    "CPRCF003.INC".
      *
       FD  RECE031-FILE.
       01  RECE031-REC. 
           COPY    "CPRCF0031.INC".
      *    ���顼�ե�����
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
      *     �쥻�ץ����٥ե����� ̾���ΰ� 
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE03PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE031PARA//.
      *
      *    ���顼�ե����� ̾���ΰ� 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECEERR//.
      *
      *    �ե�����̾����
           COPY    "CPRECEDAT1.INC".
           COPY    "CPTEMPFL.INC".
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-RECE031                             PIC X(02).
           03  STS-RECE03                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-OUT                                 PIC 9(06).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC  X(06).
               05  WRK-PARA-TERMID                     PIC  X(16).
               05  WRK-PARA-SYSYMD                     PIC  X(08).
               05  WRK-PARA-JOBID                      PIC  9(07).
               05  WRK-PARA-SHELLID                    PIC  X(08).
           03  WRK-PARA-HOSPNUM                        PIC 9(02).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-HOSPNUM       PIC 9(02).
               05  KEY-N-SRYYM         PIC 9(06).
               05  KEY-N-NYUGAIKBN     PIC X(01).
               05  KEY-N-PTID          PIC 9(10).
               05  KEY-N-RECEKA        PIC X(02).
               05  KEY-N-TEISYUTUSAKI  PIC 9(01).
               05  KEY-N-RECESYUBETU   PIC 9(04).
               05  KEY-N-HKNJANUM      PIC X(08).
               05  KEY-N-HOJOKBN       PIC X(01).
               05  KEY-N-KOHID         PIC 9(10).
               05  KEY-N-TEKSTYMD      PIC X(08). 
               05  KEY-N-RECEIPT-KBN   PIC X(10).
               05  KEY-N-SRYYMD        PIC X(08).
               05  KEY-N-TENKIKBN      PIC X(01).
               05  KEY-N-TENKIYMD      PIC X(08).
               05  KEY-N-BYOMEI        PIC X(160).
           03  KEY-OLD.
               05  KEY-O-HOSPNUM       PIC 9(02).
               05  KEY-O-SRYYM         PIC 9(06).
               05  KEY-O-NYUGAIKBN     PIC X(01).
               05  KEY-O-PTID          PIC 9(10).
               05  KEY-O-RECEKA        PIC X(02).
               05  KEY-O-TEISYUTUSAKI  PIC 9(01).
               05  KEY-O-RECESYUBETU   PIC 9(04).
               05  KEY-O-HKNJANUM      PIC X(08).
               05  KEY-O-HOJOKBN       PIC X(01).
               05  KEY-O-KOHID         PIC 9(10).
               05  KEY-O-TEKSTYMD      PIC X(08). 
               05  KEY-O-RECEIPT-KBN   PIC X(10).
               05  KEY-O-SRYYMD        PIC X(08).
               05  KEY-O-TENKIKBN      PIC X(01).
               05  KEY-O-TENKIYMD      PIC X(08).
               05  KEY-O-BYOMEI        PIC X(160).
      *
       01  WRK-ERRMSG-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-ERRMSG          PIC X(300).
           03  WRK-MCP-RC          PIC 9(9).
           03  WRK-ERR-FILE-STS    PIC X(02).
           03  WRK-ERR-FILE-MSG    PIC X(100).
           03  WRK-ERR-FILE-MSG1   PIC X(17).
           03  WRK-ERR-FLG         PIC 9(01).
      *    ���顼�ե�����̾���ΰ� 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME���BASENAME���ϰ���
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK2005.INC".
           COPY    "CPSK200501.INC"
                                   REPLACING  //SYS-200501//
                                   BY         //SYS-25//.
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
      *
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
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
      *
           PERFORM 300-TERM-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       SPA-AREA
      *
           MOVE    ZERO                TO  FLG-END
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    "RECE030"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           MOVE    "RECE031"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (3)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE03PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE031PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        �ģ¥����ץ����
               PERFORM 100-DBOPEN-SEC
      *
      *        ���ƥå״������Ͻ���
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0055"      TO  JOB-PGID
               MOVE    "����̾�Խ�"    TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        ��������������
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           OPEN    OUTPUT              RECE031-FILE
           CLOSE                       RECE031-FILE
      *
           OPEN    INPUT               RECE03-FILE
           IF      STS-RECE03          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERRMSG-AREA
               INITIALIZE                      WRK-ERRMSG-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE03PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE03          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE031-FILE
      *     
      *    �쥻�ץ��Խ���ʬ����
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    "2005"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM  TO  SYS-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           PERFORM 960-KANRIMST-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-2005-REC
           ELSE    
               MOVE    SPACE           TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-TENKIHENKBN    =   SPACE
               MOVE    ZERO             TO  SYS-2005-TENKIHENKBN
           END-IF
      *         
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    "2005"          TO  SYS-KANRICD
           MOVE    "01"            TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM  TO  SYS-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           PERFORM 960-KANRIMST-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-25-REC
           ELSE    
               INITIALIZE                   SYS-25-REC
           END-IF
           IF      SYS-25-BYOMEI-PRTJYUN
                                    =   SPACE
               MOVE    ZERO             TO  SYS-25-BYOMEI-PRTJYUN
           END-IF
      *
      *    �쥻�ץ����٣����ɤ߹���
           PERFORM 900-RECE03-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *    
           INITIALIZE                      RECE031-REC
           MOVE    RECE03-KEY1         TO  RECE031-KEY1
           MOVE    RECE03-SRYYMD       TO  RECE031-SRYYMD
           MOVE    RECE03-SYUBYOFLG    TO  RECE031-SYUBYOFLG   
           MOVE    RECE03-TENKIKBN     TO  RECE031-TENKIKBN
           MOVE    RECE03-RENNUM       TO  RECE031-RENNUM
           MOVE    RECE03-PTNUM        TO  RECE031-PTNUM
           MOVE    RECE03-SRYKA        TO  RECE031-SRYKA
           MOVE    RECE03-KHNBYOMEICD  TO  RECE031-KHNBYOMEICD   
           MOVE    RECE03-BYOMEICD-G   TO  RECE031-BYOMEICD-G
           MOVE    RECE03-UTAGAIFLG    TO  RECE031-UTAGAIFLG
           MOVE    RECE03-MANSEIKBN    TO  RECE031-MANSEIKBN
           IF      SYS-2005-TENKIHENKBN    =   ZERO
               MOVE    SPACE               TO  RECE031-TENKIYMD
           ELSE    
               MOVE    RECE03-TENKIYMD     TO  RECE031-TENKIYMD   
           END-IF    
           MOVE    RECE03-BYOMEI       TO  RECE031-BYOMEI
           MOVE    RECE03-BYOMEIMOJI   TO  RECE031-BYOMEIMOJI
           MOVE    RECE03-BYOMEIHENFLG TO  RECE031-BYOMEIHENFLG   
           MOVE    RECE03-IN-BYOMEI    TO  RECE031-IN-BYOMEI
           MOVE    RECE03-IN-BYOMEIMOJI
                                       TO  RECE031-IN-BYOMEIMOJI
           MOVE    RECE03-HOSOKU-COMMENT
                                       TO  RECE031-HOSOKU-COMMENT
      *
      *    ��̾���ܽ��б�
           IF      SYS-25-BYOMEI-PRTJYUN
                                       =   "0"  OR  "1"
               IF    ( SYS-25-BYOMEI-PRTJYUN
                                           =   "1"   )
               AND   ( RECE031-TENKIKBN    =   SPACE )
                   MOVE    "9"                 TO  RECE031-TENKIKBN-X
               ELSE
                   MOVE    RECE031-TENKIKBN    TO  RECE031-TENKIKBN-X
               END-IF 
               MOVE    RECE031-TENKIYMD    TO  RECE031-TENKIYMD-X
           END-IF
      *
           WRITE   RECE031-REC
           ADD     1                   TO  CNT-OUT
      *
           MOVE    KEY-NEW             TO  KEY-OLD   
           PERFORM             UNTIL   FLG-END       =   1
                               OR      KEY-NEW   NOT =   KEY-OLD
               PERFORM 900-RECE03-READ-SEC      
      *
      *        ��������������
               IF      CNT-IN-CANCEL   =   50
                   MOVE    ZERO            TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-PERFORM    
           .
       200-MAIN-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ���顼����λ����
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
      *    �ե����륨�顼����
      *****************************************************************
       500-FILE-ERR-ABORT-SEC           SECTION.
      *
           EVALUATE    WRK-ERR-FLG
               WHEN    1
                   MOVE    "�ե�����ϣУţΥ��顼��FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file open err" TO  WRK-ERR-FILE-MSG1
               WHEN    2
                   MOVE    "�ե������ɤ߹��ߥ��顼��FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file read err" TO  WRK-ERR-FILE-MSG1
               WHEN    3
                   MOVE    "�ե�����񤭹��ߥ��顼��FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file write err"
                                           TO  WRK-ERR-FILE-MSG1
               WHEN    4
                   MOVE    "�ե����빹�����顼��FILE="
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
           STRING  "ORCR0055 "             DELIMITED   BY  SIZE
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
      *    ���顼���Ͻ���
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
      *            ����ִ�����λ����
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
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
      *    ������߽���
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "��������ߤ���ޤ���"
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
      *                ����ִ�����λ����
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
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
      *    ��λ����
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE03-FILE
           CLOSE   RECE031-FILE
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ���ƥå״�����λ����
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE03  CNT-IN  :" CNT-IN
           DISPLAY "RECE031 CNT-OUT :" CNT-OUT
           DISPLAY "*** ORCR0055 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ե�����̾��������
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
      *    �쥻�ץ����ٽ񣰣��ɹ�
      *****************************************************************
       900-RECE03-READ-SEC             SECTION.
      *
           READ    RECE03-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
                   MOVE    HIGH-VALUE  TO  KEY-NEW
               NOT AT  END
                   IF      STS-RECE03          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERRMSG-AREA
                       INITIALIZE                      WRK-ERRMSG-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE03PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE03          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   ADD     1           TO  CNT-IN
                                           CNT-IN-CANCEL
                   MOVE    RECE03-HOSPNUM      TO  KEY-N-HOSPNUM
                   MOVE    RECE03-SRYYM        TO  KEY-N-SRYYM
                   MOVE    RECE03-NYUGAIKBN    TO  KEY-N-NYUGAIKBN
                   MOVE    RECE03-PTID         TO  KEY-N-PTID
                   MOVE    RECE03-RECEIPT-KBN  TO  KEY-N-RECEIPT-KBN
                   MOVE    RECE03-RECEKA       TO  KEY-N-RECEKA
                   MOVE    RECE03-TEISYUTUSAKI TO  KEY-N-TEISYUTUSAKI
                   MOVE    RECE03-RECESYUBETU  TO  KEY-N-RECESYUBETU
                   MOVE    RECE03-HKNJANUM     TO  KEY-N-HKNJANUM
                   MOVE    RECE03-HOJOKBN      TO  KEY-N-HOJOKBN
                   MOVE    RECE03-KOHID        TO  KEY-N-KOHID
                   MOVE    RECE03-TEKSTYMD     TO  KEY-N-TEKSTYMD
                   MOVE    RECE03-SRYYMD       TO  KEY-N-SRYYMD
                   MOVE    RECE03-TENKIKBN     TO  KEY-N-TENKIKBN
                   MOVE    RECE03-BYOMEI       TO  KEY-N-BYOMEI
                   IF      SYS-2005-TENKIHENKBN    =   ZERO
                       MOVE    SPACE               TO  KEY-N-TENKIYMD
                   ELSE
                       MOVE    RECE03-TENKIYMD     TO  KEY-N-TENKIYMD
                   END-IF    
      *
                   DISPLAY "RECE03  PTID=" RECE03-PTID
           END-READ
           .
       900-RECE03-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ�����ޥ����ɹ�
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ִ����ģ��������
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ӣţ̣ţãԽ���
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERRMSG-AREA
               INITIALIZE                      WRK-ERRMSG-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "�ģ��ɤ߹��ߥ��顼��TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCR0055 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ƣţԣãȽ���
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륯����������
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륢����������
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    �ģ¥����ץ����
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *