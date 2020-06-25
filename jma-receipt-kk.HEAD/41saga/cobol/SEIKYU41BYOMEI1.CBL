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
       PROGRAM-ID.             SEIKYU41BYOMEI1.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �쥻�ץ�
      *  ����ݡ��ͥ��̾  : ���졦ʣ�̼��쥻�ץ��Ѵ�����̾�ե���������У�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  05/04/22    ͭ¼     ��   ��������(2.4.0��ORCR0050�򸵤˺���)
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      ***********************     ORCR0050��������     ****************
      *  01.00.01    MCC-ƣ��     01.06.29  �ݸ����ֹ桦
      *                                     Ŭ�ѳ���ǯ�������ɲ�
      *  01.00.02    MCC-ƣ��     01.10.26  �ݸ��ȹ礻��Ƚ��ν���
      *  01.00.03    MCC-ƣ��     01.12.13  �쥻�ץ�ɽ�����Ƚ��ν���
      *  01.00.04    MCC-ƣ��     02.02.08  ž������̤�����ΤȤ�ž����
      *                                     ɽ�������
      *  01.00.06    MCC-ƣ��     02.02.13  �ݸ��ȹ礻�򣳤��飱�����ѹ�
      *  01.00.07    MCC-ƣ��     02.05.13  ��̾�ε����ե饰������Խ�
      *  01.00.08    NACL-ƣ��    02.11.11  ž������̤�����ΤȤ���̾��ɽ��
      *                                     �򤹤�褦�˽���
      *  01.00.09    NACL-ƣ��    03.03.12  �����ʬ���ɲ�
      *                                     ���Ųʿ��������飲�����ѹ�
      *  01.00.10    NACL-ƣ��    03/07/02  ����̾�Խ��ե饰�ˤ���Խ����ɲ�
      *  01.00.11    NACL-ƣ��    03/09/03  �����ե饰����Υ����ɤ��Խ�
      *  01.00.12    NACL-ƣ��    04/02/01  ��̾�����ɣ������ѹ�
      ************************    SEIKYU41BYOMEI1��������    **********
      *  01.00.01    ͭ¼         05/05/06  DB������ˡ����(MCPSUB����)
      *  01.00.02    ����         07/09/30  ���쥻ver4.0�б�
      *  01.00.03    ���         08/11/20  SPA-AREA�����
      *  01.00.04    ����         11/06/16  �쥻�ץ���̾���ܽ��б�
      *                                     ��­�������б�
      *  01.00.05    ����         14/10/27  ver4.8.0�б��ʰ���ǥ��쥯�ȥ��ѹ���
      *  01.00.06    ����         19/02/07  �������򥳥��Ⱥ��
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *
           SELECT  RECE03-FILE     ASSIGN  RECE03PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE03-KEY
                                   FILE    STATUS  IS  STS-RECE03.
      *    ���顼�ե�����
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    �쥻�ץ����ٽ�  
      *
       FD  RECE03-FILE.
       01  RECE03-REC. 
           COPY    "CPRCF003.INC".
      *    ���顼�ե�����
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     �쥻�ץ����٥ե����� ̾���ΰ� 
           COPY    "CPCOMMONDAT2.INC".
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE03//.
      *
      *    ���顼�ե����� ̾���ΰ� 
           COPY    "CPRECEERR.INC".
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-RECE03          PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-IN              PIC 9(06).
           03  CNT-OUT             PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                 PIC 9(03). 
           03  IDY                 PIC 9(03). 
           03  IDZ                 PIC 9(03).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM   PIC X(06).
               05  WRK-PARA-TERMID  PIC X(16).
               05  WRK-PARA-SYSYMD  PIC X(08).
               05  WRK-PARA-JOBID   PIC 9(07).
               05  WRK-PARA-SHELLID PIC X(08).
               05  WRK-PARA-HOSPNUM PIC 9(02).
           03  WRK-PATH             PIC X(64). 
           03  WRK-YM               PIC 9(06).
           03  WRK-RENNUM           PIC 9(02).
      *
           03  WRK-RECEERR         PIC X(200). 
      *    ����̾�Խ�
           03  WRK-BYOMEI          PIC X(240).
           03  WRK-BYOMEIW         PIC X(240).
      *    ����̾�Խ��ե饰�ʵ�����
           03  WRK-HENFLG1         PIC X(01).
      *    ����̾�Խ��ե饰�ʵ�����
           03  WRK-HENFLG2         PIC X(01). 
      *    ��̾ʸ����         
           03  WRK-MOJISU          PIC 9(03).
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-N-SRYYMD    PIC 9(08).
           03  KEY-OLD.
               05  KEY-O-SRYYMD    PIC 9(08).
      *
      *    �����Խ�
       01  WRK-UTAGAI-AREA.
      *    ����
           03  WRK-CONS-UTAGAI     PIC X(06)   VALUE   "�ε���".
           03  WRK-CONS-UTAGAI-MOJISU
                                   PIC 9(02)   VALUE   6.       
      *    ����
           03  WRK-CONS-KYUSEI     PIC X(04)   VALUE   "����".      
           03  WRK-CONS-KYUSEI-MOJISU
                                   PIC 9(02)   VALUE   4. 
      *
       01  WRK-CONS-AREA.
      *    ����
           03  WRK-CONS-BYOMEICD-UTAGAI
                                   PIC X(07)   VALUE   "ZZZ8002".   
      *    ����
           03  WRK-CONS-BYOMEICD-KYUSEI
                                   PIC X(07)   VALUE   "ZZZ4012".   
      *    DB������
           COPY    "COMMON-SPA".
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
           COPY    "CPSK200501.INC".
      *
      *    ������̾
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    ���������
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    ���ԥޥ���
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    �����Ѵ��ΰ�
           COPY    "CPORCSNUM.INC".
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    Ⱦ�ѥ����å�����
           COPY    "CPORCSKANACHK.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  WRK-PARA-LINK.
           03  WRK-PARA-LINK-SRYYM    PIC  X(06).
           03  WRK-PARA-LINK-TERMID   PIC  X(16).
           03  WRK-PARA-LINK-SYSYMD   PIC  X(08).
           03  WRK-PARA-LINK-JOBID    PIC  9(07).
           03  WRK-PARA-LINK-SHELLID  PIC  X(08).
           03  WRK-PARA-LINK-HOSPNUM  PIC  9(02).
      *
      ****************************************************************
       PROCEDURE           DIVISION
               USING    WRK-PARA-LINK.
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
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       SPA-AREA
                                       FLG-AREA
           INITIALIZE                  RECEERR
      *
           MOVE    LOW-VALUE           TO  KEY-AREA
           MOVE    WRK-PARA-LINK       TO  WRK-PARA
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ���ƥå״������Ͻ���
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "SEIKYU41BYOMEI1"   TO  JOB-PGID
           MOVE    "����̾�Խ�1"       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *
      *    ���֥ץ�����������ե�����̾�Խ�
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "RECE030"           TO  RECE03PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE03PARA-TERMID
           MOVE  WRK-PARA-HOSPNUM      TO  RECE03PARA-HOSPNUM
      *
           MOVE  RECE03PARA-BASENAME     TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (2)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  RECE03PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEERR
      *
           OPEN    OUTPUT              RECE03-FILE
           CLOSE                       RECE03-FILE
           OPEN    I-O                 RECE03-FILE
      *     
      *    �쥻�ץ��Խ���ʬ����
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "*"              TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM(1:6)
                                    TO  SYS-STYUKYMD(1:6)
                                        SYS-EDYUKYMD(1:6)
           MOVE    "01"             TO  SYS-STYUKYMD(7:2)
                                        SYS-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM        TO  SYS-HOSPNUM
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-2005-REC
                
           ELSE    
               MOVE    SPACE            TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-BYOMEIHENKBN1   =   SPACE
               MOVE    ZERO             TO  SYS-2005-BYOMEIHENKBN1
           END-IF
           IF      SYS-2005-SYUBYOHENFLG    =   SPACE
               MOVE    ZERO             TO  SYS-2005-SYUBYOHENFLG
           END-IF
      *
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "01"             TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM(1:6)
                                    TO  SYS-STYUKYMD(1:6)
                                        SYS-EDYUKYMD(1:6)
           MOVE    "01"             TO  SYS-STYUKYMD(7:2)
                                        SYS-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM        TO  SYS-HOSPNUM
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-200501-REC
           ELSE    
               INITIALIZE                   SYS-200501-REC
           END-IF
           IF      SYS-200501-CHKKBN
                                    =   SPACE
               MOVE    ZERO             TO  SYS-200501-CHKKBN
           END-IF
           IF      SYS-200501-BYOMEI-PRTJYUN
                                    =   SPACE
               MOVE    ZERO             TO  SYS-200501-BYOMEI-PRTJYUN
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *    
           PERFORM 900-KOHSKY-START-SEC
      *
           PERFORM  UNTIL  FLG-END   =   1
      *    ������̾�ɤ߹��ߡʴ��ԣɣġ�����ǯ���������Ųʽ��
             INITIALIZE                  PTBYOMEI-REC
             MOVE    RECE02-HOSPNUM      TO  PTBYO-HOSPNUM
             MOVE    RECE02-PTID         TO  PTBYO-PTID
             MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
             IF      SYS-200501-BYOMEI-PRTJYUN   =   "2"
                 MOVE    "key39"         TO  WRK-MCP-PATHNAME
             ELSE
                 MOVE    "key3"          TO  WRK-MCP-PATHNAME
             END-IF
             MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
                 MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                 PERFORM 900-PTBYO-READ-SEC
             ELSE
                 INITIALIZE                  PTBYOMEI-REC
                 MOVE    1                   TO  FLG-PTBYOMEI
             END-IF
      *
             PERFORM 2001-PTBYO-HENSYU-SEC
                             UNTIL       FLG-PTBYOMEI     =   1
      *    �������륯������ 
             INITIALIZE                  MCPAREA
             MOVE    "DBCLOSECURSOR"        TO  MCP-FUNC
             MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
             MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
             PERFORM  900-KOHSKY-READ-SEC
           END-PERFORM
      *
           CLOSE   RECE03-FILE
      *
           .
       200-MAIN-EXT.
           EXIT. 
      *            
      * 
      *****************************************************************
      *    ������̾����ǯ�����̽���
      *****************************************************************
       2001-PTBYO-HENSYU-SEC                SECTION.
      *
           MOVE    PTBYO-SRYYMD            TO  KEY-N-SRYYMD
           MOVE    ZERO                    TO  WRK-RENNUM
      *
           MOVE    KEY-NEW                 TO  KEY-OLD   
           DISPLAY "������̾�ե饰=" FLG-PTBYOMEI
                   "�����ֹ�=" RECE02-PTNUM
           PERFORM             UNTIL       FLG-PTBYOMEI    =   1
                               OR          KEY-NEW     NOT =   KEY-OLD
               DISPLAY "��̾�����å�����"
               PERFORM 2001-PTBYO-CHK-SEC
      * 
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM    
           .
       2001-PTBYO-HENSYU-EXT.
           EXIT. 
      *            
070100*****************************************************************
070200*    ������̾�����å�����
070300*****************************************************************
070400 2001-PTBYO-CHK-SEC         SECTION.
      *
      *    ���Ų�
           MOVE    ZERO                TO  FLG-CHK 
           IF      SYS-2005-BYOMEIHENKBN1      =   ZERO     
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       20
                       OR      RECE02-SRYKA(IDX)   =   SPACE
                   IF      PTBYO-SRYKA          =   RECE02-SRYKA (IDX)
                       MOVE    1                    TO  FLG-CHK
                       MOVE    20                   TO  IDX
                   END-IF
               END-PERFORM
               IF      FLG-CHK              =   ZERO
                    DISPLAY "���Ų� GO TO" 
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
           END-IF        
      *
      *    ���ų�����
           IF      PTBYO-SRYYMD(1:6)    =   RECE02-SRYYM
               CONTINUE
           ELSE
               IF      PTBYO-SRYYMD(1:6)    <   RECE02-SRYYM 
                   IF      PTBYO-TENKIKBN       =   SPACE
                       CONTINUE
                   ELSE
                       IF      PTBYO-TENKIYMD(1:6)  >=  RECE02-SRYYM
                           CONTINUE
                       ELSE
                            DISPLAY "���ų�����1 GO TO" 
                           GO  TO  2001-PTBYO-CHK-EXT
                       END-IF
                   END-IF       
               ELSE
                   DISPLAY "���ų�����2 GO TO" 
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
      *    ������ʬ
           IF      PTBYO-NYUGAIKBN      =   SPACE
               CONTINUE
           ELSE
               IF      PTBYO-NYUGAIKBN  NOT =   RECE02-NYUGAIKBN
                   DISPLAY "������ʬ GO TO" 
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
      *    �ݸ��ȹ礻
           IF      PTBYO-HKNCOMBI     =   ZERO
               CONTINUE
           ELSE 
               MOVE    ZERO            TO  IDZ  
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       10
                       OR      RECE02-HKNCOMBI (IDX)   =   ZERO 
                   IF      PTBYO-HKNCOMBI   =
                                            RECE02-HKNCOMBI (IDX)
                       MOVE    IDX             TO  IDZ
                       MOVE    10              TO  IDX
                   END-IF
               END-PERFORM
               IF      IDZ            >   ZERO          
                   CONTINUE
               ELSE
                   DISPLAY "�ݸ��ȹ礻 GO TO" 
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
      *    �쥻�ץ�ɽ����
           DISPLAY "�쥻�ץ�ɽ����=" PTBYO-REZEFLG
           IF      PTBYO-REZEFLG          =   "1"
              GO  TO  2001-PTBYO-CHK-EXT
           END-IF
      *
           IF      PTBYO-REZEMM           >   ZERO
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    PTBYO-SRYYMD        TO  LNK-DAY6-KIJUN 
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               COMPUTE LNK-DAY6-ZOGEN  =   PTBYO-REZEMM    -   1    
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN(1:6)
                                           TO  WRK-YM
               IF      WRK-YM              >=  RECE02-SRYYM
                   CONTINUE
               ELSE
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
           END-IF                    
      *                               
           PERFORM 20011-RECE03-WRITE-SEC
           .
       2001-PTBYO-CHK-EXT.
           EXIT. 
      *            
070100*****************************************************************
070200*    ;�쥻�ץ����٣����Խ�����
070300*****************************************************************
070400 20011-RECE03-WRITE-SEC              SECTION.
      *
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI 
           MOVE    PTBYO-HOSOKU-COMMENT
                                       TO  KANACHK-MAE-INPUT
           MOVE    40                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  PTBYO-HOSOKU-COMMENT
      *
      *    �����ե饰�����̾���Խ�
           MOVE    SPACE               TO  WRK-HENFLG1
                                           WRK-HENFLG2
      *       
           IF      PTBYO-UTAGAIFLG NOT =   SPACE
               COMPUTE WRK-MOJISU   =   PTBYO-BYOMEIMOJI   *   2
               IF      PTBYO-UTAGAIFLG  =   "1" OR  "3"
                   IF      WRK-MOJISU       <   8
                       MOVE    1                TO  IDZ
                   ELSE    
                       COMPUTE IDZ     =   WRK-MOJISU    -   7
                   END-IF    
                   PERFORM VARYING IDX FROM    IDZ BY  2
                           UNTIL   IDX >       WRK-MOJISU
                       IF      PTBYO-BYOMEI (IDX:2)      =   "��"
                           MOVE    "1"              TO  WRK-HENFLG1
                           MOVE    WRK-MOJISU       TO  IDX
                       END-IF
                   END-PERFORM
               END-IF                
               IF      PTBYO-UTAGAIFLG =   "2" OR  "3"
                   PERFORM VARYING IDX FROM    1   BY  2
                           UNTIL   IDX >       WRK-MOJISU
                       IF      PTBYO-BYOMEI (IDX:4)      =   "����"
                           MOVE    "1"              TO  WRK-HENFLG2
                           MOVE    WRK-MOJISU       TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *     
      *    ��̾ʸ������׻�
           COMPUTE WRK-MOJISU          =   PTBYO-BYOMEIMOJI   *   2
           MOVE    PTBYO-BYOMEI      TO  WRK-BYOMEI
      *
      *    �����ե饰�����åȤ���Ƥ���Ȥ�
           IF    ( PTBYO-UTAGAIFLG   =   "1" OR  "3" )
           AND   ( WRK-HENFLG1         =   SPACE     )                         
               MOVE    SPACE               TO  WRK-BYOMEI
               STRING  PTBYO-BYOMEI        DELIMITED BY  SPACE
                       WRK-CONS-UTAGAI     DELIMITED BY  SIZE
                       INTO                WRK-BYOMEI
               END-STRING
               COMPUTE WRK-MOJISU          =      
                                WRK-MOJISU +   WRK-CONS-UTAGAI-MOJISU
               IF      PTBYO-BYOMEICD (21)  NOT =   SPACE             
                   MOVE    "1"                 TO  PTBYO-BYOMEIHENFLG
               ELSE
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       21
                       IF      PTBYO-BYOMEICD (IDX)    =   SPACE
                           MOVE    WRK-CONS-BYOMEICD-UTAGAI
                                               TO  PTBYO-BYOMEICD (IDX)
                           MOVE    21          TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-IF    
           IF    ( PTBYO-UTAGAIFLG   =   "2" OR  "3" )
           AND   ( WRK-HENFLG2         =   SPACE     )                         
               MOVE    SPACE               TO  WRK-BYOMEIW
               STRING  WRK-CONS-KYUSEI     DELIMITED BY  SIZE
                       WRK-BYOMEI          DELIMITED BY  SPACE
                       INTO                WRK-BYOMEIW
               END-STRING
               COMPUTE WRK-MOJISU          =      
                                WRK-MOJISU +   WRK-CONS-KYUSEI-MOJISU
               MOVE    WRK-BYOMEIW         TO  WRK-BYOMEI  
               IF      PTBYO-BYOMEICD (21) NOT =   SPACE             
                   MOVE    "1"                 TO  PTBYO-BYOMEIHENFLG
               ELSE
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       21
                       IF      PTBYO-BYOMEICD (IDX)    =   SPACE
                           MOVE    WRK-CONS-BYOMEICD-KYUSEI
                                               TO  PTBYO-BYOMEICD (IDX)
                           MOVE    21          TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           IF    PTBYO-HOSOKU-COMMENT  NOT =   SPACE
               MOVE    SPACE               TO  WRK-BYOMEIW
               STRING  WRK-BYOMEI          DELIMITED BY SPACE
                       "��"                DELIMITED BY SIZE
                       PTBYO-HOSOKU-COMMENT
                                           DELIMITED BY SPACE
                       "��"                DELIMITED BY SIZE
               INTO    WRK-BYOMEIW
               END-STRING
               MOVE    WRK-BYOMEIW         TO  WRK-BYOMEI
           END-IF
      *
           ADD     1                   TO  WRK-RENNUM
           
           INITIALIZE                      RECE03-REC
           MOVE    SPA-HOSPNUM         TO  RECE03-HOSPNUM
           MOVE    RECE02-SRYYM        TO  RECE03-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE03-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE03-PTID
           MOVE    RECE02-RECEKA       TO  RECE03-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE03-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE03-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE03-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE03-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE03-TEKSTYMD
           MOVE    PTBYO-SRYYMD        TO  RECE03-SRYYMD
           MOVE    WRK-RENNUM          TO  RECE03-RENNUM
           MOVE    RECE02-PTNUM        TO  RECE03-PTNUM
           MOVE    PTBYO-SRYKA         TO  RECE03-SRYKA
           MOVE    PTBYO-KHNBYOMEICD   TO  RECE03-KHNBYOMEICD   
           MOVE    PTBYO-BYOMEICD-G    TO  RECE03-BYOMEICD-G
           MOVE    PTBYO-UTAGAIFLG     TO  RECE03-UTAGAIFLG
           IF      SYS-2005-SYUBYOHENFLG    =   ZERO
               MOVE    "9"                 TO  RECE03-SYUBYOFLG 
           ELSE
               IF      PTBYO-SYUBYOFLG     =   "1"
                   MOVE    PTBYO-SYUBYOFLG     TO  RECE03-SYUBYOFLG    
               ELSE
                   MOVE    "9"                 TO  RECE03-SYUBYOFLG 
               END-IF
           END-IF
           MOVE    PTBYO-MANSEIKBN     TO  RECE03-MANSEIKBN
           IF      RECE02-SRYYM        <   PTBYO-TENKIYMD(1:6)
               CONTINUE
           ELSE    
               MOVE    PTBYO-TENKIKBN      TO  RECE03-TENKIKBN
               MOVE    PTBYO-TENKIYMD      TO  RECE03-TENKIYMD   
           END-IF    
           MOVE    WRK-BYOMEI          TO  RECE03-BYOMEI
           COMPUTE RECE03-BYOMEIMOJI    =   WRK-MOJISU    /   2
           MOVE    PTBYO-BYOMEIHENFLG  TO  RECE03-BYOMEIHENFLG   
           MOVE    PTBYO-HOSOKU-COMMENT
                                       TO  RECE03-HOSOKU-COMMENT
      *
           DISPLAY "������̾�ե�����WRITE �����ֹ�=" RECE02-PTNUM
           WRITE   RECE03-REC
           ADD     1                   TO  CNT-OUT   
076800     .
076900 20011-RECE03-WRITE-EXT.
077000     EXIT.
057400*
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
      *        ����ִ������Ͻ���
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
057400*
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̾�ޥ������ңţ���
      *****************************************************************
       900-PTBYO-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTBYOMEI
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC 
           ELSE
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
           .
       900-PTBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ�����ޥ����ɹ�
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      * 
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
             MOVE  "tbl_syskanri"          TO  MCP-TABLE
             MOVE  "key10"                 TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE  "tbl_syskanri"          TO  MCP-TABLE
           MOVE  "key10"                 TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"             USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ���������ޥ����ɹ�
      *****************************************************************
       900-KOHSKY-START-SEC            SECTION.
      *
           INITIALIZE                  RECE02-REC
           MOVE    SPA-HOSPNUM         TO  RECE02-HOSPNUM
           MOVE    WRK-PARA-SRYYM      TO  RECE02-SKYYM
           MOVE    RECE02-REC          TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 900-KOHSKY-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
               PERFORM  900-KOHSKY-CLOSE-SEC
           END-IF
           .
       900-KOHSKY-START-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ���������ޥ����ңţ���  ʣ�̼��쥻���оݤȤʤ��������ꤹ��
      *****************************************************************
       900-KOHSKY-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE02-REC
               IF  RECE02-KOHNUM(1)   =  "181"
               OR  RECE02-KOHNUM(2)   =  "181"
               OR  RECE02-KOHNUM(3)   =  "181"
               OR  RECE02-KOHNUM(4)   =  "181"
      *    �ƥ��ȴ��ԤΤȤ����ɤ����Ф�
                   MOVE    SPACE               TO  PTINF-REC
                   INITIALIZE                      PTINF-REC
                   MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                   MOVE    RECE02-PTID         TO  PTINF-PTID
                   MOVE    PTINF-REC           TO  MCPDATA-REC
                   PERFORM 800-PTINF-READ-SEC
                   IF      FLG-PTINF           =   ZERO
                   AND     PTINF-TSTPTNUMKBN   =   "1"
                       GO  TO  900-KOHSKY-READ-SEC
                   END-IF
               ELSE
                 GO  TO  900-KOHSKY-READ-SEC
               END-IF
           ELSE
               MOVE    1               TO  FLG-END
               PERFORM  900-KOHSKY-CLOSE-SEC
           END-IF
           .
       900-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������ޥ���CLOSE 05/05/06 INS
      *****************************************************************
       900-KOHSKY-CLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_kohsky"          TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       900-KOHSKY-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ԥޥ����ɹ�
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_ptinf"            TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           .
       800-PTINF-READ-EXT.
           EXIT.