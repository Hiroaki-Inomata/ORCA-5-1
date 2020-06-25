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
       PROGRAM-ID.             ORCR1250.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �쥻�ץ�
      *  ����ݡ��ͥ��̾  : �����쥻�ץȡ�������
      *                      ����̾�Խ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  07/11/16    NACL��ƣ��    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  04.06.01    NACL-ƣ��    10/11/04  ��­�������б�
      *  04.06.02    NACL-ƣ��    12/07/04  ɽ�����֤ˤ��쥻�ץȵ����б�
      *
      *  04.08.01    NACL-ƣ��    14/07/07  ����ǥ��쥯�ȥ��б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE200-FILE    ASSIGN  RECE200PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE200-KEY
                                   FILE    STATUS  IS  STS-RECE200.
           SELECT  RECE210-FILE    ASSIGN  RECE210PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE210-KEY
                                   FILE    STATUS  IS  STS-RECE210.
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       FD  RECE200-FILE.
       01  RECE200-REC. 
           COPY    "CPRCF200.INC".
      *
       FD  RECE210-FILE.
       01  RECE210-REC. 
           COPY    "CPRCF110.INC"  REPLACING  //RECE110//
                                   BY         //RECE210//.
      *
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE200//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE210//.
      *
      *    ���顼�ե�����̾���ΰ�
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    �ե�����̾���ΰ�
           COPY    "CPRECEDAT1.INC".
           COPY    "CPTEMPFL.INC".
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-RECE200         PIC X(02).
           03  STS-RECE210         PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-RECE210         PIC 9(01).
      *
           03  FLG-BYOMEI-UMU      PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-IN              PIC 9(06).
           03  CNT-IN-CANCEL       PIC 9(06).
           03  CNT-OUT             PIC 9(06).
      *
           03  CNT-SYU-BYOMEI      PIC 9(01).
      *
       01  INDEX-AREA.
           03  IDX                 PIC 9(03). 
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM  PIC X(06).
               05  WRK-PARA-TERMID PIC X(16).
               05  WRK-PARA-SYSYMD PIC X(08).
               05  WRK-PARA-JOBID  PIC 9(07).
               05  WRK-PARA-SHELLID
                                   PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *
           03  WRK-YM              PIC 9(06).
           03  WRK-RENNUM          PIC 9(02).
      *
           03  WRK-RECEERR         PIC X(200). 
      *
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-N-SRYYMD    PIC 9(08).
           03  KEY-OLD.
               05  KEY-O-SRYYMD    PIC 9(08).
      *
       01  WRK-CONS-AREA.
      *    �絤�ϣ��������
           03  WRK-CONS-BYOMEI1    PIC X(12)   VALUE
                                                   "�������ɻٱ�".  
           03  WRK-CONS-BYOMEI2    PIC X(12)   VALUE
                                                   "���ɻ٤���©".  
           03  WRK-CONS-BYOMEI3    PIC X(10)   VALUE
                                                   "���ɻ���©".  
           03  WRK-CONS-BYOMEI4    PIC X(16)   VALUE
                                                   "����©�����ɻٱ�".  
           03  WRK-CONS-BYOMEI5    PIC X(14)   VALUE
                                                   "��©�����ɻٱ�".  
           03  WRK-CONS-BYOMEI6    PIC X(06)   VALUE
                                                   "�ٵ���".  
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ������̾
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *     
           COPY    "CPSK200501.INC".
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
      *    ��̾��Ω����
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *    Ⱦ�ѥ����å�����
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
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
                                       FLG-AREA
                                       STS-AREA
           MOVE    LOW-VALUE           TO  KEY-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *
           MOVE    "RECE700"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE200PARA
      *
           MOVE    "RECE710"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE210PARA
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        �ģ¥����ץ����
               PERFORM 100-DBOPEN-SEC
      *
      *        ���ƥå״������Ͻ���
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR1250"      TO  JOB-PGID
               MOVE    "����̾�Խ�"    TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        ��������������
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF    
      *
           OPEN    INPUT               RECE200-FILE   
      * 
           OPEN    OUTPUT              RECE210-FILE
           CLOSE                       RECE210-FILE
           OPEN    I-O                 RECE210-FILE
      *         
      *    �쥻�ץ��Խ���ʬ����
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "01"             TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-200501-REC
           ELSE    
               INITIALIZE                   SYS-200501-REC
           END-IF
           IF      SYS-200501-TENKIBYOMEI-KBN
                                    =   SPACE
               MOVE    ZERO             TO  SYS-200501-TENKIBYOMEI-KBN
           END-IF
      *
           DISPLAY "SYS-200501-TENKIBYOMEI-KBN="
                                    SYS-200501-TENKIBYOMEI-KBN
      *
           PERFORM 900-RECE200-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *    
      *    ������̾�ɤ߹��ߡʴ��ԣɣġ�����ǯ���������Ųʽ��
           INITIALIZE                      PTBYOMEI-REC
           MOVE    RECE200-HOSPNUM     TO  PTBYO-HOSPNUM
           MOVE    RECE200-PTID        TO  PTBYO-PTID
           MOVE    RECE200-HKNCOMBI    TO  PTBYO-HKNCOMBI
           MOVE    RECE200-SRYYM       TO  PTBYO-SRYYMD (1:6)
           MOVE    "31"                TO  PTBYO-SRYYMD (7:2)
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key32"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key32"             TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
               ADD     1               TO  CNT-IN-CANCEL
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM 2001-PTBYO-HENSYU-SEC
                           UNTIL       FLG-PTBYOMEI     =   1
                           OR          FLG-END          =   1
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key32"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-END             =   ZERO
               PERFORM 900-RECE200-READ-SEC      
      *
      *        ��������������
               IF      CNT-IN-CANCEL   =   50
                   MOVE    ZERO            TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF
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
           PERFORM             UNTIL       FLG-END         =   1
                               OR          FLG-PTBYOMEI    =   1
                               OR          KEY-NEW     NOT =   KEY-OLD
               PERFORM 2001-PTBYO-CHK-SEC
      * 
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key32"             TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
               ADD     1               TO  CNT-IN-CANCEL
      *
      *        ��������������
               IF      CNT-IN-CANCEL   =   50
                   MOVE    ZERO            TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-PERFORM    
           .
       2001-PTBYO-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    ������̾�����å�����
      *****************************************************************
       2001-PTBYO-CHK-SEC         SECTION.
      *
      *    ������ʬ
           IF      PTBYO-NYUGAIKBN      =   SPACE
               CONTINUE
           ELSE
               IF      PTBYO-NYUGAIKBN  NOT =   RECE200-NYUGAIKBN
                   GO  TO  2001-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
      *    ���ų�����
           IF      PTBYO-SRYYMD(1:6)    =   RECE200-SRYYM
               CONTINUE
           ELSE
               IF      PTBYO-SRYYMD(1:6)    >   RECE200-SRYYM 
                   GO  TO  2001-PTBYO-CHK-EXT
               ELSE
      *            ž����
                   IF      PTBYO-TENKIKBN       =   SPACE
                       CONTINUE
                   ELSE
                       IF    ( SYS-200501-TENKIBYOMEI-KBN
                                                   =   "0"  )
                       OR    ( PTBYO-REZEMM        =   ZERO )
                           IF      PTBYO-TENKIYMD(1:6) >=  RECE200-SRYYM
                               CONTINUE
                           ELSE
                               GO  TO  2001-PTBYO-CHK-EXT
                           END-IF
                       END-IF       
                   END-IF       
               END-IF
      *
      *        �쥻�ץ�ɽ������
               MOVE    ZERO                   TO  WRK-YM
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
                   IF    ( SYS-200501-TENKIBYOMEI-KBN
                                               =   "0"   )
                   OR    ( PTBYO-TENKIKBN      =   SPACE )
                       IF      WRK-YM              >=  RECE200-SRYYM
                           CONTINUE
                       ELSE
                           GO  TO  2001-PTBYO-CHK-EXT
                       END-IF
                   END-IF
               END-IF                    
      *
      *        �쥻�ץ�ɽ�����֤�ž������ͥ������ˤ��Ƚ��
               IF    ( PTBYO-TENKIKBN      =   SPACE )
               OR    ( PTBYO-REZEMM        =   ZERO  )
                   CONTINUE
               ELSE
                   IF      SYS-200501-TENKIBYOMEI-KBN
                                               =   "1"
                       IF      WRK-YM              >=  RECE200-SRYYM
                           CONTINUE
                       ELSE
                           GO  TO  2001-PTBYO-CHK-EXT
                       END-IF
                   END-IF    
               END-IF
           END-IF
      *                               
      *    �Խ�����
           PERFORM 20011-RECE210-WRITE-SEC
           .
       2001-PTBYO-CHK-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    �Խ�����
      *****************************************************************
       20011-RECE210-WRITE-SEC              SECTION.
      *
      *    ����̾�ե饰������
           IF      PTBYO-SYUBYOFLG     =   "1"
               CONTINUE
           ELSE
      *        �絤�ϣ�������¤ΤȤ��ϼ��¤Ȥ���
               MOVE    ZERO                TO  CNT-SYU-BYOMEI
               INSPECT PTBYO-BYOMEI    TALLYING  
                       CNT-SYU-BYOMEI  FOR ALL WRK-CONS-BYOMEI1,
                                           ALL WRK-CONS-BYOMEI2,
                                           ALL WRK-CONS-BYOMEI3,
                                           ALL WRK-CONS-BYOMEI4,
                                           ALL WRK-CONS-BYOMEI5,
                                           ALL WRK-CONS-BYOMEI6
               IF      CNT-SYU-BYOMEI      >   ZERO      
                   MOVE    "1"                 TO  PTBYO-SYUBYOFLG 
               ELSE
                   MOVE    "9"                 TO  PTBYO-SYUBYOFLG
               END-IF 
           END-IF
      *
      *    ž�����ȿ��ŷ�ˤ��ž��������
           IF      RECE200-SRYYM       <   PTBYO-TENKIYMD(1:6)
               MOVE    SPACE               TO  PTBYO-TENKIKBN
                                               PTBYO-TENKIYMD
           END-IF    
      *
           IF      PTBYO-BYOMEIHENFLG  =   "1"
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    PTBYO-BYOMEI        TO  KANACHK-MAE-INPUT
               MOVE    80                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT   TO  PTBYO-BYOMEI
               COMPUTE PTBYO-BYOMEIMOJI    =   KANACHK-MAX  /   2
           END-IF        
      *
      *    �����ե饰����­�����Ȥ����̾���Խ�
           IF    ( PTBYO-UTAGAIFLG NOT =   SPACE )
           OR    ( PTBYO-HOSOKU-COMMENT
                                   NOT =   SPACE )
               INITIALIZE                      ORCSMKBYOMEIAREA
               MOVE    PTBYO-UTAGAIFLG     TO  SMKBYO-UTAGAIFLG
               MOVE    PTBYO-BYOMEI        TO  SMKBYO-BYOMEI
               MOVE    PTBYO-HOSOKU-COMMENT
                                           TO  SMKBYO-HOSOKU-COMMENT
               MOVE    PTBYO-BYOMEIHENFLG  TO  SMKBYO-BYOMEIHENFLG
               MOVE    PTBYO-BYOMEICD-G    TO  SMKBYO-BYOMEICD-G
               CALL    "ORCSMKBYOMEI"      USING   ORCSMKBYOMEIAREA
      *
               COMPUTE PTBYO-BYOMEIMOJI    =   SMKBYO-LENGTH   /   2
               MOVE    SMKBYO-BYOMEI       TO  PTBYO-BYOMEI
               MOVE    SMKBYO-BYOMEIHENFLG TO  PTBYO-BYOMEIHENFLG
               MOVE    SMKBYO-BYOMEICD-G   TO  PTBYO-BYOMEICD-G
           END-IF
      *
           MOVE    ZERO                TO  FLG-BYOMEI-UMU
      *
           INITIALIZE                      RECE210-REC
           MOVE    RECE200-KEY         TO  RECE210-KEY1
           MOVE    PTBYO-SYUBYOFLG     TO  RECE210-SYUBYOFLG    
           MOVE    PTBYO-SRYYMD        TO  RECE210-SRYYMD
           MOVE    PTBYO-TENKIKBN      TO  RECE210-TENKIKBN
           MOVE    PTBYO-TENKIYMD      TO  RECE210-TENKIYMD
           START   RECE210-FILE
               KEY IS    >=   RECE210-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE210
               NOT INVALID
                   PERFORM 900-RECE210-READ-SEC
           END-START
      *
           PERFORM UNTIL  ( FLG-END        =   1                 )
                   OR     ( FLG-RECE210    =   1                 )
                   OR     ( FLG-BYOMEI-UMU =   1                 )
                   OR     ( RECE200-KEY
                                       NOT =   RECE210-KEY1      )
                   OR     ( PTBYO-SYUBYOFLG
                                       NOT =   RECE210-SYUBYOFLG )
                   OR     ( PTBYO-SRYYMD
                                       NOT =   RECE210-SRYYMD    )
                   OR     ( PTBYO-TENKIKBN
                                       NOT =   RECE210-TENKIKBN  )
                   OR     ( PTBYO-TENKIYMD
                                       NOT =   RECE210-TENKIYMD  )
               IF      PTBYO-BYOMEI    =   RECE210-BYOMEI
                   MOVE    1               TO  FLG-BYOMEI-UMU
               ELSE
                   PERFORM 900-RECE210-READ-SEC
               END-IF
           END-PERFORM
      *
           IF      FLG-BYOMEI-UMU      =   ZERO
               ADD     1                   TO  WRK-RENNUM
      *
               INITIALIZE                      RECE210-REC
               MOVE    RECE200-KEY         TO  RECE210-KEY1
               MOVE    PTBYO-SYUBYOFLG     TO  RECE210-SYUBYOFLG
               MOVE    PTBYO-SRYYMD        TO  RECE210-SRYYMD
               MOVE    PTBYO-TENKIKBN      TO  RECE210-TENKIKBN
               MOVE    PTBYO-TENKIYMD      TO  RECE210-TENKIYMD
               MOVE    WRK-RENNUM          TO  RECE210-RENNUM
               MOVE    PTBYO-BYOMEI        TO  RECE210-BYOMEI
               MOVE    PTBYO-BYOMEIMOJI    TO  RECE210-BYOMEIMOJI
      *
               WRITE   RECE210-REC
               ADD     1                   TO  CNT-OUT
           END-IF
           .
       20011-RECE210-WRITE-EXT.
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
           CLOSE   RECE200-FILE
           CLOSE   RECE210-FILE
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
           DISPLAY "RECE200  CNT-IN  :" CNT-IN
           DISPLAY "RECE210  CNT-OUT :" CNT-OUT  
           DISPLAY "*** ORCR1250 END ***"
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
           MOVE    SGETTEMP-TEMPDIR    TO  TEMPFLPARA
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  TEMPFLPARA (SGETTEMP-ST:)
           .
       800-FILENM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̾�ޥ������ңţ���
      *****************************************************************
       900-PTBYO-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
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
      *    ���ܾ���ե������ɹ�����
      *****************************************************************
       900-RECE200-READ-SEC             SECTION.
      *
           READ    RECE200-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
               NOT AT  END
                   ADD     1           TO  CNT-IN
                                           CNT-IN-CANCEL
      *
                   DISPLAY "RECE200  PTNUM=" RECE200-PTNUM
           END-READ
           .
       900-RECE200-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��̾����ե������ɹ�����
      *****************************************************************
       900-RECE210-READ-SEC           SECTION.
      *
           READ    RECE210-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-RECE210
               NOT AT  END
                   MOVE    ZERO        TO  FLG-RECE210
           END-READ
           .
       900-RECE210-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ�����ޥ����ɹ�
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    WRK-PARA-SRYYM  TO  SYS-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
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
       900-SYSKANRI-READ-EXT.
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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