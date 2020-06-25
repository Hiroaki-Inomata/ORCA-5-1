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
       PROGRAM-ID.             ORCR0930.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �쥻�ץ�������ϫ�ҡ������ա�
      *  ����ݡ��ͥ��̾  : ���̳��ϫ�Ҽ��������ٽ�ե��������
      *                      ���Ŧ�����Խ���ϫ�ҡ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  03/06/01    NACL-ƣ��      ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-����    04.04.26  �桼�����������ɲ��б�
      *  ��߽����ɲ�
      *  01.01.01    NACL-ƣ��    04/10/13  ��߽������ɲ�
      *  01.01.02    NACL-����    05/07/15  �����ս����ͼ��б�
      *
      *  03.05.01    NACL-����    07/06/01  ���롼�׿����б�
      *  03.05.02    NACL-ƣ��    07/06/01  realtime preview �����ɲ�
      *  03.05.03    NACL-����    07/06/26  ���׹��ܺ��
      *  04.01.01    NACL-����    07/11/01  ��̳�ҳ��б�
      *  04.01.02    NACL-����    07/11/26  �����ྦ��̾�����б�
      *  04.03.01    NACL-����    09/01/23  ��Ŧ�����Խ�����
      *
      *  04.05.01    NACL-����    09/06/18  ��Ŧ���󥳥����Խ��б�
      *  04.05.02    NACL-ƣ��    09/07/14  �������ʲ������б�
      *  04.05.03    NACL-����    13/02/07  ��۹��ܣ����б�
      *
      *  04.08.01    NACL-ƣ��    14/07/07  ����ǥ��쥯�ȥ��б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE82-FILE     ASSIGN  RECE01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE82-KEY
                                   FILE    STATUS  IS  STS-RECE82.
      *   
           SELECT  RECE86-FILE     ASSIGN  RECE86PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE86-KEY
                                   FILE    STATUS  IS  STS-RECE86.
      *   
           SELECT  RECE87-FILE     ASSIGN  RECE87PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE87-KEY
                                   FILE    STATUS  IS  STS-RECE87.
      *    
           SELECT  RECE87X-FILE     ASSIGN  RECE87PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE87X-KEY
                                   FILE    STATUS  IS  STS-RECE87.
      *    ���顼�ե�����
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       FD  RECE82-FILE.
       01  RECE82-REC.
           COPY    "CPRCF082.INC".
      *
       FD  RECE86-FILE.
       01  RECE86-REC.
           COPY    "CPRCF0861.INC" REPLACING  //RECE861//
                                   BY         //RECE86//.
      *
      *
       FD  RECE87-FILE.
       01  RECE87-REC. 
           COPY    "CPRCF0871.INC"  REPLACING //RECE871//
                                   BY         //RECE87//.
      *
       FD  RECE87X-FILE.
       01  RECE87X-REC. 
           COPY    "CPRCF0871.INC"  REPLACING //RECE871//
                                   BY         //RECE87X//.
      *    ���顼�ե�����
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE01//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE86//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE87//.
      *    ���顼�ե����� ̾���ΰ� 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  SPA-STS             PIC 9(04).
           03  STS-RECE82          PIC X(02).
           03  STS-RECE87          PIC X(02).
           03  STS-RECE86          PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE82          PIC 9(01).
           03  FLG-RECE86          PIC 9(01).
           03  FLG-RECE87          PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-NOPRT           PIC 9(01).
           03  FLG-HENSYU          PIC 9(01).
           03  FLG-HENSYU2         PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-RECE82          PIC 9(06).
           03  CNT-RECE87          PIC 9(06).
           03  CNT-RECE86          PIC 9(06).
           03  CNT-IN-CANCEL       PIC 9(06).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *    ��������Խ���     
           03  IDA                 PIC 9(04).
           03  IDB                 PIC 9(04).
      *    �޿��Ź԰٥��������Խ���     
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM              PIC  X(06).
               05  WRK-PARA-TERMID             PIC  X(16).
               05  WRK-PARA-SYSYMD             PIC  X(08).
               05  WRK-PARA-RECEKBN            PIC  X(01).
               05  WRK-PARA-JOBID              PIC  9(07).
               05  WRK-PARA-SHELLID            PIC  X(08).
           03  WRK-PARA-JIBAIPRTKBN            PIC  X(01).
           03  WRK-PARA-HOSPNUM                PIC  9(02).
      *
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-LINENUM         PIC 9(05).
      *    �ް����Կ�
           03  WRK-TBLMAX          PIC 9(03).
      *
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-KINGAKU         PIC 9(09)V9(02).
      *
      *    ����Ⱦ�������Ѵ���     
           03  WRK-NUM                         PIC 9(01).
           03  WRK-CHAR-IDX                    PIC 9(02).
           03  WRK-CHAR-HENKAN-AREA.
               05  WRK-CHAR-X                  PIC X(10).
               05  WRK-CHAR-N                  PIC X(20).
               05  WRK-CHAR-CNT                PIC 9(02).
               05  WRK-CHAR-FLG                PIC 9(01). 
               05  WRK-CHAR-N-CNT              PIC 9(02).
      *
           03  WRK-EDIT-AREA.
               05  WRK-CLM                     PIC 9(03).
               05  WRK-WCLM                    PIC 9(03).
      *
           03  WRK-KOUI                        PIC X(200).
      *
           03  WRK-ZAI.
               05  WRK-ZAI-NAME                PIC X(52).
      *
           03  WRK-ZAI1.
               05  WRK-ZAI1-NAME               PIC X(28).
      *
      *    ����������Խ���
           03  WRK-ZAITENX                     PIC ZZZZZZZZ9.99.
           03  WRK-ZAIKAISUX                   PIC ZZZ9.
           03  WRK-TNSKAI-AREA.
               05  WRK-TNSKAI-ST               PIC 9(03).
               05  WRK-TNSKAI-ST1              PIC 9(03).
               05  WRK-TNSKAI-KETA             PIC 9(02).
               05  WRK-TNSKAI-LEN              PIC 9(02).
      *
               05  WRK-TNSKAI.
                   07  WRK-ZAITEN              PIC X(14).
                   07  WRK-X                   PIC X(02).
                   07  WRK-ZAIKAISU            PIC X(06).
      *             
           03  WRK-ST                          PIC 9(03).
      *     
           03  WRK-WLINETEK                    PIC 9(03).
           03  WRK-SRYSURYOW                   PIC X(24).
           03  WRK-ZAIIDX                      PIC 9(03).
           03  WRK-NAMEKETA                    PIC 9(03).
           03  WRK-YOHAKU                      PIC 9(03).
           03  WRK-WKETA                       PIC 9(03).
      *     
           03  WRK-SRYKBN                      PIC X(02).
           03  WRK-Z9                          PIC ZZZ9.
      *
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *          
      *    ���Ź԰��̥ơ��֥�
       01  TBL-ZAIW-AREA.
           03  TBL-ZAIW-REC.
               05  TBL-ZAIW-SRYACTINF.
      *            ���ť�����/����/���
                   07  TBL-ZAIW-SRYCD           PIC  X(09).
                   07  TBL-ZAIW-SRYSURYO        PIC  9(05)V9(05).
                   07  TBL-ZAIW-SRYKAISU        PIC  9(03).
      *            ñ�̥�����/ñ��̾��ͭ�����/ñ��̾��
                   07  TBL-ZAIW-TANICD          PIC  9(03).
                   07  TBL-ZAIW-TANIMOJISU      PIC  9(01).
                   07  TBL-ZAIW-TANINAME        PIC  X(24).
      *            �������̶�ʬ/����/��������ե饰
                   07  TBL-ZAIW-TENSIKIBETU     PIC  9(01).
                   07  TBL-ZAIW-TEN             PIC  9(09)V9(02).
                   07  TBL-ZAIW-MEISKYFLG       PIC  X(01).
      *            ��ư�����ʬ/̾�������ֹ�/�����󽸷��輱��
                   07  TBL-ZAIW-AUTOKBN         PIC  X(01).
                   07  TBL-ZAIW-INPUTNUM        PIC  9(03).
                   07  TBL-ZAIW-TENTTLKBN       PIC  9(03).
               05  TBL-ZAIW-NAMEINF.
      *            ����ͭ�����/����̾��
                   07  TBL-ZAIW-YUKOKETA        PIC  9(03).
                   07  TBL-ZAIW-NAME            PIC  X(200).
      *
      *    ������
       01  CONST-AREA.
      *         
      *    Ŭ�������ʸ�����ʶ�����
           03  CONS-TEKIYOU1-LEN           PIC 9(02)   VALUE   28.
      *     
      *    Ŭ�������ʸ�������������
           03  CONS-TEKIYOU2-LEN           PIC 9(02)   VALUE   52.
      *
      *    Ŭ�������Կ��ʶ�����
           03  CONS-TEKIYOU1-LINE-MAX      PIC  9(03)  VALUE   7.
      *
      *
           COPY    "MCPAREA".
      *
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
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *     
      *    �����Ѵ�����
           COPY    "CPORCSNUM.INC".
      *
      *    Ⱦ�ѥ����å�����
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ģ¸���
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
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       SPA-STS     =   2
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
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  SPA-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  RECEERR
      *
      *    �ե�����ϣУţν���
      *
           PERFORM 110-FILE-OPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 120-DBOPEN-SEC
      *
      *        ���ƥå״������Ͻ���
               MOVE    "STS"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    "ORCR0930"          TO  JOB-PGID
               MOVE    "���Ŧ�����Խ���ϫ�ҡ�"
                                           TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        ��������������
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF 
      *
           IF      SPA-STS         =   ZERO
               PERFORM 990-RECE82-READ-SEC
           END-IF 
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ե�����ϣУţν���
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE01PARA
           INITIALIZE                      RECE01PARA
           MOVE    "RECE820"           TO  RECE01PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE01PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE01PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE86PARA
           INITIALIZE                      RECE86PARA
           MOVE    "RECE861"           TO  RECE86PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE86PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE86PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE87PARA
           INITIALIZE                      RECE87PARA
           MOVE    "RECE871"           TO  RECE87PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE87PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE87PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE01PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE86PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           MOVE    RECE87PARA-BASENAME TO  SGETTEMP-BASENAMES  (4)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE01PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE86PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE87PARA
      *
           OPEN    INPUT               RECE82-FILE
      *
           OPEN    INPUT               RECE86-FILE
      *
           OPEN    OUTPUT              RECE87X-FILE
           CLOSE                       RECE87X-FILE
      *
           OPEN    I-O                 RECE87-FILE
      *
           .
       110-FILE-OPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    �ԥ����󥿽����
           MOVE    ZERO                TO  WRK-LINENUM
           MOVE    ZERO                TO  WRK-TBLMAX
      *
           INITIALIZE                      TBL-ZAIW-AREA
      *
           MOVE        ZERO            TO  WRK-ZAINUM
                                           WRK-KINGAKU
           MOVE        ZERO            TO  FLG-HENSYU2
      *
      *    �ѥ�᡼�������ե��������Խ�
           INITIALIZE                      RECE86-REC
           MOVE    RECE82-HOSPNUM      TO  RECE86-HOSPNUM
           MOVE    RECE82-SRYYM        TO  RECE86-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE86-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE86-PTID
           MOVE    RECE82-RECEKA       TO  RECE86-RECEKA
           MOVE    RECE82-HKNID        TO  RECE86-HKNID
           MOVE    RECE82-TEKSTYMD     TO  RECE86-TEKSTYMD
      *
           START   RECE86-FILE
               KEY IS    >=   RECE86-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE86
               NOT INVALID
                   PERFORM 990-RECE86-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( SPA-STS              =   2                )
               OR     ( FLG-RECE86           =   1                )
               OR     ( RECE82-HOSPNUM   NOT =   RECE86-HOSPNUM   )
               OR     ( RECE82-SRYYM     NOT =   RECE86-SRYYM     )
               OR     ( RECE82-NYUGAIKBN NOT =   RECE86-NYUGAIKBN )
               OR     ( RECE82-PTID      NOT =   RECE86-PTID      )
               OR     ( RECE82-RECEKA    NOT =   RECE86-RECEKA    )
               OR     ( RECE82-HKNID     NOT =   RECE86-HKNID     )
               OR     ( RECE82-TEKSTYMD  NOT =   RECE86-TEKSTYMD  )
      *
                   IF      RECE86-ZAINUM   NOT =   WRK-ZAINUM
                      MOVE    RECE86-ZAINUM  TO    WRK-ZAINUM
                      MOVE    ZERO           TO    WRK-KINGAKU
                      MOVE    ZERO           TO    FLG-HENSYU2
                   END-IF
      *
                   ADD     RECE86-TEN        TO    WRK-KINGAKU
                   MOVE    WRK-KINGAKU       TO    WRK-ZAITENX
                   MOVE    RECE86-ZAIKAISU   TO    WRK-ZAIKAISUX
      *
                   PERFORM 2001-RECE44-ZAIHEN-MAE-SEC
      *            
                   PERFORM 2002-TBLZAIW-TAIHI-SEC
      *
                   PERFORM 2003-TEKIYOU-HENSYU-SEC
      *
                   PERFORM 990-RECE86-NEXT-SEC
      *
      *           ��������������
                   IF      CNT-IN-CANCEL   =   50
                       MOVE    ZERO            TO  CNT-IN-CANCEL
                       PERFORM 500-CANCEL-HENSYU-SEC
                   END-IF
           END-PERFORM
      *
           PERFORM 990-RECE82-READ-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Խ�������
      *****************************************************************
       2001-RECE44-ZAIHEN-MAE-SEC     SECTION.
      *
           MOVE    SPACE               TO  WRK-ZAI
                                           WRK-ZAI1
           MOVE    ZERO                TO  WRK-CLM
      *
           .
       2001-RECE44-ZAIHEN-MAE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ���٥ơ��֥��������
      *****************************************************************
       2002-TBLZAIW-TAIHI-SEC    SECTION.
      *
           MOVE    RECE86-SRYACTINF  TO  TBL-ZAIW-SRYACTINF
           MOVE    RECE86-NAMEINF    TO  TBL-ZAIW-NAMEINF  
      *
           .
       2002-TBLZAIW-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    �Խ�����
      *****************************************************************
       2003-TEKIYOU-HENSYU-SEC    SECTION.
      *
      *    ��ۡ�����Խ�
      ***> MOVE    TBL-ZAIW-TEN        TO  WRK-ZAITENX
      ***> MOVE    RECE86-ZAIKAISU     TO  WRK-ZAIKAISUX
      *     
           PERFORM 4304-TENSUKAI-HEN-SEC 
      *
      *    �����Խ������ٷ���׻�
           PERFORM 4302-MEISAI-KETA-HEN1-SEC
      *
      *    �ޡ����Ź԰٥ơ��֥���Ͻ���
      **** PERFORM 450-RECE87-OUT-SEC
      *
           .
       2003-TEKIYOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���١����ٷ塡�Խ�����
      *****************************************************************
       4302-MEISAI-KETA-HEN1-SEC    SECTION.
      *
           MOVE    1                   TO  WRK-ST              
      *
           MOVE    ZERO                TO  FLG-HENSYU
      *     
           PERFORM UNTIL   FLG-HENSYU  =   1  
               IF      WRK-TBLMAX         <   CONS-TEKIYOU1-LINE-MAX 
                   MOVE    SPACE               TO  WRK-ZAI1
                   IF  FLG-HENSYU2  =  ZERO
                       MOVE   "��"  TO   WRK-ZAI1-NAME(1:2)
                       MOVE    1    TO   FLG-HENSYU2
                   END-IF
                   MOVE    TBL-ZAIW-NAME (WRK-ST:)
                                               TO  WRK-ZAI1-NAME(3:)
                   IF      WRK-TNSKAI      NOT =   SPACE                            
                       IF      WRK-ZAI1-NAME (WRK-TNSKAI-ST1:)
                                               =   SPACE
                           MOVE    WRK-TNSKAI (WRK-TNSKAI-LEN:)
                                               TO
                                        WRK-ZAI1-NAME(WRK-TNSKAI-ST1:)
                           MOVE    1               TO  FLG-HENSYU
                       END-IF                 
                   END-IF                                                  
      *            �ޡ����Ź԰٥ơ��֥��Խ�����
                   PERFORM 450-RECE87-OUT-SEC
                   COMPUTE WRK-ST  =
                           WRK-ST  + CONS-TEKIYOU1-LEN - 2
               ELSE
                   MOVE    SPACE               TO  WRK-ZAI
                   IF  FLG-HENSYU2  =  ZERO
                       MOVE   "��"  TO   WRK-ZAI-NAME(1:2)
                       MOVE    1    TO   FLG-HENSYU2
                   END-IF
                   MOVE    TBL-ZAIW-NAME (WRK-ST:)
                                               TO  WRK-ZAI-NAME(3:)
                   IF      WRK-TNSKAI      NOT =   SPACE                            
                       IF      WRK-ZAI-NAME (WRK-TNSKAI-ST:)
                                               =   SPACE
                           MOVE    WRK-TNSKAI (WRK-TNSKAI-LEN:)
                                               TO
                                        WRK-ZAI-NAME(WRK-TNSKAI-ST:)
                           MOVE    1               TO  FLG-HENSYU
                       END-IF                 
                   END-IF                                                  
      *            �ޡ����Ź԰٥ơ��֥��Խ�����
                   PERFORM 450-RECE87-OUT-SEC
                   COMPUTE WRK-ST  =
                           WRK-ST  + CONS-TEKIYOU2-LEN - 2
               END-IF
      *
      *����������ۤ��Խ����ʤ��Ȥ��ޤ���������ʸ�������뤫         
               IF      WRK-TNSKAI          =   SPACE                            
                   IF      TBL-ZAIW-NAME (WRK-ST:)
                                               =   SPACE
                       MOVE    1               TO  FLG-HENSYU
                   END-IF                                                  
               END-IF
           END-PERFORM        
           .
       4302-MEISAI-KETA-HEN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ۡ�����Խ�����
      *****************************************************************
       4304-TENSUKAI-HEN-SEC    SECTION.
      *
           INITIALIZE              WRK-TNSKAI-AREA
      *
      ***> IF    ( TBL-ZAIW-SRYCD (1:1)  =   "8"   )
      ***> OR    ( TBL-ZAIW-SRYCD (1:3)  =   "008" )
           IF    ( RECE86-ZAILAST   =   ZERO )
           OR    ( WRK-KINGAKU      =   ZERO )
               GO  TO  4304-TENSUKAI-HEN-EXT
           END-IF
      *                                           
           INITIALIZE              ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI 
           MOVE    WRK-ZAITENX(3:7)
                                   TO  KANACHK-MAE-INPUT (1:7)
           MOVE    "X"             TO  KANACHK-MAE-INPUT (8:1)
           MOVE    WRK-ZAIKAISUX (2:3)
                                   TO  KANACHK-MAE-INPUT (9:3)
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   ZERO
           AND     KANACHK-MAX     >   ZERO
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                       TO  WRK-TNSKAI
               MOVE    "��"                TO  WRK-X
           END-IF
      *
      *    ɬ��ͭ�����������ZZZZZ X ����ZZZ �����Ѥη����
           MOVE    ZERO                TO  WRK-TNSKAI-KETA
           PERFORM VARYING   IDZ     FROM    1   BY  2
                   UNTIL     IDZ     >       22
      ***>     IF      WRK-TNSKAI (IDZ:2)      NOT =   SPACE
               IF      WRK-TNSKAI (IDZ:2)      NOT =   "��"
                   IF   IDZ = 1
                      COMPUTE WRK-TNSKAI-KETA = 22 - IDZ + 1
                      MOVE    IDZ   TO   WRK-TNSKAI-LEN
                   ELSE
                      COMPUTE WRK-TNSKAI-KETA = 22 - IDZ + 1 + 2
                      COMPUTE WRK-TNSKAI-LEN  = IDZ  -  2
                   END-IF
                   MOVE    22                  TO  IDZ
               END-IF 
           END-PERFORM
      *     
      *    �������ϰ��֤����
           COMPUTE   WRK-TNSKAI-ST     =   ( CONS-TEKIYOU2-LEN  -
                                             WRK-TNSKAI-KETA      )
                                       +   1
           COMPUTE   WRK-TNSKAI-ST1    =   ( CONS-TEKIYOU1-LEN  -
                                             WRK-TNSKAI-KETA      )
                                       +   1
      *
           .
       4304-TENSUKAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �쥻�ץȣ������Ͻ���
      *****************************************************************
       450-RECE87-OUT-SEC    SECTION.
      *
           ADD     1                   TO  WRK-LINENUM
           MOVE    SPACE               TO  RECE87-REC
           INITIALIZE                      RECE87-REC
      *     
           IF      WRK-TBLMAX         <   CONS-TEKIYOU1-LINE-MAX 
               ADD     1                   TO  WRK-TBLMAX
               MOVE    WRK-ZAI1            TO  RECE87-TEKIYO-INF
           ELSE    
               MOVE    WRK-ZAI             TO  RECE87-TEKIYO-INF
           END-IF
      *     
           PERFORM 4501-RECE87-HENSYU-SEC
      *
           MOVE    SPACE                TO  WRK-ZAI
                                            WRK-ZAI1
           .
       450-RECE87-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �쥻�ץȣ����Խ�����
      *****************************************************************
       4501-RECE87-HENSYU-SEC    SECTION.
      *
           MOVE    RECE86-KEY        TO  RECE87-KEY
           MOVE    WRK-LINENUM       TO  RECE87-LINENUM
           MOVE    RECE86-PTNUM      TO  RECE87-PTNUM
           MOVE    RECE86-SRYKA      TO  RECE87-SRYKA
      **** MOVE    RECE86-SRYKBN     TO  RECE87-SRYKBN
      *
           WRITE   RECE87-REC
           IF      STS-RECE87          NOT =   ZERO
               DISPLAY "*** ORCR0930 RECE87 WRITE ERR  ***"
               MOVE    2             TO  SPA-STS                       
           ELSE
               ADD     1             TO  CNT-RECE87
           END-IF    
      *
           .
       4501-RECE87-HENSYU-EXT.
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
           MOVE    2                   TO  SPA-STS                       
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
                   MOVE    2                   TO  SPA-STS
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           CLOSE                       RECE82-FILE
           CLOSE                       RECE87-FILE
           CLOSE                       RECE86-FILE     
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
           DISPLAY "RECE82  IN  " CNT-RECE82   
           DISPLAY "RECE87  CNT " CNT-RECE87 
           DISPLAY "*** ORCR0930 END ***"
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    �쥻�ץȣ����ǡ����縡������
      *****************************************************************
       990-RECE82-READ-SEC           SECTION.
      *
           READ    RECE82-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
                   MOVE    2           TO  SPA-STS
               NOT AT  END
                   ADD     1           TO  CNT-RECE82
      *
                   DISPLAY "RECE82  PTNUM=" RECE82-PTNUM
      *
                   IF       RECE82-HKNKBN   =   "5"
                       GO  TO  990-RECE82-READ-SEC
                   END-IF
      *
           END-READ
      *
           .
       990-RECE82-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    �쥻�ץȣ����ǡ����ɤ�
      ****************************************************************
       990-RECE86-NEXT-SEC           SECTION.
      *
           READ    RECE86-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE86
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE86
                   ADD     1         TO  CNT-IN-CANCEL 
           END-READ
      *
           .
       990-RECE86-NEXT-EXT.
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
      *    �ģ¡������ץ����
      *****************************************************************
       120-DBOPEN-SEC                SECTION.
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
       120-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡�������������
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