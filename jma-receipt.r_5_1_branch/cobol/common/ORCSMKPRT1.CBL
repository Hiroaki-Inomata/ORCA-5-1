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
       PROGRAM-ID.             ORCSMKPRT1.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : ����饤��Ģɼ��������
      *                    : (�ư�����DB�񤭹����ɲ�)
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/09/12    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����    ����
      *  04.07.00    NACL-����  11/03/20  ���饤����Ȱ����б�
      *****************************************************************
      *
      *  ����饤��Ģɼ����������ή��(2017.05.29)
      *  1.�̾����
      *  2.���饤����Ȱ�����Ģɼ�����
      *  3.Ginbee�Ķ�
      *  4.API����ΰ���
      *  5.HAORI����ΰ���
      *
      *  1.�̾�����ʽ�����֤Ǥΰ�����
      *    �����ƥ������1010 ���������
      *     +�θ��������
      *       +�ץ�󥿤ؤΥǡ���������0 ���ڡ������������
      *       +���饤����Ȱ���      ��0 ���ʤ���
      *
      *    ORCSONPRTSET �ư�����UUID����Ф�
      *    ��
      *    ĢɼPG
      *       +ORCSMKPRT1 Ģɼ����
      *         +HC01.sh ��Ģɼ������
      *         +online-reprint.sh (�ư����ǡ�����Ͽ��
      *          +ORCBONPRT
      *
      *       +ORCRPMAIN ĢɼAPI�ǡ����������ư����ǡ�����Ͽ
      *         +rpmain1.sh (push����)
      *
      *
      *  2.���饤����Ȱ�����Ģɼ�����
      *  3.Ginbee�Ķ�
      *    �����ƥ������1010 ���������
      *     +�θ��������
      *       +�ץ�󥿤ؤΥǡ���������1 Ģɼ���������
      *       +���饤����Ȱ���      ��1 �����
      *
      *    ORCSONPRTSET
      *    ��
      *    ORCSPRTCTRL ���饤����Ȱ����ե饰����
      *    ��
      *    ĢɼPG
      *    �� +ORCSMKPRT1
      *    ��   +online-reprint.sh (�ư����ǡ�����Ͽ��
      *    ��    +ORCBONPRT
      *    ��
      *    �� +ORCRPMAIN
      *    ��   +online-reprint.sh (�ư����ǡ�����Ͽ��
      *    ��    +ORCBONPRT
      *    ��
      *    ORCHCDUMMY
      *       .ORCSMKPRT1
      *         +client_print.sh (Ģɼ������
      *           +ORCBCLPRT
      *             +print_prv.sh
      *               +ORCBPRT1
      *                 +HCALL_prv.sh��Ģɼ������
      *
      *                 +push_form.sh��push���Ρ�
      *
      *
      *  4.API����ΰ���
      *    ORCSONPRTSET
      *    ��
      *    ORCSPRTCTRL ���饤����Ȱ����ե饰����
      *    ��
      *    ĢɼPG
      *    �� +ORCRPMAIN ĢɼAPI�ǡ����������ư����ǡ�����Ͽ
      *    ��
      *    ORCSAPIFM(�쥹�ݥ󥹺���)
      *      +ORCHCDUMMY 
      *        .ORCSAPIFMTERM(push���Ρ� <- Ģɼ�ǡ�������API�ѥǡ��������������
      *        .ORCSMKPRT1               <- ĢɼPDF�����������
      *          +client_print.sh (Ģɼ������
      *            +ORCBCLPRT
      *              +print_prv.sh
      *                +ORCBPRT1
      *                  +HCALL_prv.sh��Ģɼ������
      *
      *                +push����
      *
      *  5.HAORI����ΰ���
      *    ORCSONPRTSET
      *    ��
      *    ORCSPRTCTRL ���饤����Ȱ����ե饰����
      *    ��
      *    ĢɼPG
      *    �� +ORCRPMAIN ĢɼAPI�ǡ����������ư����ǡ�����Ͽ
      *    ��
      *    ORCHCDUMMY
      *      .rpmain1.sh                 <- Ģɼ�ǡ�������API�ѥǡ��������������
      *        +ORCSAPIFMTERM(push���Ρ�
      *      .ORCSMKPRT1                 <- ĢɼPDF�����������
      *       +client_print.sh (Ģɼ������
      *         +ORCBCLPRT
      *           +print_prv.sh
      *             +ORCBPRT1
      *               +HCALL_prv.sh��Ģɼ������
      *
      *             +push����
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    �����ѥǡ���
           SELECT  PRT-IN-FILE     ASSIGN  ASGNPARA
                                   FILE    STATUS  IS  STS-IN.
           SELECT  PRT-OUT-FILE    ASSIGN  HOUTPARA
                                   FILE    STATUS  IS  STS-OUT.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    �����ѥǡ���
       FD  PRT-IN-FILE.
       01  PRT-IN-REC.
           03  PRT-IN-PRTDATA      PIC X(20000).
       FD  PRT-OUT-FILE.
       01  PRT-OUT-REC.
           03  PRT-OUT-PRTDATA     PIC X(20000).
      *
       WORKING-STORAGE             SECTION.
      *
       01  PRTOUTPARA.
           03  PRTOUTPARA-FORM-ID  PIC  X(04).
           03  FILLER              PIC  X(01) VALUE ".".
           03  PRTOUTPARA-UUID     PIC  X(31).
           03  FILLER              PIC  X(01) VALUE ".".
           03  PRTOUTPARA-SYOHMS   PIC  9(08).
           03  FILLER              PIC  X(01) VALUE ".".
           03  PRTOUTPARA-CNT      PIC  9(06).
           03  FILLER              PIC  X(04) VALUE ".dat".
      *
       01  STS-AREA.
           03  STS-IN              PIC X(02).
           03  STS-OUT             PIC X(02).
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *     �����ѥǡ��� ̾���ΰ� 
           COPY    "CPCOMMONPRT.INC".
      *
           COPY    "CPCOMMONPRT.INC"       REPLACING   //HC01PARA//
                                           BY          //HC99PARA//.
           COPY    "CPASGNPARA.INC".
           COPY    "CPASGNPARA.INC"       REPLACING    //ASGNPARA//
                                           BY          //HOUTPARA//.
      *
      *    ���������ΰ�
           COPY    "CPSHELLTBL.INC".
      *    COPY    "ORCA-DBPATH".
      *
       01  FLG-AREA.
           03  FLG-SYORI           PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-PRTCTRL         PIC 9(01).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-DIA             PIC X(10).
           03  WRK-SHORI-RENNUM    PIC 9(02).
           03  SGYOUMU-CODE        PIC 9(03).
           03  WRK-ENCODING        PIC X(01).
           03  WRK-ENCODING9       REDEFINES WRK-ENCODING
                                   PIC 9(01).
           03  WRK-PRT-CNT         PIC 9(04).
           03  WRK-PRT-FLG         PIC 9(02).
           03  WRK-PRTDATA         PIC X(20000).
      *
      *    �ǥ��쥯�ȥ����
       01  SHELL-NAME.
           03  FILLER              PIC X(30)   VALUE
               "scripts/daily/".
      *   �ģ��
       01  SHELL-DIA.
           03  FILLER              PIC X(40)   VALUE
               "scripts/daily/print-data.rb".
      *   �ͣϣΣУ�
       01  SHELL-MONPE.
           03  FILLER              PIC X(10)   VALUE
               "red2ps".
      *    �ƣϣңͰ���
       01  SHELL-FORM.
           03  FILLER              PIC X(10)   VALUE
               "form/".
      *    �쥳���ɰ���
       01  SHELL-RECORD.
           03  FILLER              PIC X(10)   VALUE
               "record/".
      *
      *   ONLIEN REPRINT
       01  SHELL-ONLINE.
           03  FILLER              PIC X(40)   VALUE
               "scripts/daily/online-reprint.sh".
       01  WRK-SHELL-NAME.
           05  FILLER             PIC X(16) VALUE
                                             "scripts/allways/".
           05  WRK-SHELL-SHELL    PIC X(20).         
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *    ��������ޥ���
       01  PRTCTRL-REC.
           COPY    "CPPRTCTRL.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    ����饤��Ģɼ�ɣ�Ϣ��
           COPY    "CPORCSRPLNK.INC".
      *
      *    ���ե��å���
           COPY   "CPSK1044.INC".
      *    �ǥ��쥯�ȥ�����Խ�����
           COPY   "CPMKPASS.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      *
      *    �ե��������ѥ�᥿
           COPY    "CPORCSFDEL.INC".
      *
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
           COPY  "CPORCSMKPRT.INC".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSMKPRTAREA
           SPA-AREA.
      *
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  IDX-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           CALL "ORCSGYOUMUGET"        USING
                                       SPA-AREA
                                       SGYOUMU-CODE
           DISPLAY "take gyomu_cd = " SGYOUMU-CODE
      *
           CALL    "ORCSENCODING"      USING
                                       MCPAREA
                                       SPA-AREA
           MOVE    SPA-ENCODING    TO  WRK-ENCODING9
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    MKPRT-DAT
                                   TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  ASGNPARA
           MOVE    SGETTEMP-ST     TO  ASGNPARA-ST
           MOVE    ASGNPARA (ASGNPARA-ST:)
                                   TO  HC01PARA-BASENAME
      *
           IF    ( SPA-CLIENT-PRT-FLG  =  3 )
               PERFORM 301-CLIENTPRINT-SEC
           ELSE
      *
               MOVE    "PRINTED"   TO  SPA-PUTTYPE
      *
               OPEN    INPUT           PRT-IN-FILE
               IF    ( STS-IN      NOT =   ZERO )
                   DISPLAY "INPUT OPEN ERR"
               ELSE
                   INITIALIZE      PRT-IN-REC
                   READ    PRT-IN-FILE
                   END-READ
                   MOVE    PRT-IN-REC  TO WRK-PRTDATA
               END-IF
               CLOSE   PRT-IN-FILE
      *
               IF    ( SPA-CLIENT-PRT-FLG  NOT =  1 )
                   PERFORM 200-MON-PRINT-SEC
               END-IF
      *
      *        �ư����񤭽Ф���Ƚ��
               IF    ( SPA-PRT-FLG             NOT =   SPACE )
                   IF    ( SPA-CLIENT-PRT-FLG      =   ZERO )
                    AND  ( SPA-PRTCONF             =   SPACE )
                    AND  ( SPA-CLIENT-TEMP-FLG     =   2 )
                       CONTINUE
                   ELSE
                       PERFORM 300-REPRINT-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    �Խ�����
      *****************************************************************
       200-MON-PRINT-SEC            SECTION.
      *
      *    �ե�����ǥ��쥯�ȥ���ֻ���
           INITIALIZE                  ORCSMKPASSAREA
      *    �ӣȣţ̣�̾
           STRING  SHELL-NAME          DELIMITED  BY  SPACE
                   MKPRT-ID            DELIMITED  BY  SPACE
                                       INTO    MKPASS-IN-01
           END-STRING
      *    �ƣϣң�
           STRING  SHELL-FORM          DELIMITED  BY  SPACE
                   MKPRT-DIA           DELIMITED  BY  SPACE
                                       INTO    MKPASS-IN-03
           END-STRING
      *    DEF̾
           IF    ( MKPRT-DEF           =   "__SITE__" )
               CALL    "ORCSMKPASSSITE"
                                       USING   ORCSMKPASSAREA
           ELSE
               CALL    "ORCSMKPASS"    USING   ORCSMKPASSAREA
           END-IF
      *
           PERFORM 201-OFFSET-GET-SEC
      *
      *    SHELL
           MOVE    SPACE               TO  SHELLTBL
           INITIALIZE                      SHELLTBL
      *
           MOVE    MKPRT-ID            TO  SHELLTBL-NAME
      *    MONP
           MOVE    SHELL-MONPE         TO  SHELLTBL-ARG1
      *    FROM
           MOVE    MKPASS-OT-03        TO  SHELLTBL-ARG2
      *    ���᡼���ݻ��ե饰��T:�ݻ���
           IF    ( SPA-CLIENT-TEMP-FLG =   2 )
               MOVE    "T"             TO  SHELLTBL-ARG3
           END-IF
      *    �ǡ�������
           MOVE    MKPRT-DAT (ASGNPARA-ST:)
                                       TO  SHELLTBL-ARG4
      *    ��������
           MOVE    MKPRT-PRTNM         TO  SHELLTBL-ARG5
      *    ���ե��å�
           MOVE    SYS-1044-OFFSET-X   TO  SHELLTBL-ARG6
           MOVE    SYS-1044-OFFSET-Y   TO  SHELLTBL-ARG7
      *    ���󥳡��ǥ���
           MOVE    WRK-ENCODING        TO  SHELLTBL-ARG8
      *
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
      *****MOVE    PATH-SHELL-SHELL    TO  MCP-PATH
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "daily"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       200-MON-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    ���ե��åȼ�������
      *****************************************************************
       201-OFFSET-GET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SYS-1044-REC
           INITIALIZE                  SYS-1044-REC
           MOVE    "1044"              TO  SYS-1044-KANRICD
           MOVE    MKPRT-DIA           TO  SYS-1044-FORMID
           STRING  MKPRT-PRTNM         DELIMITED  BY  SPACE
                   "%"                 DELIMITED  BY  SIZE
                                       INTO    SYS-1044-TBL(21:)
           END-STRING
           MOVE    "99999999"          TO  SYS-1044-STYUKYMD
           MOVE    "99999999"          TO  SYS-1044-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1044-HOSPNUM
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    SYS-1044-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE  MCPDATA-REC   TO  SYS-1044-REC
                   IF   (SYS-1044-FORMID  =   MKPRT-DIA  )  AND
                        (SYS-1044-PRTNM   =   MKPRT-PRTNM)
                         CONTINUE
                   ELSE
                         INITIALIZE    SYS-1044-REC
                   END-IF
               ELSE
                   INITIALIZE          SYS-1044-REC
               END-IF
           ELSE
               INITIALIZE              SYS-1044-REC
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       201-OFFSET-GET-EXT.
           EXIT.
      *****************************************************************
      *    �Խ�����
      *****************************************************************
       300-REPRINT-SEC                 SECTION.
      *
           MOVE    MKPRT-DAT (ASGNPARA-ST:)
                                       TO  HC99PARA-BASENAME
      *
           MOVE HC99PARA-FORM-ID       TO  PRTOUTPARA-FORM-ID
           MOVE SPA-PRT-UUID           TO  PRTOUTPARA-UUID
           MOVE HC99PARA-SYOHMS        TO  PRTOUTPARA-SYOHMS
           MOVE HC99PARA-CNT           TO  PRTOUTPARA-CNT
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    PRTOUTPARA
                                       TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO HOUTPARA
           MOVE    SGETTEMP-ST         TO HOUTPARA-ST
      *
           OPEN    OUTPUT                 PRT-OUT-FILE
           MOVE    WRK-PRTDATA         TO PRT-OUT-REC
           WRITE   PRT-OUT-REC
           CLOSE   PRT-OUT-FILE
      *
           INITIALIZE                  LNK-PRTKANRI-AREA
      *    �ե�����ǥ��쥯�ȥ���ֻ���
           INITIALIZE              ORCSMKPASSAREA
           MOVE    SHELL-ONLINE    TO  MKPASS-IN-01
      *    �ƣϣң�
           STRING  SHELL-FORM      DELIMITED  BY  SPACE
                   MKPRT-DIA       DELIMITED  BY  SPACE
                                   INTO    MKPASS-IN-02
           END-STRING
           CALL    "ORCSMKPASS"    USING   ORCSMKPASSAREA
      *
           MOVE    SPACE               TO  SHELLTBL
           INITIALIZE                      SHELLTBL
      *
           MOVE    "online-reprint.sh"
                                   TO  SHELLTBL-NAME
           MOVE    1               TO  LNK-PRTKANRI-RENNUM
           MOVE    "ONLINE"        TO  LNK-PRTKANRI-TBL-KEY
           DISPLAY "rep SPA-PRT-GROUP = " SPA-PRT-GROUP
           MOVE    SPA-PRT-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
                                       LNK-PRTKANRI-SRYYM
           MOVE    1               TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
      *
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG3
           IF      SPA-PRT-FLG     IS  NUMERIC
               MOVE SPA-PRT-FLG    TO  WRK-SHORI-RENNUM
           ELSE
               MOVE ZERO           TO  WRK-SHORI-RENNUM
           END-IF
           MOVE    WRK-SHORI-RENNUM
                                   TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG6
      *    MOVE    LNK-PRTKANRI-SHELLID
           MOVE   "online-reprint.sh"
                                   TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG10
           MOVE    MKPRT-PRTNM
                                   TO  SHELLTBL-ARG11
           MOVE    SPA-SRYYMD      TO  SHELLTBL-ARG12
           MOVE    SPA-PRT-UUID    TO  SHELLTBL-ARG13
           MOVE    SPA-PTID        TO  SHELLTBL-ARG14
      *    SITE_KBN
           IF    ( MKPRT-DEF           =   "__SITE__" )
               MOVE    "2"         TO  SHELLTBL-ARG15
           ELSE
               MOVE    "1"         TO  SHELLTBL-ARG15
           END-IF
      *
           MOVE    SGYOUMU-CODE    TO  SHELLTBL-ARG16
           MOVE    MKPRT-INFO      TO  SHELLTBL-ARG17
      *    DIA
           MOVE    MKPRT-ID        TO  SHELLTBL-ARG18
      *    FORM
           MOVE    MKPRT-DIA       TO  SHELLTBL-ARG19
      *    TYPE
           MOVE    MKPRT-TYPE      TO  SHELLTBL-ARG20
      *    �ǡ�������
           MOVE    HOUTPARA (HOUTPARA-ST:)
                                   TO  SHELLTBL-ARG21
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG22
      *
           IF    ( SPA-CLIENT-TEMP-FLG =   2 )
            OR   ( SPA-PRTCONF         =   "3" )
               MOVE    "D"         TO  SHELLTBL-ARG23
           ELSE
               MOVE    "1"         TO  SHELLTBL-ARG23
           END-IF
           MOVE    MKPRT-DAT (ASGNPARA-ST:)
                                   TO  SHELLTBL-ARG24
      *
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "daily"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( SPA-PRTCONF         =   "3" )
      *
               INITIALIZE                  SRPLNK-AREA
               MOVE    "SEL"           TO  SRPLNK-MODE
               CALL    "ORCSRPLNK"         USING
                                           SRPLNK-AREA
                                           SPA-AREA
      *
               MOVE    "UPD"           TO  SRPLNK-MODE
               MOVE    MKPRT-INFO      TO  SRPLNK-TITLE
               CALL    "ORCSRPLNK"         USING
                                           SRPLNK-AREA
                                           SPA-AREA
      *
           END-IF
      *
           .
       300-REPRINT-EXT.
           EXIT.
      *****************************************************************
      *    �Խ�����
      *****************************************************************
       301-CLIENTPRINT-SEC         SECTION.
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "client_print.sh"   TO  SHELLTBL-NAME
      *
           MOVE    SPACE               TO  LNK-PRTKANRI-AREA
           INITIALIZE                      LNK-PRTKANRI-AREA
      *
           MOVE    1                   TO  LNK-PRTKANRI-RENNUM
           MOVE    "ONLINE"            TO  LNK-PRTKANRI-TBL-KEY
           MOVE    SPA-PRT-GROUP       TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-SYSYMD          TO  LNK-PRTKANRI-SKYYMD
                                           LNK-PRTKANRI-SRYYM
           MOVE    1                   TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID          TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
           MOVE    SPA-PRT-UUID        TO  LNK-PRTKANRI-TBL-UUID
           MOVE    SPA-PRT-FLG         TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    "client_print.sh"   TO  LNK-PRTKANRI-SHELLID
      *
           MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SHELLTBL-ARG3
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID   TO  SHELLTBL-ARG10
           MOVE    LNK-PRTKANRI-PRTNM  TO  SHELLTBL-ARG11
           MOVE    LNK-PRTKANRI-TBL-UUID
                                       TO  SHELLTBL-ARG12
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG13
           MOVE    SPA-CLIENT-SHOWDIALOG
                                       TO  SHELLTBL-ARG14
           MOVE    SPA-PRTCONF         TO  SHELLTBL-ARG15
           MOVE    SPA-PTID            TO  SHELLTBL-ARG16
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       301-CLIENTPRINT-EXT.
           EXIT.