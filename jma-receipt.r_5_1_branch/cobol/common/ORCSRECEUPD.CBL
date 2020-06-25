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
       PROGRAM-ID.             ORCSRECEUPD.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : 
      *  ������            : �쥻�ץȴ�����������
      *  05/02/08    NACL��ƣ��    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-¿��Ǽ  05/11/11  MONFUNC �б� ¾
      *  03.05.00    NACL-¿��Ǽ  07/04/XX  ���롼�׿����б�
      *
      *  04.07.01    NACL-¿��Ǽ  14/05/15  �쥻�ץȰ��������ޤ��ΤȤ���
      *                                     �������ʤ�
      *                                     �������оݳ��Ȥ���
      *                                     �軰�Թ԰٤ΤȤ��ϰ���ʬ�⹹�� 
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-RECEUPD         PIC 9(01).
           03  FLG-RECE07          PIC 9(01).
      *
      *    1:���ݤΤ� 2:�軰�Թ԰٤Τ� 9:ξ������
           03  FLG-DAISANSYA       PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYM       PIC X(06).
               05  WRK-SYSDD       PIC 9(02).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
      *    �쥻�ץȺ�����������
       01  WRK-RECEUPD-REC.
           COPY    "CPRECEUPD.INC" REPLACING  //RECEUPD-//
                                   BY         //WRK-RECEUPD-//.
      *     
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ��������
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    �쥻�ץȺ�������
       01  RECEUPD-REC.
           COPY    "CPRECEUPD.INC".
      *
      *    �쥻�ץȴ���     
       01  RECE07-REC. 
           COPY    "CPRCF007.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSRECEUPD.INC".
grpsys*
grpsys      COPY    "COMMON-SPA".
      *****************************************************************
     *
       PROCEDURE                   DIVISION    USING
                                   ORCSRECEUPDAREA
           SPA-AREA.
     *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *    
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYSYMD
           IF      WRK-SYSDD           <=   10
               INITIALIZE                   STS-AREA-DAY
               INITIALIZE                   LNK-DAY6-AREA
               MOVE  "61"                   TO  LNK-DAY6-IRAI
               MOVE  WRK-SYSYMD             TO  LNK-DAY6-KIJUN
               MOVE  "2"                    TO  LNK-DAY6-ZOGENPTN
               MOVE  -1                     TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"              USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN      TO  WRK-SYSYMD 
           END-IF
      *
           MOVE    ZERO                TO  LNK-UPD-RC
                                           FLG-DAISANSYA
      *
           IF      LNK-UPD-SYORI       =   "2"
      *        �������оݳ�
               IF      LNK-UPD-NYUGAIKBN   =   "1"
                               DISPLAY "NYUIN "
                   GO  TO  000-PROC-EXT
               END-IF
      *
      *        ������Υ쥻�ץȰ�������̵ͭ
               INITIALIZE                  RECE07-REC
      *        0:����
      *        1:ϫ�ҡ���̳�ҳ���ϫ�ҽ���
      *        2:�����ա�ϫ�ҽ���
      *        4:�����աʷ��ݽ���
      *        5:��̳�ҳ��ʷ��ݽ���
      *        6:����
      *        7:�����ա��軰�Թ԰١�
               EVALUATE    LNK-UPD-HKNKBN
                   WHEN    "0"
      *                ����
                       MOVE    "HC06"              TO  RECE07-PRTID
                       MOVE    "0"                 TO  RECE07-RECEKBN
                   WHEN    "1"
                   WHEN    "5"
      *                ϫ��
                       MOVE    "HC21"              TO  RECE07-PRTID
                       MOVE    "3"                 TO  RECE07-RECEKBN
                   WHEN    "2"
                   WHEN    "4"
      *                �����աʿ��ͼ���
                       MOVE    "HC23"              TO  RECE07-PRTID
                       MOVE    "4"                 TO  RECE07-RECEKBN
                   WHEN    "7"
      *                �����ա��軰�Թ԰١�
                       MOVE    "HC23"              TO  RECE07-PRTID
                       MOVE    "8"                 TO  RECE07-RECEKBN
                   WHEN    "6"
      *                ����
                       MOVE    "HC25"              TO  RECE07-PRTID
                       MOVE    "6"                 TO  RECE07-RECEKBN
               END-EVALUATE
               MOVE    LNK-UPD-SRYYM       TO  RECE07-SRYYM
               MOVE    "1"                 TO  RECE07-SYOKBN
               MOVE    LNK-UPD-NYUGAIKBN   TO  RECE07-NYUGAIKBN
grpsys         MOVE    LNK-UPD-HOSPNUM     TO  RECE07-HOSPNUM
               MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
               MOVE    RECE07-REC          TO  MCPDATA-REC
               PERFORM 900-RECE07-INV-SEC
               IF      FLG-RECE07          =   1
                   EVALUATE    LNK-UPD-HKNKBN
                       WHEN    "2"
                       WHEN    "4"
      *                    �����աʽ����ͼ���
                           INITIALIZE                  RECE07-REC
                           MOVE    "HC23"          TO  RECE07-PRTID
                           MOVE    "5"             TO  RECE07-RECEKBN
                           MOVE    LNK-UPD-SRYYM   TO  RECE07-SRYYM
                           MOVE    "1"             TO  RECE07-SYOKBN
                           MOVE    LNK-UPD-NYUGAIKBN
                                                   TO  RECE07-NYUGAIKBN
grpsys                     MOVE    LNK-UPD-HOSPNUM TO  RECE07-HOSPNUM
                           MOVE    RECE07-REC      TO  MCPDATA-REC
                           PERFORM 900-RECE07-INV-SEC
                           IF      FLG-RECE07      =   1
                               DISPLAY "RECEPRT NOT FOUND"
                               GO  TO  000-PROC-EXT
                           END-IF
                       WHEN    "7"
                           MOVE    1           TO  FLG-DAISANSYA
                       WHEN    OTHER
                               DISPLAY "RECEPRT NOT FOUND"
                           GO  TO  000-PROC-EXT
                   END-EVALUATE
               END-IF
      *
      *        �軰�Թ԰٤ΤȤ��ϰ���ʬ������å�
               EVALUATE    LNK-UPD-HKNKBN
                   WHEN    "7"
                       INITIALIZE                  RECE07-REC
                       MOVE    "HC06"          TO  RECE07-PRTID
                       MOVE    "0"             TO  RECE07-RECEKBN
                       MOVE    LNK-UPD-SRYYM   TO  RECE07-SRYYM
                       MOVE    "1"             TO  RECE07-SYOKBN
                       MOVE    LNK-UPD-NYUGAIKBN
                                               TO  RECE07-NYUGAIKBN
grpsys                 MOVE    LNK-UPD-HOSPNUM TO  RECE07-HOSPNUM
                       MOVE    RECE07-REC      TO  MCPDATA-REC
                       PERFORM 900-RECE07-INV-SEC
                       IF      FLG-RECE07      =   1
                           IF      FLG-DAISANSYA   =   1
                               DISPLAY "RECEPRT NOT FOUND"
                               GO  TO  000-PROC-EXT
                           ELSE                          
                               MOVE    2           TO  FLG-DAISANSYA
                           END-IF
                       ELSE
                           IF      FLG-DAISANSYA   =   1
                               CONTINUE
                           ELSE                          
                               MOVE    9           TO  FLG-DAISANSYA
                           END-IF
                       END-IF
               END-EVALUATE 
      *??
           DISPLAY "FLG-DAISANSYA=" FLG-DAISANSYA
      *??
           END-IF
      *
           EVALUATE    LNK-UPD-SYORI
               WHEN    "1"
                   INITIALIZE                  JYURRK-REC
                   MOVE    LNK-UPD-HOSPNUM TO  JYURRK-HOSPNUM
                   MOVE    LNK-UPD-PTID    TO  JYURRK-PTID
                   MOVE    WRK-SYSYMD(1:6) TO  JYURRK-SRYYMD (1:6)
                   MOVE    "__"            TO  JYURRK-SRYYMD (7:2)
                   MOVE    "tbl_jyurrk"    TO  WRK-MCP-TABLE
                   MOVE    "key27"         TO  WRK-MCP-PATHNAME
                   PERFORM 200-HENSYU-SEC
               WHEN    "2"
                   INITIALIZE                  JYURRK-REC
                   MOVE    LNK-UPD-HOSPNUM TO  JYURRK-HOSPNUM
                   MOVE    LNK-UPD-PTID    TO  JYURRK-PTID
                   MOVE    LNK-UPD-NYUGAIKBN
                                           TO  JYURRK-NYUGAIKBN
                   MOVE    LNK-UPD-SRYYM   TO  JYURRK-SRYYMD (1:6)
                   MOVE    "__"            TO  JYURRK-SRYYMD (7:2)
                   MOVE    LNK-UPD-HKNKBN  TO  JYURRK-HKNKBN
                   MOVE    "tbl_jyurrk"    TO  WRK-MCP-TABLE
                   MOVE    "key28"         TO  WRK-MCP-PATHNAME
                   PERFORM 200-HENSYU-SEC
               WHEN    OTHER
                   MOVE    "1"             TO  LNK-UPD-RC
           END-EVALUATE
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *     
      *****************************************************************
      *    �쥻�ץȺ��������Խ�����
      *****************************************************************
       200-HENSYU-SEC            SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    JYURRK-REC          TO  MCPDATA-REC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-JYURRK-READ-N-SEC
           ELSE
               MOVE    1                  TO  FLG-JYURRK
           END-IF
      *
           PERFORM     UNTIL       FLG-JYURRK     =   1
                       OR          LNK-UPD-RC NOT =   ZERO
               INITIALIZE                  RECEUPD-REC
               MOVE    JYURRK-HKNKBN   TO  RECEUPD-HKNKBN
      *        �軰�Թ԰٤ǰ��ݤΤ�¸�ߤ���Ȥ�
               IF      FLG-DAISANSYA   =   1
                   MOVE    "0"             TO  RECEUPD-HKNKBN
               END-IF
               PERFORM 2001-RECEUPD-SEC
               IF      LNK-UPD-RC      =   "2"
                   CONTINUE
               ELSE
      *            �軰�Թ԰٤ǰ��ݡ��軰�Թ԰٤�ξ��¸�ߤ���Ȥ�
                   IF      FLG-DAISANSYA   =   9
                       INITIALIZE                  RECEUPD-REC
                       MOVE    "0"             TO  RECEUPD-HKNKBN
                       PERFORM 2001-RECEUPD-SEC
                   END-IF
      *
                   PERFORM 900-JYURRK-READ-N-SEC
               END-IF
           END-PERFORM      
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       200-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �쥻�ץȺ���������������
      *****************************************************************
       2001-RECEUPD-SEC         SECTION.
      *
           MOVE    JYURRK-HOSPNUM  TO  RECEUPD-HOSPNUM
           MOVE    JYURRK-NYUGAIKBN
                                       TO  RECEUPD-NYUGAIKBN
           MOVE    JYURRK-PTID     TO  RECEUPD-PTID
           MOVE    JYURRK-SRYKA    TO  RECEUPD-SRYKA
           MOVE    JYURRK-SRYYMD (1:6)
                                   TO  RECEUPD-SRYYM
           MOVE    SPACE           TO  RECEUPD-HKNKBN2
           MOVE    RECEUPD-REC     TO  WRK-RECEUPD-REC
      *??
           DISPLAY "RECEUPD-HKNKBN=" RECEUPD-HKNKBN " " RECEUPD-SRYYM
      *??
           PERFORM 900-RECEUPD-READ-SEC
           IF      FLG-RECEUPD     =   1
               MOVE    WRK-RECEUPD-REC     TO  RECEUPD-REC
           END-IF
      *
           MOVE    "1"             TO  RECEUPD-ONLUPD
      *
           EVALUATE    FLG-RECEUPD
               WHEN    1
                   MOVE    LNK-UPD-TERMID      TO  RECEUPD-TERMID
                   MOVE    LNK-UPD-OPID        TO  RECEUPD-OPID
                   MOVE    SMCNDATE-YMD        TO  RECEUPD-CREYMD
                   MOVE    SMCNDATE-YMD        TO  RECEUPD-UPYMD
                   MOVE    SMCNDATE-HMS        TO  RECEUPD-UPHMS
                   MOVE    "DBINSERT"          TO  MCP-FUNC
               WHEN    ZERO
                   MOVE    SMCNDATE-YMD        TO  RECEUPD-UPYMD
                   MOVE    SMCNDATE-HMS        TO  RECEUPD-UPHMS
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
           END-EVALUATE    
      *
           MOVE    RECEUPD-REC         TO  MCPDATA-REC
           MOVE    "tbl_receupd"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "2"                 TO  LNK-UPD-RC
           END-IF
           .
       2001-RECEUPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    �쥻�ץȺ��������ɹ�
      *****************************************************************
       900-RECEUPD-READ-SEC         SECTION.
      *
           MOVE    RECEUPD-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_receupd"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_receupd"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECEUPD
                   MOVE    MCPDATA-REC         TO  RECEUPD-REC
               ELSE
                  MOVE    1                    TO  FLG-RECEUPD
               END-IF
           ELSE
               MOVE    1                       TO  FLG-RECEUPD
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_receupd"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-RECEUPD-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������ɹ�
      *****************************************************************
       900-JYURRK-READ-N-SEC         SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-JYURRK
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               IF      JYURRK-NYUGAIKBN    =   "1"
                   GO  TO  900-JYURRK-READ-N-SEC
               END-IF
           ELSE
              MOVE    1               TO  FLG-JYURRK
              INITIALIZE                  JYURRK-REC
           END-IF
           .
       900-JYURRK-READ-N-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    �쥻�ץȴ����ɹ�
      *****************************************************************
       900-RECE07-INV-SEC         SECTION.
      *
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE07
                   MOVE    MCPDATA-REC         TO  RECE07-REC
               ELSE
                   MOVE    1                   TO  FLG-RECE07
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECE07-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ӣţ̣ţãԽ���
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.