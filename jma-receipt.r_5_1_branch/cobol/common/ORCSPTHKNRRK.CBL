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
       PROGRAM-ID.             ORCSPTHKNRRK.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ������Ͽ
      *  ����ݡ��ͥ��̾  : �����ݸ���ǧǯ�������򹹿�����
      *  �ֵѥ��顼������  : 00:���09:�������顼��01:�оݤʤ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  11.08.04    NACL-¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����      ����
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-PTHKNRRK        PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-RENNUM               PIC 9(02).
      *
      *    �ݸ���ǧǯ��������ơ��֥�
       01  TBL-PTHKNRRK-AREA.
           02  TBL-PTHKNRRK-REC             OCCURS   10.
           COPY    "CPPTHKNRRK.INC"  REPLACING
                                     //PTHKNRRK-// BY //TBL-PTHKNRRK-//.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �ݸ���ǧǯ��������ޥ���
       01  PTHKNRRK-REC.
           COPY    "CPPTHKNRRK.INC".
      *
           COPY    "MCPAREA".
      *
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *    �ݸ���ǧǯ����������Ͽ����
           COPY    "CPORCSPTHKNRRK.INC".
      *
      *
       PROCEDURE                    DIVISION    USING
           SPA-AREA
           ORCSPTHKNRRKAREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
      *    �������
           PERFORM 100-INIT-SYORI-SEC
      *
           IF      ORCSHKNRRK-RC       =   ZERO
      *        �硡����
               PERFORM 200-MAIN-SYORI-SEC
           END-IF
           .
      *
       000-PROC-EXT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SYORI-SEC                SECTION.
      *
           IF   (ORCSHKNRRK-PTID       =   ZERO )
           OR   (ORCSHKNRRK-HKNID      =   ZERO )
           OR   (ORCSHKNRRK-KAKUNINYMD =   SPACE
                                       OR  ZERO  )
               MOVE    1                   TO  ORCSHKNRRK-RC
           END-IF
           .
       100-INIT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    �硡����
      *****************************************************************
       200-MAIN-SYORI-SEC                SECTION.
      *
           INITIALIZE                  TBL-PTHKNRRK-AREA
      *
           INITIALIZE                  PTHKNRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTHKNRRK-HOSPNUM
           MOVE    ORCSHKNRRK-PTID     TO  PTHKNRRK-PTID
           MOVE    ORCSHKNRRK-HKNID    TO  PTHKNRRK-HKNID
      *
           MOVE    PTHKNRRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthknrrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthknrrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PTHKNRRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNRRK
               INITIALIZE                  PTHKNRRK-REC
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL     (FLG-PTHKNRRK     =   1    )
               ADD     1                   TO  IDX
               MOVE    PTHKNRRK-REC        TO  TBL-PTHKNRRK-REC (IDX)
      *
               MOVE    "tbl_pthknrrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PTHKNRRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_pthknrrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     ORCSHKNRRK-KAKUNINYMD    =   TBL-PTHKNRRK-KAKUNINYMD
                                                                 (1)
      *        Ʊ���ι����ʤ�
               CONTINUE
           ELSE
      *        ��ǧǯ������Ͽ����
               PERFORM 2001-PTHKNRRK-SYORI-SEC
           END-IF
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ݸ���ǧǯ�����������
      *****************************************************************
       2001-PTHKNRRK-SYORI-SEC              SECTION.
      *
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    ��ǧǯ����������
           INITIALIZE                  PTHKNRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTHKNRRK-HOSPNUM
           MOVE    ORCSHKNRRK-PTID     TO  PTHKNRRK-PTID
           MOVE    ORCSHKNRRK-HKNID    TO  PTHKNRRK-HKNID
      *
           MOVE    PTHKNRRK-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_pthknrrk"      TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ORCSPTHKNRRK DEL ERR:" PTHKNRRK-KEY
                                    ",MCP-RC:" MCP-RC
               MOVE    9                   TO  ORCSHKNRRK-RC
               GO      TO      2001-PTHKNRRK-SYORI-EXT
           END-IF
      *
      *    ����ʬ�ɲ�
           MOVE    ZERO                TO  WRK-RENNUM
           PERFORM 20011-PTHKNRRK-ADD-SEC
      *    ����ʬ�ɲ�
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   5    )
                   OR         (ORCSHKNRRK-RC   NOT =   ZERO )
                   OR         (TBL-PTHKNRRK-KAKUNINYMD (IDX) =   SPACE)
               MOVE    TBL-PTHKNRRK-REC (IDX)  TO  PTHKNRRK-REC
      *
               ADD     1                   TO  WRK-RENNUM
               IF      WRK-RENNUM          >   5
      *            ��Ͽ����ϣ���ޤǤȤ���
                   MOVE    10                  TO  IDX
               ELSE
                   MOVE    WRK-RENNUM          TO  PTHKNRRK-RENNUM
      *
                   MOVE    SMCNDATE-YMD        TO  PTHKNRRK-UPYMD
                   MOVE    SMCNDATE-HMS        TO  PTHKNRRK-UPHMS
                   PERFORM 20013-PTHKNRRK-INS-SEC
               END-IF
           END-PERFORM
           .
       2001-PTHKNRRK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ʬ�ݸ���ǧǯ����������Ͽ����
      *****************************************************************
       20011-PTHKNRRK-ADD-SEC              SECTION.
      *
           INITIALIZE                  PTHKNRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTHKNRRK-HOSPNUM
           MOVE    ORCSHKNRRK-PTID     TO  PTHKNRRK-PTID
           MOVE    ORCSHKNRRK-HKNID    TO  PTHKNRRK-HKNID
           ADD     1                   TO  WRK-RENNUM
           MOVE    WRK-RENNUM          TO  PTHKNRRK-RENNUM
           MOVE    ORCSHKNRRK-KAKUNINYMD
                                       TO  PTHKNRRK-KAKUNINYMD
      *
           MOVE    SMCNDATE-YMD        TO  PTHKNRRK-CREYMD
           MOVE    SMCNDATE-YMD        TO  PTHKNRRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTHKNRRK-UPHMS
           PERFORM 20013-PTHKNRRK-INS-SEC
           .
       20011-PTHKNRRK-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ݸ���ǧǯ����������Ͽ����
      *****************************************************************
       20013-PTHKNRRK-INS-SEC          SECTION.
      *
           MOVE    SPA-OPID            TO  PTHKNRRK-OPID
      *
           MOVE    PTHKNRRK-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_pthknrrk"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ORCSPTHKNRRK INS ERR:" PTHKNRRK-KEY
                                    ",MCP-RC:" MCP-RC
               MOVE    9                   TO  ORCSHKNRRK-RC
           END-IF
           .
       20013-PTHKNRRK-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ݸ���ǧǯ��������ޥ�����������
      *****************************************************************
       900-PTHKNRRK-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTHKNRRK-REC
               MOVE    ZERO                TO  FLG-PTHKNRRK
           ELSE
               INITIALIZE                  PTHKNRRK-REC
               MOVE    1                   TO  FLG-PTHKNRRK
           END-IF
           .
       900-PTHKNRRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *