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
       PROGRAM-ID.             ORCHCN03V02.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : �����������μ��񡡰����ʣȣãΣ����֣�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  07/10/18    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-��Ʀ��  04/01/29  �ݸ�Ŭ�ѳ��ε��ܤȾ������б�
      *  01.00.02    NACL-��Ʀ��  05/05/25  ϫ�ҡ������դδ�������ʬ�����٤�
      *                                     ɽ������褦���ѹ�
      *  02.08.00    NACL-����    06.02.15  �ư������ץ�ӥ塼�б�
      *  03.04.01    NACL-����    07/01/17  ���ŵ���̾�Ρ����ꥻ�å��ѹ�
      *  03.04.02    NACL-����    07/01/30  ���������H19ǯ4���б�
      *  03.05.00    NACL-����    07.05.17  ���롼�׿����б�
      *  04.01.00    NACL-����    07/10/22  ��̳�ҳ��б�
      *  04.02.00    NACL-����    08/03/07  �����б�
      *  04.03.00    NACL-����    08/09/17  �����б�
      *  04.04.00    NACL-����    09/01/14  �ץ�����४�ץ�����б�
      *  04.05.00    NACL-����    09/07/15  ORCHCN03V03������
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    �������μ����̥ѥ�᥿
           COPY    "CPORCHCN03.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCHCN03AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    "V2"            TO      ORCHCN03-SIZE
           CALL    "ORCHCN03V03"   USING   ORCHCN03AREA
                                           SPA-AREA
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
